#!/usr/bin/env perl

# Annotate a declaration file with implicit lackwit comparability
# information.  Overwrites the file with Lackwit comparability
# information, saving a copy of the original as
# filename.decls.nonlackwit.

# Assumes:
#   - $LACKWIT_HOME is set to the directory where lackwit is installed
#   - The lackwit database has already been created

# Perl port of a similar C++ program written by Adam Czeisler

# This program is complicated for several reasons.  First, we need to
# "translate" between dfec and lackwit.  We need to massage the output
# of dfec before it is sent to lackwit, and we need to massage the
# output of lackwit before writing it to the decls file.  Second,
# there are some bugs and missing features in lackwit we need to work
# around.  Last, lackwit outputs comparability as a list of comparable
# variables.  Daikon needs to have variables grouped into equivalence
# classes.  We must translate from lackwit's notion of comparability
# to Daikon's.

# MISC: Lackwit doesn't know about procedure returns, so we added a
# dummy variable named "lh_return_value".

# We must remove the leading :: from global variables in the decls
# file, since lackwit can't handle ::.

# We have to be careful about preserving the transitivity of the
# comparable relationship.  That is, if a cmp b, then b cmp a.  To
# guarantee transitivity, we calculate the equivalence classes as
# follows.  First, we query lackwit to get a set of variables
# comparable to each variable at the ppt.  Then, we unify the sets by
# merging sets that have elements in common.  The result is a set of
# disjoint sets, with each set representing an equivalence class.
# Next, we filter the equivalence classes, removing elements not
# present at the ppt.  Finally, we calculate the implicit type of each
# variable using the equivalence classes.

# ARRAYS/POINTERS: In C, there is almost no difference between a
# pointer to a single thing, and an array of things.  As far as I can
# tell, Lackwit treats pointers and arrays identically.  However,
# Lackwit does not have very good support for arrays.  We need to
# determine three different comparabilities for an array: the
# comparability of the pointer, the comparability of the elements, and
# the comparability of the indices.  Assume we have an array a[].

# Lackwit can find pointer comparability fine.  Just query for "a".

# Lackwit has a bug when finding element comparability.  If you query
# for "a[]" or "*a", you will usually get the right answer, except
# when the comparability crosses procedure boundaries.  Look at the
# lackwit.c file in dfec-tests/lackwit.  On its own, lackwit gets the
# right answer for array_elements, but the wrong answer for
# call_array_elements.  To fix this, we add the variable "a_element"
# to the source code before instrumenting it.  Expressions in the
# program are changed from "a[0] = b" to "a_element = b, a[0] =
# a_element".  To find the comparability of "a[]", we actually query
# lackwit for "a_element".  Then, the results must be processed to
# convert "a_element" back to "a[]".

# Lackwit cannot find array index comparability.  We add the variable
# "a_index" to the source code before instrumenting it.  Expressions
# in the program are changed from "a[0] = b" to "a_index = 0,
# a[a_index] = b".  We query lackwit for "a_index" to find the
# comparability of the variables used to index "a".  We currently only
# handle single-dimensional arrays.

# This script can handle only one-dimensional arrays.  This is OK,
# since dfec can also handle only one-dimensional arrays.

# STRUCTS: Lackwit gets structs about 90% right.  It has problems with
# pointers to structs.

# If two struct pointers are comparable, their respective fields
# should also be comparable.  If s1 cmp s2, then s1->a cmp s2->a.
# However, Lackwit gets this wrong.  Thus, when checking the
# comparability of a structure field, we need to do some work on our
# end.  When checking the comparability of s1->a, also check the
# comparability of s1.  Say s1 is comparable to s2 and s3.  If either
# s2->a or s3->a is in the list of interesting variables (that is,
# variables that appear in the decls file), make s1->a comparable to
# them.

# When determining the comparability of a struct field, lackwit can
# crash if the expression involves pointers (like a->b) and the
# expression is not present in the program text.  However, if the
# expression is not in the program text, it can only be comparable due
# to its parent structure.  Thus, if lackwit crashes on a struct
# expression, just assume it returned no comparability information,
# and use the comparability of its parent.

# lh adds _element and _index variables for pointers to and arrays of
# structs, just like it does for scalars.  When determining the
# comparability of a struct field, lackwit will return something like
# "a_element.b", but it might *not* return "a->b" as well.  Thus,
# results of the form "a_element.b" are converted to "a->b".

use English;
use strict;
$WARNING = 1;
use vars qw(%cache %interesting_vars @compar_lists);

my $element_suffix = "_element";
my $index_suffix = "_index";
my $dummy_return = "lh_return_value";

if (@ARGV != 2) {
  die "Usage: lwpp.pl <lackwitdb> <filename.decls>\n";
}

my ($lackwitdb, $outfn) = @ARGV;

# Check args
-d $lackwitdb or die "$lackwitdb is not a directory\n";
-e "$lackwitdb/globals.db" or die "$lackwitdb/globals.db does not exist\n";
-e $outfn or die "decls file $outfn does not exist\n";

# Exit if the decls file is empty.
if (-z $outfn) {
  print STDERR "Warning: decls file $outfn is empty\n";
  exit 0;
}

# Check that LACKWIT_HOME is set correctly
my $lackwit_home = $ENV{LACKWIT_HOME};
-e "$lackwit_home/bin/Lackwit"
  or die "Environment variable LACKWIT_HOME is not set correctly\n";

$ENV{LACKWITDB} = $lackwitdb;
$ENV{PATH} = "$lackwit_home/bin:" . $ENV{PATH};

my $backupdecls = $outfn . ".nonlackwit";
system("mv -f $outfn $backupdecls")
    and die "Couldn't back up $outfn!\n";
open DECLS, $backupdecls or die "Can't open $backupdecls: $!\n";

sub die_and_restore {
    my $diemsg = shift;
    `mv -f $backupdecls $outfn`;
    die $diemsg;
}

my $saw_comparability = 0;
my $first_line = <DECLS>;
if ($first_line =~ /VarComparability/) {
  my $second_line = <DECLS>;
  if ($second_line =~ /implicit/) {
    $saw_comparability = 1;
    die_and_restore "Invalid declaration file.  " .
      "Perhaps it has already been processed.\n";
  } elsif ($second_line =~ /none/) {
    # Old file was "none", new file will be "implicit"
    $saw_comparability = 1;
  }
  <DECLS>; # skip blank line
}

if (!$saw_comparability) {
  # reset the filehandle position
  seek(DECLS, 0, 0);
}

my $lwpp_mode = "robust";
if (exists $ENV{"LWPP_MODE"}) {
  $lwpp_mode = $ENV{"LWPP_MODE"};
}

if ($lwpp_mode eq "fast") {
  start_lackwit();
}

open OUT, ">$outfn" or die_and_restore "Can't open $outfn for write: $!\n";
print OUT "VarComparability\n";
print OUT "implicit\n";
print OUT "\n";

# maps explicit types (a string of space-separated comparable
# variables) to implicit types (integers)
my %explicit_to_implicit = ();

while (<DECLS>) {
  print OUT;

  if (/DECLARE/) {

    my $ppt = <DECLS>;
    chomp $ppt;
    print OUT "$ppt\n";
    $ppt =~ /^.*\.(.*)\(/;
    my $function = $1;

    # location in the file where the variable declarations start
    my $start_vars = tell DECLS;

    # variables at this program point that Daikon finds interesting,
    # namely those appearing in the decls file.  This includes
    # parameters and global variables.
    %interesting_vars = ();

    # list of lists of comparable variables at this program point.
    # includes uninteresting variables, which will be filtered out later
    @compar_lists = ();

    # read the whole program point, creating the hash of interesting
    # variables and the list of lists of comparable variables
    get_vars_and_comp($function);

    # combine the lists with common elements
    @compar_lists = disjoint_lists(@compar_lists);

    # remove all elements that *aren't* interesting
    @compar_lists = filter_lists_by_hash(\@compar_lists, \%interesting_vars);

    # reset the file pointer to the start of the ppt
    seek DECLS, $start_vars, 0;

    # read the program point, printing the variables and implicit
    # comparability types to the output file
    print_vars_and_comp();
  }
  print OUT "\n";
}

print OUT "# Implicit Type to Explicit Type\n";
foreach my $implicit_type (sort {$a <=> $b;} values %explicit_to_implicit) {
  my %explicit_types = reverse %explicit_to_implicit;
  my $explicit_type = $explicit_types{$implicit_type};
  printf OUT "# %3s : $explicit_type\n", $implicit_type;
}

if ($lwpp_mode eq "fast") {
  stop_lackwit();
}

# finished processing
exit(0);


###########################################################################
### Subroutines
###

# read the whole program point, creating the hash of interesting
# variables and the list of lists of comparable variables
sub get_vars_and_comp {
  my ($function) = @_;

  while (my $variable = <DECLS>) {
    last if ((!$variable) || ($variable =~ /^\s+$/));
    chomp $variable;

    $variable = transform($variable);

    my $declared_type = <DECLS>;
    my $rep_type = <DECLS>;
    my $old_comparability = <DECLS>;

    if (is_array_element($variable)) {
      my $array_base = array_base($variable);

      my $element_variable = $array_base . $element_suffix;
      $interesting_vars{$element_variable} = 1;
      my %element_comparable_variables =
        get_comparable_variables($element_variable, $function);
      push @compar_lists, [keys %element_comparable_variables];

      my $index_variable = $array_base . $index_suffix;
      $interesting_vars{$index_variable} = 1;
      my %index_comparable_variables =
        get_comparable_variables($index_variable, $function);
      push @compar_lists, [keys %index_comparable_variables];
    } else {
      $interesting_vars{$variable} = 1;
      my %comparable_variables =
          get_comparable_variables($variable, $function);
      push @compar_lists, [keys %comparable_variables];
    }
  }
}


# read the program point, printing the variables and implicit
# comparability types to the output file
sub print_vars_and_comp {
  my ($function) = @_;

  # process the variable declarations, one at a time
  while (my $variable = <DECLS>) {
    last if ((!$variable) || ($variable =~ /^\s+$/));

    chomp $variable;
    my $declared_type = <DECLS>;
    chomp $declared_type;
    my $representation_type = <DECLS>;
    chomp $representation_type;

    print OUT "$variable\n";
    print OUT "$declared_type\n";
    print OUT "$representation_type\n";

    # throw away old comparability information
    <DECLS>;

    $variable = transform($variable);

    if (is_array_element($variable)) {
      my $array_base = array_base($variable);
      my $element_variable = $array_base . $element_suffix;
      my $index_variable = $array_base . $index_suffix;
      my $elem_imp_type = implicit_type($element_variable);
      my $index_imp_type = implicit_type($index_variable);
      print OUT "$elem_imp_type\[$index_imp_type\]\n";
    } else {
      my $imp_type = implicit_type($variable);
      print OUT "$imp_type\n";
    }
  }
}


#  For array elements, dfec outputs "foo[]".  Lackwit accepts as input
#  "foo[]" and "foo[0]", but outputs "foo[0]".  Array elements output
#  by Lackwit are immediately converted to the "foo_element" form.
#  is_array_element and array_base are only called on variables from
#  the decls file, so they only need to handle the "foo[]" case.
sub is_array_element {
  my ($variable) = @_;
  return $variable =~ /\[\]$/;
}
sub array_base {
  my ($variable) = @_;
  $variable =~ s/\[\]$//;
  return $variable;
}

# returns true if the variable is a struct field
sub is_struct_field {
  my ($variable) = @_;
  return $variable =~ /->/ || $variable =~ /\./;
}


# Returns the list of comparable variables for the specified variable
# in the specified function.  Includes possibly uninteresting
# variables.
sub get_comparable_variables {
  my ($variable, $function) = @_;

  my %comparable_variables = ();

  # every variable is comparable to itself
  $comparable_variables{$variable} = 1;

  my $lackwit_results = lackwit($function, $variable);

  foreach (split /\n/, $lackwit_results) {
    chomp;

    # we are looking for lines of the form "(filename:line) variable".
    # skip lines not matching this description.
    next if not /^\(.*\) (.*)$/;

    my $comparable = $1;

    # skip type-cast variables ("{") and parameters of other
    # functions ("@")
    next if ($comparable =~ /\{|@/);

    # skip function static variables ("#")
    next if ($comparable =~ /\#/);

    # If the variable name contains a colon, it is either local to
    # a function, or a function parameter.  It will be of the
    # format "function:variable".  If the name does not contain a
    # colon, it is a global variable.
    my $comparable_variable;
    if ($comparable =~ /^(.*):(.*)$/) {
      my $comparable_function = $1;
      $comparable_variable = $2;

      # skip variables in other functions
      next if ($comparable_function ne $function);
    } else {
      $comparable_variable = $comparable;
    }

    # change array[0] to array_element
    $comparable_variable =~ s/\[0\]/$element_suffix/;

    # fields of struct array elements are in the trace file with an "->"
    # convert "a_element.b_element.c->d" to "a->b->c->d"
    $comparable_variable =~ s/$element_suffix\./->/g;

    $comparable_variables{$comparable_variable} = 1;

    # an array element may be represented in the trace file as a pointer
    if ($comparable_variable =~ /^(.*)$element_suffix/) {
      my $array_base = $1;
      my $pointer = "*$array_base";
      $comparable_variables{$pointer} = 1;
    }
  }

  # If the variable is an array element, we need to check the
  # comparability of the array itself.  If the array is comparable to
  # another array, the elements of the arrays should be comparable as
  # well.
  if ($variable =~ /^(.*)$element_suffix$/) {
    my $array_base = $1;
    my %base_comp_var =
      get_comparable_variables($array_base, $function);
    foreach my $base_comp_var (keys %base_comp_var) {
      my $elem_comp_var = $base_comp_var . $element_suffix;
      $comparable_variables{$elem_comp_var} = 1;
    }
  }

  # If the variable is an array index, we need to check the
  # comparability of the array itself.  If the array is comparable to
  # another array, the indices of the arrays should be comparable as
  # well.
  if ($variable =~ /^(.*)$index_suffix$/) {
    my $pointer_var = $1;
    my %pointer_comp_var =
      get_comparable_variables($pointer_var, $function);
    foreach my $pointer_comp_var (keys %pointer_comp_var) {
      my $index_comp_var = $pointer_comp_var . $index_suffix;
      $comparable_variables{$index_comp_var} = 1;
    }
  }

  # If the variable is a field of a structure, we need to check the
  # comparability of its parent structure.  If the parent is
  # comparable to other structures, we need to add the corresponding
  # fields of those structures.  This is done recursively, to handle
  # multiply-nested structures like "a.b.c.d"
  if (is_struct_field($variable)) {
    my ($struct, $field) = split_struct($variable);
    my %parent_comp_var =
      get_comparable_variables($struct, $function);
    foreach my $parent_comp_var (keys %parent_comp_var) {
      my $field_comp_var = $parent_comp_var . $field;
      $comparable_variables{$field_comp_var} = 1;
    }
  }

  return %comparable_variables;
}


# Splits a structure field variable into the parent structure and the
# field.  The field accessor is appended to the front of the field.
# For example:
# get_struct("a.b") = ("a", ".b")
# get_struct("a.b.c.d") = ("a.b.c", ".d")
# get_struct("a.b->c") = ("a.b", "->c")
sub split_struct {
  my ($variable) = @_;
  $variable =~ /(.*)(\.|->)([^\.-]*)$/;
  return ($1, $2.$3);
}


# Returns the string output by BackEnd when queried for the specified
# variable and function.  Memoizes calls to BackEnd to improve
# performance.
sub lackwit {
  my ($variable, $function) = @_;

  my $key = "$variable $function";
  if (exists $cache{$key}) {
    return ${$cache{$key}};
  }

  my $retval;
  if ($lwpp_mode eq "fast") {
    $retval = _lackwit_fast(@_);
  } else {
    $retval = _lackwit_robust(@_);
  }
  $cache{$key} = \$retval;
  for my $line (split /\n/, $retval) {
    if ($line =~ /^\(.*\) ([^:]+):([^:]+)$/) {
      my($other_func, $other_var) = ($1, $2);
      my $key = "$other_var $other_func";
      if (not exists $cache{$key}) {
        $cache{$key} = \$retval;
      }
    }
  }
  return $retval;
}

use IPC::Open2 'open2';
my($lackwit_pid, $lackwit_in_fh, $lackwit_out_fh);

sub start_lackwit {
  $lackwit_pid = open2($lackwit_out_fh, $lackwit_in_fh,
                       "BackEnd -batch");
}

sub stop_lackwit {
  print $lackwit_in_fh "quit\n";
  waitpid $lackwit_pid, 0;
}

sub _lackwit_fast {
  my ($function, $variable) = @_;
  while (my $line = <$lackwit_out_fh>) {
    last if $line eq "<<READY FOR COMMAND>>\n";
  }
  print $lackwit_in_fh "searchlocal $function:$variable -all\n";
  my @lines;
  while (my $line = <$lackwit_out_fh>) {
    last if $line eq "<<END OF RESPONSE>>\n";
    push @lines, $line unless $line =~ /[#\@\{]/;
  }
  return join("", @lines);
}

sub _lackwit_robust {
  my ($function, $variable) = @_;
  # On Solaris, "sh -c" is required to prevent the 'Segmentation Fault'
  # error message from going to standard error.  For some reason, in
  # Solaris, segfaults bypass stderr redirection in an interactive shell.
  my $result =
      `echo "searchlocal $function:$variable -all" | sh -c BackEnd 2>&1`;
  if ($CHILD_ERROR != 0) {
    if (is_struct_field($variable)) {
      return "";
    } else {
#      die_and_restore "FAILURE: BackEnd failed on $function:$variable";
      print "warning: BackEnd failed on $function:$variable.  continuing.\n";
      return "";
    }
  }
  return $result;
}


# returns the implicit type of a variable, as an integer
sub implicit_type {
  my ($var) = @_;
  my @explicit_type = explicit_type($var);

  my $key = join ' ', (sort @explicit_type);

  if (not exists $explicit_to_implicit{$key}) {
    my @types = sort {$a <=> $b;} values %explicit_to_implicit;
    if (my $maximum_type = pop @types) {
      $explicit_to_implicit{$key} = $maximum_type + 1;
    } else {
      $explicit_to_implicit{$key} = 1;
    }
  }
  return $explicit_to_implicit{$key};
}


# returns the explicit type of a variable, as a list of other
# variables
sub explicit_type {
  my ($var) = @_;
  foreach my $list_ref (@compar_lists) {
    my @compar_list = @{$list_ref};
    if (grep {$_ eq $var} @compar_list) {
      return @compar_list;
    }
  }
}


# transforms a variable read from a decls file into a variable that
# lackwit can process
sub transform {
  my ($var) = @_;

  # change "return" to "$dummy_return" when appropriate
  if ($var eq "return" || $var eq "return[]" ||
      $var =~ /^return->/ || $var =~ /^return\./) {
    $var =~ s/^return/$dummy_return/;
  }

  # remove the leading "::" from global variables
  $var =~ s/^:://;

  return $var;
}


# Given a list of list refs, unifies the lists with common elements.
# Returns a list of list refs, such that the sublists are disjoint.
sub disjoint_lists {
  my (@lists) = @_;
  my $cur_list_index = 0;
  while ($cur_list_index < @lists) {
    my $cur_list_ref = $lists[$cur_list_index];
    my $cur_elem_index = 0;
    while ($cur_elem_index < @{$cur_list_ref}) {
      my $cur_elem = $cur_list_ref->[$cur_elem_index];
      my $other_list_index = $cur_list_index + 1;
      while ($other_list_index < @lists) {
        my $other_list_ref = $lists[$other_list_index];
        my $other_list_removed = 0;
        if (grep {$_ eq $cur_elem} @{$other_list_ref}) {
          unify_lists($cur_list_ref, $other_list_ref);
          # remove the other list
          splice @lists, $other_list_index, 1;
          $other_list_removed = 1;
        }
        if (! $other_list_removed) {
          $other_list_index++;
        }
      }
      $cur_elem_index++;
    }
    $cur_list_index++;
  }
  return @lists;
}


# Takes two list references as arguments.  Adds each element of the
# second list to the end of the first, if the element is not already
# in the first list
sub unify_lists {
  my ($list_ref1, $list_ref2) = @_;
  foreach my $elem (@{$list_ref2}) {
    if (! (grep {$_ eq $elem} @{$list_ref1})) {
      push @{$list_ref1}, $elem;
    }
  }
}


# takes a list of lists and a hash.
# filters each sublist, keeping only the elements present in the hash.
# returns the new list of lists
sub filter_lists_by_hash {
  my ($lists_ref, $hash_ref) = @_;
  my @new_lists = ();
  foreach my $list_ref (@{$lists_ref}) {
    my @list = @{$list_ref};
    my @new_list = grep {exists $hash_ref->{$_}} @list;
    push @new_lists, \@new_list;
  }
  return @new_lists;
}
