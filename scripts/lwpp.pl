: # Use -*- Perl -*- without knowing its path
  eval 'exec perl -S -w $0 "$@"'
  if 0;

# Annotate a declaration file with implicit lackwit comparability information.
# Prints the new declaration file to STDOUT.  Assumes:
#   - $LACKWIT_HOME is set to the directory where lackwit is installed
#   - The lackwit database has already been created
#   - The BackEnd executable is in your path

# Perl port of a similar C++ program written by Adam Czeisler

use strict;

my $element_suffix = "_element";
my $index_suffix = "_index";
my $dummy_return = "lh_return_value";

if (@ARGV != 2) {
  die "Usage: lwpp.pl <lackwitdb> <filename.decls>\n";
}

my ($lackwitdb, $decls) = @ARGV;

# Check that LACKWIT_HOME is set correctly
my $lackwit_home = $ENV{LACKWIT_HOME};
-d $lackwit_home or die "LACKWIT_HOME is not set correctly\n";

$ENV{LACKWITDB} = $lackwitdb;
$ENV{PATH} = "$lackwit_home/bin:" . $ENV{PATH};

open DECLS, $decls or die "Can't open $decls: $!\n";

print "VarComparability\n";
print "implicit\n";
print "\n";

# maps explicit types (a string of space-separated comparable
# variables) to implicit types (integers)
my %implicit_types = ();

while (<DECLS>) {
  print;

  if (/DECLARE/) {
    
    # show progress
    print STDERR '.';

    # variables at this program point that Daikon finds interesting,
    # namely parameters and global variables
    my %interesting_variables = ();

    # read the whole paragraph into a string, excluding the blank line
    my $ppt_declaration = <DECLS>;
    while (1) {
      my $variable = <DECLS>;
      last if ((!$variable) || ($variable =~ /^\s+$/));

      $ppt_declaration .= $variable;

      chomp $variable;
      
      $variable = transform($variable);

      if (is_array_element($variable)) {
        add_array_variables_to_interesting_variables($variable,
                                                     \%interesting_variables);
      } else {
        $interesting_variables{$variable} = 1;
      }

      # read the next three lines
      $ppt_declaration .= <DECLS>;
      $ppt_declaration .= <DECLS>;
      $ppt_declaration .= <DECLS>;
    }

    my @ppt_declaration = split(/\n/, $ppt_declaration);
    my $ppt = shift @ppt_declaration;

    $ppt =~ /^.*\.(.*)\(/;
    my $function = $1;

    print "$ppt\n";

    # process the variable declarations, one at a time
    while(my $variable = shift @ppt_declaration) {
      my $declared_type = shift @ppt_declaration;
      my $representation_type = shift @ppt_declaration;

      # throw away the old comparability information
      shift @ppt_declaration;

      print "$variable\n";
      print "$declared_type\n";
      print "$representation_type\n";

      if (is_array_element($variable)) {
        print_implicit_type($variable, $function, \%interesting_variables);
        print_array_index_types($variable, $function, $representation_type,
                                \%interesting_variables);
      } else {
        print_implicit_type($variable, $function, \%interesting_variables);
      }
      
      print "\n";
    }
  }
  print "\n";
}

print STDERR "\n";

print "# Implicit Type to Explicit Type\n";
foreach my $implicit_type (sort {$a <=> $b;} values %implicit_types) {
  my %explicit_types = reverse %implicit_types;
  my $explicit_type = $explicit_types{$implicit_type};
  printf "# %3s : $explicit_type\n", $implicit_type;
}


sub is_array_element {
  my ($variable) = @_;
  return $variable =~ /\[\]/ || $variable =~ /\[0\]/;
}


sub get_comparable_variables {
  my ($variable, $function, $interesting_variables) = @_;

  my %comparable_variables = ();

  # every variable is comparable to itself, except array elements,
  # which are comparable to their _element variable
  if (is_array_element($variable)) {
    my $array_base = $variable;
    $array_base =~ s/\[\]//;
    my $element_variable = $array_base . $element_suffix;
    $comparable_variables{$element_variable} = 1;
  } else {
    $comparable_variables{$variable} = 1;
  }

  my $lackwit_results = 
    `echo "searchlocal $function:$variable -all" | BackEnd 2> /dev/null`;
  
  foreach (split /\n/, $lackwit_results) {
    chomp;
    
    # we are looking for lines of the form "(filename:line) variable".
    # skip lines not matching this description.
    next if not /^\(.*\) (.*)$/;
    
    my $comparable = $1;
    
    # skip type-cast variables ("{") and parameters of other
    # functions ("@")
    next if ($comparable =~ /\{|@/);
    
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
    
    if (is_array_element($comparable_variable)) {
      # change array[0][0]... to array_element
      $comparable_variable =~ s/(\[0\])+/$element_suffix/;
    }

    if (exists $interesting_variables->{$comparable_variable}) {
      $comparable_variables{$comparable_variable} = 1;
    } elsif ($comparable_variable =~ /^(.*)$element_suffix/) {
      # an array element may be represented in the trace file as a pointer
      my $array_base = $1;
      my $pointer = "*$array_base";
      if (exists $interesting_variables->{$pointer}) {
        $comparable_variables{$pointer} = 1;
      }
    }
  }

  return %comparable_variables;
}


sub add_array_variables_to_interesting_variables {
  my ($variable, $interesting_variables) = @_;
  my $array_base = $variable;
  $array_base =~ s/\[\]//;
  my $element_variable = $array_base . $element_suffix;
  $interesting_variables->{$element_variable} = 1;
  if ($variable =~ /\[\]/) {
    my $index_variable = $array_base . $index_suffix;
    $interesting_variables->{$index_variable} = 1;
  }
}


sub print_array_index_types {
  my ($variable, $function, $representation_type, $interesting_variables) = @_;

  $variable = transform($variable);

  my $array_base = $variable;
  $array_base =~ s/\[\]//;

  # Treat String rep type like an array, because the underlying C
  # implementation is actually an array
  if ($representation_type eq "String") {
    $representation_type = $array_base . "[]";
  }

  if ($representation_type =~ /\[\]/) {
    my $index_variable = $array_base . $index_suffix;
    my %comparable_variables = get_comparable_variables($index_variable,
      $function, $interesting_variables);

    if (is_array_element($variable)) {
      my $pointer_var = $variable;
      $pointer_var =~ s/(\[\]|\[0\])//;
      my %pointer_comp_var = get_comparable_variables($pointer_var,
        $function, $interesting_variables);
      foreach my $pointer_comp_var (keys %pointer_comp_var) {
        my $index_comp_var = $pointer_comp_var . $index_suffix;
        if (exists $interesting_variables->{$index_comp_var}) {
          $comparable_variables{$index_comp_var} = 1;
        }
      }
    }

    my $implicit_type = get_implicit_type(%comparable_variables);
    
    print "[" . $implicit_type . "]";
  }
}


sub print_implicit_type {
  my ($variable, $function, $interesting_variables) = @_;

  $variable = transform($variable);

  my %comparable_variables = get_comparable_variables($variable,
    $function, $interesting_variables);

  if (is_array_element($variable)) {
    my $pointer_var = $variable;
    $pointer_var =~ s/(\[\]|\[0\])//;
    my %pointer_comp_var = get_comparable_variables($pointer_var,
      $function, $interesting_variables);
    foreach my $pointer_comp_var (keys %pointer_comp_var) {
      my $elem_comp_var = $pointer_comp_var . $element_suffix;
      if (exists $interesting_variables->{$elem_comp_var}) {
        $comparable_variables{$elem_comp_var} = 1;
      }
    }
  }

  my $implicit_type = get_implicit_type(%comparable_variables);
  print $implicit_type;
}


sub get_implicit_type {
  my (%comparable_variables) = @_;
  my $key = join ' ', (sort keys %comparable_variables);

  if (not exists $implicit_types{$key}) {
    my @types = sort {$a <=> $b;} values %implicit_types;
    if (my $maximum_type = pop @types) {
      $implicit_types{$key} = $maximum_type + 1;
    } else {
      $implicit_types{$key} = 1;
    }
  }
  return $implicit_types{$key};
}


# transforms a variable read from a decls file into a variable that
# lackwit can process
sub transform {
  my ($var) = @_;
  if ($var eq "return" || $var eq "return[]") {
    $var =~ s/return/$dummy_return/;
  }
  
  # remove the leading "::" from global variables
  $var =~ s/^:://;

  return $var;
}
