: # Use -*- Perl -*- without knowing its path
  eval 'exec perl -S -w $0 "$@"'
  if 0;

# Annotate a declaration file with implicit lackwit comparability information.
# Prints the new declaration file to STDOUT.  Assumes:
#   - The lackwit database has already been created
#   - The LACKWITDB environment variable has been set
#   - The BackEnd executable is in your path

# Essentially a Perl port of a similar C++ program written by Adam
# Czeisler

$ELEMENT_SUFFIX = "_element";
$INDEX_SUFFIX = "_index_";

if ($#ARGV != 0) {
  die "Usage: lwpp.pl <filename.decls>\n";
}

$DECLS = $ARGV[0];
open DECLS or die "Can't open $DECLS: $!\n";

print "VarComparability\n";
print "implicit\n";
print "\n";

# maps explicit types (a string of space-separated comparable
# variables) to implicit types (integers)
%implicit_types = ();

while (<DECLS>) {
  print;

  if (/DECLARE/) {
    
    # show progress
    print STDERR '.';

    # variables at this program point that Daikon finds interesting,
    # namely parameters and global variables
    %interesting_variables = ();

    # read the whole paragraph into a string, excluding the blank line
    $ppt_declaration = <DECLS>;
    while (1) {
      $variable = <DECLS>;
      last if ((!$variable) || ($variable =~ /^\s+$/));

      $ppt_declaration .= $variable;

      chomp $variable;
      if (is_array_element($variable)) {
        add_array_variables_to_interesting_variables($variable);
      } else {
        $interesting_variables{$variable} = 1;
      }

      # read the next three lines
      $ppt_declaration .= <DECLS>;
      $ppt_declaration .= <DECLS>;
      $ppt_declaration .= <DECLS>;
    }

    
    @ppt_declaration = split(/\n/, $ppt_declaration);
    $ppt = shift @ppt_declaration;

    $ppt =~ /^.*\.(.*)\(/;
    $function = $1;

    print "$ppt\n";

    # process the variable declarations, one at a time
    while($variable = shift @ppt_declaration) {
      $declared_type = shift @ppt_declaration;
      $representation_type = shift @ppt_declaration;

      # throw away the old comparability information
      shift @ppt_declaration;

      print "$variable\n";
      print "$declared_type\n";
      print "$representation_type\n";
      
      if (is_array_element($variable)) {
        print_implicit_type($variable, $function);
        print_array_index_types($variable, $function);
      } else {
        print_implicit_type($variable, $function);
      }
      
      print "\n";
    }
  }
  print "\n";
}

print STDERR "\n";

print "# Implicit Type to Explicit Type\n";
foreach $implicit_type (sort numerically values %implicit_types) {
  %explicit_types = reverse %implicit_types;
  $explicit_type = $explicit_types{$implicit_type};
  printf "# %3s : $explicit_type\n", $implicit_type;
}



# to sort numbers numerically
sub numerically { $a <=> $b; }


sub is_array_element {
  my ($variable) = @_;
  return $variable =~ /\[\]/ || $variable =~ /\[0\]/;
}


sub get_comparable_variables {
  my ($variable, $function) = @_;

  my %comparable_variables = ();

  # every variable is comparable to itself, except array elements,
  # which are comparable to their _element variable
  if (is_array_element($variable)) {
    my $array_base = $variable;
    $array_base =~ s/\[\]//;
    my $element_variable = $array_base . $ELEMENT_SUFFIX;
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
      $comparable_variable =~ s/(\[0\])+/$ELEMENT_SUFFIX/;
    }

    if (exists $interesting_variables{$comparable_variable}) {
      $comparable_variables{$comparable_variable} = 1;
    } elsif ($comparable_variable =~ /^([^\[]*)(\[\])*$/) {
      # an array may be represented in the trace file as a pointer
      my $array_base = $1;
      my $pointer = "*$array_base";
      if (exists $interesting_variables{$pointer}) {
        $comparable_variables{$pointer} = 1;
      }
    }
  }

  return (join ' ', sort keys %comparable_variables);
}


sub add_array_variables_to_interesting_variables {
  my ($variable) = @_;
  my $array_base = $variable;
  $array_base =~ s/\[\]//;
  my $element_variable = $array_base . $ELEMENT_SUFFIX;
  $interesting_variables{$element_variable} = 1;
  my $count = 0;
  while ($variable =~ /\[\]/g) {
    my $index_variable = $array_base . $INDEX_SUFFIX . $count;
    $interesting_variables{$index_variable} = 1;
    $count++;
  }
}


sub print_array_index_types {
  my ($variable, $function) = @_;
  my $array_base = $variable;
  $array_base =~ s/\[\]//;
  my $count = 0;
  while ($representation_type =~ /\[\]/g) {
    my $index_variable = $array_base . $INDEX_SUFFIX . $count;
    my $comparable_variables =
      get_comparable_variables($index_variable, $function);
    my $implicit_type = get_implicit_type($comparable_variables);
    
    print "[" . $implicit_type . "]";
    $count++;
  }
}


sub print_implicit_type {
  my ($variable, $function) = @_;
  my $comparable_variables = get_comparable_variables($variable, $function);
  my $implicit_type = get_implicit_type($comparable_variables);
  print $implicit_type;
}


sub get_implicit_type {
  my ($comparable_variables) = @_;
  if (not exists $implicit_types{$comparable_variables}) {
    my @types = sort numerically values %implicit_types;
    if (my $maximum_type = pop @types) {
      $implicit_types{$comparable_variables} = $maximum_type + 1;
    } else {
      $implicit_types{$comparable_variables} = 1;
    }
  }
  return $implicit_types{$comparable_variables};
}
