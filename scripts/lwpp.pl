: # Use -*- Perl -*- without knowing its path
  eval 'exec perl -S -w $0 "$@"'
  if 0;

# Annotate a declaration file with lackwit comparability information.
# Prints the new declaration file to STDOUT.  Assumes:
#   - The lackwit database has already been created
#   - The LACKWITDB environment variable has been set
#   - The BackEnd executable is in your path

# Essentially a Perl port of a similar C++ program written by Adam
# Czeisler

if ($#ARGV != 0) {
  die "Usage: lwpp.pl <filename.decls>\n";
}

$DECLS = $ARGV[0];
open DECLS or die "Can't open $DECLS: $!\n";

print "VarComparability\n";
print "explicit\n";
print "\n";

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
      $interesting_variables{$variable} = 1;

      # read the next three lines
      $ppt_declaration .= <DECLS>;
      $ppt_declaration .= <DECLS>;
      $ppt_declaration .= <DECLS>;
    }
    
    @ppt_declaration = split(/\n/, $ppt_declaration);
    $ppt = shift @ppt_declaration;
    print "$ppt\n";

    $ppt =~ /^.*\.(.*)\(/;
    $function = $1;

    # process the variable declarations, one at a time
    while($variable = shift @ppt_declaration) {
      %comparable_variables = ();

      # every variable is comparable to itself
      # note: Lackwit should provide this information,
      # but sometimes doesn't.  Can't hurt to add it explicitly.
      $comparable_variables{$variable} = 1;
      
      print "$variable\n";
      
      $declared_type = shift @ppt_declaration;
      print "$declared_type\n";

      $representation_type = shift @ppt_declaration;
      print "$representation_type\n";
      
      # throw away the old comparability information
      shift @ppt_declaration;
      
      $lackwit_results = 
        `echo "searchlocal $function:$variable -all" | BackEnd 2> /dev/null`;

      foreach (split /\n/, $lackwit_results) {
        chomp;
        
        # we are looking for lines of the form "(filename:line)
        # variable".  skip lines not matching this
        # description.
        next if not /^\(.*\) (.*)$/;
        
        $comparable = $1;

        # skip type-cast variables ("{") and parameters of other
        # functions ("@")
        next if ($comparable =~ /\{|@/);

        # If the variable name contains a colon, it is either local to
        # a function, or a function parameter.  It will be of the
        # format "function:variable".  If the name does not contain a
        # colon, it is a global variable.
        
        if ($comparable =~ /^(.*):(.*)$/) {
          $comparable_function = $1;
          $comparable_variable = $2;
          
          # skip variables in other functions
          next if ($comparable_function ne $function);
        } else {
          $comparable_variable = $comparable;
        }

        # change array[0] to array[]
        $comparable_variable =~ s/\[0\]/[]/;

        if ($interesting_variables{$comparable_variable}) {
          $comparable_variables{$comparable_variable} = 1;
        } elsif ($comparable_variable =~ /^([^\[]*)(\[\])*$/) {
          # an array may be represented in the trace file as a pointer
          $array_base = $1;
          $pointer = "*$array_base";
          if ($interesting_variables{$pointer}) {
            $comparable_variables{$pointer} = 1;
          }
        }
      }

      @comparable_variables = sort keys %comparable_variables;
      print "(@comparable_variables)";

      # if the representation type is an array, print the index information
      $index = 1;
      while ($representation_type =~ /\[\]/g) {
        print "[(" . $variable . "-index" . $index . ")]";
        $index++;
      }
      
      print "\n"

    }
  }
  print "\n";
}

print STDERR "\n";
