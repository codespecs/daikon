: # Use -*- Perl -*- without knowing its path
  eval 'exec perl -S -w $0 "$@"'
  if 0;

# Annotate a declaration file with lackwit comparability information
# Assumes the lackwit database has already been created, and the
# necessary environment variables have been set.

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
    $ppt =~ /.*-(.*):::/;
    $function = $1;

    # process the variable declarations, one at a time
    while($variable = shift @ppt_declaration) {
      @comparable_variables = ();
      
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

        # skip type-cast variables ("{") and parameters of other
        # functions ("@")
        next if ($comparable_variable =~ /\{|@/);
        
        # skip uninteresting variables
        next if not $interesting_variables{$comparable_variable};
        
        push @comparable_variables, $comparable_variable;        
      }
      
      print "(@comparable_variables)";

      # if the declared type is an array, print the index information
      $index = 1;
      while ($declared_type =~ /\[\]/g) {
        print "[(" . $variable . "-index" . $index . ")]";
        $index++;
      }
      
      print "\n"

    }
  }
  print "\n";
}
