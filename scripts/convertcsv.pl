#!/usr/bin/env perl

# This program converts a csv file (that is, a file
# in which values are separated by commas) into
# a .dtrace file (and a .decls file) to be used by  Daikon.
# @author: Cemal Akcaba <akcabac@mit.edu>
# @date: 01:48 PM Mon May 05
# USAGE: ./convertcsv.pl [options] filename.csv
# OUTPUT: filename.dtrace, filename.decls

use English;
use strict;
use Text::CSV; 
use Getopt::Long;
$WARNING=1;

my $csv; # csv object

my $USAGE =
  "Usage: convertcsv.pl [options] <inputfilename>
  Options:
     -m [behavior]:  behavior for missing values in the csv file
        -m nonsensical : use Daikon \"nonsensical\" values.
        -m old : use the last non-missing value of the variable (*DEFAULT*).
        -m interpolate : linearly interpolate between non-missing values.
                         If a .csv file contains variables of String type,
                         this option will use the last non-missing value 
                         for all String variables.                         
        -m zero : use zero 

     -decl [declarationsfilename]: use the declarations in the  
                          <declarationsfilename> to create the
                          data trace file. No .decls files
                          will be created.
   
  Output: If <inputfilename> has a \".csv\" extension, creates
          a declaration file and a data trace file named by 
          substituting the \".csv\" extension by \".decls\" 
          and \".dtrace\" respectively. 
          If <inputfilename> does not have \".csv\" extension, 
          creates a declaration file and a data trace file by 
          appending \".decls\" and \".dtrace\" to <inputfilename>. 

";

# number of input arguments.
my $noOfArgs = scalar(@ARGV);

# by default insert the last value seen for missing values
my $missingaction = "old";

# variable $missingact is used to capture the input.
my $missingact =""; 

# name of the csv file. 
my $inputfilename;

# name of the provided decls file (if any)
my $declarationfilename = "";

GetOptions( 'decl:s' => \$declarationfilename,
            'm:s' => \$missingact                       
            );

# now, check the value provided to "-m"
if (($missingact eq "interpolate") ||
    ($missingact eq "old") ||
    ($missingact eq "zero") ||
    ($missingact eq "nonsensical")){   
    $missingaction = $missingact;
}
elsif ($missingact eq ""){
    # no value provided, use default $missingaction = old.
    # as set above.
}
else {
    # unknown value given. Exit.
    print "Invalid option for missing values : ".$missingact."\n\n";
    print $USAGE;
    exit(1);
}

# Default program point name. 
my $programpoint = "aprogram.point".":::"."POINT";                   

# variable names from the provided declaration file.
my @actualVarNames; 

# Maps variable names to the index within the csv file.
my %varNameIndex;

# 1 if a declaration file is provided.
# 0 otherwise
my $declarationexists=1;

if ($declarationfilename ne "") {
    # parse provided declaration file
    my @returnvalues = parseDecl($declarationfilename);
    $programpoint = shift(@returnvalues);
    
    # Names of the variables present in the declaration file.
    @actualVarNames = @returnvalues;   
}
else {
    $declarationexists = 0;
}
   

# GetOptions removes all the options and their
# values from @ARGV. So now, check if a <filename> 
# is provided.

# number of input arguments.
my $noOfArgs = scalar(@ARGV);

if ($noOfArgs > 0 ) {
    $inputfilename = $ARGV[0];
}
else {
    print "error: missing input filename.\n" ;
    print $USAGE;
    exit(1)
}



# Create output filenames by using the inputfilename.
# Replaces every occurence of ".csv" by ".dtrace" 
# and ".decls" if <inputfilename> contains ".csv".
# Appends ".decls", ".dtrace" to the <inputfilename>
# if input filename does not have
# ".csv" extension.

# creates the dtrace file name
my $outputfilename = $inputfilename.".dtrace";
$outputfilename =~ s/\.csv\.dtrace$/\.dtrace/;
# creates the declaration file name.
my $outputdecfilename = $inputfilename.".decls"; 
$outputdecfilename =~ s/\.csv\.decls$/\.decls/;


open (CSVHANDLE,$inputfilename)||
    die("Could not open $inputfilename for input.");
open (DTRACEHANDLE, ">".$outputfilename) ||
    die("Could not open $outputfilename for output.");

if ($declarationexists == 0) {
    open (DECLSHANDLE, ">".$outputdecfilename) ||
        die("Could not open $outputdecfilename for output.");
}
my $zerovalue = "0\n1\n";
my $nonsensicalvalue = "nonsensical\n2\n";


my $firstpass = 1;
# used as a boolean to check if
# the firstline of the input file is read.
# the firstline of the input file is assumed to
# contain the variable names.

my $decl = 1;
# flag for including the "DECLARE" statement.
# if this flag is 1, the "DECLARE" statement
# will be included in the beginning of each
# program point in the file.

my $counter = 0;
# counts the number of lines in the
# input file. (hence, the number of program points)


# Arrays for holding variable names, variable values, 
# and values of previous iteration. 
my @variablenames;
my @variables;
my @prevvalues;

# Arrays for holding information needed by 
# interpolation. (e.g. distance between existing
# values of a variable)
# Keys => index of a variable in a line
# Values => arrays containing all instances of a variable 
# (columns in the csv file)  
my %variableArray;


my $length;

# These are just variables used to store
# strings that will be written to the output file.
my $dtraceline;
my $decls;
my $replacement;

# Now parse the input file and create the declartions and dtrace files.
$csv = Text::CSV->new();

my $csvstatus;

$_ = <CSVHANDLE> ;
# capture the variable names in the global 
# variabe @variablenames.
getVariableNames($_);

if ($declarationexists == 0){
    @actualVarNames = @variablenames;
}

my @isNumber;
$length = scalar(@actualVarNames);

for (my $j = 0; $j<$length; $j++) {
    my $activeindex = $varNameIndex{$actualVarNames[$j]};
    $isNumber[$activeindex] = 1;
}


while(<CSVHANDLE>) {
    # open the input file.
    my $i = 0;
    chomp ($_); # remove the end of line.
    
    # if there is an extra comma at the end of the line,
    # remove it.
    $_ =~ s/,$//g;           
    if ($_ ne "") {            
        
        $csvstatus = $csv->parse($_);# parse a CSV string into fields
        if (!$csvstatus) {
            die("Corrupted csv file. Exiting...");
        }
        @variables = $csv->fields(); # get the parsed fields            
        
        
        $length = scalar(@actualVarNames);
        
        # There might be an issue if the csv file is
        # not well formed. For example, if there are 
        # more variable names than there are values
        # in a line, append ""'s to fill in for the
        # missing values.
        # If reverse is true, warn the user, and
        # truncate the line.
        
        my $varlength = scalar(@variables);
        if ($length > $varlength) {
            for (my $z = $varlength; $z < $length; $z++){
                $variables[$z] = "";
            }}
        elsif ($varlength > $length) {            
            print ("WARNING! csv file contains more variable values than declared\nvariables at line". 
                   ($counter+1).". Truncating line ".($counter+1).".\n");
        }
        
        $dtraceline = $programpoint."\n";
        $decls = "DECLARE\n".$programpoint."\n";
        
        for (my $j = 0; $j<$length; $j++)
        {                
            my $activeindex = $varNameIndex{$actualVarNames[$j]};

            if ($variables[$activeindex] eq "") {
                if ($missingaction  eq "old") {
                    if (isnumber($prevvalues[$activeindex])) {
                        $replacement = $prevvalues[$activeindex]."\n"."1"."\n"; 
                    }
                    else {
                        $replacement = "\"".$prevvalues[$activeindex]."\"\n"."1"."\n";
                    }}                   
                elsif($missingaction eq "zero") {
                    if (isnumber($prevvalues[$activeindex])){
                        $replacement = $zerovalue; 
                    }
                    else {
                        $replacement = "\"0\"\n1\n";
                    }}                    
                elsif($missingaction eq "nonsensical") {
                    $replacement = $nonsensicalvalue; 
                }
                elsif($missingaction eq "interpolate") {
                    $variableArray{$activeindex}[$counter] = $variables[$activeindex];
                    $replacement = "";
                }
                $dtraceline = $dtraceline.$variablenames[$activeindex]."\n".$replacement; 
            }
            else {
                # capture required values for interpolation.
                $variableArray{$activeindex}[$counter] = $variables[$activeindex];
                if (isnumber($variables[$activeindex])) {
                    $dtraceline = $dtraceline.$variablenames[$activeindex].
                        "\n".$variables[$activeindex]."\n"."1"."\n";
                }
                else {
                    $dtraceline = $dtraceline.$variablenames[$activeindex].
                        "\n\"".$variables[$activeindex]."\"\n"."1"."\n";
                }
                $prevvalues[$activeindex]  = $variables[$activeindex];
                }
            print DTRACEHANDLE $dtraceline;
 
            if ($decl == 1) {
                if ((isnumber($variables[$activeindex])) || ($variables[$activeindex] eq "")) {
                    
                }
                else {
                    $isNumber[$activeindex] = 0;
                }
                
            }
            $dtraceline = "";
        }

        print DTRACEHANDLE "\n";        
        $counter += 1;            
    }}

for (my $j = 0; $j<$length; $j++) {                
    my $activeindex = $varNameIndex{$actualVarNames[$j]};
    if ($isNumber[$activeindex]) {
        $decls = $decls.$variablenames[$activeindex].
            "\n"."double\n"."double\n1\n";       
    }
    else {
        $decls = $decls.$variablenames[$activeindex].
            "\n"."String\n"."String\n1\n";
    }
    if ($declarationexists == 0) { print DECLSHANDLE $decls; }
    $decls = "";
}


close(DECLSHANDLE);
close(CSVHANDLE);
close(DTRACEHANDLE);

# interpolation requires extra work, as we first need
# to find new values for missing values, then write 
# them into the file.

if ($missingaction =~ /interpolate/) { 
    interpolate(); 
}


# This subroutine carries out linear interpolation to
# fill in the missing values in the csv file. Assumes
# the initial value of a variable is zero, if it is not
# initialized. If the last n values of a variable is missing,
# the procedure inserts the last non-missing value for 
# all n values.

sub interpolate() {
    open (DTRACEHANDLE, ">".$outputfilename) ||
        die("Could not open $outputfilename for output.");
    if ($declarationexists == 0) {
        open (DECLSHANDLE, ">".$outputdecfilename) ||
            die("Could not open $outputdecfilename for output.");
    }

    my $m;    

    my $declsline = "DECLARE\n".$programpoint."\n";
   
    # set flag for creating a declarations file.
    $decl = 1; 
    for (my $k = 0; $k < $counter; $k++) {
        $dtraceline = "\n".$programpoint."\n";        
        for (my $j = 0; $j < $length; $j++) {                             
            my $activeindex = $varNameIndex{$actualVarNames[$j]};
            if ($variableArray{$activeindex}[$k] eq "") {                
                for ($m=1; $m< $counter-$k; $m++) {
                    if ($variableArray{$activeindex}[$k+$m] eq "") { 
                    }
                    else {                      
                        for (my $n=0; $n<$m; $n++) {                            
                            if (isnumber($variableArray{$activeindex}[$k+$n-1]) 
                                || isnumber($variableArray{$activeindex}[$k+$m])) {                                   
                                my $y1 = $variableArray{$activeindex}[$k+$m];
                                my $y0 = $variableArray{$activeindex}[$k-1+$n];
                                if ($y1 eq "") {
                                    $y1 = 0;
                                }
                                if ($y0 eq "") {
                                    $y0 = 0; 
                                }
                                my $gradient = ($y1-$y0)/($m+1-$n);
                                my $yintercept = $y1-$gradient*($k+$m);
                                $variableArray{$activeindex}[$k+$n] = $gradient*
                                    ($k+$n)+$yintercept;
                            }
                            else {
                                # if not a number cannot interpolate;
                                # just copy the previous value.
                                $variableArray{$activeindex}[$k+$n] = 
                                    $variableArray{$activeindex}[$k+$n-1];
                            }}
                        $m = $counter;
                    }}
                if (($m == $counter-$k) && ($variableArray{$activeindex}[$k+$m-1] eq "")) {
                    $variableArray{$activeindex}[$k] = $prevvalues[$activeindex];
                }
            }
            if (isnumber($variableArray{$activeindex}[$k])) {                            
                # store it as a number
                $dtraceline = $dtraceline.$variablenames[$activeindex]."\n".
                    $variableArray{$activeindex}[$k]."\n"."1"."\n";
                if ($decl == 1) {
                    $declsline = $declsline.$variablenames[$activeindex]."\n".
                        "double\ndouble\n1\n"
                    }
            }
            else {              
                # not a number so store it as a string.   
                $dtraceline = $dtraceline.$variablenames[$activeindex].
                    "\n\"".$variableArray{$activeindex}[$k]."\"\n"."1"."\n";
                if ($decl == 1) {
                    $declsline = $declsline.$variablenames[$activeindex]."\n".
                        "String\nString\n1\n"
                    }
            }
            print DTRACEHANDLE $dtraceline;
            if ($declarationexists == 0) {
                print DECLSHANDLE $declsline;
            }
            $dtraceline = "";
            $declsline = "";
        }
        $decl = 0;
    }
    close(DTRACEHANDLE);
    close(DECLSHANDLE);
}


# isnumber(Object)
# Returns 0 if passed Object is not a number.
# Returns 1 if passed Object is a number (integer, double etc.)
sub isnumber{  
    my ($num) = @_;   
    if ($num =~ /^([+-]?)(?=\d|\.\d)\d*(\.\d*)?([Ee]([+-]?\d+))?$/) {
        return 1;
    }
    else {
        return 0;
      } 
}


# Removes spaces, double quotes, slashes, (square|curly|regular) brackets
# from variable names.

sub formatVarNames{
    my ($input) = @_;
    # To facilitate commenting out
    # one or more of these expressions,
    # each one was written in a separate line.
    $input =~ s/ /_/g;
    $input =~ s/\./_/g;
    $input =~ s/(\"|-|\/)/_/g;
    $input =~ s/(\(|\))//g;
    $input =~ s/(\[|\])//g;
    $input =~ s/(\<|\>)//g;
    return $input;
}


# Captures the variable names in the global
# variable @variablenames.
sub getVariableNames{
    $_ = @_[0];
    chomp ($_);     # remove end of line character.
    $_ =~ s/,$//;   # remove commas at the end of each line in 
                    # csv file.            
    
    $csvstatus = $csv->parse($_);# parse a CSV string into fields
       
    if ($csvstatus){
        @variablenames = $csv->fields(); # get the parsed fields        
        my $i = 0;
        foreach my $var (@variablenames) {
            # Remove spaces, double quotes,slashes from the names of variables.
            $variablenames[$i] = formatVarNames($variablenames[$i]);
            $prevvalues[$i] = 0; # initialize all previous values to 0  

            # Generate a hash that maps variable name to the index of 
            # variable.
            $varNameIndex{$variablenames[$i]} = $i;
            $i += 1; 
            
        }}
    else {
        die("Corrupted csv file. Exiting...");
    }}   

# parses the provided declaration file.
# and returns an array containing 
# the program point name and the names of  variables.

sub parseDecl{
    my $inputfile = @_[0];
    open(DECHANDLE, $inputfile) ||
        die("Cannot open declarations file $inputfile");
    $_ = <DECHANDLE>;
    my $programpoint = <DECHANDLE>; 
    chomp($programpoint);
    my $i = 0;
    my @varnames; 
    my @varDecType;
    my @varActType;
    my @varComparable;

    while(<DECHANDLE>) {
        chomp($_);
        $varnames[$i] = $_;
        $varDecType[$i] =<DECHANDLE>;
        $varActType[$i] = <DECHANDLE>;
        $varComparable[$i] = <DECHANDLE>;
        $i = $i +1 ;
    }
    close(DECHANDLE);
    return ($programpoint, @varnames);    
}
