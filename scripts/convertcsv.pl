#!/usr/bin/env perl

# This program converts a csv file (that is, a file
# in which values are separated by commas) into
# a .dtrace file (and a .decls file) to be used by  Daikon.
# @author: Cemal Akcaba <akcabac@mit.edu>
# @date: 09:56 PM Tue Apr 22
# USAGE: ./convertcsv.pl [options] filename.csv
# OUTPUT: filename.dtrace, filename.decls

use English;
use strict;
use Text::CSV;
$WARNING=1;

my $csv; # csv object

my $USAGE =
  "Usage: ./convertcsv.pl [options] <inputfilename>
  Options:
     -m [behavior]:  behavior for missing values in the csv file
        -m nonsensical : use Daikon \"nonsensical\" values.
        -m old : use the last non-missing value of the variable (*DEFAULT*).
        -m interpolate : linearly interpolate between non-missing values.
                         If a .csv file contains variables of String type,
                         this option will use the last non-missing value
                         for all String variables.
        -m zero : use zero

  Output: If <inputfilename> has a \".csv\" extension, creates
          a declaration file and a data trace file named by
          substituting the \".csv\" extension by \".decls\"
          and \".dtrace\" respectively.
          If <inputfilename> does not have \".csv\" extension,
          creates a declaration file and a data trace file by
          appending \".decls\" and \".dtrace\" to <inputfilename>.

";

# number of input arguments.
my $noOfArgs = @ARGV ;

# by default insert the last value seen for missing values
my $status = "old";

# name of the csv file.
my $inputfilename;

if (($noOfArgs != 1) && ($noOfArgs != 3)) {
    # Invalid number of arguments; print the correct usage.
    print $USAGE;
    exit();
}
if ($noOfArgs == 3) {
    my $input1 = $ARGV[0]." ".$ARGV[1]." ".$ARGV[2];
    if ($input1 =~ /((-m )(zero|interpolate|nonsensical|old)(.+))/) {
        $inputfilename = $ARGV[2];
        $status = $ARGV[1];
    }
    elsif ($input1 =~ /(.+(-m )(zero|interpolate|nonsensical|old))/) {
        $inputfilename = $ARGV[0];
        $status = $ARGV[2];
    }
    else {
        print "Invalid flags $ARGV[0] $ARGV[1] $ARGV[2] \n";
        exit;
    }}
elsif ($noOfArgs == 1) {
    $inputfilename = $ARGV[0];
}

# Create output filenames by using the inputfilename.
# Replaces every occurence of ".csv" by ".dtrace"
# and ".decls" if <inputfilename> contains ".csv".
# Appends ".decls", ".dtrace" to the <inputfilename>
# if input filename does not have
# ".csv" extension.

my $outputfilename = $inputfilename;
my $outputdecfilename = $inputfilename;

if ($outputfilename =~ /\.csv/) {
    $outputfilename =~ s/\.csv/\.dtrace/;
    # creates the dtrace file name.
    $outputdecfilename =~ s/\.csv/\.decls/;
    # creates the declaration file name.
}
else {
    $outputfilename = $inputfilename.".dtrace";
    $outputdecfilename = $inputfilename.".decls";
}

open (CSVHANDLE,$inputfilename)||
    die("Could not open $inputfilename for input.");
open (DTRACEHANDLE, ">".$outputfilename) ||
    die("Could not open $outputfilename for output.");
open (DECLSHANDLE, ">".$outputdecfilename) ||
    die("Could not open $outputdecfilename for output.");

my $zerovalue = "0"."\n"."1"."\n";
my $nonsensicalvalue = "nonsensical"."\n"."2"."\n";


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

my %variableArray;


# utility variables needed for iterations etc.

my $var;my $i; my $j; my $k; my $n ;my $m; my $z;
my $programpoint;
my $length;

# These are just variables used to store
# strings that will be written to the output file.
my $dummy;
my $decls;
my $replacement;


my $y0; my $y1; my $gradient; my $yintercept;


# Now parse the input file and create the declartions and dtrace files.
$csv = Text::CSV->new();

my $csvstatus;


while(<CSVHANDLE>) {
    # open the input file.
    if ($firstpass == 1) {

        chomp ($_);     # remove end of line character.
        $_ =~ s/,$//;   # remove commas at the end of each line in csv file.

        $csvstatus = $csv->parse($_);# parse a CSV string into fields
        @variablenames = $csv->fields(); # get the parsed fields

        $i = 0;
        foreach $var (@variablenames) {
            # Remove spaces, double quotes,slashes from the names of variables.
            $variablenames[$i] =~ s/ /_/g;
            $variablenames[$i] =~ s/\./_/g;
            $variablenames[$i] =~ s/(\"|-|\/)/_/g;
            $variablenames[$i] =~ s/(\(|\))//g;
            $variablenames[$i] =~ s/(\[|\])//g;
            $variablenames[$i] =~ s/(\<|\>)//g;
            $prevvalues[$i] = 0; # initialize all previous values to 0
            $i += 1;
        }
        $firstpass = 0;
    }
    else {
        $i = 0;
        chomp ($_);

        # if there is an extra comma at the end of the line,
        # remove it.
        $_ =~ s/,$//g;

        if ($_ ne "") {
            $csvstatus = $csv->parse($_);# parse a CSV string into fields
                @variables = $csv->fields(); # get the parsed fields


            $length = @variablenames;

            # There might be an issue if the csv file is
            # not well formed. For example, if there are
            # more variable names than there are values
            # in a line, append ""'s to fill in for the
            # missing values.

            my $varlength = @variables;
            if ($length > $varlength) {
                for ($z = $varlength; $z < $length; $z++){
                    $variables[$z] = "";
                }}

            $programpoint = "aprogram.point".":::"."POINT";
            $dummy = $programpoint."\n";
            $decls = $programpoint."\n";

            if ($decl == 1) {
                print DECLSHANDLE "DECLARE\n";
            }

            for ($j = 0; $j<$length; $j++)
            {
                if ($variables[$j] eq "") {
                    if ($status  =~ m/old/) {
                        if (isnumber($prevvalues[$i])) {
                            $replacement = $prevvalues[$i]."\n"."1"."\n";
                        }
                        else {
                            $replacement = "\"".$prevvalues[$i]."\"\n"."1"."\n";
                        }}
                     elsif($status =~ m/zero/) {
                         if (isnumber($prevvalues[$i])){
                             $replacement = $zerovalue;
                         }
                         else {
                             $replacement = "\"0\"";
                         }}
                    elsif($status =~ /nonsensical/) {
                        $replacement = $nonsensicalvalue;
                    }
                    elsif($status =~ /interpolate/) {
                        $variableArray{$j}[$counter] = $variables[$j];
                        $replacement = "";
                    }
                    $dummy = $dummy.$variablenames[$i]."\n".$replacement;
                }
                else {
                    # capture required values for interpolation.
                    $variableArray{$j}[$counter] = $variables[$j];
                    if (isnumber($variables[$j])) {
                        $dummy = $dummy.$variablenames[$i].
                            "\n".$variables[$j]."\n"."1"."\n";
                    }
                    else {
                        $dummy = $dummy.$variablenames[$i].
                            "\n\"".$variables[$j]."\"\n"."1"."\n";
                    }
                    $prevvalues[$i]  = $variables[$j];
                }
                print DTRACEHANDLE $dummy;
                if ($decl == 1) {
                    if (isnumber($variables[$j])) {
                        $decls = $decls.$variablenames[$i].
                            "\n"."double\n"."double\n1\n";
                    }
                    else {
                        $decls = $decls.$variablenames[$i].
                            "\n"."String\n"."String\n1\n";
                    }
                    print DECLSHANDLE $decls;
                }
                $i += 1;
                $dummy = "";
                $decls = "";
            }
            print DTRACEHANDLE "\n";

            if ($decl == 1) {
                print DECLSHANDLE "\n";
                $decl = 0;
            }
            $counter += 1;
        }}}

close(DECLSHANDLE);
close(CSVHANDLE);
close(DTRACEHANDLE);

# interpolation requires extra work, as we first need
# to find new values for missing values, then write
# them into the file.

if ($status =~ /interpolate/) {
    interpolate();
}


# This subroutine carries out linear interpolation to
# fill in the missing values in the csv file. Assumes
# the initial value of a variable is zero, if it is not
# initialized. If the last n values of a variable is missing,
# the procedure inserts the last non-missing value for
# all n values.

sub interpolate()
{
    open (DTRACEHANDLE, ">".$outputfilename) ||
        die("Could not open $outputfilename for output.");

    for ($k = 0; $k < $counter; $k++) {
        $dummy = "\n".$programpoint."\n";
        for ($j = 0; $j < $length; $j++) {
            if ($variableArray{$j}[$k] eq "") {
                for ($m=1; $m< $counter-$k; $m++) {
                    if ($variableArray{$j}[$k+$m] eq "") {
                    }
                    else {
                        for ($n=0; $n<$m; $n++) {
                            if (isnumber($variableArray{$j}[$k+$n-1])) {
                                $y1 = $variableArray{$j}[$k+$m];
                                $y0 = $variableArray{$j}[$k-1+$n];
                                if ($y1 eq "") {
                                    $y1 = 0;
                                }
                                if ($y0 eq "") {
                                    $y0 = 0;
                                }
                                $gradient = ($y1-$y0)/($m+1-$n);
                                $yintercept = $y1-$gradient*($k+$m);
                                $variableArray{$j}[$k+$n] = $gradient*
                                    ($k+$n)+$yintercept;
                            }
                            else {
                                # if not a number cannot interpolate;
                                # just copy the previous value.
                                $variableArray{$j}[$k+$n] =
                                    $variableArray{$j}[$k+$n-1];
                            }}
                        $m = $counter;
                    }}
                if ($m == $counter-$k) {
                    $variableArray{$j}[$k] = $prevvalues[$j];
                }}
            if (isnumber($variableArray{$j}[$k])) {
                # store it as a number
                $dummy = $dummy.$variablenames[$j]."\n".
                    $variableArray{$j}[$k]."\n"."1"."\n";
            }
            else {
                # not a number so store it as a string.
                $dummy = $dummy.$variablenames[$j].
                    "\n\"".$variableArray{$j}[$k]."\"\n"."1"."\n";
            }
            print DTRACEHANDLE $dummy;
            $dummy = "";
        }}
    close(DTRACEHANDLE);
}



# isnumber(Object)
# Returns 0 if passed Object is not a number.
# Returns 1 if passed Object is a number (integer, double etc.)
sub isnumber{
    my ($num) = @_;
    # if it contains anything apart from
    # a "." and [0-9], it can't be a number.

    if ($num =~ /[^0-9\.]/ ) {
        return 0;
    }
    # if it contains two dots, it can't
    # be a number either.
    elsif ($num =~ /\..*\./) {
        return 0;
    }
    else {
        return 1;
    }}


