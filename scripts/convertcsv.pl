#!/usr/bin/env perl

# This program converts a csv file (that is, a file
# in which values are separated by commas) into
# a .dtrace file (and a .decls file) to be used by  Daikon.
# @author: Cemal Akcaba <akcabac@mit.edu>
# @date: 09:56 PM Tue Apr 22

# USAGE: ./convertcsv.pl [options] filename.csv
# OUTPUT: filename.dtrace, filename.decls
#

use English;
use strict;
$WARNING=1;

my $USAGE =
  "Usage: ./convertcsv.pl [options] <inputfilename>
  Options:
     -m [behavior]:  behavior for missing values in the csv file
        -m nonsensical : use Daikon \"nonsensical\" values
        -m old : use the last non-missing value of the variable (default).
        -m interpolate : linearly interpolate between non-missing values.
        -m zero : use zero
";

# number of input arguments.
$noOfArgs = @ARGV ;

# by default insert the last value seen for missing values.
my $status = "old";

if (($noOfArgs != 1) && ($noOfArgs != 3)) {
    # Invalid number of arguments; print the correct usage.
    print $USAGE;
    die; }


if ($noOfArgs == 3) {
    $input1 = $ARGV[0]." ".$ARGV[1]." ".$ARGV[2];
    if ($input1 =~ /((-m )(zero|interpolate|nonsensical|old)(.+))/) {
        $inputfilename = $ARGV[2];
        $status = $ARGV[1]; }
    elsif ($input1 =~ /(.+(-m )(zero|interpolate|nonsensical|old))/) {
        $inputfilename = $ARGV[0];
        $status = $ARGV[2]; }
    else {
        print "Invalid flags $ARGV[0] $ARGV[1] $ARGV[2] \n";
        exit;
    }}
elsif ($noOfArgs == 1) {
    $inputfilename = $ARGV[0];
}

# Create output filenames by using the inputfilename.
# Replaces the every occurence of ".csv" by ".dtrace" and ".decls" if input filename contains ".csv".
# Appends ".decls", ".dtrace" to the input filename if input filename does not have
# ".csv" extension.

$outputfilename = $inputfilename;
$outputdecfilename = $inputfilename;
if ($outputfilename =~ /\.csv/) {
    $outputfilename =~ s/\.csv/\.dtrace/;
    # creates the dtrace file name.
    $outputdecfilename =~ s/\.csv/\.decls/;
    # creates the declaration file name.
}
else {
    $outputfilename = $inputfilename.".dtrace";
    $outputdecfilename = $inputfilename.".decls"; }


open (CSVHANDLE,$inputfilename)||
    die("Could not open $inputfilename for input.");
open (DTRACEHANDLE, ">".$outputfilename) ||
    die("Could not open $outputfilename for output.");
open (DECLSHANDLE, ">".$outputdecfilename) ||
    die("Could not open $outputdecfilename for output.");



my $zerovalue = "0"."\n"."1"."\n";
my $nonsensicalvalue = "nonsensical"."\n"."2"."\n";



$firstpass = 1;
# used as a boolean to check if
# the firstline of the input file is read.
# the firstline of the input file is assumed to
# contain the variable names.


my $decl = 1;
# flag for including the "DECLARE" statement.
# if this flag is 1, the "DECLARE" statement
# will be included in the beginning of each
# program point in the file.


$counter = 0;
# counts the number of lines in the
# input file. (hence, the number of program points)

# Now parse the input file and create the declartions and dtrace files.

while(<CSVHANDLE>) {
    # open the input file.
    if ($firstpass == 1) {
        chomp ($_); # remove end-line characters
        chop($_);   # remove the ',' at the end of each line in csv file.

        @variablenames = split(/,/,$_); # now get the variable names.
        my $i = 0;

        foreach $var (@variablenames) {
            # Remove spaces, double quotes,slashes from the variable names.

            $variablenames[$i] =~ s/ /_/g;
            $variablenames[$i] =~ s/\./_/g;
            $variablenames[$i] =~ s/(\"|-|\/)/_/g;
            $variablenames[$i] =~ s/(\(|\))//g;
            $variablenames[$i] =~ s/(\[|\])//g;
            $variablenames[$i] =~ s/(\<|\>)//g;
            $prevvalues[$i] = 0; # initialize all previous values to 0
            $i += 1; }

        $firstpass = 0; }
    else {
        my $i = 1;
        chomp ($_);
        chop($_);
        if ($_ ne "")
        {
            $_ =~ s/,0,/,1e-69,/g;

            @variables = split(/\,/,$_);
            #$programpoint = $variables[0];
            #$programpoint =~ s/://g;
            #$programpoint =~ s/ /\./;

            $programpoint = "aprogram.point".":::"."POINT";
            $length = @variablenames;
            $dummy = $programpoint."\n";
            $decls = $programpoint."\n";

            if ($decl == 1)
            {
                print DECLSHANDLE "DECLARE\n";
            }
            for ($j = 1; $j<$length; $j++)
            {

                if ($variables[$j] == "") {
                    if ($status =~ /old/) {
                        $replacement = $prevvalues[$i]."\n"."1"."\n"; }
                    elsif($status =~ /zero/) {
                        $replacement = $zerovalue; }
                    elsif($status =~/nonsensical/) {
                        $replacement = $nonsensicalvalue; }
                    elsif($status =~ /interpolate/) {
                        $variableArray{$j}[$counter] = $variables[$j];
                        $variableDistance{$j}[$counter] = $variableDistance{$j}[$counter-1] + 1;
                    }
                    $dummy = $dummy.$variablenames[$i]."\n".$replacement; }
                else {

                    # These arrays are needed for interpolation

                    $variableArray{$j}[$counter] = $variables[$j];
                    $variableDistance{$j}[$counter] = 0;
                    # End of interpolation arrays.
                    $dummy = $dummy.$variablenames[$i]."\n".$variables[$j]."\n"."1"."\n";
                    $prevvalues[$i]  = $variables[$j];
                }
                print DTRACEHANDLE $dummy;
                if ($decl == 1) {
                    $decls = $decls.$variablenames[$i]."\n"."double\n"."double\n1\n";
                    print DECLSHANDLE $decls; }
                $i += 1;
                $dummy = "";
                $decls = "";
            }
            print DTRACEHANDLE "\n";
            if ($decl == 1) {
                print DECLSHANDLE "\n";
                $decl = 0;
            }}}
    $counter += 1;
}

close(DECLSHANDLE);
close(CSVHANDLE);
close(DTRACEHANDLE);

if ($status =~ /interpolate/)
{    interpolate(); }



# This subroutine carries out linear interpolation to
# fill in the missing values in the csv file. Assumes
# the initial value of a variable is zero, if it is not
# initialized. If the last n values of a variable is missing,
# the procedure inserts
sub interpolate()
{
    open (DTRACEHANDLE, ">".$outputfilename) ||
        die("Could not open $outputfilename for output.");
    $j = 1;
    $k = 1;
    for ($k=1; $k<$counter; $k++) {
        print DTRACEHANDLE "\n";
        $dummy = $programpoint."\n";
        for ($j=1; $j<$length; $j++) {
            if ($variableArray{$j}[$k] == "") {
                for ($m=1; $m< $counter-$k; $m++) {
                    if ($variableArray{$j}[$k+$m] == "") {
                    }
                    else {
                        for ($n=0; $n<$m; $n++) {
                            $y1 = $variableArray{$j}[$k+$m];
                            $y0 = $variableArray{$j}[$k-1+$n];
                            $gradient = ($y1-$y0)/($m+1-$n);
                            $yintercept = $variableArray{$j}[$k+$m]-$gradient*($k+$m);
                            $variableArray{$j}[$k+$n] = $gradient*($k+$n)+$yintercept;
                            }
                        $m = $counter;
                    }}
                if ($m == $counter-$k) {
                    $variableArray{$j}[$k] = $prevvalues[$j];
                }}
            $dummy =
                $dummy.$variablenames[$j]."\n".$variableArray{$j}[$k]."\n"."1"."\n";

            print DTRACEHANDLE $dummy;
            $dummy = "";
        }}
    close(DTRACEHANDLE);
}
