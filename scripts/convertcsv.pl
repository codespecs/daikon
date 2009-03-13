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
$WARNING=1;

use Text::CSV;
use Getopt::Long;
use checkargs;

# TODO:
#   If using "-m interpolate", then what about missing values before the
#   first non-missing value?  Perhaps permit a second argument to specify
#   what to do in that case, though leaving it as missing seems reasonable.

my $USAGE =
  "Usage: convertcsv.pl [options] <inputfilename>
  Options:
     -m [behavior]:  behavior for missing values in the csv file.
                     (A value is missing if solely whitespace.)
        -m nonsensical : use Daikon \"nonsensical\" values.
        -m old         : use the last non-missing value of the variable
                         (*DEFAULT*).
        -m interpolate : linearly interpolate between non-missing values.
                         If a .csv file contains variables of String type,
                         this option will use the last non-missing value
                         for all String variables.
        -m zero        : use zero (or empty string, for strings)

     -decl [declarationsfilename]: use the declarations in the
                          <declarationsfilename> to create the
                          data trace file.  No .decls file
                          will be created.

  Input:  A file in CSV (comma-separated values) format.  The first row
          contains variable names.  Each subsequent row contains one value
          for each variable.

  Output: If <inputfilename> has a \".csv\" extension, creates
          a declaration file and a data trace file named by
          substituting the \".csv\" extension by \".decls\"
          and \".dtrace\" respectively.
          If <inputfilename> does not have \".csv\" extension,
          creates a declaration file and a data trace file by
          appending \".decls\" and \".dtrace\" to <inputfilename>.

";

# by default insert the last value seen for missing values
my $missingaction = "old";

# Name of the csv file.
my $inputfilename;

my $dtrace_file;
# This is used in opposite senses in the two parts of this program.
# In first part:  is set if provided on command line, undefined if not.
# In second part:  is set if being written, undefined if not.
my $decls_file;

GetOptions( 'decl:s' => \$decls_file,
            'm:s' => \$missingaction
            );

# Check "$missingaction" (the value provided to "-m")
if (($missingaction ne "interpolate") &&
    ($missingaction ne "old") &&
    ($missingaction ne "zero") &&
    ($missingaction ne "nonsensical")) {
  # unknown value given. Exit.
  print "Invalid -m option: $missingaction\n\n";
  print $USAGE;
  exit(1);
}


# GetOptions removes all the options and their values from @ARGV.
# So now, check if a <filename> is provided.
if (scalar(@ARGV) == 1) {
  $inputfilename = $ARGV[0];
} elsif (scalar(@ARGV) == 0) {
  print "$0: missing input filename.\n";
  print $USAGE;
  exit(1);
} else {
  print "$0: too many input filenames supplied.\n";
  print $USAGE;
  exit(1);
}


# variable names in the declaration file (whether provided or generated).
my @decl_varnames;

# Maps variable names to the index within the csv file (which is
# the index in the @csv_varnames array).
my %varNameCsvIndex;

# Name of the single program point in the resulting file.
my $programpointname;

# Set filenames for output.
$dtrace_file = "$inputfilename.dtrace";
$dtrace_file =~ s/\.csv\.dtrace$/\.dtrace/;
# At this point, decls_file is defined if it was provided on the command line.
if (defined($decls_file)) {
  ($programpointname, @decl_varnames) = parseDecl($decls_file);
  undef $decls_file;
} else {
  $decls_file = "$inputfilename.decls";
  $decls_file =~ s/\.csv\.decls$/\.decls/;
  $programpointname = "aprogram.point:::POINT"
}
# At this point, decls_file is defined if it should be written.

open (CSVHANDLE, $inputfilename) ||
  die("Could not open $inputfilename for input.");
open (DTRACEHANDLE, ">$dtrace_file") ||
  die("Could not open $dtrace_file for output.");

if (defined($decls_file)) {
  open (DECLSHANDLE, ">$decls_file") ||
    die("Could not open $decls_file for output.");
}


# Counts the number of lines (samples) in the input file.
my $num_samples = 0;


# Arrays indexed by csv index
my @csv_varnames;               # variable names
my @prevvalues;                 # previous values
my @isNumber;                   # type:  true if numeric, false if string

# Contains the entire CSV file, transposed.
# @variableArray[i] is all the values of the ith column in the CSV file.
my %variableArray;


# Now parse the input file and create the declartions and dtrace files.
my $csv = Text::CSV->new();

my $varnames_input = <CSVHANDLE>;
# capture the variable names in the global
# variable @csv_varnames.
getVariableNames($varnames_input);

for (my $i=0; $i<scalar(@csv_varnames); $i++) {
  $prevvalues[$i] = 0;          # initialize all previous values to 0
  $isNumber[$i] = 1;            # initialize all types to number
  $varNameCsvIndex{$csv_varnames[$i]} = $i;
}

if (defined($decls_file)) {
  @decl_varnames = @csv_varnames;
}
my $num_decl_vars = scalar(@decl_varnames);


while (<CSVHANDLE>) {
  chomp ($_);                   # remove the end of line.

  # If there is an extra comma at the end of the line, remove it.
  # (But how do we know that it is an extra?)
  $_ =~ s/,$//g;
  # Skip blank lines
  if ($_ eq "") {
    next;
  }

  my $csvstatus = $csv->parse($_); # parse a CSV string into fields
  if (!$csvstatus) {
    die("Unparseable line in csv file $inputfilename: $_");
  }
  my @sample = $csv->fields();  # get the parsed fields

  {
    # Check the number of columns in the csv file.
    my $sample_length = scalar(@sample);
    if ($sample_length > $num_decl_vars) {
      die "csv file $inputfilename line " . ($num_samples+1) . " contains $sample_length values, was declared to have $num_decl_vars";
    }
    if ($sample_length < $num_decl_vars) {
      die "csv file $inputfilename line " . ($num_samples+1) . " contains $sample_length values, was declared to have $num_decl_vars";
      for (my $z = $sample_length; $z < $num_decl_vars; $z++) {
        $sample[$z] = "";
      }
    }
  }

  print DTRACEHANDLE "$programpointname\n";
  for (my $j = 0; $j<$num_decl_vars; $j++) {
    my $csvindex = $varNameCsvIndex{$decl_varnames[$j]};
    my $value = $sample[$csvindex];
    my $modbit = 1;

    if ($value =~ /^ *$/) {
      # The value is missing (or consists only of spaces)
      my $prevvalue = $prevvalues[$csvindex];
      if ($missingaction eq "old") {
        $value = $prevvalue;
      } elsif($missingaction eq "zero") {
        if (isnumber($prevvalue)) {
          $value = "0";
        } else {
          $value = "\"\"";
        }
      } elsif ($missingaction eq "nonsensical") {
        $value = "nonsensical";
        $modbit = 2;
      } elsif ($missingaction eq "interpolate") {
        $variableArray{$csvindex}[$num_samples] = $value;
      }
    } else {
      if (!isnumber($value)) {
        $value = "\"$value\"";
        $isNumber[$csvindex] = 0;
      }
      $prevvalues[$csvindex] = $value;
      if ($missingaction eq "interpolate") {
        # capture required values for interpolation.
        $variableArray{$csvindex}[$num_samples] = $value;
      }
    }
    print DTRACEHANDLE "$csv_varnames[$csvindex]\n";
    print DTRACEHANDLE "$value\n";
    print DTRACEHANDLE "$modbit\n";
  }

  print DTRACEHANDLE "\n";
  $num_samples++;
}

if (defined($decls_file)) {
  print DECLSHANDLE "DECLARE\n$programpointname\n";
  for (my $j = 0; $j<$num_decl_vars; $j++) {
    my $csvindex = $varNameCsvIndex{$decl_varnames[$j]};
    print DECLSHANDLE "$csv_varnames[$csvindex]\n";
    if ($isNumber[$csvindex]) {
      print DECLSHANDLE "double\ndouble\n";
    } else {
      print DECLSHANDLE "java.lang.String\njava.lang.String\n";
    }
    print DECLSHANDLE "1\n";
  }
}

close(DECLSHANDLE);
close(CSVHANDLE);
close(DTRACEHANDLE);

# Interpolation requires extra work, as we first need to find new values
# for missing values, then write them into the file.
if ($missingaction =~ /interpolate/) {
  interpolate();
}

exit();


###########################################################################
### Subroutines
###


# This subroutine carries out linear interpolation to
# fill in the missing values in the csv file. Assumes
# the initial value of a variable is zero, if it is not
# initialized. If the last n values of a variable is missing,
# the procedure inserts the last non-missing value for
# all n values.
# Since the decls file is already written (and is correct),
# it need not be rewritten.
# TODO: integrate in earlier processing so that it does not require
# storing the entire file in the @variableArray array.

sub interpolate () {
  check_args(0, @_);
  open (DTRACEHANDLE, "> $dtrace_file") ||
    die("Could not open $dtrace_file for output.");

  for (my $k = 0; $k < $num_samples; $k++) {
    print DTRACEHANDLE "\n$programpointname\n";
    for (my $j = 0; $j < $num_decl_vars; $j++) {
      my $csvindex = $varNameCsvIndex{$decl_varnames[$j]};
      if ($variableArray{$csvindex}[$k] eq "") {
        my $m;
        for ($m=1; $m< $num_samples-$k; $m++) {
          if ($variableArray{$csvindex}[$k+$m] eq "") {
          } else {
            for (my $n=0; $n<$m; $n++) {
              if (isnumber($variableArray{$csvindex}[$k+$n-1])
                  || isnumber($variableArray{$csvindex}[$k+$m])) {
                my $y1 = $variableArray{$csvindex}[$k+$m];
                my $y0 = $variableArray{$csvindex}[$k+$n-1];
                if ($y1 eq "") {
                  $y1 = 0;
                }
                if ($y0 eq "") {
                  $y0 = 0;
                }
                my $gradient = ($y1-$y0)/($m+1-$n);
                my $yintercept = $y1-$gradient*($k+$m);
                $variableArray{$csvindex}[$k+$n] = $gradient*
                  ($k+$n)+$yintercept;
              } else {
                # If not a number cannot interpolate;
                # just copy the previous value.
                $variableArray{$csvindex}[$k+$n] =
                  $variableArray{$csvindex}[$k+$n-1];
              }
            }
            $m = $num_samples;
          }
        }
        if (($m == $num_samples-$k) && ($variableArray{$csvindex}[$k+$m-1] eq "")) {
          $variableArray{$csvindex}[$k] = $prevvalues[$csvindex];
        }
      }
      print DTRACEHANDLE "$csv_varnames[$csvindex]\n";
      print DTRACEHANDLE "$variableArray{$csvindex}[$k]\n";
      print DTRACEHANDLE "1\n";
    }
  }
  close(DTRACEHANDLE);
}


# Returns true if string represents a number (integer, double etc.).
sub isnumber ( $ ) {
  my ($num) = check_args(1, @_);
  # The regular expression is from the Perl FAQ.
  return ($num =~ /^([+-]?)(?=\d|\.\d)\d*(\.\d*)?([Ee]([+-]?\d+))?$/);
}


# Removes spaces, double quotes, slashes, (square|curly|regular) brackets
# from variable names.
# (This is presumably required by the variable name parsing code.)
sub formatVarName ( $ ) {
  my ($input) = check_args(1, @_);
  my $simplified = $input;
  # To facilitate commenting out
  # one or more of these expressions,
  # each one was written in a separate line.
  $simplified =~ s/ /_/g;
  $simplified =~ s/\./_/g;
  $simplified =~ s/(\"|-|\/)/_/g;
  $simplified =~ s/(\(|\))//g;
  $simplified =~ s/(\[|\])//g;
  $simplified =~ s/(\<|\>)//g;
  if ($simplified eq $input) {
    return $input;
  }
  $simplified = $input;
  # Quote backslashes and quotes (in that order)
  $simplified =~ s/\\/\\\\/g;
  $simplified =~ s/\"/\\\"/g;
  return '"' . $simplified . '"';
}


# Captures the variable names in the global variable @csv_varnames.
sub getVariableNames ( $ ) {
  my ($line) = check_args(1, @_);
  chomp ($line);                # remove end of line character.
  $line =~ s/,$//;     # remove commas at the end of each line in csv file.

  my $csvstatus = $csv->parse($line); # parse a CSV string into fields
  if (!$csvstatus) {
    die("Corrupted csv file; cannot parse variable names: $line");
  }

  @csv_varnames = $csv->fields(); # get the parsed fields
  my $i = 0;
  foreach my $var (@csv_varnames) {
    # Remove spaces, double quotes, slashes from the names of variables.
    $csv_varnames[$i] = formatVarName($csv_varnames[$i]);
    $i++;
  }

}


# Parses the provided declaration file.
# Assumes the declaration file contains no blank lines or comments,
# and that it contains only one program point.
# Returns an array containing the program point name and the names of
# variables.
sub parseDecl ( $ ) {
  my ($inputfile) = check_args(1, @_);
  open(DECLHANDLE, $inputfile) ||
    die("Cannot open declarations file $inputfile");
  my $declare = <DECLHANDLE>;
  if ($declare ne "DECLARE\n") {
    die "Didn't see \"DECLARE\" as first line of declaration file $inputfile";
  }
  my $ppt = <DECLHANDLE>;
  chomp($ppt);
  my $i = 0;
  my @varnames;
  my @varDecType;
  my @varActType;
  my @varComparable;

  while (<DECLHANDLE>) {
    chomp($_);
    $varnames[$i] = $_;
    $varDecType[$i] = <DECLHANDLE>;
    $varActType[$i] = <DECLHANDLE>;
    $varComparable[$i] = <DECLHANDLE>;
    $i++;
  }
  close(DECLHANDLE);
  return ($ppt, @varnames);

}
