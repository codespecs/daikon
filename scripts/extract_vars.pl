#!/usr/bin/env perl

# extract_vars.pl - Extract variable values from a dtrace file and write
# them out in one of various input formats for different clustering
# algoritm implementations.  For each program point found in the decls
# file(s), this program creates a new file containing the variable values.
# These files are formatted for input into an implementation of a
# clustering algorithm (e.g., see the subroutines 'output_xmeans()',
# 'output_seq()').

# In extracting the variables, we want only scalar data.  We ignore
# (1) Hashcode of Objects (2) Arrays (3) Strings

use English;
use strict;
$WARNING = 1;			# "-w" flag

use Carp;
use List::Util 'shuffle';
use util_daikon;

sub usage () {
    print STDERR
	"Usage: extract_vars.pl [OPTIONS] DTRACE_FILES DECL_FILES\n",
	"Reads compressed (gzipped) or uncompressed dtrace files\n",
	"Options:\n",
	" -a, --algorithm ALG\n",
	"        ALG specifies an implemtation of a clustering algorithm.\n",
	"        Current options are 'km' (for kmeans), 'hierarchical',\n",
	"        and 'xm' (for xmeans). Default is xmeans.\n",
	;
} # usage

# die gracefully while printing the usage.
sub dieusage ( $ ) {
  my ($msg) = @_;
  if ($msg !~ /^\s*$/) {
    print STDERR "$msg\n";
  }
  &usage();
  croak;
}				# dieusage


srand(1);
my %nvars_to_maxsamples = ();
my %pptname_to_fhandles = (); # PPTNAME -> FILEHANDLE

# PPTNAME -> NUM (# variables to be clustered at that ppt)
my %pptname_to_nvars = ();

# PPTNAME -> ARRAY[<PPTNAME, INVOCATION_NONCE, @VAL>]
# each pptname is mapped to an array of its executions.
my %pptname_to_vararrays = ();

# PPTNAME -> ARRAY[VARNAME]
# each pptname is mapped to an array of the variables at that ppt
# which are Objects. These are represented by hashcodes and are not
# included in the clustering.
my %pptname_to_objectvars = ();

# PPTNAME -> ARRAY[INVOCATION_NONCE]
# For the ppts which don't have invocation nonces, this maps the pptname
# to an array of invocation nonces. The nonce starts from 0, and increases
# by 1 anytime the ppt is encountered in the data trace file.
my %pptname_to_nonces = ();

# PPTNAME -> ARRAY[VARNAME]
# keep track of the variable names to be clustered.
my %pptname_to_varnames = ();

# This option is used to select the actual clustering algorithm
# implementation we are planning to use. It determines the format with
# which the extracted variables will be written to a file to be read
# in by a clustering tool. Each algorithm implementation needs a
# corresponding output subroutine to print out variable values in the
# appropriate format for the clustering tool's input. Different
# clustering implementations could use the same input format.
my $algorithm = "xm"; # currently, alternatives are 'hierarchical', 'xm' and 'km'

my @dtrace_files;		# the .dtrace files
my @decls_files;		# the decls files

while (scalar(@ARGV) > 0) {
  my $arg = shift @ARGV;
  if ($arg eq '-a' || $arg eq '--algorithm') {
    $algorithm = shift @ARGV;
  } elsif ($arg =~ /\.decls/) {
    push @decls_files, $arg;
  } elsif ( $arg =~/\.dtrace/ ) {
    push @dtrace_files, $arg;
  } else {
    &dieusage("Unrecognized argument \"$arg\"");
  }
}

if (!($algorithm eq 'km' || $algorithm eq 'hierarchical' || $algorithm eq 'xm')) {
  dieusage("Bad output format $algorithm");
}
if (scalar(@dtrace_files) == 0) {
    &dieusage("No dtrace files specified");
}
if (scalar(@decls_files) == 0) {
    &dieusage("No decls files specified");
}


foreach my $decls_file (@decls_files) {
    &read_decls_file($decls_file);
}

foreach my $dtrace_file (@dtrace_files) {
  if ($dtrace_file =~ /\.gz/) {
    open (DTRACE, "zcat $dtrace_file |") || die("couldn't open dtrace file $dtrace_file with zcat\n");
  } else {
    open (DTRACE, $dtrace_file) || die("couldn't open dtrace file $dtrace_file\n");
  }

  # print "opened $dtrace_file\n";
  while (<DTRACE>) {
    my $line = $_;
    if ($line =~ /:::/) {
      my $pptname = $line;
      chomp ($pptname);

      if ($pptname =~ /:::ENTER/ || !(exists $pptname_to_fhandles{$pptname})) {
	&skip_till_next(*DTRACE);
      } else {
	my @ppt_execution = &read_execution($pptname); # [pptname, invocation_nonce, @variable_values]
	push @{$pptname_to_vararrays{$pptname}}, @ppt_execution;
	if ($algorithm eq 'km' || $algorithm eq 'hierarchical') {
	  &output_seq(@ppt_execution);
	} elsif ($algorithm eq 'xm') {
	  &output_xmeans(@ppt_execution);
	} else {
	  croak("bad output format $algorithm");
	}
      }
    }
  }
}

# close all open filehandles
foreach my $pptname (keys %pptname_to_fhandles) {
  *FH = $pptname_to_fhandles{$pptname};
  close FH;
}

foreach my $pptname (keys %pptname_to_nvars) {
  if (!exists $pptname_to_vararrays{$pptname}) {
    my $to_delete = &cleanup_pptname($pptname);
    $to_delete = "$to_delete.runcluster_temp";
    unlink ($to_delete);
    # Does this mean there were no executions of the program point? -MDE
    # print "deleting $to_delete because no data\n";
  }
}

&sample_large_ppts();

exit();

####################### subroutines ###################

sub sample_large_ppts () {
# so far, memory has not been a problem. If it turns out to be a
# problem, we can read trace file again and printing only the sampled
# invocations, instead of saving all the values in
# %pptname_to_vararrays

  # sample limits for clustering algorithm implementations.
  if ($algorithm eq 'hierarchical') {
    # hierarchical implementation can't handle as much data.
    %nvars_to_maxsamples = (1, 1500, 2, 1500, 3, 1500 , 4, 1200, 5, 1000, 6, 900);
  } elsif ($algorithm eq 'km' || $algorithm eq 'xm')  {
    %nvars_to_maxsamples = (1, 6000, 2, 5000);
  }

  foreach my $pptname (keys %pptname_to_vararrays) {
    my $num = $pptname_to_nvars{$pptname};
    my $maxsamples;		# sample limit for this ppt.
    if (exists $nvars_to_maxsamples{$num}) {
      $maxsamples = $nvars_to_maxsamples{$num};
    } elsif ($algorithm eq 'hierarchical') {
      $maxsamples = 600;
    } elsif ($algorithm eq 'km' || $algorithm eq 'xm') {
      $maxsamples = 4000;
    } else {
      croak("bad output format $algorithm");
    }

    # find the number of samples for this program point
    my $temp = scalar( @{$pptname_to_vararrays{$pptname}} );
    my $nsamples = $temp/($num + 2);

    # if the number of samples is too large, sample.
    if ($nsamples > $maxsamples) {
      my @samples = &get_random_numbers($maxsamples, $nsamples);

      # open a file and print, but first clobber the original one
      my $pptfilename = &cleanup_pptname($pptname);
      my $old = "$pptfilename.runcluster_temp";
      unlink $old;
      my $new_filename = $pptfilename.".runcluster_temp.samp";
      if ($algorithm eq 'km' || $algorithm eq 'hierarchical') {
	&open_file_for_output_seq($pptname, $new_filename);
      } elsif ($algorithm eq 'xm') {
	&open_file_for_output_xmeans($pptname, $new_filename);
      } else {
	croak("bad output format $algorithm");
      }

      # vararray contains the pptname and the invoc, in addition to
      # the variable values
      my $block_size = $pptname_to_nvars{$pptname} + 2;

      for (my $i = 0; $i < $maxsamples; $i++) {
	my $start = $samples[$i] * $block_size;
	my $end = $start + $block_size - 1;

	my @invocation_slice = ();

	for (my $j = $start; $j <= $end; $j++) {
	  push @invocation_slice, $pptname_to_vararrays{$pptname}[$j];
	}
	if ($algorithm eq 'km' || $algorithm eq 'hierarchical') {
	  &output_seq(@invocation_slice);
	} elsif ($algorithm eq 'xm') {
	  &output_xmeans(@invocation_slice);
	} else {
	  croak("bad output format $algorithm");
	}
      }
    }
  }
}

# reads a paragraph of a dtrace file (an execution) and returns an
# array in the format [pptname, invocation_nonce, @variable_values]
sub read_execution ( $ ) {
  my @vararray = ();
  my @objarray;

  my $pptname = $_[0];
  # the pptname is the first element in the array. The variables follow it
  push @vararray, $pptname;

  # find out what the Object variables are for this ppt
  if (exists $pptname_to_objectvars{$pptname}) {
    @objarray = @{$pptname_to_objectvars{$pptname}};
  }

  my ($varname, $value);

  $varname = <DTRACE>;
  if ($varname !~ /this.invocation.nonce/) {
    $pptname_to_nonces{$pptname}++;
    push @vararray, $pptname_to_nonces{$pptname};
  } else {
    $value = <DTRACE>;
    chomp($value);
    push @vararray, $value;
    $varname = <DTRACE>;
  }

  # get the values of the variables at this ppt that we want to cluster
  while ($varname !~ /^$/) {
    chomp( $varname );
    $value = <DTRACE>;
    chomp ($value);

    # see if the variable is an Object variable
    my $object = 0;
    if (scalar(@objarray) > 0) {
      foreach my $variablename (@objarray) {
	if ($variablename eq $varname) {
	  $object = 1;
	}
      }
    }

    if ($object) {
      if ($value =~ /null/) {
	$value =~ -5;
      } elsif ($value =~ /missing/) {
	$value = 0;
      } else {
	$value = 10;
      }
    }

    $value =~ s/false/-10/;
    $value =~ s/true/10/;
    $value =~ s/null/0/;
    $value =~ s/missing/-11111/;
    $value =~ s/NaN/1e10/;
    my $mod = <DTRACE>;		# "$mod" is unused

    # extract variables to be clustered.
    # Omit Object variables, .class, array[] or a string
    if ( $varname !~ /\.class/ && $varname !~ /\[\]/ && $varname !~ /\.toString/) {
      if ( exists $pptname_to_varnames{$pptname}{$varname}) {
	push @vararray, $value;
      }
    }

    $varname = <DTRACE>;
  }
  return @vararray;
}

sub open_file_for_output_seq ( $$ ) {
  # Prep the file for writing the variable values. Now it just prints the
  # number of variables at this program point.
  my ($pptname, $pptfilename) = @_;

  # create a filehandle for this program point
  my $nvars = $pptname_to_nvars{$pptname};
  if ($nvars > 0) {
    local *FNAME;
    open(FNAME, ">$pptfilename") || die("couldn't open $pptfilename for writing\n");
    my $fhandle = *FNAME;
    $pptname_to_fhandles{$pptname} = $fhandle;
    print $fhandle "$nvars\n";
  }
}				# open_file_for_output_seq

# This is for xmeans output. For each execution
# it stores a translation between the sequence
# number of the execution (the order in which it
# appears in the dtrace file, and hence in the file
# we are writing) and the invocation nonce.
my %pptname_to_nonce_translation = ();

sub open_file_for_output_xmeans ( $$ ) {
  # open and prepare a file for writing the variable values for the ppt
  # specified in args(0)

  my ($pptname, $pptfilename) = @_;

  # create a filehandle for this program point and for translation table
  my $nvars = $pptname_to_nvars{$pptname};
  if ($nvars > 0) {

    my $trans = $pptfilename.".trans";
    local *TRANS;
    open (TRANS, ">$trans") || die("couldn't open translation file $trans\n");
    my $fhandle2 = *TRANS;
    $pptname_to_nonce_translation{$pptname} = $fhandle2;

    local *FNAME;
    open(FNAME, ">$pptfilename") || die("couldn't open $pptfilename for writing\n");
    my $fhandle = *FNAME;
    $pptname_to_fhandles{$pptname} = $fhandle;


    print $fhandle "# Generated by extract_vars.pl\n\n";
    for (my $i = 0; $i < $nvars; $i++) {
      print $fhandle "x$i ";
    }
    print $fhandle "\n";
  }
}

# prints out the variable values in column format, one invocation per line
# point1 var1 var2 ... varN
# point2 var1 var2 ... varN
sub output_xmeans ( @ ) {
  my @output_vararray = @_;
  my $output_pptname = $output_vararray[0];
  my $output_nvars = $pptname_to_nvars{$output_pptname};
  my $output_string = "";
  local *FH = $pptname_to_fhandles{$output_pptname};
  local *FH2 = $pptname_to_nonce_translation{$output_pptname};

  ### ignore the invocation nonce (xmeans)
  for (my $k = 1; $k < $output_nvars+1; $k++) {
    $output_string = $output_string.$output_vararray[$k+1]." ";
  }
  chop $output_string;
  print FH "$output_string\n";
  print FH2 "$output_vararray[1]\n"; # print the invocation nonce to the translation table.
}


# prints out the variable values in the following form:
# vector length (N)
# point 1
# var1
# var2
# .
# .
# .
# varN
# point 2
# var1
# var2
# .
# .
# .
# varN
# point 3
# .
# .
# .
sub output_seq ( @ ) {
  my @output_vararray = @_;
  my $output_pptname = $output_vararray[0];
  my $output_nvars = $pptname_to_nvars{$output_pptname};
  local *FH = $pptname_to_fhandles{$output_pptname};
  # print the invoc nonce in addition to the variables.
  for (my $k = 0; $k < $output_nvars+1; $k++) {
    print FH "$output_vararray[$k+1]\n";
  }
}

# read an opened file till you reach a blank line, then return
# (used to skip a paragraph of lines).
sub skip_till_next ( * ) {
  local *FHANDLE = $_[0];
  while (my $line = <FHANDLE>) {
    if ($line =~ /^\s*$/) {
      return;
    }
  }
}				# skip_till_next

# return an array of $target random numbers between 0 (inclusive) and
# $max(exclusive). These are used to sample the invocations at a
# program point.
sub get_random_numbers ( $$ ) {
  my ($target, $max) = @_;
  if ($target < 2*$max) {
    my @list = (0.. $max-1);
    @list = shuffle(@list);
    return @list[0..$target-1];
  } else {
    my $num_selected = 0;
    my @list = ();
    my @selected = ();
    while ($num_selected < $target) {
      my $rand = int(rand($max));
      if ($selected[$rand] != 1) {
	push @list, $rand;
	$num_selected++;
	$selected[$rand] = 1;
      }
    }
    return @list;
  }
}				# get_random_numbers

# read a decls file, figure out the number of variables at each program
# point, and open an output file for each decls file
sub read_decls_file ( $ ) {
  my $decls_file = $_[0];
  open(DECL, $decls_file) || &dieusage("cannot read decls file $decls_file");
  while (<DECL>) {
    my $line = $_;
    if ($line =~ /^DECLARE$/) {
      my $pptname = &read_decl_ppt();

      # extract the variables out of only the EXIT program
      # points. Corresponding ENTER and EXIT invocations must belong to a
      # single cluster, so we can perform clustering on either the ENTER or
      # the EXIT, but not both. We choose the exit program point because it
      # has more variables in scope there (eg. the return variable, etc).
      if ($pptname !~ /ENTER/) {
	my $pptfilename = &cleanup_pptname($pptname);
	$pptfilename = $pptfilename.".runcluster_temp";
	if ($algorithm eq 'km' || $algorithm eq 'hierarchical') {
	  &open_file_for_output_seq($pptname, $pptfilename);
	} elsif ($algorithm eq 'xm') {
	  &open_file_for_output_xmeans($pptname, $pptfilename);
	} else {
	  croak("bad output format $algorithm");
	}
      }
    }
  }
}				# read_decls_file

# read a program point declaration in the decls file.
sub read_decl_ppt () {

  my $nvars;			# number of variables at the program point
  my $pptname = <DECL>;		# the pptname.
  chomp ($pptname);

  # now read the variable names and types
  my $varname;
  while ( defined($varname = <DECL>) && ($varname !~ /^$/) ) {
    chomp ($varname);
    my $declared_type = <DECL>;	# "$declared_type" is unused
    my $rep_type = <DECL>;

    # If the variable is an Object, keep note of that. Will be ignored (not
    # be clustered) later because its value is a hashcode.
    if ( $varname !~ /\.class/ && $varname !~ /\[\]/ && $varname !~ /\.toString/) {
      if ($rep_type =~ /hashcode/) {
	push @{$pptname_to_objectvars{$pptname}}, $varname;
	$nvars++;		# added for object
	$pptname_to_varnames{$pptname}{$varname} = 1;
      } elsif ( $rep_type =~ /=/) {
	# definition. do nothing
      } else {
	$nvars++;
	$pptname_to_varnames{$pptname}{$varname} = 1;
      }
    }
    my $var_comp = <DECL>;  # variable comparability; "$var_comp" is unused
  }
  # Store the number of variables at this program point. Remember that
  # @vararray[1] stores the program point name. The invocation nonce is
  # included in @vararray, but is not counted as a variable.
  $pptname_to_nvars{$pptname} = $nvars;
  return $pptname;
}				# read_decl_ppt
