#!/usr/bin/env perl

# context-remap.pl -- Read dfej's context-sensitivity .map files and
# textually re-write daikon invariants in a more human-readable form,
# translating $callsite numbers to names of classes or methods.

# Jeremy Nimmer <jwnimmer@lcs.mit.edu>
# Time-stamp: <2002-03-11 16:21:41 mistere>

use Carp;
use File::Find;
use English;
# use strict;
$WARNING = 1;			# "-w" flag

my $debug = 0;

# ********** Read command line options **********

sub usagedie {
  print STDERR "Usage: context-remap.pl invs.txt {--grain line | method | class} file1.map [file2.map ...] (", @_, ")\n";
  exit 1;
}

if (($#ARGV >= 0) && ($ARGV[0] eq "-d")) {
  print STDERR "Debugging on\n";
  $debug = 1;
  shift @ARGV;
}

sub debugln ( $ ) {
  return unless $debug;
  print STDERR @_, "\n";
}

usagedie("Expecting filename") unless ($#ARGV >= 0);
$remap_file = shift @ARGV;

usagedie("Expecting grain") unless ($ARGV[0] eq '--grain');
shift @ARGV; # --grain
my $grain = shift @ARGV;
usagedie("Unknown grain") unless ($grain =~ /^(line|method|class)$/);

# ********** Helpers **********

sub slurpfile {
  # returns the contents of the first argument (filename) as a list
  my $name = shift;
  open(F, $name) or die("Cannot open $name");
  my @result = <F>;
  close(F);
  return @result;
}

# ********** Read map files into a lookup table **********

my %records = ();   # map from callsite_id as integer to corresponding record

for my $filename (@ARGV) {
  debugln("Reading $filename ...");
  my @lines = slurpfile($filename);
  for my $line (@lines) {
    $line =~ s/\#.*//;           # strip hash comments
    next if ($line =~ /^\s*$/);  # skip blank lines

    # lines e.g.
    # 0x85c2e8c PC.RPStack get [PC/RPStack.java:156:29] -> "getCons" [(I)LPC/Cons;] PC.RP

    my @rec;
    if (@rec = ($line =~ /^(0x[0-9a-f]+)\s+([\w\$\.]+)\s+([\w\$\<\>]+)\s+\[(.*?):(\d+):(\d+)\]\s+->\s+"([^\"]*?)"\s+\[(.*?)\]\s+([\w\$\.]+)\s+([\w\$\<\>]+)$/)) {
      # id, fromclass, frommeth, fromfile, fromline, fromcol, toexpr, toargs, toclass, tometh
      $rec[1] =~ s/.*\.//; # remove package from fromclass
      $rec[8] =~ s/.*\.//; # remove package from toclass

      my $idint = hex($rec[0]);
      $records{$idint} = \@rec;

    } else {
      die("Unknown line format: $line");
    }
  }

}

# ID 0 is special -- we don't know what call graph edge we took (it was an uninstrumented edge)
{
  # id, fromclass, frommeth, fromfile, fromline, fromcol, toexpr, toargs, toclass
  my @rec = (0,
	     "UnknownClass", "unknownMethod", "UnknownClass.java", -1, -1,
	     "unknownCallingExpression", "(??)", "UnknownClass", "unknownMethod");
  $records{0} = \@rec;
}

debugln("Loaded " . scalar(keys %records) . " call sites.");

# ********** Table building **********

my %interp = ();   # map from callsite_id as integer to our interpretation of it

for my $idint (sort keys %records) {
  my $recref = $records{$idint};
  my ($id, $fromclass, $frommeth, $fromfile, $fromline, $fromcol, $toexpr, $toargs, $toclass, $tometh) = @{$recref};

  if ("line" eq $grain) {
    # "daikon_callsite_id == 222222" ==> "<Called from Class.method:#:#>"
    $interp{$idint} = "<Called from " . $fromclass . "." . $frommeth . ":" . $fromline . ":" . $fromcol . ">";
  } elsif("method" eq $grain) {
    # "daikon_callsite_id == 222222" ==> "<Called from Class.method>"
    $interp{$idint} = "<Called from " . $fromclass . "." . $frommeth . ">";
  } elsif("class" eq $grain) {
    # "daikon_callsite_id == 222222" ==> "<Called from Class.method>"
    $interp{$idint} = "<Called from " . $fromclass . ">";
  } else {
    die;
  }
}

# ********** Core processing **********

debugln("Rewriting $remap_file to stdout ...");

# Given some daikon output, say what it means (given grain, etc.)
sub describe ( $ ) {
  (my $text) = @_;

  # parse $text into @ids
  my $ids = $text;
  $ids =~ s/[^ 0-9]//g;               # only digits
  $ids =~ s/^\s+//; $ids =~ s/\s+$//; # trim
  my @ids = split(/\s+/, $ids);

  # map @ids into @sites
  my %sites = ();
  for my $id (@ids) {
    $sites{$interp{$id}} = 1;
  }
  my @sites = sort keys %sites;

  # return @sites as text
  return join(" or ", @sites);
}

my @lines = slurpfile($remap_file);
for my $line (@lines) {
  # remove "daikon_callsite_id == orig(daikon_callsite_id"
  if ("daikon_callsite_id == orig(daikon_callsite_id)\n" eq $line) {
    next;
  }
  if ($line =~ /(daikon_callsite_id == \d+( || daikon_callsite_id == \d+)*)/ or
      $line =~ /(daikon_callsite_id one of { \d+(, \d+)* })/) {
    my $match = $1;
    my $replace = describe($match);
    debugln("Writing '$match' as '$replace'");
    $line =~ s/\Q$match/$replace/;
  }
  print $line;
}

# Local Variables:
# mode: cperl
# End:
