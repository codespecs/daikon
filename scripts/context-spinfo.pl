: # Use -*- Perl -*- without knowing its path
  eval 'exec perl -S -w $0 "$@"'
  if 0;
# context.pl -- Read dfej's context-sensitivity .map files and produce various things from them.
# Jeremy Nimmer <jwnimmer@lcs.mit.edu>
# Time-stamp: <2001-12-09 18:58:12 mistere>

# The input is ... TODO

use Carp;
use File::Find;

my $debug = 0;

# ********** Read command line options **********

sub usagedie {
  print STDERR "Usage: context-spinfo.pl {--grain line | method | class} file1.map [file2.map ...] (", @_, ")\n";
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

# ********** For each map entry **********

my @records = ();
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
      push @records, \@rec;
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
  push @records, \@rec;
}

# ********** Post-processing **********

debugln("Building maps (" . scalar(@records) . " records) ...");

my %spinfos = ();  # in creating spinfo file, just dump this out (key -> header, values -> list of splits)

if ("line" eq $grain) {
  foreach (@records) {
    my ($id, $fromclass, $frommeth, $fromfile, $fromline, $fromcol, $toexpr, $toargs, $toclass, $tometh) = @{$_};
    $id = hex($id);

    # PPT_NAME Class.method
    # daikon_callsite_id == 222222
    my $header = "PPT_NAME " . $toclass . "." . $tometh . "\n";
    my $splitter = "daikon_callsite_id == " . $id . "\n";
    $spinfos{$header} .= $splitter;
  }

} elsif("method" eq $grain) {
  my %method2num = ();
  foreach (@records) {
    my ($id, $fromclass, $frommeth, $fromfile, $fromline, $fromcol, $toexpr, $toargs, $toclass, $tometh) = @{$_};
    $id = hex($id);

    my $method = $fromclass . "." . $frommeth . "*" . $toclass . "." . $tometh;
    if ($method2num{$method}) {
      $method2num{$method} .= " || "
    }
    $method2num{$method} .= "daikon_callsite_id == " . $id;
  }

  for my $method (sort keys %method2num) {
    my $num = $method2num{$method};
    ($method =~ /(.*)\*(.*)/);
    my ($caller, $callee) = ($1, $2);
    # PPT_NAME Class.method
    # daikon_callsite_id == 222222 || daikon_callsite_id == 333333 || ..
    my $header = "PPT_NAME " . $callee . "\n";
    my $splitter = $num . "\n";
    $spinfos{$header} .= $splitter;
  }

} elsif("class" eq $grain) {
  my %class2num = ();
  foreach (@records) {
    my ($id, $fromclass, $frommeth, $fromfile, $fromline, $fromcol, $toexpr, $toargs, $toclass, $tometh) = @{$_};
    $id = hex($id);

    my $class = $fromclass . "*" . $toclass . "." . $tometh;
    if ($class2num{$class}) {
      $class2num{$class} .= " || "
    }
    $class2num{$class} .= "daikon_callsite_id == " . $id;
  }

  for my $class (sort keys %class2num) {
    my $num = $class2num{$class};
    ($class =~ /(.*)\*(.*)/);
    my ($caller, $callee) = ($1, $2);
    # PPT_NAME Class.method
    # daikon_callsite_id == 222222 || daikon_callsite_id == 333333 || ..
    my $header = "PPT_NAME " . $callee . "\n";
    my $splitter = $num . "\n";
    $spinfos{$header} .= $splitter;
  }
}

debugln("Final pass ...");

for my $header (sort keys %spinfos) {
  my $lines = $spinfos{$header};
  print $header;
  print $lines;
  print "\n";
}

# Local Variables:
# mode: cperl
# End:
