: # Use -*- Perl -*- without knowing its path
  eval 'exec perl -S -w $0 "$@"'
  if 0;
# context.pl -- Read dfej's context-sensitivity .map files and produce various things from them.
# Jeremy Nimmer <jwnimmer@lcs.mit.edu>
# Time-stamp: <2001-11-29 17:04:21 mistere>

# The input is ... TODO

use Carp;
use File::Find;

my $debug = 0;

# ********** Read command line options **********

sub usagedie {
  print STDERR "Usage: context.pl {--spinfo | --remap invs.txt} {--grain line | meth | class} file1.map [file2.map ...] (", @_, ")\n";
  exit 1;
}

if (($#ARGV >= 0) && ($ARGV[0] eq "-d")) {
  print STDERR "Debugging on\n";
  $debug = 1;
  shift @ARGV;
}

usagedie("Expecting mode") unless ($ARGV[0] =~ /^--(.*)/);
my $mode = $1;
shift @ARGV; # --$mode
usagedie("Unknown mode") unless ($mode =~ /^(spinfo|remap)$/);
if ($mode eq "remap") {
  $remap_file = shift @ARGV;
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
  my @lines = slurpfile($filename);
  for my $line (@lines) {
    $line =~ s/\#.*//;           # strip hash comments
    next if ($line =~ /^\s*$/);  # skip blank lines

    # lines e.g.
    # 0x85c2e8c PC.RPStack get [PC/RPStack.java:156:29] -> "getCons" [(I)LPC/Cons;] PC.RP

    my @rec;
    if (@rec = ($line =~ /^(0x[0-9a-f]+)\s+([\w\.]+)\s+([\w\.]+)\s+\[(.*?):(\d+):(\d+)\]\s+->\s+"([\w\.]+)"\s+\[(.*?)\]\s+([\w\.]+)$/)) {
      # id, fromclass, frommeth, fromfile, fromline, fromcol, toexpr, toargs, toclass
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
	     "unknownCallingExpression", "(??)", "UnknownClass");
  push @records, \@rec;
}

# ********** Post-processing **********

my %spinfos = ();  # in creating spinfo file, just dump this out (key -> header, values -> list of splits)
my %remap = ();   # in processing daikon output, remap these keys to the values

if ("line" eq $grain) {
  foreach (@records) {
    my ($id, $fromclass, $frommeth, $fromfile, $fromline, $fromcol, $toexpr, $toargs, $toclass) = @{$_};
    my $tometh = $toexpr;
    $tometh =~ s/.*\.//;
    $id = hex($id);

    if ("spinfo" eq $mode) {
      # PPT_NAME Class.method
      # daikon_callsite_id == 222222
      my $header = "PPT_NAME " . $toclass . "." . $tometh . "\n";
      my $splitter = "daikon_callsite_id == " . $id . "\n";
      $spinfos{$header} .= $splitter;
    } elsif ("remap" eq $mode) {
      # "daikon_callsite_id == 222222" ==> "<Called from Class.method:#:#>"
      my $from = "daikon_callsite_id == " . $id;
      my $to = "<Called from " . $fromclass . "." . $frommeth . ":" . $fromline . ":" . $fromcol . ">";
      $remap{$from} = $to;

      # Now do the cross product
      foreach (@records) {
	my ($id2, $fromclass2, $frommeth2, $fromfile2, $fromline2, $fromcol2, $toexpr2, $toargs2, $toclass2) = @{$_};
	my $tometh2 = $toexpr2;
	$tometh2 =~ s/.*\.//;
	$id2 = hex($id2);

	# "daikon_callsite_id one of { 222222, 333333 }" ==> "Called from one of { Class.method:#:#, Class.method:#:# }"
	my $from2 = "daikon_callsite_id one of { " . (join ", ", sort($id, $id2)) . " }";
	my $to2 = "<Called from one of { " .
	  $fromclass . "." . $frommeth . ":" . $fromline . ":" . $fromcol . ", " .
	  $fromclass2 . "." . $frommeth2 . ":" . $fromline2 . ":" . $fromcol2 . " }>";
	$remap{$from2} = $to2;
      }
    }
  }

} elsif("method" eq $grain) {
  my %method2num = ();
  foreach (@records) {
    my ($id, $fromclass, $frommeth, $fromfile, $fromline, $fromcol, $toexpr, $toargs, $toclass) = @{$_};
    my $tometh = $toexpr;
    $tometh =~ s/.*\.//;
    $id = hex($id);

    my $method = $fromclass . "." . $frommeth . "*" . $toclass . "." . $tometh;
    if ($method2num{$method}) {
      $method2num{$method} .= " || "
    }
    $method2num{$method} .= "daikon_callsite_id == " . $id;
  }

  for my $method (keys %method2num) {
    my $num = $method2num{$method};
    ($method =~ /(.*)\*(.*)/);
    my ($caller, $callee) = ($1, $2);
    if ("spinfo" eq $mode) {
      # PPT_NAME Class.method
      # daikon_callsite_id == 222222 || daikon_callsite_id == 333333 || ..
      my $header = "PPT_NAME " . $callee . "\n";
      my $splitter = $num . "\n";
      $spinfos{$header} .= $splitter;
    } elsif ("remap" eq $mode) {
      # "daikon_callsite_id == 222222 || daikon_callsite_id == 333333 || .." ==> "<Called from Class.method>"
      my $from = $num;
      my $to = "<Called from " . $caller . ">";
      $remap{$from} = $to;
    }
  }

} elsif("class" eq $grain) {
  my %class2num = ();
  foreach (@records) {
    my ($id, $fromclass, $frommeth, $fromfile, $fromline, $fromcol, $toexpr, $toargs, $toclass) = @{$_};
    my $tometh = $toexpr;
    $tometh =~ s/.*\.//;
    $id = hex($id);

    my $class = $fromclass . "*" . $toclass . "." . $tometh;
    if ($class2num{$class}) {
      $class2num{$class} .= " || "
    }
    $class2num{$class} .= "daikon_callsite_id == " . $id;
  }

  for my $class (keys %class2num) {
    my $num = $class2num{$class};
    ($class =~ /(.*)\*(.*)/);
    my ($caller, $callee) = ($1, $2);
    if ("spinfo" eq $mode) {
      # PPT_NAME Class.method
      # daikon_callsite_id == 222222 || daikon_callsite_id == 333333 || ..
      my $header = "PPT_NAME " . $callee . "\n";
      my $splitter = $num . "\n";
      $spinfos{$header} .= $splitter;
    } elsif ("remap" eq $mode) {
      # "daikon_callsite_id == 222222 || daikon_callsite_id == 333333 || .." ==> "<Called from Class>"
      my $from = $num;
      my $to = "<Called from " . $caller . ">";
      $remap{$from} = $to;
    }
  }
}

if ("spinfo" eq $mode) {
  for my $header (keys %spinfos) {
    my $lines = $spinfos{$header};
    print $header;
    print $lines;
    print "\n";
  }
} elsif ("remap" eq $mode) {
  my @lines = slurpfile($remap_file);
  for my $line (@lines) {
    next if ("daikon_callsite_id == orig(daikon_callsite_id)\n" eq $line);
    for my $from (keys %remap) {
      my $to = $remap{$from};
      $line =~ s/\Q$from/$to/g;
    }
    print $line;
  }
}

# Local Variables:
# mode: cperl
# End:
