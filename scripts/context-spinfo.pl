: # Use -*- Perl -*- without knowing its path
  eval 'exec perl -S -w $0 "$@"'
  if 0;
# context.pl -- Read dfej's context-sensitivity .map files and produce various things from them.
# Jeremy Nimmer <jwnimmer@lcs.mit.edu>
# Time-stamp: <2001-11-15 16:18:56 mistere>

# The input is ...

use Carp;
use File::Find;

#  if (($#ARGV >= 0) && ($ARGV[0] eq "-d")) {
#    print STDERR "Debugging on\n";
#    $debug = 1;
#    shift @ARGV;
#  }

unless ($#ARGV >= 0) {
  print STDERR "Usage: context.pl file1.map [file2.map ...]\n";
}


sub slurpfile {
  # returns the contents of the first argument (filename) as a list
  my $name = shift;
  open(F, $name) or die("Cannot open $name");
  my @result = <F>;
  close(F);
  return @result;
}

for my $filename (@ARGV) {
  my @lines = slurpfile($filename);
  for my $line (@lines) {
    $line =~ s/\#.*//;           # strip hash comments
    next if ($line =~ /^\s*$/);  # skip blank lines

    # lines e.g.
    # 0x85a6a24 RandomMean main [RandomMean.java:6:13] -> compute [()V] RandomMean

    if ($line =~ /^(0x[0-9a-f]+)\s+([\w\.]+)\s+([\w\.]+)\s+\[(.*?)\]\s+->\s+([\w\.]+)\s+\[(.*?)\]\s+([\w\.]+)$/) {
      my ($id, $fromclass, $frommeth, $fromline, $toexpr, $toargs, $toclass) = ($1, $2, $3, $4, $5, $6, $7);
      ($toexpr =~ /^.*\.(.+?)$/); # after the dot
      my $tometh = $1;

      # PPT_NAME SomeClass.someMethod
      # daikon_callsite_id == 222222
      print "PPT_NAME " . $toclass . "." . $tometh . "\n";
      print "daikon_callsite_id == " . hex($id) . "\n";

    } else {
      print STDERR "Unknown line format: $line";
    }
  }

}
