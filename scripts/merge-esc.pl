: # Use -*- Perl -*- without knowing its path
  eval 'exec perl -S -w -n $0 "$@"'
  if 0;
# merge-esc.pl -- Merge Daikon output into Java source code as ESC assnotations
# Michael Ernst <mernst@lcs.mit.edu>
# Time-stamp: <2001-03-03 16:25:03 mernst>

# The input is a Daikon output file; files from the current directory are
# rewritten into -escannotated versions.

# 

BEGIN {


}

if ((/^Inv filename = /)
    || (/^Reading (declaration|data trace) files /)
    || (/^Read [0-9]+ declaration file/)
    || (/^    Samples breakdown: /)
    || (/^    Variables: /)
    || (/^\[No views for /)
    || (/^Exiting$/)) {
  next;
}

if (/^===========================================================================$/) {
  undef($methodname);
  next;
}


if (/:::/) {
  if (defined($methodname)) {
    die "methodname is $methodname while reading header line:\n  $_";
  }
  $methodname = $_;
  chomp($methodname);
  $methodname =~ s/ *[0-9]+ samples?$//;
  next;
}

s/[ \t]+\([0-9]+ values?, [0-9]+ samples?\)$//;
$raw{$methodname} .= $_;


END {

  my %meth_ppt = ();
  for my $ppt (keys %raw) {
    my $methodname = $ppt;
    $methodname =~ s/\(.*$//;
    $methodname =~ s/^(\w+)\.<init>$/$1.$1/;
    $meth_ppt{$methodname} = $ppt;

    # print "method: $methodname\n";
    # print "ppt: $ppt\n";
    # print $raw{$ppt};
  }

  opendir(DIR, ".") || die "can't opendir \".\": $!";
  my @javafiles = grep { /\.java$/ && -f "$_" } readdir(DIR);
  closedir DIR;

  for my $javafile (@javafiles) {
    open(IN, "$javafile") or die "Cannot open $javafile: $!";
    open(OUT, ">$javafile-escannotated") or die "Cannot open $javafile-escannotated: $!";

    my $classname = $javafile;
    $classname =~ s/\.java$//;

    while (defined($line = <IN>)) {
      if ($line =~ /\bpublic\b.*\b(\w+)\s*\(/) {
	# This looks like a declaration of public method $methodname.
	my $methodname = $1;
	my $fullmeth = "$classname.$methodname";
	if (! exists $meth_ppt{$fullmeth}) {
	  print "Warning:  no invariants for method $fullmeth\n";
	  print OUT $line;
	  next;
	}
	my $prebrace;
	my $postbrace;
	if ($line =~ /^(.*)\{(.*)$/) {
	  $prebrace = $1 . "\n";
	  $postbrace = $2;
	} elsif ($line !~ /\)/) {
	  die "Put all args on same line as declaration of $fullmeth";
	} else {
	  $prebrace = $line;
	  $postbrace = "";
	}
	print OUT $prebrace;
	for my $inv (split("\n", $raw{$meth_ppt{$fullmeth}})) {
	  print OUT "/*@ " . $inv . " */\n";
	}
	print OUT $postbrace;
	next;
      }

      # Alternately, put object invariants at the very end.  (Jeremy did
      # that; is it necessary, or just something he happened to do?)
      if ($line =~ /^[^\/]*\bclass\s+$classname\b/) {
	# Looks like the declaration of class $classname
	print OUT $line;
	if ($line !~ /\{/) {
	  my $nextline = <IN>;
	  if ($nextline !~ /\{/) {
	    die "Didn't find open curly brace in first two lines of class definition:\n  $line  $nextline";
	  }
	  print OUT $nextline;
	}
	my $fullmeth = "$classname" . ":::OBJECT";
	for my $inv (split("\n", $raw{$fullmeth})) {
	  if ($inv =~ /format_esc class .* needs to be changed/) {
	    print OUT "/*! invariant " . $inv . " */\n";
	  } else {
	    print OUT "/*@ invariant " . $inv . " */\n";
	  }
	}
	next;
      }

      # default
      print OUT $line;
    }
    close IN;
    close OUT;
  }

}
