: # Use -*- Perl -*- without knowing its path
  eval 'exec perl -S -w -n $0 "$@"'
  if 0;
# merge-esc.pl -- Merge Daikon output into Java source code as ESC assnotations
# Michael Ernst <mernst@lcs.mit.edu>
# Time-stamp: <2001-03-11 02:20:51 mernst>

# The input is a Daikon output file; files from the current directory are
# rewritten into -escannotated versions.


use Carp;

my $warn_on_no_invariants;

BEGIN {
  # Nothing to do
  $warn_on_no_invariants = 0;
}

if ((/^Inv filename = /)
    || (/^Reading (declaration|data trace) files /)
    || (/^Read [0-9]+ declaration file/)
    || (/^    Samples breakdown: /)
    || (/^    Variables:/)
    || (/^      Unmodified variables: /)
    || (/^\[No views for /)
    || (/^esc_name =/)
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
# %raw contains the invariant text directly from the file.
# It is indexed by the complete program point name.
if (defined($methodname)) {
  $raw{$methodname} .= $_;
}


sub simplify_args( $ ) {
  my ($args) = @_;
  $args =~ s/^\s*\(\s*//;
  $args =~ s/\s*\)\s*$//;
  $args =~ s/\s+([\[\]])/$1/g;
  # remove "final" and such
  @args = split(/\s*,\s*/, $args);
  @newargs = ();
  for my $arg (@args) {
    # print "before: $arg\n";
    $arg =~ s/(^|\s)(\w+[\[\]]*)\s+\w+([\[\]]*)$/$1$2/;
    # print "after: $arg\n";
    push @newargs, $arg;
  }
  $newargs = "(" . join(", ", @newargs) . ")";
  return $newargs;
}

sub approx_argsmatch($$) {
  my ($args1, $args2) = @_;
  $args1 =~ s/^\((.*)\)$/$1/;
  $args2 =~ s/^\((.*)\)$/$1/;
  @args1 = split(/\s*,\s*/, $args1);
  @args2 = split(/\s*,\s*/, $args2);
  if (scalar(@args1) != scalar(@args2)) {
    return 0;
  }
  for my $i (0..$#args1) {
    if (! approx_argmatch($args1[$i], $args2[$i])) {
      return 0;
    }
  }
  return 1;
}


sub approx_argmatch($$) {
  my ($x, $y) = @_;
  if ($x eq $y) {
    return 1;
  }
  if (($x eq "") || ($y eq "")) {
    return 0;
  }

  if (length($x) > length($y)) {
    ($x, $y) = ($y, $x);
  }
  if ($x eq substr($y, length($y)-length($x))) {
    return 1;
  }
  return 0;
}


sub is_bogus_invariant( $ ) {
  my ($inv) = @_;
  return (($inv =~ /format_esc class .* needs to be changed/)
	  || ($inv =~ /"null"/)
	  || ($inv =~ /\[\] ==/)
	  || ($inv =~ /\\typeof\([^ ]*\.length/));
}


END {

  my %meth_ppt = ();
  for my $ppt (keys %raw) {
    my $methodname = $ppt;
    $methodname =~ s/^(\w+)\.<init>\($/$1.$1\(/;

    $methodname =~ s/\(([^\(\)]*)\).*$//;
    $newargs = simplify_args($1);
    $methodname .= $newargs;

    $meth_ppt{$methodname} = $ppt;

    # print "method: $methodname\n";
    # print "ppt: $ppt\n";
    # print $raw{$ppt};
  }

  opendir(DIR, ".") || die "can't opendir \".\": $!";
  my @javafiles = grep { /\.java$/ && -f "$_" } readdir(DIR);
  closedir DIR;

  for my $javafile (@javafiles) {
    @fields = ();
    open(GETFIELDS, "$javafile") or die "Cannot open $javafile: $!";
    while (defined($line = <GETFIELDS>)) {
      if ($line =~ /^(\s+)(private[^=]*\b(\w+)\s*[;=].*)$/) {
	my $fieldname = $3;
	if (($line =~ /\[\s*\]/)
	    || ($line !~ /\b(boolean|byte|char|double|float|int|long|short)\b/)) {
	  push(@fields,$fieldname);
	}
      }
    }
    close(GETFIELDS);

    open(IN, "$javafile") or die "Cannot open $javafile: $!";
    open(OUT, ">$javafile-escannotated") or die "Cannot open $javafile-escannotated: $!";

    my $classname = $javafile;
    $classname =~ s/\.java$//;

    while (defined($line = <IN>)) {
      if ($line =~ /\bpublic\b.*\b(\w+)\s*(\([^\)]*\))/) {
	# This looks like a declaration of public method $methodname.
	my $methodname = $1;
	my $args = $2;
	my $fullmethname = "$classname.$methodname";
	my $simple_args = simplify_args($args);
	my $fullmeth = $fullmethname . $simple_args;
	my $prebrace;
	my $postbrace;
	# This is because "$)" in regexp screws up Emacs parser.
	my $eolre = "\\n?\$";
	my $need_newline = 1;
	if ($line =~ /^\s*\{.*$eolre/) {
	  $prebrace = "";
	  $postbrace = $line;
	  $need_newline = 0;
	} elsif ($line =~ /^(.*)(\{.*$eolre)/) {
	  $prebrace = $1;
	  $postbrace = $2;
	} elsif ($line !~ /\)/) {
	  die "Put all args on same line as declaration of $fullmeth";
	} else {
	  my $nextline = <IN>;
	  if ($nextline =~ /^\s*\{.*$eolre/) {
	    $prebrace = $line;
	    $postbrace = $nextline;
	    $need_newline = 0;
	  } elsif ($nextline =~ /^(.*)(\{.*$eolre)/) {
	    $prebrace = $line . $1;
	    $postbrace = $2;
	  } else {
	    die "Didn't find open curly brace in first two lines of method definition:\n  $line  $nextline";
	  }
	}
	print OUT $prebrace;
	my $found = 0;
	for my $ppt (keys %raw) {
	  # print "Checking $fullmeth against $ppt\n";
	  my $ppt_fullmeth = $ppt;
	  $ppt_fullmeth =~ s/:::.*$//;
	  $ppt_methname = $ppt_fullmeth;
	  $ppt_methname =~ s/\(.*$//;
	  $ppt_args = $ppt_fullmeth;
	  $ppt_args =~ s/^.*\(/\(/;
	  if (($fullmeth eq $ppt_fullmeth)
	      || (($fullmethname eq $ppt_methname)
		  && approx_argsmatch($simple_args, $ppt_args))) {
	    $found = 1;
	    if ($need_newline) {
	      print OUT "\n";
	    }
	    if ($ppt =~ /:::ENTER/) {
	      for my $inv (split("\n", $raw{$ppt})) {
		if (is_bogus_invariant($inv)) {
		  print OUT "/*! requires " . $inv . " */\n";
		} else {
		  print OUT "/*@ requires " . $inv . " */\n";
		}
	      }
	    } elsif ($ppt =~ /:::EXIT/) {
	      for my $inv (split("\n", $raw{$ppt})) {
		if ($inv =~ s/^      Modified variables: //) {
		  $inv =~ s/\[\]/[*]/g;
		  my @mods = split(/ /, $inv);
		  @mods = grep(!/\.class$/, @mods);
		  @mods = grep(!/\[[^*]+\]/, @mods);
		  if (scalar(@mods) > 0) {
		    print OUT "/*@ modifies " . join(', ', @mods) . " */\n";
		  }
		} elsif (is_bogus_invariant($inv)) {
		  print OUT "/*! ensures " . $inv . " */\n";
		} else {
		  print OUT "/*@ ensures " . $inv . " */\n";
		}
	      }
	    } else {
	      die "What ppt? $ppt";
	    }
	  }
	}

	if ((! $found) & ($warn_on_no_invariants)) {
	  print "Warning:  no invariants for method $fullmeth on line $line";
	}

	print OUT $postbrace;
	if ($methodname eq $classname) {
	  if (scalar(@fields) > 0) {
	    # Skip over as many lines as possible, until I see a control
	    # structure or a block end or some such.
	    my $nextline;
	    while ((defined($nextline = <IN>))
		   && (! (($nextline =~ /\b(if|while|for)\b|\}/)
			  || (($nextline =~ /\/\*(.*)/) && (! $1 =~ /\*\//))))) {
	      print OUT $nextline;
	    }
	    for my $field (@fields) {
	      print OUT "/*@ set $field.owner = this */\n";
	    }
	    print OUT $nextline;
	  }
	}

	next;
      }

      # This puts object invariants at the beginning.
      # Alternately, put them at the end.
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
	if (defined($raw{$fullmeth})) {
	  for my $inv (split("\n", $raw{$fullmeth})) {
	    if (is_bogus_invariant($inv)) {
	      print OUT "/*! invariant " . $inv . " */\n";
	    } else {
	      print OUT "/*@ invariant " . $inv . " */\n";
	    }
	  }
	}
	next;
      }

      if ($line =~ /^(\s+)(private[^=]*\b(\w+)\s*[;=].*)$/) {
	my ($spaces, $body, $fieldname) = ($1, $2, $3);
	my $is_object = grep(/^$fieldname$/, @fields);
	print OUT "$spaces/*@ spec_public */ $body\n";
	if ($is_object) {
	  print OUT "/*@ invariant $fieldname.owner == this */\n";
	}
	next;
      }

      # default
      print OUT $line;
    }

    close IN;
    close OUT;
  }

  exit 0;			# success

}
