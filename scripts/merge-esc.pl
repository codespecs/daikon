: # Use -*- Perl -*- without knowing its path
  eval 'exec perl -S -w -n $0 "$@"'
  if 0;
# merge-esc.pl -- Merge Daikon output into Java source code as ESC
# annotations.  Most users will want to use daikon.tools.jtb.MergeESC
# instead, since it supersedes this script.

# Michael Ernst <mernst@lcs.mit.edu>
# Time-stamp: <2002-03-18 12:28:48 mistere>

# The input is a Daikon output file.  Files from the current directory
# are rewritten into -escannotated versions (use the -r switch as the
# first thing on the command line to recursively search from the
# current directory).

use Carp;
use File::Find;


my $java_modifier_re;		# one modifier
my $java_modifiers_plus_re;	# at least one modifier
my $field_decl_re;		# matches a full line;
				#  groups = ($spaces, $mods, $body, $fieldname).

my $warn_on_no_invariants;	# whether to warn if no invariants for a ppt.
my $merge_unexpressible;	# whether to merge unexpressible invariants;
				#   if false, they are simply discarded.
my $recursive;			# whether to look recursively for files.
my $slashslash;                 # whether to use // or /* comments.

my $methodname;
my %raw;
my $javafile;

BEGIN {
  $java_modifier_re = '\b(?:abstract|final|private|protected|public|static|strictfp|synchronized|transient)\b';
  # $java_modifiers_re = '\s*(?:' . $java_modifier_re . '\s*)*';
  $java_modifiers_plus_re = '\s*(?:' . $java_modifier_re . '\s*)+';

  my $__dollar = "\$";  # to mollify emacs perl-mode source parsing
  $field_decl_re = '^(\s+)(' . $java_modifiers_plus_re . ')([^=;]*\b(\w+)(?:\s*\[\])*\s*[;=].*)' . $__dollar;

  $warn_on_no_invariants = 0;
  $merge_unexpressible = 1;
  $recursive = 0;
  $slashslash = 0;

  if (defined $ARGV[0] && $ARGV[0] eq '-r') {
    $recursive = 1;
    shift @ARGV;
  }
  if (defined $ARGV[0] && $ARGV[0] eq '-s') {
    $slashslash = 1;
    shift @ARGV;
  }
}

if ((/^Inv filename = /)
    || (/^Reading (declaration|data trace) files /)
    || (/^Read [0-9]+ declaration file/)
    || (/^    Samples breakdown: /)
    || (/^    Variables:/)
    || (/^      Unmodified variables: /)
    || (/^      Modified primitive variables: /)
    || (/^\[No views for /)
    || (/^esc_name =/)
    || (/^Variables not equal: /)
    || (/^Condition always satisfied: /)
    || (/^OneOf problem: /)
    || (/^Exiting$/)
    || (/\.toString /)
    )
{
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


###########################################################################
### Subroutines
###

sub esc_comment( $ ) {
  my ($annot) = @_;
  if ($slashslash) {
    return "//$annot\n";
  } else {
    return "/*$annot */\n";
  }
}


# Given an arglist string, return a list of arg strings; basically just
# splits on commas.
sub args_to_list( $ ) {
  my ($args) = @_;
  if (!defined($args)) {
    confess "undefined args";
  }
  $args =~ s/^\s*\(\s*//;
  $args =~ s/\s*\)\s*$//;
  $args =~ s/\s+([\[\]])/$1/g;	# remove space before array brackets
  # remove "final" and such
  my @args = split(/\s*,\s*/, $args);
  return @args;
}

# Given an arglist string, return a string with a list of types.
sub simplify_args( $ ) {
  my ($args) = @_;
  my @args = args_to_list($args);
  my @newargs = ();
  for my $arg (@args) {
    # print "before: $arg\n";
    $arg =~ s/(^|\s)(\w+[\[\]]*)\s+\w+([\[\]]*)$/$1$2/;
    # print "after: $arg\n";
    push @newargs, $arg;
  }
  my $newargs = "(" . join(", ", @newargs) . ")";
  return $newargs;
}

## I'm not sure of the point of the approximate matching.
## Maybe string equal would be good enough, if I also used simplify_args.
# Return true if the arguments are the same modulo whitespace;
# also, names are permitted to match only up to a prefix.
sub approx_argsmatch($$) {
  my ($args1, $args2) = @_;
  my @args1 = args_to_list($args1);
  my @args2 = args_to_list($args2);

  # Compare
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


# Return true if the arguments are the same or one is a prefix of the other.
sub approx_argmatch($$) {
  my ($x, $y) = @_;
  if ($x eq $y) {
    return 1;
  }
  if (($x eq "") || ($y eq "")) {
    return 0;
  }

  # Ensure $x is not longer than $y.
  if (length($x) > length($y)) {
    ($x, $y) = ($y, $x);
  }
  if ($x eq substr($y, length($y)-length($x))) {
    return 1;
  }
  return 0;
}


sub is_non_supported_invariant( $ ) {
  my ($inv) = @_;
  return (($inv =~ /format_esc/)
	  || ($inv =~ /"null"/)
	  || ($inv =~ /\[\] ==/)
	  || ($inv =~ /~/)
	  || ($inv =~ /\bmax\(/)
	  || ($inv =~ /\bmin\(/)
	  || ($inv =~ /\bsum\(/)
	  || ($inv =~ /\\new\(/)
	  || ($inv =~ / has only one value/)
	  || ($inv =~ /\\old\([^\)]*\\old\(/)
	  || ($inv =~ /\\typeof\([^ ]*\.length/));
}

# Given a program point name, return the canonical method name
sub ppt_to_meth( $ ) {
  my ($ppt) = @_;

  my $methodname2 = $ppt;
  # Change "Foo.<init>" to "Foo.Foo".
  $methodname2 =~ s/^(\w+)\.<init>\($/$1.$1\(/;

  # Replace arglist by canonicalized version
  if (($methodname2 !~ /:::(OBJECT|CLASS)/)
      && ($methodname2 !~ s/\(([^\(\)]*)\).*$/&simplify_args($1)/)) {
    die "Can't parse methodname: $methodname2";
  }

  return $methodname2;
}


# Look for the curly brace "{" that begins the method body.
# Returns a list of ($prebrace, $postbrace, $need_newline).
sub parse_method_header( $ ) {
  my ($line) = @_;
  my ($prebrace, $postbrace, $need_newline);

  # This is because "$)" in regexp screws up Emacs parser.
  my $eolre = "\\n?\$";

  if ($line =~ /^\s*\{.*$eolre/o) {
    # Found an open curly brace on this line, following only space.
    # I'm not sure how this can happen; after all, this line matched a
    # method declaration.
    die("How can this happen? line = `$line'");

    $prebrace = "";
    $postbrace = $line;
    $need_newline = 0;
  } elsif ($line =~ /\babstract\b/i) {
    $prebrace = "";
    $postbrace = $line;
    $need_newline = 0;
  } elsif ($line =~ /^(.*)(\{.*$eolre)/o) {
    $prebrace = $1;
    $postbrace = $2;
    $need_newline = 1;
  } elsif ($line !~ /\)/) {
    die "Put all args on same line as declaration:  $line";
  } else {
    my $nextline;
    while (defined($nextline = <IN>)) {
      if ($nextline =~ m:^\s*(/[/*]|\*):) {
	# Line starts with "//" or "/*", or "*" which might be comment continuation
	$line .= $nextline;
      } elsif ($nextline =~ /^\s*\{.*$eolre/o) {
	$prebrace = $line;
	$postbrace = $nextline;
	$need_newline = 0;
	last;
      } elsif ($nextline =~ /^(.*)(\{.*$eolre)/o) {
	$prebrace = $line . $1;
	$postbrace = $2;
	$need_newline = 1;
	last;
      } else {
	  die "In $javafile, didn't find open curly brace in method definition:\n$line\n$nextline";
      }
    }
  }
  return ($prebrace, $postbrace, $need_newline);
}


###########################################################################
### Main processing
###

END {

  # maps from method name to canonical program point name
  my %meth_ppt = ();
  for my $ppt (keys %raw) {
    my $methodname2 = ppt_to_meth($ppt);
    $meth_ppt{$methodname2} = $ppt;
    # print "method: $methodname2\n";
    # print "ppt: $ppt\n";
    # print $raw{$ppt};
  }

  my @javafiles = ();
  my $nodeep = !$recursive;
  find(sub {
    push @javafiles, $File::Find::name if (/\.java$/ && -f);
    $File::Find::prune = (-d && $nodeep && ($_ ne '.'));
  }, ".");

  for $javafile (@javafiles) {
    my @fields = ();		# only non-primitive fields
    my @owned_fields = ();
    my @final_fields = ();	# only non-primitive final fields

    # Set @fields, @owned_fields, and @final_fields.
    # A more sophisticated algorithm would count braces to avoid getting
    # fields of inner classes.
    open(GETFIELDS, "$javafile") or die "Cannot open $javafile: $!";
    while (defined(my $line = <GETFIELDS>)) {
      if ($line =~ /$field_decl_re/o) {
	my $fieldname = $4;
	if (($line =~ /\[\s*\]/)
	    || ($line !~ /\b(boolean|byte|char|double|float|int|long|short)\b/)) {
	  # This is not a primitive field
	  push(@fields,$fieldname);
	  if ($line =~ /\[\s*\]/) {
	    push(@owned_fields, $fieldname);
	  }
	  if ($line =~ /\bfinal\b/) {
	    push(@final_fields, $fieldname);
	  }
	}
      }
    }
    close(GETFIELDS);

    my $classname = $javafile;
    $classname =~ s|\.java$||;  # remove .java
    $classname =~ s|^\./||;     # in case there is a ./ prefix
    $classname =~ s|/|.|g;      # all / to .

    # We assume one class per file (really: >1 implies all are instrumented)
    unless (grep { m/^[()]*$classname/; } (keys %raw)) {
	# print "Skipping $classname due to no invariant\n";
	next;
    }

    open(IN, "$javafile") or die "Cannot open $javafile: $!";
    open(OUT, ">$javafile-escannotated") or die "Cannot open $javafile-escannotated: $!";

    while (defined(my $line = <IN>)) {
      if (($line !~ m|;\s*(//.*)?$|)
	  && ($line =~ /\b(?:public|private|protected)\b/)
	  && ($line =~ /\b(\w+)\s*(\([^\)]*\))/)) {
	# This looks like a declaration of method $methodname2.
	# (Requires public or private or protected to avoid false alarms.)
	my $methodname2 = $1;
	my $args = $2;
	my $fullmethname = "$classname.$methodname2";
	# print "Found $fullmethname in $line";
	my $simple_args = simplify_args($args);
	my $fullmeth = $fullmethname . $simple_args;

	my $prebrace;		# Text up to "{"
	my $postbrace;		# Text following "{"
	my $need_newline = 1;	# Whether to insert a newline
	# Set $prebrace, $postbrace, and $need_newline.
	($prebrace, $postbrace, $need_newline) = parse_method_header($line);

	print OUT $prebrace;
	my $found = "";
	for my $ppt (keys %raw) {
	  # print "Checking $fullmeth against $ppt\n";
	  my $ppt_fullmeth = $ppt;
	  $ppt_fullmeth =~ s/:::.*$//;
	  my $ppt_methname = $ppt_fullmeth;
	  $ppt_methname =~ s/\(.*$//;
	  my $ppt_args = $ppt_fullmeth;
	  $ppt_args =~ s/^.*\(/\(/;
	  if (($fullmeth eq $ppt_fullmeth)
	      || (($fullmethname eq $ppt_methname)
		  && approx_argsmatch($simple_args, $ppt_args))) {
	    # Skip @requires clauses for overridden methods which already
	    # have them; ESC doesn't allow them and they perhaps shouldn't hold.
	    my $no_requires = (($ppt =~ /\.equals\s*\(\s*java\.lang\.Object\b/)
			       || ($ppt =~ /\.toString\s*\(\s*\)/));
	    # add more tests here
	    my $overriding = ($no_requires || 0);
	    # print "overriding=$overriding for $ppt\n";
	    my $requires = ($overriding ? "also_requires" : "requires");
	    my $ensures = ($overriding ? "also_ensures" : "ensures");
	    my $modifies = ($overriding ? "also_modifies" : "modifies");
	    if ($need_newline) {
	      print OUT "\n";
	      if ($found) {
		# print OUT esc_comment(" Just found $ppt $ppt_fullmeth");
		# print OUT esc_comment(" Already found$found");
	      }
	    }
	    $found .= "  $ppt=$ppt_fullmeth";
	    if ($ppt =~ /:::ENTER/) {
	      if (! $no_requires) {
		for my $inv (split("\n", $raw{$ppt})) {
		  if (is_non_supported_invariant($inv)) {
		    if ($merge_unexpressible) {
		      print OUT esc_comment("! $requires " . $inv);
		    }
		  } else {
		    print OUT esc_comment("@ $requires " . $inv);
		  }
		}
	      }
	    } elsif ($ppt =~ /:::EXIT/) {
	      my $ppt_combined = $ppt;
	      $ppt_combined =~ s/(:::EXIT)[0-9]+$/$1/;
	      # If this is :::EXIT22 but :::EXIT exists, suppress this.
	      if (($ppt eq $ppt_combined)
		  || (! exists($raw{$ppt_combined}))) {
		for my $inv (split("\n", $raw{$ppt})) {
		  if ($inv =~ s/^      Modified variables: //) {
		    $inv =~ s/\[\]/[*]/g;
		    $inv =~ s/\[[^][]+\.\.[^][]+\]/[*]/g;
		    my @mods = split(/ /, $inv);
		    @mods = grep(!/\.class$/, @mods);
		    @mods = grep(!/\~/, @mods);

		    # was: @mods = grep(!/\[[^*]+\]/, @mods);
		    # better to a[n..] => a[*] than nothing at all
		    grep(s/\[(.*[^*a-zA-Z0-9._].*)\]/[*]/g, @mods);
		    grep(s/\[(.*\.\..*)\]/[*]/g, @mods);
		    # even better would be to collect the list of
		    # indices which are modified, and create a
		    # \forall to specify that the rest aren't

		    # change ary[*].field to ary[*]
		    grep(s/\[\*\]\..*/[*]/g, @mods);

		    # remove size(foo[*])
		    @mods = grep(!/^size\(.*\)$/, @mods);

		    for my $field (@final_fields) {
		      @mods = grep(!/^this.$field$/, @mods);
		    }
		    if (scalar(@mods) > 0) {
		      print OUT esc_comment("@ $modifies " . join(', ', @mods));
		    }
		  } elsif ($inv =~ /^The invariant on the following line means:/) {
		    print OUT esc_comment(" $inv");
		  } elsif (is_non_supported_invariant($inv)) {
		    if ($merge_unexpressible) {
		      print OUT esc_comment("! $ensures " . $inv);
		    }
		  } else {
		    print OUT esc_comment("@ $ensures " . $inv);
		  }
		}
	      } else {
		# print OUT "Suppressing $ppt because of $ppt_combined\n";
	      }
	    } else {
	      die "What ppt? $ppt";
	    }
	  }
	}

	if ((! $found) & ($warn_on_no_invariants)) {
	  print "Warning:  no invariants for method $fullmeth on line $line";
	}

	# Take special action if this is a constructor.
	if (($methodname2 ne $classname) || (scalar(@owned_fields) == 0)) {
	  print OUT $postbrace;
	} elsif ($postbrace =~ /^(.*)(\}.*\n?)$/) {
	  print OUT "$1\n";
	  for my $field (@owned_fields) {
	    print OUT esc_comment("@ set $field.owner = this");
	  }
	  print OUT $2;
	} else {
	  print OUT $postbrace;
	  if (scalar(@owned_fields) > 0) {
	    # Skip over as many lines as possible, until I see a control
	    # structure or a block end or some such.  This attempts to set
	    # the owner annotation after as many lines as possible, and in
	    # particular to set it after the variable is set.
	    # (Even better would be to look for places the variable is set
	    # (in any method) and set its owner there.)
	    my $nextline;
	    while ((defined($nextline = <IN>))
		   && (! (
			  # entering a control structure; don't want to do
			  # assignments conditionally
			  ($nextline =~ /\b(if|while|for)\b|\}/)
			  # in a comment
			  || (($nextline =~ /\/\*(.*)/) && (! $1 =~ /\*\//))
			  # method call, but not an assignment,
			  # nor a this() or super() call.
			  || (($nextline =~ /^[^=]*\(/)
			      && ($nextline !~ /\b(this|super)\s*\(/))))) {
	      print OUT $nextline;
	    }
	    for my $field (@owned_fields) {
	      print OUT esc_comment("@ set $field.owner = this");
	    }
	    print OUT $nextline;
	  }
	}

	next;
      } elsif ($line =~ /(private|public|protected)/) {
	# print "No match on $line";
      }

      # This puts object invariants at the beginning.
      # (Alternately, one could put them at the end instead.)
      if ($line =~ /^[^\/]*\bclass\s+$classname\b/) {
	# Looks like the declaration of class $classname
	if ($line !~ /\{/) {
	  my $nextline;
	  while (defined($nextline = <IN>)) {
	    if ($nextline =~ m:^\s*/[/*]:) {
	      # Line starts with "//" or "/*"
	      $line .= $nextline;
	    } elsif ($nextline =~ m:^\s*\*:) {
	      # Line starts with "*", which might be comment continuation
	      $line .= $nextline;
	    } elsif ($nextline =~ /\{/) {
	      $line .= $nextline;
	      last;
	    } else {
	      die "Didn't find open curly brace in class definition:\n$line\n$nextline";
	    }
	  }
	}
	print OUT $line;
	for my $fullmeth ("$classname" . ":::OBJECT", "$classname" . ":::CLASS") {
	  if (defined($raw{$fullmeth})) {
	    for my $inv (split("\n", $raw{$fullmeth})) {
	      if (is_non_supported_invariant($inv)) {
	        if ($merge_unexpressible) {
		  print OUT esc_comment("! invariant " . $inv);
	        }
	      } else {
	        print OUT esc_comment("@ invariant " . $inv);
	      }
	    }
	  }
        }
	next;
      }

      if ($line =~ /$field_decl_re/) {
	my ($spaces, $mods, $body, $fieldname) = ($1, $2, $3, $4);
	$mods = "" unless defined($mods); # to prevent warnings
	my $is_owned = grep(/^$fieldname$/, @owned_fields);
	unless ($mods =~ /public/) {
	  print OUT "$spaces/*@ spec_public */ $mods$body\n";
	} else {
	  print OUT "$spaces $mods$body\n";
	}
	if ($is_owned) {
	  print OUT esc_comment("@ invariant $fieldname.owner == this");
	} else {
	  # print "not owned: $fieldname @owned_fields\n";
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
