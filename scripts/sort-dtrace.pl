#!/usr/bin/env perl

# Canonicalize a .decls, .dtrace, or combined .dtrace file by sorting
# PPT declarations, and the variables within each program point, into
# alphabetical order. Each contiguous series of ppt declaration paragraphs is
# reordered, while trace paragraphs remain in the same order as in the
# original file.
# This change is semantics-preserving: Daikon produces the same
# invariants for the original or sorted file.

use strict;
use 5.006;
use warnings;

# Put the script directory on the @INC path.
use File::Basename;
use lib dirname (__FILE__);
# The file `util_daikon.pm` appears in the same directory as this script.
use util_daikon;

# The program point declarations that have not yet been output.  Each is a
# [comments, text] pair, where comments are the comment lines that precede
# the declaration.
my @decls;

sub flush_decls {
    foreach my $decl (sort { $a->[1] cmp $b->[1] } @decls) {
	print $decl->[0], $decl->[1], "\n";
    }
    @decls = ();
}

# Returns the program point declaration that is the argument, with its
# variables sorted.  A comment line stays with the line that follows it.
sub sort_variables {
    my ($text) = @_;
    my @lines = split(/^/m, $text);
    # The ppt line and the ppt-level records.
    my $head = shift @lines;
    # Each element is a [comments, text] pair for one variable.
    my @vars;
    # Comment lines that are not yet attached to a line.
    my $pending = "";
    foreach my $line (@lines) {
	if (is_comment_line($line)) {
	    $pending .= $line;
	} elsif ($line =~ /^\s*variable\s/) {
	    push @vars, [$pending, $line];
	    $pending = "";
	} elsif (@vars) {
	    $vars[-1][1] .= $pending . $line;
	    $pending = "";
	} else {
	    $head .= $pending . $line;
	    $pending = "";
	}
    }
    if (@vars) {
	$vars[-1][1] .= $pending;
    } else {
	$head .= $pending;
    }
    return join("", $head,
		map { $_->[0] . $_->[1] } sort { $a->[1] cmp $b->[1] } @vars);
}

foreach my $file (@ARGV ? @ARGV : ("-")) {
  open(my $fh, $file) or die "Cannot open $file: $!";
  while (defined(my $record = read_record($fh, $file))) {
    my $kind = $record->{kind};
    if ($kind eq "ppt") {
	my $text = $record->{text};
	$text .= "\n" if $text !~ /\n\z/;
	push @decls, [$record->{comments}, sort_variables($text)];
	next;
    }
    flush_decls();
    print $record->{comments};
    if ($kind eq "header") {
	print $record->{text}, "\n";
    } elsif ($kind eq "data") {
	my @lines = split(/\n/, $record->{text});
	my @header;
	push @header, shift @lines;
	if (@lines and $lines[0] eq "this_invocation_nonce") {
	    push @header, shift @lines;
	    push @header, shift @lines;
	}
	die "Bad number of lines" unless @lines % 3 == 0;
	my @vars;
	while (@lines) {
	    push @vars, join("\n", splice(@lines, 0, 3));
	}
	@vars = sort @vars;
	print join("\n", @header, @vars), "\n\n";
    }
  }
  close($fh);
  flush_decls();
}
