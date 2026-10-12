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

# True if the most recent output was a single-line header, such as
# "decl-version 2.0".  Consecutive single-line headers are output without
# blank lines between them, and a blank line follows the last of them.
my $after_header = 0;

# The line ending of the current file, "\n" or "\r\n".
my $newline = "\n";

# Prints the arguments, using the line ending of the current file.
sub output {
    my $text = join("", @_);
    $text =~ s/\n/\r\n/g if $newline ne "\n";
    print $text;
}

sub end_headers {
    if ($after_header) {
	output "\n";
	$after_header = 0;
    }
}

sub flush_decls {
    foreach my $decl (sort { $a->[1] cmp $b->[1] } @decls) {
	output $decl->[0], $decl->[1], "\n";
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
	my ($stripped) = $line =~ /\A\s*(.*)/s;
	if (is_comment_line($stripped)) {
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

# The files are read through the ARGV filehandle, so Perl's -i command-line
# option rewrites each file in place.
for_each_record(\&process_record, sub { end_headers(); flush_decls(); });

sub process_record {
    my ($record) = @_;
    $newline = $record->{newline};
    my $kind = $record->{kind};
    if ($kind eq "header") {
	flush_decls();
	output $record->{comments}, $record->{text};
	$after_header = 1;
	return;
    }
    end_headers();
    if ($kind eq "ppt") {
	my $text = $record->{text};
	$text .= "\n" if $text !~ /\n\z/;
	push @decls, [$record->{comments}, sort_variables($text)];
	return;
    }
    flush_decls();
    output $record->{comments};
    if ($kind eq "list-implementors") {
	output $record->{text}, "\n";
    } elsif ($kind eq "data") {
	my ($pptname, @lines) = split(/\n/, $record->{text});
	# Each element is a [comments, line] pair for one non-comment line.  A
	# comment line stays with the line that follows it.
	my @items;
	# Comment lines that are not yet attached to a line.
	my $pending = "";
	foreach my $line (@lines) {
	    if (is_comment_line($line)) {
		$pending .= "$line\n";
	    } else {
		push @items, [$pending, $line];
		$pending = "";
	    }
	}
	my @header = ($pptname);
	if (@items and $items[0][1] eq "this_invocation_nonce") {
	    push @header, map { $_->[0] . $_->[1] } splice(@items, 0, 2);
	}
	die "Bad number of lines in $ARGV at line $record->{line}" unless @items % 3 == 0;
	# Each element is a [name, text] pair for one variable.
	my @vars;
	while (@items) {
	    my @var = splice(@items, 0, 3);
	    push @vars, [$var[0][1], join("\n", map { $_->[0] . $_->[1] } @var)];
	}
	@vars = map { $_->[1] } sort { $a->[0] cmp $b->[0] || $a->[1] cmp $b->[1] } @vars;
	output join("\n", @header, @vars), "\n", $pending, "\n";
    }
}
