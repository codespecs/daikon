#!/usr/bin/env perl

# prepare-texinfo-for-spellcheck.pl
# argument: .texinfo file
#
# Reads the input .texinfo file and outputs a filtered
# version that elides the texinfo commands.  It also
# removes other text that is not appropriate as input
# to spellcheckx such as examples and comments.
#
# The resulting file can be spell-checked in batch mode
# to output misspelled words; then the user can correct
# those misspellings in the original version.

use v5.14;    #to get given/when

# Turn off warnings (dangerous, but reduces output)
no if $] >= 5.018, warnings => "experimental::smartmatch";

use strict;
use Text::ParseWords;

my @tokens;
my $index;
my $skip_to_match = "";

# parser states
use constant NORMAL             => 0;
use constant SKIPPING_TO_BRACE  => 1;
use constant PRINTING_TO_BRACE  => 2;

# current parser state - this needs to be a stack!
my $cur_state = NORMAL;
my @state_stack;

# @ line operator actions
use constant SKIP_LINE       => 0;
use constant SKIP_TO_MATCHING_END  => 1;

# not even close to a complete list, but covers our docs
my %line_operators = (
    "\@appendix"          => SKIP_LINE,
    "\@bye"               => SKIP_LINE,
    "\@c"                 => SKIP_LINE,
    "\@cartouche"         => SKIP_LINE,
    "\@chapter"           => SKIP_LINE,
    "\@cindex"            => SKIP_LINE,
    "\@clear"             => SKIP_LINE,
    "\@codequotebacktick" => SKIP_LINE,
    "\@contents"          => SKIP_LINE,
    "\@deffn"             => SKIP_LINE,
#   "\@end"               => SKIP_LINE,  # we special case @end
    "\@enumerate"         => SKIP_LINE,
    "\@everyfooting"      => SKIP_LINE,
    "\@example"           => SKIP_TO_MATCHING_END,
    "\@exampleindent"     => SKIP_LINE,
    "\@finalout"          => SKIP_LINE,
    "\@firstparagraphindent" => SKIP_LINE,
    "\@float"             => SKIP_LINE,
    "\@html"              => SKIP_LINE,
    "\@ifclear"           => SKIP_LINE,
    "\@ifhtml"            => SKIP_LINE,
    "\@ifnothtml"         => SKIP_LINE,
    "\@ifnottex"          => SKIP_LINE,
    "\@ifset"             => SKIP_LINE,
    "\@iftex"             => SKIP_LINE,
    "\@include"           => SKIP_LINE,
    "\@indentedblock"     => SKIP_LINE,
    "\@item"              => SKIP_LINE,
    "\@itemize"           => SKIP_LINE,
    "\@itemx"             => SKIP_LINE,
    "\@macro"             => SKIP_TO_MATCHING_END,
    "\@menu"              => SKIP_TO_MATCHING_END,
    "\@need"              => SKIP_LINE,
    "\@node"              => SKIP_LINE,
    "\@noindent"          => SKIP_LINE,
    "\@page"              => SKIP_LINE,
    "\@paragraphindent"   => SKIP_LINE,
    "\@printindex"        => SKIP_LINE,
    "\@quotation"         => SKIP_LINE,
    "\@section"           => SKIP_LINE,
    "\@set"               => SKIP_LINE,
    "\@setfilename"       => SKIP_LINE,
    "\@smallexample"      => SKIP_TO_MATCHING_END,
    "\@sp"                => SKIP_LINE,
    "\@subsection"        => SKIP_LINE,
    "\@subsubsection"     => SKIP_LINE,
    "\@table"             => SKIP_LINE,
    "\@tex"               => SKIP_TO_MATCHING_END,
    "\@titlepage"         => SKIP_LINE,
    "\@top"               => SKIP_LINE,
    "\@unnumbered"        => SKIP_LINE,
    "\@vskip"             => SKIP_LINE,
    );

# @ token operator actions
use constant OUTPUT_ARG   => 0;
use constant SKIP_ARG     => 1;
use constant SKIP_TOKEN   => 2;
use constant OUTPUT_TOKEN => 3;
use constant SKIP_REST    => 4;

# not even close to a complete list, but covers our docs
my %token_operators = (
    "\@anchor"           => SKIP_ARG,
    "\@b"                => OUTPUT_ARG,
    "\@c"                => SKIP_REST,
    "\@caption"          => OUTPUT_ARG,
    "\@center"           => SKIP_TOKEN,
    "\@cite"             => OUTPUT_ARG,
    "\@code"             => SKIP_ARG,
    "\@command"          => SKIP_ARG,
    "\@copyright"        => SKIP_ARG,
    "\@dfn"              => SKIP_ARG,
    "\@dots"             => SKIP_ARG,
    "\@email"            => SKIP_ARG,
    "\@emph"             => OUTPUT_ARG,
    "\@env"              => SKIP_ARG,
    "\@file"             => SKIP_ARG,
    "\@i"                => OUTPUT_ARG,
    "\@image"            => SKIP_ARG,
    "\@kbd"              => SKIP_ARG,
    "\@key"              => SKIP_ARG,
    "\@nospellcheck"     => SKIP_ARG,
    "\@option"           => SKIP_ARG,
    "\@pxref"            => SKIP_ARG,
    "\@quoteleft"        => SKIP_ARG,
    "\@quoteright"       => SKIP_ARG,
    "\@r"                => OUTPUT_ARG,
    "\@ref"              => SKIP_ARG,
    "\@samp"             => SKIP_ARG,
    "\@settitle"         => SKIP_TOKEN,
    "\@strong"           => OUTPUT_ARG,
    "\@t"                => OUTPUT_ARG,
    "\@tie"              => SKIP_ARG,
    "\@titlefont"        => OUTPUT_ARG,
    "\@today"            => SKIP_ARG,
    "\@uref"             => SKIP_ARG,
    "\@url"              => SKIP_ARG,
    "\@value"            => SKIP_ARG,
    "\@var"              => SKIP_ARG,
    "\@verb"             => SKIP_ARG,
    "\@w"                => OUTPUT_ARG,
    "\@xref"             => SKIP_ARG,
    "\@:"                => SKIP_TOKEN,
    "\@/"                => SKIP_TOKEN,
    "\@\*"               => SKIP_TOKEN,
    "\@\."               => OUTPUT_TOKEN,
    "\@-"                => OUTPUT_TOKEN,
    "\@\@"               => OUTPUT_TOKEN,
    "\@\{"               => OUTPUT_TOKEN,
    "\@}"                => OUTPUT_TOKEN,
    );

# make sure we're looking at a .texinfo file
if ($_ = <STDIN>) {
    if (/^\\input/) {
        print "\n";  #convert to blank line
    } else {
        die "Expected '\\input' as first line of .texinfo file, stopped";
    }
} else {
    die "Can't read input file, stopped";
}

while (<>) {

    #skip blank lines
    if (/^[     ]*$/ ) {
        next;
    }

    # preprocess input line to set up for tokenizing
    # quotewords doesn't like appostrope's
    $_ =~ s/'/''/g;
    # make } a separate token
    $_ =~ s/}/  } /g;
    # make { a separate token
    $_ =~ s/{/  { /g;
    # make sure @ starts it's own token
    $_ =~ s/(.)@/$1  @/g;
    # make @ followed by a special character it's own token
    $_ =~ s/(@[:|\/|\.|-|@|\*|\{|}])/$1 /g;

    @tokens = quotewords('\s+', 1, $_);

    # debugging output
    # print "orig: $_";

    $index = 0;

    if ($tokens[0] =~ /^@/) {
        if ($tokens[0] =~ /^\@end$/) {
            if ($skip_to_match eq $tokens[1]) {
                $skip_to_match = "";
            }
            # don't output an @end line
            next;
        } else {
            if ($skip_to_match ne "") {
                # we're in the middle of skip until matching @end
                next;
            }
            my $lop = $line_operators{$tokens[0]};
            if ($lop eq SKIP_LINE) {
                next;
            } elsif ($lop eq SKIP_TO_MATCHING_END) {
                $skip_to_match = substr $tokens[0], 1;
                # don't output line
                next;
            }
            # its either unknown or a token operator
            # we'll just fall into loop below to process
        }
    }

    for ( ; $index < $#tokens; $index++) {

        # debugging output
        # print "$index:<$tokens[$index]> $cur_state\n";

        if ($skip_to_match ne "") {
            # we're in the middle of skip until matching @end
            next;
        }

        if ($tokens[$index] =~ /^}$/) {
            # we have a '}'; process according to current state
            if ($cur_state eq NORMAL) {
                print "} ";
            } elsif ($cur_state eq SKIPPING_TO_BRACE) {
                if ($#state_stack < 0) { die "Missmatched \'{}\', stopped"; }
                $cur_state = pop @state_stack;
            } elsif ($cur_state eq PRINTING_TO_BRACE) {
                if ($#state_stack < 0) { die "Missmatched \'{}\', stopped"; }
                $cur_state = pop @state_stack;
            }
        } elsif ($tokens[$index] =~ /^@/) {
            my $top = $token_operators{$tokens[$index]};
            if ($top eq SKIP_ARG) {
                push @state_stack, $cur_state;
                $cur_state = SKIPPING_TO_BRACE;
            } elsif ($top eq OUTPUT_ARG) {
                push @state_stack, $cur_state;
                # skipping takes precedence over printing
                if ($cur_state != SKIPPING_TO_BRACE) {
                    $cur_state = PRINTING_TO_BRACE;
                }
                # we assume next token is "{" - skip it
                $index++;
            } elsif ($top eq SKIP_TOKEN) {
            } elsif ($top eq SKIP_REST) {
                next;
            } elsif ($top eq OUTPUT_TOKEN) {
                print substr $tokens[$index], 1;
            } elsif ($tokens[$index] eq "\@") {
                print " ";
            } else {
                # it's an unknown operator
                print STDERR "unknown $tokens[$index] at line ", $. + 1, "\n";
            }
        } else {
            # regular text
            if ($cur_state == NORMAL || $cur_state == PRINTING_TO_BRACE) {
                # need to undouble single quotes
                $tokens[$index] =~ s/''/'/g;
                # now get rid of any remaining double quotes as
                # they confuse the spell checker.
                $tokens[$index] =~ s/''//g;
                print "$tokens[$index] ";
            }
        }
    }

} continue {
    # end of line; print it out
    print "\n";
}
