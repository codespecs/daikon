#!/usr/bin/env perl
# java-cpp -- C preprocessor specialized for Java
# Michael Ernst and Josh Kataoka
# Time-stamp: <2002-11-23 15:10:12 mernst>

# This acts like the C preprocessor, but
#  * it does not remove comments
#  * it cleans up spacing in the processed file

# If last argument is a file, it is used as input.  Otherwise, input comes
# from standard in.  Output goes to standard out.

# Problem:  single quote marks (') in comment can cause "unterminated
# character constant" warnings.
# The workaround is not to use single quotes in comments; for instance,
# avoid contractions and possessives such as "can't", "won't", "Mike's",
# "Josh's".
# (Implementation note:  I do want substitution to occur in comments.
# Therefore, I do not use the -C (leave comments in) flag to cpp, or make
# JAVACPP_DOUBLESLASHCOMMENT put the rest of the line in a string, both
# of which would also avoid the problem but would prevent substitution.)

# I'm not calling this jpp because someone else has probably already taken
# that name.
# This is a script rather than a shell alias so it's sure to work in
# Makefiles, scripts, etc.




# Original csh script:
#
# if (-e $1) then
#   # Last argument is a file
#   set filearg = $1
#   shift
# else
#   set filearg =
# endif
#
# # echo filearg $filearg
# # echo argv $argv
#
# perl -p -w -e 's/\/\//DOUBLESLASHCOMMENT/g;' -e 's/\/\*/SLASHSTARCOMMENT/g;' $filearg > /tmp/java-cpp-$$-input
# cpp $argv java-cpp-$$-input > java-cpp-$$-output
# cat /tmp/java-cpp-$$-output | perl -p -w -e 's/DOUBLESLASHCOMMENT/\/\//g;' -e 's/SLASHSTARCOMMENT/\/\*/g;' -e 's/"  ?\+ "//g;' -e 's/^(package .*\.) ([^ ]*) ?;/$1$2;/;' -e 's/^# [0-9]+ ".*$//;' | perl -p -w -e 'use English; $INPUT_RECORD_SEPARATOR = "";' | lines-from "package"
# # Problem:  doesn't propagate error codes correctly
# rm java-cpp-$$-input java-cpp-$$-output



use English;
use strict;
$WARNING = 1;			# "-w" command-line switch

my $system_temp_dir = -d '/tmp' ? '/tmp' : $ENV{TMP} || $ENV{TEMP} ||
    die "Cannot determine system temporary directory, stopped";
my $tmpfile_in = "$system_temp_dir/java-cpp-$$-in";
my $tmpfile_out = "$system_temp_dir/java-cpp-$$-out";
my $tmpfile_err = "$system_temp_dir/java-cpp-$$-err";

my $file_handle_nonce = 'fh00';

{
  my $filename;
  if ((scalar(@ARGV) > 0) && (-r $ARGV[$#ARGV])) {
    $filename = pop @ARGV;	# remove last (filename) element
  } else {
    # print STDERR "Last arg not a filename; reading from standard in."
    $filename = "-";
  }

  open(TMPFILE, ">$tmpfile_in") || die "Cannot open $tmpfile_in: $!\n";
  escape_comments($filename);
  close(TMPFILE);

  my $argv = join(' ', @ARGV);
  # intentionally does not capture standard error; it goes straight through
  my $system_result = system("cpp $argv $tmpfile_in > $tmpfile_out 2> $tmpfile_err");
  if ($system_result != 0) {
    rewrite_errors($tmpfile_err, $filename);
    # unlink($tmpfile_in);
    unlink($tmpfile_out);
    # unlink($tmpfile_err);
    die "java-cpp.pl: cpp $argv $filename failed";
  }

  unescape_comments($tmpfile_out);
  unlink($tmpfile_in);
  unlink($tmpfile_out);
  unlink($tmpfile_err);
}

exit();

###########################################################################
### Subroutines
###

# Also processes #include directives.
# Perhaps I should instead do this; the advantage would be correct
# "#line" information.
# ## Use of cpp for #include only:
# cpp -C -nostdinc -undef

sub escape_comments ( $ ) {
  my ($filename) = @_;

  # Indirect through this filehandle name in order to make this routine
  # (escape_comments) re-entrant.
  my $inhandle = $file_handle_nonce++;   # this is a string increment

  # print STDERR "Opening $filename\n";

  no strict 'refs';
  open($inhandle, $filename) || die "Can't open $filename: $!\n";

  while (<$inhandle>) {

    if (/^\#include "(.*)"/) {
      escape_comments($1);
      next;
    }
    s|//|JAVACPP_DOUBLESLASHCOMMENT|g;
    s|/\*|JAVACPP_SLASHSTARCOMMENT|g;
    s/\'/JAVACPP_SINGLEQUOTE/g;
    print TMPFILE;
  }
  close($inhandle);

}


sub unescape_comments ( $ ) {
  my ($filename) = @_;

  # This causes strings to potentially have many trailing blanks.
  $INPUT_RECORD_SEPARATOR = "";

  open(CPPFILE, $filename) || die "Cannot open $filename: $!\n";

  my $post_return_space = "";
  my $next_post_return_space = "";
  my $post_else_space = "";
  my $next_post_else_space = "";

  while (<CPPFILE>) {
    s|JAVACPP_DOUBLESLASHCOMMENT|//|g;
    s|JAVACPP_SLASHSTARCOMMENT|/\*|g;
    s/JAVACPP_SINGLEQUOTE/\'/g;

    # Convert string concatenation ("a" + "b") single string ("ab").
    while (s/(".*)"  ?\+ "(.*")/$1$2/g) { }
    # Remove "# 22" lines.
    s/^\# [0-9]+ ".*"($|\n)//g;	# don't leave blank line at start of file
    s/(\n)\# [0-9]+ ".*"($|\n)/$1$2/g;

    ## Remove extra horizontal space
    ## (Some of these are cosmetic; others are necessary to get identical
    ## output under all versions of cpp.)
    # Remove all trailing space
    s/[ \t]+\n/\n/g;
    # Remove space after package name
    s/((?:^|\n)package .*\.) ([^ ]*) ?;/$1$2;/g;
    # Remove all extra spaces in import list
    while (s/^(import [^ \n]*) (.*;)$/$1$2/m) { }
    # convert " );" to ");"; requires "=" somewhere earlier in line
    s/(=.*[^ \t\n\}]) (\);\n)/$1$2/g;
    # convert "(Foo )" to "(Foo)"
    s/\((\b[A-Za-z]\w*) \)/($1)/g;
    # convert "a .b" to "a.b".
    s/(\b[A-Za-z]\w*) \.([A-Za-z]\w*\b)/$1.$2/g;
    # convert "a. foo (" to "a.foo("
    # (Note single spaces, lowercase first letter.)
    s/(\b[A-Za-z]\w*)\. ([a-z]\w*) \(/$1.$2\(/g;
    # convert " instanceof long [])" to " instanceof long[])"
    s/( instanceof \w+) ((\[\])*\))/$1$2/g;
    ## These are necessary to work around cpp differences
    # convert "new int[2 ]" to "new int[2]"
    s/(\bnew [a-z]+\[\w+) *(\])/$1$2/g;
    # convert "public PptSlice1 (" to "public PptSlice1("
    s/(^ *public \w+) (\()/$1$2/g;

    ## Remove extra vertical space
    # compress out duplicate blank lines
    s/\n\n\n+/\n\n/g;
    # This does not work:  it applies to *every* paragraph.
    # # compress out blank lines at end (due to the above, this can be simpler)
    # s/\n\n\z/\n/;
    # Remove newline after "if" statement
    # if no open curly brace or semicolon but 2 newlines.
    if (/^[ \t]*if[ \t]*\([^\n\{;]*\n\n\z/) {
      # not "chomp":  it removes all of the trailing newlines rather than one
      s/\n\z//;
    }

    # Remove newline after "return" statement if followed by 2 nelines and
    # open curly brace.  But I have no way of knowing that open curly follows.
    # Thus, the post_return_space hack.
    if (/\breturn [^\n]*;\n\n\z/) {
      s/\n\z//;
      $next_post_return_space = "\n";
    }
    # Same for closing up space in "} else\n\n{"
    if (/\} else\n\n\z/) {
      s/\n\n\z//;
      $next_post_else_space = "\n\n";
      $next_post_else_space = "POST_ELSE_SPACE for <<<$_>>>\n";
    }

    # Skip if nothing to print (e.g., this paragraph was just a "# 22" line)
    if (! /[^\n]/) { next; }

    if (/^[ \t]*\}/) {
      $post_return_space = "";
    }
    print $post_return_space;
    $post_return_space = $next_post_return_space;
    $next_post_return_space = "";

    if (($post_else_space ne "") && /^[ \t]*\{/) {
      s/^[ \t]*\{/ \{/;
      $post_else_space = "";
    }
    print $post_else_space;
    $post_else_space = $next_post_else_space;
    $next_post_else_space = "";

    print;
  }

  close(CPPFILE);
}


sub rewrite_errors ( $$ ) {
  my ($tmp_filename, $orig_filename) = @_;

  open(ERRFILE, $tmp_filename) || die "Cannot open $tmp_filename: $!\n";

  while (<ERRFILE>) {
    s/$tmpfile_in/$orig_filename/o;
    print STDERR;
  }

  close(ERRFILE);
}
