: # Use -*- Perl -*- without knowing its path
  eval 'exec perl -S -w $0 "$@"'
  if 0;
# java-cpp -- C preprocessor specialized for Java
# Michael Ernst and Josh Kataoka
# Time-stamp: <2001-03-03 12:20:05 mernst>

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
# DOUBLESLASHCOMMENT put the rest of the line in a string, both of which
# would also avoid the problem.)

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

my $tmpfile = "/tmp/java-cpp-$$";

main:
{
  &escape_comments_toplevel;
  &run_cpp;
  unlink($tmpfile);
}

sub escape_comments_toplevel {

  my $filename;
  if ((scalar(@ARGV) > 0) && (-r $ARGV[$#ARGV])) {
    $filename = pop @ARGV;	# remove last (filename) element
  } else {
    $filename = "-";
  }
  $argv = join(' ', @ARGV);

  open(TMPFILE, ">$tmpfile") || die "Cannot open $tmpfile: $!\n";

  escape_comments($filename);

  close(TMPFILE);
}

my $input = 'fh00';

# Also processes #include directives.
# Perhaps I should instead do this; the advantage would be correct
# #line information.
# ## Use of cpp for #include only:
# cpp -C -nostdinc -undef

sub escape_comments( $ ) {
  my ($filename) = @_;
  my $inhandle = $input++;               # this is a string increment

  # print STDERR "Opening $filename\n";

  if (! open($inhandle, $filename)) {
    print STDERR "Can't open $filename: $!\n";
    return;
  }
  while (<$inhandle>) {             # note use of indirection

    if (/^\#include "(.*)"/) {
      escape_comments($1, $input);
      next;
    }
    s|//|DOUBLESLASHCOMMENT|g;
    s|/\*|SLASHSTARCOMMENT|g;
    s/\'/SINGLEQUOTE/g;
    print TMPFILE;
  }
  close($inhandle);

}


sub run_cpp {
  # This causes strings to potentially have many trailing blanks.
  $INPUT_RECORD_SEPARATOR = "";

  open(CPPFILE, "cpp $argv $tmpfile |") || die "Cannot open $tmpfile: $!\n";

  while (<CPPFILE>) {
    s|DOUBLESLASHCOMMENT|//|g;
    s|SLASHSTARCOMMENT|/\*|g;
    s/SINGLEQUOTE/\'/g;
    s/"  ?\+ "//g;
    s/((?:^|\n)package .*\.) ([^ ]*) ?;/$1$2;/;
    s/(^|\n)\# [0-9]+ ".*"($|\n)/$1$2/;
    # compress out duplicate blank lines
    s/\n\n\n+/\n\n/g;
    # Skip if nothing to print (eg, if this paragraph was just a "# 22" line)
    if (! /[^\n]/) { next; }
    print;
  }

  close(CPPFILE);
}
