: # Use -*- Perl -*- without knowing its path
  eval 'exec perl -S -w -n $0 "$@"'
  if 0;
# Perl version of java-cpp (which see for comments)
# Josh Kataoka
# Time-stamp: <2000-10-31 07:37:51 mernst>

$tmpfile = "/tmp/java-cpp-$$";

main:
{
    &escape_comment;
    &run_cpp;
    unlink($tmpfile);
}

sub escape_comment {
    if (!open(INFILE, $ARGV[0])) {
	open(INFILE, "-");
    } else {
	shift;
    }

    foreach $arg (@ARGV) {
	$argv .= "$arg ";
    }

    open(TMPFILE, ">$tmpfile") || die "Cannot open $tmpfile: $!\n";

    while (<INFILE>) {
	s|//|DOUBLESLASHCOMMENT|g;
	s|/\*|SLASHSTARCOMMENT|g;
	s/\'/SINGLEQUOTE/g;
	print TMPFILE;
    }

    close(INFILE);
    close(TMPFILE);
}

sub run_cpp {
    open(CPPFILE, "cpp $argv $tmpfile |") || die "Cannot open $tmpfile: $!\n";

    $outputting = 0;
    $pattern = "package";

    while (<CPPFILE>) {
	s|DOUBLESLASHCOMMENT|//|g;
	s|SLASHSTARCOMMENT|/\*|g;
	s/SINGLEQUOTE/\'/g;
	s/"  ?\+ "//g;
	s/^(package .*\.) ([^ ]*) ?;/1$2;/;
	s/^# [0-9]+ ".*"//;
	use English;
	$INPUT_RECORD_SEPARATOR = "";
	if ($outputting) {
	    print;
	} elsif (/$pattern/io) {
	    $outputting = 1;
	    print;
	}
    }

    close(CPPFILE);
}
