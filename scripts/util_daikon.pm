#!/usr/bin/env perl
# util_daikon.pm -- Perl utilities for the Daikon project.
# The externally-visible procedures are listed in the @EXPORT statement.

package util_daikon;
require 5.003;			# uses prototypes
require Exporter;
our @ISA = qw(Exporter);
our @EXPORT = qw( cleanup_pptname system_or_die backticks_or_die
                  die_if_version_1_decl is_declaration_paragraph skip_till_next );

use English;
use strict;
$WARNING = 1;			# "-w" flag

use Carp;

# Put the script directory on the @INC path.
use File::Basename;
use lib dirname (__FILE__);

# The file `checkargs.pm` appears in the same directory as this script.
use checkargs;

# Execute the command; die if its execution is erroneous.
# If optional second argument is non-zero, print the command to standard out,
# which may be helpful for indicating progress.
sub system_or_die ( $;$ ) {
  my ($command, $verbose) = check_args_range(1, 2, @_);
  if ($verbose) { print "$command\n"; }
  my $result = system($command);
  if ($result != 0) { croak "Failed executing $command"; }
  return $result;
}

# Execute the command and return the output; die if its execution is erroneous.
# If optional second argument is non-zero, print the command to standard out,
# which may be helpful for indicating progress.
sub backticks_or_die ( $;$ ) {
  my ($command, $verbose) = check_args_range(1, 2, @_);
  if ($verbose) { print "$command\n"; }
  if (wantarray) {
    my @result = `$command`;
    if ($CHILD_ERROR != 0) { croak "Failed executing $command"; }
    return @result;
  } else {
    my $result = `$command`;
    if ($CHILD_ERROR != 0) { croak "Failed executing $command"; }
    return $result;
  }
}

# Remove non-word characters from a program point name, as declared in a
# decls file.  Used for converting ppt names to file names.
my @pptname_unwanted = ('<', '>', '\\', '/', ';', '(', ')',
			# these appear in program point names for C programs
			'*', ' ');
my %pptname_cache = ();
sub cleanup_pptname ( $ ) {
  my ($ppt) = @_;
  if (exists($pptname_cache{$ppt})) {
    return $pptname_cache{$ppt};
  }
  my $result = $ppt;
  $result =~ s/:::/./;
  $result =~ s/Ljava.lang././;
  foreach my $token (@pptname_unwanted) {
    $result =~ s/\Q$token//g;
  }
  $result =~ s/\(\s*(\S+)\s*\)/_$1_/;
  # Replace two or more dots in a row with just one dot.
  $result =~ s/\.\.+/\./g;
  $pptname_cache{$ppt} = $result;
  return $result;
}

# Dies if the argument, which is a paragraph or the first line of a
# paragraph from a .decls or .dtrace file, is a version 1 declaration.
# Only the first line is examined, because a later line of a data record
# may be a variable name such as "DECLARE".  The optional second argument
# is the name of the file, for use in the error message.
sub die_if_version_1_decl ( $;$ ) {
  my ($para, $filename) = check_args_range(1, 2, @_);
  if ($para =~ /\A(DECLARE|VarComparability)$/m) {
    my $file = defined($filename) ? " $filename" : "";
    croak "Version 1 declarations are not supported; convert$file to version 2 format";
  }
}

# Returns true if the argument, which is a paragraph or the first line of a
# paragraph from a version 2 .decls or .dtrace file, is not a data record:
# that is, it is a program point declaration, a header, or a comment.
sub is_declaration_paragraph ( $ ) {
  my ($para) = @_;
  return $para =~ /\A(ppt |decl-version|decl-input|var-comparability|input-language|ListImplementors|\/\/)/;
}

# Reads lines from the filehandle until reaching a blank line or end of
# file.  This skips the remainder of the current paragraph.
sub skip_till_next ( * ) {
  my ($fh) = @_;
  while (defined(my $line = <$fh>)) {
    last if $line =~ /^\s*$/;
  }
}


###########################################################################
### End of file
###

# Return true to indicate success loading this package.
1;
