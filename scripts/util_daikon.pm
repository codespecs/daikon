#!/usr/bin/env perl
# util_daikon.pm -- Perl utilities for the Daikon project.
# The externally-visible procedures are listed in the @EXPORT statement.

package util_daikon;
require 5.003;			# uses prototypes
require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw( cleanup_pptname system_or_die backticks_or_die );

use English;
use strict;
$WARNING = 1;			# "-w" flag

use checkargs;
use Carp;

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


###########################################################################
### End of file
###

# Return true to indicate success loading this package.
1;
