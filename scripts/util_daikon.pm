#!/usr/bin/env perl
package util_daikon;
require 5.003;			# uses prototypes
require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw( cleanup_pptname system_or_die );

use English;
use strict;
$WARNING = 1;			# "-w" flag

use checkargs;
use Carp;

# Execute the command; die if its execution is erroneous.
sub system_or_die ( $ ) {
  my ($cmd) = check_args(1, @_);
  my $result = system($cmd);
  if ($result != 0)
    { croak "Failed executing $cmd"; }
  return $result;
}

# Remove non-word characters from a program point name, as declared in a
# decls file.  Used for converting ppt names to file names.
my @pptname_unwanted = ('<', '>', '\\', '/', ';', '(', ')');
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
