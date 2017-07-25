#!/usr/bin/env perl

# Checks that all the explicit lackwit comparability classes are
# disjoint at each program point.  This guarantees that the
# comparability relationship is transitive.

# Could also check that the explicit comparability classes are
# exhaustive.  That is, each variable at the program point must be in
# exactly one comparability class.  However, this isn't a problem
# we've been seeing, so this check isn't implemented.

# Note: We don't need this script so much anymore, since the new
# lwpp.pl design guarantees disjoint sets.  However, this script is
# still a good sanity check, to make sure lwpp.pl is working
# correctly.

use English;
use strict;
$WARNING = 1;

my $element_suffix = "_element";
my $index_suffix = "_index";
my $dummy_return = "lh_return_value";

if (@ARGV != 1) {
  die "Usage: decls-lackwit-consistent.pl <filename.decls>\n";
}

my ($decls) = @ARGV;
-f $decls or die "$decls is not a file\n";

open DECLS, $decls or die "can't open $decls\n";

my $first_line = <DECLS>;
chomp $first_line;
my $second_line = <DECLS>;
chomp $second_line;
if (! (($first_line eq "VarComparability") && ($second_line eq "implicit"))) {
  die "Invalid declaration file.  Perhaps it was not processed by lackwit.\n";
}

# maps implicit types (an integer) to explicit types (hash of
# comparable types)
my %implicit_to_explicit = ();

# fill the map of implicit to explicit types, from the comments at the
# end of the decls file
while (<DECLS>) {
  chomp;
  if ($_ eq "# Implicit Type to Explicit Type") {
    while (<DECLS>) {
      chomp;
      s/\#//;
      my ($implicit_type, $colon, @explicit_types) = split;
      my %explicit_types = ();
      foreach my $x (@explicit_types) {
        $explicit_types{$x} = 1;
      }
      $implicit_to_explicit{$implicit_type} = \%explicit_types;
    }
  }
}

# go to the start of the file
seek(DECLS, 0, 0);

my $error = 0;

while (<DECLS>) {
  next if /^\#/; #skip comments

  if (/DECLARE/) {
    # All of the implicit types at this program point
    my @implicit_types = ();

    my $ppt_declaration = <DECLS>;
    chomp $ppt_declaration;
    while (1) {
      my $var = <DECLS>;
      last if ((!$var) || ($var =~ /^\s+$/));
      my $declared_type = <DECLS>;
      my $rep_type = <DECLS>;
      my $implicit = <DECLS>;
      chomp $implicit;

      if ($implicit =~ /^(\d+)\[(\d+)\]$/) {
        push @implicit_types, $1;
        push @implicit_types, $2;
      } elsif ($implicit =~ /^\d+$/) {
        push @implicit_types, $implicit;
      } else {
        die "invalid implicit type: $implicit\n";
      }
    }

    foreach my $imp1 (@implicit_types) {
      foreach my $imp2 (@implicit_types) {
        next if ($imp1 == $imp2);
        my $explicit;
        if (common_key($implicit_to_explicit{$imp1},
                       $implicit_to_explicit{$imp2}, \$explicit)) {
          print "$ppt_declaration\nexplicit type \"$explicit\" " .
            "in multiple implicit types: $imp1, $imp2\n\n";
          $error = 1
        }
      }
    }
  }
}

if ($error) {
  exit 1;
}


# returns true if two hashes have a common key, false otherwise
# the common key is placed into $keyref
sub common_key {
  my ($hashref1, $hashref2, $key_ref) = @_;
  foreach my $key (keys %{$hashref1}) {
    if (exists $hashref2->{$key}) {
      $$key_ref = $key;
      return 1;
    }
  }
  return 0;
}
