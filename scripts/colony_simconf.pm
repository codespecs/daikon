#!/uns/bin/perl -w
package colony_simconf;
require 5.003;			# uses prototypes
require Exporter;
@ISA = qw(Exporter);
# I can't get the hashes to be exported (unclear why), so instead,
# provide accessor functions.
@EXPORT = qw( read_sim_conf name_to_package package_to_line packages );

use checkargs;

use strict;
use Carp;
use English;


###########################################################################
### Read sim.conf file
###

## Uses these variables:
my %number_to_name = ();
my %number_to_package = ();
my %name_to_number = ();
my %name_to_package = ();       # also used to determine package of winner.
my %package_to_number = ();
my %package_to_name = ();
my %package_to_line = ();

# I can't get the hashes to be exported (unclear why), so instead,
# provide accessor functions.
sub name_to_package ( $ ) {
  my ($name) = check_args(1, @_);
  if ($name eq "No winner.") {
    return "NoWinner";
  }
  my $result = $name_to_package{$name};
  if (! defined $result) {
    die "not defined: name_to_package{$name}";
  }
  return $result;
}
sub package_to_line ( $ ) {
  my ($package) = check_args(1, @_);
  my $result = $package_to_line{$package};
  if (! defined $result) {
    die "not defined: package_to_line{$package}";
  }
  return $result;
}
sub packages () {
  check_args(0, @_);
  return sort keys %package_to_line;
}

# Read a sim.conf file.
# Format of each line of the sim.conf file is:
#  number,name,package,U,color,color,usernames
sub read_sim_conf ( $ ) {
  my ($master_list) = check_args(1, @_);
  open(IN, $master_list) or die "Cannot read file $master_list";
  my $line;
  while (defined($line = <IN>)) {
    $line =~ s/^\# *//;
    $line =~ s/\r$//;           # deal with DOS line terminators
    if ($line =~ /^$/) {
      next;
    }
    my ($number,$name,$package,$U,$color1,$color2,$usernames) = split(',', $line, 7);
    if (!defined($usernames)) {
      die "Didn't find 7 fields on line: $line";
    }
    if (0) { print $U, $color1, $color2, $usernames; } # avoid unused vars warning
    check_sim_conf_consistency($number, $name, $package);
    $package_to_line{$package} = $line;
  }
  close(IN);
}

# Find inconsistencies in the sim.conf file.
sub check_sim_conf_consistency ( $$$ ) {
  my ($number, $name, $package) = check_args(3, @_);

  # Check for sim.conf inconsistencies
  if (defined($number_to_name{$number}) && ($name ne $number_to_name{$number})) {
    die "Team number \"$number\" has two names:  \"$number_to_name{$number}\" and \"$name\"";
    }
  $number_to_name{$number} = $name;
  if (defined($number_to_package{$number}) && ($package ne $number_to_package{$number})) {
    die "Team number \"$number\" has two packages:  \"$number_to_package{$number}\" and \"$package\"";
    }
  $number_to_package{$number} = $package;
  if (defined($name_to_number{$name}) && ($name ne $name_to_number{$name})) {
    die "Team named \"$name\" has two numbers:  $name_to_number{$name} and $number";
  }
  $name_to_number{$name} = $name;
  if (defined($name_to_package{$name}) && ($package ne $name_to_package{$name})) {
    die "Team named \"$name\" has two packages:  $name_to_package{$name} and \"$package";
  }
  $name_to_package{$name} = $package;
  if (defined($package_to_number{$package}) && ($package ne $package_to_number{$package})) {
    die "Team with package \"$package\" has two numbers:  $package_to_number{$package} and $number";
  }
  $package_to_number{$package} = $package;
  if (defined($package_to_name{$package}) && ($name ne $package_to_name{$package})) {
    die "Team with package \"$package\" has two names:  \"$package_to_name{$package}\" and \"$name\"";
  }
  $package_to_name{$package} = $name;
}

# indicate success in loading the file
1;
