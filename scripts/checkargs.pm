#!/uns/bin/perl
# checkargs -- check number of args in function calls
# Michael D. Ernst <mernst@cs.washington.edu>
# http://sdg.lcs.mit.edu/~mernst/software/checkargs.pm
# Time-stamp: <2000-10-26 23:08:27 mernst>

package checkargs;
require 5.004;			# uses "for my $var"
require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw(check_args check_args_range check_args_at_least);
use strict;
use Carp;

=head1 NAME

checkargs -- Check number of args in function calls

=head1 SYNOPSIS

  check_args(cArgsExpected, @_)
  check_args_range(cArgsMin, cArgsMax, @_)
  check_args_at_least(cArgsMin, @_)
where "@_" should be supplied literally.

=head1 DESCRIPTION

checkargs checks the number of arguments passed to a perl function at run
time, catching some common errors that could otherwise go undetected until
later in the program.

As the first line of user-written subroutine foo, do one of the following:

  my ($arg1, $arg2) = check_args(2, @_);
  my ($arg1, @rest) = check_args_range(1, 4, @_);
  my ($arg1, @rest) = check_args_at_least(1, @_);
  my @args = check_args_at_least(0, @_);

These functions may also be called for side effect (put a call to one
of the functions near the beginning of the subroutine), but using the
argument checkers to set the argument list is the recommended usage.

The number of arguments and their definedness are checked; if the wrong
number are received, the program exits with an error message.

Use of checkargs is not without cost:  profiling reveals that in one
system, checkargs consumes 35% of all time.  To create a faster version of
your program after you have thoroughly tested it, name the following script
remove_check_args and run remove_check_args on the perl source to create a
faster version.  (You should probably keep around the original version,
containing the checkargs calls, for future debugging.)

  #!/uns/bin/perl -wp
  # remove_check_args -- replace check_args_.* function calls by "@_"
  # (check_args may consume 35% or more of a program's running time)
  s/^(\s*{)?\s*\bcheck_args(|_at_least|_range)\s*\(.*;/$1||""/e;
  s/=\s*\bcheck_args(|_at_least|_range)\s*\(.*;/= \@_;/;

=head1 AUTHOR

Michael D. Ernst <F<mernst@cs.washington.edu>>

=cut

## Need to check that use of caller(1) really gives desired results.
## Need to give input chunk information.
## Is this obviated by Perl 5.003's declarations?  Not entirely, I think.

sub check_args ( $@ )
{
  my ($num_formals, @args) = @_;
  my ($pack, $file_arg, $line_arg, $subname, $hasargs, $wantarr) = caller(1);
  # Fake uses to satisfy shadowed-variables-perl: $pack $hasargs $wantarr.
  if (@_ < 1) { croak "check_args needs at least 7 args, got ", scalar(@_), ": @_\n "; }
  if ((!wantarray) && ($num_formals != 0))
    { croak "check_args called in scalar context"; }
  # Can't use croak below here: it would only go out to caller, not its caller
  my $num_actuals = @args;
  if ($num_actuals != $num_formals)
    { die "$file_arg:$line_arg: function $subname expected $num_formals argument",
      (($num_formals == 1) ? "" : "s"),
      ", got $num_actuals",
      (($num_actuals == 0) ? "" : ": @args"),
      "\n"; }
  for my $index (0..$#args)
    { if (!defined($args[$index]))
	{ die "$file_arg:$line_arg: function $subname undefined argument ", $index+1, ": @args[0..$index-1]\n"; } }
  return @args;
}

sub check_args_range ( $$@ )
{
  my ($min_formals, $max_formals, @args) = @_;
  my ($pack, $file_arg, $line_arg, $subname, $hasargs, $wantarr) = caller(1);
  # Fake uses to satisfy shadowed-variables-perl: $pack $hasargs $wantarr.
  if (@_ < 2) { croak "check_args_range needs at least 8 args, got ", scalar(@_), ": @_"; }
  if ((!wantarray) && ($max_formals != 0) && ($min_formals !=0) )
    { croak "check_args_range called in scalar context"; }
  # Can't use croak below here: it would only go out to caller, not its caller
  my $num_actuals = @args;
  if (($num_actuals < $min_formals) || ($num_actuals > $max_formals))
    { die "$file_arg:$line_arg: function $subname expected $min_formals-$max_formals arguments, got $num_actuals",
      ($num_actuals == 0) ? "" : ": @args", "\n"; }
  for my $index (0..$#args)
    { if (!defined($args[$index]))
	{ die "$file_arg:$line_arg: function $subname undefined argument ", $index+1, ": @args[0..$index-1]\n"; } }
  return @args;
}

sub check_args_at_least ( $@ )
{
  my ($min_formals, @args) = @_;
  my ($pack, $file_arg, $line_arg, $subname, $hasargs, $wantarr) = caller(1);
  # Fake uses to satisfy shadowed-variables-perl: $pack $hasargs $wantarr.
  # Don't do this, because we want every sub to start with a call to check_args*
  # if ($min_formals == 0)
  #   { die "Isn't it pointless to check for at least zero args to $subname?\n"; }
  if (scalar(@_) < 1)
    { croak "check_args_at_least needs at least 1 arg, got ", scalar(@_), ": @_"; }
  if ((!wantarray) && ($min_formals != 0))
    { croak "check_args_at_least called in scalar context"; }
  # Can't use croak below here: it would only go out to caller, not its caller
  my $num_actuals = @args;
  if ($num_actuals < $min_formals)
    { die "$file_arg:$line_arg: function $subname expected at least $min_formals argument",
      ($min_formals == 1) ? "" : "s",
      ", got $num_actuals",
      ($num_actuals == 0) ? "" : ": @args", "\n"; }
  for my $index (0..$#args)
    { if (!defined($args[$index]))
	{ warn "$file_arg:$line_arg: function $subname undefined argument ", $index+1, ": @args[0..$index-1]\n"; last; } }
  return @args;
}

1;				# successful import
__END__
