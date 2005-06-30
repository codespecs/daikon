#!/usr/bin/env perl
# Summarize the output of "cvs log".
# Run like this:  cvs -q log -d ">1 month ago" -N | cvs-log-summarize.pl

# The input to this script is the output of "cvs log".
# "cvs log" gives information about each file in turn.
# This script rearranges the output so that it is given checkin-by-checkin.

# This script groups any sequence of CVS checkins by the same author with
# no more thna 2 minutes separating them (but not necessarily with
# identical checkin messages).  For each such sequence of CVS checkins, a
# list of files and checkin messages is presented.

use strict;
use English;
$WARNING = 1;

# use Date::Calc;
use Date::Manip;

my $max_checkin_separation = 2; # in minutes

# Record separator (a string, not a regexp).
# We'll hope none of these appear in commit messages.
$INPUT_RECORD_SEPARATOR = "\n=============================================================================\n";

my @commit_records = ();

my $file_record;
while (defined($file_record = <>)) {
  # print "RECORD:\n";
  # print $file_record;

  $file_record =~ s/$INPUT_RECORD_SEPARATOR//o;

  my @this_commit_records = split(/\n----------------------------\n/, $file_record);
  if (scalar(@this_commit_records) == 1) {
    next;
  }
  my $header = shift(@this_commit_records);
  if ($header !~ /\nWorking file: (.*?)\n/) {
    die "could not find 'Working file' in: $header";
  }
  my $filename = $1;
  for my $commit_record (@this_commit_records) {
    $commit_record =~ s/^revision [0-9.]+\n//;
    $commit_record =~ s/;  state: .*\n/;  file: $filename\n/;
    # print "COMMIT_RECORD FOUND:\n$commit_record\n";
    push @commit_records, $commit_record;
  }

}

@commit_records = sort @commit_records;

# Records are now in sorted order

my $current_author = "";
my $current_files = "";
my %current_files = ();
my @current_messages = ();
my %current_messages = ();
my $current_date = "";          # in parsed format

if (scalar(@commit_records) == 0) {
  exit;
}

for my $commit_record (@commit_records) {
  # print "COMMIT_RECORD PROCESSED:\n$commit_record\n";
  $commit_record =~ s/date: (.*);  author: (.*);  file: (.*)\n//;
  my $date_string = $1;
  my $date = ParseDate($date_string);
  my $author = $2;
  my $file = $3;
  my $message = $commit_record;

  if ($current_author eq "") {
    $current_author = $author;
    append_commit_record($file, $date, $message);
    next;
  }

  my $err;
  my $delta = DateCalc($current_date,$date,\$err,1);
  if ($delta !~ /([0-9]+):[0-9]+$/) { die "bad delta: $delta"; }
  my $delta_minutes = $1;
  # print "Delta: $delta; minutes: $delta_minutes\n";

  if (($current_author eq $author)
      && ($delta_minutes < $max_checkin_separation)) {
    append_commit_record($file, $date, $message);
    next;
  }

  print_commit_records();

  append_commit_record($file, $date, $message);
  $current_author = $author;
}

print_commit_records();

###########################################################################
### Subroutines
###

sub append_commit_record ( $$$ ) {
  my ($file, $date, $message) = @_;
  if (! defined($current_files{$file})) {
    $current_files{$file} = 1;
    $current_files .= ", $file";
  }
  $current_date = $date;
  if (! defined($current_messages{$message})) {
    $current_messages{$message} = 1;
    push @current_messages, $message;
  }

}

sub print_commit_records () {
  print "----------------\n";
  print "author: $current_author\n";
  $current_files =~ s/^, //;
  print "files: $current_files\n";
  print join("\n", @current_messages), "\n";
  reset_commit_records();
}

sub reset_commit_records () {
  $current_author = "";
  $current_files = "";
  %current_files = ();
  @current_messages = ();
  %current_messages = ();
  $current_date = "";
}

# end.
