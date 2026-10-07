#!/usr/bin/env perl
# util_daikon.pm -- Perl utilities for the Daikon project.
# The externally-visible procedures are listed in the @EXPORT statement.

package util_daikon;
require 5.003;			# uses prototypes
require Exporter;
our @ISA = qw(Exporter);
our @EXPORT = qw( cleanup_pptname system_or_die backticks_or_die
                  escape_decl unescape_decl is_comment_line split_leading_comments
                  record_kind read_ppt_decl read_ppt_decls
                  skip_till_next );

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

# Converts a name to the form used in declaration and data trace files:
# backslashes, newlines, and carriage returns are escaped, and blanks are
# replaced by "\_".  Like FileIO.escape_decl in Daikon.
sub escape_decl ( $ ) {
  my ($name) = @_;
  $name =~ s/\\/\\\\/g;
  $name =~ s/ /\\_/g;
  $name =~ s/\n/\\n/g;
  $name =~ s/\r/\\r/g;
  return $name;
}

# The inverse of escape_decl.  Like FileIO.unescape_decl in Daikon:  a
# backslash followed by any other character yields that character, and a
# trailing backslash is retained.
sub unescape_decl ( $ ) {
  my ($name) = @_;
  # Fast path, because this is called on every variable name in a trace.
  return $name if index($name, "\\") < 0;
  $name =~ s/\\(.)/$1 eq "_" ? " " : $1 eq "n" ? "\n" : $1 eq "r" ? "\r" : $1/ges;
  return $name;
}

# Returns true if the argument, which is a line (or the start of a
# paragraph), is a comment.  Like FileIO.isComment in Daikon.
sub is_comment_line ( $ ) {
  my ($line) = @_;
  return $line =~ /\A(?:\/\/|#)/;
}

# Splits a paragraph into its leading comment lines and the remainder.
# Daikon does not require a blank line after a comment, so a paragraph may
# consist of comments followed by a record.  Returns a two-element list;
# either element may be the empty string.  If the paragraph consists only of
# comments, the first element is the entire paragraph, including any
# trailing blank lines.
sub split_leading_comments ( $ ) {
  my ($para) = @_;
  if ($para =~ /\A((?:(?:\/\/|#)[^\n]*(?:\n|\z))+)/) {
    my $comments = $1;
    my $rest = substr($para, length($comments));
    if ($rest !~ /\S/) {
      return ($para, "");
    }
    return ($comments, $rest);
  }
  return ("", $para);
}

# Dies, saying that version 1 declarations are not supported.  The optional
# argument is the name of the file, for use in the error message.
sub die_version_1 ( ;$ ) {
  my ($filename) = @_;
  my $file = defined($filename) ? " $filename" : "";
  croak "Version 1 declarations are not supported; convert$file to version 2 format";
}

# Returns the kind of a record in a version 2 .decls or .dtrace file.  The
# argument is a paragraph, or the first line of a paragraph.  The result is
# one of:
#   "ppt"      a program point declaration
#   "header"   decl-version, var-comparability, input-language, or
#              ListImplementors
#   "comment"  a comment line
#   "data"     anything else, which is a data trace record
# Dies if the record is a version 1 declaration.  Only the first line is
# examined, because a later line of a data record may be a variable name
# such as "DECLARE".  The optional second argument is the name of the file,
# for use in the error message.
# This is called on every record of a potentially huge trace file, so it
# uses a single regular expression.
sub record_kind ( $;$ ) {
  my ($para, $filename) = @_;
  if ($para !~ /\A(?:(ppt\s)|(decl-version|var-comparability|input-language|ListImplementors)|(\/\/|#)|((?:DECLARE|VarComparability)[ \t\r]*$))/m) {
    return "data";
  }
  return "ppt" if defined($1);
  return "header" if defined($2);
  return "comment" if defined($3);
  die_version_1($filename);
}

# Reads the remainder of a version 2 program point declaration from the
# filehandle, up to a blank line or end of file.  The first argument is the
# "ppt" line, which has already been read.  Comment lines are skipped.
# Returns a reference to a hash with these keys:
#   name     the (unescaped) program point name
#   parents  a reference to an array of the ppt-level parent records, each
#            a reference to a [relation-type, parent-ppt-name, relation-id]
#            array
#   vars     a reference to an array of the variables, in order.  Each is a
#            reference to a hash with keys name (unescaped), dec_type,
#            rep_type, comparability, and constant (the value of a constant
#            variable, or undef if the variable is not a constant).  The
#            value of a constant variable does not appear in data trace
#            records.
sub read_ppt_decl ( $$ ) {
  my ($pptline, $fh) = @_;
  $pptline =~ /\Appt\s+(.*?)\s*\z/s
    or croak "Not a program point declaration: $pptline";
  my $ppt = { name => unescape_decl($1), parents => [], vars => [] };
  my $var;                      # the variable currently being read
  while (defined(my $line = <$fh>)) {
    $line =~ s/\A\s+//;
    $line =~ s/\s+\z//;
    last if $line eq "";
    next if is_comment_line($line);
    my ($key, $value) = split(/\s+/, $line, 2);
    if ($key eq "variable") {
      $var = { name => unescape_decl($value), dec_type => "", rep_type => "",
               comparability => "", constant => undef };
      push @{$ppt->{vars}}, $var;
    } elsif (!defined($var)) {
      if ($key eq "parent") {
        push @{$ppt->{parents}}, [split(/\s+/, $value)];
      }
    } elsif ($key eq "dec-type") {
      $var->{dec_type} = $value;
    } elsif ($key eq "rep-type") {
      $var->{rep_type} = $value;
    } elsif ($key eq "comparability") {
      $var->{comparability} = $value;
    } elsif ($key eq "constant") {
      $var->{constant} = $value;
    }
  }
  return $ppt;
}

# Reads all the program point declarations from the filehandle, which is a
# version 2 .decls or combined .dtrace file.  Headers, comments, and data
# trace records are skipped.  Returns a list of references to hashes, as
# returned by read_ppt_decl.  Dies if the file is in version 1 format or
# contains no program point declarations.  The second argument is the name
# of the file, for use in error messages.
sub read_ppt_decls ( $$ ) {
  my ($fh, $filename) = @_;
  my @ppts = ();
  # Each line read here is a blank line or the first line of a record,
  # because each record is read in its entirety.
  while (defined(my $line = <$fh>)) {
    next if $line =~ /\A\s*\z/;
    my $kind = record_kind($line, $filename);
    if ($kind eq "ppt") {
      push @ppts, read_ppt_decl($line, $fh);
    } elsif ($kind ne "comment") {
      skip_till_next($fh);
    }
  }
  if (!@ppts) {
    croak "No program point declarations in $filename";
  }
  return @ppts;
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
