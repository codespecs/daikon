#!/usr/bin/env perl
# util_daikon.pm -- Perl utilities for the Daikon project.
# The externally-visible procedures are listed in the @EXPORT statement.

package util_daikon;
require 5.003;			# uses prototypes
require Exporter;
our @ISA = qw(Exporter);
our @EXPORT = qw( cleanup_pptname system_or_die backticks_or_die
                  escape_decl unescape_decl is_comment_line
                  record_kind record_reader for_each_record
                  parse_ppt_decl read_ppt_decls );

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

# Returns true if the argument, which is a line, is a comment.  Like
# FileIO.isComment in Daikon.
sub is_comment_line ( $ ) {
  my ($line) = @_;
  return $line =~ /\A(?:\/\/|#)/;
}

# Dies, saying that version 1 declarations are not supported.  The optional
# argument is the name of the file, for use in the error message.
sub die_version_1 ( ;$ ) {
  my ($filename) = @_;
  my $file = defined($filename) ? " $filename" : "";
  croak "Version 1 declarations are not supported; convert$file to version 2 format";
}

# Returns the kind of a record in a version 2 .decls or .dtrace file.  The
# argument is the first line of the record, which is not a comment.  The
# result is one of:
#   "ppt"                a program point declaration
#   "header"             decl-version, var-comparability, or input-language,
#                        each of which is a single line
#   "list-implementors"  ListImplementors, which extends to a blank line
#   "data"               anything else, which is a data trace record
# The tests are the same as in FileIO.read_data_trace_record in Daikon.
# Dies if the record is a version 1 declaration.  The optional second
# argument is the name of the file, for use in the error message.
# This is called on every record of a potentially huge trace file, so it
# uses a single regular expression.
sub record_kind ( $;$ ) {
  my ($line, $filename) = @_;
  if ($line !~ /\A(?:(ppt )|(decl-version|var-comparability|input-language)|(ListImplementors\r?\n?\z)|((?:DECLARE|VarComparability)\r?\n?\z))/) {
    return "data";
  }
  return "ppt" if defined($1);
  return "header" if defined($2);
  return "list-implementors" if defined($3);
  die_version_1($filename);
}

# Reading records.  Like FileIO.read_data_trace_record in Daikon, the
# decl-version, var-comparability, and input-language headers are a single
# line, comment lines at the start of a record are separate from it, and
# every other record extends to an empty line or end of file.  As in
# FileIO.read_ppt_decl, a program point declaration also ends at a line
# that contains only whitespace.
#
# A record is a reference to a hash with these keys:
#   comments  the comment lines that precede the record, or ""
#   kind      the kind of the record, as returned by record_kind; or "" if
#             the file ends after the comments
#   text      the lines of the record, without the empty line that
#             terminates it; or "" if kind is ""
#   line      the line number of the first line of the record
#
# For speed, the input is read a paragraph at a time rather than a line at
# a time.  A paragraph ends with an empty line, so it contains at most one
# record that is not a single-line header, and that record is last.

# Returns a new reader state for the file with the given name.
sub new_reader_state ( $ ) {
  my ($filename) = @_;
  return { filename => $filename, line => 1, comments => "" };
}

# Returns the records in the paragraph, which is the next one read from the
# file whose reader state is the first argument.  Updates the state.
sub paragraph_records ( $$ ) {
  my ($state, $paragraph) = @_;
  my @records = ();
  my $line = $state->{line};
  $state->{line} += ($paragraph =~ tr/\n//);
  while (1) {
    # Skip empty lines.
    if ($paragraph =~ s/\A(\n+)//) {
      $line += length($1);
    }
    last if $paragraph eq "";
    my $end = index($paragraph, "\n");
    my $first = ($end < 0) ? $paragraph : substr($paragraph, 0, $end + 1);
    my $kind;
    if (!is_comment_line($first)) {
      $kind = record_kind($first, $state->{filename});
    }
    if (!defined($kind) || $kind eq "header") {
      if (defined($kind)) {
        push @records, { comments => $state->{comments}, kind => $kind,
                         text => $first, line => $line };
        $state->{comments} = "";
      } else {
        $state->{comments} .= $first;
      }
      $paragraph = ($end < 0) ? "" : substr($paragraph, $end + 1);
      $line++;
      next;
    }
    my $text = $paragraph;
    $paragraph = "";
    if ($kind eq "ppt" && $text =~ /^[^\S\n]+(?:\n|\z)/m) {
      $paragraph = substr($text, $LAST_MATCH_END[0]);
      $text = substr($text, 0, $LAST_MATCH_START[0]);
    } else {
      chop($text) if substr($text, -2) eq "\n\n";
    }
    push @records, { comments => $state->{comments}, kind => $kind,
                     text => $text, line => $line };
    $state->{comments} = "";
    $line += ($text =~ tr/\n//) + 1;
  }
  return @records;
}

# Returns the records, if any, that remain at the end of the file whose
# reader state is the argument:  comments that no record follows.
sub end_of_file_records ( $ ) {
  my ($state) = @_;
  return () if $state->{comments} eq "";
  my $record = { comments => $state->{comments}, kind => "", text => "",
                 line => $state->{line} };
  $state->{comments} = "";
  return ($record);
}

# Returns a reader for the filehandle, which is a version 2 .decls or
# .dtrace file.  Each call to the reader returns the next record, or undef
# at end of file.  The optional second argument is the name of the file,
# for use in error messages.
sub record_reader ( $;$ ) {
  my ($fh, $filename) = @_;
  my $state = new_reader_state($filename);
  my @pending = ();
  my $at_eof = 0;
  return sub {
    while (!@pending && !$at_eof) {
      local $INPUT_RECORD_SEPARATOR = "\n\n";
      my $paragraph = <$fh>;
      if (defined($paragraph)) {
        @pending = paragraph_records($state, $paragraph);
      } else {
        @pending = end_of_file_records($state);
        $at_eof = 1;
      }
    }
    return shift @pending;
  };
}

# Calls the first argument, a subroutine, on each record (as described
# above) of each file named on the command line, or of standard input if
# there are none.  The files are read through the ARGV filehandle, so
# Perl's -i command-line option rewrites each file in place with whatever
# the subroutines print.  After the last record of each file, calls the
# optional second argument, a subroutine.  While the subroutines run, $ARGV
# is the name of the current file.
sub for_each_record ( $;$ ) {
  my ($process_record, $end_of_file) = @_;
  my $state;
  local $INPUT_RECORD_SEPARATOR = "\n\n";
  while (defined(my $paragraph = <ARGV>)) {
    $state = new_reader_state($ARGV) if !defined($state);
    foreach my $record (paragraph_records($state, $paragraph)) {
      $process_record->($record);
    }
    if (eof(ARGV)) {
      foreach my $record (end_of_file_records($state)) {
        $process_record->($record);
      }
      $end_of_file->() if defined($end_of_file);
      undef $state;
    }
  }
}

# The keys that may appear in a program point declaration, before the
# first variable and within a variable, as in FileIO.read_ppt_decl in
# Daikon.
my %ppt_keys = map { $_ => 1 } qw( parent flags ppt-type );
my %var_keys = map { $_ => 1 }
  qw( var-kind enclosing-var reference-type array function-args rep-type
      dec-type flags lang-flags parent comparability constant min-value
      max-value min-length max-length valid-values );

# Parses a version 2 program point declaration.  The first argument is the
# text of the record, as returned by a record reader.  Comment lines are
# skipped.  The optional second and third arguments are the name of the
# file and the line number of the record, for use in error messages.
# Dies if the declaration is malformed.
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
sub parse_ppt_decl ( $;$$ ) {
  my ($text, $filename, $line_number) = @_;
  my ($pptline, @lines) = split(/\n/, $text);
  # Returns the location of the line at the given offset from the start of
  # the record, for use in error messages.
  my $where = sub {
    my ($offset) = @_;
    my $result = defined($filename) ? $filename : "declaration";
    $result .= " line " . ($line_number + $offset) if defined($line_number);
    return $result;
  };
  $pptline =~ /\Appt\s+(.*?)\s*\z/s
    or croak "Not a program point declaration in " . $where->(0) . ": $pptline";
  my $ppt = { name => unescape_decl($1), parents => [], vars => [] };
  my $var;                      # the variable currently being read
  for (my $i = 0; $i < @lines; $i++) {
    my $line = $lines[$i];
    $line =~ s/\A\s+//;
    $line =~ s/\s+\z//;
    next if $line eq "" || is_comment_line($line);
    my ($key, $value) = split(/\s+/, $line, 2);
    my $error_prefix = "Malformed declaration of $ppt->{name} in " . $where->($i + 1);
    if ($key eq "variable") {
      defined($value) or croak "$error_prefix: variable has no name";
      $var = { name => unescape_decl($value), dec_type => "", rep_type => "",
               comparability => "", constant => undef };
      push @{$ppt->{vars}}, $var;
    } elsif (!defined($var)) {
      $ppt_keys{$key}
        or croak "$error_prefix: \"$key\" found where \"variable\", \"parent\", \"flags\", or \"ppt-type\" expected";
      if ($key eq "parent") {
        push @{$ppt->{parents}}, [split(/\s+/, defined($value) ? $value : "")];
      }
    } elsif (!$var_keys{$key}) {
      croak "$error_prefix: unexpected variable item \"$key\"";
    } elsif (!defined($value) && $key =~ /\A(?:dec-type|rep-type|comparability|constant)\z/) {
      croak "$error_prefix: \"$key\" has no value";
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
# version 2 .decls file.  Headers and comments are skipped.  Returns a list
# of references to hashes, as returned by parse_ppt_decl.  Dies if the file
# is in version 1 format, contains a data trace record, or contains no
# program point declarations.  The second argument is the name of the
# file, for use in error messages.
sub read_ppt_decls ( $$ ) {
  my ($fh, $filename) = @_;
  my @ppts = ();
  my $reader = record_reader($fh, $filename);
  while (defined(my $record = $reader->())) {
    if ($record->{kind} eq "ppt") {
      push @ppts, parse_ppt_decl($record->{text}, $filename, $record->{line});
    } elsif ($record->{kind} eq "data") {
      croak "Declaration files should not contain data trace records, but $filename does at line $record->{line}";
    }
  }
  if (!@ppts) {
    croak "No program point declarations in $filename";
  }
  return @ppts;
}


###########################################################################
### End of file
###

# Return true to indicate success loading this package.
1;
