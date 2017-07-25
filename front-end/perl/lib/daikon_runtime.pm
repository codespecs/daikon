package daikon_runtime;

# This is the package of routines that Daikon-annotated Perl code
# uses.

# This file is part of the Daikon distribution. It may be used and
# redistributed under the same terms as the rest of Daikon or, at your
# option, under the same terms as Perl itself, following either the
# GNU General Public License or the Perl Artistic License.  The Daikon
# Perl front end, of which this file is a part, and the Daikon dynamic
# invariant detection tool are separate programs, neither derived from
# the other, which are merely aggregated for convenience of
# distribution. As such, licensing the Perl front end under the terms
# of the GPL neither requires nor entitles you to license other parts
# of the Daikon distribution under the same terms.

# Among the 5.6 features we use is three-argument open. Note that for
# backwards compatilibity it would be silly to say "use 5.6.0", since
# that version syntax wasn't introduced until 5.6.
use 5.006;

use Exporter;

use strict;
#use warnings;

use Carp qw(carp cluck croak confess);
use IO::File;
use File::Path 'mkpath'; # like "mkdir -p"
use File::Basename;

use vars '@EXPORT_OK';
use vars '@ISA';

@ISA = 'Exporter';
@EXPORT_OK = qw(trace trace_enter trace_return);

use Daikon::PerlType qw(LIST_LIMIT type_lub unparse_type
                        guess_type_ref guess_type_list set_limits);

use Daikon::Output qw(declare_var output_var);

# indexed by {package name}. When tracing, holds the filehandle for
# the .dtrace file. When typing, holds a dummy value to remind us to
# output types at the end of the run.
my %filehandles = ();

# indexed by {package_name}{program point name}{variable name}
my %types;

# indexed by {package_name}{program point name}, gives an array of the
# variable names in that PPT, in the order in which they will appear
# in the dtrace file.
my %order;

# indexed by {types|dtrace}{basedir|style|append|combined_fh}
my %output_style;

# True, if we have types and are ready to make a .dtrace file.
# Defined but false, if we're tracing untyped programs just to get
# their types.
# Intially undefined.
my $tracing = undef;

sub set_output_style {
    my (@styles) = @_;
    my $i = 0;
    for my $kind ('types', 'dtrace') {
        for my $info ('basedir', 'style', 'append') {
            if (exists $output_style{$kind}{$info} and
                $output_style{$kind}{$info} ne $styles[$i]) {
                die "Inconsistent output specification: ".
                    "conflicting $info for $kind files.\n";
            }
            $output_style{$kind}{$info} = $styles[$i];
            $i++;
        }
    }
}

sub set_depths {
    my @depths = @_;
    Daikon::PerlType::set_limits(@depths);
}

sub set_tracing {
    my($arg_tracing) = @_;
    if (defined($tracing) and $tracing != $arg_tracing) {
        croak "Can't mix type-annotated and non-type-annotated code";
    } else {
        $tracing = $arg_tracing;
    }
}

sub program_name {
    my $name = $0;
    if ($name eq "-e") {
        # We aren't yet good at instrumenting -e scripts, but maybe in
        # the future...
        $name = "minus_e";
    } else {
        $name = basename($name, ".plx", ".pl", ".t");
    }
    return $name;
}

# Open (if it isn't alreay open) the file we'll use to output KIND
# data (where KIND is types, decls or dtrace) generated for the
# package PACKAGE. Returns a file handle to use, and a boolean value
# that tells if the caller should close the file when done writing, or
# leave it open for later.
sub open_for_output {
    my($package, $kind) = @_;
    my $dir = $output_style{$kind}{basedir};
    if ($package eq "main") {
        $package = program_name() . "-main";
    }
    my $fname = "$package.$kind";
    my $op;
    if ($output_style{$kind}{append} or $ENV{uc($kind)."APPEND"}) {
        $op = ">>";
    } else {
        $op = ">";
    }
    my $fh;
    my $close_when_done = 1;
    my $combined = 0;
    if ($output_style{$kind}{style} eq "tree") {
        if ($package =~ /::/) {
            (my $asdir = $package) =~ s[::][/]g;
            $dir .= "/" . dirname($asdir);
            $fname = basename($asdir) . ".$kind";
        }
    } elsif ($output_style{$kind}{style} eq "combined") {
        $fname = program_name() . "-combined.$kind";
        $combined = 1;
    }
    my $path = "$dir/$fname";
    if ($ENV{uc($kind)."FILE"}) {
        $path = $ENV{uc($kind)."FILE"};
        $combined = 1;
    }
    if ($combined and exists $output_style{$kind}{combined_fh}) {
        $fh = $output_style{$kind}{combined_fh};
        $close_when_done = 0;
    } else {
        if (not -d $dir) {
            mkpath($dir) or die "Couldn't create $dir: $!\n";
        }
        $fh = new IO::File;
        open($fh, $op, $path) or carp "Can't create $fname: $!";
        if ($combined) {
            $output_style{$kind}{combined_fh} = $fh;
            $close_when_done = 0;
        }
    }
    return ($fh, $close_when_done);
}

# Start tracing the subroutines in the given package. It's no longer
# necessary to call this explicitly, since the trace routines call it
# when they're first invoked in a given package.
sub begin_trace {
    my($package) = @_;
    if (exists $filehandles{$package}) {
        carp "Already tracing package $package\n";
        return;
    }
    if ($tracing) {
        my($fh, $close_when_done) = open_for_output($package, "dtrace");
        $filehandles{$package} = $fh;
    } else {
        $filehandles{$package} = 1;
    }
}

# Finish tracing the subroutines in the given package. This is called
# automatically from an END { } block. We wait until we're done to
# output the .decls file, and any type information we might have
# guessed. (For the type information, we have to wait because our
# guesses might need to be refined as we get more data. For the .decls
# file, we have to wait because we're being lazy and only generating
# declarations when we see a corresponding execution.)
sub end_trace {
    my($package) = @_;
#    warn "Finishing $package";
    if ($tracing and not exists $output_style{dtrace}{combined_fh}) {
        close $filehandles{$package};
    }
    delete $filehandles{$package};

    if (!$tracing and keys %{$types{$package}}) {
        my ($fh, $close_when_done) = open_for_output($package, "types");
        for my $ppt (keys %{$types{$package}}) {
            my @vars = @{$order{$package}{$ppt}};
            if (@vars == 0) {
                print $fh "$ppt seen-but-has-no-variables\n";
            } # else
            for my $var (@vars) {
                print $fh "$ppt $var ";
                print $fh unparse_type($types{$package}{$ppt}{$var});
                print $fh "\n";
            }
        }
        close $fh if $close_when_done;
    }
}

# Close out tracing and output .decls files for all the packages we
# started tracing.
sub end_trace_all {
    for my $p (keys %filehandles) {
        end_trace($p);
    }
    if (exists $output_style{types}{combined_fh}) {
        close $output_style{types}{combined_fh};
        delete $output_style{types}{combined_fh};
    }
    if (exists $output_style{dtrace}{combined_fh}) {
        close $output_style{dtrace}{combined_fh};
        delete $output_style{dtrace}{combined_fh};
    }
}

# At the end of regular execution, finish any unfinished tracing.
END { end_trace_all(); }

# This is a little bit strange. We'd like to have a flag that is set
# to 1 inside the tracing routines, and tells re-entrant invocations
# of them not to do anything, to avoid confusing the
# output. (Reentrant calls to trace occur for instance if a value
# we're tracing is actually an overloaded object with a stringify or
# numify method that we're also tracing. They don't represent a real
# part of the execution we're trying to capture, so we'd like to
# ignore them).  I'd like to use Perl's local() operator to set a flag
# to 1 in such a way that it will always be reset to zero when I leave
# the tracing routine, even by some exception or other non-local exit.
# Unfortunately, for reasons of avoiding confusing beginning
# programmers and of implementation simplification, Perl doesn't let
# you use local() on a my() variable. It does, however, let you use
# local() an element of any array, my() or not. Therefore I use the
# 0th element of the @trace_mutex array as if it were a variable
# $trace_mutex for this purpose.
my @trace_mutex = (0);

# The main routine to handle the tracing at a single program
# point. The name argument is something like "ENTER" or "EXIT42", and
# @vars is a list of variable specifications like ['$x', \$x, 'int']
# (variable name, reference to value, type).
sub trace_name {
    my($name, @vars) = @_;
    if ($trace_mutex[0]) {
        if ($ENV{"DAIKON_PERL_DEBUG_REENTRANT"}) {
            $trace_mutex[0] = 0; # Prevent looping during the backtrace
            cluck "Whoa! Re-entering trace!\n";
            $SIG{__DIE__} = 0;
            use POSIX;
            POSIX::_exit(0); # Really die, even in an eval
        } else {
            return;
        }
    }
    local($trace_mutex[0]) = 1;
    # These calls to caller() are the way we extract information about
    # where the trace routine was called, to save the caller from
    # passing it explicitly. caller(i) gives you information about the
    # invocation i-levels up the call stack, but the return convention
    # is a bit tricky. For a given level, $package and $line (really
    # the 0th and 2th elements) are the package and line number of the
    # caller of that subroutine, while $subname and $wantlist (3th and
    # 5th) are the name and the scalar/list context of the subroutine
    # itself. Thus we need two calls. Also we have to be careful that
    # trace_name is always the same number of levels down the stack
    # from the code whenever it's called. For the values below, this
    # works if the routine being traced calls foo() and foo() calls
    # trace_name().
    my(undef,    undef, undef, $subname, undef, $wantlist) = caller(2);
    my($package, undef, $line, undef,    undef, undef)     = caller(1);
    $subname =~ s/.*:://; # Remove leading package qualifiers
    if ($wantlist) {
        $subname .= "_l";
    } else {
        $subname .= "_s";
    }
#    my $javaish_package = $package;
#    $javaish_package =~ s/::/./g;

    # The parens in the next line are important to get some tools
    # (e.g. the tree GUI) to work right. I guess otherwise they can't
    # tell that this is a subroutine.
    my $ppt_name = "${package}.${subname}():::$name";
    if (not exists $filehandles{$package}) {
        begin_trace($package);
    }
    if ($tracing) {
        my $fh = $filehandles{$package};
        print $fh "$ppt_name\n";
        for my $v (@vars) {
            # Output a real value
            if ($v->[2] eq "unknown") {
                cluck("Found an unknown type while tracing!");
            }
            output_var($fh, @$v);
        }
        print $fh "\n";
    } else {
        # Do type inference rather than data collection this pass
        if (not exists $types{$package}{$ppt_name}) {
            # Make sure these exist, even if they're empty, to record
            # the fact that we saw this PPT.
            $types{$package}{$ppt_name} = {};
            $order{$package}{$ppt_name} = [];
            for my $v (@vars) {
                confess "Bad variable format" unless ref($v) eq "ARRAY";
                if (not exists $types{$package}{$ppt_name}{$v->[0]}) {
                    # Every variable starts out at the bottom type, since
                    # that's the identity for LUB.
                    $types{$package}{$ppt_name}{$v->[0]} = 'bottom';
                    push @{$order{$package}{$ppt_name}}, $v->[0];
                }
            }
        }
        for my $v (@vars) {
            croak "Found a type in an untyped file"
              if $v->[2] ne "unknown";
            # Type guessing
            my $t = $types{$package}{$ppt_name}{$v->[0]};
            if ($v->[0] eq "return" and $ppt_name =~ /_l\(\):::EXIT/) {
                # List return types are the only place lists appear
                $t = type_lub($t, guess_type_list($v->[1]));
            } else {
                # Other values are just single.
                my $this_type = guess_type_ref($v->[1])->[1];
                $t = type_lub($t, $this_type);
            }
            $types{$package}{$ppt_name}{$v->[0]} = $t;
        }
    }
}

# Trace the entrance to a subroutine. The arguments are a list of
# variable specs.
sub trace_enter { trace_name("ENTER", @_); }

# Trace the return from a subroutine. This is more complicated than
# entering because we need to distinguish scalar from list context,
# and different exit points from one another. We assume that
# "return(FOO)" will get rewritten into something like

# return trace_return((wantarray()?[FOO]:\scalar(FOO)),'int','(int),42,...)

# where FOO returns an int in scalar context and a list containing a
# single int in list ("array") context. Recall that there's no way to
# derive the list context return value from the scalar context value
# or vice versa, so we have to have code paths that capture both, but
# the FOO expression might also have side effects, so we only want
# to capture one value for each call to the subroutine. The scalar and
# list types are in the usual unparsed format; they may not always be
# as similar as they are the example above. "42" is a sequence number
# chosen by the annotator to be differed from all other returns in the
# same subroutine; otherwise it's arbitrary. Any other arguments are
# additional variables to trace the value of, as with the other trace
# routines.

my $include_exit_points = 0;

sub trace_return {
    my($ret_val_ref, $scalar_type, $list_type, $seq, @vars) = @_;
    my $point_type = "int";
    $point_type = "unknown"
      if $scalar_type eq "unknown" and $list_type eq "unknown";
    my @ep = ();
    @ep = (['daikonEXITpoint', \$seq, $point_type])
      if $include_exit_points;
    if ((caller(1))[5]) {
        # list context
        trace_name("EXIT$seq", @vars,
                   ['return', $ret_val_ref, $list_type], @ep);
        return @$ret_val_ref;
    } else {
        # scalar (or void) context
        trace_name("EXIT$seq", @vars,
                   ['return', $ret_val_ref, $scalar_type], @ep);
        return $$ret_val_ref;
    }
}

# This would be the routine to trace at points other than entrance and
# exit, but that isn't supported at the moment.
# sub trace { trace_name("LINE", @_); }

1;
