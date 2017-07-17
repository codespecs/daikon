package B::DeparseDaikon;

# After Perl has parsed and byte-compiled some code, translate the OPs
# back into Perl source, adding tracing functions for Daikon along the
# way.

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

use B::Deparse;

# OPpCONST_ARYBASE and CVf_LOCKED removed from Perl > 5.10 (markro)
# use B qw(main_root main_start main_cv SVf_POK CVf_METHOD CVf_LOCKED
#          CVf_LVALUE OPpCONST_ARYBASE class);

use B qw(main_root main_start main_cv SVf_POK CVf_METHOD CVf_LVALUE class);

use Daikon::PerlType qw(parse_type unparse_type type_lub
                        unparse_to_code read_types_into);

use Carp 'croak', 'cluck', 'carp', 'confess';

@ISA = ("B::Deparse");

# We use this enough that I just copied it from Deparse.pm rather than say
# B::Deparse::null everywhere.
sub null {
    my $op = shift;
    return class($op) eq "NULL";
}

use strict;

# This could be any name that isn't used in the code being annotated.
# Something with Daikon in the name is a pretty good bet not to be
# used, except if we're trying to annotate some already annotated
# code, which won't work right anyway.
my $dv_name = '@Daikon_variables';

# indexed by {program point name}{variable name}
my %types;

# Read a .types file from the given filename, and incorporate that
# information into %types for use in annotating.
sub read_types {
    my($fname) = @_;
    open(TYPES, "<$fname") or die "Can't open $fname: $!";
    read_types_into(\*TYPES, \%types); # in Daikon::PerlType
    close TYPES;
}

# Read and return the method names in a given file, skipping blank
# lines and comments.
sub read_accessors {
    my($fname) = @_;
    open(ACC, "<$fname") or die "Can't open $fname: $!";
    my $l;
    my @acc = ();
    while ($l = <ACC>) {
        $l =~ s/#.*$//;
        next if $l =~ /^\s*$/;
        chomp $l;
        if ($l !~ /^\w+$/) {
            warn "`$l' is a strange name for a method\n";
        }
        push @acc, $l;
    }
    close ACC;
    return @acc;
}

# This is the entry point called first when you run this module with
# -MO=DeparseDaikon. Mainly, it returns a subroutine to call after the
# program has been compiled (into an internal form by the perl
# interpreter)
sub compile {
    my(@args) = @_;
    my @output_style = ();
    my @accessors = ();
    my @depths = ();
    my $saw_types = 0;
    return sub {
        # Parse and remove any Daikon-specific args from @args
        for my $i (0 .. $#args) {
            next unless defined($args[$i]);
            if ($args[$i] eq "-a") {
                # -a: read a list of accessor methods from a given file
                my $fname = $args[$i+1];
                push @accessors, read_accessors($fname);
                $args[$i] = $args[$i+1] = undef;
            } elsif ($args[$i] eq "-t") {
                # -t: read type information from a given file
                my $fname = $args[$i+1];
                read_types($fname);
                $args[$i] = $args[$i+1] = undef;
                $saw_types = 1;
            } elsif ($args[$i] eq "-o") {
                # -o: write output to a given file, rather than STDOUT
                my $fname = $args[$i+1];
                open(STDOUT, ">", "$fname") or die "Can't open $fname: $!\n";
                $args[$i] = $args[$i+1] = undef;
            } elsif ($args[$i] eq "-O") {
                # -O: control how generated code outputs results
                @output_style = @args[$i + 1 .. $i + 6];
                @args[$i .. $i + 6] = (undef) x (1 + 6);
            } elsif ($args[$i] eq "-D") {
                # -D: set data structure tracing depths
                @depths = @args[$i + 1 .. $i + 3];
                @args[$i .. $i + 3] = (undef) x (1 + 3);
            }
        }
        @args = grep(defined($_), @args);
        my $self = B::DeparseDaikon->new(@args);
        # As the first thing in the output, put in a hook to our
        # tracing routines.
        print "use daikon_runtime;\n";
        if (@output_style) {
            print "BEGIN { daikon_runtime::set_output_style(";
            print join(", ", map(qq/"$_"/, @output_style));
            print "); }\n";
        }
        if (@depths) {
            print "BEGIN { daikon_runtime::set_depths(";
            print join(", ", @depths);
            print "); }\n";
        }
        print "BEGIN { daikon_runtime::set_tracing(",
          ($saw_types ? 1 : 0), "); }\n";
        $self->deparse_program();
        if (@accessors) {
            print "sub DAIKON_ACCESSORS { (";
            print join(", ", map(qq/"$_"/, @accessors));
            print "); }\n";
        }
    }
}

# Used to be unchanged, but now slightly modified to turn off "void
# context" warnings.
sub deparse_program {
    my $self = shift;
    # First deparse command-line args
    if (defined $^I) { # deparse -i
        print q(BEGIN { $^I = ).perlstring($^I).qq(; }\n);
    }
    ###
    if ($^W) {
        # deparse -w, or more likely for us the file said
        # "#!/bin/perl -w"

        # The following code turns off the "useless use of foo in void
        # context" warning, which we produce a lot for the scalar
        # context return of lists. Unlike the simpler "no warnings
        # 'void'", we turn all the other warnings back on, this time
        # in the lexically scoped way, so that we don't lose other
        # warnings, if the user requested them. Also, we still set $^W
        # to duplicate the effect of the -w switch for other modules,
        # if any, that don't use lexical warnings.
        print 'BEGIN { require warnings; ',
          'warnings->import(); warnings->unimport("void"); ',
            '$^W = 1; }', "\n";
    } else {
        # Even if the author of this file didn't ask for warnings, to
        # match the behavior of the uninstrumented program we still
        # want to emit warnings if we're being used from another file
        # that had -w set. The following code only works if $^W was
        # set before this file was loaded, but that's the most common
        # case, covering both a -w flag on the main program and a
        # BEGIN { $^W = 1 } added by DeparseDaikon operating on
        # another file from the above branch.
        print 'BEGIN { if ($^W) { require warnings; ',
          'warnings->import(); warnings->unimport("void"); ',
            '} }', "\n";
    }
    ###

    if ($/ ne "\n" or defined $O::savebackslash) { # deparse -l and -0
        my $fs = perlstring($/) || 'undef';
        my $bs = perlstring($O::savebackslash) || 'undef';
        print qq(BEGIN { \$/ = $fs; \$\\ = $bs; }\n);
    }
    my @BEGINs  = B::begin_av->isa("B::AV") ? B::begin_av->ARRAY : ();
    my @CHECKs  = B::check_av->isa("B::AV") ? B::check_av->ARRAY : ();
    my @INITs   = B::init_av->isa("B::AV") ? B::init_av->ARRAY : ();
    my @ENDs    = B::end_av->isa("B::AV") ? B::end_av->ARRAY : ();
    for my $block (@BEGINs, @CHECKs, @INITs, @ENDs) {
        $self->todo($block, 0);
    }
    $self->stash_subs();
    $self->{'curcv'} = main_cv;
    $self->{'curcvlex'} = undef;
    print $self->print_protos;
    @{$self->{'subs_todo'}} =
        sort {$a->[0] <=> $b->[0]} @{$self->{'subs_todo'}};
    print $self->indent($self->deparse(main_root, 0)), "\n"
        unless null main_root;
    my @text;
    while (scalar(@{$self->{'subs_todo'}})) {
        push @text, $self->next_todo;
    }
    print $self->indent(join("", @text)), "\n" if @text;

    # Print __DATA__ section, if necessary
    no strict 'refs';
    my $laststash = defined $self->{'curcop'}
        ? $self->{'curcop'}->stash->NAME : $self->{'curstash'};
    if (defined *{$laststash."::DATA"}{IO}) {
        print "__DATA__\n";
        print readline(*{$laststash."::DATA"});
    }
}

# Mainly unchanged
sub todo {
    my $self = shift;
    my($cv, $is_form) = @_;
    ###
    # Throw out subs from other files, but (changed from 5.8 version)
    # keep constant subs that show up as being from "op.c"
    return unless ($cv->FILE eq $0 || exists $self->{files}{$cv->FILE}
		  || $cv->FILE eq "op.c");
    ###
    my $seq;
    if (!null($cv->START) and B::Deparse::is_state($cv->START)) {
        $seq = $cv->START->cop_seq;
    } else {
        $seq = 0;
    }
    push @{$self->{'subs_todo'}}, [$seq, $cv, $is_form];
    unless ($is_form || class($cv->STASH) eq 'SPECIAL') {
        $self->{'subs_deparsed'}{$cv->STASH->NAME."::".$cv->GV->NAME} = 1;
    }
}

# This is the main driver routine that's called recursively on every
# OP in the tree, and returns a representation of that OP as a
# slightly marked-up string. We piggyback a test here to find OPs
# whose next OP is the leavesub at the end of a subroutine (perhaps
# with intervening leave(loop|try)s for enclosing blocks) to wrap
# trace_return()s around them.
sub deparse {
    my $self = shift;
    my($op, $cx, $flags) = @_;

    Carp::confess("Null op in deparse") if !defined($op)
                                        || class($op) eq "NULL";
    my $meth = "pp_" . $op->name;
    my $add_trace_return = 0;
    if ($self->{'leavesub'}) {
        # We're on the lookout for an OP whose next OP (roughly) is
        # $self->{'leavesub'}.
        my $o = $op;
        $o = $o->next while !null($op->next) and
            $o->next->name =~ /^leave(loop|try)?$/;
        if (${$o->next} == ${$self->{'leavesub'}}) {
            $self->{'returns_count'}++ if $meth eq "pp_return";
            if ($meth !~ /^pp_(return|leave(loop|try)?)$/) {
                # This is the one we want.
                $add_trace_return = 1;
            }
        }
    }
    if ($add_trace_return and $cx == 0) {
        # Put the return value in a low-precedence (since we're going
        # to parenthesize it) but non-statement context. For instance,
        # we want to get "a and b" rather than "b if a".
        $cx = 1;
    }
    local($self->{'leavesub'}) = "" if $add_trace_return; # avoid nesting
    my @flags = ();
    if (B::Deparse::is_scope($op)) {
        @flags = ($flags);
    }
    my $val = $self->$meth($op, $cx);
    if ($add_trace_return) {
        $self->{'returns_count'}++;
        # Wrap it.
        return $self->make_trace_return($val, $op);
    } else {
        return $val;
    }
}

# Given the string representing a piece of code and the OP that
# generated that, return a string in which the code is appropriately
# wrapped in a call to trace_return().
sub make_trace_return {
    my $self = shift;
    my($val, $op) = @_;
    my $ppt = $self->{'ppt_base'};
    $ppt .= ":::EXIT";
    (my $ppt_s = $ppt) =~ s/\(\)/_s()/;
    (my $ppt_l = $ppt) =~ s/\(\)/_l()/;

    my $scalar_type;
    if (exists $types{$ppt_s}{"return"}) {
        $scalar_type = unparse_type($types{$ppt_s}{"return"});
    } else {
        $scalar_type = "unknown";
    }
    my $list_type;
    if (exists $types{$ppt_l}{"return"}) {
        $list_type = unparse_type($types{$ppt_l}{"return"});
    } else {
        $list_type = "unknown";
    }
    # We use scalar() to ensure we're forcing scalar context before
    # taking the reference. This is needed because the \ operator
    # doesn't itself force scalar context. (In fact \(@a), note the
    # parens, is the same as map(\$_, @a)!)
    my $scalar = "\\scalar($val)";
    # For the list context version, just wrap the list in an anonymous
    # array.
    my $list = "[$val]";
    # Name this exit point by its op sequence number relative to the
    # sequence number of the beginning of this sub. Note that the op
    # sequence number is only 16 bits, so it might wrap around on
    # large programs, causing weird looking but hopefully still
    # correct output. This number is easier to find on successive runs
    # than a line number, but it may change between different versions
    # of perl looking at the same code.

    # UNDONE:
    # $op->seq has been removed in Perl > 5.10.  I have not been able
    # to locate a suitable replacement as yet.  (markro)
#   my $seq = $op->seq - $self->{'op_seq_base'};
    my $seq = 0;
    # Evaluate only the appropriate version, depending on the runtime
    # context.
    my $ret_val = "(wantarray() ? $list : $scalar)";
    return "daikon_runtime::trace_return" .
        "($ret_val, '$scalar_type', '$list_type', $seq, $dv_name)";
}

# Mainly unchanged from B::Deparse
sub next_todo {
    my $self = shift;
    my $ent = shift @{$self->{'subs_todo'}};
    my $cv = $ent->[1];
    my $gv = $cv->GV;
    my $name = $self->gv_name($gv);
    ####
    # save the name of the subroutine for later use
    local($self->{'sub_name'}) = $name;
    # But throw out our own subs (like our constants)
    return "" if $name =~ /^(B|Daikon)::/;
    ####
    if ($ent->[2]) {
        return "format $name =\n"
            . $self->deparse_format($ent->[1]). "\n";
    } else {
        $self->{'subs_declared'}{$name} = 1;
        if ($name eq "BEGIN") {
            my $use_dec = $self->begin_is_use($cv);
            if (defined ($use_dec)) {
                return () if 0 == length($use_dec);
                return $use_dec;
            }
        }
        my $l = '';
        if ($self->{'linenums'}) {
            my $line = $gv->LINE;
            my $file = $gv->FILE;
            $l = "\n\f#line $line \"$file\"\n";
        }
        return "${l}sub $name " . $self->deparse_sub($cv);
    }
}

# Deparse all but the name of a named or anonymous subroutine. The
# effort to catch the last statement in the subroutine is mainly
# dispatched from here.
sub deparse_sub {
    my $self = shift;
    my $cv = shift;
    my $proto = "";
    if ($self->{'sub_name'} =~ /^BEGIN|END|INIT|CHECK$/ or null $cv->ROOT) {
        # Don't try to trace special blocks, or undefined subs,
        # or constant subs
        return B::Deparse::deparse_sub($self, $cv, @_);
    }
    Carp::confess("NULL in deparse_sub") if !defined($cv)||$cv->isa("B::NULL");
    Carp::confess("SPECIAL in deparse_sub") if $cv->isa("B::SPECIAL");
    local $self->{'curcop'} = $self->{'curcop'};
    if ($cv->FLAGS & SVf_POK) {
        $proto = "(". $cv->PV . ") ";
    }

# CVf_LOCKED removed from Perl > 5.10 (markro)
#   if ($cv->CvFLAGS & (CVf_METHOD|CVf_LOCKED|CVf_LVALUE)) {
    if ($cv->CvFLAGS & (CVf_METHOD|CVf_LVALUE)) {
        $proto .= ": ";
        $proto .= "lvalue " if $cv->CvFLAGS & CVf_LVALUE;
#       $proto .= "locked " if $cv->CvFLAGS & CVf_LOCKED;
        $proto .= "method " if $cv->CvFLAGS & CVf_METHOD;
    }

    # The part of the program point name before the :::
    my $ppt_base = $cv->GV->STASH->NAME . "." . $cv->GV->SAFENAME . "()";
    local($self->{'ppt_base'}) = $ppt_base;

    # The lowest OP sequence number in this sub, to which all others
    # will be compared

    # UNDONE:
    # $op->seq has been removed in Perl > 5.10.  I have not been able
    # to locate a suitable replacement as yet.  (markro)
#   local($self->{'op_seq_base'}) = $cv->START->seq;
    local($self->{'op_seq_base'}) = 0;

    local($self->{'curcv'}) = $cv;
    local($self->{'curcvlex'});
    local(@$self{qw'curstash warnings hints'})
                = @$self{qw'curstash warnings hints'};
    my $body;
    # This is the op whose predecessors we're interested
    local($self->{'leavesub'}) = $cv->ROOT;
    # Keep track of the number of statements we wrap as being the
    # last in the sub, so that if we don't find any, we can put an
    # empty return at the end.
    local($self->{'returns_count'}) = 0;
    my $lineseq = $cv->ROOT->first;
    if ($lineseq->name eq "lineseq") {
        my @ops;
        for(my$o=$lineseq->first; $$o; $o=$o->sibling) {
            push @ops, $o;
        }
        {
            # We treat the top level sequence of statements in the
            # sub specially.
            local($self->{'sub_top_level'}) = 1;
            $body = $self->lineseq(undef, @ops).";";
        }
        if ($self->{'returns_count'} == 0) {
            # If all else faills, add a trace of an empty
            # return. This can happen, for instance, if the last
            # statement in the subroutine is a loop.
            $body .= "\n" . $self->make_trace_return("()", $cv->ROOT);
        }
        my $scope_en = $self->find_scope_en($lineseq);
        if (defined $scope_en) {
            my $subs = join"", $self->seq_subs($scope_en);
            $body .= ";\n$subs" if length($subs);
        }
    }
    else {
        #           $body = $self->deparse($cv->ROOT->first, 0);
        $body = "";
    }
    return $proto ."{\n\t$body\n\b}" ."\n";
}

# This predicate guesses whether its op argument is @_ or some related
# value containing our arguments.
sub is_args {
    my $self = shift;
    my($op) = @_;
    if ($op->name eq "rv2av") {
        # @_ itself?
        return $self->pp_rv2av($op, 0) eq '@_';
    } elsif ($op->name eq "null" and not null($op->first)
             and $op->first->name eq "null" and not null($op->first->first)
             and $op->first->first->name eq "aelemfast") {
        # $_[0], $_[1], etc.?
        return $self->gv_or_padgv($op->first->first)->SAFENAME
            eq "_";
    } elsif ($op->name eq "null" and not null($op->first)
             and not null($op->first->sibling)
             and $op->first->sibling->name eq "entersub") {
        # Subroutine call, check args recursively
        my $k = $op->first->sibling->first;
        return 0 unless $$k;
	if ($k->can("first")) {
	    # Usual subroutine case
	    $k = $k->first;
	} else {
	    # Can happen with methods; we're already at the right level
	}
        for (; $$k; $k = $k->sibling) {
            # If any of the args is @_, that counts.
            return 1 if $self->is_args($k);
        }
    } else {
        return 0;
    }
}

# This guesses whether a statement is an assignment whose right-hand
# side is @_ or some related value, which implies that its left-hand
# side is a list of our arguments.
sub is_arg_assign {
    my $self = shift;
    my($op) = @_;
    # only assignments count
    return 0 if $op->name ne "sassign" and $op->name ne "aassign";
    my $rhs = $op->first;
    my $lhs = $rhs->sibling;
    return 1 if $rhs->name eq "shift"; # my $self = shift;
    return 1 if $self->is_args($rhs); # my $x = $_[0];
    if ($rhs->name eq "null" and not null($rhs->first)
        and $rhs->first->name eq "pushmark"
        and $self->is_args($rhs->first->sibling)) {
        # my($x, $y) = @_;
        return 1;
    }
    return 0;
}

# Given an assignment op (for which is_arg_assign() would have
# returned true), get a list of the names of the my() (or local())
# variables declared in it.
sub get_args {
    my $self = shift;
    my($op) = @_;
    my $rhs = $op->first;
    my $lhs = $rhs->sibling;
    if ($lhs->name eq "padsv") {
        # my $x = ...
        my $name = $self->pp_padsv($lhs, 0);
        $name =~ s/^my //;
        return $name;
    } elsif ($lhs->name eq "null" and not null($lhs->first)) {
        if ($lhs->first->name eq "gvsv") {
            # local $x = (...)
            my $name = $self->pp_null($lhs, 0);
            $name =~ s/^local //;
            return $name;
        } elsif ($lhs->first->name eq "pushmark") {
            my @ret = ();
            # my($x, $y) = ...
            for (my $kid = $lhs->first->sibling; $$kid; $kid = $kid->sibling) {
                my $name = $self->deparse($kid, 0);
                $name =~ s/^(my|local) //;
                push @ret, $name;
            }
            return @ret;
        }
    }
    return ();
}

# In general, this routine handles OPs that represent a sequence of
# statements like a block, subroutine body, etc. We've messed with it
# in the particular case of the a subroutine body, to find the
# variables that look like arguments so we can trace them.
sub lineseq {
    my($self, $root, @ops) = @_;
    my($expr, @exprs);

    # We set $sub_top_level if we're the top-level subroutine body
    my $sub_top_level = $self->{'sub_top_level'};
    local($self->{'sub_top_level'}) = 0;

    my $out_cop = $self->{'curcop'};
    my $out_seq = defined($out_cop) ? $out_cop->cop_seq : undef;
    my $limit_seq;
    if (defined $root) {
        $limit_seq = $out_seq;
        my $nseq = $self->find_scope_st($root->sibling) if ${$root->sibling};
        $limit_seq = $nseq if !defined($limit_seq)
                           or defined($nseq) && $nseq < $limit_seq;
    }
    $limit_seq = $self->{'limit_seq'}
        if defined($self->{'limit_seq'})
        && (!defined($limit_seq) || $self->{'limit_seq'} < $limit_seq);
    local $self->{'limit_seq'} = $limit_seq;
    # $in_args is true as long as we've only seen statements that look
    # like argument assignments, and false afterwards.
    my $in_args = $sub_top_level;
    my @args = ();
    for (my $i = 0; $i < @ops; $i++) {
        $expr = "";
        if (B::Deparse::is_state $ops[$i]) {
            $expr = $self->deparse($ops[$i], 0);
            $i++;
            if ($i > $#ops) {
                push @exprs, $expr;
                last;
            }
        }
        if (!B::Deparse::is_state $ops[$i] and (my $ls = $ops[$i+1]) and
            !null($ops[$i+1]) and $ops[$i+1]->name eq "lineseq")
        {
            if ($ls->first && !null($ls->first)
                && B::Deparse::is_state($ls->first)
                && (my $sib = $ls->first->sibling)) {
                if (!null($sib) && $sib->name eq "leaveloop") {
                    push @exprs, $expr . $self->for_loop($ops[$i], 0);
                    $i++;
                    next;
                }
            }
        }
        if ($in_args) {
            if ($self->is_arg_assign($ops[$i])) {
                # If $ops[$i] is an argument assignment, snarf the
                # arguments
                push @args, $self->get_args($ops[$i]);
            } else {
                # Otherwise, we've reached the end of the arguments,
                # so output code to populate the @Daikon_variables
                # array and call trace_enter().
                $expr .= "my $dv_name = (";
                for my $arg (@args) {
                    my $type = $types{$self->{'ppt_base'}.":::UNION"}{$arg}
                        || "unknown";
                    $type = unparse_type($type);
                    # Escape all the backslashes used for reference
                    # type constuctors
                    $type =~ s/\\/\\\\/g;
                    $expr .= "['$arg', \\$arg, '$type'], ";
                }
                $expr .= ");\n";
                $expr .= "daikon_runtime::trace_enter($dv_name);\n";
                $in_args = 0;
            }
        }
        $expr .= $self->deparse($ops[$i], 0, (@ops != 1));
        $expr =~ s/;\n?\z//;
        push @exprs, $expr;
    }
    my $body = join(";\n", grep {length} @exprs);
    my $subs = "";
    if (defined $root && defined $limit_seq && !$self->{'in_format'}) {
        $subs = join "\n", $self->seq_subs($limit_seq);
    }
    return join(";\n", grep {length} $body, $subs);
}

# Mostly unmodified from B::Deparse
sub pp_return {
    my $self = shift;
    my($op, $cx) = @_;
    my(@exprs);
    my $kid = $op->first->sibling;
    if (null $kid) {
        ####
        # scalar() is an error (too few args to scalar), but
        # scalar(()) is undef. This is convenient, since return with
        # no arguments from a subroutine in scalar context also
        # produces undef. Thus we can treat "return;" as if it were
        # "return(());".
        @exprs = ("()");
        ####
    } else {
        push @exprs, $self->deparse($kid, 6);
        $kid = $kid->sibling;
        for (; !null($kid); $kid = $kid->sibling) {
            push @exprs, $self->deparse($kid, 6);
        }
    }
    my $val;
    $val = join(", ", @exprs);
    ####
    # Here we also add a trace_return
    return "return(" . $self->make_trace_return($val, $op) .")";
    ####
}

# Mostly unmodified from B::Deparse
sub pp_cond_expr {
    my $self = shift;
    my($op, $cx) = @_;
    my $cond = $op->first;
    my $true = $cond->sibling;
    my $false = $true->sibling;
    my $cuddle = $self->{'cuddle'};
    unless ($cx == 0 and (B::Deparse::is_scope($true)
                          and $true->name ne "null") and
            (B::Deparse::is_scope($false)
             || B::Deparse::is_ifelse_cont($false))
            and $self->{'expand'} < 7) {
        $cond = $self->deparse($cond, 8);
        # XXX Need to think about precedence and associativity more here
        # maybe a bug in Deparse (which had 8 on the next line)
        $true = $self->deparse($true, 6);
        $false = $self->deparse($false, 8);
        return $self->maybe_parens("$cond ? $true : $false", $cx, 8);
    }

    $cond = $self->deparse($cond, 1);
    $true = $self->deparse($true, 0);
    my $head = "if ($cond) {\n\t$true\n\b}";
    my @elsifs;
    my $newop;
    while (!null($false) and B::Deparse::is_ifelse_cont($false)) {
        $newop = $false->first;
        my $newcond = $newop->first;
        my $newtrue = $newcond->sibling;
        $false = $newtrue->sibling; # last in chain is OP_AND => no else
        $newcond = $self->deparse($newcond, 1);
        $newtrue = $self->deparse($newtrue, 0);
        push @elsifs, "elsif ($newcond) {\n\t$newtrue\n\b}";
    }
    if (!null($false)) {
        $false = $cuddle . "else {\n\t" .
            $self->deparse($false, 0) . "\n\b}\cK";
    } else {
        $false = "\cK";
        ####
        # When the last statement in a subroutine is an
        # if-elsif-elsif-...  with no else, we have to catch the last
        # elsif here because the relevant OP (an OP_AND) wouldn't
        # normally be passed to deparse().
        if ($newop and ${$newop->next} == ${$self->{'leavesub'}} ) {
            $false = "\n" . $self->make_trace_return("()", $newop);
        }
        ####
    }
    return $head . join($cuddle, "", @elsifs) . $false;
}

# Mostly unchanged
sub dq {
    my $self = shift;
    my $op = shift;
    my $type = $op->name;
    if ($type eq "const") {

# OPpCONST_ARYBASE removed from Perl > 5.10 (markro)
#       return '$[' if $op->private & OPpCONST_ARYBASE;
        return B::Deparse::uninterp(B::Deparse::escape_str(B::Deparse::unback($self->const_sv($op)->as_string)));
    } elsif ($type eq "concat") {
        my $first = $self->dq($op->first);
        my $last  = $self->dq($op->last);

	####
	# Fix in the next statement: handle "$a\::foo" correctly
        # This was also fixed (independently) in bleadperl #19127

        # Disambiguate "${foo}bar", "${foo}{bar}", "${foo}[1]"
        ($last =~ /^[A-Z\\\^\[\]_?]/ &&
            $first =~ s/([\$@])\^$/${1}{^}/)  # "${^}W" etc
            || ($last =~ /^([{\[\w_]|::)/ &&
                $first =~ s/([\$@])([A-Za-z_]\w*)$/${1}{$2}/);
	###

        return $first . $last;
    } elsif ($type eq "uc") {
        return '\U' . $self->dq($op->first->sibling) . '\E';
    } elsif ($type eq "lc") {
        return '\L' . $self->dq($op->first->sibling) . '\E';
    } elsif ($type eq "ucfirst") {
        return '\u' . $self->dq($op->first->sibling);
    } elsif ($type eq "lcfirst") {
        return '\l' . $self->dq($op->first->sibling);
    } elsif ($type eq "quotemeta") {
        return '\Q' . $self->dq($op->first->sibling) . '\E';
    } elsif ($type eq "join") {
        return $self->deparse($op->last, 26); # was join($", @ary)
    } else {
        return $self->deparse($op, 26);
    }
}

# This isn't a method in older versions, so we have to replace all the
# methods that call it.
sub const {
    my $self = shift;
    my($sv, $cx) = @_;
    if (class($sv) eq "SPECIAL") {
        # In the 5.8.0 version of Deparse, the string for sv_no was
        # '0', '""' isn't quite right either, in fact.
	# sv_undef, sv_yes, sv_no
	return ('undef', '1', $self->maybe_parens("!1", $cx, 21))[$$sv-1];
    } else {
        if ($B::Deparse::VERSION <= 0.63) {
            return B::Deparse::const($sv);
        } else {
            return $self->SUPER::const($sv, $cx);
        }
    }
}

# Unchanged, except for the call to "const"
sub pp_const {
    my $self = shift;
    my($op, $cx) = @_;

# OPpCONST_ARYBASE removed from Perl > 5.10 (markro)
#
#   if ($op->private & OPpCONST_ARYBASE) {
#       return '$[';
#   }
#    if ($op->private & OPpCONST_BARE) { # trouble with `=>' autoquoting
#       return $self->const_sv($op)->PV;
#    }
    my $sv = $self->const_sv($op);
#    return const($sv);
    my $c = $self->const($sv, $cx);
    return $c =~ /^-\d/ ? $self->maybe_parens($c, $cx, 21) : $c;
}

# Unchanged, except for the call to "const"
sub pp_rv2av {
    my $self = shift;
    my($op, $cx) = @_;
    my $kid = $op->first;
    if ($kid->name eq "const") { # constant list
        my $av = $self->const_sv($kid);
        return "(" . join(", ", map($self->const($_, $cx), $av->ARRAY)) . ")";
    } else {
        return $self->maybe_local($op, $cx, $self->rv2x($op, $cx, "\@"));
    }
 }



1;
