package B::DeparseDaikon;

use B::Deparse;

use B qw(main_root main_start main_cv SVf_POK CVf_METHOD CVf_LOCKED
         CVf_LVALUE class);

use Daikon::PerlType qw(parse_type unparse_type type_lub
			unparse_to_code);

@ISA = ("B::Deparse");

# We use this enough that I just copied it from Deparse.pm rather than say
# B::Deparse::null everywhere.
sub null {
    my $op = shift;
    return class($op) eq "NULL";
}

use strict;

# This could be any name that isn't used in the code being annotated.
my $dv_name = '@Daikon_variables';

# indexed by {program point name}{variable name}
my %types;

# Read a .types file from the given filename, and incorporate that
# information into %types for use in annotating. The types file might
# have multiple entries for a given <program point, varaiable> pair
# (for instance, it might be the result of appending the types for
# several runs), which we just lub together. We also union together
# the types from the _s and _l (scalar and list context) versions of a
# program point, and the different exit points of a single subroutine.
sub read_types {
    my($fname) = @_;
    open(TYPES, "<$fname") or die "Can't open $fname: $!";
    my $l;
    while ($l = <TYPES>) {
	my($ppt, $var, $type) = split(' ', $l);
	if (exists $types{$ppt}{$var}) {
	    my $t = $types{$ppt}{$var};
	    my $t2 = parse_type($type);
	    $t = type_lub($t, $t2);
	    $types{$ppt}{$var} = $t;
	} else {
	    $types{$ppt}{$var} = parse_type($type);
	}
	my $union_ppt = $ppt;
	$union_ppt =~ s/_[sl]\(\)/()/;
	if (not exists $types{$union_ppt}{$var}) {
	    $types{$union_ppt}{$var} = 'undef';
	}
	$types{$union_ppt}{$var} = type_lub($types{$union_ppt}{$var},
					    $types{$ppt}{$var});
	if ($ppt =~ /:::EXIT/) {
	    my $exit_union_ppt = $ppt;
	    $exit_union_ppt =~ s/:::EXIT(\d+)/:::EXIT/;
	    if (not exists $types{$exit_union_ppt}{$var}) {
		$types{$exit_union_ppt}{$var} = 'undef';
	    }
	    $types{$exit_union_ppt}{$var} =
		type_lub($types{$exit_union_ppt}{$var}, $types{$ppt}{$var});
	}
    }
    close TYPES;
}

# This is the entry point called first when you run this module with
# -MO=DeparseDaikon. Mainly, it returns a subroutine to call after the
# program has been compiled (into an internal form by the perl
# interpreter)
sub compile {
    my(@args) = @_;
    return sub {
	# Parse and remove any Daikon-specific args from @args
	for my $i (0 .. $#args) {
	    next unless defined($args[$i]);
	    if ($args[$i] eq "-t") {
		# -t: read type information from a given file
		my $fname = $args[$i+1];
		read_types($fname);
		$args[$i] = $args[$i+1] = undef;
	    }
	}
	@args = grep(defined($_), @args);
	my $self = B::DeparseDaikon->new(@args);
	# As the first thing in the output, put in a hook to our
	# tracing routines.
	print "use daikon_runtime;\n";
	$self->deparse_program();
   }
}

# This is a bunch of code that was factored out of the B::Deparse
# compile routine. To work with the 5.8.0 B::Deparse, we have to keep
# it here, but eventually it should go back to B::Deparse since
# there's nothing Daikon-specific about it.
sub deparse_program {
    my $self = shift;
    # First deparse command-line args
    if (defined $^I) { # deparse -i
        print q(BEGIN { $^I = ).perlstring($^I).qq(; }\n);
    }
    if ($^W) { # deparse -w
        print qq(BEGIN { \$^W = $^W; }\n);
    }
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
	$o = $o->next while $o->next->name =~ /^leave(loop|try)?$/;
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
    $ppt =~ s/\(\)/_s()/;
    # Name this exit point by its op sequence number relative to the
    # sequence number of the beginning of this sub. Note that the op
    # sequence number is only 16 bits, so it might wrap around on
    # large programs, causing weird looking but hopefully still
    # correct output. This number is easier to find on successive runs
    # than a line number, but it may change between different versions
    # of perl looking at the same code.
    my $seq = $op->seq - $self->{'op_seq_base'};
#    $ppt .= ":::EXIT$seq";
    $ppt .= ":::EXIT";
    my $scalar_type;
    if (exists $types{$ppt}{"return"}) {
	$scalar_type = unparse_type($types{$ppt}{"return"});
    } else {
	$scalar_type = "unknown";
    }
    $ppt =~ s/_s\(\)/_l()/;
    my $list_type;
    if (exists $types{$ppt}{"return"}) {
	$list_type = unparse_type($types{$ppt}{"return"});
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
    if ($self->{'sub_name'} =~ /^BEGIN|END|INIT|CHECK$/) {
	# Don't try to trace special blocks
	return B::Deparse::deparse_sub($self, $cv, @_);
    }
    Carp::confess("NULL in deparse_sub") if !defined($cv)||$cv->isa("B::NULL");
    Carp::confess("SPECIAL in deparse_sub") if $cv->isa("B::SPECIAL");
    local $self->{'curcop'} = $self->{'curcop'};
    if ($cv->FLAGS & SVf_POK) {
	$proto = "(". $cv->PV . ") ";
    }
    if ($cv->CvFLAGS & (CVf_METHOD|CVf_LOCKED|CVf_LVALUE)) {
        $proto .= ": ";
        $proto .= "lvalue " if $cv->CvFLAGS & CVf_LVALUE;
        $proto .= "locked " if $cv->CvFLAGS & CVf_LOCKED;
        $proto .= "method " if $cv->CvFLAGS & CVf_METHOD;
    }

    # The part of the program point name before the :::
    my $ppt_base = $cv->GV->STASH->NAME . "." . $cv->GV->SAFENAME . "()";
    local($self->{'ppt_base'}) = $ppt_base;

    # The lowest OP sequence number in this sub, to which all others
    # will be compared
    local($self->{'op_seq_base'}) = $cv->START->seq;

    local($self->{'curcv'}) = $cv;
    local($self->{'curcvlex'});
    local(@$self{qw'curstash warnings hints'})
		= @$self{qw'curstash warnings hints'};
    my $body;
    if (not null $cv->ROOT) {
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
#	    $body = $self->deparse($cv->ROOT->first, 0);
	    $body = "";
	}
    }
    else {
	my $sv = $cv->const_sv;
	if ($$sv) {
	    # no tracing for constant subroutines (yet?)
	    return $proto . "{ " . const($sv) . " }\n";
	} else { # XSUB? (or just a declaration)
	    return "$proto;\n";
	}
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
	for ($k = $k->first; $$k; $k = $k->sibling) {
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

# In general, this routine handles OPs which represent a sequence of
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
		    my $type = $types{$self->{'ppt_base'}.":::ENTER"}{$arg}
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
#    return  if null $kid;
    my $first;
    $first = $self->deparse($kid, 6);
     push @exprs, $first;
    $kid = $kid->sibling;
    for (; !null($kid); $kid = $kid->sibling) {
	push @exprs, $self->deparse($kid, 6);
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
        $true = $self->deparse($true, 8);
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

1;
