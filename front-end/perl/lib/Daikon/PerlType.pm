package Daikon::PerlType;

# Routines to handle our slightly contrived notion of "types" for Perl
# values. We can guess a type for a particular runtime value by
# examining it, take the lub of two such types, or convert between the
# internal form and a string representaion.

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

use strict;

use vars ('@ISA', '@EXPORT_OK');
use Exporter;
@ISA = ('Exporter');

use Carp 'croak', 'confess', 'cluck', 'carp';

@EXPORT_OK = qw(DEPTH_LIMIT LIST_LIMIT
                parse_type unparse_type type_lub unparse_to_code
                guess_type_ref guess_type_scalar guess_type_array
                guess_type_hash guess_type_object guess_type_list
                is_zero set_limits read_types_into);

sub max { $_[0] > $_[1] ? $_[0] : $_[1] }
sub min { $_[0] < $_[1] ? $_[0] : $_[1] }

# How deep in terms of nested references to go in a linked data
# structure. We don't have any notion of recursive types, so this has
# to be finite to keep from getting stuck in reference loops, and it
# should probably be small because the size of the types we guess can
# potentially be exponential in it. In arbitrary units; the value
# 189000 was chosen because it has a lot of factors, which should
# reduce the chance of floating-point rounding problems.
sub DEPTH_LIMIT () { 189000 }

# How much traversing a scalar reference adds to our depth
my $ref_depth = DEPTH_LIMIT/6.1;
sub SCALAR_REF_DEPTH { $ref_depth; }

# How much looking at the output of an accessor method adds to our
# depth
my $accessor_depth = DEPTH_LIMIT/2.99;
sub ACCESSOR_METHOD_DEPTH { $accessor_depth; }

# The number of elements at the start of a list type to possibly treat
# as distinct entities. After this many, any remaining elements in the
# list go in one "bucket", so to speak.
my $list_limit = 3;
sub LIST_LIMIT { $list_limit }

# Modify the recursion limits (globally). At some point in the future,
# it would be nice to change this so that the limit can be set
# per-module.
sub set_limits {
    my($ref_lim, $accessor_lim, $list_lim) = @_;
    $ref_depth = DEPTH_LIMIT/$ref_lim;
    $accessor_depth = DEPTH_LIMIT/$accessor_lim;
    $list_limit = $list_lim;
}

# There are several basic scalar types:
#
# type           values
# ----           ------
# nothing        the undefined value, or the defined empty string
# bit            0 or 1
# bool           undef/0/"" for false, or 1 for true
# int            integers
# num            arbitrary floating-point numbers
# ref            references
# str            arbitrary scalars (which always have a stringified form)

# These obey bit <= int <= num <= str and ref <= str, where "<=" is
# the subtype relation. For each scalar type that doesn't include the
# empty string or undef, there's also a `lifted' variant that
# does. Generally these are formed with a "?" on the end like int?,
# ref?, num?, and str?. "bit?" is called bool, and "\T?" is written
# "\?T" to make the paser's job easier.

# There are also some type combiners. Let T be a type.

# \T is the type of references to values of type T. \T <= ref.

# [T] is the type of arrays, all of whose elements have type T. The
# elements of arrays are always scalars, so the most general array
# type is [str].

# {Tk=>Tv} is the type of hashes whose keys and values have the scalar
# types Tv and Th.

# <a:T1,b:T2,c:T3,..> is the type of blessed hashes (objects) of any
# class with elements (fields) a of type T1, b of type T2, etc. It's
# okay for any particular instance to be missing some fields, so the
# lub has the union of the field names, eaching having the lub of the
# corresponding types.

# (T1,T2,...,Ti,T*) is the type of a list whose first i elements have
# types T1, T2, ..., Ti, and remaining elements have type T. Lists
# only exist trasiently during expression evaluation, so they can't
# appear inside any other values. If you lub lists with different
# numbers of fixed elements (different values of "i"), the result has
# the smaller number, and all the extras are lub in at the end. This
# tries to capture the common case of returning ($a, $b, @foo), where
# @foo might have any number of elements. The T* part is optional.

# There are also "code" and "glob" types to represent the other kinds
# of data you can refer to; they have no internal structure.

# ------------------------------------------------------------------

# Internally, types are repesented in a subset of perl values
# isomorphic to Lisp s-expressions: array references whose elements
# are either other array references or one of a few strings. For instance,
# \{int=>\num} is represented as

# ['ref', ['hash', 'int', ['ref', 'num']]]

# As an exception, object types use a hash table to store the type
# corresponding to a particular field key. The full correspondance:

# bottom                  'bottom'
# bool                    'bool'
# ...
# ref                     ['ref', 'top']
# T?                      ['maybe', T]
# bool                    ['maybe', 'bit']
# nothing                 ['maybe', 'bottom']
# \T                      ['ref', T]
# \?T                     ['maybe', ['ref', T]]
# ref?                    ['maybe', ['ref', 'top']]
# {Tk=>Tv}                ['hash', Tk, Tv]
# <a:T1,b:T2;f->T3,g->T4> ['object', {'a' => T1, 'b' => T2},
#                                    {'f' => T3, 'g' => T4}]
# (T1, T2)                ['list', [T1, T2]]
# (T1, T2, T*)            ['list', [T1, T2], T]

# There is a generic bottom type named 'bottom', which is no longer
# the same thing as undef.  There's also a generic top type, called
# 'top', but it should never show up as a type itself, since scalars,
# arrays, hashes, code, globs, and lists are all syntactically
# segregated in Perl; there's no context where you might have more
# than one. References to all of these things (except lists) are
# possible as scalars, though, so the ref type turns into the
# equivalent of \top internally.

# Given an internal type representation, return a string representing
# it. Dies if the type is top or otherwise invalid.
sub unparse_type {
    my($t) = @_;
    if (not ref $t) {
        die "Can't print top" if $t eq "top";
        return $t; # 'int' unparses to 'int', etc.
    } else {
        if ($t->[0] eq "ref") {
            if ($t->[1] eq "top") {
                return "ref";
            } else {
                return "\\" . unparse_type($t->[1]);
            }
        } elsif ($t->[0] eq "maybe") {
            if ($t->[1] eq "bottom") {
                return "nothing";
            } elsif ($t->[1] eq "bit") {
                return "bool";
            } elsif (ref($t->[1]) and $t->[1]->[0] eq "ref") {
                if ($t->[1]->[1] eq "top") {
                    return "ref?";
                } else {
                    return "\\?" . unparse_type($t->[1]->[1]);
                }
            } elsif (not ref($t->[1])) {
                return $t->[1] . "?";
            } else {
                die "Invalid maybe type";
            }
        } elsif ($t->[0] eq "array") {
            return "[" . unparse_type($t->[1]) . "]";
        } elsif ($t->[0] eq "hash") {
            return "{" . unparse_type($t->[1]) .
                "=>" . unparse_type($t->[2]) . "}";
        } elsif ($t->[0] eq "object") {
            my @contents = ();
            for my $k (sort keys %{$t->[1]}) {
                push @contents,  $k . ":" . unparse_type($t->[1]{$k});
            }
            my @accessors = ();
            for my $k (sort keys %{$t->[2]}) {
                push @accessors, $k . "->" . unparse_type($t->[2]{$k});
            }
            my $acc = "";
            $acc = ";" . join(",", @accessors) if @accessors;
            return "<" . join(",", @contents) . "$acc>";
        } elsif ($t->[0] eq "list") {
            my @elts = ();
            foreach my $elt (@{$t->[1]}) {
                push @elts, unparse_type($elt);
            }
            if ($t->[2]) {
                push @elts, unparse_type($t->[2]) . "*";
            }
            return "(" . join(",", @elts). ")";
        } else {
            die "Invalid type object";
        }
    }
}

# Return a string of Perl code to construct a type representation
# like the argument. This doesn't handle 'object' types.

# sub unparse_to_code {
#     my($t) = @_;
#     if (not ref $t) {
#       return "'$t'";
#     } else {
#       return "[" . join(",",map(unparse_to_code($_), @$t)) . "]";
#     }
# }

# The main recursive routine for parsing the string representation of
# a type into our internal representation. Takes a string as an
# argument, and returns the representation of the first type named in
# the string, followed by the rest of the string. Dies on various
# syntax errors.
sub parse_type_from {
    my($s) = @_;
    if ($s =~ /^ref\?(.*)\z/) {
        return (['maybe', ['ref', 'top']]);
    } elsif ($s =~ /^(int|num|str|ref)\?(.*)\z/) {
        return (['maybe', $1], $2);
    } elsif ($s =~ /^(bottom|bit|int|num|str|code|glob)(.*)\z/) {
        return ($1, $2);
    } elsif ($s =~ /^ref(.*)\z/) {
        return (['ref', 'top'], $1);
    } elsif ($s =~ /^nothing(.*)\z/) {
        return (['maybe', 'bottom'], $1);
    } elsif ($s =~ /^bool(.*)\z/) {
        return (['maybe', 'bit'], $1);
    } elsif ($s =~ /^\\\?(.*)\z/) {
        my($subtype, $rest) = parse_type_from($1);
        return (['maybe', ['ref', $subtype]], $rest);
    } elsif ($s =~ /^\\(.*)\z/) {
        my($subtype, $rest) = parse_type_from($1);
        return (['ref', $subtype], $rest);
    } elsif ($s =~ /^\[(.*)\z/) {
        my($subtype, $rest) = parse_type_from($1);
        $rest =~ s/^\]// or die "expected ] in type";
        return (['array', $subtype], $rest);
    } elsif ($s =~ /^\{(.*)\z/) {
        my($key_type, $rest) = parse_type_from($1);
        $rest =~ s/^=>// or die "expected => in type";
        (my($val_type), $rest) = parse_type_from($rest);
        $rest =~ s/^\}// or die "expected } in type";
        return (['hash', $key_type, $val_type], $rest);
    } elsif ($s =~ /^\<(.*)\z/) {
        my $rest = $1;
        my %obj;
        while ($rest =~ /^(\w+):(.*)\z/) {
            my $key = $1;
            (my($val_type), $rest) = parse_type_from($2);
            $obj{$key} = $val_type;
            if ($rest !~ /^[;>]/) {
                $rest =~ s/^,// or die "expected , ; or > in type";
            }
        }
        my %acc;
        if ($rest =~ /^;(.*)\z/) {
            $rest = $1;
            while ($rest =~ /^(\w+)->(.*)\z/) {
                my $func = $1;
                (my($ret_type), $rest)= parse_type_from($2);
                $acc{$func} = $ret_type;
                if ($rest !~ /^>/) {
                    $rest =~ s/^,// or die "expected , or > in type";
                }
            }
        }
        $rest =~ s/^>// or die "expected field:type, ;, or > in type";
        return (['object', {%obj}, {%acc}], $rest);
    } elsif ($s =~ /^\((.*)\z/) {
        my $rest = $1;
        my @types = ();
        while (not $rest =~ /^[\)*]/) {
            (my($type), $rest) = parse_type_from($rest);
            push @types, $type;
            if ($rest !~ /^[\)*]/) {
                $rest =~ s/^,// or die "expected , * or ) in list type";
            }
        }
        my $type;
        if ($rest =~ /^\*(.*)\z/) {
            $rest = $1;
            my $extras = pop @types;
            $type = ['list', [@types], $extras];
        } else {
            $type = ['list', [@types]];
        }
        $rest =~ s/^\)// or die "expected ) in list type";
        return ($type, $rest);
    } else {
        #use Carp 'confess';
        die "Can't parse type, stuck at $s";
    }
}

# The entry point for our recursive-descent type parser. Removes
# whitespace to make the recursive routine's job easier, and checks
# that there's no extra junk left in the string after the type.
sub parse_type {
    my($s) = @_;
    $s =~ tr/ \t\n\f//d;
    my($type, $rest) = parse_type_from($s);
    if (length $rest) {
        die "Junk `$rest' after type";
    }
    return $type;
}

# Compute the least-upper-bound of two types in our internal representation.
sub type_lub {
    my($t1, $t2) = @_;
    return $t1 if $t1 eq $t2; # efficient special case
    return $t1 if $t2 eq "bottom"; # 'bottom' is the bottom
    return $t2 if $t1 eq "bottom";
    return "top" if $t1 eq "top" or $t2 eq "top"; # 'top' is the top
    # code and glob are incomparable with anything else
    return "top" if $t1 eq "code" xor $t2 eq "code";
    return "top" if $t1 eq "glob" xor $t2 eq "glob";
    my $maybe1 = (ref($t1) and $t1->[0] eq "maybe");
    my $maybe2 = (ref($t2) and $t2->[0] eq "maybe");
    if ($maybe1 or $maybe2) {
        my $ret = type_lub(($maybe1 ? $t1->[1] : $t1),
                           ($maybe2 ? $t2->[1] : $t2));
        return "top" if $ret eq "top"; # top? is top, because top is top.
        return ["maybe", $ret];
    }
    if (ref $t1 and ref $t2) {
        # all the other composite types are mutually incomparable
        return "top" if $t1->[0] ne $t2->[0];
        if ($t1->[0] eq "array") {
            return ["array", type_lub($t1->[1], $t2->[1])];
        } elsif ($t1->[0] eq "ref") {
            return ["ref", type_lub($t1->[1], $t2->[1])];
        } elsif ($t1->[0] eq "hash") {
            return ["hash", type_lub($t1->[1], $t2->[1]),
                    type_lub($t1->[2], $t2->[2])];
        } elsif ($t1->[0] eq "object") {
            # The keys of the lub object are the union of the keys
            my %h = (%{$t1->[1]}, %{$t2->[1]});
            foreach my $k (keys %h) {
                # For values that exist in both, lub them
                $h{$k} = type_lub($t1->[1]{$k}, $t2->[1]{$k})
                    if exists($t1->[1]{$k}) and exists($t2->[1]{$k});
            }
            my %acc_union = (%{$t1->[2]}, %{$t2->[2]});
            my %acc;
            foreach my $k (keys %acc_union) {
                $acc{$k} = type_lub($t1->[2]{$k}, $t2->[2]{$k})
                    if exists($t1->[2]{$k}) and exists($t2->[2]{$k});
            }
            return ["object", {%h}, {%acc}];
        } elsif ($t1->[0] eq "list") {
            my($s1, $s2) = (1 + $#{$t1->[1]}, 1 + $#{$t2->[1]});
            if ($s2 < $s1) {
                # Assume WLOG that $s1 <= $s2
                ($t1, $t2) = ($t2, $t1);
                ($s1, $s2) = ($s2, $s1);
            }
            my @fixed = ();
            for my $i (0 .. $s1 - 1) {
                # lub the elements that fixed in both elementwise
                push @fixed, type_lub($t1->[1][$i], $t2->[1][$i]);
            }
            if ($t1->[2] or $t2->[2] or $s1 != $s2) {
                # The T* of $t1, if any...
                my $extra = $t1->[2] || 'bottom';
                for my $i ($s1 .. $s2 - 1) {
                    # ... and the extra fixed types of the longer list
                    $extra = type_lub($extra, $t2->[1][$i]);
                }
                if ($t2->[2]) {
                    # ... and the T* of $t2, if any...
                    # lub together to give the T* of the lub
                    $extra = type_lub($extra, $t2->[2]);
                }
                return ['list', [@fixed], $extra];
            } else {
                return ['list', [@fixed]];
            }
        } else {
            die "Invalid type object";
        }
    } elsif (!ref $t1 and !ref $t2) {
        # The scalar types form a chain
        return "str" if $t1 eq "str" or $t2 eq "str";
        return "num" if $t1 eq "num" or $t2 eq "num";
        return "int" if $t1 eq "int" or $t2 eq "int";
        return "bit" if $t1 eq "bit" or $t2 eq "bit";
        die "Invalid type object";
    } else {
        # The only relationship between the simple and composite types
        # is that references are scalars
        return "str" if ref($t1) and $t1->[0] eq "ref";
        return "str" if ref($t2) and $t2->[0] eq "ref";
        return "top";
    }
}

# Guess a type for a reference to an object. This is the most general
# entry point to the guessing routines, handling everything except
# lists.
sub guess_type_ref {
    my($r) = @_;
    die unless ref($r);
    my $t;
    if (UNIVERSAL::isa($r, "CODE")) {
        $t = "code";
    } elsif (UNIVERSAL::isa($r, "GLOB")) {
        $t = "glob";
    } elsif (UNIVERSAL::isa($r, "ARRAY")) {
        $t = guess_type_array($r);
    } elsif (UNIVERSAL::isa($r, "HASH")) {
        if (ref($r) eq "HASH") {
            # unblessed
            $t = guess_type_hash($r);
        } else {
            # blessed
            $t = guess_type_object($r);
        }
    } elsif (UNIVERSAL::isa($r, "SCALAR") or UNIVERSAL::isa($r, "REF")
             or UNIVERSAL::isa($r, "LVALUE")) {
        # A quirk of the internal routine that implements ref() and
        # UNIVERSAL::isa is that it calls references to references
        # "REF" references rather than "SCALAR" references (even
        # though references are scalars just as much as numbers and
        # strings are), and exposes an internal distinction that
        # certain scalars have an lvalue magic that makes funny things
        # happen when you assign to them ($r = \substr($s, 0, 1); $$r
        # = "hah!";). These all count as scalars for our purposes.
        $t = guess_type_scalar($$r);
    } else {
        die "Can't figure out the type";
    }
    return ['ref', $t];
}

# We guess that the type of an array (passed by reference) is array-of
# the lub of the types of the elements.
sub guess_type_array {
    my($ar) = @_;
    my $elt_t = "bottom";
    for my $idx (0 .. $#$ar) {
        $elt_t = type_lub($elt_t, guess_type_scalar($ar->[$idx]));
    }
    return ['array', $elt_t];
}

# Like guess_type_array, but we guess separate types for the keys and
# the values.
sub guess_type_hash {
    my($hr) = @_;
    my($key_t, $value_t) = ("bottom", "bottom");
    my($k, $v);
    while (($k, $v) = each %$hr) {
        $key_t = type_lub($key_t, guess_type_scalar($k));
        $value_t = type_lub($value_t, guess_type_scalar($v));
    }
    return ['hash', $key_t, $value_t];
}

{
    # A static variable, to avoid passing it around all the guessing routines
    my $depth = 0;

    # For an object, we guess separate types for each field.
    sub guess_type_object {
        my($hr) = @_;
        # We only look at the fields whose names look like identifiers,
        # for two reasons:
        # 1. It would be a pain to include arbitrary strings in our
        # printed type representation
        # 2. Weirdly named keys are more likely to be used for strange,
        # un-field-like purposes.
        my @fields = grep(/^\w+\z/, keys %$hr);
        my $field_types = {map(($_ => guess_type_scalar($hr->{$_})), @fields)};
        my @accessors;
        if ($hr->can("DAIKON_ACCESSORS")) {
            @accessors = $hr->DAIKON_ACCESSORS();
        }
        $depth += ACCESSOR_METHOD_DEPTH;
        my $acc_types =
          {map(($_ => guess_type_scalar(scalar($hr->$_()))),
               @accessors)};
        $depth -= ACCESSOR_METHOD_DEPTH;
        return ['object', $field_types, $acc_types];
    }

    sub guess_type_scalar {
        my($s) = @_;
        return ['maybe', 'bottom'] if not defined $s; # nothing
        if (ref $s) {
            if ($depth > DEPTH_LIMIT) {
                # Botttom out by remaining ignorant about what this is
                # a reference to.
                return ['ref', 'top'];
            } else {
                my $t;
                $depth += SCALAR_REF_DEPTH();
                $t = guess_type_ref($s);
                $depth -= SCALAR_REF_DEPTH();
                return $t;
            }
        }
        # The way we recognize numbers is a little tricky, since we're
        # trying to reintroduce a distinction that Perl deliberately
        # abstracts away. This also works slightly differently than it
        # used to, since on futher consideration I've decided that a
        # float followed by other junk should count as a 'str', not a
        # 'num'. This means we can't use the compare-to-zero trick,
        # and instead must resort to defining the set of legal numeric
        # values as strings with a regex. -SMcC
        no warnings 'numeric'; # for $s eq int($s) test
        if (not length $s) {
            # The empty string is also nothing.
            return ['maybe', 'bottom'];
        } elsif ($s eq "0" or $s eq "1") {
            return 'bit';
        } elsif ($s =~ /^-?\d{1,15}$/) {
            # Note this includes numbers that Perl internally
            # represents with a double, as long as they're small
            # enough to be represented exactly.
            # The test "$s eq int($s)" would be equivalent, except for
            # the presence of leading zeros.
            # The constant 15 is right for IEEE doubles because IEEE
            # doubles have 53 bits of mantissa, and 2**53 =~=
            # 9*10**15.
            return 'int';
        } elsif ($s =~ /^[\d+-.]/) {
            # This regex is guarded by the fast one above, since it
            # looks ugly.  It may not be that bad in practice, though;
            # I should benchmark it.
            # This might also be an opportune place to sneak a look at
            # the internal NOK flag, somehow. That could only be a
            # fast yes test, though, not a fast no, since to match the
            # expectation of Perl programmers, a value whose string
            # representation is a number should count as a number even
            # if you haven't done any numeric operations on it:
            # consider the case of reading an ASCII float from a file.
            if ($s =~
                /^(?:[+-]?)(?=\d|\.\d)\d*(?:\.\d*)?(?:[Ee](?:[+-]?\d+))?$/) {
                return 'num';
            } else {
                return 'str';
            }
        } else {
            return 'str';
        }
    }
}

# Guess a type for a list, passed in an array reference. Seeing a
# single value, we can't tell which elements are fixed an which are
# variable, so we just assume that they're all fixed up to a
# predetermined limit.
sub guess_type_list {
    my($ar) = @_;
    my $fixed_len = min($#$ar + 1, LIST_LIMIT);
    my @fixed = map(guess_type_scalar($_), @$ar[0 .. $fixed_len - 1]);
    if (@$ar > LIST_LIMIT) {
        my $extra = 'bottom';
        for my $i ($fixed_len .. $#$ar) {
            $extra = type_lub($extra, guess_type_scalar($ar->[$i]));
        }
        return ['list', [@fixed], $extra];
    }
    return ['list', [@fixed]];
}

# Given a string, return a boolean value that's true if the string
# looks like a numeric zero, and false otherwise.
sub is_zero {
    my($s) = @_;
    return $s =~ /^-?(0+\.?0*|0*\.0+)([eE][-+]?(\d+))?\z$/;
}

# Read the .types file opened of $fh, and store the types into the
# hash table referenced by $types_r, which should be a nested hash
# indexed first by program point and second by variable name.
# The types file might have multiple entries for a given <program
# point, variable> pair (for instance, it might be the result of
# appending the types for several runs), which we just lub
# together.

# We also do some lubbing together of type information between places
# that we don't distinguish between while tracing; in particular,
# Daikon expects a variable to always have the same type, so we have
# to cater to that. In the .types file, we keep a separate type for a
# variable at the ENTER program point and at each EXIT program point,
# as well as separately for the scalar and list context versions of a
# sub. For every variable except "return", though, we merge all of
# these together, in a fake program point named
# "...foo():::UNION". For the sake of "return", we also merge all the
# scalar :::EXIT PPTs into a "...foo_s():::EXIT" PPT, and similarly
# for all the list exit points. (Note that this combinining isn't
# related to the combining of numbered exit points into a single
# ":::EXIT" program point that Daikon itself does, except that we use
# the same name).
sub read_types_into {
    my($fh, $types_r, $order_r) = @_;
    my $l;
    while ($l = <$fh>) {
        my($ppt, $var, $type) = split(' ', $l);
        if ($var eq "seen-but-has-no-variables") {
            $order_r->{$ppt} = [];
            $types_r->{$ppt} = {};
            next;
        }
        my $parsed = parse_type($type);
        if (exists $types_r->{$ppt}{$var}) {
            my $t = $types_r->{$ppt}{$var};
            $t = type_lub($t, $parsed);
            $types_r->{$ppt}{$var} = $t;
        } else {
            $types_r->{$ppt}{$var} = $parsed;
            push @{$order_r->{$ppt}}, $var if $order_r;
        }
        my $union_ppt = $ppt;
        $union_ppt =~ s/_[sl]\(\)/()/;
        $union_ppt =~ s/:::.*$/:::UNION/;
        if (not exists $types_r->{$union_ppt}{$var}) {
            $types_r->{$union_ppt}{$var} = 'bottom';
        }
        $types_r->{$union_ppt}{$var} = type_lub($types_r->{$union_ppt}{$var},
                                                $types_r->{$ppt}{$var})
          unless $var eq "return";
        if ($ppt =~ /:::EXIT/) {
            my $exit_union_ppt = $ppt;
            $exit_union_ppt =~ s/:::EXIT(\d+)/:::EXIT/;
            if (not exists $types_r->{$exit_union_ppt}{$var}) {
                $types_r->{$exit_union_ppt}{$var} = 'bottom';
            }
            $types_r->{$exit_union_ppt}{$var} =
                type_lub($types_r->{$exit_union_ppt}{$var},
                         $types_r->{$ppt}{$var});
        }
    }
}

# Say perl -MDaikon::PerlType -e 'Daikon::PerlType::test_guess()'
# to play with the guessing routines in a read-eval-print-loop way.
sub test_guess {
    my $lub = "bottom";
    $|=1;
    while (<>) {
        if (/^\s*$/) {
            $lub = "bottom";
            next;
        }
        my $type = guess_type_list([eval $_]);
        print unparse_type($type);
        $lub = type_lub($lub, $type);
        print ", lub is ", unparse_type($lub), "\n";
    }
}

# Say perl -MDaikon::PerlType -e 'Daikon::PerlType::test_parse()'
# to play with the parsing routines in a read-eval-print-loop way.
sub test_parse {
    my $lub = "bottom";
    while (<>) {
        if (/^\s*$/) {
            $lub = "bottom";
            next;
        }
        my $type = parse_type($_);
        $lub = type_lub($lub, $type);
        print unparse_type($type), ", lub is ", unparse_type($lub), "\n";
    }
}

1;
