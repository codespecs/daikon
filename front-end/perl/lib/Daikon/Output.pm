package Daikon::Output;

# Routines for printing Perl values in the format that Daikon likes.

use strict;
use Exporter;
use Carp ('carp', 'cluck', 'croak', 'confess');
use vars qw(@ISA @EXPORT_OK);
@ISA = 'Exporter';
@EXPORT_OK = qw(declare_var output_var);

use Daikon::PerlType qw(parse_type is_zero read_types_into);

# Convert an integer (given a reference to it) to a string form,
# warning if it wasn't really an integer.
sub output_int {
    no warnings 'uninitialized';
    my($x_ref) = @_;
    confess "Arg to output must be a reference" unless ref($x_ref);
    my $x = $$x_ref;
    if ($x == 0 and length $x and !is_zero($x)) {
        carp "`$x' isn't a number, you said it was an int";
    }
    $x = $x + 0;
    if ($x != int($x)) {
        carp "`$x' isn't an integer, you said it was an int";
    }
    return sprintf("%d", $x);
}

sub output_num {
    no warnings 'uninitialized';
    my($x_ref) = @_;
    confess "Arg to output must be a reference" unless ref($x_ref);
    my $x = $$x_ref;
    if ($x == 0 and length $x and !is_zero($x)) {
        carp "`$x' isn't a number, you said it was an int";
    }
    $x = $x + 0;
    return sprintf("%g", $x);
}

# Like 0 + $x for a reference, but avoids calling overload routines
# if $x has them. The squeamish might want to avert their eyes.
# Based on overload::StrVal
sub address {
    my $package = ref $_[0];
    return 0 + $_[0] unless $package;
    return 0 + $_[0] if $package =~ /^SCALAR|REF|LVALUE|ARRAY|HASH\z/;
    bless $_[0], "overload::Fake";  # Non-overloaded package
    my $val =  0 + $_[0];
    bless $_[0], $package;          # Back
    return $val;
}

# Backslash escape a character (', ", \r, or \n) in the way that
# Daikon likes.
sub escape_char {
    my($c) = @_;
    if ($c eq "\n") {
        return '\\n';
    } elsif ($c eq "\r") {
        return '\\r';
    } elsif ($c eq '"' or $c eq '\\') {
        return "\\$c";
    } else {
        return $c;
    }
}

# Convert an arbitrary string into a quoted string in a
# Daikon-friendly format.
sub output_string {
    no warnings 'uninitialized';
    my($s_ref) = @_;
    my $s = $$s_ref;
    $s =~ s/([\\\"\r\n])/escape_char($1)/eg;
    return qq'"$s"';
}

# Given a type, figure out how to print it for Daikon.

# Returns a list with one element for each daikon-variable that'll
# correspond to this one perl variable.
# Each element in the list is a tuple:
# [$prefix, $suffix, $type, $rep_type, \&output]
# where $prefix is text to write at the beginning of the variable
# name, $suffix is text to write at the end, $type is the type to tell
# Daikon, $rep_type is the representation type to tell Daikon, and
# &output is a sub to convert the value (passed by reference) into a
# string.

# If the $single argument is true, we're in a context like an array
# element, so we can only return one value. Otherwise, we can return
# many specs, like one for each field of an object.

# This routine is still unfinished in places, but it works for simple
# cases.

# Note that when passing $prefix and $suffix up between recursive
# calls, you usually want to append to the prefix and prepend to the
# suffix, since the accessors for compound values work from the inside
# out.

sub daikon_output_spec {
    my($t, $single) = @_;
    if ($t eq "int") {
        return ["", "", "int", "int", \&output_int];
    } elsif ($t eq "bit") {
        return ["", "", "int", "int", \&output_int];
    } elsif ($t eq "num") {
        return ["", "", "double", "double", \&output_num];
    } elsif ($t eq "str") {
        # It seems that if you use strings in an array, Daikon works
        # better if you say the representation type is
        # "java.lang.String", though in other cases just "String"
        # works fine.
        return ["", "", "String", "java.lang.String", \&output_string];
    } elsif (ref($t)) {
        if ($t->[0] eq "array") {
            if ($single) {
                return ["\\", "", "Object", "int",
                        sub { output_int(\ (address($_[0]))); }];
            } else {
                my($prefix, $suffix, $type, $rep_type, $output) =
                    @{daikon_output_spec($t->[1], 1)};
                return [$prefix, "[]".$suffix, $type."[]", $rep_type."[]",
                        sub { "[" . join(" ", map($output->(\$_), @{$_[0]}))
                                  . "]"}];
            }
        } elsif ($t->[0] eq "hash") {
            my @specs;
            my($kprefix, $ksuffix, $ktype, $krep_type, $koutput) =
                @{daikon_output_spec($t->[1], 1)};
            push @specs, [$kprefix, ".keys".$ksuffix, $ktype."[]",
                          $krep_type."[]",
                          sub { "[" . join(" ", map($koutput->(\$_),
                                                    keys %{$_[0]})) . "]" }];
            my($vprefix, $vsuffix, $vtype, $vrep_type, $voutput) =
                @{daikon_output_spec($t->[2], 1)};
            push @specs, [$vprefix, ".values".$vsuffix, $vtype."[]",
                          $vrep_type."[]",
                          sub { "[" . join(" ", map($voutput->(\$_),
                                                    values %{$_[0]})) . "]" }];
            return @specs;
        } elsif ($t->[0] eq "list") {
            die if $single; # lists should only be at top level
            my $num_fixed = @{$t->[1]};
            my @specs;
            for my $index (0 .. $num_fixed - 1) {
                for my $spec (daikon_output_spec($t->[1][$index], 0)) {
                    my($prefix, $suffix, $type, $rep_type, $output) = @$spec;
                    push @specs, [$prefix, "[$index]".$suffix, $type,
                                  $rep_type,
                                  sub { $output->(\ ($_[0]->[$index])) }];
                }
            }
            if ($t->[2]) {
                my $rest = $t->[2];
                for my $spec (daikon_output_spec($rest, 1)) {
                    my($prefix, $suffix, $type, $rep_type, $output) = @$spec;
                    $rep_type = "java.lang.String" if $rep_type eq "String";
                    push @specs,
                        [$prefix, $suffix."[$num_fixed..][]", $type."[]",
                         $rep_type."[]",
                         sub { "[" .
                                   join(" ",
                                        map($output->(\$_),
                                            @{$_[0]}[$num_fixed .. $#{$_[0]}]))
                                       . "]" }];
                }
            }
            return @specs;
        } elsif ($t->[0] eq "object") {
            if ($single) {
                return ["\\", "", "Object", "int",
                        sub { output_int(\ (address(${$_[0]}))); }];
            } else {
                my @specs;
                # When this was enabled, I'd picked ".Class" to look
                # distinct from usual field names that are
                # lowercase. But Daikon treats ".class" specially, so
                # maybe it should be that.
#                push @specs, ["", ".Class", "String",
#                             "java.lang.String", sub { '"'. ref($_[0]).'"' }];
                for my $k (keys %{$t->[1]}) {
                    for my $spec (daikon_output_spec($t->[1]{$k})) {
                        my($prefix, $suffix, $type, $rep_type, $output)
                            = @$spec;
                        push @specs, [$prefix, ".$k$suffix", $type, $rep_type,
                                      sub { exists($_[0]->{$k}) ? 
                                                $output->(\ ($_[0]->{$k}))
                                            : $output->(\undef) }];
                    }
                }
                for my $k (keys %{$t->[2]}) {
                    for my $spec (daikon_output_spec($t->[2]{$k})) {
                        my($prefix, $suffix, $type, $rep_type, $output)
                            = @$spec;
                        push @specs, [$prefix, ".$k()$suffix", $type,
                                      $rep_type,
                                      sub { $output->(\scalar($_[0]->$k)) }];
                    }
                }
                return @specs;
            }
        } elsif ($t->[0] eq "ref") {
            # Some Perl documenters have tried to promulgate "sigil"
            # as the name for the funny characters at the front of
            # Perl variables. At the moment we can't use them here,
            # though, because the complexities of Perl's dereferencing
            # syntax are currently too much for Daikon to handle.
#           my $sigil;
#           if (ref($t->[1]) and $t->[1][0] eq "array") {
#               $sigil = '@';
#           } elsif (ref($t->[1]) and $t->[1][0] =~ /^hash|object$/) {
#               $sigil = '%';
#           } else {
#               $sigil = '$';
#           }

            if ($t->[1] eq "top") {
                return ["", "", "Object", "hashcode",
                        sub { output_int(\ (address($_[0]))); }];
            } else {
                my @specs;
                for my $spec (daikon_output_spec($t->[1], $single)) {
                    my($prefix, $suffix, $type, $rep_type, $output) = @$spec;
                    push @specs, [$prefix, '.deref'.$suffix, $type, $rep_type,
                                  sub { $output->(${$_[0]}) }];
                }
                push @specs, ["", "", "Object", "hashcode",
                              sub { output_int(\ (address(${$_[0]}))); }];
                return $single ? $specs[0] : @specs;
            }
        } elsif ($t->[0] eq "maybe") {
            my $is_def = ["", ".is_defined", "boolean", "boolean",
                          sub { defined(${$_[0]}) ? "true" : "false"}];
            if ($t->[1] eq "bottom") {
                # 'nothing' is always either the empty string or the
                # undefined value, so the only interesting information
                # is whether it's defined.
                return $is_def;
            }
            my $is_empty = ["", ".is_empty", "boolean", "boolean",
                            sub { length(${$_[0]})==0 ? "true" : "false"}];
            my @extra_specs = ($is_def, $is_empty);
            if ($t->[1] eq "bit") {
                # Boolean is a special case, since we want 'bit's to
                # show up as integers, but 'bool's to show up as
                # booleans.
                my $bool = ["", "", "boolean", "boolean",
                            sub { ${$_[0]} ? "true" : "false"}];
                return $single ? $bool : ($bool, @extra_specs);
            } elsif ($t->[1] eq "str") {
                my @specs = daikon_output_spec($t->[1], $single);
                # is_empty isn't needed, since the empty string is a
                # regular string.
                return $single ? $specs[0] : (@specs, $is_def);
            } else {
                my @specs = daikon_output_spec($t->[1], $single);
                return $single ? $specs[0] : (@specs, @extra_specs);
            }
        }
    }
    return ();
}

# A cache of the output specification for a type, indexed by their
# string representations.
my %specs;

# Format a variable declaration for a .decls file, given the name for
# the variable and list of specifications. Returns a list of lines.
sub declare_var_spec {
    my($varname, @specs) = @_;
    my @lines = ();

    for my $spec (@specs) {
        my($prefix, $suffix, $type, $rep_type, undef) = @$spec;
        my $name = "$prefix$varname$suffix";
        my $is_param = "";
        if ($varname ne "return" and $name !~ /\.deref/) {
            # It might be more elegant to do this by counting
            # dereferences as part of the spec, rather than text
            # matching the name. It would be more work, though.
            $is_param = " # isParam = true";
        }
        $name =~ s/\.deref\././g;
        push @lines, "$name\n"; # Variable name
        push @lines, "$type$is_param\n";
        push @lines, "$rep_type\n";
        push @lines, "22\n"; # comparability: not available
    }
    return @lines;
}

# Format a variable declaration for a .decls file, given the name for
# the variable and an unparsed type. Returns a list of lines.
sub declare_var {
    my($varname, $type) = @_;
    if (not exists $specs{$type}) {
        $specs{$type} = [daikon_output_spec(parse_type($type, 0))];
    }
    return declare_var_spec($varname, @{$specs{$type}});
}

# Output a variable to a .dtrace file, given the filehandle, the name
# of the variable, a reference to the value of the variable, and a
# list of specifications.
sub output_var_spec {
    my($fh, $varname, $value, @specs) = @_;
    for my $spec (@specs) {
        my($prefix, $suffix, undef, undef, $output) = @$spec;
        my $name = "$prefix$varname$suffix";
        # This is a cosmetic hack to reduce the number of redundant
        # ".deref"s in the output. It could be done more carefully.
        $name =~ s/\.deref\././g;
        print $fh "$name\n";
        print $fh $output->($value), "\n";
        print $fh "1\n"; # Pretend always modified
    }
}

# Output a variable to a .dtrace file, given the filehandle, the name
# of the variable, a reference to the value of the variable, the type
# of the variable in unparsed form.
sub output_var {
    my($fh, $varname, $value, $type) = @_;
    if (not exists $specs{$type}) {
        $specs{$type} = [daikon_output_spec(parse_type($type, 0))];
    }
    output_var_spec($fh, $varname, $value, @{$specs{$type}});
}

# Write the type information in $types_r and $order_r into a decls
# file open on $out_fh.
sub types_info_to_decls {
    my($types_r, $order_r, $out_fh) = @_;
    print $out_fh "\nVarComparability\nnone\n\n";
    for my $ppt_name (sort keys %$types_r) {
        next unless $ppt_name =~ /_[sl]\(\)/;
        next if $ppt_name =~ /EXIT$/;
        my $union_ppt = $ppt_name;
        $union_ppt =~ s/_[sl]\(\)/()/;
        $union_ppt =~ s/:::.*$/:::UNION/;
        (my $exit_union_ppt = $ppt_name) =~ s/:::EXIT(\d+)/:::EXIT/;
        my @lines = ("DECLARE\n");
        push @lines, "$ppt_name\n";
        my $vars_ref = $types_r->{$ppt_name};
        for my $var (@{$order_r->{$ppt_name}}) {
            my $ppt;
            if ($var eq "return") {
                $ppt = $exit_union_ppt;
            } else {
                $ppt = $union_ppt;
            }
            my @specs = daikon_output_spec($types_r->{$ppt}{$var});
            push @lines, declare_var_spec($var, @specs);
        }
        print $out_fh @lines;
        print $out_fh "\n";
    }
}

sub types_file_to_decls_file {
    my($types_fname, $decls_fname) = @_;
    open(TYPES, "<$types_fname") or die "Can't open $types_fname: $!";
    my(%types, %order);
    read_types_into(\*TYPES, \%types, \%order);
    close TYPES;
    open(DECLS, ">$decls_fname") or die "Can't open $decls_fname: $!";
    types_info_to_decls(\%types, \%order, \*DECLS);
    close DECLS;
}

1;
