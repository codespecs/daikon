#!/usr/bin/env perl

sub my_abs {
    my($x) = @_;

    if ($x == 0) {
	return 0;
    } elsif ($x < 0) {
	return -$x;
    } else {
	return $x;
    }
}

sub my_uc {
    my($s) = @_;

    if (!length $s) {
	return $s;
    }

    $s = ucfirst(substr($s, 0, 1)) . my_uc(substr($s, 1));

    return $s;
}

sub my_sort {
    my(@x) = @_;
    my $divider = 0;
    my(@left, @right);
    if (@x < 2) {
	return @x;
    }
    $divider = shift @x;
    @left = grep($_ <= $divider, @x);
    @right = grep($_ > $divider, @x);
    @left = my_sort(@left);
    @right = my_sort(@right);
    return (@left, $divider, @right);
}

sub num2bits {
    my($n) = @_;
    my @a = reverse split //, unpack("b*", pack("L", $n));
    shift @a while @a > 1 and $a[0] == 0;
    return [@a];
}

sub bits2num {
    my(@a) = @_;
    my $lsb = pop @a;
    if (@a == 0) {
        return $lsb;
    } else {
        return $lsb + 2*bits2num(@a);
    }
}

sub odd {
    my($x) = @_;
    ($x & 1) == 1;
}

sub param_f {
    my($x, @a) = @_;
    $x++;
    push @a, 5;
    return $x + @a + 2;
}

sub param_g {
    my($ar) = @_;
    push @$ar, 5;
    return 1 + @$ar;
}

sub true { 1 == 1 }
sub false { 1 == 0 }

sub aref_compare {
    my($a, $b) = @_;
    if ($a == $b) {
        return true;
    } elsif (@$a != @$b) {
        return false;
    } else {
        for my $i (0 .. $#$a) {
            return false if $a->[$i] != $b->[$i];
        }
        return true;
    }
}

sub maybe_maybe_int {
    my($x) = @_;
    my $which = $x % 4;
    if ($which == 0) {
        return "";
    } elsif ($which == 1) {
        return \ "";
    } elsif ($which == 2) {
        return \$x;
    } else {
        return \ ($x + 1);
    }
}

for my $v (5,1,12,-3,2,13,-5,-1,2,55,0,1,-4,-1,42,32768,-32768,3,5) {
    print "|$v| = ", my_abs($v), "\n";
}

for my $s ("cat", "Cat", "CAT", "pAg", "pAG", "42", "home-run", "!@` ",
	   " ", "!a", "\0", "the quick Brown FOX jumps over the lazy dog") {
    print "uc($s) = ", my_uc($s), "\n";
}

for my $a ([], [1], [1,2], [2,1], [1,2,3], [3,2,1], [3,1,2], [2,1,3], [2,3,1],
	   [1,3,2], [3,1,4,1,5,9,2,6,2], [1,1,2,2], [2,2,1,1], [0,0,0,0],
	   [2,7,1,8,2,8,1,8,2,8,4,5,9], [-1,-2,-3], [-42,42]) {
    print "sort(", join(",", @$a), ") = (", join(",", my_sort(@$a)), ")\n";
}

for my $i (qw(0 1 2 3 4 5 7 13 24 84 323 523 65535 65536 1234234)) {
    my @a = @{num2bits($i)};
    print "$i: ", join(",", @a), ", ", 0+bits2num(@a), "\n";
}

sub first_defined {
    my(@a) = @_;
    foreach my $e (@a) {
        return $e if defined($e);
    }
}

print first_defined(0, 5, undef);
print first_defined(5, 0, undef);
print first_defined(undef, 0, 5);
print first_defined(undef, undef, 2, 3, 4);

print join(",", map(odd($_), -2,-1,0,1,2,3,4,5,6,7,8,9)), "\n";

param_f(1, 4, 2);
param_f(-2, 2, 34, 4, 4, 4);
param_f(5, 5);
param_f(6);
param_f(23, 23);
param_f(-1, -2, -4);

@a = (3);
param_g(\@a);
param_g(\@a);
param_g(\@a);
param_g([2, 3]);
param_g([-1]);
param_g([]);

@refs = ([], [-1], [0], [4], [5], [5], [4], [1, 2], [0, 0], [0, 1], [1, 0],
         [2, 0], [-4, 2], [0, 1], [1, 2, 4], [3, 2, 1], [0, 0, 0], [-1, 0, 1],
         [1, 3, 4], [1, 2, 4], [1, 5, 10, 15], [-5, -19, -5, 0],
         [3, 1, 4, 1, 6], [2, 1, 7, 1, 8, 2, 8]);

for my $i (0 .. $#refs) {
    for my $j (0 .. $#refs) {
        aref_compare($refs[$i], $refs[$j]);
    }
}

for my $i (0 .. 20) {
    scalar maybe_maybe_int($i);
}
