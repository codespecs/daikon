#!/usr/bin/env perl
use strict;
use Carp;
use warnings;

no warnings 'recursion';

# Usage: zcat foo.inv.gz | perl dump-serial.pl

my $buf;
read(STDIN, $buf, 2);
if ($buf eq "\xac\xed") {
    print "Java serialized data ";
    read(STDIN, $buf, 2);
    my $ver = unpack("n", $buf);
    print "version $ver\n";
} else {
    die "Not in Java serialization format";
}
my $indent = 0;
my $handle = -1;
my @save;
my @save_names;
my $show_nums = 0;
content();

sub iprint {
    print " " x $indent;
    print @_;
}

sub read_string {
    my $buf;
    read(STDIN, $buf, 2);
    my $len = unpack("n", $buf);
    read(STDIN, $buf, $len);
    return $buf;
}

sub read_tstring {
    read(STDIN, my $buf, 1);
    confess "<$buf>" unless $buf eq "t";
    return read_string();
}

sub read_byte {
    my $buf;
    read(STDIN, $buf, 1);
    return unpack("c", $buf);
}

sub read_uint {
    my $buf;
    read(STDIN, $buf, 4);
    return unpack("N", $buf);
}

sub read_ulong {
    return read_uint() * 2**32 + read_uint();
}

sub read_float {
    my $buf;
    read(STDIN, $buf, 4);
    $buf = reverse $buf if pack("S", 1) eq "\1\0";
    return unpack("f", $buf);
}

sub read_double {
    my $buf;
    read(STDIN, $buf, 8);
    $buf = reverse $buf if pack("S", 1) eq "\1\0";
    return unpack("d", $buf);
}

sub read_short {
    my $buf;
    read(STDIN, $buf, 2);
    $buf = reverse $buf if pack("S", 1) eq "\1\0";
    return unpack("s", $buf);
}

sub content {
    my $lookahead;
    $indent++;
    defined(read(STDIN, $lookahead, 1)) or die $!;
    if ($lookahead eq "s") { # TC_OBJECT
	iprint "object";
	print "<$handle>" if $show_nums;
	print "\n";
	my $fields = maybe_class_desc();
	$handle++;
	for (my $i = 0; $i < @$fields; $i++) {
	    my($name, $sub) = @{$fields->[$i]};
	    iprint "${i}th field ($name) value is ";
	    $sub->();
	}
    } elsif ($lookahead eq "x") { # TC_END_BLOCKDATA
        no warnings 'exiting';
        # Break out of the innermost dynamically enclosing loop,
        # which had better be in the caller of content().
	$indent--; last;
    } elsif ($lookahead eq "w") { # TC_BLOCKDATA
	my $buf;
	read(STDIN, $buf, 1);
	my $len = unpack("C", $buf);
	read(STDIN, $buf, $len);
	print "block data($len) '$buf'\n";
    } elsif ($lookahead eq "t") { # TC_STRING
	my $str = read_string();
	iprint "string";
	print "<$handle>" if $show_nums;
	print " '$str'\n";
	$handle++;
    } elsif ($lookahead eq "q") { # TC_REF
	my $ref = read_uint() - 0x7e0000;
	if ($show_nums) {
	    iprint "previous thingie $ref\n";
	} else {
	    my $diff = $handle - $ref + 1;
#	    iprint "previous thingie, back $diff\n";
	    iprint "previous thingie\n";
	}
    } elsif ($lookahead eq "p") { # TC_NULL
	iprint "null\n";
    } elsif ($lookahead eq "u") { # TC_ARRAY
	iprint "array";
	print "<$handle>" if $show_nums;
	print "\n";
	my $fields = maybe_class_desc();
	$handle++;
	my $size = read_uint();
	iprint "$size elements\n";
	for (my $i = 0; $i < $size; $i++) {
	    iprint "${i}th element is ";
	    $fields->[0]->();
	}
    } else {
	confess "<$lookahead>";
    }
    $indent--;
}

sub maybe_class_desc {
    my $lookahead;
    read(STDIN, $lookahead, 1);
    if ($lookahead eq "r") { # TC_CLASSDESC
	return class_desc();
    } elsif ($lookahead eq "p") { # TC_NULL
	iprint "(null)\n";
	return [];
    } elsif ($lookahead eq "q") { # TC_REFERENCE
	my $id = read_uint() - 0x7e0000;
	iprint "previous class $id ($save_names[$id])\n" if $show_nums;
	iprint "previous class $save_names[$id]\n" if !$show_nums;
	return $save[$id];
    } else {
	die "<$lookahead>";
    }
}

sub class_desc {
    my $name = read_string();
    $handle++;
    iprint "class";
    print "<$handle>" if $show_nums;
    print "named $name\n";
    my $buf;
    my $uid = read_ulong();
    read(STDIN, $buf, 1);
    my $flags = unpack("C", $buf);
    iprint "serial uid $uid, flags $flags\n";
    read(STDIN, $buf, 2);
    my $field_cnt = unpack("n", $buf);
    iprint "$field_cnt fields\n";
    my $subs = [];
    $save[$handle] = $subs;
    $save_names[$handle] = $name;
    my %prims = ("B" => ["byte", sub {
			     print "byte " . read_byte() . "\n";
			 }],
		 "C" => ["char", sub {die} ],
		 "D" => ["double", sub {
			     print "double " . read_double() . "\n";
			 } ],
		 "F" => ["float", sub {
			     print "float " . read_float() . "\n";
			 } ],
		 "I" => ["int", sub {
			     print "int " . read_uint() . "\n";
			 }],
		 "J" => ["long", sub {
			     print "long " . read_ulong() . "\n";
			 } ],
		 "S" => ["short", sub {
			     print "short " . read_short() . "\n";
			 } ],
		 "Z" => ["boolean", sub {
			     print "boolean " .
			       (read_byte() eq "\0" ? "false" : "true")."\n";
			 } ]);
    if (substr($name, 0, 1) eq "[") {
	# array class
	my $elt_type = substr($name, 1, 1);
	if ($prims{$elt_type}) {
	    iprint "array of $prims{$elt_type}[0]\n";
	    push @$subs, $prims{$elt_type}[1];
	} elsif ($elt_type eq "L") {
	    iprint "array of objects\n";
	    push @$subs, sub {
		print "object\n";
		content();
	    };
	} elsif ($elt_type eq "[") {
	    iprint "array of arrays\n";
	    push @$subs, sub {
		print "array\n";
		content();
	    };
	} else {
	    die;
	}
    } # else
    for (my $i = 0; $i < $field_cnt; $i++) {
	my $typecode;
	read(STDIN, $typecode, 1);
	if ($prims{$typecode}) {
	    my $name = read_string();
	    iprint "$prims{$typecode}[0] field $name\n";
	    push @$subs, [$name, $prims{$typecode}[1]];
	} elsif ($typecode eq "[") {
	    my $name = read_string();
	    iprint "array field $name of type " . decl_type() ."\n";
	    push @$subs, [$name, sub {
		print "array\n";
		content();
	    }];
	} elsif ($typecode eq "L") {
	    my $name = read_string();
	    iprint "object field $name of type " . decl_type() . "\n";
	    push @$subs, [$name, sub {
		print "object\n";
		content();
	    }];
	}
    }
    read(STDIN, $buf, 1);
    die unless $buf eq "x"; # Else class annotation
    iprint "superclass\n";
    my $super = maybe_class_desc();
    @$subs = (@$super, @$subs);
    push @$subs, ["extra", sub { while (1) { content() }}] if $flags == 3;
    return $subs;
}

sub decl_type {
    my $lookahead;
    read(STDIN, $lookahead, 1);
    if ($lookahead eq "t") {
	$handle++;
	my $name = read_string();
	$save_names[$handle] = $name;
	$name = "<$handle>" . $name if $show_nums;
	return $name;
    } elsif ($lookahead eq "q") {
	my $ref = read_uint() - 0x7e0000;
	return "previous type $ref ($save_names[$ref])" if $show_nums;
	return "(previous) $save_names[$ref]";
    }
}
