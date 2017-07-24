#!/usr/bin/env perl
use strict;
use Carp;
use warnings;
no warnings 'recursion';

# Usage: zcat foo.inv.gz | perl serial2vcg.pl >foo.vcg
#        ~smcc/bin/xvcg foo.vcg
# VCG is also at http://rw4.cs.uni-sb.de/users/sander/html/gsvcg1.html

my $buf;
read(STDIN, $buf, 2);
if ($buf eq "\xac\xed") {
    #print "Java serialized data ";
    read(STDIN, $buf, 2);
    my $ver = unpack("n", $buf);
    #print "version $ver\n";
} else {
    die "Not in Java serialization format";
}
my $indent = 0;
my $handle = -1;
my @save;
my @save_names;
my $show_nums = 0;
my @edges;
my %nodes_out;
my %strings;

print "graph: {\n";
# print "layout_downfactor: 10\n";
# print "layout_upfactor:   1\n";
# print "layout_nearfactor: 5\n";
print "layoutalgorithm: mindepth\n";

content();

for my $e (@edges) {
    my($from, $anchor, $to) = @$e;
    if (!$nodes_out{$to}) {
        warn "Missing to $to";
        $to-- if $nodes_out{$to-1};
    }
    next unless $nodes_out{$from} and $nodes_out{$to};
    print qq'edge: { sourcename: "n$from" targetname: "n$to" ' .
      qq'anchor: $anchor }\n';
}

print "}\n";

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

sub draw_node {
    my($num, $color, $header, @lines) = @_;
    my $i = 2;
    for my $l (@lines) {
        if ($l =~ s/<\[<(\d+)>\]>/$1/) {
            push @edges, [$num, $i, $1] if $i < 50;
        }
        $i++;
    }
    print "node: { ";
    print qq'title: "n$num" ';
    print qq'color: $color ';
    my $label = join("\n", $header, @lines);
    $label =~ s/\"/''/g;
    print qq'label: "$label"';
    print "}\n\n";
    $nodes_out{$num}++;
}

sub content {
    my $lookahead;
    $indent++;
    defined(read(STDIN, $lookahead, 1)) or die $!;
    if ($lookahead eq "s") { # TC_OBJECT
        my @lines;
	my($classname, $fields) = maybe_class_desc();
        my $this_handle = $handle;
	$handle++;
	for (my $i = 0; $i < @$fields; $i++) {
	    my($name, $sub) = @{$fields->[$i]};
            my @entries = $sub->();
            for my $e (@entries) {
                push @lines, "$name value is $e";
            }
	}
        my $color = "white";
        if ($classname =~ /java\.util\./) {
            $color = "lightgrey";
        } elsif ($classname =~ /ValueTracker/) {
            $color = "orange";
        } elsif ($classname =~ /Invariants/) {
            $color = "lightgrey";
        } elsif ($classname =~ /daikon\.inv\./) {
            $color = "lightgreen";
        } elsif ($classname =~ /daikon\.suppress\./) {
            $color = "lightred";
        } elsif ($classname =~ /PptTopLevel/) {
            $color = "yellow";
        } elsif ($classname =~ /PptSlice\d/) {
            $color = "lightyellow";
        } elsif ($classname =~ /\.VarInfo$/) {
            $color = "aquamarine";
        } elsif ($classname =~ /\.VarInfoName/) {
            $color = "lightblue";
        }
        draw_node($this_handle, $color, "$classname ($this_handle)", @lines);
        $indent--;
        return "object <[<$this_handle>]>";
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
        return "block data";
    } elsif ($lookahead eq "t") { # TC_STRING
	my $str = read_string();
        $strings{$handle} = $str;
	$handle++;
        return "string '$str'";
    } elsif ($lookahead eq "q") { # TC_REF
	my $ref = read_uint() - 0x7e0000;
        if (exists $strings{$ref}) {
            return "string '$strings{$ref}'";
        }
        return "object <[<$ref>]>";
    } elsif ($lookahead eq "p") { # TC_NULL
        return "null";
    } elsif ($lookahead eq "u") { # TC_ARRAY
        my @lines;
	my($aryname, $fields) = maybe_class_desc();
        my $this_handle = $handle;
	$handle++;
	my $size = read_uint();
	for (my $i = 0; $i < $size; $i++) {
            my $ent = $fields->[0]->();
	    push @lines, "${i}th element is $ent";
	}
        draw_node($this_handle, "white", "Array $this_handle", @lines);
        $indent--;
        return "array <[<$this_handle>]>";
    } else {
	confess "<$lookahead>";
    }
    $indent--;
}

sub maybe_class_desc {
    my $lookahead;
    read(STDIN, $lookahead, 1);
    if ($lookahead eq "r") { # TC_CLASSDESC
        my $class_handle = $handle + 1;
        my $desc = class_desc();
	return ($save_names[$class_handle], $desc);
    } elsif ($lookahead eq "p") { # TC_NULL
	return ("null", []);
    } elsif ($lookahead eq "q") { # TC_REFERENCE
	my $id = read_uint() - 0x7e0000;
	return ($save_names[$id], $save[$id]);
    } else {
	die "<$lookahead>";
    }
}

sub class_desc {
    my $name = read_string();
    $handle++;
    my $buf;
    my $uid = read_ulong();
    read(STDIN, $buf, 1);
    my $flags = unpack("C", $buf);
    read(STDIN, $buf, 2);
    my $field_cnt = unpack("n", $buf);
    my $subs = [];
    $save[$handle] = $subs;
    $save_names[$handle] = $name;
    my %prims = ("B" => ["byte", sub {
			     return "byte " . read_byte();
			 }],
		 "C" => ["char", sub {die} ],
		 "D" => ["double", sub {
			     return "double " . read_double();
			 } ],
		 "F" => ["float", sub {
			     return "float " . read_float();
			 } ],
		 "I" => ["int", sub {
			     return "int " . read_uint();
			 }],
		 "J" => ["long", sub {
			     return "long " . read_ulong();
			 } ],
		 "S" => ["short", sub {
			     return "short " . read_short();
			 } ],
		 "Z" => ["boolean", sub {
			     return "boolean " .
			       (read_byte() eq "\0" ? "false" : "true");
			 } ]);
    if (substr($name, 0, 1) eq "[") {
	# array class
	my $elt_type = substr($name, 1, 1);
	if ($prims{$elt_type}) {
	    push @$subs, $prims{$elt_type}[1];
	} elsif ($elt_type eq "L") {
	    push @$subs, sub {
		content();
	    };
	} elsif ($elt_type eq "[") {
	    push @$subs, sub {
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
	    push @$subs, [$name, $prims{$typecode}[1]];
	} elsif ($typecode eq "[") {
	    my $name = read_string();
            decl_type();
	    push @$subs, [$name, sub {
		content();
	    }];
	} elsif ($typecode eq "L") {
	    my $name = read_string();
            decl_type();
	    push @$subs, [$name, sub {
		content();
	    }];
	}
    }
    read(STDIN, $buf, 1);
    die unless $buf eq "x"; # Else class annotation
    my($supername, $super) = maybe_class_desc();
    @$subs = (@$super, @$subs);
    push @$subs, ["extra", sub {
                      my @data;
                      while (1) {
                          push @data, content()
                      }
                      return @data;
                  }] if $flags == 3;
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
