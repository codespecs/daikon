#!/usr/bin/perl

# add these object invariants
@add_to_object =
    (
     "(\\forall int i; (0 <= i & i <= this.s.length-1) ==> (this.s[i] >= this.ROOT))\n",
     "(\\forall int i; (0 <= i & i <= this.s.length-1) ==> (this.s[i] != i))\n",
     );

# kill these invariants (test case factors)

%kill =
    (
     'this.s.length-1 > \old(this.s[this.s.length-2])' => 1,
     'this.s.length-1 > this.s[this.s.length-2]' => 1,
     );

# fix these invariants (smarter modifies)

%swap =
    (
     '  Modified variables: this.s[-1] this.s[0] this.s[set2]' => '  Modified variables: this.s[set2]',
     );
     
while (<>) {
    chomp;

    next if $kill{$_};

    ($_ = $swap{$_}) if $swap{$_};

    # guard array lookups
    $pred = "";
    if (!/[Vv]ariables/) {
	$pred = "this.s.length-1 >= 0" if (/this\.s\[0\]/);
	$pred = "this.s.length-1 >= 1" if (/this\.s\[1\]/);
	$pred = "this.s.length-1 >= 0" if (/this\.s\[this\.s\.length-1\]/);
	$pred = "this.s.length-2 >= 0" if (/this\.s\[this\.s\.length-2\]/);
	if ($pred) {
	    print "(", $pred, ") ==> (", $_, ")\n";
	    next;
	}
    }

    print $_, "\n";

    if ($_ eq "this\.ROOT == -1") {
	print @add_to_object;
    }
}
