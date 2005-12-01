#!/usr/bin/perl

use strict;

my $afs_results = "/afs/csail.mit.edu/u/d/daikonuser/build";

my $out_dir = "/afs/csail.mit.edu/u/s/smcc/public_html/kvasir-nightly";

# We expect to run on yam
my $other_results = "/scratch3/smcc/kvasir-portability-results";

my @platforms = ("redhat-9", "fedora-core-3", "debian-etch");

sub latest_in {
    my($dir) = @_;
    opendir(DIR, $dir) or die $!;
    my @dirs = readdir(DIR);
    closedir DIR;
    @dirs = grep(/^\d{4}\d{2}\d{2}-\d{2}\d{2}\d{2}$/, @dirs);
    @dirs = sort {$b cmp $a} @dirs;
    return $dirs[0];
}

my %results;
my @tests;

sub parse_results {
    my($dir, $ident) = @_;
    open(RESULTS, "<$dir/kvasir_regression_test_summary.out")
      or die "$dir/kvasir_regression_test_summary.out: $!";
    my $test;
    while (<RESULTS>) {
	if (/^\t\[([\w-]+)\]$/) {
	    $test = $1;
	    push @tests, $test if not exists $results{$test};
	} elsif (/^(OK|FAILED)\s+(\w+\.(\w+)\.diff)$/) {
	    my($result, $fname, $type) = ($1, $2, $3);
	    if ($result eq "OK") {
		$results{$test}{$type}{$ident} = "OK";
	    } else {
		$results{$test}{$type}{$ident} = "$dir/diffs/kvasir/$fname";
	    }
	}
    }
}

parse_results($afs_results . "/". latest_in($afs_results), "CSAIL Debian");
for my $p (@platforms) {
    parse_results("$other_results/$p" . "/" . latest_in("$other_results/$p"),
		  $p);
}

my $file_count = 1;

open(OUT, ">$out_dir/index.html");

print OUT "<html>\n";
print OUT "<head><title>Kvasir portability test results</title></head>\n";
print OUT "<body>\n";
print OUT "<table border=1>\n";
print OUT "<tr><th></th><th></th><th><font size=-1>CSAIL Debian</font></th>\n";
print OUT "<th><font size=-1>$_</font></th>" for @platforms;
for my $test (@tests) {
    my $num_types = keys %{$results{$test}};
    my $test_id = "<td rowspan=$num_types align=right>$test</td>";
    for my $type (keys %{$results{$test}}) {
	print OUT "<tr>$test_id</td><td>$type</td>";
	$test_id = "";
	for my $p ("CSAIL Debian", @platforms) {
	    if ($results{$test}{$type}{$p} eq "OK") {
		print OUT "<td bgcolor='#bbffbb'></td>";
	    } else {
		my $file = sprintf("%03d.txt", $file_count++);
		system("/bin/cp", $results{$test}{$type}{$p},
		       "$out_dir/$file");
		print OUT "<td align=center bgcolor='#ffbbbb'>";
		print OUT "<a href='$file'>diffs</a>";
		print OUT "</td>";
	    }
	}
	print OUT "</tr>\n";
    }
    print OUT "<tr></tr>\n";
}
print OUT "</tr>\n";
print OUT "</table>\n";
print OUT "</body>\n";
print OUT "</html>\n";

close OUT;

#print "Latest AFS test is ", latest_in($afs_results), "\n";
