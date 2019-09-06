#!/usr/bin/perl

# This script is a wrapper around the normal buildtest.pl for use in
# running tests on a machine that doesn't have access to PAG's AFS
# repository; in particular, VMware guest versions of Linux running
# behind a virtual NAT translation. It fetches the latest code
# versions, and uploads its results, via rsync over SSH, which for
# automatic operation should be configured with a DSA keypair.  To
# work around problems with cron and mail that arise from clock skew
# and not having a real IP address, it incorporates its own cron-like
# functionality and sends mail over SSH.

use 5.008; # For three-arg pipe open
use strict;

use Time::Local 'timelocal';

sub min { $_[0] < $_[1] ? $_[0] : $_[1] }
sub max { $_[0] > $_[1] ? $_[0] : $_[1] }

if (@ARGV != 3) {
    die "Usage: buildtest-remote.pl <directory-name> <hour> <minute>\n";
}

my $dirname = $ARGV[0];
my $start_time = 60*60*$ARGV[1] + 60*$ARGV[2];

# Locations on the host side.
my $host_machine = "yam";
my $domain = "csail.mit.edu";
my $rsync_loc = "/scratch2/smcc/daikon-auto-checkout/invariants";
my $results_loc = "/scratch3/smcc/kvasir-portability-results/$dirname";
my $mailto = 'smcc@csail.mit.edu';
my $mailfrom = 'smcc@yam.csail.mit.edu';

# Locations on the guest side
my $home = $ENV{'HOME'};
my $builds_dir = "$home/nightly-build";
#my $ssh_info = "$home/ssh-agent-info";
my $inv_dir = "$home/research/invariants";
my $buildtest_pl = "$inv_dir/scripts/buildtest.pl";

# Paths to programs
my @rsync_opts = ("-e", "ssh -x", "-ra", "--delete");
my $perl = "/usr/bin/perl";
my $rsync = "/usr/bin/rsync";
my $sendmail = "/usr/sbin/sendmail";
my $ifconfig = "/sbin/ifconfig";
my $ssh = "/usr/bin/ssh";

my $os_ver;
if (-e "/etc/redhat-release") {
    open(RR, "</etc/redhat-release");
    $os_ver = <RR>;
    chomp $os_ver;
    close RR;
} elsif (-e "/etc/debian_version") {
    my $version;
    open(DV, "</etc/debian_version");
    $version = <DV>;
    chomp $version;
    close DV;
    my $dist;
    open(ASL, "</etc/apt/sources.list");
    while (<ASL>) {
	if (m[^deb (?:ht|f)tp.\S*/debian (\w+) main contrib non-free$]) {
	    $dist = $1;
	    last;
	}
    }
    close ASL;
    $os_ver = "Debian GNU/Linux $version ($dist)";
}

my $machine_name = "<unknown>";
#my $hostname = `hostname -f`;
#chomp $hostname;
#if ($hostname !~ /localhost/) {
#    $machine_name = $hostname;
#} else {
    open(IFC, "$ifconfig |");
    while (<IFC>) {
	if (/inet addr:([\d.]+) / and $1 ne "127.0.0.1") {
	    $machine_name = $1;
	    last;
	}
    }
    close IFC;
#}

my $kernel_version = `uname -srvmo`;
chomp $kernel_version;
my $gcc_version = `gcc -v 2>&1 | fgrep version`;
chomp $gcc_version;
my $libc_version = `/lib/libc.so.6 2>&1 | grep 'Library.*version'`;
chomp $libc_version;

my $message = <<EOM;
Kvasir portability test for $os_ver on $machine_name.
Detailed results are in /var/autofs/net/$host_machine$results_loc.
$kernel_version
$gcc_version
$libc_version
EOM
chomp $message;

# open(SSH_INFO, "<$ssh_info");
# while (<SSH_INFO>) {
#     chomp;
#     die "Bad format in $ssh_info" unless /^(\w+)=(.*)$/;
#     $ENV{$1} = $2;
# }
# close SSH_INFO;

sub run {
    my($month, $mday) = (localtime())[4, 3];
    $month++;

    system($rsync, @rsync_opts, "$host_machine.$domain:$rsync_loc/",
	   "$inv_dir");

    chdir($builds_dir);

    # We used to use "--rsync_location=$host_machine.$domain:$rsync_loc"
    # to make a fresh copy from the host machine, but after we switched
    # to VMware 5.5, big rsyncs started mysteriously hanging. So instead
    # use rsync to do a local copy from the version rsync'd above (which
    # is fast because it's incremental)
    open(RUN, "-|", $perl, $buildtest_pl, "--message=$message", "--quiet",
	 "--rsync_location=$inv_dir",
	 "--test_kvasir", "--skip_daikon_build")
      or die "Couldn't start test: $!";
    my @lines = <RUN>;
    close RUN;

    system($rsync, @rsync_opts, "$builds_dir/",
	   "$host_machine.$domain:$results_loc");

    if (@lines) {
	open(MAIL, "|-", $ssh, "-x", "$host_machine.$domain",
	     $sendmail, "-f$mailfrom", $mailto);
	print MAIL "From: $mailfrom\n";
	print MAIL "To: $mailto\n";
	print MAIL "Subject: Kvasir portability test $dirname $month/$mday\n";
	print MAIL "\n";
	print MAIL @lines;
	print MAIL ".";
	close MAIL;
    }
}

my $period = 24*60*60;

my $today_start;
{
    my($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);
    $today_start = timelocal(0, 0, 0, $mday, $mon, $year);
}

my $next_run = $today_start + $start_time;
$next_run += $period if $next_run < time;

for (;;) {
    print "Current time is ", "".localtime(time), "\n";
    print "Next run is at  ", "".localtime($next_run), "\n";
    if ($next_run <= time) {
	print "Running...\n";
	run();
	$next_run += $period;
    } else {
	print "Sleeping...\n";
	sleep max(1, min(15*60, ($next_run - time) / 2));
    }
}
