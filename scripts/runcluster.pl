#!/usr/bin/env perl

# performs a clustering of a Daikon data trace to produce a cluster info file out
# of the useful clusters. Input is a dtrace and a decls file. Output is "cluster.spinfo"
#

use English;
#use strict;

$WARNING = 0;                    # -w flag


$ncluster = 4;

$other_decls_files; #the decls files
$decls_file;
$main_decls_found = 0;
foreach $arg (@ARGV) {
    if ($arg  =~ /\.decls/){
	if ($main_decls_found == 0) {
	    $decls_file = $arg;
	    $main_decls_found = 1;
	} else {
	    $other_decls_files = "$other_decls_files $arg";
	}
    } elsif ( $arg =~/\.dtrace/ ) {
        $dtrace_file = $arg;
    } elsif ($arg =~ /^\s*(\d*)\s*$/) {
	$ncluster = $1;
    }
}

print "runcluster.pl:  $ncluster clusters\n";

#remove files from a previous run that might have aborted...
remove_temporary_files();

#extract the variables from the dtrace file
print "\nextracting variables from dtrace file ...\n";
$command = "perl $ENV{INV}/scripts/extract_vars.pl $decls_file $dtrace_file";
print "$command\n";
system($command);

$command = "";
#this is a hack. careful!
@exit_ppts = glob("*\\.EXIT*");
@object_ppts = glob("*\\.OBJECT\\.*");
@to_cluster = (@exit_ppts, @object_ppts);
foreach $filename (@to_cluster){
    $outfile = "$filename.cluster";
    #this is for kmeans clustering
    $command = $command . " $ENV{INV}/scripts/kmeans $filename $ncluster > $outfile; ";
    #this is for hierarchical clustering
    #$command = $command . " difftbl $filename | cluster -w | clgroup -n $ncluster > $outfile; ";
}

#perform the clustering
print "\nperforming clustering ....\n";
print "$command\n";
system($command);

#rewrite dtrace file
print "\nrewriting dtrace file ...\n";
$clustered_files = `ls *cluster`;
@files = split ' ', $clustered_files;
$command = "perl $ENV{INV}/scripts/write_dtrace.pl $dtrace_file " . join(' ', @files);
print "$command\n";
system($command);

#rewrite .decls file to add cluster info
$decls_new = $decls_file;
#remove everything from the pathname except the filename itself.
if ($decls_new =~ /\//) {
    $decls_new =~ s/.*\///;
}

$decls_new =~ s/\.decls/_new\.decls/;
# print "$decls_new will be the new decls file\n";

$command = "perl $ENV{INV}/scripts/decls-add-cluster.pl  $decls_file $decls_new";
print "\nrewriting .decls files ($decls_file -> $decls_new) to include cluster info...\n";
print "$command\n";
system($command);
# print "decls-add-cluster finished\n";

#write spinfo file
$spinfo_file = "temp.spinfo";
print "\nwriting spinfo file $spinfo_file ...\n";
open (SPINFO, ">$spinfo_file") || die "couldn't write cluster spinfo file temp.spinfo\n";
$spinfostring  = "PPT_NAME OBJECT\ncluster == 1 \ncluster == 2 \ncluster == 3 \ncluster == 4 \ncluster == 5";
print SPINFO $spinfostring;
close SPINFO;

#run daikon with cluster spinfo file and cluster dtrace file.
$dtrace_file =~ /(.*)\.dtrace/;
$new_dtrace = "$1_new.dtrace";
$invfile = "result$ncluster.inv";
$command = "java -Xmx512m daikon.Daikon -o $invfile --no_text_output --suppress_redundant --suppress_post $spinfo_file $decls_new $new_dtrace";
print "$command\n";
system($command);

#print out the invariants
$invfile =~ /(.*)\.inv/;
$textout = $1;
$command = "java daikon.PrintInvariants --java_output $invfile > $textout";
print "\n$command\n";
system($command);

#clean up results
# $command = "$ENV{INV}/scripts/extract_implications.pl $textout; rm $textout";
$command = "$ENV{INV}/scripts/extract_implications.pl $textout";
print "\n$command\n";
system($command);

#remove all temporary files
# remove_temporary_files();

#$spinfo_file = "$textout.spinfo";
#$other_decls_files =~ s/$decls_file//;
#$command = "java -Xmx256m daikon.Daikon -o final.inv $decls_file $other_decls_files $spinfo_file $dtrace_file";
#print "$command\n";
#system($command);

exit 0;


###########################################################################
### Subroutines
###

sub unlink_glob ( $ ) {
  my ($glob) = @_;
  my @list = glob($glob);
  foreach $f (@list) {
    # print "removing $f\n";
    unlink $f;
  }
}

sub remove_temporary_files () {
  unlink_glob("*\\.EXIT*");
  unlink_glob("*\\.OBJECT\\.*");
  unlink_glob("*\\.CLASS\\.*");
  unlink_glob("*_new.dtrace*");
  unlink_glob("*_new.decls");
  unlink("temp.spinfo");
}

