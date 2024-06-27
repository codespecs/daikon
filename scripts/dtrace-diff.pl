#!/usr/bin/env perl

# dtrace-diff.pl
# arguments:  dtrace-diff declsfile dtrace1 dtrace2
# Outputs differences that aren't hashcodes.
# Optionally also ignores differences in exit ppt numbers.

use English;
use strict;
$WARNING = 1;

my $ignore_exitno = 0;

if ($ARGV[0] eq "--ignore_exitno") {
  $ignore_exitno = 1;
  shift @ARGV;
}

if (scalar(@ARGV) != 3) {
  die "Usage: $0 [--ignore-exitno] <declsname> <dtrace1> <dtrace2>\n";
}
my ($declsname, $dtaname, $dtbname) = @ARGV;

my $differences_found = 0;
my $errors_found = 0;


###########################################################################
### Subroutines
###

sub getline ( $ ) {
# gets a (non-comment, non-blank) line from the filehandle $1
    my ($fh) = @_;
    my $l;
    do {
	$l = <$fh>;
	if ($l) { chomp $l; }
    } while ($l && ($l =~ m|^\#|));
    return $l;
}

sub gzopen ( $$ ) {
# takes a fh and a filename, opens it (using zcat if necessary), and returns
# a filehandle.
    my ($fh, $fn) = @_;
    if ($fn =~ /\.gz$/) {
        my $gzcat = `which gzcat 2>&1`;
	if ($gzcat =~ /not found|which: no gzcat in/ or $gzcat eq "") {
	  $gzcat = 'zcat';
	} else {
	  $gzcat = 'gzcat';	# command output contains newline, etc.
	}
	# print STDERR "gzcat = $gzcat\n";
	$fn = "$gzcat " . $fn . "|";
    }
    open ($fh, $fn) or die "couldn't open \"$fn\"\n";
    return $fh;
}


sub load_decls ( $ ) {
# Loads the decls file given by $1 into a hash, returns a ref.
# The hash maps from ppt name to (map from varname to triple).
    my ($mydeclsname) = @_;
#    open DECLS, $mydeclsname or die "couldn't open decls \"$decls\"\n";
    my $decls = gzopen(\*DECLS, $mydeclsname);
    my $declshash = {};
    my $ppt_seen = 0;
    while (defined (my $l = getline($decls))) {

        $l =~ s://.*::; # strip any comments on this line

	if (($l =~ /(^ppt\s+)(.+)/)) {
	    my $currppt = $2;
	    my $lhashref = {};
            my @varorder = ();

            my $curvar = "";
            my $currep = "";
            my $curdec = "";
            my $curcomp = "";

            while(my $subline = getline($decls)) {
                $subline =~ s/^\s+//;
                $subline  =~ s/\s+$//;

                if($subline =~ /(^variable\s+)(.+)/) {
                    unless($curvar eq "") { # Push stuff to the stack
                        $$lhashref{$curvar} = [$curdec, $currep, $curcomp];
                        push @varorder, $curvar;
                    }
                    $curvar = $2;
                }elsif (($subline =~ /^parent.+/) ||
                        ($subline =~ /^ppt\-type.+/) ||
                        ($subline =~ /^flags.+/)) {
                }elsif ($curvar eq "") {
                    die "Malformed decls file: \"$subline\" at line $INPUT_LINE_NUMBER instead of variable declaration";
                }elsif ($subline =~ /(^rep\-type\s*)(.+)/) {
                    $currep = $2;
                }elsif ($subline =~ /(^dec-type\s*)(.+)/) {
                    $curdec = $2;
                }elsif ($subline =~ /(^comparability\s*)(.+)/) {
                    $curcomp = $2;
                }
            }
            unless($curvar eq "") { # Push stuff to the stack
                $$lhashref{$curvar} = [$curdec, $currep, $curcomp];
                push @varorder, $curvar;
            }
            $$lhashref{"variable order"} = [ @varorder ];
            $$declshash{$currppt} = $lhashref;
            $ppt_seen = 1;
	} elsif ($l eq "DECLARE") {
	    my $currppt = getline($decls);
	    my $lhashref = {};
            my @varorder = ();
	    while (my $varname = getline($decls)) {
		(defined (my $dtype = getline($decls)))
		    or die "malformed decls file";
		(defined (my $rtype = getline($decls)))
		    or die "malformed decls file";
		(defined (my $ltype = getline($decls)))
		    or die "malformed decls file";
                # print STDERR "decls defining $varname for $currppt\n";
		$$lhashref{$varname} = [$dtype, $rtype, $ltype];
                push @varorder, $varname;
	    }
            $$lhashref{"variable order"} = [ @varorder ];
	    $$declshash{$currppt} = $lhashref;
	    $ppt_seen = 1;
        }elsif (($l eq "VarComparability") && !$ppt_seen) {
	    # It's ok to have a VarComparability as the first thing
	    # in the decls file.  Read the type of comparability,
	    # then move on.
	    $l = getline($decls);
	} elsif (($l eq "ListImplementors") && !$ppt_seen) {
	    # It's ok to have a ListImplementors in the decls file.
	    # Read the type of comparability, then move on.
	    $l = getline($decls);
	} elsif (($l =~ /^input\-language.+/) ||
                 ($l =~ /^decl\-version.+/) ||
                 ($l =~ /^var\-comparability.+/)){
        } elsif ($l) {
	    die "malformed decls file: \"$l\" at line $INPUT_LINE_NUMBER of $mydeclsname";
	}
    }
    close \*DECLS;
    return $declshash;
}

sub load_ppt ( $$ ) {
# Loads a single ppt from a dtrace fh given by $1.
# Returns a "ppt_trace_info": a 3-element array of pptname, line number in
# file, and hash mapping varname to array of value and modbit.
    my ($dtfh, $dtfhname) = @_;
    my $pptname = getline($dtfh);
    while ((defined $pptname) && (($pptname eq "") ||
                                  ($pptname =~ /^input\-language.+/) ||
                                  ($pptname =~ /^decl\-version.+/) ||
                                  ($pptname =~ /^var\-comparability.+/))) {
	$pptname = getline($dtfh);
    }


    (defined $pptname)
	or return undef;

    my $pptline = $INPUT_LINE_NUMBER;

    my $ppthash = {};

    my @varorder = ();

    while (my $varname = getline($dtfh)) {
        my ($modbit, $varval);
	(defined ($varval = getline($dtfh)))
	    # or die "malformed dtrace file (ppt $pptname, var $varname, no varval) $dtfhname";
	    or die "malformed dtrace file (ppt $pptname) $dtfhname";
	unless ($varname eq 'this_invocation_nonce') {
	(defined ($modbit = getline($dtfh)))
	    # or die "malformed dtrace file (ppt $pptname, var $varname, val $varval, no modbit) $dtfhname";
  	    or die "malformed dtrace file (ppt $pptname, no modbit) $dtfhname";
        }
	die "duplicate entry in dtracefile for var $varname at $pptname in $dtfhname\n"
	    if (defined $$ppthash{$varname});
	$$ppthash{$varname} = [$varval, $modbit];
        push @varorder, $varname;
    }
    $$ppthash{"variable order"} = [ @varorder ];

    return [$pptname, $pptline, $ppthash];
}

sub print_ppt ( $ ) {
# prints a ppt for debugging purposes
    my ($ppt) = @_;
    my $pptname = $$ppt[0];
    my $pptline = $$ppt[1];
    my $ppth = $$ppt[2];
    print "Name - \"${pptname}\"\n";
    print "Line number ${pptline}\n";
    foreach my $var (keys %$ppth) {
	my $val = $$ppth{$var};
	print "  variable \"${var}\" = (\"" . $$val[0] . "\", "
	    . $$val[1] . ")\n";
    }
}

sub cmp_ppts ( $$$ ) {
# Compares, according to the decls $1, the two ppts given by $2 and $3.
# Arguments 2 and 3 are "ppt_trace_info" objects (see load_ppt for definition).
    my ($declshash, $ppta, $pptb) = @_;
    if ($$ppta[0] ne $$pptb[0]) {
	print "ppt name difference: ${dtaname}=\"" . $$ppta[0] . " [line " . $$ppta[1]
        . "] ". "\", ${dtbname}=\"" . $$pptb[0] . "\" [line ". $$pptb[1] ."]\n";
        $differences_found++;
	return;
    }
    my $pptname = $$ppta[0];
    my $ppt = $$declshash{$pptname};
    if (not defined $ppt) {
	print "ppt name not in decls: \"${pptname}\"\n";
	$errors_found++;
	return;
    }
    my $ha = $$ppta[2];  my $hb = $$pptb[2];
    my @decls_varnames = @{$$ppt{"variable order"}};
    my @ppt1_varnames = @{$$ha{"variable order"}};
    my @ppt2_varnames = @{$$hb{"variable order"}};

    if ((scalar(@ppt1_varnames) > 0) && ($ppt1_varnames[0] eq "this_invocation_nonce")) {
      shift @ppt1_varnames;
    }
    if ((scalar(@ppt2_varnames) > 0) && ($ppt2_varnames[0] eq "this_invocation_nonce")) {
      shift @ppt2_varnames;
    }
    if (("@decls_varnames" ne "@ppt1_varnames")
        || ("@decls_varnames" ne "@ppt2_varnames")) {
      print "Mismatched variables for ppt $pptname.\n";
      print "  decls:   @decls_varnames\n";
      print "  trace1:  @ppt1_varnames\n";
      print "  trace2:  @ppt2_varnames\n";
      $errors_found++;
    }

    foreach my $varname (@decls_varnames) {
	my $varl = $$ppt{$varname};
	my $la = $$ha{$varname};
	my $lb = $$hb{$varname};
	#la == lb == [varval, modbit]
	if ((not defined $la) && (not defined $lb)) {
	    print "${varname} \@ ${pptname} undefined in both dtrace files\n";
	    $errors_found++;
	} elsif (not defined $la) {
	    print "${varname} \@ ${pptname} undefined in ${dtaname}\n";
	    $errors_found++;
	} elsif (not defined $lb) {
	    print "${varname} \@ ${pptname} undefined in ${dtbname}\n";
	    $errors_found++;
	} elsif ($$varl[1] eq "double") {
  	    my $difference;
	    if (($$la[0] eq "uninit")||($$lb[0] eq "uninit")) {
		$difference = !($$la[0] eq $$lb[0]);
	    } elsif (($$la[0] eq "nonsensical")||($$lb[0] eq "nonsensical")) {
		$difference = !($$la[0] eq $$lb[0]);
	    } elsif (($$la[0] eq "nan")||($$lb[0] eq "nan")) {
		$difference = !($$la[0] eq $$lb[0]);
            } elsif (($$la[0] == 0)) {
                $difference = !(abs($$lb[0]) <= 1e-4);
            } elsif (($$lb[0] == 0)) {
                $difference = !(abs($$la[0]) <= 1e-4);
	    } else {
                $difference = !(abs(1 - $$la[0]/$$lb[0]) <= 1e-4);
	    }
	    if ($difference) {
		print "${varname} \@ ${pptname} floating-point difference:\n"
		    . "  \"" . $$la[0] . "\" in ${dtaname} (line " . $$ppta[1] . ")\n"
			. "  \"" . $$lb[0] . "\" in ${dtbname} (line " . $$pptb[1] . ")\n";
		$differences_found++;
	    }
	    if ($$la[1] ne $$lb[1]) {
		print "${varname} \@ ${pptname} modbit difference:\n"
		    . "  \"" . $$la[1] . "\" in ${dtaname} (line " . $$ppta[1] . ")\n"
			. "  \"" . $$lb[1] . "\" in ${dtbname} (line " . $$pptb[1] . ")\n";
		$differences_found++;
	    }
	} else {
	    if ($$varl[1] =~ /^hashcode/) {
	        # It's a hashcode, or array of hashcodes; we only care
	        # about which ones are null or not.
	        $$la[0] =~ s/\d*[1-9]\d*/non-null/g; # match numbers except 0
	        $$lb[0] =~ s/\d*[1-9]\d*/non-null/g; # match numbers except 0
	    }
	    if ($$la[0] ne $$lb[0]) {
		print "${varname} \@ ${pptname} difference:\n"
		    . "  \"" . $$la[0] . "\" in ${dtaname} (line " . $$ppta[1] . ")\n"
			. "  \"" . $$lb[0] . "\" in ${dtbname} (line " . $$pptb[1] . ")\n";
		$differences_found++;
	    }
	    if ($$la[1] ne $$lb[1]) {
		print "${varname} \@ ${pptname} modbit difference:\n"
		    . "  \"" . $$la[1] . "\" in ${dtaname} (line " . $$ppta[1] . ")\n"
			. "  \"" . $$lb[1] . "\" in ${dtbname} (line " . $$pptb[1] . ")\n";
		$differences_found++;
	    }
	}
    }
}

sub cmp_dtracen ( $$$ ) {
# opens the dtrace files named by $2 and $3, compares them using decls hash $1
    my ($declshash, $mydtaname, $mydtbname) = @_;
#    open DTA, $mydtaname or die "couldn't open dtrace \"$mydtaname\"\n";
#    open DTB, $mydtbname or die "couldn't open dtrace \"$mydtbname\"\n";
    my $dta = gzopen(\*DTA, $mydtaname);
    my $dtb = gzopen(\*DTB, $mydtbname);

  PPT: while (1) {

      #Skip headers


      my $ppta = load_ppt($dta, $mydtaname);
      my $pptb = load_ppt($dtb, $mydtbname);
      if ((not defined $ppta) && (not defined $pptb)) {
	  last PPT;
      } elsif (not defined $ppta) {
	  print "dtrace file $mydtaname ends before $mydtbname.\n";
	  $differences_found++;
	  last PPT;
      } elsif (not defined $pptb) {
	  print "dtrace file $mydtbname ends before $mydtaname.\n";
	  $differences_found++;
	  last PPT;
      } else {
	  cmp_ppts($declshash, $ppta, $pptb);
	  next PPT;
      }
      die "Execution cannot reach this point";
  }

    close \*DTA;
    close \*DTB;
 }

sub dump_decls ( $ ) {
# dump the decls struct given by $1
    my ($declshash) = @_;
    foreach my $ppt (keys %$declshash) {
	my $lhashref = $$declshash{$ppt};
	print "\@${ppt}:\n";
	foreach my $var (keys %$lhashref) {
	    print "  ${var}:\n";
	    my ($d, $r, $l) = @{$$lhashref{$var}};
	    print "    declare ${d}\n";
	    print "    reptype ${r}\n";
	    print "    lackwit ${l}\n";
	}
    }
}

###########################################################################
### Main code
###

# load decls file
my $gdeclshash = load_decls($declsname);

# dump it
# dump_decls($gdeclshash);

# compare the dtraces
cmp_dtracen($gdeclshash, $dtaname, $dtbname);

# Exit status is same as for "diff" program:  0 if no differences, 1 if
# differences, 2 if error.
exit($errors_found ? 2 : $differences_found ? 1 : 0);
