#!/usr/bin/env perl

#dtrace-diff
#arguments:  dtrace-diff declsfile dtrace1 dtrace2
#outputs differences that aren't hashcodes.

use English;
# use strict;
$WARNING = 1;

($declsname, $dtaname, $dtbname) = @ARGV;

#load decls file
$gdeclshash = load_decls($declsname);

#dump it
#dump_decls($gdeclshash);

#compare the dtraces
cmp_dtracen($gdeclshash, $dtaname, $dtbname);

sub getline {
#gets a line from the filehandle/count $1
    my $fhobj = shift;
    my $fh = $$fhobj[0];
    my $l;
    do {
	$l = <$fh>;
	if ($l) {chomp $l; ($$fhobj[1])++;}
    } while ($l && ($l =~ m|^\#|));
    return $l;
}

sub load_decls {
#loads the decls file given by $1 into a hash, returns a ref
    my $declsname = shift;
    open DECLS, $declsname or die "couldn't open decls \"$decls\"\n";
    my $decls = [DECLS, 0];
    my $declshash = {};
    my $ppt_seen = 0;
    while (defined (my $l = getline($decls))) {
        $l =~ s://.*::; # strip any comments on this line
	if ($l eq "DECLARE") {
	    my $currppt = getline($decls);
	    my $lhashref = {};
	    while (my $varname = getline($decls)) {
		(defined (my $dtype = getline($decls)))
		    or die "malformed decls file";
		(defined (my $rtype = getline($decls)))
		    or die "malformed decls file";
		(defined (my $ltype = getline($decls)))
		    or die "malformed decls file";
		$$lhashref{$varname} = [$dtype, $rtype, $ltype];
	    }
	    $$declshash{$currppt} = $lhashref;
	    $ppt_seen = 1;
	} elsif (($l eq "VarComparability") && !$ppt_seen) {
	    #it's ok to have a VarComparability as the first thing
	    #in the decls file.  Read the type of comparability,
	    #then move on.
	    $l = getline($decls);
	} elsif (($l eq "ListImplementors") && !$ppt_seen) {
	    #it's ok to have a ListImplementors in the decls file.
	    #Read the type of comparability, then move on.
	    $l = getline($decls);
	} elsif ($l) {
	    die "malformed decls file: \"$l\" at line" . $$decls[1];
	}
    }
    close DECLS;
    return $declshash;
}

sub load_ppt {
#loads a single ppt from a dtrace fh given by $1
    my $dtfh = shift;
    my $pptname = getline($dtfh);
    while ((defined $pptname) && ($pptname eq "")) {
	$pptname = getline($dtfh);
    }
    (defined $pptname)
	or return undef;

    my $pptline = $$dtfh[1]-1;

    my $ppthash = {};

    while (my $varname = getline($dtfh)) {
        my $modbit, $varval;
	(defined ($varval = getline($dtfh)))
	    or die "malformed dtrace file";
	unless ($varname eq 'this_invocation_nonce') {
	(defined ($modbit = getline($dtfh)))
	    or die "malformed dtrace file";
        }
	die "duplicate entry in dtracefile for var $varname at $pptname\n"
	    if (defined $$ppthash{$varname});
	$$ppthash{$varname} = [$varval, $modbit];
    }

    return [$pptname, $pptline, $ppthash];
}

sub print_ppt {
#prints a ppt for debugging purposes
    my $ppt = shift;
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

sub cmp_ppts {
#compares the two ppts given by $2 and $3 according to the decls $1
    my $declshash = shift;
    my $ppta = shift;
    my $pptb = shift;
    if ($$ppta[0] ne $$pptb[0]) {
	print "ppt name difference: ${dtaname}=\"" . $$ppta[0] .
	    "\", ${dtbname}=\"" . $$pptb[0] . "\"\n";
	return;
    }
    my $pptname = $$ppta[0];
    my $ppt = $$declshash{$pptname};
    if (not defined $ppt) {
	print "ppt name not in decls: \"${pptname}\"\n";
	return;
    }
    my $ha = $$ppta[2];  my $hb = $$pptb[2];
    foreach my $varname (keys %$ppt) {
	my $varl = $$ppt{$varname};
	my $la = $$ha{$varname};
	my $lb = $$hb{$varname};
	#la == lb == [varval, modbit]
	if ((not defined $la) && (not defined $lb)) {
	    print "${varname} \@ ${pptname} undefined in both dtrace files\n";
	} elsif (not defined $la) {
	    print "${varname} \@ ${pptname} undefined in ${dtaname}\n";
	} elsif (not defined $lb) {
	    print "${varname} \@ ${pptname} undefined in ${dtbname}\n";
	} elsif ($$varl[1] eq "double") {
	    if (($$la[0] eq "uninit")||($$lb[0] eq "uninit")) {
		$difference = !($$la[0] eq $$lb[0]);
	    } elsif (($$la[0] eq "nan")||($$lb[0] eq "nan")) {
		$difference = !($$la[0] eq $$lb[0]);
	    } else {
		$difference = (abs($$la[0] - $$lb[0]) > 0.00001)
	    }
	    if ($difference) {
		print "${varname} \@ ${pptname} floating-point difference:\n"
		    . "  \"" . $$la[0] . "\" in ${dtaname} (line " . $$ppta[1] . ")\n"
			. "  \"" . $$lb[0] . "\" in ${dtbname} (line " . $$pptb[1] . ")\n";
	    }
	    if ($$la[1] ne $$lb[1]) {
		print "${varname} \@ ${pptname} modbit difference:\n"
		    . "  \"" . $$la[1] . "\" in ${dtaname} (line " . $$ppta[1] . ")\n"
			. "  \"" . $$lb[1] . "\" in ${dtbname} (line " . $$pptb[1] . ")\n";
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
	    }
	    if ($$la[1] ne $$lb[1]) {
		print "${varname} \@ ${pptname} modbit difference:\n"
		    . "  \"" . $$la[1] . "\" in ${dtaname} (line " . $$ppta[1] . ")\n"
			. "  \"" . $$lb[1] . "\" in ${dtbname} (line " . $$pptb[1] . ")\n";
	    }
	}
    }
}

sub cmp_dtracen {
#opens the dtrace files named by $2 and $3, compares them using decls hash $1
    my $declshash = shift;
    my $dtaname = shift;
    my $dtbname = shift;
    open DTA, $dtaname or die "couldn't open dtrace \"$dtaname\"\n";
    open DTB, $dtbname or die "couldn't open dtrace \"$dtbname\"\n";

    $dta = [DTA, 0];
    $dtb = [DTB, 0];

  PPT: while (1) {
      my $ppta = load_ppt($dta); my $pptb = load_ppt($dtb);
      if ((not defined $ppta) && (not defined $pptb)) {
	  last PPT;
      } elsif (not defined $ppta) {
	  print "dtrace file $dtaname ends before $dtbname.\n";
	  last PPT;
      } elsif (not defined $pptb) {
	  print "dtrace file $dtbname ends before $dtaname.\n";
	  last PPT;
      } else {
	  cmp_ppts($declshash, $ppta, $pptb);
	  next PPT;
      }
  }

    close DTA;
    close DTB;
}

sub dump_decls {
#dump the decls struct given by $1
    my $declshash = shift;
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
