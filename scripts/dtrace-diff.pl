#!/usr/local/bin/perl

#dtrace-diff
#arguments:  dtrace-diff declsfile dtrace1 dtrace2
#outputs differences that aren't hashcodes.

($decls, $dta, $dtb) = @ARGV;

#load decls file
$gdeclshash = load_decls($decls);

#dump it
#dump_decls($gdeclshash);

#compare the dtraces
cmp_dtracen($gdeclshash, $dta, $dtb);

sub getline {
#gets a line from the filehandle $1
    my $fh = shift;
    my $l = <$fh>;
    chomp $l;
    if ($l =~ m|^\#|) {
	return getline($fh);
    }
    return $l;
}

sub load_decls {
#loads the decls file given by $1 into a hash, returns a ref
    my $decls = shift;
    open DECLS, $decls or die "couldn't open decls \"$decls\"\n";
    my $declshash = {};
    my $firstppt = 1;
    while (defined (my $l = getline(DECLS))) {
	if ($l eq "DECLARE") {
	    my $currppt = getline(DECLS);
	    my $lhashref = {};
	    while (my $varname = getline(DECLS)) {
		(defined (my $dtype = getline(DECLS)))
		    or die "malformed decls file";
		(defined (my $rtype = getline(DECLS)))
		    or die "malformed decls file";
		(defined (my $ltype = getline(DECLS)))
		    or die "malformed decls file";
		$$lhashref{$varname} = [$dtype, $rtype, $ltype];
	    }
	    $$declshash{$currppt} = $lhashref;
	} elsif (($l eq "VarComparability") && $firstppt) {
	    #it's ok to have a VarComparability as the first thing
	    #in the decls file.  Read the type of comparability,
	    #then move on.
	    $l = getline(DECLS);
	} elsif ($l) {
	    die "malformed decls file";
	}
	$firstppt = 0;
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

    my $ppthash = {};

    while (my $varname = getline($dtfh)) {
	(defined (my $varval = getline($dtfh)))
	    or die "malformed dtrace file";
	(defined (my $modbit = getline($dtfh)))
	    or die "malformed dtrace file";
	die "duplicate entry in dtracefile for var $varname at $pptname\n"
	    if (defined $$ppthash{$varname});
	$$ppthash{$varname} = [$varval, $modbit];
    }

    return [$pptname, $ppthash];
}

sub print_ppt {
#prints a ppt for debugging purposes
    my $ppt = shift;
    my $pptname = $$ppt[0];
    my $ppth = $$ppt[1];
    print "Name - \"${pptname}\"\n";
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
	print "ppt name difference: ${dta}=\"" . $$ppta[0] .
	    "\", ${dtb}=\"" . $$pptb[0] . "\"\n";
	return;
    }
    my $pptname = $$ppta[0];
    my $ppt = $$declshash{$pptname};
    if (not defined $ppt) {
	print "ppt name not in decls: \"${pptname}\"\n";
	return;
    }
    my $ha = $$ppta[1];  my $hb = $$pptb[1]; 
    foreach my $varname (keys %$ppt) {
	my $varl = $$ppt{$varname};
	my $la = $$ha{$varname};
	my $lb = $$hb{$varname};
	#la == lb == [varval, modbit]
	if ((not defined $la) && (not defined $lb)) {
	    print "${varname} \@ ${pptname} undefined in both dtrace files\n";
	} elsif (not defined $la) {
	    print "${varname} \@ ${pptname} undefined in ${dta}\n";
	} elsif (not defined $lb) {
	    print "${varname} \@ ${pptname} undefined in ${dtb}\n";
	} elsif ($$varl[1] eq "hashcode") {
	    #it's a hashcode - we don't care if they're different
	} elsif ($$varl[1] eq "double") {
	    if (abs($$la[0] - $$lb[0]) <= 0.00001) {
		print "${varname} \@ ${pptname} floating-point difference:\n"
		    . "  \"" . $$la[0] . "\" in ${dta}\n"
			. "  \"" . $$lb[0] . "\" in ${dtb}\n";
	    }
	    if ($$la[1] ne $$lb[1]) {
		print "${varname} \@ ${pptname} modbit difference:\n"
		    . "  \"" . $$la[1] . "\" in ${dta}\n"
			. "  \"" . $$lb[1] . "\" in ${dtb}\n";
	    }
	} else {
	    if ($$la[0] ne $$lb[0]) {
		print "${varname} \@ ${pptname} difference:\n"
		    . "  \"" . $$la[0] . "\" in ${dta}\n"
			. "  \"" . $$lb[0] . "\" in ${dtb}\n";
	    }
	    if ($$la[1] ne $$lb[1]) {
		print "${varname} \@ ${pptname} modbit difference:\n"
		    . "  \"" . $$la[1] . "\" in ${dta}\n"
			. "  \"" . $$lb[1] . "\" in ${dtb}\n";
	    }
	}
    }
}

sub cmp_dtracen {
#opens the dtrace files named by $2 and $3, compares them using decls hash $1
    my $declshash = shift;
    my $dta = shift;
    my $dtb = shift;
    open DTA, $dta or die "couldn't open dtrace \"$dta\"\n";
    open DTB, $dtb or die "couldn't open dtrace \"$dtb\"\n";

  PPT: while (1) {
      my $ppta = load_ppt(DTA); my $pptb = load_ppt(DTB);
      if ((not defined $ppta) && (not defined $pptb)) {
	  last PPT;
      } elsif (not defined $ppta) {
	  print "dtrace file $dta ends before $dtb.\n";
	  last PPT;
      } elsif (not defined $pptb) {
	  print "dtrace file $dtb ends before $dta.\n";
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
