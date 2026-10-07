#!/usr/bin/env perl

# dtrace-diff.pl
# How to invoke:  declsfile dtrace1 dtrace2
# Outputs differences that aren't hashcodes.
# Optionally also ignores differences in exit ppt numbers.

use English;
use strict;
$WARNING = 1;

# Put the script directory on the @INC path.
use File::Basename;
use lib dirname (__FILE__);
# The file `util_daikon.pm` appears in the same directory as this script.
use util_daikon;

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
# The hash maps from ppt name to the hash returned by parse_ppt_decl,
# augmented with two keys:
#   "var by name":  map from varname to the variable's hash
#   "variable order":  the names of the non-constant variables, in order.
#     These are the variables whose values appear in data trace records.
    my ($mydeclsname) = @_;
    my $decls = gzopen(\*DECLS, $mydeclsname);
    my $declshash = {};
    foreach my $ppt (read_ppt_decls($decls, $mydeclsname)) {
	my $by_name = {};
	my @varorder = ();
	foreach my $var (@{$$ppt{vars}}) {
	    $$by_name{$$var{name}} = $var;
	    if (!defined($$var{constant})) {
		push @varorder, $$var{name};
	    }
	}
	$$ppt{"var by name"} = $by_name;
	$$ppt{"variable order"} = [ @varorder ];
	$$declshash{$$ppt{name}} = $ppt;
    }
    close \*DECLS;
    return $declshash;
}

sub load_ppt ( $$ ) {
# Loads a single ppt from a dtrace fh given by $1.
# Returns a "ppt_trace_info": a 3-element array of pptname, line number in
# file, and hash mapping varname to array of value and modbit.
    my ($dtfh, $dtfhname) = @_;
    # Skip records other than data records, such as headers and program
    # point declarations in a combined .dtrace file.
    my $record;
    do {
	$record = read_record($dtfh, $dtfhname);
	(defined $record)
	    or return undef;
    } while ($record->{kind} ne "data");
    my @lines = grep { !is_comment_line($_) } split(/\n/, $record->{text});
    my $pptname = unescape_decl(shift @lines);
    my $pptline = $record->{line};

    my $ppthash = {};

    my @varorder = ();

    while (defined(my $varname = shift @lines)) {
        $varname = unescape_decl($varname);
        my ($modbit, $varval);
	(defined ($varval = shift @lines))
	    # or die "malformed dtrace file (ppt $pptname, var $varname, no varval) $dtfhname";
	    or die "malformed dtrace file (ppt $pptname) $dtfhname";
	unless ($varname eq 'this_invocation_nonce') {
	(defined ($modbit = shift @lines))
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

sub lists_equal ( $$ ) {
# Returns true if the two lists of strings, given by reference, are equal.
    my ($x, $y) = @_;
    return 0 if scalar(@$x) != scalar(@$y);
    for (my $i = 0; $i < scalar(@$x); $i++) {
	return 0 if $$x[$i] ne $$y[$i];
    }
    return 1;
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
    if (!lists_equal(\@decls_varnames, \@ppt1_varnames)
        || !lists_equal(\@decls_varnames, \@ppt2_varnames)) {
      print "Mismatched variables for ppt $pptname.\n";
      print "  decls:   " . join(" ", map { escape_decl($_) } @decls_varnames) . "\n";
      print "  trace1:  " . join(" ", map { escape_decl($_) } @ppt1_varnames) . "\n";
      print "  trace2:  " . join(" ", map { escape_decl($_) } @ppt2_varnames) . "\n";
      $errors_found++;
      foreach my $trace ([$dtaname, \@ppt1_varnames], [$dtbname, \@ppt2_varnames]) {
	my ($tracename, $trace_varnames) = @$trace;
	foreach my $varname (@$trace_varnames) {
	  my $var = $$ppt{"var by name"}{$varname};
	  if (not defined $var) {
	    print "  ${varname} appears in ${tracename} but is not declared\n";
	  } elsif (defined $$var{constant}) {
	    print "  ${varname} appears in ${tracename} but is declared as constant $$var{constant}\n";
	  }
	}
      }
    }

    foreach my $varname (@decls_varnames) {
	my $rep_type = $$ppt{"var by name"}{$varname}{rep_type};
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
	} elsif ($rep_type eq "double") {
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
	    if ($rep_type =~ /^hashcode/) {
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
    foreach my $pptname (keys %$declshash) {
	print "\@${pptname}:\n";
	foreach my $var (@{$$declshash{$pptname}{vars}}) {
	    print "  $$var{name}:\n";
	    print "    dec-type $$var{dec_type}\n";
	    print "    rep-type $$var{rep_type}\n";
	    print "    comparability $$var{comparability}\n";
	    if (defined $$var{constant}) {
		print "    constant $$var{constant}\n";
	    }
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
