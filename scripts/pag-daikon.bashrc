# pag-daikon.bashrc
# This file should be kept in sync with pag-daikon.cshrc.

export LC_ALL=${LC_ALL:-en_US}

export DAIKONPARENT=${DAIKONPARENT:-${HOME}/research}
export DAIKONDIR=${DAIKONPARENT}/invariants
export DAIKONBIN=${DAIKONDIR}/scripts
export INV=${DAIKONDIR}
export inv=${INV}
export DAIKONCLASS_SOURCES=1

export PATH=/usr/local/bin:${PATH}:/g4/projects/invariants/binaries:/g4/projects/invariants/tools/escjava/current/bin:$DAIKONDIR/front-end/c

source ${INV}/scripts/daikon.bashrc

export LD_LIBRARY_PATH=/usr/X11R6/lib:/usr/local/lib:/usr/lib:/lib

export DAIKON_LIBS=`/usr/bin/perl -e 'print join(":", @ARGV);' ${INV}/java/lib/*.jar`
export CLASSPATH=.:${CLASSPATH}:${DAIKON_LIBS}
export LACKWIT_HOME=${INV}/front-end/c/lackwit

# Remove duplicates so path and classpath don't get too long
export CLASSPATH=`echo $CLASSPATH | path-remove.pl`
export PATH=`echo $PATH | ${INV}/scripts/path-remove.pl`


## Someone needs to rewrite this as a shell function, since bash aliases
## can't handle arguments.
## # Like "cvs update", but filters out output that is unlikely to be of interest.
## # Alternately, run CVS under emacs via "M-x cvs-update".
## alias	cvsupdate	'cvs -q update -d \!* |& egrep -e "^C |update aborted|non-existent repository|Permission denied|cannot open|^cvs update: [^U]"'

alias jikes='/g2/users/mernst/bin/Linux-i686/jikes -g +E +F'

export DFEJ_VERBOSE=1

export BIBINPUTS=.:/g2/users/mernst/bib:..:
alias bibfind='/g2/users/mernst/bin/Linux-i686/help .n .F /g2/users/mernst/bib/bibroot.non-mde'
