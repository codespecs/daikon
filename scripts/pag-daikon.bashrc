# pag-daikon.bashrc
# This file should be kept in sync with pag-daikon.cshrc.

export DAIKONPARENT=${HOME}/research
export DAIKONDIR=${DAIKONPARENT}/invariants
export INV=${DAIKONPARENT}/invariants
export inv=${INV}
export DAIKONCLASS_SOURCES=1

source ${INV}/scripts/daikon.bashrc

# Should also remove "daikon/bin" from PATH.
export PATH=$DAIKONDIR/scripts:/usr/local/bin:${PATH}:/home/httpd/html/pag/daikon/download/binaries/mit:/g4/projects/invariants/tools/escjava/current/bin

export LD_LIBRARY_PATH=/usr/X11R6/lib:/usr/local/lib:/usr/lib:/lib

export DAIKON_LIBS=`/usr/bin/perl -e 'print join(":", @ARGV);' ${INV}/java/lib/*.jar`
export CLASSPATH=.:${CLASSPATH}:${DAIKON_LIBS}

## Someone needs to rewrite this as a shell function, since bash aliases
## can't handle arguments.
## # Like "cvs update", but filters out output that is unlikely to be of interest.
## # Alternately, run CVS under emacs via "M-x cvs-update".
## alias	cvsupdate	'cvs -q update -d \!* |& egrep -e "^C |update aborted|non-existent repository|Permission denied|cannot open|^cvs update: [^U]"'

alias jikes='/g2/users/mernst/bin/Linux-i686/jikes -g +E +F'
