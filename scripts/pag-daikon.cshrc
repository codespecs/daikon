# pag-daikon.cshrc
# This file should be kept in sync with pag-daikon.bashrc.

setenv DAIKONPARENT ${HOME}/research
setenv DAIKONDIR ${DAIKONPARENT}/invariants
setenv INV ${DAIKONPARENT}/invariants
setenv inv ${INV}
setenv DAIKONCLASS_SOURCES 1

source ${INV}/scripts/daikon.cshrc

# Should also remove "invariants/bin" from PATH.
setenv PATH $DAIKONDIR/scripts:/usr/local/bin:${PATH}:/home/httpd/html/pag/daikon/download/binaries/mit:/g4/projects/invariants/tools/escjava/current/bin

setenv LD_LIBRARY_PATH /usr/X11R6/lib:/usr/local/lib:/usr/lib:/lib

setenv DAIKON_LIBS `/usr/bin/perl -e 'print join(":", @ARGV);' ${INV}/java/lib/*.jar`
setenv CLASSPATH .:${CLASSPATH}:${DAIKON_LIBS}
setenv LACKWIT_HOME ${INV}/front-end/c/lackwit

# Like "cvs update", but filters out output that is unlikely to be of interest.
# Alternately, run CVS under emacs via "M-x cvs-update".
alias	cvsupdate	'cvs -q update -d \!* |& egrep -e "^C |update aborted|non-existent repository|Permission denied|cannot open|^cvs update: [^U]"'

alias jikes /g2/users/mernst/bin/Linux-i686/jikes -g +E +F
