# pag-daikon.cshrc
# This file should be kept in sync with pag-daikon.bashrc.

if (! $?LC_ALL) setenv LC_ALL en_US

if (! $?DAIKONPARENT) setenv DAIKONPARENT ${HOME}/research
setenv DAIKONDIR ${DAIKONPARENT}/invariants
setenv DAIKONBIN ${DAIKONDIR}/scripts
setenv INV ${DAIKONDIR}
setenv inv ${INV}
setenv DAIKONCLASS_SOURCES 1

setenv PATH /usr/local/bin:${PATH}:/g4/projects/invariants/binaries:/g4/projects/invariants/tools/escjava/current/bin

source ${INV}/scripts/daikon.cshrc

setenv LD_LIBRARY_PATH /usr/X11R6/lib:/usr/local/lib:/usr/lib:/lib

setenv DAIKON_LIBS `/usr/bin/perl -e 'print join(":", @ARGV);' ${INV}/java/lib/*.jar`
setenv CLASSPATH .:${CLASSPATH}:${DAIKON_LIBS}
setenv LACKWIT_HOME ${INV}/front-end/c/lackwit

# Remove duplicates so path and classpath don't get too long
setenv CLASSPATH `echo $CLASSPATH | path-remove.pl`
setenv PATH `echo $PATH | ${INV}/scripts/path-remove.pl`

# Like "cvs update", but filters out output that is unlikely to be of interest.
# Alternately, run CVS under emacs via "M-x cvs-update".
alias	cvsupdate	'cvs -q update -d \!* |& egrep -e "^C |update aborted|non-existent repository|Permission denied|cannot open|^cvs update: [^U]"'

alias jikes "/g2/users/mernst/bin/Linux-i686/jikes-1.15 -g +E +F"

setenv DFEJ_VERBOSE 1

setenv BIBINPUTS .:/g2/users/mernst/bib:..:
alias bibfind /g2/users/mernst/bin/Linux-i686/help .n .F /g2/users/mernst/bib/bibroot.non-mde

setenv EDITOR emacsclient
setenv ALTERNATE_EDITOR emacs
setenv VISUAL emacsclient

# So that Eclipse is always started from the same directory.
# alias eclipse 'eclipse -data $HOME/.eclipse''
