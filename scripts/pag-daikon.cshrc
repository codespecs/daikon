# pag-daikon.cshrc
# This file should be kept in sync with pag-daikon.bashrc.

if (! $?LC_ALL) setenv LC_ALL en_US

if (! $?DAIKONPARENT) setenv DAIKONPARENT ${HOME}/research
setenv DAIKONDIR ${DAIKONPARENT}/invariants
setenv DAIKONBIN ${DAIKONDIR}/scripts
setenv INV ${DAIKONDIR}
setenv inv ${INV}
setenv DAIKONCLASS_SOURCES 1

## Set this directory to the directory containing the JDK.
# Simplify to just the first branch after the AFS move is done.
if (-e /afs/csail/group/pag/software) then
    if (! $?JDKDIR) setenv JDKDIR /afs/csail/group/pag/software/pkg/jdk
else
    if (! $?JDKDIR) setenv JDKDIR /g2/jdk
endif

# Remove references to /g4 after AFS move is complete
setenv PATH /usr/local/bin:${PATH}:/g4/projects/invariants/binaries:/afs/csail/group/pag/projects/invariants/binaries:/g4/projects/invariants/tools/escjava/current/bin

source ${INV}/scripts/daikon.cshrc

setenv LD_LIBRARY_PATH /usr/X11R6/lib:/usr/local/lib:/usr/lib:/lib

if (! $?DAIKON_LIBS) then
  setenv DAIKON_LIBS `/usr/bin/perl -e 'print join(":", @ARGV);' ${INV}/java/lib/*.jar`
  setenv CLASSPATH .:${CLASSPATH}:${DAIKON_LIBS}
endif
setenv LACKWIT_HOME ${INV}/front-end/c/lackwit

# Remove duplicates so path and classpath don't get too long
setenv CLASSPATH `echo $CLASSPATH | path-remove.pl`
setenv PATH `echo $PATH | ${INV}/scripts/path-remove.pl`

# Like "cvs update", but filters out output that is unlikely to be of interest.
# Alternately, run CVS under emacs via "M-x cvs-update".
alias	cvsupdate	'cvs -q update -d \!* |& egrep -e "^C |update aborted|non-existent repository|Permission denied|cannot open|^cvs update: [^U]"'

setenv DFEJ_VERBOSE 1

# Update after AFS move
if (-e /afs/csail.mit.edu/u/m/mernst) then
    setenv BIBINPUTS .:/afs/csail.mit.edu/u/m/mernst/bib:..:
    alias bibfind /afs/csail.mit.edu/u/m/mernst/bin/Linux-i686/help .n .F /afs/csail.mit.edu/u/m/mernst/bib/bibroot.non-mde
else if (-e /g2/users/mernst) then
    setenv BIBINPUTS .:/g2/users/mernst/bib:..:
    alias bibfind /g2/users/mernst/bin/Linux-i686/help .n .F /g2/users/mernst/bib/bibroot.non-mde
else if (-e /var/autofs/net/pag/g2/users/mernst/bib) then
    setenv BIBINPUTS .:/var/autofs/net/pag/g2/users/mernst/bib:..:
    alias bibfind /var/autofs/net/pag/g2/users/mernst/bin/Linux-i686/help .n .F /var/autofs/net/pag/g2/users/mernst/bib/bibroot.non-mde
endif

if (! $?EDITOR) setenv EDITOR emacsclient
if (! $?ALTERNATE_EDITOR) setenv ALTERNATE_EDITOR emacs
if (! $?VISUAL) setenv VISUAL emacsclient

# So that Eclipse is always started from the same directory.
alias eclipse 'eclipse -data $HOME/.eclipse'

