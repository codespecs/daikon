# pag-daikon.bashrc
# This file should be kept in sync with pag-daikon.cshrc.

export LC_ALL=${LC_ALL:-en_US}

export DAIKONPARENT=${DAIKONPARENT:-${HOME}/research}
export DAIKONDIR=${DAIKONPARENT}/invariants
export DAIKONBIN=${DAIKONDIR}/scripts
export INV=${DAIKONDIR}
export inv=${INV}
export DAIKONCLASS_SOURCES=1

## Set this directory to the directory containing the JDK.
# Simplify to just the first branch after the AFS move is done.
if [ -e /afs/csail/group/pag ]; then
  export JDKDIR=${JDKDIR:-/afs/csail/group/pag/software/pkg/jdk}
else
  export JDKDIR=${JDKDIR:-/g2/jdk}
fi

# Remove references to /g4 after AFS move is complete
export PATH=/usr/local/bin:${PATH}:/g4/projects/invariants/binaries:/afs/csail/group/pag/projects/invariants/binaries:/g4/projects/invariants/tools/escjava/current/bin:$DAIKONDIR/front-end/c

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

export DFEJ_VERBOSE=1

# Update after AFS move
if [ -e /g2/users/mernst ]; then
  alias bibfind='/g2/users/mernst/bin/Linux-i686/help .n .F /g2/users/mernst/bib/bibroot.non-mde'
  export BIBINPUTS=.:/g2/users/mernst/bib:..:
elif [ -e /var/autofs/net/pag/g2/users/mernst ]; then
  alias bibfind='/var/autofs/net/pag/g2/users/mernst/bin/Linux-i686/help .n .F /var/autofs/net/pag/g2/users/mernst/bib/bibroot.non-mde'
  export BIBINPUTS=.:/var/autofs/net/pag/g2/users/mernst/bib:..:
fi

export EDITOR=${EDITOR:-emacsclient}
export ALTERNATE_EDITOR=${ALTERNATE_EDITOR:-emacs}
export VISUAL=${VISUAL:-emacsclient}

# So that Eclipse is always started from the same directory.
alias eclipse='eclipse -data $HOME/.eclipse'
