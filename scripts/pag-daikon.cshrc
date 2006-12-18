# pag-daikon.cshrc
# This file should be kept in sync with pag-daikon.bashrc.

if (! $?LC_ALL) setenv LC_ALL en_US

if (! $?DAIKONPARENT) setenv DAIKONPARENT ${HOME}/research
setenv DAIKONDIR ${DAIKONPARENT}/invariants

if (! -d ${DAIKONDIR}) then
  echo "*****"
  echo "pag-daikon.cshrc cannot find ${DAIKONDIR}"
  echo "Please check out Daikon to correct this problem."
  echo "*****"
  # Default to Michael Ernst's version of Daikon, just so references to
  # ${INV} don't die, preventing this script from completing.  This is not
  # tested.
  if (-d /afs/csail.mit.edu/u/m/mernst/research/invariants) then
    setenv DAIKONDIR /afs/csail.mit.edu/u/m/mernst/research/invariants
  endif
endif

setenv DAIKONBIN ${DAIKONDIR}/scripts
setenv INV ${DAIKONDIR}
setenv inv ${INV}
setenv DAIKONCLASS_SOURCES 1
setenv PAG /afs/csail.mit.edu/group/pag
setenv pag ${PAG}

## Set this directory to the directory containing the JDK.
if (! $?JDKDIR) setenv JDKDIR /afs/csail/group/pag/software/pkg/jdk
if (! $?JDK4DIR) setenv JDK4DIR /afs/csail/group/pag/software/pkg/j2sdk-1.4.2
if (! $?JDK5DIR) setenv JDK5DIR /afs/csail/group/pag/software/pkg/j2sdk-1.5

# Remove duplicates so path and classpath don't get too long
if (-x ${INV}/scripts/path-remove.pl) then
  if ($?CLASSPATH) setenv CLASSPATH `echo $CLASSPATH | path-remove.pl`
  setenv PATH `echo $PATH | ${INV}/scripts/path-remove.pl`
endif

setenv PATH /usr/local/bin:${PATH}:/afs/csail/group/pag/projects/invariants/binaries:$DAIKONDIR/front-end/c
setenv PATH `echo $PATH | ${INV}/scripts/path-remove.pl`

if ($?debuglogin) echo "about to source daikon.cshrc: ${INV}/scripts/daikon.cshrc"
source ${INV}/scripts/daikon.cshrc
if ($?debuglogin) echo "sourced daikon.cshrc from pag-daikon.cshrc"

setenv LD_LIBRARY_PATH /usr/X11R6/lib:/usr/local/lib:/usr/lib:/lib

setenv CLASSPATH `echo $CLASSPATH | path-remove.pl`

setenv DAIKON_LIBS `/usr/bin/perl -e 'print join(":", @ARGV);' ${INV}/java/lib/*.jar`
setenv CLASSPATH .:${CLASSPATH}:${DAIKON_LIBS}
unsetenv DAIKON_LIBS

setenv LACKWIT_HOME ${INV}/front-end/c/lackwit

# Remove duplicates so path and classpath don't get too long
setenv CLASSPATH `echo $CLASSPATH | path-remove.pl`
setenv PATH `echo $PATH | ${INV}/scripts/path-remove.pl`

# Like "cvs update", but filters out output that is unlikely to be of interest.
# Alternately, run CVS under emacs via "M-x cvs-update".
alias	cvsupdate	'cvs -q update -d \!* |& egrep -e "^C |update aborted|non-existent repository|Permission denied|cannot open|^cvs update: [^U]"'

# Enable use of group bibliographies, and the "bibfind" command.
# Private comments in bib files are not to be shared outside the group.
setenv BIBINPUTS .:$PAG/doc/wisdom/latest-read-only/bib:..:
alias bibfind 'java -jar $pag/software/arch/common/pkg/lookup.jar -l -f $PAG/doc/wisdom/latest-read-only/bib/bibroot'

if (! $?EDITOR) setenv EDITOR emacsclient
if (! $?ALTERNATE_EDITOR) setenv ALTERNATE_EDITOR emacs
if (! $?VISUAL) setenv VISUAL emacsclient
