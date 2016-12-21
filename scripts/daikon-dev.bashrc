# daikon-dev.bashrc
# This file should be kept in sync with daikon-dev.cshrc.

export LC_ALL=${LC_ALL:-en_US}

export DAIKONPARENT=${DAIKONPARENT:-${HOME}/research/invariants}
export DAIKONDIR=${DAIKONPARENT}/daikon

if [ ! -d "${DAIKONDIR}" ]; then
  echo "*****"
  echo "daikon-dev.bashrc cannot find ${DAIKONDIR}"
  echo "Please check out Daikon to correct this problem."
  echo "Or, if you've checked it out to a different location, set the"
  echo "DAIKONPARENT environment variable to point to the directory that"
  echo "contains the 'invariants' directory."
  echo "*****"
  # Default to Michael Ernst's version of Daikon, just so references to
  # ${INV} don't die, preventing this script from completing.
  if [ -d /afs/csail.mit.edu/u/m/mernst/research/invariants ]; then
    export DAIKONDIR=/afs/csail.mit.edu/u/m/mernst/research/invariants
  else
    # If we couldn't find suitable scripts anywhere, we can't do anything
    # sensible, so get out before we do any damage.
    return 1;
  fi
fi

export DAIKONBIN=${DAIKONDIR}/scripts
export PLUMEBIN=${DAIKONDIR}/plume-lib/bin
# export INV=${DAIKONDIR}
# export inv=${INV}
export DAIKONCLASS_SOURCES=1
export PAG=/afs/csail.mit.edu/group/pag
export pag=${PAG}

## Set this directory to the directory containing the JDK.
export JAVA_HOME=${JAVA_HOME:-/afs/csail/group/pag/software/pkg/jdk}

# "source" is a bash, not sh, feature, so use "." instead
. ${DAIKONDIR}/scripts/daikon.bashrc

export LD_LIBRARY_PATH=/usr/X11R6/lib:/usr/local/lib:/usr/lib:/lib

export DAIKON_LIBS=`/usr/bin/perl -e 'print join(":", @ARGV);' ${INV}/java/lib/*.jar`
export CLASSPATH=${DAIKON_LIBS}:.:${CLASSPATH}
unset DAIKON_LIBS

export LACKWIT_HOME=${INV}/front-end/c/lackwit

# Remove duplicates so path and classpath don't get too long
if [ -x ${PLUMEBIN}/path-remove.pl ]; then
  export CLASSPATH=`echo $CLASSPATH | ${PLUMEBIN}/path-remove.pl`
  export PATH=`echo $PATH | ${PLUMEBIN}/path-remove.pl`
fi

# Enable use of group bibliographies, and the "bibfind" command.
# Private comments in bib files are not to be shared outside the group.
export BIBINPUTS=.:$PAG/doc/wisdom/latest-read-only/bib:..:
alias bibfind='java -jar $pag/software/arch/common/pkg/lookup.jar -l -f $PAG/doc/wisdom/latest-read-only/bib/bibroot'

export EDITOR=${EDITOR:-emacsclient}
export ALTERNATE_EDITOR=${ALTERNATE_EDITOR:-emacs}
export VISUAL=${VISUAL:-emacsclient}
