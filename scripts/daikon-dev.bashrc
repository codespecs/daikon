# daikon-dev.bashrc

export LC_ALL=${LC_ALL:-en_US}

DAIKONDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )../" && pwd )"

export DAIKONSCRIPTS=${DAIKONDIR}/scripts
: "${PLUME_SCRIPTS:=${DAIKONDIR}/.utils/plume-scripts}"
export PLUME_SCRIPTS
export DAIKONCLASS_SOURCES=1
export PAG=/afs/csail.mit.edu/group/pag
export pag=${PAG}

# "source" is a bash, not sh, feature, so use "." instead
. ${DAIKONDIR}/scripts/daikon.bashrc

export LD_LIBRARY_PATH=/usr/X11R6/lib:/usr/local/lib:/usr/lib:/lib

# Remove duplicates so path and classpath don't get too long
if [ -x ${PLUME_SCRIPTS}/path-remove.pl ]; then
  export CLASSPATH=$(echo "${CLASSPATH}" | ${PLUME_SCRIPTS}/path-remove.pl)
  export PATH=$(echo "${PATH}" | ${PLUME_SCRIPTS}/path-remove.pl)
fi

export EDITOR=${EDITOR:-emacsclient}
export ALTERNATE_EDITOR=${ALTERNATE_EDITOR:-emacs}
export VISUAL=${VISUAL:-emacsclient}
