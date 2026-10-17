## daikon.bashrc
## Daikon initialization file for bash users.
## It sets DAIKONDIR and puts Daikon's scripts on your PATH.
## If JAVA_HOME is set, it also appends $JAVA_HOME/bin to your PATH.

## You should not need to edit this file.

if [ ${#BASH_SOURCE[@]} -eq 0 ]; then
  # Cannot infer DAIKONDIR.
  if [ -z ${DAIKONDIR+x} ]; then
    echo "Please set DAIKONDIR environment variable.  Aborting daikon.bashrc ."
    return 2
  elif [ ! -d "$DAIKONDIR" ]; then
    echo "DAIKONDIR is set to non-existent directory: $DAIKONDIR"
    echo "Please set DAIKONDIR to an existing directory.  Aborting daikon.bashrc ."
    return 2
  fi
else
  ## Note that this overrides any previous setting.
  # Mac OS does not have "-e" argument to readlink
  # export DAIKONDIR="$( readlink -e "$( dirname "${BASH_SOURCE[0]}" )/..")"
  # Code from: https://stackoverflow.com/q/59895/173852
  export DAIKONDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." >/dev/null && pwd )"
fi

export PATH=${DAIKONDIR}/scripts:${PLUME_SCRIPTS:-${DAIKONDIR}/.utils/plume-scripts}:${PATH}${JAVA_HOME:+:${JAVA_HOME}/bin}
