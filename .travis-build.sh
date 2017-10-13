#!/bin/bash

# Optional argument $1 is one of:
#   all, quick-txt-diff, nonquick-txt-diff, non-txt-diff, misc, kvasir
# If it is omitted, this script does everything.

export GROUP=$1
if [[ "${GROUP}" == "" ]]; then
  export GROUP=all
fi

if [[ "${GROUP}" != "all" && "${GROUP}" != "quick-txt-diff" && "${GROUP}" != "nonquick-txt-diff" && "${GROUP}" != "non-txt-diff" && "${GROUP}" != "misc" && "${GROUP}" != "kvasir" && "${GROUP}" != "dyncomp" ]]; then
  echo "Bad argument '${GROUP}'; should be omitted or one of: all, quick-txt-diff, nonquick-txt-diff, non-txt-diff, misc, kvasir."
  exit 1
fi


# Fail the whole script if any command fails
set -e
set -o pipefail

## Useful for debugging and sometimes for interpreting the script.
# # Output lines of this script as they are read.
# set -o verbose
# # Output expanded lines of this script as they are executed.
# set -o xtrace

export SHELLOPTS

# TODO: The tests ought to work even if $DAIKONDIR is not set.
export DAIKONDIR=`pwd`

./.travis-build-without-test.sh

# The JDK was built already; there is no need to rebuild it again.
# Don't use "-d" to debug ant, because that results in a log so long
# that Travis truncates the log and terminates the job.

if [[ "${GROUP}" == "quick-txt-diff" || "${GROUP}" == "all" ]]; then
  echo ".travis-build.sh is running quick-txt-diff tests"
  make dyncomp-jdk
  make -C tests MPARG=-Otarget quick-txt-diff results
fi

if [[ "${GROUP}" == "nonquick-txt-diff" || "${GROUP}" == "all" ]]; then
  echo ".travis-build.sh is running nonquick-txt-diff tests"
  make dyncomp-jdk
  make -C tests MPARG=-j1 nonquick-txt-diff results
fi

if [[ "${GROUP}" == "non-txt-diff" || "${GROUP}" == "all" ]]; then
  echo ".travis-build.sh is running non-txt-diff tests"
  make -C tests non-txt-diff results
fi

if [[ "${GROUP}" == "misc" || "${GROUP}" == "all" ]]; then
  ## misc tests: miscellaneous tests that shouldn't depend on JDK version.
  ## (Maybe they don't even need the full ./.travis-build-without-test.sh .)
  echo ".travis-build.sh is running misc tests"

  # Code style & quality
  make -C java error-prone

  # Code formatting
  # Problem: this target requires Java 8, but is being run under Java 7.
  # It is failing, but is not failing the build.  Why?
  make -C java check-format

  # Documentation
  make javadoc doc-all
fi

if [[ "${GROUP}" == "kvasir" || "${GROUP}" == "all" ]]; then
  echo ".travis-build.sh is running kvasir and DynComp tests"

  # Running Kvasir tests here may seem redundant with the fjalar project's Travis
  # build; however, it  means that they are run on each branch and pull request.

  # The Valgrind configure script fails if SHELLOPTS is defined.
  export -n SHELLOPTS
  make kvasir

  make -C tests/dyncomp-tests regression-tests
  make -C tests/kvasir-tests regression-tests
fi
