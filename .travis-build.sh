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

make showvars compile daikon.jar

# The JDK was built already; there is no need to rebuild it again.
# Don't use "-d" to debug ant, because that results in a log so long
# that Travis truncates the log and terminates the job.

if [[ "${GROUP}" == "quick-txt-diff" || "${GROUP}" == "all" ]]; then
  echo ".travis-build.sh is running quick-txt-diff tests"
  make dyncomp-jdk
  MAKE_VERSION=$(make --version 2>&1 | head -1)
  if [[ $MAKE_VERSION =~ "GNU Make 4" ]]; then
    MPARG_ARG="MPARG=-Otarget"
  fi
  make -C tests $MPARG_ARG quick-txt-diff results
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
  make -C java check-format

  # Documentation
  make javadoc doc-all

  if [ -d "/tmp/plume-scripts" ] ; then
    (cd /tmp/plume-scripts && git pull -q) > /dev/null 2>&1
  else
    (cd /tmp && (git clone --depth 1 -q https://github.com/plume-lib/plume-scripts.git || (sleep 1m && git clone --depth 1 -q https://github.com/plume-lib/plume-scripts.git)))
  fi
  # For refactorings that touch a lot of code that you don't understand, create
  # top-level file SKIP-REQUIRE-JAVADOC.  Delete it after the pull request is merged.
  if [ ! -f SKIP-REQUIRE-JAVADOC ]; then
    (make -C java requireJavadocPrivate > /tmp/warnings.txt 2>&1) || true
    /tmp/plume-scripts/ci-lint-diff /tmp/warnings.txt
  fi
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
