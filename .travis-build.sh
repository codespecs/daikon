#!/bin/bash

# Fail the whole script if any command fails
set -e

## Useful for debugging and sometimes for interpreting the script.
# # Output lines of this script as they are read.
# set -o verbose
# # Output expanded lines of this script as they are executed.
# set -o xtrace

export SHELLOPTS

# TODO: The tests ought to work even if $DAIKONDIR is not set.
export DAIKONDIR=`pwd`

./.travis-build-without-test.sh

# Optional argument $1 is one of:  quick-txt-diff, nonquick-txt-diff, non-txt-diff, misc
# If it is omitted, this script does everything.

# The JDK was built already; there is no need to rebuild it again.
# Don't use "-d" to debug ant, because that results in a log so long
# that Travis truncates the log and terminates the job.

if [[ "$1" != "nonquick-txt-diff" && "$1" != "non-txt-diff" && "$1" != "misc" ]]; then
  # Daikon txt-diff tests
  echo ".travis-build.sh is running quick-txt-diff tests"
  make dyncomp-jdk
  make -C tests MPARG=-Otarget quick-txt-diff results
fi

if [[ "$1" != "quick-txt-diff" && "$1" != "non-txt-diff" && "$1" != "misc" ]]; then
  # Daikon txt-diff tests
  echo ".travis-build.sh is running nonquick-txt-diff tests"
  make dyncomp-jdk
  make -C tests MPARG=-j1 nonquick-txt-diff results
fi

# There should be a separate job for Fjalar, or run it here.
if [[ "$1" != "quick-txt-diff" && "$1" != "nonquick-txt-diff" && "$1" != "misc" ]]; then
  # Daikon tests other than txt-diff
  echo ".travis-build.sh is running non-txt-diff tests"
  make -C tests non-txt-diff results
fi

if [[ "$1" != "quick-txt-diff" && "$1" != "nonquick-txt-diff" && "$1" != "non-txt-diff" ]]; then
  ## misc tests: miscellaneous tests that shouldn't depend on JDK version.
  ## (Maybe they don't even need the full ./.travis-build-without-test.sh .)
  echo ".travis-build.sh is running misc tests"

  # Code formatting
  make -C java check-format

  # Documentation
  make javadoc doc-all
fi
