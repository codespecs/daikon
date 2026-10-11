#!/bin/sh

# Runs a Kvasir command, writing its output to a file.  If the command fails,
# prints the output to standard error and exits with a nonzero status.
#
# Usage:
#   run-kvasir.sh [--expected-status N] OUTFILE COMMAND [ARGS...]
#     Writes the command's standard output and standard error to OUTFILE.
#
# The command fails if it cannot be run, if it is killed by a signal, or if its
# exit status differs from N, which defaults to 0.  A test sets N (via the
# EXPECTED_EXIT_STATUS Makefile variable) when its program exits with a nonzero
# status on its test inputs; tot_info, for example, exits with status 1.
# Valgrind and Kvasir exit with a nonzero status on fatal errors, so in every
# other test, any fatal error is a failure.  In a test whose N is nonzero, a
# fatal error that exits with status N is not detected here; it shows up only as
# a difference in the test's output.  This script is the only place that this
# policy is implemented.
#
# ARGS (and PROGRAM_ARGS or a TESTS_SCRIPT line, which become ARGS) must not
# contain an output redirection or a pipe, because it would apply to this
# script rather than to COMMAND.  An input redirection ("<") works, because
# COMMAND inherits standard input.
#
# Used by Makefile.common and run-many.pl in tests/kvasir-tests/, and by
# tests/dyncomp-tests/Makefile.common.

expected_status=0
if [ "$1" = --expected-status ]; then
  expected_status="$2"
  shift 2
fi
out="$1"
shift

"$@" > "$out" 2>&1
status=$?

if [ "$status" -ne "$expected_status" ]; then
  echo "Command failed with exit status $status (expected $expected_status); output follows:" >&2
  cat "$out" >&2
  if [ "$status" -eq 0 ]; then
    exit 1
  fi
  exit "$status"
fi
exit 0
