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
# other test, any fatal error is a failure.  When N is nonzero, the command
# also fails unless its output contains Valgrind's "ERROR SUMMARY:" line.
# Valgrind prints that line only when the traced program exits, not when
# Valgrind or Kvasir exits on a fatal error (via VG_(exit)), so a fatal error
# whose exit status happens to be N is still a failure.  This script is the
# only place that this policy is implemented.
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

# A string comparison, so that a non-numeric expected status is a failure
# rather than an error that "if" would treat as success.
if [ "$status" = "$expected_status" ]; then
  if [ "$status" = 0 ] || grep -q 'ERROR SUMMARY:' "$out"; then
    exit 0
  fi
  echo "Command exited with the expected status $status, but Valgrind did not report that the program exited; output follows:" >&2
  cat "$out" >&2
  exit 1
fi
echo "Command failed with exit status $status (expected $expected_status); output follows:" >&2
cat "$out" >&2
if [ "$status" -eq 0 ]; then
  exit 1
fi
exit "$status"
