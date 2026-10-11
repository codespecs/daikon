#!/bin/sh

# Runs a Kvasir command, writing its output to a file.  If the command fails,
# prints the output to standard error and exits with a nonzero status.
#
# Usage:
#   run-kvasir.sh [--expected-status N] OUTFILE COMMAND [ARGS...]
#     Writes the command's standard output and standard error to OUTFILE.
#   run-kvasir.sh [--expected-status N] --counts DTRACE_COUNT COUNTSFILE OUTFILE COMMAND [ARGS...]
#     Writes the command's standard error to OUTFILE.  The command's standard
#     output is a dtrace file; the DTRACE_COUNT script (dtrace-count.pl)
#     summarizes it into COUNTSFILE.
#
# The command fails if it cannot be run, if it is killed by a signal, or if its
# exit status differs from N, which defaults to 0.  A test sets N (via the
# EXPECTED_EXIT_STATUS Makefile variable) when its program exits with a nonzero
# status on its test inputs; tot_info, for example, exits with status 1.
# Valgrind and Kvasir exit with a nonzero status on fatal errors, so in every
# other test, any fatal error is a failure.  This script is the only place that
# this policy is implemented.
#
# Used by Makefile.common and run-many.pl in tests/kvasir-tests/, and by
# tests/dyncomp-tests/Makefile.common.

expected_status=0
dtrace_count=
counts=
while :; do
  case "$1" in
    --expected-status)
      expected_status="$2"
      shift 2
      ;;
    --counts)
      dtrace_count="$2"
      counts="$3"
      shift 3
      ;;
    *)
      break
      ;;
  esac
done
out="$1"
shift

count_status=0
if [ -n "$counts" ]; then
  # A pipeline's exit status is that of its last command, so the command's
  # exit status is passed out of the pipeline on file descriptor 3.
  status=$({ { "$@" 2> "$out" 3>&-; echo $? >&3; } | perl "$dtrace_count" > "$counts" 3>&-; } 3>&1)
  count_status=$?
  # $status is empty if the subshell was killed before it reported the status.
  [ -n "$status" ] || status=255
else
  "$@" > "$out" 2>&1
  status=$?
fi

# This check precedes the check of the command's exit status, because if
# $dtrace_count dies, the command is killed by SIGPIPE.
if [ "$count_status" -ne 0 ]; then
  echo "$dtrace_count failed with exit status $count_status (command exit status $status); command output follows:" >&2
  cat "$out" >&2
  exit "$count_status"
fi
if [ "$status" -ne "$expected_status" ]; then
  echo "Command failed with exit status $status (expected $expected_status); output follows:" >&2
  cat "$out" >&2
  if [ "$status" -eq 0 ]; then
    exit 1
  fi
  exit "$status"
fi
exit 0
