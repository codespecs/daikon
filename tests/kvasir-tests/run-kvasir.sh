#!/bin/sh

# Runs a Kvasir command, writing its output to a file.  If the command fails,
# prints the output to standard error and exits with a nonzero status.
#
# Usage:
#   run-kvasir.sh OUTFILE COMMAND [ARGS...]
#     Writes the command's standard output and standard error to OUTFILE.
#   run-kvasir.sh --counts DTRACE_COUNT COUNTSFILE OUTFILE COMMAND [ARGS...]
#     Writes the command's standard error to OUTFILE.  The command's standard
#     output is a dtrace file; the DTRACE_COUNT script (dtrace-count.pl)
#     summarizes it into COUNTSFILE.
#
# The command fails if it cannot be run, if it is killed by a signal, or if it
# exits with status greater than 1.  Exit status 1 alone is not a failure,
# because some programs (such as tot_info) exit with status 1 on their test
# inputs.  But Valgrind and Kvasir also exit with status 1 on fatal errors, so
# exit status 1 is a failure if the output contains one of their fatal error
# messages.  This script is the only place that this policy is implemented.
#
# Used by Makefile.common and run-many.pl in tests/kvasir-tests/, and by
# tests/dyncomp-tests/Makefile.common.

dtrace_count=
counts=
if [ "$1" = --counts ]; then
  dtrace_count="$2"
  counts="$3"
  shift 3
fi
out="$1"
shift

count_status=0
if [ -n "$counts" ]; then
  # A pipeline's exit status is that of its last command, so the command's
  # exit status is passed out of the pipeline on file descriptor 3.
  status=$({ { "$@" 2> "$out" 3>&-; echo $? >&3; } | perl "$dtrace_count" > "$counts" 3>&-; } 3>&1)
  count_status=$?
else
  "$@" > "$out" 2>&1
  status=$?
fi

# Each regular expression matches a fatal error message from Valgrind, from
# Kvasir, or from the kvasir-dtrace script, including the context that
# distinguishes the message from ordinary program output.
if [ "$status" -eq 1 ] \
  && perl -0777 -ne 'exit(/^valgrind: |^Valgrind: FATAL|^Error: .*\n\n?Exiting\.$|^Out of memory allocating .*\nTerminating program\.$|^dwarf\.c: out of memory|^(dwarf\.c: Error: )?Attempt to .* with an excessive number of elements|^Kvasir is not (installed|built)/m ? 0 : 1)' "$out"; then
  echo "Kvasir failed with a fatal error; output follows:" >&2
  cat "$out" >&2
  exit 1
fi
# This check precedes the check of the command's exit status, because if
# $dtrace_count dies, the command is killed by SIGPIPE.
if [ "$count_status" -ne 0 ]; then
  echo "$dtrace_count failed with exit status $count_status (command exit status $status); command output follows:" >&2
  cat "$out" >&2
  exit "$count_status"
fi
if [ "$status" -gt 1 ]; then
  echo "Command failed with exit status $status; output follows:" >&2
  cat "$out" >&2
  exit "$status"
fi
exit 0
