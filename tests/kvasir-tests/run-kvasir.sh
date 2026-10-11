#!/bin/sh

# Runs a Kvasir command, writing its output to a file.  If the command fails,
# prints the file and exits with the command's exit status.
#
# Usage:
#   run-kvasir.sh [--append] OUTFILE COMMAND [ARGS...]
#     Writes the command's standard output and standard error to OUTFILE.
#     With --append, appends to OUTFILE.
#   run-kvasir.sh --counts COUNTSFILE OUTFILE COMMAND [ARGS...]
#     Writes the command's standard error to OUTFILE.  The command's standard
#     output is a dtrace file; dtrace-count.pl summarizes it into COUNTSFILE.
#
# Used by tests/kvasir-tests/Makefile.common and tests/dyncomp-tests/Makefile.common.

append=false
counts=
case "$1" in
  --append)
    append=true
    shift
    ;;
  --counts)
    counts="$2"
    shift 2
    ;;
esac
out="$1"
shift

if [ -n "$counts" ]; then
  # A pipeline's exit status is that of its last command, so the command's
  # exit status is passed out of the pipeline on file descriptor 3.
  dtrace_count="$(dirname "$0")/../../scripts/dtrace-count.pl"
  status=$({ { "$@" 2> "$out" 3>&-; echo $? >&3; } | perl "$dtrace_count" > "$counts" 3>&-; } 3>&1)
elif $append; then
  "$@" >> "$out" 2>&1
  status=$?
else
  "$@" > "$out" 2>&1
  status=$?
fi

if [ "$status" -ne 0 ]; then
  echo "Command failed with exit status $status; contents of $out follow:"
  cat "$out"
fi
exit "$status"
