#!/usr/bin/env python3

"""Report malformed variable references in Makefiles.

Usage: check-makefile-dollar.py MAKEFILE ...

In a Makefile, `$FOO` means `${F}OO`, not `${FOO}`, and `($warning ...)` means
`(${w}arning ...)`, not `$(warning ...)`.  Make expands such text silently,
usually to something wrong.  This script reports `$` followed by two or more
identifier characters.  A literal `$` is written `$$` and is not reported.
Neither are full-line comments, except in recipe lines (which start with a tab),
because Make expands a recipe line before passing it to the shell even if the
line starts with `#`.  Single-letter references such as `$x` are not reported,
because they are legitimate for variables bound by `$(foreach x,...)`.

Exits with status 1 if any problem is found.
"""

import pathlib
import re
import sys

BAD_REFERENCE = re.compile(r"\$[A-Za-z_][A-Za-z0-9_]")


def check_file(filename: str) -> bool:
    """Print each malformed reference in the file.

    Returns:
        True if the file has no malformed references.
    """
    ok = True
    with pathlib.Path(filename).open(encoding="utf-8", errors="replace") as f:
        for lineno, line in enumerate(f, start=1):
            if not line.startswith("\t") and line.lstrip().startswith("#"):
                continue
            if BAD_REFERENCE.search(line.replace("$$", "")):
                print(f"{filename}:{lineno}: malformed variable reference: {line.rstrip()}")
                ok = False
    return ok


def main() -> int:
    """Check each Makefile named on the command line.

    Returns:
        The exit status: 0 if no problems were found, 1 otherwise.
    """
    ok = True
    for filename in sys.argv[1:]:
        ok = check_file(filename) and ok
    if not ok:
        print("Write `${FOO}` or `$(FOO)` for a Make variable, and `$$FOO` for a shell variable.")
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
