#!/usr/bin/python3

"""Converts a .decls file to a file organized by variable comparability sets at each program point.

This helps out with automating regression tests of DynComp.

Input: .decls file with comparability numbers

Output: A file which lists the comparability sets of all relevant
variables at each program point, alphabetically sorted and separated
by spaces.  All program points are also sorted by alphabetical
order. Output is written to stdout by default

Variables are in the same comparability set if their comparabilities
are identical.  For arrays, this is stricter than Daikon, which
compares the element and index comparabilities (as in "9[10]")
separately.  A variable whose comparability is negative and is not an
array is comparable to every variable; all such variables are listed on
the last line for the program point, which starts with "-1:".

Usage: ./decls2comp.py input.decls 'no-hashcodes' [optional]
Running this with the 'no-hashcodes' string as the 2nd arg results
in the tool ignoring all variables of rep. type 'hashcode' or
'hashcode[]', etc...

Prog pt name
All variable names in one comp set
All variable names in another comp set
...
<blank line>

Input:

input-language C/C++
decl-version 2.0
var-comparability none

ppt ..returnIntSum():::ENTER
 ppt-type enter
 variable a
  var-kind variable
  rep-type int
  dec-type int
  comparability 1
 variable b
  var-kind variable
  rep-type int
  dec-type int
  comparability 1
 variable c
  var-kind variable
  rep-type int
  dec-type int
  comparability 2
 variable d
  var-kind variable
  rep-type int
  dec-type int
  comparability -1

Output:

..returnIntSum():::ENTER
a b
c
-1: d
"""

import re
import sys
from pathlib import Path

VAR_START = "variable "
PPT_START = "ppt "
REP_START = "rep-type "
COMP_START = "comparability "

ignore_hashcodes = False

if len(sys.argv) < 2:
    print("Usage: decls2compy decls-file [no-hashcodes]")
    sys.exit()


if (len(sys.argv) == 3) and sys.argv[2] == "no-hashcodes":
    ignore_hashcodes = True

# If 'no-hashcodes' option is on, then ignore all variables whose
# rep. type is hashcode
hashcode_re = re.compile(r"hashcode.*")


# Break each program point declaration up into separate lists.
# Each program point declaration starts with a "ppt" record
# Key: program point name
# Value: list of all strings following program point
all_ppts = {}

temp_all_ppts = []  # Temporary before placing in all_ppts

with Path(sys.argv[1]).open() as f:
    for line in f:
        line = line.strip()

        if line[0:4] == "ppt ":
            temp_all_ppts.append([line[len(PPT_START) :]])  # Start a new list
            is_intermediate = 0
        elif line != "" and line[0] != "#":  # Don't add blank lines & comments
            if len(temp_all_ppts) > 0:
                temp_all_ppts[-1].append(line)  # Append line to the latest entry


# Init all_ppts from temp_all_ppts
for ppt_list in temp_all_ppts:
    # Allow duplicates by appending numeric indices onto program point
    # name
    index = 1

    ppt_name = ppt_list[0]

    # There is already an entry
    if ppt_list[0] in all_ppts:
        # Try appending numbers until there isn't an entry
        found = 1
        while found:
            ppt_name = ppt_list[0] + " (" + str(index) + ")"
            if ppt_name not in all_ppts:
                break
            index += 1

    all_ppts[ppt_name] = ppt_list[1:]

# Alphabetically sort the program points
sorted_ppt_keys = sorted(all_ppts.keys())

# Process each PPT
for ppt_name in sorted_ppt_keys:
    v = all_ppts[ppt_name]
    i = 0
    var2comp: dict[str, str] = {}  # Key: variable name, Value: comparability number

    # The comparability info for a variable at a program point is
    # prefixed by comparability

    cur_var = None
    hashcode_vars = set()  # Variables whose rep. type is a hashcode
    for line in v:
        stripped_line = line.strip()

        if stripped_line[0 : len(VAR_START)] == VAR_START:
            cur_var = stripped_line[len(VAR_START) :]
        elif stripped_line[0 : len(REP_START)] == REP_START:
            assert cur_var  # There should have been a variable entry before rep-type
            cur_rep = stripped_line[len(REP_START) :]
            if hashcode_re.match(cur_rep):
                hashcode_vars.add(cur_var)
        elif stripped_line[0 : len(COMP_START)] == COMP_START:
            assert (
                cur_var  # There should have been a variable entry before comparability
            )
            cur_comp = stripped_line[len(COMP_START) :]
            var2comp[cur_var] = cur_comp

    if ignore_hashcodes:
        for var in hashcode_vars:
            var2comp.pop(var, None)

    # Now we can do the real work of grouping variables together
    # in comparability sets based on their numbers
    groups = {}  # Key: comparability, Value: sorted list of variable names
    always_comparable = []  # Variables that are comparable to every variable
    for var_name in sorted(var2comp.keys()):
        comp = var2comp[var_name]
        if comp.startswith("-") and "[" not in comp:
            always_comparable.append(var_name)
        else:
            groups.setdefault(comp, []).append(var_name)

    print(ppt_name)
    # Each group is sorted, so this orders the groups by their first variable.
    for group in sorted(groups.values()):
        print(" ".join(group))
    if always_comparable:
        print("-1:", " ".join(always_comparable))
    print()
