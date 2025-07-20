#!/usr/bin/python3

"""Converts a .decls file to a file organized by variable comparability sets at each program point.

The input is a .decls file with comparability numbers.
This helps with automating regression tests of DynComp.

Input: .decls file with comparability numbers

Output: A file which lists the comparability sets of all relevant
variables at each program point, alphabetically sorted and separated
by spaces.  All program points are also sorted by alphabetical
order. Output is written to stdout by default

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

DECLARE
..add():::ENTER
a
int # isParam=true
int
1
b
int # isParam=true
int
1
c
int # isParam=true
int
2
d
int # isParam=true
int
-1

Output:

..add():::ENTER
a b
c
-1: d

Note: Lackwit produces comparability numbers for arrays in the
following format: '9[10]' - we are going to ignore what is between
the brackets so we will treat it as '9'.
"""

import re
import sys
from pathlib import Path

LWArrayRExp = re.compile(r"\[.\]")

ignore_hashcodes = False

if (len(sys.argv) == 3) and sys.argv[2] == "no-hashcodes":
    ignore_hashcodes = True

# If 'no-hashcodes' option is on, then ignore all variables whose
# rep. type is hashcode
hashcode_re = re.compile(r"hashcode.*")


# Break each program point declaration up into separate lists.
# Program points are separated by "DECLARE" statements
# Key: program point name
# Value: list of all strings following program point
all_ppts: dict[str, list[str]] = {}

temp_all_ppts: list[list[str]] = []  # Temporary before placing in all_ppts

is_intermediate = 0

with Path(sys.argv[1]).open() as f:
    for line in f:
        line = line.strip()

        if line == "DECLARE":
            temp_all_ppts.append([])  # Start a new list
            is_intermediate = 0
        elif line == "INTERMEDIATE DECLARE":
            temp_all_ppts.append([])  # Start a new list
            is_intermediate = 1
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
    var2comp = {}  # Key: variable name, Value: comparability number

    # All info. about variables at a program point come in sets of 4
    # lines. e.g.
    #
    # a
    # int # isParam=true
    # int
    # 1
    while i < len(v):
        cur_rep_type = v[i + 2]
        cur_comp = v[i + 3]

        if (not ignore_hashcodes) or (not hashcode_re.match(cur_rep_type)):
            is_array_match = LWArrayRExp.search(cur_comp)
            if is_array_match:
                var2comp[v[i]] = cur_comp[: is_array_match.start()]
            else:
                var2comp[v[i]] = cur_comp

        i += 4

    # Now we can do the real work of grouping variables together
    # in comparability sets based on their numbers
    sorted_vars = sorted(var2comp.keys())

    print(ppt_name)

    while len(sorted_vars) > 0:
        var_name = sorted_vars[0]

        #        if var2comp[var_name] == '-1': # Remember that everything is a string
        #            print '-1:', var_name,
        #        else:
        print(var_name, end=" ")

        comp_num = var2comp[var_name]

        if comp_num:
            del var2comp[var_name]

            sorted_vars = sorted(var2comp.keys())

            for other_var in sorted_vars:
                if var2comp[other_var] == comp_num:
                    print(other_var, end=" ")
                    del var2comp[other_var]
        print()

        # Update sorted_vars after deleting the appropriate entries
        # from var2comp
        sorted_vars = sorted(var2comp.keys())

    print()
