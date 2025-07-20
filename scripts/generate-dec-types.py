#!/usr/bin/python3

"""Generates a .decls file with declared type comparability numbers.

Usage: ./generate-dec-types.py <decls-file>
"""

# Warning: This was hacked together by copy/paste from
# dfec-to-kvasir.py so some of the comments may make no sense at all

import sys
from enum import Enum
from pathlib import Path

# Process command-line args:
with Path(sys.argv[1]).open() as decls_f:
    all_lines = [line.strip() for line in decls_f]


def strip_comp_number(comp_num: str) -> str:
    """Kvasir does not support comparability for array indices, so strip those off.

    e.g. '104[105]' becomes '104'.

    Args:
        comp_num: a comparability, possibly in array form

    Returns:
        the comparibility without the array part
    """
    if "[" in comp_num:
        return comp_num[: comp_num.find("[")]
    return comp_num


def strip_comments(comp_num: str) -> str:
    """Strip all comments after "#".

    Example:
    # space-delimited token:
    Input:  int # isParam=true
    Output: int

    Args:
        comp_num: a string

    Returns:
        the string with trailing comments stripped
    """
    return comp_num.split("#", maxsplit=1)[0].strip()


# States:
# About to read in ...
# 0 = Nothing important
# 1 = program point name
# 2 = variable name
# 3 = variable declared type
# 4 = variable rep. type
# 5 = variable comparability number - VERY important
class DeclState(Enum):
    """The parse state: what is about to be read."""

    Uninit = 0
    PptName = 1
    VarName = 2
    DecType = 3
    RepType = 4
    CompNum = 5


# The current parse state.
my_state = DeclState.Uninit


# Key: program point name
# Value: A list of 5-element sub-lists
#          Each sub-list is:
#            (variable name, dec_type, rep_type, declaredTypeCompNum)
# declaredTypeCompNum is calculated later in the next step by assigning
# each variable of the same declared type at a particular program point
# the SAME number
kvasir_ppt_map = {}

# A list of the same strings which are keys to kvasir_ppt_map
# This is desirable because we want to output the program points
# in the same order as they were read in
kvasir_ppt_names = []

my_state = DeclState.Uninit

cur_var_list: list[list[str]] = []
for line in all_lines:
    if my_state == DeclState.Uninit:
        # The program point name always follows the
        # line called "DECLARE"
        if line == "DECLARE":
            my_state = DeclState.PptName

    elif my_state == DeclState.PptName:
        cur_var_list = []
        # Remember to add an entry to both the list and the map
        kvasir_ppt_names.append(line)
        kvasir_ppt_map[line] = cur_var_list
        my_state = DeclState.VarName

    elif my_state == DeclState.VarName:
        if line == "DECLARE":
            my_state = DeclState.PptName
        elif line == "":
            my_state = DeclState.Uninit
        else:
            cur_var_list.append([])
            cur_var_list[-1].append(line)
            my_state = DeclState.DecType

    elif my_state == DeclState.DecType:
        cur_var_list[-1].append(line)
        my_state = DeclState.RepType

    elif my_state == DeclState.RepType:
        cur_var_list[-1].append(line)
        my_state = DeclState.CompNum

    elif my_state == DeclState.CompNum:
        cur_var_list[-1].append(line)

        # Assume we are gonna read another variable.
        # When we actually read the subsequent line,
        # we'll branch according to whether it's a real
        # variable or another thing
        my_state = DeclState.VarName


# Now we are going to initialize the declaredTypeCompNum of each entry
# within kvasir_ppt_map.  All variables with identical declared type
# strings will have the same comparability number at each program
# point.
for cur_var_list in kvasir_ppt_map.values():
    cur_comp_num = 1  # Start at 1 and monotonically increase

    # Key: declared type; Value: comp. num associated with that type
    dec_types_map: dict[str, int] = {}

    for elt in cur_var_list:
        cur_dec_type = strip_comments(elt[1])
        if cur_dec_type in dec_types_map:
            elt.append(str(dec_types_map[cur_dec_type]))  # Use the stored comp. num
        else:
            elt.append(str(cur_comp_num))  # Use a fresh new comp. num
            dec_types_map[cur_dec_type] = cur_comp_num  # and add the entry to the map
            cur_comp_num += 1  # Don't forget to increment this!


# Output the various .decls files
# (Read these names from kvasir_ppt_names to preserve ordering)
for ppt in kvasir_ppt_names:
    print("DECLARE")
    print(ppt)

    for var_entry in kvasir_ppt_map[ppt]:
        # Variable name
        print(var_entry[0])

        # Declared type
        print(var_entry[1])

        # Representation type
        print(var_entry[2])

        # Comp. num (based on declared types only):
        print(str(var_entry[4]))

    # Newline separating neighboring program points
    print()
