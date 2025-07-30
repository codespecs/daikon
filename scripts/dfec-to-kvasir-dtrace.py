#!/usr/bin/python3

"""Outputs a Kvasir-compatible .dtrace file from a Dfec .dtrace file.

Outputs a Kvasir-compatible .dtrace file from the Dfec .dtrace file
based on the variables and ordering in the Kvasir .decls file.

Usage: ./dfec-to-kvasir-dtrace.py <kvasir .decls file> <dfec .dtrace file>

Cannibalized from dfec-to-kvasir.py so most of these comments
will make absolutely no sense!!!
"""

import re
import sys
from enum import Enum
from pathlib import Path

# Process command-line args:
with Path(sys.argv[1]).open() as kvasir_decls_f:
    kvasir_decls_all_lines = [line.strip() for line in kvasir_decls_f]


DfecGlobalRE = re.compile(r"^::")

# Dfec and Kvasir variable differences:

# Globals are prefixed with a '::' in Dfec
# and with a '/' in Kvasir

# Dfec doesn't append the filename in front of
# file-static global variables like Kvasir does

# Dfec also doesn't print out function-static variables
# while Kvasir does

# Dfec derives two Daikon variables for strings
# one for the pointer and the other for the contents,
# while Kvasir only has one for the contents.

# Dfec uses the arrow notation for struct names while
# Kvasir uses the bracket notation (unless disambiguation
# information is provided)


def convert_dfec_var_name(var: str) -> str:
    """Convert variable var's name from Dfec conventions to Kvasir conventions.

    Args:
        var: the variable's name in Dfec form.

    Returns:
       the variable's name in Kvasir form.
    """
    global_converted = DfecGlobalRE.sub("/", var)
    return global_converted.replace("->", "[].")


def convert_kvasir_var_name(var: str) -> str:
    """Strip off everything before the '/', if there is one.

    Dfec does not print out the function name for function-static variables,
    e.g., 'flex_c@epsclosure/did_stk_init' becomes '/did_stk_init'.

    Args:
        var: a string

    Returns:
        The string starting after the '/', or the whole string.
    """
    if var[0] == "/":
        return var
    if "/" in var:
        return "/" + var.split("/", maxsplit=1)[1]
    return var


def strip_comp_number(comp_num: str) -> str:
    """Kvasir does not support comparability for array indices so strip those off.

    e.g. '104[105]' becomes '104'.

    Args:
        comp_num: a comparability, possibly in array form

    Returns:
        the comparibility without the array part
    """
    if "[" in comp_num:
        return comp_num[: comp_num.find("[")]
    return comp_num


# Dfec and Kvasir program point name differences:

# Global program point names in Dfec are prefixed by 'std.'
# while they are prefixed by '..' in Kvasir

# Dfec uses 'EXIT1' while Kvasir uses 'EXIT0' for the exit
# so we should probably simply search for 'EXIT' without
# regard to the number behind it.  However, Dfec can have
# more than 1 exit while Kvasir can only have 1.  Hmmm,
# what do we do about that?
# (Right now, we just keep 'EXIT' without the number before
#  putting it in the hashtable.  Thus, only one EXIT ppt
#  is kept for each function ... I'm just not sure which
#  one, though, but that's ok)

# Dfec's names for C functions have crap in between the parens
# while Kvasir's doesn't.  Let's just not worry about what's
# in the parens since C doesn't have overloading.  We just want
# to strip off the canonical function name.


def strip_dfec_ppt_name(ppt: str) -> tuple[str, str]:
    """Strip the extraneous stuff off of Dfec's names and split into parts.

    Input:  'std.ccladd(int;int;)void:::ENTER'
    Output: ('ccladd', 'ENTER')

    Args:
        ppt: a dfec program point name

    Returns:
        a 2-tuple of cleaned ppt name and either 'ENTER' or 'EXITxxx'
    """
    fnname, enter_or_exit = ppt.split(":::")
    if fnname[:4] == "std.":
        fnname = fnname[4:]
    # Find the first '(' and end the function name there
    fnname = fnname[: fnname.index("(")]

    # Just return 'ENTER' or 'EXIT' with no numbers
    # (This means that we can only keep one exit ppt)
    if enter_or_exit[1] == "N":
        enter_or_exit = "ENTER"
    else:
        enter_or_exit = "EXIT"

    # Return a pair of the function name and 'ENTER' or 'EXITxxx'
    return (fnname, enter_or_exit)


def strip_kvasir_ppt_name(ppt: str) -> tuple[str, str]:
    """Strip the extraneous stuff off of Kvasir's names and split into parts.

    Args:
        ppt: a Kvasir program point name

    Returns:
        a 2-tuple of cleaned ppt name and either 'ENTER' or 'EXITxxx'
    """
    fnname, enter_or_exit = ppt.split(":::")

    # For globals, grab everything from '..' to '('
    # e.g. for '..main():::ENTER'
    # we want 'main'
    if fnname[:2] == "..":
        fnname = fnname[2 : fnname.find("(")]

    # For file-static names, we need to take everything between
    # the LAST period ('.') and the '('
    # e.g. for 'flex.c.yy_push_state():::EXIT0',
    # we want 'yy_push_state'
    else:
        fnname = fnname[fnname.rfind(".") + 1 : fnname.find("(")]

    # Just return 'ENTER' or 'EXIT' with no numbers
    # (This means that we can only keep one exit ppt)
    if enter_or_exit[1] == "N":
        enter_or_exit = "ENTER"
    else:
        enter_or_exit = "EXIT"

    # Return a pair of the function name and 'ENTER' or 'EXITxxx'
    return (fnname, enter_or_exit)


# .decls States:
# About to read in ...
# 0 = Nothing important
# 1 = program point name
# 2 = variable name
# 3 = variable declared type
# 4 = variable rep. type
# 5 = variable comparability number - VERY important
class DeclsState(Enum):
    """The parse state: what is about to be read."""

    Uninit = 0
    PptName = 1
    VarName = 2
    DecType = 3
    RepType = 4
    CompNum = 5


cur_var_map = 0  # The current variable map
cur_var_name = ""

# Key: program point name (stripped using strip_kvasir_ppt_name)
# Value: A list of 2 elts: car: full ppt name
#                          cdr: A list of lists,
#                               where each sub-list is: [variable name, rep. type]
kvasir_ppt_map = {}

# The current parse state.
my_state = DeclsState.Uninit

cur_var_list: list[list[str]] = []
for line in kvasir_decls_all_lines:
    if my_state == DeclsState.Uninit:
        # The program point name always follows the line called "DECLARE".
        if line == "DECLARE":
            my_state = DeclsState.PptName

    elif my_state == DeclsState.PptName:
        cur_var_list = []
        kvasir_ppt_map[strip_kvasir_ppt_name(line)] = [line, cur_var_list]
        my_state = DeclsState.VarName

    elif my_state == DeclsState.VarName:
        if line == "DECLARE":
            my_state = DeclsState.PptName
        elif line == "":
            my_state = DeclsState.Uninit
        else:
            cur_var_list.append([])
            cur_var_list[-1].append(line)
            my_state = DeclsState.DecType

    elif my_state == DeclsState.DecType:
        #        cur_var_list[-1].append(line)
        my_state = DeclsState.RepType

    elif my_state == DeclsState.RepType:
        cur_var_list[-1].append(line)
        my_state = DeclsState.CompNum

    elif my_state == DeclsState.CompNum:
        #        cur_var_list[-1].append(line)

        # Assume we are gonna read another variable.
        # When we actually read the subsequent line,
        # we'll branch according to whether it's a real
        # variable or another thing
        my_state = DeclsState.VarName


def process_ppt(ppt_name: str, var_info: dict[str, list[str]]) -> None:
    """Print out the Kvasir version of the name.

    Does nothing if this ppt is not in the Kvasir .decls file.
    """
    stripped = strip_dfec_ppt_name(ppt_name)

    if stripped in kvasir_ppt_map:
        print(kvasir_ppt_map[stripped][0])

        # Iterate thru all variables in .decls file (to preserve
        # order) and print ut the corresponding entries in the .dtrace
        # file:
        var_list = kvasir_ppt_map[stripped][1]
        for var_entry in var_list:
            var_name = var_entry[0]
            rep_type = var_entry[1]

            # Try to look up var_name in the var_info dict., remembering
            # the differences between Kvasir and Dfec names:

            # If rep_type == "java.lang.String", then look
            # up the entry for the variable + '[]' because
            # Dfec has separate variables for the pointer
            # and content of strings
            var_to_lookup = var_name
            if "java.lang.String" in rep_type:
                var_to_lookup += "[]"

            var_to_lookup = convert_kvasir_var_name(var_to_lookup)

            if var_to_lookup in var_info:
                stuff = var_info[var_to_lookup]
                print(var_name)

                if (
                    rep_type[-2:] == "[]"
                    and stuff[0][0] != "["
                    and stuff[0] != "uninit"
                    and stuff[0] != "nonsensical"
                ):
                    print("[", stuff[0], "]")
                else:
                    print(stuff[0])

                print(stuff[1])
            # Total cop out ... print blank
            else:
                print(var_name)

                print("uninit")

                print("2")

        # Blank line ends this ppt
        print()


# .dtrace States:
# About to read in ...
# 0 = program point name or junk (uninit)
# 1 = variable name
# 2 = value
# 3 = modbit
# 4 = Ignore nonce
class DtraceState(Enum):
    """The parse state: what is about to be read."""

    Uninit = 0
    VarName = 1
    Value = 2
    Modbit = 3
    IgnoreNonce = 4


dt_state = DtraceState.Uninit

cur_ppt_name = "DUMMY PPT NAME"
# For current program point only:
# Key: Variable name (after running through convert_dfec_var_name())
# Value: list of 2 elts: [value, modbit]
# TODO: Change value from list to tuple?
var_info: dict[str, list[str]] = {}

cur_var_name = "DUMMY VAR NAME"
cur_var_info: list[str] = []

# This is shorthand for xreadlines so that it doesn't have to read the
# entire file in at once, which is crucial for huge examples:
with Path(sys.argv[2]).open() as lines:
    for line in lines:
        line = line.strip()

        if dt_state == DtraceState.Uninit:
            # Match program point name with ':::ENTER' or ':::EXIT'
            if ":::ENTER" in line or ":::EXIT" in line:
                cur_ppt_name = line
                dt_state = DtraceState.VarName

        elif dt_state == DtraceState.IgnoreNonce:
            dt_state = DtraceState.VarName

        elif dt_state == DtraceState.VarName:
            if line == "this_invocation_nonce":
                dt_state = DtraceState.IgnoreNonce
            elif line == "":
                # We've reached the end of a ppt entry!!!
                # So process it
                process_ppt(cur_ppt_name, var_info)

                cur_ppt_name = "DUMMY PPT NAME"
                var_info = {}

                dt_state = DtraceState.Uninit
            else:
                cur_var_name = line
                cur_var_info = []
                dt_state = DtraceState.Value

        elif dt_state == DtraceState.Value:
            cur_var_info.append(line)
            dt_state = DtraceState.Modbit

        elif dt_state == DtraceState.Modbit:
            cur_var_info.append(line)
            var_info[convert_dfec_var_name(cur_var_name)] = cur_var_info
            cur_var_name = "DUMMY VAR NAME"
            cur_var_info = []
            dt_state = DtraceState.VarName


# result_map = {}

# for ppt in kvasir_ppt_map:
##    stripped = strip_kvasir_ppt_name(ppt)
##    if stripped in dfec_ppt_map:
##        KvasirVarList = kvasir_ppt_map[ppt]
##        DfecVarMap = dfec_ppt_map[stripped]

###        print "KvasirVarList:"
###        print KvasirVarList
###        print "DfecVarMap:"
###        print DfecVarMap
###        print
###        print

##        cur_result_var_list = []

###        print ppt

##        # Now iterate through the Kvasir variable list:
##        for entry in KvasirVarList:
##            var = entry[0]
##            dec_type = entry[1]
##            rep_type = entry[2]
##            kvasir_comp_num = entry[3]
##            dec_type_comp_num = entry[4]

##            # If rep_type == "java.lang.String", then look
##            # up the entry for the variable + '[]' because
##            # Dfec has separate variables for the pointer
##            # and content of strings
##            var_to_lookup = var
##            if rep_type == "java.lang.String":
##                var_to_lookup += '[]'

##            var_to_lookup = convert_kvasir_var_name(var_to_lookup)

##            if var_to_lookup in DfecVarMap:
##                # Throw the comparability number on the end
##                # of the entry for that variable

##                # Make this a tuple 'cause it should be immutable:
##                # Each entry should be the following:
##                #  (variable name, dec. type, rep. type,
##                #           (Lackwit comp. num, Kvasir comp. num, dec. type comp num)
##                cur_result_var_list.append((var, dec_type, rep_type,
##                                         (DfecVarMap[var_to_lookup],
##                                          kvasir_comp_num,
##                                          dec_type_comp_num)))
##                if DfecVarMap[var_to_lookup] == "":
##                    print "EMPTY COMP. NUMBER!", var, var_to_lookup

##                # Only for debugging
###                DfecVarMap.pop(var_to_lookup)

##        result_map[ppt] = cur_result_var_list

### This is important to see how much of the intersection between
### Dfec and Kvasir variables that we've successfully picked up:

###        print "Leftovers", DfecVarMap.keys()
###        print "# vars in Dfec:  ", len(DfecVarMap.keys())
###        print "# vars in Kvasir:", len(KvasirVarList)
###        print "# vars in result:", len(cur_result_var_list)
###        print

### Output the resulting .decls file and the var list file:

### Globals section ... let's just take the first program point and use
### the global vars in that one for the globals section.  This makes the
### assumption that the same global variables appear everywhere at all
### program points ... will have to investigate further later ...

# output_vars_f.write("----SECTION----\n")
# output_vars_f.write("globals\n")

# example_var_list = result_map[kvasir_ppt_names[0]]

# for var_entry in example_var_list:
##    if '/' in var_entry[0]: # only print out globals and file-statics
##        output_vars_f.write(var_entry[0])
##        output_vars_f.write("\n")

# output_vars_f.write("\n")


### Filter kvasir_ppt_names to remove any program points that are NOT
### in the Dfec-generated .decls file:
# kvasir_ppt_names = [name for
##                  name in kvasir_ppt_names
##                  if (strip_kvasir_ppt_name(name) in dfec_ppt_map)]


###

# output_no_comp_decls_f.write("VarComparability\nnone\n\n");


# all_decls_files = [output_lackwit_decls_f,
##                 output_dyn_comp_decls_f,
##                 output_dec_types_decls_f,
##                 output_no_comp_decls_f]

### Output the various .decls files
### (Read these names from kvasir_ppt_names to preserve ordering)
# for ppt in kvasir_ppt_names:

##    for f in all_decls_files:
##        f.write("DECLARE\n")
##        f.write(ppt)
##        f.write("\n")

##    # Only print the :::EXIT program point to the var list file
##    # because then we can grab the return value 'return'

##    # Remember that we need to print program points in the form of
##    # '..main()' and NOT '..main():::ENTER' and '..main():::EXIT0'
##    is_exit = False

##    fnname, enter_or_exit = ppt.split(':::')
##    if enter_or_exit[:4] == "EXIT":
##        is_exit = True

##    if is_exit:
##        output_vars_f.write("----SECTION----\n")
##        output_vars_f.write(fnname)
##        output_vars_f.write("\n")

##    for var_entry in result_map[ppt]:

##        for f in all_decls_files:
##            # Variable name
##            f.write(var_entry[0])
##            f.write("\n")

##            # Declared type
##            f.write(var_entry[1])
##            f.write("\n")

##            # Representation type
##            f.write(var_entry[2])
##            f.write("\n")

##        # Comparability number - this is where the action is!
##        # For Lackwit, we choose the car of the tuple,
##        output_lackwit_decls_f.write(var_entry[3][0])
##        # For DynComp, we choose the cadr
##        output_dyn_comp_decls_f.write(var_entry[3][1])
##        # For dec. type, we choose the caddr
##        output_dec_types_decls_f.write(str(var_entry[3][2]))
##        # For no comparability, simply print out '22'
##        output_no_comp_decls_f.write("22")

##        for f in all_decls_files:
##            f.write("\n")

##        # Don't print out globals or file-static vars in the
##        # var-list-file for individual program points
##        if is_exit and not ('/' in var_entry[0]):
##            output_vars_f.write(var_entry[0])
##            output_vars_f.write("\n")

##    # Newline separating neighboring program points
##    for f in all_decls_files:
##        f.write("\n")

##    if is_exit:
##        output_vars_f.write("\n")


# print '# Dfec ppts:', len(dfec_ppt_map.keys())
# print '# Kvasir ppts:', len(kvasir_ppt_map.keys())
# print '# Common ppts:', len(result_map.keys())


# for f in all_decls_files:
#     f.close()

# output_vars_f.close()
