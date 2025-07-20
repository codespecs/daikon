#!/usr/bin/python3

"""Convert Dfec-generated .decls data with Lackwit comparability information into Kvasir form.

Then, we don't have to run Dfec at all.

Takes a .decls file that Dfec produced (with Lackwit comparability),
a .decls file that Kvasir produced (with DynComp comparability), and
outputs 4 .decls file that contain the intersection of Dfec and
Kvasir program points and variables in a format that is compatible
with Kvasir:

kvasir-with-lackwit.decls: comparability numbers from Lackwit
kvasir-with-dyncomp.decls: comparability numbers from DynComp
kvasir-with-declared-types.decls: comparability numbers inferred from declared types
kvasir-no-comp.decls: no comparability numbers (this is the same thing that
                      --var-list-file=intersection.vars would generate)

and outputs a variable list file (intersection.vars) so that Kvasir
can be run and trace data can be collected only for those variables.
"""

import re
import sys
from enum import Enum
from pathlib import Path

"""Takes in 7 filenames as params.  The first 2 files are inputs and the latter 5 are outputs.

Usage:
./Lackwit2DynComp.py dfec-produced.decls kvasir-produced.decls \
 kvasir-with-lackwit.decls kvasir-with-dyncomp.decls \
 kvasir-with-declared-types.decls kvasir-no-comp.decls intersection.vars

If everything goes correctly, kvasir-with-lackwit.decls and
kvasir-with-dyncomp.decls should only differ in their comparability
numbers.  Also, when running Kvasir with the
--var-list-file=intersection.vars option, both of these .decls files
should be compatible with the resulting .dtrace file so that Daikon
can run on both sets of output.
"""

# Process command-line args:
with Path(sys.argv[1]).open() as dfec_f:
    dfec_all_lines = [line.strip() for line in dfec_f]

with Path(sys.argv[2]).open() as kvasir_f:
    kvasir_all_lines = [line.strip() for line in kvasir_f]

output_lackwit_decls_f = Path(sys.argv[3]).open("w")  # noqa: SIM115
output_dyn_comp_decls_f = Path(sys.argv[4]).open("w")  # noqa: SIM115

output_dec_types_decls_f = Path(sys.argv[5]).open("w")  # noqa: SIM115
output_no_comp_decls_f = Path(sys.argv[6]).open("w")  # noqa: SIM115

output_vars_f = Path(sys.argv[7]).open("w")  # noqa: SIM115


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


# Run the state machine to build up a map (dfec_ppt_map)
# where the keys are program point names (stripped using strip_dfec_ppt_name)
# and the values are maps where the keys are variable names and the
# values are comparability numbers
dfec_ppt_map: dict[tuple[str, str], dict[str, str]] = {}

cur_var_map: dict[str, str] = {}  # The current variable map, which is a value in dfec_ppt_map.
cur_var_name = "DUMMY VAR NAME"

for line in dfec_all_lines:
    if my_state == DeclState.Uninit:
        # The program point name always follows the line called "DECLARE".
        if line == "DECLARE":
            my_state = DeclState.PptName

    elif my_state == DeclState.PptName:
        cur_var_map = {}
        dfec_ppt_map[strip_dfec_ppt_name(line)] = cur_var_map
        my_state = DeclState.VarName

    elif my_state == DeclState.VarName:
        if line == "DECLARE":
            my_state = DeclState.PptName
        elif line == "":
            my_state = DeclState.Uninit
        else:
            cur_var_name = convert_dfec_var_name(line)
            my_state = DeclState.DecType

    elif my_state == DeclState.DecType:
        my_state = DeclState.RepType

    elif my_state == DeclState.RepType:
        my_state = DeclState.CompNum

    elif my_state == DeclState.CompNum:
        # strip off array index comparability numbers
        # e.g. '217[337]' should become '217'
        cur_var_map[cur_var_name] = strip_comp_number(line)

        # Assume we are gonna read another variable.
        # When we actually read the subsequent line,
        # we'll branch according to whether it's a real
        # variable or another thing
        my_state = DeclState.VarName


# Key: program point name
# Value: A list of 5-element sub-lists
#          Each sub-list is:
#            (variable name, dec_type, rep_type, kvasir_comp_num, declaredTypeCompNum)
# declaredTypeCompNum is calculated later in the next step by assigning
# each variable of the same declared type at a particular program point
# the SAME number
kvasir_ppt_map: dict[str, list[list[str]]] = {}

# A list of the same strings which are keys to kvasir_ppt_map
# This is desirable because we want to output the program points
# in the same order as they were read in
kvasir_ppt_names = []

my_state = DeclState.Uninit

cur_var_list: list[list[str]] = []
for line in kvasir_all_lines:
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


# Now both dfec_ppt_map and kvasir_ppt_map should be initialized.  We want
# to now iterate through kvasir_ppt_map, translate program
# point/variable names to the names that will appear in dfec_ppt_map,
# look up the appropriate entries, and add them to result_map, which
# contains all program points and variables that are present in BOTH
# the Dfec and Kvasir-generated .decls files.

# Remember that our goal is to output a Kvasir-compatible .decls file
# with the variables and comparability numbers gathered from the
# Dfec-generated .decls file to output_lackwit_decls_f and one with
# the numbers gathered from DynComp to output_dyn_comp_decls_f.


result_map = {}

for ppt, KvasirVarList in kvasir_ppt_map.items():
    stripped = strip_kvasir_ppt_name(ppt)
    if stripped in dfec_ppt_map:
        DfecVarMap = dfec_ppt_map[stripped]

        #        print "KvasirVarList:"
        #        print KvasirVarList
        #        print "DfecVarMap:"
        #        print DfecVarMap
        #        print
        #        print

        cur_result_var_list = []

        #        print ppt

        # Now iterate through the Kvasir variable list:
        for entry in KvasirVarList:
            var = entry[0]
            dec_type = entry[1]
            rep_type = entry[2]
            kvasir_comp_num = entry[3]
            dec_type_comp_num = entry[4]

            # If rep_type == "java.lang.String", then look
            # up the entry for the variable + '[]' because
            # Dfec has separate variables for the pointer
            # and content of strings
            var_to_lookup = var
            if rep_type == "java.lang.String":
                var_to_lookup += "[]"

            var_to_lookup = convert_kvasir_var_name(var_to_lookup)

            if var_to_lookup in DfecVarMap:
                # Throw the comparability number on the end
                # of the entry for that variable

                # Make this a tuple 'cause it should be immutable:
                # Each entry should be the following:
                #  (variable name, dec. type, rep. type,
                #           (Lackwit comp. num, Kvasir comp. num, dec. type comp num)
                cur_result_var_list.append(
                    (
                        var,
                        dec_type,
                        rep_type,
                        (DfecVarMap[var_to_lookup], kvasir_comp_num, dec_type_comp_num),
                    )
                )
                if DfecVarMap[var_to_lookup] == "":
                    print("EMPTY COMP. NUMBER!", var, var_to_lookup)

                # Only for debugging
        #                DfecVarMap.pop(var_to_lookup)

        result_map[ppt] = cur_result_var_list

# This is important to see how much of the intersection between
# Dfec and Kvasir variables that we've successfully picked up:

#        print "Leftovers", DfecVarMap.keys()
#        print "# vars in Dfec:  ", len(DfecVarMap.keys())
#        print "# vars in Kvasir:", len(KvasirVarList)
#        print "# vars in result:", len(cur_result_var_list)
#        print

# Output the resulting .decls file and the var list file:

# Globals section ... let's just take the first program point and use
# the global vars in that one for the globals section.  This makes the
# assumption that the same global variables appear everywhere at all
# program points ... will have to investigate further later ...

output_vars_f.write("----SECTION----\n")
output_vars_f.write("globals\n")

example_var_list = result_map[kvasir_ppt_names[0]]

for var_entry in example_var_list:
    if "/" in var_entry[0]:  # only print out globals and file-statics
        output_vars_f.write(var_entry[0])
        output_vars_f.write("\n")

output_vars_f.write("\n")


# Filter kvasir_ppt_names to remove any program points that are NOT
# in the Dfec-generated .decls file:
kvasir_ppt_names = [
    name for name in kvasir_ppt_names if (strip_kvasir_ppt_name(name) in dfec_ppt_map)
]


#

output_no_comp_decls_f.write("VarComparability\nnone\n\n")


all_decls_files = [
    output_lackwit_decls_f,
    output_dyn_comp_decls_f,
    output_dec_types_decls_f,
    output_no_comp_decls_f,
]

# Output the various .decls files
# (Read these names from kvasir_ppt_names to preserve ordering)
for ppt in kvasir_ppt_names:
    for f in all_decls_files:
        f.write("DECLARE\n")
        f.write(ppt)
        f.write("\n")

    # Only print the :::EXIT program point to the var list file
    # because then we can grab the return value 'return'

    # Remember that we need to print program points in the form of
    # '..main()' and NOT '..main():::ENTER' and '..main():::EXIT0'
    is_exit = False

    fnname, enter_or_exit = ppt.split(":::")
    if enter_or_exit[:4] == "EXIT":
        is_exit = True

    if is_exit:
        output_vars_f.write("----SECTION----\n")
        output_vars_f.write(fnname)
        output_vars_f.write("\n")

    for var_entry in result_map[ppt]:
        for f in all_decls_files:
            # Variable name
            f.write(var_entry[0])
            f.write("\n")

            # Declared type
            f.write(var_entry[1])
            f.write("\n")

            # Representation type
            f.write(var_entry[2])
            f.write("\n")

        # Comparability number - this is where the action is!
        # For Lackwit, we choose the car of the tuple,
        output_lackwit_decls_f.write(var_entry[3][0])
        # For DynComp, we choose the cadr
        output_dyn_comp_decls_f.write(var_entry[3][1])
        # For dec. type, we choose the caddr
        output_dec_types_decls_f.write(str(var_entry[3][2]))
        # For no comparability, simply print out '22'
        output_no_comp_decls_f.write("22")

        for f in all_decls_files:
            f.write("\n")

        # Don't print out globals or file-static vars in the
        # var-list-file for individual program points
        if is_exit and "/" not in var_entry[0]:
            output_vars_f.write(var_entry[0])
            output_vars_f.write("\n")

    # Newline separating neighboring program points
    for f in all_decls_files:
        f.write("\n")

    if is_exit:
        output_vars_f.write("\n")


# print '# Dfec ppts:', len(dfec_ppt_map.keys())
# print '# Kvasir ppts:', len(kvasir_ppt_map.keys())
# print '# Common ppts:', len(result_map.keys())


for f in all_decls_files:
    f.close()

output_vars_f.close()
