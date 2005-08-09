#!/usr/bin/python

# Lackwit to DynComp (by Philip Guo)
# Tries to transform Lackwit's output into something that DynComp likes

# Usage:
# ./Lackwit2DynComp.py dfec-produced.decls kvasir-produced.decls

import sys

dfecF = open(sys.argv[1], 'r')
DfecAllLines = [line.strip() for line in dfecF.readlines()]

kvasirF = open(sys.argv[2], 'r')
KvasirAllLines = [line.strip() for line in kvasirF.readlines()]

import re
DfecGlobalRE = re.compile('^::')


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

# Converts variable var's name from Dfec conventions
# to Kvasir conventions and returns it as the result
def ConvertDfecVarName(var):
    globalConverted = DfecGlobalRE.sub('/', var)
    return globalConverted.replace('->', '[].')


# Dfec and Kvasir program point name differences:

# Global program point names in Dfec are prefixed by 'std.'
# while they are prefixed by '..' in Kvasir

# Dfec uses 'EXIT1' while Kvasir uses 'EXIT0' for the exit
# so we should probably simply search for 'EXIT' without
# regard to the number behind it.  However, Dfec can have
# more than 1 exit while Kvasir can only have 1.  Hmmm,
# what do we do about that?

# Dfec's names for C functions have crap in between the parens
# while Kvasir's doesn't.  Let's just not worry about what's
# in the parens since C doesn't have overloading.  We just want
# to strip off the canonical function name.

# Strips the extraneous stuff off of Dfec's names and returns
# a 2-tuple of ppt name and either 'ENTER' or 'EXITxxx'

# Input:  'std.ccladd(int;int;)void:::ENTER'
# Output: ('ccladd', 'ENTER')

EnterRE = re.compile('ENTER$')
ExitRE = re.compile('EXIT.*$') # Remember that you can have multiple exits

def StripDfecPptName(ppt):
    fnname, enterOrExit = ppt.split(':::')
    if fnname[:4] == 'std.':
        fnname = fnname[4:]
    # Find the first '(' and end the function name there
    fnname = fnname[:fnname.index('(')]

    # Return a pair of the function name and 'ENTER' or 'EXITxxx'
    return (fnname, enterOrExit)

def StripKvasirPptName(ppt):
    fnname, enterOrExit = ppt.split(':::')

    # For globals, grab everything from '..' to '('
    # e.g. for '..main():::ENTER'
    # we want 'main'
    if fnname[:4] == '..':
        fnname = fnname[4:fnname.index('(')]

    # For file-static names, we need to take everything between
    # the LAST period ('.') and the '('
    # e.g. for 'flex.c.yy_push_state():::EXIT0',
    # we want 'yy_push_state'
    
    # Find the first '(' and end the function name there

    # Return a pair of the function name and 'ENTER' or 'EXITxxx'
    return (fnname, enterOrExit)
   

# States:
# About to read in ...
# 0 = Nothing important
# 1 = program point name
# 2 = variable name
# 3 = variable declared type
# 4 = variable rep. type
# 5 = variable comparability number - VERY important
class State:
    Uninit, PptName, VarName, DecType, RepType, CompNum = range(6)

myState = State.Uninit


# Run the state machine to build up a map (DfecPptMap)
# where the keys are program point names
# and the values are lists of 4-element list of variable entries.
#   The 4 entries in each variable list are:
#     VarName, DecType, RepType, and CompNum
DfecPptMap = {}

curVarList = 0 # The current variable list that we are operating on

for line in DfecAllLines:

    if myState == State.Uninit:

        # The program point name always follows the
        # line called "DECLARE"
        if line == "DECLARE":
            myState = State.PptName
            
    elif myState == State.PptName:
        curVarList = []
        DfecPptMap[StripDfecPptName(line)] = curVarList
        myState = State.VarName
        
    elif myState == State.VarName:
        if line == "DECLARE":
            myState = State.PptName
        elif line == "":
            myState = State.Uninit
        else:
            curVarList.append([])
            curVarList[-1].append(ConvertDfecVarName(line))
            myState = State.DecType
        
    elif myState == State.DecType:
        curVarList[-1].append(line)
        myState = State.RepType
        
    elif myState == State.RepType:
        curVarList[-1].append(line)
        myState = State.CompNum
        
    elif myState == State.CompNum:
        curVarList[-1].append(line)

        # Assume we are gonna read another variable.
        # When we actually read the subsequent line,
        # we'll branch according to whether it's a real
        # variable or another thing
        myState = State.VarName


KvasirPptMap = {}

myState = State.Uninit

for line in KvasirAllLines:
  
    if myState == State.Uninit:

        # The program point name always follows the
        # line called "DECLARE"
        if line == "DECLARE":
            myState = State.PptName
            
    elif myState == State.PptName:
        curVarList = []
        KvasirPptMap[line] = curVarList
        myState = State.VarName
        
    elif myState == State.VarName:
        if line == "DECLARE":
            myState = State.PptName
        elif line == "":
            myState = State.Uninit
        else:
            curVarList.append([])
            curVarList[-1].append(ConvertDfecVarName(line))
            myState = State.DecType
        
    elif myState == State.DecType:
        curVarList[-1].append(line)
        myState = State.RepType
        
    elif myState == State.RepType:
        curVarList[-1].append(line)
        myState = State.CompNum
        
    elif myState == State.CompNum:
        curVarList[-1].append(line)

        # Assume we are gonna read another variable.
        # When we actually read the subsequent line,
        # we'll branch according to whether it's a real
        # variable or another thing
        myState = State.VarName



# Now both DfecPptMap and KvasirPptMap should be initialized.  We want
# to now iterate through KvasirPptMap, translate program
# point/variable names to the names that will appear in DfecPptMap,
# look up the appropriate entries, and add them to ResultMap, which
# contains all program points and variables that are present in BOTH
# the Dfec and Kvasir-generated .decls files.

# Remember that our goal is to output a Kvasir-compatible .decls file
# with the variables and comparability numbers gathered from the
# Dfec-generated .decls file.


ResultMap = {}

for key in DfecPptMap:
    print key
