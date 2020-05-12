#!/usr/bin/python3

# Usage: ./generate-dec-types.py <decls-file>

# Generates a .decls file with declared type comparability numbers

# Warning: This was hacked together by copy/paste from
# dfec-to-kvasir.py so some of the comments may make no sense at all

import sys

# Process command-line args:
declsF = open(sys.argv[1], 'r')
allLines = [line.strip() for line in declsF.readlines()]
declsF.close()

import re

def StripCompNumber(comp_num):
    if '[' in comp_num:
        return comp_num[:comp_num.find('[')]
    else:
        return comp_num

# Strips all comments after #
# space-delimited token:
# Input:  int # isParam=true
# Output: int
def StripComments(comp_num):
    return comp_num.split('#')[0].strip()

# States:
# About to read in ...
# 0 = Nothing important
# 1 = program point name
# 2 = variable name
# 3 = variable declared type
# 4 = variable rep. type
# 5 = variable comparability number - VERY important
class State:
    Uninit, PptName, VarName, DecType, RepType, CompNum = list(range(6))

myState = State.Uninit


# Key: program point name
# Value: A list of 5-element sub-lists
#          Each sub-list is:
#            (variable name, decType, repType, declaredTypeCompNum)
# declaredTypeCompNum is calculated later in the next step by assigning
# each variable of the same declared type at a particular program point
# the SAME number
KvasirPptMap = {}

# A list of the same strings which are keys to KvasirPptMap
# This is desirable because we want to output the program points
# in the same order as they were read in
KvasirPptNames = []

myState = State.Uninit

for line in allLines:

    if myState == State.Uninit:

        # The program point name always follows the
        # line called "DECLARE"
        if line == "DECLARE":
            myState = State.PptName

    elif myState == State.PptName:
        curVarList = []
        # Remember to add an entry to both the list and the map
        KvasirPptNames.append(line)
        KvasirPptMap[line] = curVarList
        myState = State.VarName

    elif myState == State.VarName:
        if line == "DECLARE":
            myState = State.PptName
        elif line == "":
            myState = State.Uninit
        else:
            curVarList.append([])
            curVarList[-1].append(line)
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


# Now we are going to initialize the declaredTypeCompNum of each entry
# within KvasirPptMap.  All variables with identical declared type
# strings will have the same comparability number at each program
# point.
for ppt in KvasirPptMap:
    curCompNum = 1 # Start at 1 and monotonically increase

    # Key: declared type; Value: comp. num associated with that type
    decTypesMap = {}

    curVarList = KvasirPptMap[ppt]

    for elt in curVarList:
        curDecType = StripComments(elt[1])
        if curDecType in decTypesMap:
            elt.append(decTypesMap[curDecType]) # Use the stored comp. num
        else:
            elt.append(curCompNum) # Use a fresh new comp. num
            decTypesMap[curDecType] = curCompNum # and add the entry to the map
            curCompNum += 1 # Don't forget to increment this!


# Output the various .decls files
# (Read these names from KvasirPptNames to preserve ordering)
for ppt in KvasirPptNames:

    print("DECLARE")
    print(ppt)

    for varEntry in KvasirPptMap[ppt]:

        # Variable name
        print(varEntry[0])

        # Declared type
        print(varEntry[1])

        # Representation type
        print(varEntry[2])

        # Comp. num (based on declared types only):
        print(str(varEntry[4]))

    # Newline separating neighboring program points
    print()
