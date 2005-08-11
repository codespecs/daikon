#!/usr/bin/python

# Converts a .decls file with comparability numbers to a file which is
# organized by variable comparability sets at each program point.
# This helps out with automating regression tests of DynComp.

# Created on 2005-05-13 by Philip J. Guo
# MIT CSAIL Program Analysis Group

# Input: .decls file with comparability numbers

# Output: A file which lists the comparability sets of all relevant
# variables at each program point, alphabetically sorted and separated
# by spaces.  All program points are also sorted by alphabetical
# order. Output is written to stdout by default

# Usage: ./decls2comp.py input.decls 'no-hashcodes' [optional]
# Running this with the 'no-hashcodes' string as the 2nd arg results
# in the tool ignoring all variables of rep. type 'hashcode' or
# 'hashcode[]', etc...

# Prog pt name
# All variable names in one comp set
# All variable names in another comp set
# ...
# <blank line>

# Input:

# DECLARE
# ..add():::ENTER
# a
# int # isParam=true
# int
# 1
# b
# int # isParam=true
# int
# 1
# c
# int # isParam=true
# int
# 2
# d
# int # isParam=true
# int
# -1

# Output:

# ..add():::ENTER
# a b
# c
# -1: d

# Prints out a '-1: ' prefix in front of the special comparability set
# with a number of -1

import sys

# Note: Lackwit produces comparability numbers for arrays in the
# following format: '9[10]' - we are going to ignore what is between
# the brackets so we will treat it as '9'
import re
LWArrayRExp = re.compile('\[.\]')

ignoreHashcodes = False

if ((len(sys.argv) == 3) and
    sys.argv[2] == "no-hashcodes"):
    ignoreHashcodes = True

# If 'no-hashcodes' option is on, then ignore all variables whose
# rep. type is hashcode
hashcodeRE = re.compile('hashcode.*')


f = open(sys.argv[1], 'r')
allLines = [line.strip() for line in f.readlines()]

# Skip comparability declaration, if any
if allLines[0] == "VarComparability":
    allLines = allLines[3:]

# Break each program point declaration up into separate lists.
# Program points are separated by "DECLARE" statements
# Key: program point name
# Value: list of all strings following program point
allPpts = {}

tempAllPpts = [] # Temporary before placing in allPpts

for line in allLines:
    if line == "DECLARE":
        tempAllPpts.append([]) # Start a new list
    elif line != "" and line[0] != "#":   # Don't add blank lines or comments
        tempAllPpts[-1].append(line) # Append line to the last entry

# Init allPpts from tempAllPpts
for pptList in tempAllPpts:
    allPpts[pptList[0]] = pptList[1:]

# Alphabetically sort the program points
sortedPptKeys = allPpts.keys()
sortedPptKeys.sort()

# Process each PPT
for pptName in sortedPptKeys:
    v = allPpts[pptName]
    i = 0
    var2comp = {} # Key: variable name, Value: comparability number
    
    # All info. about variables at a program point come in sets of 4
    # lines. e.g.
    #
    # a
    # int # isParam=true
    # int
    # 1
    while i < len(v):
        curRepType = v[i+2]
        curComp = v[i+3]

        if ((not ignoreHashcodes) or
            (not hashcodeRE.match(curRepType))):
            isArrayMatch = LWArrayRExp.search(curComp)
            if isArrayMatch:
                var2comp[v[i]] = curComp[:isArrayMatch.start()]
            else:
                var2comp[v[i]] = curComp
            
        i += 4

    # Now we can do the real work of grouping variables together
    # in comparability sets based on their numbers
    sortedVars = var2comp.keys()
    sortedVars.sort()

    print pptName

    while len(sortedVars) > 0:
        varName = sortedVars[0]

        if var2comp[varName] == '-1': # Remember that everything is a string
            print '-1:', varName,
        else:
            print varName,

        compNum = var2comp[varName]

        if compNum:
            del var2comp[varName]

            sortedVars = var2comp.keys()
            sortedVars.sort()
            
            for otherVar in sortedVars:
                if var2comp[otherVar] == compNum:
                    print otherVar,
                    del var2comp[otherVar]
        print

        # Update sortedVars after deleting the appropriate entries
        # from var2comp
        sortedVars = var2comp.keys()
        sortedVars.sort()
        
    print
