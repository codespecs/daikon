#!/usr/bin/python

# Converts a .decls file with comparability numbers to a file which is
# organized by variable comparability sets at each program point.
# This helps out with automating regression tests of DynComp.

# Input: .decls file with comparability numbers

# Output: A file which lists the comparability sets of all relevant
# variables at each program point, alphabetically sorted and separated
# by spaces.  All program points are also sorted by alphabetical
# order.

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

# Output:

# ..add():::ENTER
# a b
# c

# Created on 2005-05-13 by Philip J. Guo

import sys

f = open(sys.argv[1], 'r')
allLines = [line.strip() for line in f.readlines()]

# Break each program point declaration up into separate lists.
# Program points are separated by "DECLARE" statements
# Key: program point name
# Value: list of all strings following program point
allPpts = {}

tempAllPpts = [] # Temporary before placing in allPpts

for line in allLines:
    if line == "DECLARE":
        tempAllPpts.append([]) # Start a new list
    elif line != "":       # Don't add blank lines
        tempAllPpts[-1].append(line) # Append line to the last entry

# Init allPpts from tempAllPpts
for pptList in tempAllPpts:
    allPpts[pptList[0]] = pptList[1:]

# Alphabetically sort the program points
sortedKeys = allPpts.keys()
sortedKeys.sort()

# Process each PPT
for k in sortedKeys:
    v = allPpts[k]
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
        var2comp[v[i]] = v[i+3]
        i += 4
