#!/usr/bin/python

# The point of this script is to convert Dfec-generated .decls data
# with Lackwit comparability info. into a form that Kvasir can
# understand so that we don't have to run Dfec at all.

# Takes a .decls file that Dfec produced (with Lackwit comparability),
# a .decls file that Kvasir produced (with DynComp comparability), and
# outputs 4 .decls file that contain the intersection of Dfec and
# Kvasir program points and variables in a format that is compatible
# with Kvasir:
#
# kvasir-with-lackwit.decls: comparability numbers from Lackwit
# kvasir-with-dyncomp.decls: comparability numbers from DynComp
# kvasir-with-declared-types.decls: comparability numbers inferred from declared types
# kvasir-no-comp.decls: no comparability numbers (this is the same thing that
#                       --var-list-file=intersection.vars would generate)
#
# and outputs a variable list file (intersection.vars) so that Kvasir
# can be run and trace data can be collected only for those variables.

# Created on 2005-08-09 by Philip Guo

# Usage: (Takes in 7 filenames as params.  The first 2 files are inputs
#         and the latter 5 are outputs.)
# ./Lackwit2DynComp.py dfec-produced.decls kvasir-produced.decls kvasir-with-lackwit.decls kvasir-with-dyncomp.decls kvasir-with-declared-types.decls kvasir-no-comp.decls intersection.vars

# If everything goes correctly, kvasir-with-lackwit.decls and
# kvasir-with-dyncomp.decls should only differ in their comparability
# numbers.  Also, when running Kvasir with the
# --var-list-file=intersection.vars option, both of these .decls files
# should be compatible with the resulting .dtrace file so that Daikon
# can run on both sets of output.

import sys

# Process command-line args:
dfecF = open(sys.argv[1], 'r')
DfecAllLines = [line.strip() for line in dfecF.readlines()]
dfecF.close()

kvasirF = open(sys.argv[2], 'r')
KvasirAllLines = [line.strip() for line in kvasirF.readlines()]
kvasirF.close()

outputLackwitDeclsF = open(sys.argv[3], 'w')
outputDynCompDeclsF = open(sys.argv[4], 'w')

outputDecTypesDeclsF = open(sys.argv[5], 'w')
outputNoCompDeclsF = open(sys.argv[6], 'w')

outputVarsF = open(sys.argv[7], 'w')



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

# Ok, we are going to just strip off everything before
# the '/', if there is one, because Dfec does not print
# out the function name for function-static variables
# e.g. 'flex_c@epsclosure/did_stk_init' becomes '/did_stk_init'
def ConvertKvasirVarName(var):
    if var[0] == '/':
        return var
    elif '/' in var:
        return '/' + var.split('/')[1]
    else:
        return var

# Kvasir does not support comparability for array indices
# so strip those off.
# e.g. '104[105]' becomes '104'
def StripCompNumber(comp_num):
    if '[' in comp_num:
        return comp_num[:comp_num.find('[')]
    else:
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

# Strips the extraneous stuff off of Dfec's names and returns
# a 2-tuple of ppt name and either 'ENTER' or 'EXITxxx'

# Input:  'std.ccladd(int;int;)void:::ENTER'
# Output: ('ccladd', 'ENTER')

def StripDfecPptName(ppt):
    fnname, enterOrExit = ppt.split(':::')
    if fnname[:4] == 'std.':
        fnname = fnname[4:]
    # Find the first '(' and end the function name there
    fnname = fnname[:fnname.index('(')]

    # Just return 'ENTER' or 'EXIT' with no numbers
    # (This means that we can only keep one exit ppt)
    if enterOrExit[1] == 'N':
        enterOrExit = 'ENTER'
    else:
        enterOrExit = 'EXIT'

    # Return a pair of the function name and 'ENTER' or 'EXITxxx'
    return (fnname, enterOrExit)


def StripKvasirPptName(ppt):
    fnname, enterOrExit = ppt.split(':::')

    # For globals, grab everything from '..' to '('
    # e.g. for '..main():::ENTER'
    # we want 'main'
    if fnname[:2] == '..':
        fnname = fnname[2:fnname.find('(')]

    # For file-static names, we need to take everything between
    # the LAST period ('.') and the '('
    # e.g. for 'flex.c.yy_push_state():::EXIT0',
    # we want 'yy_push_state'
    else:
        fnname = fnname[fnname.rfind('.')+1:fnname.find('(')]

    # Just return 'ENTER' or 'EXIT' with no numbers
    # (This means that we can only keep one exit ppt)
    if enterOrExit[1] == 'N':
        enterOrExit = 'ENTER'
    else:
        enterOrExit = 'EXIT'

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
# where the keys are program point names (stripped using StripDfecPptName)
# and the values are maps where the keys are variable names and the
# values are comparability numbers
DfecPptMap = {}

curVarMap = 0 # The current variable map
curVarName = ""

for line in DfecAllLines:

    if myState == State.Uninit:

        # The program point name always follows the
        # line called "DECLARE"
        if line == "DECLARE":
            myState = State.PptName

    elif myState == State.PptName:
        curVarMap = {}
        DfecPptMap[StripDfecPptName(line)] = curVarMap
        myState = State.VarName

    elif myState == State.VarName:
        if line == "DECLARE":
            myState = State.PptName
        elif line == "":
            myState = State.Uninit
        else:
            curVarName = ConvertDfecVarName(line)
            myState = State.DecType

    elif myState == State.DecType:
        myState = State.RepType

    elif myState == State.RepType:
        myState = State.CompNum

    elif myState == State.CompNum:
        # strip off array index comparability numbers
        # e.g. '217[337]' should become '217'
        curVarMap[curVarName] = StripCompNumber(line)

        # Assume we are gonna read another variable.
        # When we actually read the subsequent line,
        # we'll branch according to whether it's a real
        # variable or another thing
        myState = State.VarName


# Key: program point name
# Value: A list of 5-element sub-lists
#          Each sub-list is:
#            (variable name, decType, repType, kvasirCompNum, declaredTypeCompNum)
# declaredTypeCompNum is calculated later in the next step by assigning
# each variable of the same declared type at a particular program point
# the SAME number
KvasirPptMap = {}

# A list of the same strings which are keys to KvasirPptMap
# This is desirable because we want to output the program points
# in the same order as they were read in
KvasirPptNames = []

myState = State.Uninit

for line in KvasirAllLines:

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


# Strips all comments after #
# space-delimited token:
# Input:  int # isParam=true
# Output: int
def StripComments(comp_num):
    return comp_num.split('#')[0].strip()


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


# Now both DfecPptMap and KvasirPptMap should be initialized.  We want
# to now iterate through KvasirPptMap, translate program
# point/variable names to the names that will appear in DfecPptMap,
# look up the appropriate entries, and add them to ResultMap, which
# contains all program points and variables that are present in BOTH
# the Dfec and Kvasir-generated .decls files.

# Remember that our goal is to output a Kvasir-compatible .decls file
# with the variables and comparability numbers gathered from the
# Dfec-generated .decls file to outputLackwitDeclsF and one with
# the numbers gathered from DynComp to outputDynCompDeclsF.


ResultMap = {}

for ppt in KvasirPptMap:
    stripped = StripKvasirPptName(ppt)
    if stripped in DfecPptMap:
        KvasirVarList = KvasirPptMap[ppt]
        DfecVarMap = DfecPptMap[stripped]

#        print "KvasirVarList:"
#        print KvasirVarList
#        print "DfecVarMap:"
#        print DfecVarMap
#        print
#        print

        curResultVarList = []

#        print ppt

        # Now iterate through the Kvasir variable list:
        for entry in KvasirVarList:
            var = entry[0]
            decType = entry[1]
            repType = entry[2]
            kvasirCompNum = entry[3]
            decTypeCompNum = entry[4]

            # If repType == "java.lang.String", then look
            # up the entry for the variable + '[]' because
            # Dfec has separate variables for the pointer
            # and content of strings
            varToLookup = var
            if repType == "java.lang.String":
                varToLookup += '[]'

            varToLookup = ConvertKvasirVarName(varToLookup)

            if varToLookup in DfecVarMap:
                # Throw the comparability number on the end
                # of the entry for that variable

                # Make this a tuple 'cause it should be immutable:
                # Each entry should be the following:
                #  (variable name, dec. type, rep. type,
                #           (Lackwit comp. num, Kvasir comp. num, dec. type comp num)
                curResultVarList.append((var, decType, repType,
                                         (DfecVarMap[varToLookup],
                                          kvasirCompNum,
                                          decTypeCompNum)))
                if DfecVarMap[varToLookup] == "":
                    print "EMPTY COMP. NUMBER!", var, varToLookup

                # Only for debugging
#                DfecVarMap.pop(varToLookup)

        ResultMap[ppt] = curResultVarList

# This is important to see how much of the intersection between
# Dfec and Kvasir variables that we've successfully picked up:

#        print "Leftovers", DfecVarMap.keys()
#        print "# vars in Dfec:  ", len(DfecVarMap.keys())
#        print "# vars in Kvasir:", len(KvasirVarList)
#        print "# vars in result:", len(curResultVarList)
#        print

# Output the resulting .decls file and the var list file:

# Globals section ... let's just take the first program point and use
# the global vars in that one for the globals section.  This makes the
# assumption that the same global variables appear everywhere at all
# program points ... will have to investigate further later ...

outputVarsF.write("----SECTION----\n")
outputVarsF.write("globals\n")

exampleVarList = ResultMap[KvasirPptNames[0]]

for varEntry in exampleVarList:
    if '/' in varEntry[0]: # only print out globals and file-statics
        outputVarsF.write(varEntry[0])
        outputVarsF.write("\n")

outputVarsF.write("\n")


# Filter KvasirPptNames to remove any program points that are NOT
# in the Dfec-generated .decls file:
KvasirPptNames = [name for
                  name in KvasirPptNames
                  if (StripKvasirPptName(name) in DfecPptMap)]


#

outputNoCompDeclsF.write("VarComparability\nnone\n\n");


allDeclsFiles = [outputLackwitDeclsF,
                 outputDynCompDeclsF,
                 outputDecTypesDeclsF,
                 outputNoCompDeclsF]

# Output the various .decls files
# (Read these names from KvasirPptNames to preserve ordering)
for ppt in KvasirPptNames:

    for f in allDeclsFiles:
        f.write("DECLARE\n")
        f.write(ppt)
        f.write("\n")

    # Only print the :::EXIT program point to the var list file
    # because then we can grab the return value 'return'

    # Remember that we need to print program points in the form of
    # '..main()' and NOT '..main():::ENTER' and '..main():::EXIT0'
    isExit = False

    fnname, enterOrExit = ppt.split(':::')
    if enterOrExit[:4] == "EXIT":
        isExit = True

    if isExit:
        outputVarsF.write("----SECTION----\n")
        outputVarsF.write(fnname)
        outputVarsF.write("\n")

    for varEntry in ResultMap[ppt]:

        for f in allDeclsFiles:
            # Variable name
            f.write(varEntry[0])
            f.write("\n")

            # Declared type
            f.write(varEntry[1])
            f.write("\n")

            # Representation type
            f.write(varEntry[2])
            f.write("\n")

        # Comparability number - this is where the action is!
        # For Lackwit, we choose the car of the tuple,
        outputLackwitDeclsF.write(varEntry[3][0])
        # For DynComp, we choose the cadr
        outputDynCompDeclsF.write(varEntry[3][1])
        # For dec. type, we choose the caddr
        outputDecTypesDeclsF.write(str(varEntry[3][2]))
        # For no comparability, simply print out '22'
        outputNoCompDeclsF.write("22")

        for f in allDeclsFiles:
            f.write("\n")

        # Don't print out globals or file-static vars in the
        # var-list-file for individual program points
        if isExit and not ('/' in varEntry[0]):
            outputVarsF.write(varEntry[0])
            outputVarsF.write("\n")

    # Newline separating neighboring program points
    for f in allDeclsFiles:
        f.write("\n")

    if isExit:
        outputVarsF.write("\n")


#print '# Dfec ppts:', len(DfecPptMap.keys())
#print '# Kvasir ppts:', len(KvasirPptMap.keys())
#print '# Common ppts:', len(ResultMap.keys())



for f in allDeclsFiles:
    f.close()

outputVarsF.close()
