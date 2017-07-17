#!/usr/bin/python

# Usage: ./dfec-to-kvasir-dtrace.py <kvasir .decls file> <dfec .dtrace file>

# Outputs a Kvasir-compatible .dtrace file from the Dfec .dtrace file
# based on the variables and ordering in the Kvasir .decls file

# Cannibalized from dfec-to-kvasir.py so most of these comments
# will make absolutely no sense!!!
import sys

# Process command-line args:
kvasirDeclsF = open(sys.argv[1], 'r')
kvasirDeclsAllLines = [line.strip() for line in kvasirDeclsF.readlines()]
kvasirDeclsF.close()



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


# .decls States:
# About to read in ...
# 0 = Nothing important
# 1 = program point name
# 2 = variable name
# 3 = variable declared type
# 4 = variable rep. type
# 5 = variable comparability number - VERY important
class DeclsState:
    Uninit, PptName, VarName, DecType, RepType, CompNum = range(6)

curVarMap = 0 # The current variable map
curVarName = ""

# Key: program point name (stripped using StripKvasirPptName)
# Value: A list of 2 elts: car: full ppt name
#                          cdr: A list of lists,
#                               where each sub-list is: [variable name, rep. type]
KvasirPptMap = {}

myState = DeclsState.Uninit

for line in kvasirDeclsAllLines:

    if myState == DeclsState.Uninit:

        # The program point name always follows the
        # line called "DECLARE"
        if line == "DECLARE":
            myState = DeclsState.PptName

    elif myState == DeclsState.PptName:
        curVarList = []
        KvasirPptMap[StripKvasirPptName(line)] = [line, curVarList]
        myState = DeclsState.VarName

    elif myState == DeclsState.VarName:
        if line == "DECLARE":
            myState = DeclsState.PptName
        elif line == "":
            myState = DeclsState.Uninit
        else:
            curVarList.append([])
            curVarList[-1].append(line)
            myState = DeclsState.DecType

    elif myState == DeclsState.DecType:
#        curVarList[-1].append(line)
        myState = DeclsState.RepType

    elif myState == DeclsState.RepType:
        curVarList[-1].append(line)
        myState = DeclsState.CompNum

    elif myState == DeclsState.CompNum:
#        curVarList[-1].append(line)

        # Assume we are gonna read another variable.
        # When we actually read the subsequent line,
        # we'll branch according to whether it's a real
        # variable or another thing
        myState = DeclsState.VarName


def processPpt(pptName, varInfo):
    # First check if this ppt is in the Kvasir .decls file
    # by munging its name, and if so, print out the Kvasir
    # version of the name
    stripped = StripDfecPptName(pptName)

    if stripped in KvasirPptMap:
        print KvasirPptMap[stripped][0]

        # Iterate thru all variables in .decls file (to preserve
        # order) and print ut the corresponding entries in the .dtrace
        # file:
        varList = KvasirPptMap[stripped][1]
        for varEntry in varList:
            varName = varEntry[0]
            repType = varEntry[1]

            # Try to look up varName in the varInfo dict., remembering
            # the differences between Kvasir and Dfec names:

            # If repType == "java.lang.String", then look
            # up the entry for the variable + '[]' because
            # Dfec has separate variables for the pointer
            # and content of strings
            varToLookup = varName
            if "java.lang.String" in repType:
                varToLookup += '[]'

            varToLookup = ConvertKvasirVarName(varToLookup)

            if varToLookup in varInfo:
                stuff = varInfo[varToLookup]
                print varName

                if repType[-2:] == '[]' and stuff[0][0] != '[' and stuff[0] != "uninit" and stuff[0] != "nonsensical":
                    print '[', stuff[0], ']'
                else:
                    print stuff[0]

                print stuff[1]
            # Total cop out ... print blank
            else:
                print varName

                print "uninit"

                print "2"

        # Blank line ends this ppt
        print

# .dtrace States:
# About to read in ...
# 0 = program point name or junk (uninit)
# 1 = variable name
# 2 = value
# 3 = modbit
# 4 = Ignore nonce
class DtraceState:
    Uninit, VarName, Value, Modbit, IgnoreNonce = range(5)

dState = DtraceState.Uninit

curPptName = ""
# For current program point only:
# Key: Variable name (after running through ConvertDfecVarName())
# Value: list of 2 elts: [value, modbit]
VarInfo = {}

curVarName = None
curVarInfo = None

# This is shorthand for xreadlines so that it doesn't have to read the
# entire file in at once, which is crucial for huge examples:
for line in open(sys.argv[2], 'r'):
    line = line.strip()

    if dState == DtraceState.Uninit:
        # Match program point name with ':::ENTER' or ':::EXIT'
        if ':::ENTER' in line or ':::EXIT' in line:
            curPptName = line
            dState = DtraceState.VarName

    elif dState == DtraceState.IgnoreNonce:
        dState = DtraceState.VarName

    elif dState == DtraceState.VarName:
        if line == "this_invocation_nonce":
            dState = DtraceState.IgnoreNonce
        elif line == "":
            # We've reached the end of a ppt entry!!!
            # So process it
            processPpt(curPptName, VarInfo)

            curPptName = None
            VarInfo = {}

            dState = DtraceState.Uninit
        else:
            curVarName = line
            curVarInfo = []
            dState = DtraceState.Value

    elif dState == DtraceState.Value:
        curVarInfo.append(line)
        dState = DtraceState.Modbit

    elif dState == DtraceState.Modbit:
        curVarInfo.append(line)
        VarInfo[ConvertDfecVarName(curVarName)] = curVarInfo
        curVarName = None
        curVarInfo = None
        dState = DtraceState.VarName


##ResultMap = {}

##for ppt in KvasirPptMap:
##    stripped = StripKvasirPptName(ppt)
##    if stripped in DfecPptMap:
##        KvasirVarList = KvasirPptMap[ppt]
##        DfecVarMap = DfecPptMap[stripped]

###        print "KvasirVarList:"
###        print KvasirVarList
###        print "DfecVarMap:"
###        print DfecVarMap
###        print
###        print

##        curResultVarList = []

###        print ppt

##        # Now iterate through the Kvasir variable list:
##        for entry in KvasirVarList:
##            var = entry[0]
##            decType = entry[1]
##            repType = entry[2]
##            kvasirCompNum = entry[3]
##            decTypeCompNum = entry[4]

##            # If repType == "java.lang.String", then look
##            # up the entry for the variable + '[]' because
##            # Dfec has separate variables for the pointer
##            # and content of strings
##            varToLookup = var
##            if repType == "java.lang.String":
##                varToLookup += '[]'

##            varToLookup = ConvertKvasirVarName(varToLookup)

##            if varToLookup in DfecVarMap:
##                # Throw the comparability number on the end
##                # of the entry for that variable

##                # Make this a tuple 'cause it should be immutable:
##                # Each entry should be the following:
##                #  (variable name, dec. type, rep. type,
##                #           (Lackwit comp. num, Kvasir comp. num, dec. type comp num)
##                curResultVarList.append((var, decType, repType,
##                                         (DfecVarMap[varToLookup],
##                                          kvasirCompNum,
##                                          decTypeCompNum)))
##                if DfecVarMap[varToLookup] == "":
##                    print "EMPTY COMP. NUMBER!", var, varToLookup

##                # Only for debugging
###                DfecVarMap.pop(varToLookup)

##        ResultMap[ppt] = curResultVarList

### This is important to see how much of the intersection between
### Dfec and Kvasir variables that we've successfully picked up:

###        print "Leftovers", DfecVarMap.keys()
###        print "# vars in Dfec:  ", len(DfecVarMap.keys())
###        print "# vars in Kvasir:", len(KvasirVarList)
###        print "# vars in result:", len(curResultVarList)
###        print

### Output the resulting .decls file and the var list file:

### Globals section ... let's just take the first program point and use
### the global vars in that one for the globals section.  This makes the
### assumption that the same global variables appear everywhere at all
### program points ... will have to investigate further later ...

##outputVarsF.write("----SECTION----\n")
##outputVarsF.write("globals\n")

##exampleVarList = ResultMap[KvasirPptNames[0]]

##for varEntry in exampleVarList:
##    if '/' in varEntry[0]: # only print out globals and file-statics
##        outputVarsF.write(varEntry[0])
##        outputVarsF.write("\n")

##outputVarsF.write("\n")


### Filter KvasirPptNames to remove any program points that are NOT
### in the Dfec-generated .decls file:
##KvasirPptNames = [name for
##                  name in KvasirPptNames
##                  if (StripKvasirPptName(name) in DfecPptMap)]


###

##outputNoCompDeclsF.write("VarComparability\nnone\n\n");


##allDeclsFiles = [outputLackwitDeclsF,
##                 outputDynCompDeclsF,
##                 outputDecTypesDeclsF,
##                 outputNoCompDeclsF]

### Output the various .decls files
### (Read these names from KvasirPptNames to preserve ordering)
##for ppt in KvasirPptNames:

##    for f in allDeclsFiles:
##        f.write("DECLARE\n")
##        f.write(ppt)
##        f.write("\n")

##    # Only print the :::EXIT program point to the var list file
##    # because then we can grab the return value 'return'

##    # Remember that we need to print program points in the form of
##    # '..main()' and NOT '..main():::ENTER' and '..main():::EXIT0'
##    isExit = False

##    fnname, enterOrExit = ppt.split(':::')
##    if enterOrExit[:4] == "EXIT":
##        isExit = True

##    if isExit:
##        outputVarsF.write("----SECTION----\n")
##        outputVarsF.write(fnname)
##        outputVarsF.write("\n")

##    for varEntry in ResultMap[ppt]:

##        for f in allDeclsFiles:
##            # Variable name
##            f.write(varEntry[0])
##            f.write("\n")

##            # Declared type
##            f.write(varEntry[1])
##            f.write("\n")

##            # Representation type
##            f.write(varEntry[2])
##            f.write("\n")

##        # Comparability number - this is where the action is!
##        # For Lackwit, we choose the car of the tuple,
##        outputLackwitDeclsF.write(varEntry[3][0])
##        # For DynComp, we choose the cadr
##        outputDynCompDeclsF.write(varEntry[3][1])
##        # For dec. type, we choose the caddr
##        outputDecTypesDeclsF.write(str(varEntry[3][2]))
##        # For no comparability, simply print out '22'
##        outputNoCompDeclsF.write("22")

##        for f in allDeclsFiles:
##            f.write("\n")

##        # Don't print out globals or file-static vars in the
##        # var-list-file for individual program points
##        if isExit and not ('/' in varEntry[0]):
##            outputVarsF.write(varEntry[0])
##            outputVarsF.write("\n")

##    # Newline separating neighboring program points
##    for f in allDeclsFiles:
##        f.write("\n")

##    if isExit:
##        outputVarsF.write("\n")


###print '# Dfec ppts:', len(DfecPptMap.keys())
###print '# Kvasir ppts:', len(KvasirPptMap.keys())
###print '# Common ppts:', len(ResultMap.keys())



##for f in allDeclsFiles:
##    f.close()

##outputVarsF.close()
