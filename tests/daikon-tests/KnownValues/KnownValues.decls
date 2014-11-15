VarComparability
implicit

# If minvalue == maxvalue == 1 and 'x == 1' is inferred, 'x == 1' is suppressed
DECLARE
constantMinMaxMatch:::ENTER

DECLARE
constantMinMaxMatch:::EXIT1
return
int # minvalue=1, maxvalue=1
int
1

# If minvalue == maxvalue == 2 and 'x == 1' is inferred, 'x == 1' is NOT suppressed
DECLARE
constantMinMaxDifferent:::ENTER

DECLARE
constantMinMaxDifferent:::EXIT1
return
int # minvalue=2, maxvalue=2
int
1

# If a.maxvalue == a.minvalue == b.maxvalue == b.minvalue, 'a == b' is suppressed
DECLARE
constantPairMinMaxMatch:::ENTER

DECLARE
constantPairMinMaxMatch:::EXIT1
a
int # minvalue=0, maxvalue=0
int
1
b
int # minvalue=0, maxvalue=0
int
1

# If a.maxvalue == 0 and return.minvalue == 0, 'a <= return' is suppressed
DECLARE
variableGreaterEqualSuppressed:::ENTER

DECLARE
variableGreaterEqualSuppressed:::EXIT1
a
int # maxvalue=0
int
1
return
int # minvalue=0
int
1

# If a.minvalue > b.maxvalue, 'a > b' is suppressed
DECLARE
variableGreaterSuppressed:::ENTER

DECLARE
variableGreaterSuppressed:::EXIT1
a
int # minvalue=12
int
1
b
int # maxvalue=11
int
1

# If a.validvalues == [0] and a.minlength == a.maxlength == 1, 'a == [0]'
# and 'size(a) == 1' are suppressed
DECLARE
singletonSequenceOneOfSuppressed:::ENTER

DECLARE
singletonSequenceOneOfSuppressed:::EXIT1
a
int[]
hashcode
0
a[]
int[] # validvalues=[0], minlength=1, maxlength=1
int[]
1[2]

# If a.validavalues == [0, 1], 'a is one of (0, 1)' is suppressed
DECLARE
sequenceElementOneOfSuppressed:::ENTER

DECLARE
sequenceElementOneOfSuppressed:::EXIT1
a
int[]
hashcode
0
a[]
int[] # validvalues=[0 1]
int[]
1

# If a.validavalues == [0, 1] and a.maxlength == a.minlength = 1,
# 'a[] is one of ([0], [1])' is suppressed.
DECLARE
sequenceOneOfSuppressed:::ENTER

DECLARE
sequenceOneOfSuppressed:::EXIT1
a
int[]
hashcode
0
a[]
int[] # validvalues=[0 1], maxlength=1, minlength=1
int[]
1

# If a.minvalue == 1, 'a >= 1' is suppressed. Note that the
# trace repeats 'a = 1' at least five more times than the 
# next values, ensuring the invariant gets produced (and
# then suppressed).
DECLARE
lowerBoundSuppressed:::ENTER

DECLARE
lowerBoundSuppressed:::EXIT1
a
int # minvalue=1
int
1

# If a.minvalue == 0, 'a >= 1' is NOT suppressed (stronger invariant than expected)
DECLARE
lowerBoundNotSuppressedStronger:::ENTER

DECLARE
lowerBoundNotSuppressedStronger:::EXIT1
a
int # minvalue=0
int
1

# If a.minvalue == 2, 'a >= 1' is NOT suppressed (weaker invariant than expected)
DECLARE
lowerBoundNotSuppressedWeaker:::ENTER

DECLARE
lowerBoundNotSuppressedWeaker:::EXIT1
a
int # minvalue=2
int
1

# If a.maxvalue == 2, 'a <= 2' is suppressed.
DECLARE
upperBoundSuppressed:::ENTER

DECLARE
upperBoundSuppressed:::EXIT1
a
int # maxvalue=2
int
1

# If a.validvalues=['true' 'false'], "a is one of {'true', 'false'}" is suppressed
DECLARE
stringValidValuesSuppressed:::ENTER

DECLARE
stringValidValuesSuppressed:::EXIT1
a
string # validvalues=[true false]
java.lang.String
1

# If a.validvalues=['true' 'false'], "a is one of {'true', 'false', 'potato'}" is NOT suppressed
DECLARE
stringValidValuesNotSuppressed:::ENTER

DECLARE
stringValidValuesNotSuppressed:::EXIT1
a
string # validvalues=[true false]
java.lang.String
1

DECLARE
sequenceStringValidValuesSuppressed:::ENTER

DECLARE
sequenceStringValidValuesSuppressed:::EXIT1
a
java.lang.String[]
hashcode
1
a[]
java.lang.String[] # validvalues=[true false]
java.lang.String[]
2[3]

DECLARE
sequenceStringValidValuesNotSuppressed:::ENTER

DECLARE
sequenceStringValidValuesNotSuppressed:::EXIT1
a
java.lang.String[]
hashcode
1
a[]
java.lang.String[] # validvalues=[true false]
java.lang.String[]
2[3]

DECLARE
intOneOfSuppressed:::ENTER

DECLARE
intOneOfSuppressed:::EXIT1
a
int # validvalues=[0 1]
int
1

DECLARE
intOneOfNotSuppressed:::ENTER

DECLARE
intOneOfNotSuppressed:::EXIT1
a
int # validvalues=[0 1]
int
1

DECLARE
sequenceIntOneOfSuppressed:::ENTER

DECLARE
sequenceIntOneOfSuppressed:::EXIT1
a
int[]
hashcode
0
a[]
int[] # validvalues=[0 1]
int[]
1[2]

DECLARE
sequenceIntOneOfNotSuppressed:::ENTER

DECLARE
sequenceIntOneOfNotSuppressed:::EXIT1
a
int[]
hashcode
0
a[]
int[] # validvalues=[0 1]
int[]
1[2]
