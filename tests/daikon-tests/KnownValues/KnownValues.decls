decl-version 2.0
var-comparability implicit

# If minvalue == maxvalue == 1 and 'x == 1' is inferred, 'x == 1' is suppressed
ppt constantMinMaxMatch:::ENTER
ppt-type enter

ppt constantMinMaxMatch:::EXIT1
ppt-type exit
variable return
  var-kind return
  dec-type int
  rep-type int
  min-value 1
  max-value 1
  comparability 1

# If minvalue == maxvalue == 2 and 'x == 1' is inferred, 'x == 1' is NOT suppressed
ppt constantMinMaxDifferent:::ENTER
ppt-type enter

ppt constantMinMaxDifferent:::EXIT1
ppt-type exit
variable return
  var-kind return
  dec-type int
  rep-type int
  min-value 2
  max-value 2
  comparability 1

# If a.maxvalue == a.minvalue == b.maxvalue == b.minvalue, 'a == b' is suppressed
ppt constantPairMinMaxMatch:::ENTER
ppt-type enter

ppt constantPairMinMaxMatch:::EXIT1
ppt-type exit
variable a
  var-kind variable
  dec-type int
  rep-type int
  min-value 0
  max-value 0
  comparability 1
variable b
  var-kind variable
  dec-type int
  rep-type int
  min-value 0
  max-value 0
  comparability 1

# If a.maxvalue == 0 and return.minvalue == 0, 'a <= return' is suppressed
ppt variableGreaterEqualSuppressed:::ENTER
ppt-type enter

ppt variableGreaterEqualSuppressed:::EXIT1
ppt-type exit
variable a
  var-kind variable
  dec-type int
  rep-type int
  max-value 0
  comparability 1
variable return
  var-kind return
  dec-type int
  rep-type int
  min-value 0
  comparability 1

# If a.minvalue > b.maxvalue, 'a > b' is suppressed
ppt variableGreaterSuppressed:::ENTER
ppt-type enter

ppt variableGreaterSuppressed:::EXIT1
ppt-type exit
variable a
  var-kind variable
  dec-type int
  rep-type int
  min-value 12
  comparability 1
variable b
  var-kind variable
  dec-type int
  rep-type int
  max-value 11
  comparability 1

# If a.validvalues == [0] and a.minlength == a.maxlength == 1, 'a == [0]'
# and 'size(a) == 1' are suppressed
ppt singletonSequenceOneOfSuppressed:::ENTER
ppt-type enter

ppt singletonSequenceOneOfSuppressed:::EXIT1
ppt-type exit
variable a
  var-kind variable
  dec-type int[]
  rep-type hashcode
  comparability 1
variable a[..]
  var-kind array
  array 1
  dec-type int[]
  rep-type int[]
  comparability 1[2]
  valid-values [0]
  min-length 1
  max-length 1

# If a.validavalues == [0, 1], 'a is one of (0, 1)' is suppressed
ppt sequenceElementOneOfSuppressed:::ENTER
ppt-type enter

ppt sequenceElementOneOfSuppressed:::EXIT1
ppt-type exit
variable a
  var-kind variable
  dec-type int[]
  rep-type hashcode
  comparability 1
variable a[..]
  var-kind array
  array 1
  dec-type int[]
  rep-type int[]
  comparability 1[2]
  valid-values [0 1]

# If a.validavalues == [0, 1] and a.maxlength == a.minlength = 1,
# 'a[] is one of ([0], [1])' is suppressed.
ppt sequenceOneOfSuppressed:::ENTER
ppt-type enter

ppt sequenceOneOfSuppressed:::EXIT1
ppt-type exit
variable a
  var-kind variable
  dec-type int[]
  rep-type hashcode
  comparability 1
variable a[..]
  var-kind array
  array 1
  dec-type int[]
  rep-type int[]
  comparability 1[2]
  valid-values [0 1]
min-length 1
max-length 1

# If a.minvalue == 1, 'a >= 1' is suppressed. Note that the
# trace repeats 'a = 1' at least five more times than the 
# next values, ensuring the invariant gets produced (and
# then suppressed).
ppt lowerBoundSuppressed:::ENTER
ppt-type enter

ppt lowerBoundSuppressed:::EXIT1
ppt-type exit
variable a
  var-kind variable
  dec-type int
  rep-type int
  min-value 1
  comparability 1

# If a.minvalue == 0, 'a >= 1' is NOT suppressed (stronger invariant than expected)
ppt lowerBoundNotSuppressedStronger:::ENTER
ppt-type enter

ppt lowerBoundNotSuppressedStronger:::EXIT1
ppt-type exit
variable a
  var-kind variable
  dec-type int
  rep-type int
  min-value 0
  comparability 1

# If a.minvalue == 2, 'a >= 1' is NOT suppressed (weaker invariant than expected)
ppt lowerBoundNotSuppressedWeaker:::ENTER
ppt-type enter

ppt lowerBoundNotSuppressedWeaker:::EXIT1
ppt-type exit
variable a
  var-kind variable
  dec-type int
  rep-type int
  min-value 2
  comparability 1

# If a.maxvalue == 2, 'a <= 2' is suppressed.
ppt upperBoundSuppressed:::ENTER
ppt-type enter

ppt upperBoundSuppressed:::EXIT1
ppt-type exit
variable a
  var-kind variable
  dec-type int
  rep-type int
  max-value 2
  comparability 1

# If a.validvalues=['true' 'false'], "a is one of {'true', 'false'}" is suppressed
ppt stringValidValuesSuppressed:::ENTER
ppt-type enter

ppt stringValidValuesSuppressed:::EXIT1
ppt-type exit
variable a
  var-kind variable
  dec-type java.lang.String
  rep-type java.lang.String
  comparability 1
  valid-values [true false]

# If a.validvalues=['true' 'false'], "a is one of {'true', 'false', 'potato'}" is NOT suppressed
ppt stringValidValuesNotSuppressed:::ENTER
ppt-type enter

ppt stringValidValuesNotSuppressed:::EXIT1
ppt-type exit
variable a
  var-kind variable
  dec-type java.lang.String
  rep-type java.lang.String
  comparability 1
  valid-values [true false]

ppt sequenceStringValidValuesSuppressed:::ENTER
ppt-type enter

ppt sequenceStringValidValuesSuppressed:::EXIT1
ppt-type exit
variable a
  var-kind variable
  dec-type java.lang.String[]
  rep-type hashcode
  comparability 1
variable a[..]
  var-kind array
  array 1
  dec-type java.lang.String[]
  rep-type java.lang.String[]
  comparability 2[3]
  valid-values [true false]

ppt sequenceStringValidValuesNotSuppressed:::ENTER
ppt-type enter

ppt sequenceStringValidValuesNotSuppressed:::EXIT1
ppt-type exit
variable a
  var-kind variable
  dec-type java.lang.String[]
  rep-type hashcode
  comparability 1
variable a[..]
  var-kind array
  array 1
  dec-type java.lang.String[]
  rep-type java.lang.String[]
  comparability 2[3]
  valid-values [true false]

ppt intOneOfSuppressed:::ENTER
ppt-type enter

ppt intOneOfSuppressed:::EXIT1
ppt-type exit
variable a
  var-kind variable
  dec-type int
  rep-type int
  valid-values [0 1]
  comparability 1

ppt intOneOfNotSuppressed:::ENTER
ppt-type enter

ppt intOneOfNotSuppressed:::EXIT1
ppt-type exit
variable a
  var-kind variable
  dec-type int
  rep-type int
  valid-values [0 1]
  comparability 1

ppt sequenceIntOneOfSuppressed:::ENTER
ppt-type enter

ppt sequenceIntOneOfSuppressed:::EXIT1
ppt-type exit
variable a
  var-kind variable
  dec-type int[]
  rep-type hashcode
  comparability 1
variable a[..]
  var-kind array
  array 1
  dec-type int[]
  rep-type int[]
  comparability 1[2]
  valid-values [0 1]

ppt sequenceIntOneOfNotSuppressed:::ENTER
ppt-type enter

ppt sequenceIntOneOfNotSuppressed:::EXIT1
ppt-type exit
variable a
  var-kind variable
  dec-type int[]
  rep-type hashcode
  comparability 1
variable a[..]
  var-kind array
  array 1
  dec-type int[]
  rep-type int[]
  comparability 1[2]
  valid-values [0 1]
