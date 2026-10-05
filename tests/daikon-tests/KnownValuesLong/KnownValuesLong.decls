decl-version 2.0
var-comparability implicit

# Like the KnownValues test, but the min-value and max-value attributes
# do not fit in a Java int, or are not integers.  See
# https://github.com/codespecs/daikon/issues/678 .

# If a.maxvalue == Long.MAX_VALUE, 'a <= 9223372036854775807' is suppressed.
ppt upperBoundSuppressed:::ENTER
ppt-type enter

ppt upperBoundSuppressed:::EXIT1
ppt-type exit
variable a
  var-kind variable
  dec-type long
  rep-type int
  max-value 9223372036854775807
  comparability 1

# If a.maxvalue == Long.MAX_VALUE, 'a <= 106' is NOT suppressed.
ppt upperBoundNotSuppressed:::ENTER
ppt-type enter

ppt upperBoundNotSuppressed:::EXIT1
ppt-type exit
variable a
  var-kind variable
  dec-type long
  rep-type int
  max-value 9223372036854775807
  comparability 1

# If a.minvalue == Long.MIN_VALUE, 'a >= -9223372036854775808' is suppressed.
ppt lowerBoundSuppressed:::ENTER
ppt-type enter

ppt lowerBoundSuppressed:::EXIT1
ppt-type exit
variable a
  var-kind variable
  dec-type long
  rep-type int
  min-value -9223372036854775808
  comparability 1

# If a.minvalue == Long.MIN_VALUE, 'a >= 100' is NOT suppressed.
ppt lowerBoundNotSuppressed:::ENTER
ppt-type enter

ppt lowerBoundNotSuppressed:::EXIT1
ppt-type exit
variable a
  var-kind variable
  dec-type long
  rep-type int
  min-value -9223372036854775808
  comparability 1

# If minvalue == maxvalue == 5000000000 and 'x == 5000000000' is inferred,
# 'x == 5000000000' is suppressed.
ppt constantMinMaxMatch:::ENTER
ppt-type enter

ppt constantMinMaxMatch:::EXIT1
ppt-type exit
variable return
  var-kind return
  dec-type long
  rep-type int
  min-value 5000000000
  max-value 5000000000
  comparability 1

# If minvalue == maxvalue == 5000000001 and 'x == 5000000000' is inferred,
# 'x == 5000000000' is NOT suppressed.
ppt constantMinMaxDifferent:::ENTER
ppt-type enter

ppt constantMinMaxDifferent:::EXIT1
ppt-type exit
variable return
  var-kind return
  dec-type long
  rep-type int
  min-value 5000000001
  max-value 5000000001
  comparability 1

# If a.maxvalue == a.minvalue == b.maxvalue == b.minvalue, 'a == b' is suppressed.
ppt constantPairMinMaxMatch:::ENTER
ppt-type enter

ppt constantPairMinMaxMatch:::EXIT1
ppt-type exit
variable a
  var-kind variable
  dec-type long
  rep-type int
  min-value 5000000000
  max-value 5000000000
  comparability 1
variable b
  var-kind variable
  dec-type long
  rep-type int
  min-value 5000000000
  max-value 5000000000
  comparability 1

# If a.minvalue > b.maxvalue, 'a > b' is suppressed.
ppt variableGreaterSuppressed:::ENTER
ppt-type enter

ppt variableGreaterSuppressed:::EXIT1
ppt-type exit
variable a
  var-kind variable
  dec-type long
  rep-type int
  min-value 5000000001
  comparability 1
variable b
  var-kind variable
  dec-type long
  rep-type int
  max-value 5000000000
  comparability 1

# If a.minvalue == b.maxvalue, 'a > b' is NOT suppressed.
ppt variableGreaterNotSuppressed:::ENTER
ppt-type enter

ppt variableGreaterNotSuppressed:::EXIT1
ppt-type exit
variable a
  var-kind variable
  dec-type long
  rep-type int
  min-value 5000000000
  comparability 1
variable b
  var-kind variable
  dec-type long
  rep-type int
  max-value 5000000000
  comparability 1

# If a.minvalue == 0.5, 'a >= 0.5' is suppressed.
ppt floatLowerBoundSuppressed:::ENTER
ppt-type enter

ppt floatLowerBoundSuppressed:::EXIT1
ppt-type exit
variable a
  var-kind variable
  dec-type double
  rep-type double
  min-value 0.5
  comparability 1

# If a.minvalue == 0.25, 'a >= 0.5' is NOT suppressed.
ppt floatLowerBoundNotSuppressed:::ENTER
ppt-type enter

ppt floatLowerBoundNotSuppressed:::EXIT1
ppt-type exit
variable a
  var-kind variable
  dec-type double
  rep-type double
  min-value 0.25
  comparability 1

# If a.maxvalue < b.minvalue, 'a < b' is suppressed.
ppt floatLessSuppressed:::ENTER
ppt-type enter

ppt floatLessSuppressed:::EXIT1
ppt-type exit
variable a
  var-kind variable
  dec-type double
  rep-type double
  max-value 1.5
  comparability 1
variable b
  var-kind variable
  dec-type double
  rep-type double
  min-value 1.75
  comparability 1

# If a.maxvalue == b.minvalue, 'a < b' is NOT suppressed.
ppt floatLessNotSuppressed:::ENTER
ppt-type enter

ppt floatLessNotSuppressed:::EXIT1
ppt-type exit
variable a
  var-kind variable
  dec-type double
  rep-type double
  max-value 1.75
  comparability 1
variable b
  var-kind variable
  dec-type double
  rep-type double
  min-value 1.75
  comparability 1
