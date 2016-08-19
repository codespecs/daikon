decl-version 2.0
var-comparability implicit

# If minvalue == maxvalue == 1 and 'x == 1' is inferred, 'x == 1' is suppressed
ppt constantMinMaxMatch:::ENTER
ppt-type enter

ppt constantMinMaxMatch:::EXIT1
ppt-type exit
parent parent constantMinMaxMatch:::ENTER 1
variable return
  var-kind return
  dec-type int
  rep-type int
  min-value 1
  max-value 1
  comparability 1

# If a.validvalues == [0] and a.minlength == a.maxlength == 1, 'a == [0]'
# and 'size(a) == 1' are suppressed
ppt singletonSequenceOneOfSuppressed:::ENTER
ppt-type enter

ppt singletonSequenceOneOfSuppressed:::EXIT1
ppt-type exit
parent parent singletonSequenceOneOfSuppressed:::ENTER 1
variable a
  var-kind variable
  dec-type int[]
  rep-type hashcode
  comparability 1
variable a[..]
  var-kind array
  enclosing-var a
  array 1
  dec-type int[]
  rep-type int[]
  comparability 1[2]
  valid-values [0]
  min-length 1
  max-length 1
