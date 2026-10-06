decl-version 2.0
var-comparability none

# The range of x exceeds Long.MAX_VALUE, so computing max - min + 1 in a long overflows.
ppt NonZeroLargeRange.m(long):::ENTER
ppt-type enter
variable x
  var-kind variable
  dec-type long
  rep-type int
  comparability 1

ppt NonZeroLargeRange.m(long):::EXIT1
ppt-type exit
variable x
  var-kind variable
  dec-type long
  rep-type int
  comparability 1
