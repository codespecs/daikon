decl-version 2.0
var-comparability none

# A :::POINT program point (as produced by convertcsv.pl) is a leaf of the
# dataflow hierarchy.  SplitPoint.spinfo splits it, which should yield
# conditional invariants and implications relating x and y.
ppt SplitPoint:::POINT
ppt-type point
variable x
  var-kind variable
  dec-type int
  rep-type int
  comparability 1
variable y
  var-kind variable
  dec-type int
  rep-type int
  comparability 1
