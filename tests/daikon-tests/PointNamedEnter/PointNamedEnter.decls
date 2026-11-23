decl-version 2.0
var-comparability none

# A program point declared as a point is a leaf of the dataflow hierarchy, even
# if its name ends with ":::ENTER".  Daikon infers invariants from its samples,
# even though they have no nonce and no matching exit.  Because it is the only
# program point, Daikon would wrongly report that inference used no samples if
# it counted each of its samples as an unmatched entry rather than as a sample
# of a leaf.
ppt PointNamedEnter.m():::ENTER
ppt-type point
variable x
  var-kind variable
  dec-type int
  rep-type int
