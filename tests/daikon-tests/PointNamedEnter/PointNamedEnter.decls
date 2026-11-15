decl-version 2.0
var-comparability none

# A program point declared as a point is a leaf of the dataflow hierarchy, even
# if its name ends with ":::ENTER".  Daikon infers invariants from its samples,
# even though they have no nonce and no matching exit.  Because it is the only
# program point, Daikon would find no samples if it treated it as an entry.
ppt PointNamedEnter.m():::ENTER
ppt-type point
variable x
  var-kind variable
  dec-type int
  rep-type int
