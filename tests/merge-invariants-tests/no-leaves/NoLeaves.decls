decl-version 2.0
var-comparability none

# No program point is a leaf of the dataflow hierarchy:  the entry and the
# combined exit get their invariants from leaves, and there are no numbered
# exits.
ppt NoLeaves.m():::ENTER
ppt-type enter
variable x
  var-kind variable
  dec-type int
  rep-type int

ppt NoLeaves.m():::EXIT
ppt-type exit
variable x
  var-kind variable
  dec-type int
  rep-type int
