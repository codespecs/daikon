decl-version 2.0
var-comparability none

# Each program point is declared as a point, so it is a leaf of the dataflow
# hierarchy whatever its name:  Foo.bar():::ENTER is not an entry,
# Foo.bar():::EXIT5 is not a numbered exit, and Foo.bar():::EXIT is not a
# combined exit.  The samples have no nonces, and no exit uses orig() values.

ppt Foo.bar():::ENTER
ppt-type point
variable x
  var-kind variable
  dec-type int
  rep-type int

ppt Foo.bar():::EXIT5
ppt-type point
variable x
  var-kind variable
  dec-type int
  rep-type int

ppt Foo.bar():::EXIT
ppt-type point
variable x
  var-kind variable
  dec-type int
  rep-type int
