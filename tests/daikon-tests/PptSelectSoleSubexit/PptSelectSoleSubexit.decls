decl-version 2.0
var-comparability none

# C.m(int) has a single numbered exit point, C.m(int):::EXIT5.  Daikon creates
# the combined exit point C.m(int):::EXIT, which has the same invariants.
# PrintInvariants normally prints only the combined exit point.  The Makefile
# passes PrintInvariants a --ppt-select-pattern that matches C.m(int):::EXIT5
# but not C.m(int):::EXIT, so PrintInvariants prints the invariants of
# C.m(int):::EXIT5.
ppt C.m(int):::ENTER
ppt-type enter
variable x
  var-kind variable
  dec-type int
  rep-type int
  flags is_param

ppt C.m(int):::EXIT5
ppt-type subexit
variable x
  var-kind variable
  dec-type int
  rep-type int
  flags is_param
variable return
  var-kind return
  dec-type int
  rep-type int
