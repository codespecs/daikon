decl-version 2.0
var-comparability none

# Class C has a CLASS program point but no OBJECT program point.  The CLASS
# program point receives the samples of the static method C.m(int), so its
# invariants include "C.s == 7".
ppt C:::CLASS
ppt-type class
variable C.s
  var-kind variable
  dec-type int
  rep-type int

ppt C.m(int):::ENTER
ppt-type enter
parent parent C:::CLASS 1
variable C.s
  var-kind variable
  dec-type int
  rep-type int
  parent C:::CLASS 1
variable x
  var-kind variable
  dec-type int
  rep-type int
  flags is_param

ppt C.m(int):::EXIT1
ppt-type subexit
parent parent C:::CLASS 1
variable C.s
  var-kind variable
  dec-type int
  rep-type int
  parent C:::CLASS 1
variable x
  var-kind variable
  dec-type int
  rep-type int
  flags is_param
variable return
  var-kind return
  dec-type int
  rep-type int
