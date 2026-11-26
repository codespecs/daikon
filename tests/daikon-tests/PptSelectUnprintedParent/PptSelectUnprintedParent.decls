decl-version 2.0
var-comparability none

# C.m(int) has a single numbered exit point, C.m(int):::EXIT5, and C.n(int)
# has two, C.n(int):::EXIT7 and C.n(int):::EXIT9.  The Makefile passes
# PrintInvariants a --ppt-select-pattern that matches the exit points but not
# the entry points or C:::OBJECT, so PrintInvariants prints, at the exit
# points, the invariants that C:::OBJECT or the method's entry point imply,
# such as this.f == 10 and orig(x) one of { 1, 2, 3 }.

ppt C:::OBJECT
ppt-type object
variable this
  var-kind variable
  dec-type C
  rep-type hashcode
  flags is_param non_null
variable this.f
  var-kind field f
  enclosing-var this
  dec-type int
  rep-type int

ppt C.m(int):::ENTER
ppt-type enter
parent parent C:::OBJECT 1
variable this
  var-kind variable
  dec-type C
  rep-type hashcode
  flags is_param non_null
  parent C:::OBJECT 1
variable this.f
  var-kind field f
  enclosing-var this
  dec-type int
  rep-type int
  parent C:::OBJECT 1
variable x
  var-kind variable
  dec-type int
  rep-type int
  flags is_param

ppt C.m(int):::EXIT5
ppt-type subexit
parent parent C:::OBJECT 1
variable this
  var-kind variable
  dec-type C
  rep-type hashcode
  flags is_param non_null
  parent C:::OBJECT 1
variable this.f
  var-kind field f
  enclosing-var this
  dec-type int
  rep-type int
  parent C:::OBJECT 1
variable x
  var-kind variable
  dec-type int
  rep-type int
  flags is_param
variable return
  var-kind return
  dec-type int
  rep-type int

ppt C.n(int):::ENTER
ppt-type enter
parent parent C:::OBJECT 1
variable this
  var-kind variable
  dec-type C
  rep-type hashcode
  flags is_param non_null
  parent C:::OBJECT 1
variable this.f
  var-kind field f
  enclosing-var this
  dec-type int
  rep-type int
  parent C:::OBJECT 1
variable x
  var-kind variable
  dec-type int
  rep-type int
  flags is_param

ppt C.n(int):::EXIT7
ppt-type subexit
parent parent C:::OBJECT 1
variable this
  var-kind variable
  dec-type C
  rep-type hashcode
  flags is_param non_null
  parent C:::OBJECT 1
variable this.f
  var-kind field f
  enclosing-var this
  dec-type int
  rep-type int
  parent C:::OBJECT 1
variable x
  var-kind variable
  dec-type int
  rep-type int
  flags is_param
variable return
  var-kind return
  dec-type int
  rep-type int

ppt C.n(int):::EXIT9
ppt-type subexit
parent parent C:::OBJECT 1
variable this
  var-kind variable
  dec-type C
  rep-type hashcode
  flags is_param non_null
  parent C:::OBJECT 1
variable this.f
  var-kind field f
  enclosing-var this
  dec-type int
  rep-type int
  parent C:::OBJECT 1
variable x
  var-kind variable
  dec-type int
  rep-type int
  flags is_param
variable return
  var-kind return
  dec-type int
  rep-type int
