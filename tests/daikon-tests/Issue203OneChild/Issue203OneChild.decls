decl-version 2.0
var-comparability implicit

# x and y are comparable at C.m():::EXIT1 and always equal, so they form one
# equality set there.  They are incomparable at the OBJECT ppt.  Only
# C.m():::EXIT1 has a parent relation to the OBJECT ppt, so the OBJECT ppt has
# a single child, C.m():::EXIT.  x and y are in the same equality set at the
# OBJECT ppt even though they are incomparable there.
ppt C:::OBJECT
ppt-type object
variable this
  var-kind variable
  dec-type C
  rep-type hashcode
  flags is_param
  comparability 1
variable this.x
  var-kind field x
  enclosing-var this
  dec-type int
  rep-type int
  comparability 2
variable this.y
  var-kind field y
  enclosing-var this
  dec-type int
  rep-type int
  comparability 3

ppt C.m():::ENTER
ppt-type enter
variable this
  var-kind variable
  dec-type C
  rep-type hashcode
  flags is_param
  comparability 1
variable this.x
  var-kind field x
  enclosing-var this
  dec-type int
  rep-type int
  comparability 2
variable this.y
  var-kind field y
  enclosing-var this
  dec-type int
  rep-type int
  comparability 2

ppt C.m():::EXIT1
ppt-type subexit
parent parent C:::OBJECT 1
variable this
  var-kind variable
  dec-type C
  rep-type hashcode
  flags is_param
  comparability 1
  parent C:::OBJECT 1
variable this.x
  var-kind field x
  enclosing-var this
  dec-type int
  rep-type int
  comparability 2
  parent C:::OBJECT 1
variable this.y
  var-kind field y
  enclosing-var this
  dec-type int
  rep-type int
  comparability 2
  parent C:::OBJECT 1
