decl-version 2.0
var-comparability implicit

# x and y are comparable at the method ppts but not at the OBJECT ppt.  They
# are always equal, so they are in the same equality set at the OBJECT ppt,
# with leader x.  w is comparable to all of them at the method ppts, but only
# to y at the OBJECT ppt.  Since x and y are equal, x and w must be comparable
# at the OBJECT ppt, so that the OBJECT ppt has the invariant x < w.
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
variable this.w
  var-kind field w
  enclosing-var this
  dec-type int
  rep-type int
  comparability 3

ppt C.m():::ENTER
ppt-type enter
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
variable this.w
  var-kind field w
  enclosing-var this
  dec-type int
  rep-type int
  comparability 2
  parent C:::OBJECT 1

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
variable this.w
  var-kind field w
  enclosing-var this
  dec-type int
  rep-type int
  comparability 2
  parent C:::OBJECT 1
