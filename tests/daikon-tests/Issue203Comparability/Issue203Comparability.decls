decl-version 2.0
var-comparability implicit

# x, y, and w are comparable at the method ppts.  At the OBJECT ppt, y and w are
# declared comparable, but x is declared incomparable to both.  Daikon makes x,
# y, and w comparable at the OBJECT ppt, because they are comparable at its
# children.  x and y are always equal, so they are in the same equality set at
# the OBJECT ppt, with leader x.  Because x is comparable to w there, the OBJECT
# ppt has the invariant x < w.
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
