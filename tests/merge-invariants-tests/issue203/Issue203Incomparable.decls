decl-version 2.0
var-comparability implicit

# The same as Issue203.decls, except that this.x is incomparable to this.y and
# this.q at the method ppts too.  MergeInvariants is given this file, but the
# runs' invariants were computed using Issue203.decls.


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
variable this.q
  var-kind field q
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
  comparability 4
  parent C:::OBJECT 1
variable this.y
  var-kind field y
  enclosing-var this
  dec-type int
  rep-type int
  comparability 2
  parent C:::OBJECT 1
variable this.q
  var-kind field q
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
  comparability 4
  parent C:::OBJECT 1
variable this.y
  var-kind field y
  enclosing-var this
  dec-type int
  rep-type int
  comparability 2
  parent C:::OBJECT 1
variable this.q
  var-kind field q
  enclosing-var this
  dec-type int
  rep-type int
  comparability 2
  parent C:::OBJECT 1
