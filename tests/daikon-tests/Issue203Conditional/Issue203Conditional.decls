decl-version 2.0
var-comparability implicit

# this.x, this.w, and this.q are comparable at the method ppts.  At the OBJECT
# ppt, this.x is incomparable to this.w and this.q.  this.x == this.w only when
# this.x > 0, the splitting condition, so they are in the same equality set only
# at the conditional OBJECT ppt.  The OBJECT ppt and its conditional ppts have
# the same comparability, so this.x < this.q is reported at the OBJECT ppt
# rather than as an implication.

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
variable this.w
  var-kind field w
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
  comparability 2
  parent C:::OBJECT 1
variable this.w
  var-kind field w
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
  comparability 2
  parent C:::OBJECT 1
variable this.w
  var-kind field w
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
