decl-version 2.0
var-comparability none

# Class C has a static method sm and an instance method m.  The CLASS ppt and
# the OBJECT ppt each also have a child that is a point, not a method:
# C.checkStatic():::POINT and C.check():::POINT.  AnnotateNullable obtains
# the information about a method from its combined exit, so it ignores the
# points.

ppt C:::CLASS
ppt-type class
variable C.s
  var-kind variable
  dec-type java.lang.String
  rep-type hashcode

ppt C:::OBJECT
ppt-type object
parent parent C:::CLASS 1
variable this
  var-kind variable
  dec-type C
  rep-type hashcode
  flags is_param non_null
variable this.f
  var-kind field f
  enclosing-var this
  dec-type java.lang.Object
  rep-type hashcode
variable C.s
  var-kind variable
  dec-type java.lang.String
  rep-type hashcode
  parent C:::CLASS 1

ppt C.m(java.lang.Object):::ENTER
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
  dec-type java.lang.Object
  rep-type hashcode
  parent C:::OBJECT 1
variable C.s
  var-kind variable
  dec-type java.lang.String
  rep-type hashcode
  parent C:::OBJECT 1
variable o
  var-kind variable
  dec-type java.lang.Object
  rep-type hashcode
  flags is_param

ppt C.m(java.lang.Object):::EXIT1
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
  dec-type java.lang.Object
  rep-type hashcode
  parent C:::OBJECT 1
variable C.s
  var-kind variable
  dec-type java.lang.String
  rep-type hashcode
  parent C:::OBJECT 1
variable o
  var-kind variable
  dec-type java.lang.Object
  rep-type hashcode
  flags is_param
variable return
  var-kind return
  dec-type java.lang.Object
  rep-type hashcode

ppt C.sm(java.lang.String):::ENTER
ppt-type enter
parent parent C:::CLASS 1
variable C.s
  var-kind variable
  dec-type java.lang.String
  rep-type hashcode
  parent C:::CLASS 1
variable t
  var-kind variable
  dec-type java.lang.String
  rep-type hashcode
  flags is_param

ppt C.sm(java.lang.String):::EXIT1
ppt-type subexit
parent parent C:::CLASS 1
variable C.s
  var-kind variable
  dec-type java.lang.String
  rep-type hashcode
  parent C:::CLASS 1
variable t
  var-kind variable
  dec-type java.lang.String
  rep-type hashcode
  flags is_param
variable return
  var-kind return
  dec-type java.lang.String
  rep-type hashcode

ppt C.check():::POINT
ppt-type point
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
  dec-type java.lang.Object
  rep-type hashcode
  parent C:::OBJECT 1
variable C.s
  var-kind variable
  dec-type java.lang.String
  rep-type hashcode
  parent C:::OBJECT 1

ppt C.checkStatic():::POINT
ppt-type point
parent parent C:::CLASS 1
variable C.s
  var-kind variable
  dec-type java.lang.String
  rep-type hashcode
  parent C:::CLASS 1
