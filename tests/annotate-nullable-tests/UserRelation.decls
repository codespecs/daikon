decl-version 2.0
var-comparability none

# Static method D.sm(C) takes a parameter of class C, so each of its program
# points has a USER relation to C:::OBJECT, in addition to its PARENT relation
# to D:::CLASS.  Daikon creates USER relations only if it is run with
# daikon.PptRelation.enable_object_user=true.  AnnotateNullable outputs sm as
# a static method of D, not as an instance method of C.

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
  dec-type java.lang.Object
  rep-type hashcode

ppt C.get():::ENTER
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

ppt C.get():::EXIT1
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
variable return
  var-kind return
  dec-type java.lang.Object
  rep-type hashcode

ppt D:::CLASS
ppt-type class
variable D.s
  var-kind variable
  dec-type java.lang.String
  rep-type hashcode

ppt D:::OBJECT
ppt-type object
parent parent D:::CLASS 1
variable this
  var-kind variable
  dec-type D
  rep-type hashcode
  flags is_param non_null
variable D.s
  var-kind variable
  dec-type java.lang.String
  rep-type hashcode
  parent D:::CLASS 1

ppt D.sm(C):::ENTER
ppt-type enter
parent parent D:::CLASS 1
parent user C:::OBJECT 2
variable D.s
  var-kind variable
  dec-type java.lang.String
  rep-type hashcode
  parent D:::CLASS 1
variable c
  var-kind variable
  dec-type C
  rep-type hashcode
  flags is_param
  parent C:::OBJECT 2 this
variable c.f
  var-kind field f
  enclosing-var c
  dec-type java.lang.Object
  rep-type hashcode
  parent C:::OBJECT 2 this.f

ppt D.sm(C):::EXIT1
ppt-type subexit
parent parent D:::CLASS 1
parent user C:::OBJECT 2
variable D.s
  var-kind variable
  dec-type java.lang.String
  rep-type hashcode
  parent D:::CLASS 1
variable c
  var-kind variable
  dec-type C
  rep-type hashcode
  flags is_param
  parent C:::OBJECT 2 this
variable c.f
  var-kind field f
  enclosing-var c
  dec-type java.lang.Object
  rep-type hashcode
  parent C:::OBJECT 2 this.f
variable return
  var-kind return
  dec-type java.lang.String
  rep-type hashcode
