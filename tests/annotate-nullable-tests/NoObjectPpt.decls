decl-version 2.0
var-comparability none

# No class has an OBJECT ppt.  Class D has a CLASS ppt and a static method
# D.sm.  Class E has neither an OBJECT ppt nor a CLASS ppt, only a static
# method E.sm.  The function f has no class, so AnnotateNullable skips it.

ppt D:::CLASS
ppt-type class
variable D.s
  var-kind variable
  dec-type java.lang.String
  rep-type hashcode

ppt D.sm(java.lang.String):::ENTER
ppt-type enter
parent parent D:::CLASS 1
variable D.s
  var-kind variable
  dec-type java.lang.String
  rep-type hashcode
  parent D:::CLASS 1
variable t
  var-kind variable
  dec-type java.lang.String
  rep-type hashcode
  flags is_param

ppt D.sm(java.lang.String):::EXIT1
ppt-type subexit
parent parent D:::CLASS 1
variable D.s
  var-kind variable
  dec-type java.lang.String
  rep-type hashcode
  parent D:::CLASS 1
variable t
  var-kind variable
  dec-type java.lang.String
  rep-type hashcode
  flags is_param
variable return
  var-kind return
  dec-type java.lang.String
  rep-type hashcode

ppt E.sm(java.lang.String):::ENTER
ppt-type enter
variable t
  var-kind variable
  dec-type java.lang.String
  rep-type hashcode
  flags is_param

ppt E.sm(java.lang.String):::EXIT1
ppt-type subexit
variable t
  var-kind variable
  dec-type java.lang.String
  rep-type hashcode
  flags is_param
variable return
  var-kind return
  dec-type java.lang.String
  rep-type hashcode

ppt f(java.lang.String):::ENTER
ppt-type enter
variable t
  var-kind variable
  dec-type java.lang.String
  rep-type hashcode
  flags is_param

ppt f(java.lang.String):::EXIT1
ppt-type subexit
variable t
  var-kind variable
  dec-type java.lang.String
  rep-type hashcode
  flags is_param
variable return
  var-kind return
  dec-type java.lang.String
  rep-type hashcode
