decl-version 2.0
var-comparability none

ppt pkg.Foo.bar(int):::ENTER
ppt-type enter
variable x
  var-kind variable
  dec-type int
  rep-type int
  flags is_param

ppt pkg.Foo.bar(int):::EXIT1
ppt-type subexit
variable x
  var-kind variable
  dec-type int
  rep-type int
  flags is_param
variable return
  var-kind return
  dec-type int
  rep-type int

ppt pkg.Foo.bar(int):::EXIT2
ppt-type subexit
variable x
  var-kind variable
  dec-type int
  rep-type int
  flags is_param
variable return
  var-kind return
  dec-type int
  rep-type int
