// Declarations for misc.CharBuf, whose field buf is a char[].

decl-version 2.0
var-comparability none

ppt misc.CharBuf.isEmpty():::ENTER
ppt-type enter
variable this
  var-kind variable
  dec-type misc.CharBuf
  rep-type hashcode
  flags is_param non_null
variable this.buf
  var-kind field buf
  enclosing-var this
  dec-type char[]
  rep-type hashcode
variable this.buf[..]
  var-kind array
  enclosing-var this.buf
  array 1
  dec-type char[]
  rep-type int[]

ppt misc.CharBuf.isEmpty():::EXIT10
ppt-type subexit
variable this
  var-kind variable
  dec-type misc.CharBuf
  rep-type hashcode
  flags is_param non_null
variable this.buf
  var-kind field buf
  enclosing-var this
  dec-type char[]
  rep-type hashcode
variable this.buf[..]
  var-kind array
  enclosing-var this.buf
  array 1
  dec-type char[]
  rep-type int[]
variable return
  var-kind return
  dec-type boolean
  rep-type boolean
