decl-version 2.0
var-comparability implicit

# The arrays this.a[] and this.b[] are comparable at the method ppts and always
# equal, so they form one equality set there.  At the OBJECT ppt, they are
# incomparable:  both their elements (2 vs. 4) and their indices (3 vs. 5) are
# in different comparable sets.  At the OBJECT ppt, this.a[] and this.b[] are
# in the same equality set, and both comparable sets are merged, so this.i and
# this.j (indices) become comparable, as do this.k and this.l (elements).

ppt C:::OBJECT
ppt-type object
variable this
  var-kind variable
  dec-type C
  rep-type hashcode
  flags is_param
  comparability 1
variable this.a
  var-kind field a
  enclosing-var this
  dec-type int[]
  rep-type hashcode
  comparability 6
variable this.a[..]
  var-kind array
  enclosing-var this.a
  array 1
  dec-type int[]
  rep-type int[]
  comparability 2[3]
variable this.b
  var-kind field b
  enclosing-var this
  dec-type int[]
  rep-type hashcode
  comparability 7
variable this.b[..]
  var-kind array
  enclosing-var this.b
  array 1
  dec-type int[]
  rep-type int[]
  comparability 4[5]
variable this.i
  var-kind field i
  enclosing-var this
  dec-type int
  rep-type int
  comparability 3
variable this.j
  var-kind field j
  enclosing-var this
  dec-type int
  rep-type int
  comparability 5
variable this.k
  var-kind field k
  enclosing-var this
  dec-type int
  rep-type int
  comparability 2
variable this.l
  var-kind field l
  enclosing-var this
  dec-type int
  rep-type int
  comparability 4

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
variable this.a
  var-kind field a
  enclosing-var this
  dec-type int[]
  rep-type hashcode
  comparability 6
  parent C:::OBJECT 1
variable this.a[..]
  var-kind array
  enclosing-var this.a
  array 1
  dec-type int[]
  rep-type int[]
  comparability 2[3]
  parent C:::OBJECT 1
variable this.b
  var-kind field b
  enclosing-var this
  dec-type int[]
  rep-type hashcode
  comparability 7
  parent C:::OBJECT 1
variable this.b[..]
  var-kind array
  enclosing-var this.b
  array 1
  dec-type int[]
  rep-type int[]
  comparability 2[3]
  parent C:::OBJECT 1
variable this.i
  var-kind field i
  enclosing-var this
  dec-type int
  rep-type int
  comparability 3
  parent C:::OBJECT 1
variable this.j
  var-kind field j
  enclosing-var this
  dec-type int
  rep-type int
  comparability 3
  parent C:::OBJECT 1
variable this.k
  var-kind field k
  enclosing-var this
  dec-type int
  rep-type int
  comparability 2
  parent C:::OBJECT 1
variable this.l
  var-kind field l
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
variable this.a
  var-kind field a
  enclosing-var this
  dec-type int[]
  rep-type hashcode
  comparability 6
  parent C:::OBJECT 1
variable this.a[..]
  var-kind array
  enclosing-var this.a
  array 1
  dec-type int[]
  rep-type int[]
  comparability 2[3]
  parent C:::OBJECT 1
variable this.b
  var-kind field b
  enclosing-var this
  dec-type int[]
  rep-type hashcode
  comparability 7
  parent C:::OBJECT 1
variable this.b[..]
  var-kind array
  enclosing-var this.b
  array 1
  dec-type int[]
  rep-type int[]
  comparability 2[3]
  parent C:::OBJECT 1
variable this.i
  var-kind field i
  enclosing-var this
  dec-type int
  rep-type int
  comparability 3
  parent C:::OBJECT 1
variable this.j
  var-kind field j
  enclosing-var this
  dec-type int
  rep-type int
  comparability 3
  parent C:::OBJECT 1
variable this.k
  var-kind field k
  enclosing-var this
  dec-type int
  rep-type int
  comparability 2
  parent C:::OBJECT 1
variable this.l
  var-kind field l
  enclosing-var this
  dec-type int
  rep-type int
  comparability 2
  parent C:::OBJECT 1
