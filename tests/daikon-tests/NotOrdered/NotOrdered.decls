decl-version 2.0
var-comparability none

# Test for https://github.com/codespecs/daikon/issues/7 .
# In flagged(), the arrays are declared not_ordered, no_dups, and no_size, so Daikon
# should not report order-dependent invariants such as lexical comparisons, sortedness,
# or a[i] >= i, nor size() for them.  ordered() receives the same values without flags.

ppt NotOrdered.ordered(int[],int[]):::ENTER
ppt-type enter
variable a
  var-kind variable
  dec-type int[]
  rep-type hashcode
  flags is_param
variable a[..]
  var-kind array
  enclosing-var a
  array 1
  dec-type int[]
  rep-type int[]
variable b
  var-kind variable
  dec-type int[]
  rep-type hashcode
  flags is_param
variable b[..]
  var-kind array
  enclosing-var b
  array 1
  dec-type int[]
  rep-type int[]

ppt NotOrdered.ordered(int[],int[]):::EXIT1
ppt-type subexit
variable a
  var-kind variable
  dec-type int[]
  rep-type hashcode
  flags is_param
variable a[..]
  var-kind array
  enclosing-var a
  array 1
  dec-type int[]
  rep-type int[]
variable b
  var-kind variable
  dec-type int[]
  rep-type hashcode
  flags is_param
variable b[..]
  var-kind array
  enclosing-var b
  array 1
  dec-type int[]
  rep-type int[]

ppt NotOrdered.flagged(int[],int[]):::ENTER
ppt-type enter
variable a
  var-kind variable
  dec-type int[]
  rep-type hashcode
  flags is_param
variable a[..]
  var-kind array
  enclosing-var a
  array 1
  dec-type int[]
  rep-type int[]
  flags not_ordered no_dups no_size
variable b
  var-kind variable
  dec-type int[]
  rep-type hashcode
  flags is_param
variable b[..]
  var-kind array
  enclosing-var b
  array 1
  dec-type int[]
  rep-type int[]
  flags not_ordered no_dups no_size

ppt NotOrdered.flagged(int[],int[]):::EXIT1
ppt-type subexit
variable a
  var-kind variable
  dec-type int[]
  rep-type hashcode
  flags is_param
variable a[..]
  var-kind array
  enclosing-var a
  array 1
  dec-type int[]
  rep-type int[]
  flags not_ordered no_dups no_size
variable b
  var-kind variable
  dec-type int[]
  rep-type hashcode
  flags is_param
variable b[..]
  var-kind array
  enclosing-var b
  array 1
  dec-type int[]
  rep-type int[]
  flags not_ordered no_dups no_size
