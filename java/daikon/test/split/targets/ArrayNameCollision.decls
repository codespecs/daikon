decl-version 2.0

// Declarations for a class with int arrays a and a_array. The compilable name of the elements of a
// is this_a_array, which is also the base name of a_array.

var-comparability none

ppt misc.ArrayNameCollision.m():::EXIT10
  ppt-type subexit
  variable this
    var-kind variable
    dec-type misc.ArrayNameCollision
    rep-type hashcode
    flags is_param
  variable this.a
    var-kind field a
    enclosing-var this
    dec-type int[]
    rep-type hashcode
  variable this.a[..]
    var-kind array
    enclosing-var this.a
    array 1
    dec-type int[]
    rep-type int[]
  variable this.a_array
    var-kind field a_array
    enclosing-var this
    dec-type int[]
    rep-type hashcode
  variable this.a_array[..]
    var-kind array
    enclosing-var this.a_array
    array 1
    dec-type int[]
    rep-type int[]
