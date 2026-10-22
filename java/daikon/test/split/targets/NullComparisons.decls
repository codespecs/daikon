decl-version 2.0

// Declarations for a class whose fields are compared with null in splitting conditions.  The
// splitter represents this.sign and the elements of this.s as Strings, and this.obj and the
// elements of this.objs as hashcodes.

var-comparability none

ppt misc.NullComparisons.m():::ENTER
  ppt-type enter
  variable this
    var-kind variable
    dec-type misc.NullComparisons
    rep-type hashcode
    flags is_param
    comparability 22
  variable this.sign
    var-kind field sign
    enclosing-var this
    dec-type java.lang.String
    rep-type java.lang.String
    comparability 22
  variable this.obj
    var-kind field obj
    enclosing-var this
    dec-type java.lang.Object
    rep-type hashcode
    comparability 22
  variable this.s
    var-kind field s
    enclosing-var this
    dec-type java.lang.String[]
    rep-type hashcode
    comparability 22
  variable this.s[..]
    var-kind array
    enclosing-var this.s
    array 1
    dec-type java.lang.String[]
    rep-type java.lang.String[]
    comparability 22
  variable this.objs
    var-kind field objs
    enclosing-var this
    dec-type java.lang.Object[]
    rep-type hashcode
    comparability 22
  variable this.objs[..]
    var-kind array
    enclosing-var this.objs
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    comparability 22

ppt misc.NullComparisons.m():::EXIT10
  ppt-type subexit
  variable this
    var-kind variable
    dec-type misc.NullComparisons
    rep-type hashcode
    flags is_param
    comparability 22
  variable this.sign
    var-kind field sign
    enclosing-var this
    dec-type java.lang.String
    rep-type java.lang.String
    comparability 22
  variable this.obj
    var-kind field obj
    enclosing-var this
    dec-type java.lang.Object
    rep-type hashcode
    comparability 22
  variable this.s
    var-kind field s
    enclosing-var this
    dec-type java.lang.String[]
    rep-type hashcode
    comparability 22
  variable this.s[..]
    var-kind array
    enclosing-var this.s
    array 1
    dec-type java.lang.String[]
    rep-type java.lang.String[]
    comparability 22
  variable this.objs
    var-kind field objs
    enclosing-var this
    dec-type java.lang.Object[]
    rep-type hashcode
    comparability 22
  variable this.objs[..]
    var-kind array
    enclosing-var this.objs
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    comparability 22
