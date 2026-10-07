decl-version 2.0

// Declarations for a class with array fields, used to test splitting conditions that contain
// calls to daikon.Quant methods.

var-comparability none

ppt misc.QuantCalls.m(int):::ENTER
  ppt-type enter
  variable i
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 22
  variable this
    var-kind variable
    dec-type misc.QuantCalls
    rep-type hashcode
    flags is_param
    comparability 22
  variable this.a
    var-kind field a
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    comparability 22
  variable this.a[..]
    var-kind array
    enclosing-var this.a
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22
  variable this.b
    var-kind field b
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    comparability 22
  variable this.b[..]
    var-kind array
    enclosing-var this.b
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22
  variable this.x
    var-kind field x
    enclosing-var this
    dec-type double
    rep-type double
    comparability 22
  variable this.flags
    var-kind field flags
    enclosing-var this
    dec-type boolean[]
    rep-type hashcode
    comparability 22
  variable this.flags[..]
    var-kind array
    enclosing-var this.flags
    array 1
    dec-type boolean[]
    rep-type boolean[]
    comparability 22
  variable this.d
    var-kind field d
    enclosing-var this
    dec-type double[]
    rep-type hashcode
    comparability 22
  variable this.d[..]
    var-kind array
    enclosing-var this.d
    array 1
    dec-type double[]
    rep-type double[]
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
  variable this.str
    var-kind field str
    enclosing-var this
    dec-type char[]
    rep-type java.lang.String
    comparability 22

ppt misc.QuantCalls.m(int):::EXIT10
  ppt-type subexit
  variable i
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 22
  variable this
    var-kind variable
    dec-type misc.QuantCalls
    rep-type hashcode
    flags is_param
    comparability 22
  variable this.a
    var-kind field a
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    comparability 22
  variable this.a[..]
    var-kind array
    enclosing-var this.a
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22
  variable this.b
    var-kind field b
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    comparability 22
  variable this.b[..]
    var-kind array
    enclosing-var this.b
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22
  variable this.x
    var-kind field x
    enclosing-var this
    dec-type double
    rep-type double
    comparability 22
  variable this.flags
    var-kind field flags
    enclosing-var this
    dec-type boolean[]
    rep-type hashcode
    comparability 22
  variable this.flags[..]
    var-kind array
    enclosing-var this.flags
    array 1
    dec-type boolean[]
    rep-type boolean[]
    comparability 22
  variable this.d
    var-kind field d
    enclosing-var this
    dec-type double[]
    rep-type hashcode
    comparability 22
  variable this.d[..]
    var-kind array
    enclosing-var this.d
    array 1
    dec-type double[]
    rep-type double[]
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
  variable this.str
    var-kind field str
    enclosing-var this
    dec-type char[]
    rep-type java.lang.String
    comparability 22
