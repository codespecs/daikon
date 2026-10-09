decl-version 2.0

var-comparability implicit

ppt Math::BigFloat.STORE_s():::ENTER
  ppt-type enter

ppt Math::BigFloat.STORE_s():::EXIT13
  ppt-type subexit
  variable return
    var-kind return
    dec-type String
    rep-type java.lang.String
    comparability 22

ppt Math::BigFloat.TIESCALAR_s():::ENTER
  ppt-type enter
  variable $class
    var-kind variable
    dec-type String
    rep-type java.lang.String
    flags is_param
    comparability 22

ppt Math::BigFloat.TIESCALAR_s():::EXIT11
  ppt-type subexit
  variable $class
    var-kind variable
    dec-type String
    rep-type java.lang.String
    flags is_param
    comparability 22
  variable return.deref.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return
    var-kind return
    dec-type Object
    rep-type hashcode
    comparability 22

ppt Math::BigFloat.bdiv_s():::ENTER
  ppt-type enter
  variable $self
    var-kind variable
    dec-type String
    rep-type java.lang.String
    flags is_param
    comparability 22
  variable $x._a
    var-kind field _a
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._a.is_defined
    var-kind field is_defined
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._a.is_empty
    var-kind field is_empty
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._f
    var-kind field _f
    enclosing-var $x._e
    dec-type int
    rep-type int
    comparability 1
  variable $x._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e.sign
    var-kind field sign
    enclosing-var $x._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._e.value
    var-kind field value
    enclosing-var $x._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._e
    var-kind field _e
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m._f
    var-kind field _f
    enclosing-var $x._m
    dec-type int
    rep-type int
    comparability 1
  variable $x._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m.sign
    var-kind field sign
    enclosing-var $x._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._m.value
    var-kind field value
    enclosing-var $x._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m
    var-kind field _m
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._p
    var-kind field _p
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._p.is_defined
    var-kind field is_defined
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._p.is_empty
    var-kind field is_empty
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x.sign
    var-kind field sign
    enclosing-var $x
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable $y._a
    var-kind field _a
    enclosing-var $y
    dec-type int
    rep-type int
    comparability 1
  variable $y._a.is_defined
    var-kind field is_defined
    enclosing-var $y._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._a.is_empty
    var-kind field is_empty
    enclosing-var $y._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e._f
    var-kind field _f
    enclosing-var $y._e
    dec-type int
    rep-type int
    comparability 1
  variable $y._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e.sign
    var-kind field sign
    enclosing-var $y._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $y._e.value
    var-kind field value
    enclosing-var $y._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._e
    var-kind field _e
    enclosing-var $y
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._m._f
    var-kind field _f
    enclosing-var $y._m
    dec-type int
    rep-type int
    comparability 1
  variable $y._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._m.sign
    var-kind field sign
    enclosing-var $y._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $y._m.value
    var-kind field value
    enclosing-var $y._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._m
    var-kind field _m
    enclosing-var $y
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._p
    var-kind field _p
    enclosing-var $y
    dec-type int
    rep-type int
    comparability 1
  variable $y._p.is_defined
    var-kind field is_defined
    enclosing-var $y._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._p.is_empty
    var-kind field is_empty
    enclosing-var $y._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y.sign
    var-kind field sign
    enclosing-var $y
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable $a
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 1
  variable $a.is_defined
    var-kind field is_defined
    enclosing-var $a
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $a.is_empty
    var-kind field is_empty
    enclosing-var $a
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $p
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 1
  variable $p.is_defined
    var-kind field is_defined
    enclosing-var $p
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $p.is_empty
    var-kind field is_empty
    enclosing-var $p
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $r.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22

ppt Math::BigFloat.bdiv_s():::EXIT100
  ppt-type subexit
  variable $self
    var-kind variable
    dec-type String
    rep-type java.lang.String
    flags is_param
    comparability 22
  variable $x._a
    var-kind field _a
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._a.is_defined
    var-kind field is_defined
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._a.is_empty
    var-kind field is_empty
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._f
    var-kind field _f
    enclosing-var $x._e
    dec-type int
    rep-type int
    comparability 1
  variable $x._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e.sign
    var-kind field sign
    enclosing-var $x._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._e.value
    var-kind field value
    enclosing-var $x._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._e
    var-kind field _e
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m._f
    var-kind field _f
    enclosing-var $x._m
    dec-type int
    rep-type int
    comparability 1
  variable $x._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m.sign
    var-kind field sign
    enclosing-var $x._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._m.value
    var-kind field value
    enclosing-var $x._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m
    var-kind field _m
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._p
    var-kind field _p
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._p.is_defined
    var-kind field is_defined
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._p.is_empty
    var-kind field is_empty
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x.sign
    var-kind field sign
    enclosing-var $x
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable $y._a
    var-kind field _a
    enclosing-var $y
    dec-type int
    rep-type int
    comparability 1
  variable $y._a.is_defined
    var-kind field is_defined
    enclosing-var $y._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._a.is_empty
    var-kind field is_empty
    enclosing-var $y._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e._f
    var-kind field _f
    enclosing-var $y._e
    dec-type int
    rep-type int
    comparability 1
  variable $y._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e.sign
    var-kind field sign
    enclosing-var $y._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $y._e.value
    var-kind field value
    enclosing-var $y._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._e
    var-kind field _e
    enclosing-var $y
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._m._f
    var-kind field _f
    enclosing-var $y._m
    dec-type int
    rep-type int
    comparability 1
  variable $y._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._m.sign
    var-kind field sign
    enclosing-var $y._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $y._m.value
    var-kind field value
    enclosing-var $y._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._m
    var-kind field _m
    enclosing-var $y
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._p
    var-kind field _p
    enclosing-var $y
    dec-type int
    rep-type int
    comparability 1
  variable $y._p.is_defined
    var-kind field is_defined
    enclosing-var $y._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._p.is_empty
    var-kind field is_empty
    enclosing-var $y._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y.sign
    var-kind field sign
    enclosing-var $y
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable $a
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 1
  variable $a.is_defined
    var-kind field is_defined
    enclosing-var $a
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $a.is_empty
    var-kind field is_empty
    enclosing-var $a
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $p
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 1
  variable $p.is_defined
    var-kind field is_defined
    enclosing-var $p
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $p.is_empty
    var-kind field is_empty
    enclosing-var $p
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $r.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable return._a
    var-kind field _a
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._a.is_defined
    var-kind field is_defined
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._a.is_empty
    var-kind field is_empty
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._f
    var-kind field _f
    enclosing-var return._e
    dec-type int
    rep-type int
    comparability 1
  variable return._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e.sign
    var-kind field sign
    enclosing-var return._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._e.value
    var-kind field value
    enclosing-var return._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._e
    var-kind field _e
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m._f
    var-kind field _f
    enclosing-var return._m
    dec-type int
    rep-type int
    comparability 1
  variable return._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m.sign
    var-kind field sign
    enclosing-var return._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._m.value
    var-kind field value
    enclosing-var return._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m
    var-kind field _m
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._p
    var-kind field _p
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._p.is_defined
    var-kind field is_defined
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._p.is_empty
    var-kind field is_empty
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return.sign
    var-kind field sign
    enclosing-var return
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return
    var-kind return
    dec-type Object
    rep-type hashcode
    comparability 22

ppt Math::BigFloat.bdiv_s():::EXIT117
  ppt-type subexit
  variable $self
    var-kind variable
    dec-type String
    rep-type java.lang.String
    flags is_param
    comparability 22
  variable $x._a
    var-kind field _a
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._a.is_defined
    var-kind field is_defined
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._a.is_empty
    var-kind field is_empty
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._f
    var-kind field _f
    enclosing-var $x._e
    dec-type int
    rep-type int
    comparability 1
  variable $x._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e.sign
    var-kind field sign
    enclosing-var $x._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._e.value
    var-kind field value
    enclosing-var $x._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._e
    var-kind field _e
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m._f
    var-kind field _f
    enclosing-var $x._m
    dec-type int
    rep-type int
    comparability 1
  variable $x._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m.sign
    var-kind field sign
    enclosing-var $x._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._m.value
    var-kind field value
    enclosing-var $x._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m
    var-kind field _m
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._p
    var-kind field _p
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._p.is_defined
    var-kind field is_defined
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._p.is_empty
    var-kind field is_empty
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x.sign
    var-kind field sign
    enclosing-var $x
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable $y._a
    var-kind field _a
    enclosing-var $y
    dec-type int
    rep-type int
    comparability 1
  variable $y._a.is_defined
    var-kind field is_defined
    enclosing-var $y._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._a.is_empty
    var-kind field is_empty
    enclosing-var $y._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e._f
    var-kind field _f
    enclosing-var $y._e
    dec-type int
    rep-type int
    comparability 1
  variable $y._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e.sign
    var-kind field sign
    enclosing-var $y._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $y._e.value
    var-kind field value
    enclosing-var $y._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._e
    var-kind field _e
    enclosing-var $y
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._m._f
    var-kind field _f
    enclosing-var $y._m
    dec-type int
    rep-type int
    comparability 1
  variable $y._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._m.sign
    var-kind field sign
    enclosing-var $y._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $y._m.value
    var-kind field value
    enclosing-var $y._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._m
    var-kind field _m
    enclosing-var $y
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._p
    var-kind field _p
    enclosing-var $y
    dec-type int
    rep-type int
    comparability 1
  variable $y._p.is_defined
    var-kind field is_defined
    enclosing-var $y._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._p.is_empty
    var-kind field is_empty
    enclosing-var $y._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y.sign
    var-kind field sign
    enclosing-var $y
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable $a
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 1
  variable $a.is_defined
    var-kind field is_defined
    enclosing-var $a
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $a.is_empty
    var-kind field is_empty
    enclosing-var $a
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $p
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 1
  variable $p.is_defined
    var-kind field is_defined
    enclosing-var $p
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $p.is_empty
    var-kind field is_empty
    enclosing-var $p
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $r.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable return._a
    var-kind field _a
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._a.is_defined
    var-kind field is_defined
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._a.is_empty
    var-kind field is_empty
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._f
    var-kind field _f
    enclosing-var return._e
    dec-type int
    rep-type int
    comparability 1
  variable return._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e.sign
    var-kind field sign
    enclosing-var return._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._e.value
    var-kind field value
    enclosing-var return._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._e
    var-kind field _e
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m._f
    var-kind field _f
    enclosing-var return._m
    dec-type int
    rep-type int
    comparability 1
  variable return._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m.sign
    var-kind field sign
    enclosing-var return._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._m.value
    var-kind field value
    enclosing-var return._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m
    var-kind field _m
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._p
    var-kind field _p
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._p.is_defined
    var-kind field is_defined
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._p.is_empty
    var-kind field is_empty
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return.sign
    var-kind field sign
    enclosing-var return
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return
    var-kind return
    dec-type Object
    rep-type hashcode
    comparability 22

ppt Math::BigFloat.bdiv_s():::EXIT36
  ppt-type subexit
  variable $self
    var-kind variable
    dec-type String
    rep-type java.lang.String
    flags is_param
    comparability 22
  variable $x._a
    var-kind field _a
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._a.is_defined
    var-kind field is_defined
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._a.is_empty
    var-kind field is_empty
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._f
    var-kind field _f
    enclosing-var $x._e
    dec-type int
    rep-type int
    comparability 1
  variable $x._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e.sign
    var-kind field sign
    enclosing-var $x._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._e.value
    var-kind field value
    enclosing-var $x._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._e
    var-kind field _e
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m._f
    var-kind field _f
    enclosing-var $x._m
    dec-type int
    rep-type int
    comparability 1
  variable $x._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m.sign
    var-kind field sign
    enclosing-var $x._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._m.value
    var-kind field value
    enclosing-var $x._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m
    var-kind field _m
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._p
    var-kind field _p
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._p.is_defined
    var-kind field is_defined
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._p.is_empty
    var-kind field is_empty
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x.sign
    var-kind field sign
    enclosing-var $x
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable $y._a
    var-kind field _a
    enclosing-var $y
    dec-type int
    rep-type int
    comparability 1
  variable $y._a.is_defined
    var-kind field is_defined
    enclosing-var $y._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._a.is_empty
    var-kind field is_empty
    enclosing-var $y._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e._f
    var-kind field _f
    enclosing-var $y._e
    dec-type int
    rep-type int
    comparability 1
  variable $y._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e.sign
    var-kind field sign
    enclosing-var $y._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $y._e.value
    var-kind field value
    enclosing-var $y._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._e
    var-kind field _e
    enclosing-var $y
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._m._f
    var-kind field _f
    enclosing-var $y._m
    dec-type int
    rep-type int
    comparability 1
  variable $y._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._m.sign
    var-kind field sign
    enclosing-var $y._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $y._m.value
    var-kind field value
    enclosing-var $y._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._m
    var-kind field _m
    enclosing-var $y
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._p
    var-kind field _p
    enclosing-var $y
    dec-type int
    rep-type int
    comparability 1
  variable $y._p.is_defined
    var-kind field is_defined
    enclosing-var $y._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._p.is_empty
    var-kind field is_empty
    enclosing-var $y._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y.sign
    var-kind field sign
    enclosing-var $y
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable $a
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 1
  variable $a.is_defined
    var-kind field is_defined
    enclosing-var $a
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $a.is_empty
    var-kind field is_empty
    enclosing-var $a
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $p
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 1
  variable $p.is_defined
    var-kind field is_defined
    enclosing-var $p
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $p.is_empty
    var-kind field is_empty
    enclosing-var $p
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $r.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable return._a
    var-kind field _a
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._a.is_defined
    var-kind field is_defined
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._a.is_empty
    var-kind field is_empty
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._f
    var-kind field _f
    enclosing-var return._e
    dec-type int
    rep-type int
    comparability 1
  variable return._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e.sign
    var-kind field sign
    enclosing-var return._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._e.value
    var-kind field value
    enclosing-var return._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._e
    var-kind field _e
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m._f
    var-kind field _f
    enclosing-var return._m
    dec-type int
    rep-type int
    comparability 1
  variable return._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m.sign
    var-kind field sign
    enclosing-var return._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._m.value
    var-kind field value
    enclosing-var return._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m
    var-kind field _m
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._p
    var-kind field _p
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._p.is_defined
    var-kind field is_defined
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._p.is_empty
    var-kind field is_empty
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return.sign
    var-kind field sign
    enclosing-var return
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return
    var-kind return
    dec-type Object
    rep-type hashcode
    comparability 22

ppt Math::BigFloat.bdiv_s():::EXIT411
  ppt-type subexit
  variable $self
    var-kind variable
    dec-type String
    rep-type java.lang.String
    flags is_param
    comparability 22
  variable $x._a
    var-kind field _a
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._a.is_defined
    var-kind field is_defined
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._a.is_empty
    var-kind field is_empty
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._f
    var-kind field _f
    enclosing-var $x._e
    dec-type int
    rep-type int
    comparability 1
  variable $x._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e.sign
    var-kind field sign
    enclosing-var $x._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._e.value
    var-kind field value
    enclosing-var $x._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._e
    var-kind field _e
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m._f
    var-kind field _f
    enclosing-var $x._m
    dec-type int
    rep-type int
    comparability 1
  variable $x._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m.sign
    var-kind field sign
    enclosing-var $x._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._m.value
    var-kind field value
    enclosing-var $x._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m
    var-kind field _m
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._p
    var-kind field _p
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._p.is_defined
    var-kind field is_defined
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._p.is_empty
    var-kind field is_empty
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x.sign
    var-kind field sign
    enclosing-var $x
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable $y._a
    var-kind field _a
    enclosing-var $y
    dec-type int
    rep-type int
    comparability 1
  variable $y._a.is_defined
    var-kind field is_defined
    enclosing-var $y._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._a.is_empty
    var-kind field is_empty
    enclosing-var $y._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e._f
    var-kind field _f
    enclosing-var $y._e
    dec-type int
    rep-type int
    comparability 1
  variable $y._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e.sign
    var-kind field sign
    enclosing-var $y._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $y._e.value
    var-kind field value
    enclosing-var $y._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._e
    var-kind field _e
    enclosing-var $y
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._m._f
    var-kind field _f
    enclosing-var $y._m
    dec-type int
    rep-type int
    comparability 1
  variable $y._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._m.sign
    var-kind field sign
    enclosing-var $y._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $y._m.value
    var-kind field value
    enclosing-var $y._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._m
    var-kind field _m
    enclosing-var $y
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._p
    var-kind field _p
    enclosing-var $y
    dec-type int
    rep-type int
    comparability 1
  variable $y._p.is_defined
    var-kind field is_defined
    enclosing-var $y._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._p.is_empty
    var-kind field is_empty
    enclosing-var $y._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y.sign
    var-kind field sign
    enclosing-var $y
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable $a
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 1
  variable $a.is_defined
    var-kind field is_defined
    enclosing-var $a
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $a.is_empty
    var-kind field is_empty
    enclosing-var $a
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $p
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 1
  variable $p.is_defined
    var-kind field is_defined
    enclosing-var $p
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $p.is_empty
    var-kind field is_empty
    enclosing-var $p
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $r.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable return._a
    var-kind field _a
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._a.is_defined
    var-kind field is_defined
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._a.is_empty
    var-kind field is_empty
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._f
    var-kind field _f
    enclosing-var return._e
    dec-type int
    rep-type int
    comparability 1
  variable return._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e.sign
    var-kind field sign
    enclosing-var return._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._e.value
    var-kind field value
    enclosing-var return._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._e
    var-kind field _e
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m._f
    var-kind field _f
    enclosing-var return._m
    dec-type int
    rep-type int
    comparability 1
  variable return._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m.sign
    var-kind field sign
    enclosing-var return._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._m.value
    var-kind field value
    enclosing-var return._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m
    var-kind field _m
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._p
    var-kind field _p
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._p.is_defined
    var-kind field is_defined
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._p.is_empty
    var-kind field is_empty
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return.sign
    var-kind field sign
    enclosing-var return
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return
    var-kind return
    dec-type Object
    rep-type hashcode
    comparability 22

ppt Math::BigFloat.bdiv_s():::EXIT70
  ppt-type subexit
  variable $self
    var-kind variable
    dec-type String
    rep-type java.lang.String
    flags is_param
    comparability 22
  variable $x._a
    var-kind field _a
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._a.is_defined
    var-kind field is_defined
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._a.is_empty
    var-kind field is_empty
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._f
    var-kind field _f
    enclosing-var $x._e
    dec-type int
    rep-type int
    comparability 1
  variable $x._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e.sign
    var-kind field sign
    enclosing-var $x._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._e.value
    var-kind field value
    enclosing-var $x._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._e
    var-kind field _e
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m._f
    var-kind field _f
    enclosing-var $x._m
    dec-type int
    rep-type int
    comparability 1
  variable $x._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m.sign
    var-kind field sign
    enclosing-var $x._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._m.value
    var-kind field value
    enclosing-var $x._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m
    var-kind field _m
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._p
    var-kind field _p
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._p.is_defined
    var-kind field is_defined
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._p.is_empty
    var-kind field is_empty
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x.sign
    var-kind field sign
    enclosing-var $x
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable $y._a
    var-kind field _a
    enclosing-var $y
    dec-type int
    rep-type int
    comparability 1
  variable $y._a.is_defined
    var-kind field is_defined
    enclosing-var $y._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._a.is_empty
    var-kind field is_empty
    enclosing-var $y._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e._f
    var-kind field _f
    enclosing-var $y._e
    dec-type int
    rep-type int
    comparability 1
  variable $y._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e.sign
    var-kind field sign
    enclosing-var $y._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $y._e.value
    var-kind field value
    enclosing-var $y._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._e
    var-kind field _e
    enclosing-var $y
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._m._f
    var-kind field _f
    enclosing-var $y._m
    dec-type int
    rep-type int
    comparability 1
  variable $y._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._m.sign
    var-kind field sign
    enclosing-var $y._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $y._m.value
    var-kind field value
    enclosing-var $y._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._m
    var-kind field _m
    enclosing-var $y
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._p
    var-kind field _p
    enclosing-var $y
    dec-type int
    rep-type int
    comparability 1
  variable $y._p.is_defined
    var-kind field is_defined
    enclosing-var $y._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._p.is_empty
    var-kind field is_empty
    enclosing-var $y._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y.sign
    var-kind field sign
    enclosing-var $y
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable $a
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 1
  variable $a.is_defined
    var-kind field is_defined
    enclosing-var $a
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $a.is_empty
    var-kind field is_empty
    enclosing-var $a
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $p
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 1
  variable $p.is_defined
    var-kind field is_defined
    enclosing-var $p
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $p.is_empty
    var-kind field is_empty
    enclosing-var $p
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $r.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable return._a
    var-kind field _a
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._a.is_defined
    var-kind field is_defined
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._a.is_empty
    var-kind field is_empty
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._f
    var-kind field _f
    enclosing-var return._e
    dec-type int
    rep-type int
    comparability 1
  variable return._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e.sign
    var-kind field sign
    enclosing-var return._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._e.value
    var-kind field value
    enclosing-var return._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._e
    var-kind field _e
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m._f
    var-kind field _f
    enclosing-var return._m
    dec-type int
    rep-type int
    comparability 1
  variable return._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m.sign
    var-kind field sign
    enclosing-var return._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._m.value
    var-kind field value
    enclosing-var return._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m
    var-kind field _m
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._p
    var-kind field _p
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._p.is_defined
    var-kind field is_defined
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._p.is_empty
    var-kind field is_empty
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return.sign
    var-kind field sign
    enclosing-var return
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return
    var-kind return
    dec-type Object
    rep-type hashcode
    comparability 22

ppt Math::BigFloat.bfround_s():::ENTER
  ppt-type enter
  variable $x._a
    var-kind field _a
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._a.is_defined
    var-kind field is_defined
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._a.is_empty
    var-kind field is_empty
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._f
    var-kind field _f
    enclosing-var $x._e
    dec-type int
    rep-type int
    comparability 1
  variable $x._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e.sign
    var-kind field sign
    enclosing-var $x._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._e.value
    var-kind field value
    enclosing-var $x._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._e
    var-kind field _e
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m._f
    var-kind field _f
    enclosing-var $x._m
    dec-type int
    rep-type int
    comparability 1
  variable $x._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m.sign
    var-kind field sign
    enclosing-var $x._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._m.value
    var-kind field value
    enclosing-var $x._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m
    var-kind field _m
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._p
    var-kind field _p
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._p.is_defined
    var-kind field is_defined
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._p.is_empty
    var-kind field is_empty
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x.sign
    var-kind field sign
    enclosing-var $x
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22

ppt Math::BigFloat.bfround_s():::EXIT104
  ppt-type subexit
  variable $x._a
    var-kind field _a
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._a.is_defined
    var-kind field is_defined
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._a.is_empty
    var-kind field is_empty
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._f
    var-kind field _f
    enclosing-var $x._e
    dec-type int
    rep-type int
    comparability 1
  variable $x._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e.sign
    var-kind field sign
    enclosing-var $x._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._e.value
    var-kind field value
    enclosing-var $x._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._e
    var-kind field _e
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m._f
    var-kind field _f
    enclosing-var $x._m
    dec-type int
    rep-type int
    comparability 1
  variable $x._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m.sign
    var-kind field sign
    enclosing-var $x._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._m.value
    var-kind field value
    enclosing-var $x._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m
    var-kind field _m
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._p
    var-kind field _p
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._p.is_defined
    var-kind field is_defined
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._p.is_empty
    var-kind field is_empty
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x.sign
    var-kind field sign
    enclosing-var $x
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable return._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._f
    var-kind field _f
    enclosing-var return._e
    dec-type int
    rep-type int
    comparability 1
  variable return._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e.sign
    var-kind field sign
    enclosing-var return._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._e.value
    var-kind field value
    enclosing-var return._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._e
    var-kind field _e
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m._f
    var-kind field _f
    enclosing-var return._m
    dec-type int
    rep-type int
    comparability 1
  variable return._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m.sign
    var-kind field sign
    enclosing-var return._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._m.value
    var-kind field value
    enclosing-var return._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m
    var-kind field _m
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._p
    var-kind field _p
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return.sign
    var-kind field sign
    enclosing-var return
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return
    var-kind return
    dec-type Object
    rep-type hashcode
    comparability 22

ppt Math::BigFloat.bfround_s():::EXIT135
  ppt-type subexit
  variable $x._a
    var-kind field _a
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._a.is_defined
    var-kind field is_defined
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._a.is_empty
    var-kind field is_empty
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._f
    var-kind field _f
    enclosing-var $x._e
    dec-type int
    rep-type int
    comparability 1
  variable $x._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e.sign
    var-kind field sign
    enclosing-var $x._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._e.value
    var-kind field value
    enclosing-var $x._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._e
    var-kind field _e
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m._f
    var-kind field _f
    enclosing-var $x._m
    dec-type int
    rep-type int
    comparability 1
  variable $x._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m.sign
    var-kind field sign
    enclosing-var $x._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._m.value
    var-kind field value
    enclosing-var $x._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m
    var-kind field _m
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._p
    var-kind field _p
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._p.is_defined
    var-kind field is_defined
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._p.is_empty
    var-kind field is_empty
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x.sign
    var-kind field sign
    enclosing-var $x
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable return._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._f
    var-kind field _f
    enclosing-var return._e
    dec-type int
    rep-type int
    comparability 1
  variable return._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e.sign
    var-kind field sign
    enclosing-var return._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._e.value
    var-kind field value
    enclosing-var return._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._e
    var-kind field _e
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m._f
    var-kind field _f
    enclosing-var return._m
    dec-type int
    rep-type int
    comparability 1
  variable return._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m.sign
    var-kind field sign
    enclosing-var return._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._m.value
    var-kind field value
    enclosing-var return._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m
    var-kind field _m
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._p
    var-kind field _p
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return.sign
    var-kind field sign
    enclosing-var return
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return
    var-kind return
    dec-type Object
    rep-type hashcode
    comparability 22

ppt Math::BigFloat.bfround_s():::EXIT186
  ppt-type subexit
  variable $x._a
    var-kind field _a
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._a.is_defined
    var-kind field is_defined
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._a.is_empty
    var-kind field is_empty
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._f
    var-kind field _f
    enclosing-var $x._e
    dec-type int
    rep-type int
    comparability 1
  variable $x._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e.sign
    var-kind field sign
    enclosing-var $x._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._e.value
    var-kind field value
    enclosing-var $x._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._e
    var-kind field _e
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m._f
    var-kind field _f
    enclosing-var $x._m
    dec-type int
    rep-type int
    comparability 1
  variable $x._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m.sign
    var-kind field sign
    enclosing-var $x._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._m.value
    var-kind field value
    enclosing-var $x._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m
    var-kind field _m
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._p
    var-kind field _p
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._p.is_defined
    var-kind field is_defined
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._p.is_empty
    var-kind field is_empty
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x.sign
    var-kind field sign
    enclosing-var $x
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable return._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._f
    var-kind field _f
    enclosing-var return._e
    dec-type int
    rep-type int
    comparability 1
  variable return._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e.sign
    var-kind field sign
    enclosing-var return._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._e.value
    var-kind field value
    enclosing-var return._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._e
    var-kind field _e
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m._f
    var-kind field _f
    enclosing-var return._m
    dec-type int
    rep-type int
    comparability 1
  variable return._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m.sign
    var-kind field sign
    enclosing-var return._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._m.value
    var-kind field value
    enclosing-var return._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m
    var-kind field _m
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._p
    var-kind field _p
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return.sign
    var-kind field sign
    enclosing-var return
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return
    var-kind return
    dec-type Object
    rep-type hashcode
    comparability 22

ppt Math::BigFloat.bfround_s():::EXIT197
  ppt-type subexit
  variable $x._a
    var-kind field _a
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._a.is_defined
    var-kind field is_defined
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._a.is_empty
    var-kind field is_empty
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._f
    var-kind field _f
    enclosing-var $x._e
    dec-type int
    rep-type int
    comparability 1
  variable $x._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e.sign
    var-kind field sign
    enclosing-var $x._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._e.value
    var-kind field value
    enclosing-var $x._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._e
    var-kind field _e
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m._f
    var-kind field _f
    enclosing-var $x._m
    dec-type int
    rep-type int
    comparability 1
  variable $x._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m.sign
    var-kind field sign
    enclosing-var $x._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._m.value
    var-kind field value
    enclosing-var $x._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m
    var-kind field _m
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._p
    var-kind field _p
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._p.is_defined
    var-kind field is_defined
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._p.is_empty
    var-kind field is_empty
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x.sign
    var-kind field sign
    enclosing-var $x
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable return._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._f
    var-kind field _f
    enclosing-var return._e
    dec-type int
    rep-type int
    comparability 1
  variable return._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e.sign
    var-kind field sign
    enclosing-var return._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._e.value
    var-kind field value
    enclosing-var return._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._e
    var-kind field _e
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m._f
    var-kind field _f
    enclosing-var return._m
    dec-type int
    rep-type int
    comparability 1
  variable return._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m.sign
    var-kind field sign
    enclosing-var return._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._m.value
    var-kind field value
    enclosing-var return._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m
    var-kind field _m
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._p
    var-kind field _p
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return.sign
    var-kind field sign
    enclosing-var return
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return
    var-kind return
    dec-type Object
    rep-type hashcode
    comparability 22

ppt Math::BigFloat.bfround_s():::EXIT245
  ppt-type subexit
  variable $x._a
    var-kind field _a
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._a.is_defined
    var-kind field is_defined
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._a.is_empty
    var-kind field is_empty
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._f
    var-kind field _f
    enclosing-var $x._e
    dec-type int
    rep-type int
    comparability 1
  variable $x._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e.sign
    var-kind field sign
    enclosing-var $x._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._e.value
    var-kind field value
    enclosing-var $x._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._e
    var-kind field _e
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m._f
    var-kind field _f
    enclosing-var $x._m
    dec-type int
    rep-type int
    comparability 1
  variable $x._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m.sign
    var-kind field sign
    enclosing-var $x._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._m.value
    var-kind field value
    enclosing-var $x._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m
    var-kind field _m
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._p
    var-kind field _p
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._p.is_defined
    var-kind field is_defined
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._p.is_empty
    var-kind field is_empty
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x.sign
    var-kind field sign
    enclosing-var $x
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable return._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._f
    var-kind field _f
    enclosing-var return._e
    dec-type int
    rep-type int
    comparability 1
  variable return._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e.sign
    var-kind field sign
    enclosing-var return._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._e.value
    var-kind field value
    enclosing-var return._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._e
    var-kind field _e
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m._f
    var-kind field _f
    enclosing-var return._m
    dec-type int
    rep-type int
    comparability 1
  variable return._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m.sign
    var-kind field sign
    enclosing-var return._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._m.value
    var-kind field value
    enclosing-var return._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m
    var-kind field _m
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._p
    var-kind field _p
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return.sign
    var-kind field sign
    enclosing-var return
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return
    var-kind return
    dec-type Object
    rep-type hashcode
    comparability 22

ppt Math::BigFloat.bfround_s():::EXIT319
  ppt-type subexit
  variable $x._a
    var-kind field _a
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._a.is_defined
    var-kind field is_defined
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._a.is_empty
    var-kind field is_empty
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._f
    var-kind field _f
    enclosing-var $x._e
    dec-type int
    rep-type int
    comparability 1
  variable $x._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e.sign
    var-kind field sign
    enclosing-var $x._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._e.value
    var-kind field value
    enclosing-var $x._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._e
    var-kind field _e
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m._f
    var-kind field _f
    enclosing-var $x._m
    dec-type int
    rep-type int
    comparability 1
  variable $x._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m.sign
    var-kind field sign
    enclosing-var $x._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._m.value
    var-kind field value
    enclosing-var $x._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m
    var-kind field _m
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._p
    var-kind field _p
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._p.is_defined
    var-kind field is_defined
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._p.is_empty
    var-kind field is_empty
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x.sign
    var-kind field sign
    enclosing-var $x
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable return._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._f
    var-kind field _f
    enclosing-var return._e
    dec-type int
    rep-type int
    comparability 1
  variable return._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e.sign
    var-kind field sign
    enclosing-var return._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._e.value
    var-kind field value
    enclosing-var return._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._e
    var-kind field _e
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m._f
    var-kind field _f
    enclosing-var return._m
    dec-type int
    rep-type int
    comparability 1
  variable return._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m.sign
    var-kind field sign
    enclosing-var return._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._m.value
    var-kind field value
    enclosing-var return._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m
    var-kind field _m
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._p
    var-kind field _p
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return.sign
    var-kind field sign
    enclosing-var return
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return
    var-kind return
    dec-type Object
    rep-type hashcode
    comparability 22

ppt Math::BigFloat.bfround_s():::EXIT80
  ppt-type subexit
  variable $x._a
    var-kind field _a
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._a.is_defined
    var-kind field is_defined
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._a.is_empty
    var-kind field is_empty
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._f
    var-kind field _f
    enclosing-var $x._e
    dec-type int
    rep-type int
    comparability 1
  variable $x._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e.sign
    var-kind field sign
    enclosing-var $x._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._e.value
    var-kind field value
    enclosing-var $x._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._e
    var-kind field _e
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m._f
    var-kind field _f
    enclosing-var $x._m
    dec-type int
    rep-type int
    comparability 1
  variable $x._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m.sign
    var-kind field sign
    enclosing-var $x._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._m.value
    var-kind field value
    enclosing-var $x._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m
    var-kind field _m
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._p
    var-kind field _p
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._p.is_defined
    var-kind field is_defined
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._p.is_empty
    var-kind field is_empty
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x.sign
    var-kind field sign
    enclosing-var $x
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable return._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._f
    var-kind field _f
    enclosing-var return._e
    dec-type int
    rep-type int
    comparability 1
  variable return._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e.sign
    var-kind field sign
    enclosing-var return._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._e.value
    var-kind field value
    enclosing-var return._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._e
    var-kind field _e
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m._f
    var-kind field _f
    enclosing-var return._m
    dec-type int
    rep-type int
    comparability 1
  variable return._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m.sign
    var-kind field sign
    enclosing-var return._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._m.value
    var-kind field value
    enclosing-var return._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m
    var-kind field _m
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._p
    var-kind field _p
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return.sign
    var-kind field sign
    enclosing-var return
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return
    var-kind return
    dec-type Object
    rep-type hashcode
    comparability 22

ppt Math::BigFloat.binf_s():::ENTER
  ppt-type enter
  variable $self._a
    var-kind field _a
    enclosing-var $self
    dec-type int
    rep-type int
    comparability 1
  variable $self._a.is_defined
    var-kind field is_defined
    enclosing-var $self._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._a.is_empty
    var-kind field is_empty
    enclosing-var $self._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._e._f
    var-kind field _f
    enclosing-var $self._e
    dec-type int
    rep-type int
    comparability 1
  variable $self._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._e.sign
    var-kind field sign
    enclosing-var $self._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $self._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $self._e.value
    var-kind field value
    enclosing-var $self._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $self._e
    var-kind field _e
    enclosing-var $self
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $self._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._m._f
    var-kind field _f
    enclosing-var $self._m
    dec-type int
    rep-type int
    comparability 1
  variable $self._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._m.sign
    var-kind field sign
    enclosing-var $self._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $self._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $self._m.value
    var-kind field value
    enclosing-var $self._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $self._m
    var-kind field _m
    enclosing-var $self
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $self._p
    var-kind field _p
    enclosing-var $self
    dec-type int
    rep-type int
    comparability 1
  variable $self._p.is_defined
    var-kind field is_defined
    enclosing-var $self._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._p.is_empty
    var-kind field is_empty
    enclosing-var $self._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self.sign
    var-kind field sign
    enclosing-var $self
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $self
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable $sign
    var-kind variable
    dec-type String
    rep-type java.lang.String
    flags is_param
    comparability 22
  variable $sign.is_defined
    var-kind field is_defined
    enclosing-var $sign
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22

ppt Math::BigFloat.binf_s():::EXIT104
  ppt-type subexit
  variable $self._a
    var-kind field _a
    enclosing-var $self
    dec-type int
    rep-type int
    comparability 1
  variable $self._a.is_defined
    var-kind field is_defined
    enclosing-var $self._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._a.is_empty
    var-kind field is_empty
    enclosing-var $self._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._e._f
    var-kind field _f
    enclosing-var $self._e
    dec-type int
    rep-type int
    comparability 1
  variable $self._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._e.sign
    var-kind field sign
    enclosing-var $self._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $self._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $self._e.value
    var-kind field value
    enclosing-var $self._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $self._e
    var-kind field _e
    enclosing-var $self
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $self._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._m._f
    var-kind field _f
    enclosing-var $self._m
    dec-type int
    rep-type int
    comparability 1
  variable $self._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._m.sign
    var-kind field sign
    enclosing-var $self._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $self._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $self._m.value
    var-kind field value
    enclosing-var $self._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $self._m
    var-kind field _m
    enclosing-var $self
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $self._p
    var-kind field _p
    enclosing-var $self
    dec-type int
    rep-type int
    comparability 1
  variable $self._p.is_defined
    var-kind field is_defined
    enclosing-var $self._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._p.is_empty
    var-kind field is_empty
    enclosing-var $self._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self.sign
    var-kind field sign
    enclosing-var $self
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $self
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable $sign
    var-kind variable
    dec-type String
    rep-type java.lang.String
    flags is_param
    comparability 22
  variable $sign.is_defined
    var-kind field is_defined
    enclosing-var $sign
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable return._a
    var-kind field _a
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._a.is_defined
    var-kind field is_defined
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._a.is_empty
    var-kind field is_empty
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e.sign
    var-kind field sign
    enclosing-var return._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._e.value
    var-kind field value
    enclosing-var return._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._e
    var-kind field _e
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m.sign
    var-kind field sign
    enclosing-var return._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._m.value
    var-kind field value
    enclosing-var return._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m
    var-kind field _m
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._p
    var-kind field _p
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._p.is_defined
    var-kind field is_defined
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._p.is_empty
    var-kind field is_empty
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return.sign
    var-kind field sign
    enclosing-var return
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return
    var-kind return
    dec-type Object
    rep-type hashcode
    comparability 22

ppt Math::BigFloat.bmul_s():::ENTER
  ppt-type enter
  variable $self
    var-kind variable
    dec-type String
    rep-type java.lang.String
    flags is_param
    comparability 22
  variable $x._a
    var-kind field _a
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._a.is_defined
    var-kind field is_defined
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._a.is_empty
    var-kind field is_empty
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._f
    var-kind field _f
    enclosing-var $x._e
    dec-type int
    rep-type int
    comparability 1
  variable $x._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e.sign
    var-kind field sign
    enclosing-var $x._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._e.value
    var-kind field value
    enclosing-var $x._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._e
    var-kind field _e
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m._f
    var-kind field _f
    enclosing-var $x._m
    dec-type int
    rep-type int
    comparability 1
  variable $x._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m.sign
    var-kind field sign
    enclosing-var $x._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._m.value
    var-kind field value
    enclosing-var $x._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m
    var-kind field _m
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._p
    var-kind field _p
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._p.is_defined
    var-kind field is_defined
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._p.is_empty
    var-kind field is_empty
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x.sign
    var-kind field sign
    enclosing-var $x
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable $y._a
    var-kind field _a
    enclosing-var $y
    dec-type int
    rep-type int
    comparability 1
  variable $y._a.is_defined
    var-kind field is_defined
    enclosing-var $y._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._a.is_empty
    var-kind field is_empty
    enclosing-var $y._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e._f
    var-kind field _f
    enclosing-var $y._e
    dec-type int
    rep-type int
    comparability 1
  variable $y._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e.sign
    var-kind field sign
    enclosing-var $y._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $y._e.value
    var-kind field value
    enclosing-var $y._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._e
    var-kind field _e
    enclosing-var $y
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._m._f
    var-kind field _f
    enclosing-var $y._m
    dec-type int
    rep-type int
    comparability 1
  variable $y._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._m.sign
    var-kind field sign
    enclosing-var $y._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $y._m.value
    var-kind field value
    enclosing-var $y._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._m
    var-kind field _m
    enclosing-var $y
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._p
    var-kind field _p
    enclosing-var $y
    dec-type int
    rep-type int
    comparability 1
  variable $y._p.is_defined
    var-kind field is_defined
    enclosing-var $y._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._p.is_empty
    var-kind field is_empty
    enclosing-var $y._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y.sign
    var-kind field sign
    enclosing-var $y
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable $a
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 1
  variable $a.is_defined
    var-kind field is_defined
    enclosing-var $a
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $a.is_empty
    var-kind field is_empty
    enclosing-var $a
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $p
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 1
  variable $p.is_defined
    var-kind field is_defined
    enclosing-var $p
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $p.is_empty
    var-kind field is_empty
    enclosing-var $p
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $r.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22

ppt Math::BigFloat.bmul_s():::EXIT105
  ppt-type subexit
  variable $self
    var-kind variable
    dec-type String
    rep-type java.lang.String
    flags is_param
    comparability 22
  variable $x._a
    var-kind field _a
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._a.is_defined
    var-kind field is_defined
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._a.is_empty
    var-kind field is_empty
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._f
    var-kind field _f
    enclosing-var $x._e
    dec-type int
    rep-type int
    comparability 1
  variable $x._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e.sign
    var-kind field sign
    enclosing-var $x._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._e.value
    var-kind field value
    enclosing-var $x._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._e
    var-kind field _e
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m._f
    var-kind field _f
    enclosing-var $x._m
    dec-type int
    rep-type int
    comparability 1
  variable $x._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m.sign
    var-kind field sign
    enclosing-var $x._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._m.value
    var-kind field value
    enclosing-var $x._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m
    var-kind field _m
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._p
    var-kind field _p
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._p.is_defined
    var-kind field is_defined
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._p.is_empty
    var-kind field is_empty
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x.sign
    var-kind field sign
    enclosing-var $x
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable $y._a
    var-kind field _a
    enclosing-var $y
    dec-type int
    rep-type int
    comparability 1
  variable $y._a.is_defined
    var-kind field is_defined
    enclosing-var $y._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._a.is_empty
    var-kind field is_empty
    enclosing-var $y._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e._f
    var-kind field _f
    enclosing-var $y._e
    dec-type int
    rep-type int
    comparability 1
  variable $y._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e.sign
    var-kind field sign
    enclosing-var $y._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $y._e.value
    var-kind field value
    enclosing-var $y._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._e
    var-kind field _e
    enclosing-var $y
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._m._f
    var-kind field _f
    enclosing-var $y._m
    dec-type int
    rep-type int
    comparability 1
  variable $y._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._m.sign
    var-kind field sign
    enclosing-var $y._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $y._m.value
    var-kind field value
    enclosing-var $y._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._m
    var-kind field _m
    enclosing-var $y
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._p
    var-kind field _p
    enclosing-var $y
    dec-type int
    rep-type int
    comparability 1
  variable $y._p.is_defined
    var-kind field is_defined
    enclosing-var $y._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._p.is_empty
    var-kind field is_empty
    enclosing-var $y._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y.sign
    var-kind field sign
    enclosing-var $y
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable $a
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 1
  variable $a.is_defined
    var-kind field is_defined
    enclosing-var $a
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $a.is_empty
    var-kind field is_empty
    enclosing-var $a
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $p
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 1
  variable $p.is_defined
    var-kind field is_defined
    enclosing-var $p
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $p.is_empty
    var-kind field is_empty
    enclosing-var $p
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $r.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable return._a
    var-kind field _a
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._a.is_defined
    var-kind field is_defined
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._a.is_empty
    var-kind field is_empty
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._f
    var-kind field _f
    enclosing-var return._e
    dec-type int
    rep-type int
    comparability 1
  variable return._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e.sign
    var-kind field sign
    enclosing-var return._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._e.value
    var-kind field value
    enclosing-var return._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._e
    var-kind field _e
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m._f
    var-kind field _f
    enclosing-var return._m
    dec-type int
    rep-type int
    comparability 1
  variable return._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m.sign
    var-kind field sign
    enclosing-var return._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._m.value
    var-kind field value
    enclosing-var return._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m
    var-kind field _m
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._p
    var-kind field _p
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._p.is_defined
    var-kind field is_defined
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._p.is_empty
    var-kind field is_empty
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return.sign
    var-kind field sign
    enclosing-var return
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return
    var-kind return
    dec-type Object
    rep-type hashcode
    comparability 22

ppt Math::BigFloat.bmul_s():::EXIT113
  ppt-type subexit
  variable $self
    var-kind variable
    dec-type String
    rep-type java.lang.String
    flags is_param
    comparability 22
  variable $x._a
    var-kind field _a
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._a.is_defined
    var-kind field is_defined
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._a.is_empty
    var-kind field is_empty
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._f
    var-kind field _f
    enclosing-var $x._e
    dec-type int
    rep-type int
    comparability 1
  variable $x._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e.sign
    var-kind field sign
    enclosing-var $x._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._e.value
    var-kind field value
    enclosing-var $x._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._e
    var-kind field _e
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m._f
    var-kind field _f
    enclosing-var $x._m
    dec-type int
    rep-type int
    comparability 1
  variable $x._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m.sign
    var-kind field sign
    enclosing-var $x._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._m.value
    var-kind field value
    enclosing-var $x._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m
    var-kind field _m
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._p
    var-kind field _p
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._p.is_defined
    var-kind field is_defined
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._p.is_empty
    var-kind field is_empty
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x.sign
    var-kind field sign
    enclosing-var $x
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable $y._a
    var-kind field _a
    enclosing-var $y
    dec-type int
    rep-type int
    comparability 1
  variable $y._a.is_defined
    var-kind field is_defined
    enclosing-var $y._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._a.is_empty
    var-kind field is_empty
    enclosing-var $y._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e._f
    var-kind field _f
    enclosing-var $y._e
    dec-type int
    rep-type int
    comparability 1
  variable $y._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e.sign
    var-kind field sign
    enclosing-var $y._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $y._e.value
    var-kind field value
    enclosing-var $y._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._e
    var-kind field _e
    enclosing-var $y
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._m._f
    var-kind field _f
    enclosing-var $y._m
    dec-type int
    rep-type int
    comparability 1
  variable $y._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._m.sign
    var-kind field sign
    enclosing-var $y._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $y._m.value
    var-kind field value
    enclosing-var $y._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._m
    var-kind field _m
    enclosing-var $y
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._p
    var-kind field _p
    enclosing-var $y
    dec-type int
    rep-type int
    comparability 1
  variable $y._p.is_defined
    var-kind field is_defined
    enclosing-var $y._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._p.is_empty
    var-kind field is_empty
    enclosing-var $y._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y.sign
    var-kind field sign
    enclosing-var $y
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable $a
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 1
  variable $a.is_defined
    var-kind field is_defined
    enclosing-var $a
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $a.is_empty
    var-kind field is_empty
    enclosing-var $a
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $p
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 1
  variable $p.is_defined
    var-kind field is_defined
    enclosing-var $p
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $p.is_empty
    var-kind field is_empty
    enclosing-var $p
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $r.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable return._a
    var-kind field _a
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._a.is_defined
    var-kind field is_defined
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._a.is_empty
    var-kind field is_empty
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._f
    var-kind field _f
    enclosing-var return._e
    dec-type int
    rep-type int
    comparability 1
  variable return._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e.sign
    var-kind field sign
    enclosing-var return._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._e.value
    var-kind field value
    enclosing-var return._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._e
    var-kind field _e
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m._f
    var-kind field _f
    enclosing-var return._m
    dec-type int
    rep-type int
    comparability 1
  variable return._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m.sign
    var-kind field sign
    enclosing-var return._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._m.value
    var-kind field value
    enclosing-var return._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m
    var-kind field _m
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._p
    var-kind field _p
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._p.is_defined
    var-kind field is_defined
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._p.is_empty
    var-kind field is_empty
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return.sign
    var-kind field sign
    enclosing-var return
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return
    var-kind return
    dec-type Object
    rep-type hashcode
    comparability 22

ppt Math::BigFloat.bmul_s():::EXIT169
  ppt-type subexit
  variable $self
    var-kind variable
    dec-type String
    rep-type java.lang.String
    flags is_param
    comparability 22
  variable $x._a
    var-kind field _a
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._a.is_defined
    var-kind field is_defined
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._a.is_empty
    var-kind field is_empty
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._f
    var-kind field _f
    enclosing-var $x._e
    dec-type int
    rep-type int
    comparability 1
  variable $x._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e.sign
    var-kind field sign
    enclosing-var $x._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._e.value
    var-kind field value
    enclosing-var $x._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._e
    var-kind field _e
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m._f
    var-kind field _f
    enclosing-var $x._m
    dec-type int
    rep-type int
    comparability 1
  variable $x._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m.sign
    var-kind field sign
    enclosing-var $x._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._m.value
    var-kind field value
    enclosing-var $x._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m
    var-kind field _m
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._p
    var-kind field _p
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._p.is_defined
    var-kind field is_defined
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._p.is_empty
    var-kind field is_empty
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x.sign
    var-kind field sign
    enclosing-var $x
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable $y._a
    var-kind field _a
    enclosing-var $y
    dec-type int
    rep-type int
    comparability 1
  variable $y._a.is_defined
    var-kind field is_defined
    enclosing-var $y._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._a.is_empty
    var-kind field is_empty
    enclosing-var $y._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e._f
    var-kind field _f
    enclosing-var $y._e
    dec-type int
    rep-type int
    comparability 1
  variable $y._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e.sign
    var-kind field sign
    enclosing-var $y._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $y._e.value
    var-kind field value
    enclosing-var $y._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._e
    var-kind field _e
    enclosing-var $y
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._m._f
    var-kind field _f
    enclosing-var $y._m
    dec-type int
    rep-type int
    comparability 1
  variable $y._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._m.sign
    var-kind field sign
    enclosing-var $y._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $y._m.value
    var-kind field value
    enclosing-var $y._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._m
    var-kind field _m
    enclosing-var $y
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._p
    var-kind field _p
    enclosing-var $y
    dec-type int
    rep-type int
    comparability 1
  variable $y._p.is_defined
    var-kind field is_defined
    enclosing-var $y._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._p.is_empty
    var-kind field is_empty
    enclosing-var $y._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y.sign
    var-kind field sign
    enclosing-var $y
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable $a
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 1
  variable $a.is_defined
    var-kind field is_defined
    enclosing-var $a
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $a.is_empty
    var-kind field is_empty
    enclosing-var $a
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $p
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 1
  variable $p.is_defined
    var-kind field is_defined
    enclosing-var $p
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $p.is_empty
    var-kind field is_empty
    enclosing-var $p
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $r.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable return._a
    var-kind field _a
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._a.is_defined
    var-kind field is_defined
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._a.is_empty
    var-kind field is_empty
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._f
    var-kind field _f
    enclosing-var return._e
    dec-type int
    rep-type int
    comparability 1
  variable return._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e.sign
    var-kind field sign
    enclosing-var return._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._e.value
    var-kind field value
    enclosing-var return._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._e
    var-kind field _e
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m._f
    var-kind field _f
    enclosing-var return._m
    dec-type int
    rep-type int
    comparability 1
  variable return._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m.sign
    var-kind field sign
    enclosing-var return._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._m.value
    var-kind field value
    enclosing-var return._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m
    var-kind field _m
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._p
    var-kind field _p
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._p.is_defined
    var-kind field is_defined
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._p.is_empty
    var-kind field is_empty
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return.sign
    var-kind field sign
    enclosing-var return
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return
    var-kind return
    dec-type Object
    rep-type hashcode
    comparability 22

ppt Math::BigFloat.bmul_s():::EXIT53
  ppt-type subexit
  variable $self
    var-kind variable
    dec-type String
    rep-type java.lang.String
    flags is_param
    comparability 22
  variable $x._a
    var-kind field _a
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._a.is_defined
    var-kind field is_defined
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._a.is_empty
    var-kind field is_empty
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._f
    var-kind field _f
    enclosing-var $x._e
    dec-type int
    rep-type int
    comparability 1
  variable $x._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e.sign
    var-kind field sign
    enclosing-var $x._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._e.value
    var-kind field value
    enclosing-var $x._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._e
    var-kind field _e
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m._f
    var-kind field _f
    enclosing-var $x._m
    dec-type int
    rep-type int
    comparability 1
  variable $x._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m.sign
    var-kind field sign
    enclosing-var $x._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._m.value
    var-kind field value
    enclosing-var $x._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m
    var-kind field _m
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._p
    var-kind field _p
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._p.is_defined
    var-kind field is_defined
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._p.is_empty
    var-kind field is_empty
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x.sign
    var-kind field sign
    enclosing-var $x
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable $y._a
    var-kind field _a
    enclosing-var $y
    dec-type int
    rep-type int
    comparability 1
  variable $y._a.is_defined
    var-kind field is_defined
    enclosing-var $y._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._a.is_empty
    var-kind field is_empty
    enclosing-var $y._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e._f
    var-kind field _f
    enclosing-var $y._e
    dec-type int
    rep-type int
    comparability 1
  variable $y._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e.sign
    var-kind field sign
    enclosing-var $y._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $y._e.value
    var-kind field value
    enclosing-var $y._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._e
    var-kind field _e
    enclosing-var $y
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._m._f
    var-kind field _f
    enclosing-var $y._m
    dec-type int
    rep-type int
    comparability 1
  variable $y._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._m.sign
    var-kind field sign
    enclosing-var $y._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $y._m.value
    var-kind field value
    enclosing-var $y._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._m
    var-kind field _m
    enclosing-var $y
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._p
    var-kind field _p
    enclosing-var $y
    dec-type int
    rep-type int
    comparability 1
  variable $y._p.is_defined
    var-kind field is_defined
    enclosing-var $y._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._p.is_empty
    var-kind field is_empty
    enclosing-var $y._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y.sign
    var-kind field sign
    enclosing-var $y
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable $a
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 1
  variable $a.is_defined
    var-kind field is_defined
    enclosing-var $a
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $a.is_empty
    var-kind field is_empty
    enclosing-var $a
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $p
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 1
  variable $p.is_defined
    var-kind field is_defined
    enclosing-var $p
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $p.is_empty
    var-kind field is_empty
    enclosing-var $p
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $r.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable return._a
    var-kind field _a
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._a.is_defined
    var-kind field is_defined
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._a.is_empty
    var-kind field is_empty
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._f
    var-kind field _f
    enclosing-var return._e
    dec-type int
    rep-type int
    comparability 1
  variable return._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e.sign
    var-kind field sign
    enclosing-var return._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._e.value
    var-kind field value
    enclosing-var return._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._e
    var-kind field _e
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m._f
    var-kind field _f
    enclosing-var return._m
    dec-type int
    rep-type int
    comparability 1
  variable return._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m.sign
    var-kind field sign
    enclosing-var return._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._m.value
    var-kind field value
    enclosing-var return._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m
    var-kind field _m
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._p
    var-kind field _p
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._p.is_defined
    var-kind field is_defined
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._p.is_empty
    var-kind field is_empty
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return.sign
    var-kind field sign
    enclosing-var return
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return
    var-kind return
    dec-type Object
    rep-type hashcode
    comparability 22

ppt Math::BigFloat.bmul_s():::EXIT86
  ppt-type subexit
  variable $self
    var-kind variable
    dec-type String
    rep-type java.lang.String
    flags is_param
    comparability 22
  variable $x._a
    var-kind field _a
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._a.is_defined
    var-kind field is_defined
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._a.is_empty
    var-kind field is_empty
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._f
    var-kind field _f
    enclosing-var $x._e
    dec-type int
    rep-type int
    comparability 1
  variable $x._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e.sign
    var-kind field sign
    enclosing-var $x._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._e.value
    var-kind field value
    enclosing-var $x._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._e
    var-kind field _e
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m._f
    var-kind field _f
    enclosing-var $x._m
    dec-type int
    rep-type int
    comparability 1
  variable $x._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m.sign
    var-kind field sign
    enclosing-var $x._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._m.value
    var-kind field value
    enclosing-var $x._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m
    var-kind field _m
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._p
    var-kind field _p
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._p.is_defined
    var-kind field is_defined
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._p.is_empty
    var-kind field is_empty
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x.sign
    var-kind field sign
    enclosing-var $x
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable $y._a
    var-kind field _a
    enclosing-var $y
    dec-type int
    rep-type int
    comparability 1
  variable $y._a.is_defined
    var-kind field is_defined
    enclosing-var $y._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._a.is_empty
    var-kind field is_empty
    enclosing-var $y._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e._f
    var-kind field _f
    enclosing-var $y._e
    dec-type int
    rep-type int
    comparability 1
  variable $y._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._e.sign
    var-kind field sign
    enclosing-var $y._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $y._e.value
    var-kind field value
    enclosing-var $y._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._e
    var-kind field _e
    enclosing-var $y
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._m._f
    var-kind field _f
    enclosing-var $y._m
    dec-type int
    rep-type int
    comparability 1
  variable $y._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._m.sign
    var-kind field sign
    enclosing-var $y._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $y._m.value
    var-kind field value
    enclosing-var $y._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._m
    var-kind field _m
    enclosing-var $y
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $y._p
    var-kind field _p
    enclosing-var $y
    dec-type int
    rep-type int
    comparability 1
  variable $y._p.is_defined
    var-kind field is_defined
    enclosing-var $y._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y._p.is_empty
    var-kind field is_empty
    enclosing-var $y._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $y.sign
    var-kind field sign
    enclosing-var $y
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $y
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable $a
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 1
  variable $a.is_defined
    var-kind field is_defined
    enclosing-var $a
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $a.is_empty
    var-kind field is_empty
    enclosing-var $a
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $p
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 1
  variable $p.is_defined
    var-kind field is_defined
    enclosing-var $p
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $p.is_empty
    var-kind field is_empty
    enclosing-var $p
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable $r.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    flags is_param
    comparability 22
  variable return._a
    var-kind field _a
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._a.is_defined
    var-kind field is_defined
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._a.is_empty
    var-kind field is_empty
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._f
    var-kind field _f
    enclosing-var return._e
    dec-type int
    rep-type int
    comparability 1
  variable return._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e.sign
    var-kind field sign
    enclosing-var return._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._e.value
    var-kind field value
    enclosing-var return._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._e
    var-kind field _e
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m._f
    var-kind field _f
    enclosing-var return._m
    dec-type int
    rep-type int
    comparability 1
  variable return._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m.sign
    var-kind field sign
    enclosing-var return._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._m.value
    var-kind field value
    enclosing-var return._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m
    var-kind field _m
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._p
    var-kind field _p
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._p.is_defined
    var-kind field is_defined
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._p.is_empty
    var-kind field is_empty
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return.sign
    var-kind field sign
    enclosing-var return
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return
    var-kind return
    dec-type Object
    rep-type hashcode
    comparability 22

ppt Math::BigFloat.bnan_s():::ENTER
  ppt-type enter
  variable $self._a
    var-kind field _a
    enclosing-var $self
    dec-type int
    rep-type int
    comparability 1
  variable $self._a.is_defined
    var-kind field is_defined
    enclosing-var $self._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._a.is_empty
    var-kind field is_empty
    enclosing-var $self._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._e._f
    var-kind field _f
    enclosing-var $self._e
    dec-type int
    rep-type int
    comparability 1
  variable $self._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._e.sign
    var-kind field sign
    enclosing-var $self._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $self._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $self._e.value
    var-kind field value
    enclosing-var $self._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $self._e
    var-kind field _e
    enclosing-var $self
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $self._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._m._f
    var-kind field _f
    enclosing-var $self._m
    dec-type int
    rep-type int
    comparability 1
  variable $self._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._m.sign
    var-kind field sign
    enclosing-var $self._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $self._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $self._m.value
    var-kind field value
    enclosing-var $self._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $self._m
    var-kind field _m
    enclosing-var $self
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $self._p
    var-kind field _p
    enclosing-var $self
    dec-type int
    rep-type int
    comparability 1
  variable $self._p.is_defined
    var-kind field is_defined
    enclosing-var $self._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._p.is_empty
    var-kind field is_empty
    enclosing-var $self._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self.sign
    var-kind field sign
    enclosing-var $self
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $self
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22

ppt Math::BigFloat.bnan_s():::EXIT84
  ppt-type subexit
  variable $self._a
    var-kind field _a
    enclosing-var $self
    dec-type int
    rep-type int
    comparability 1
  variable $self._a.is_defined
    var-kind field is_defined
    enclosing-var $self._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._a.is_empty
    var-kind field is_empty
    enclosing-var $self._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._e._f
    var-kind field _f
    enclosing-var $self._e
    dec-type int
    rep-type int
    comparability 1
  variable $self._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._e.sign
    var-kind field sign
    enclosing-var $self._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $self._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $self._e.value
    var-kind field value
    enclosing-var $self._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $self._e
    var-kind field _e
    enclosing-var $self
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $self._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._m._f
    var-kind field _f
    enclosing-var $self._m
    dec-type int
    rep-type int
    comparability 1
  variable $self._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._m.sign
    var-kind field sign
    enclosing-var $self._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $self._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $self._m.value
    var-kind field value
    enclosing-var $self._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $self._m
    var-kind field _m
    enclosing-var $self
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $self._p
    var-kind field _p
    enclosing-var $self
    dec-type int
    rep-type int
    comparability 1
  variable $self._p.is_defined
    var-kind field is_defined
    enclosing-var $self._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._p.is_empty
    var-kind field is_empty
    enclosing-var $self._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self.sign
    var-kind field sign
    enclosing-var $self
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $self
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable return._a
    var-kind field _a
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._a.is_defined
    var-kind field is_defined
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._a.is_empty
    var-kind field is_empty
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e.sign
    var-kind field sign
    enclosing-var return._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._e.value
    var-kind field value
    enclosing-var return._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._e
    var-kind field _e
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m.sign
    var-kind field sign
    enclosing-var return._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._m.value
    var-kind field value
    enclosing-var return._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m
    var-kind field _m
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._p
    var-kind field _p
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._p.is_defined
    var-kind field is_defined
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._p.is_empty
    var-kind field is_empty
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return.sign
    var-kind field sign
    enclosing-var return
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return
    var-kind return
    dec-type Object
    rep-type hashcode
    comparability 22

ppt Math::BigFloat.bnorm_s():::ENTER
  ppt-type enter

ppt Math::BigFloat.bnorm_s():::EXIT146
  ppt-type subexit
  variable return._a
    var-kind field _a
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._a.is_defined
    var-kind field is_defined
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._a.is_empty
    var-kind field is_empty
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._f
    var-kind field _f
    enclosing-var return._e
    dec-type int
    rep-type int
    comparability 1
  variable return._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e.sign
    var-kind field sign
    enclosing-var return._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._e.value
    var-kind field value
    enclosing-var return._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._e
    var-kind field _e
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m._f
    var-kind field _f
    enclosing-var return._m
    dec-type int
    rep-type int
    comparability 1
  variable return._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m.sign
    var-kind field sign
    enclosing-var return._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._m.value
    var-kind field value
    enclosing-var return._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m
    var-kind field _m
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._p
    var-kind field _p
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._p.is_defined
    var-kind field is_defined
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._p.is_empty
    var-kind field is_empty
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return.sign
    var-kind field sign
    enclosing-var return
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return
    var-kind return
    dec-type Object
    rep-type hashcode
    comparability 22

ppt Math::BigFloat.bnorm_s():::EXIT24
  ppt-type subexit
  variable return._a
    var-kind field _a
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._a.is_defined
    var-kind field is_defined
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._a.is_empty
    var-kind field is_empty
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._f
    var-kind field _f
    enclosing-var return._e
    dec-type int
    rep-type int
    comparability 1
  variable return._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e.sign
    var-kind field sign
    enclosing-var return._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._e.value
    var-kind field value
    enclosing-var return._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._e
    var-kind field _e
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m._f
    var-kind field _f
    enclosing-var return._m
    dec-type int
    rep-type int
    comparability 1
  variable return._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m.sign
    var-kind field sign
    enclosing-var return._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._m.value
    var-kind field value
    enclosing-var return._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m
    var-kind field _m
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._p
    var-kind field _p
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._p.is_defined
    var-kind field is_defined
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._p.is_empty
    var-kind field is_empty
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return.sign
    var-kind field sign
    enclosing-var return
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return
    var-kind return
    dec-type Object
    rep-type hashcode
    comparability 22

ppt Math::BigFloat.bround_s():::ENTER
  ppt-type enter
  variable $x._a
    var-kind field _a
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._a.is_defined
    var-kind field is_defined
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._a.is_empty
    var-kind field is_empty
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._f
    var-kind field _f
    enclosing-var $x._e
    dec-type int
    rep-type int
    comparability 1
  variable $x._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e.sign
    var-kind field sign
    enclosing-var $x._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._e.value
    var-kind field value
    enclosing-var $x._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._e
    var-kind field _e
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m._f
    var-kind field _f
    enclosing-var $x._m
    dec-type int
    rep-type int
    comparability 1
  variable $x._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m.sign
    var-kind field sign
    enclosing-var $x._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._m.value
    var-kind field value
    enclosing-var $x._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m
    var-kind field _m
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._p
    var-kind field _p
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._p.is_defined
    var-kind field is_defined
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._p.is_empty
    var-kind field is_empty
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x.sign
    var-kind field sign
    enclosing-var $x
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22

ppt Math::BigFloat.bround_s():::EXIT118
  ppt-type subexit
  variable $x._a
    var-kind field _a
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._a.is_defined
    var-kind field is_defined
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._a.is_empty
    var-kind field is_empty
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._f
    var-kind field _f
    enclosing-var $x._e
    dec-type int
    rep-type int
    comparability 1
  variable $x._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e.sign
    var-kind field sign
    enclosing-var $x._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._e.value
    var-kind field value
    enclosing-var $x._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._e
    var-kind field _e
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m._f
    var-kind field _f
    enclosing-var $x._m
    dec-type int
    rep-type int
    comparability 1
  variable $x._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m.sign
    var-kind field sign
    enclosing-var $x._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._m.value
    var-kind field value
    enclosing-var $x._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m
    var-kind field _m
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._p
    var-kind field _p
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._p.is_defined
    var-kind field is_defined
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._p.is_empty
    var-kind field is_empty
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x.sign
    var-kind field sign
    enclosing-var $x
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable return._a
    var-kind field _a
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._a.is_defined
    var-kind field is_defined
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._a.is_empty
    var-kind field is_empty
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._f
    var-kind field _f
    enclosing-var return._e
    dec-type int
    rep-type int
    comparability 1
  variable return._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e.sign
    var-kind field sign
    enclosing-var return._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._e.value
    var-kind field value
    enclosing-var return._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._e
    var-kind field _e
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m._f
    var-kind field _f
    enclosing-var return._m
    dec-type int
    rep-type int
    comparability 1
  variable return._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m.sign
    var-kind field sign
    enclosing-var return._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._m.value
    var-kind field value
    enclosing-var return._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m
    var-kind field _m
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._p
    var-kind field _p
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._p.is_defined
    var-kind field is_defined
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._p.is_empty
    var-kind field is_empty
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return.sign
    var-kind field sign
    enclosing-var return
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return
    var-kind return
    dec-type Object
    rep-type hashcode
    comparability 22

ppt Math::BigFloat.bround_s():::EXIT149
  ppt-type subexit
  variable $x._a
    var-kind field _a
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._a.is_defined
    var-kind field is_defined
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._a.is_empty
    var-kind field is_empty
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._f
    var-kind field _f
    enclosing-var $x._e
    dec-type int
    rep-type int
    comparability 1
  variable $x._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e.sign
    var-kind field sign
    enclosing-var $x._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._e.value
    var-kind field value
    enclosing-var $x._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._e
    var-kind field _e
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m._f
    var-kind field _f
    enclosing-var $x._m
    dec-type int
    rep-type int
    comparability 1
  variable $x._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m.sign
    var-kind field sign
    enclosing-var $x._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._m.value
    var-kind field value
    enclosing-var $x._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m
    var-kind field _m
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._p
    var-kind field _p
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._p.is_defined
    var-kind field is_defined
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._p.is_empty
    var-kind field is_empty
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x.sign
    var-kind field sign
    enclosing-var $x
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable return._a
    var-kind field _a
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._a.is_defined
    var-kind field is_defined
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._a.is_empty
    var-kind field is_empty
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._f
    var-kind field _f
    enclosing-var return._e
    dec-type int
    rep-type int
    comparability 1
  variable return._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e.sign
    var-kind field sign
    enclosing-var return._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._e.value
    var-kind field value
    enclosing-var return._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._e
    var-kind field _e
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m._f
    var-kind field _f
    enclosing-var return._m
    dec-type int
    rep-type int
    comparability 1
  variable return._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m.sign
    var-kind field sign
    enclosing-var return._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._m.value
    var-kind field value
    enclosing-var return._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m
    var-kind field _m
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._p
    var-kind field _p
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._p.is_defined
    var-kind field is_defined
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._p.is_empty
    var-kind field is_empty
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return.sign
    var-kind field sign
    enclosing-var return
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return
    var-kind return
    dec-type Object
    rep-type hashcode
    comparability 22

ppt Math::BigFloat.bround_s():::EXIT201
  ppt-type subexit
  variable $x._a
    var-kind field _a
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._a.is_defined
    var-kind field is_defined
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._a.is_empty
    var-kind field is_empty
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._f
    var-kind field _f
    enclosing-var $x._e
    dec-type int
    rep-type int
    comparability 1
  variable $x._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e.sign
    var-kind field sign
    enclosing-var $x._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._e.value
    var-kind field value
    enclosing-var $x._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._e
    var-kind field _e
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m._f
    var-kind field _f
    enclosing-var $x._m
    dec-type int
    rep-type int
    comparability 1
  variable $x._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m.sign
    var-kind field sign
    enclosing-var $x._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._m.value
    var-kind field value
    enclosing-var $x._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m
    var-kind field _m
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._p
    var-kind field _p
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._p.is_defined
    var-kind field is_defined
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._p.is_empty
    var-kind field is_empty
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x.sign
    var-kind field sign
    enclosing-var $x
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable return._a
    var-kind field _a
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._a.is_defined
    var-kind field is_defined
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._a.is_empty
    var-kind field is_empty
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._f
    var-kind field _f
    enclosing-var return._e
    dec-type int
    rep-type int
    comparability 1
  variable return._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e.sign
    var-kind field sign
    enclosing-var return._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._e.value
    var-kind field value
    enclosing-var return._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._e
    var-kind field _e
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m._f
    var-kind field _f
    enclosing-var return._m
    dec-type int
    rep-type int
    comparability 1
  variable return._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m.sign
    var-kind field sign
    enclosing-var return._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._m.value
    var-kind field value
    enclosing-var return._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m
    var-kind field _m
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._p
    var-kind field _p
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._p.is_defined
    var-kind field is_defined
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._p.is_empty
    var-kind field is_empty
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return.sign
    var-kind field sign
    enclosing-var return
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return
    var-kind return
    dec-type Object
    rep-type hashcode
    comparability 22

ppt Math::BigFloat.bround_s():::EXIT94
  ppt-type subexit
  variable $x._a
    var-kind field _a
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._a.is_defined
    var-kind field is_defined
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._a.is_empty
    var-kind field is_empty
    enclosing-var $x._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e._f
    var-kind field _f
    enclosing-var $x._e
    dec-type int
    rep-type int
    comparability 1
  variable $x._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._e.sign
    var-kind field sign
    enclosing-var $x._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._e.value
    var-kind field value
    enclosing-var $x._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._e
    var-kind field _e
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m._f
    var-kind field _f
    enclosing-var $x._m
    dec-type int
    rep-type int
    comparability 1
  variable $x._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._m.sign
    var-kind field sign
    enclosing-var $x._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $x._m.value
    var-kind field value
    enclosing-var $x._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._m
    var-kind field _m
    enclosing-var $x
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $x._p
    var-kind field _p
    enclosing-var $x
    dec-type int
    rep-type int
    comparability 1
  variable $x._p.is_defined
    var-kind field is_defined
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x._p.is_empty
    var-kind field is_empty
    enclosing-var $x._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $x.sign
    var-kind field sign
    enclosing-var $x
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $x
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable return._a
    var-kind field _a
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._a.is_defined
    var-kind field is_defined
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._a.is_empty
    var-kind field is_empty
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._f
    var-kind field _f
    enclosing-var return._e
    dec-type int
    rep-type int
    comparability 1
  variable return._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e.sign
    var-kind field sign
    enclosing-var return._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._e.value
    var-kind field value
    enclosing-var return._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._e
    var-kind field _e
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m._f
    var-kind field _f
    enclosing-var return._m
    dec-type int
    rep-type int
    comparability 1
  variable return._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m.sign
    var-kind field sign
    enclosing-var return._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._m.value
    var-kind field value
    enclosing-var return._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m
    var-kind field _m
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._p
    var-kind field _p
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._p.is_defined
    var-kind field is_defined
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._p.is_empty
    var-kind field is_empty
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return.sign
    var-kind field sign
    enclosing-var return
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return
    var-kind return
    dec-type Object
    rep-type hashcode
    comparability 22

ppt Math::BigFloat.bstr_s():::ENTER
  ppt-type enter

ppt Math::BigFloat.bstr_s():::EXIT221
  ppt-type subexit
  variable return
    var-kind return
    dec-type String
    rep-type java.lang.String
    comparability 22

ppt Math::BigFloat.bstr_s():::EXIT36
  ppt-type subexit
  variable return
    var-kind return
    dec-type String
    rep-type java.lang.String
    comparability 22

ppt Math::BigFloat.bstr_s():::EXIT40
  ppt-type subexit
  variable return
    var-kind return
    dec-type String
    rep-type java.lang.String
    comparability 22

ppt Math::BigFloat.bzero_s():::ENTER
  ppt-type enter
  variable $self._a
    var-kind field _a
    enclosing-var $self
    dec-type int
    rep-type int
    comparability 1
  variable $self._a.is_defined
    var-kind field is_defined
    enclosing-var $self._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._a.is_empty
    var-kind field is_empty
    enclosing-var $self._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._e._f
    var-kind field _f
    enclosing-var $self._e
    dec-type int
    rep-type int
    comparability 1
  variable $self._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._e.sign
    var-kind field sign
    enclosing-var $self._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $self._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $self._e.value
    var-kind field value
    enclosing-var $self._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $self._e
    var-kind field _e
    enclosing-var $self
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $self._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._m._f
    var-kind field _f
    enclosing-var $self._m
    dec-type int
    rep-type int
    comparability 1
  variable $self._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._m.sign
    var-kind field sign
    enclosing-var $self._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $self._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $self._m.value
    var-kind field value
    enclosing-var $self._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $self._m
    var-kind field _m
    enclosing-var $self
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $self._p
    var-kind field _p
    enclosing-var $self
    dec-type int
    rep-type int
    comparability 1
  variable $self._p.is_defined
    var-kind field is_defined
    enclosing-var $self._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._p.is_empty
    var-kind field is_empty
    enclosing-var $self._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self.sign
    var-kind field sign
    enclosing-var $self
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $self
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22

ppt Math::BigFloat.bzero_s():::EXIT84
  ppt-type subexit
  variable $self._a
    var-kind field _a
    enclosing-var $self
    dec-type int
    rep-type int
    comparability 1
  variable $self._a.is_defined
    var-kind field is_defined
    enclosing-var $self._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._a.is_empty
    var-kind field is_empty
    enclosing-var $self._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._e._f
    var-kind field _f
    enclosing-var $self._e
    dec-type int
    rep-type int
    comparability 1
  variable $self._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._e.sign
    var-kind field sign
    enclosing-var $self._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $self._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $self._e.value
    var-kind field value
    enclosing-var $self._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $self._e
    var-kind field _e
    enclosing-var $self
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $self._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._m._f
    var-kind field _f
    enclosing-var $self._m
    dec-type int
    rep-type int
    comparability 1
  variable $self._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._m.sign
    var-kind field sign
    enclosing-var $self._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $self._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable $self._m.value
    var-kind field value
    enclosing-var $self._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $self._m
    var-kind field _m
    enclosing-var $self
    dec-type Object
    rep-type hashcode
    comparability 22
  variable $self._p
    var-kind field _p
    enclosing-var $self
    dec-type int
    rep-type int
    comparability 1
  variable $self._p.is_defined
    var-kind field is_defined
    enclosing-var $self._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self._p.is_empty
    var-kind field is_empty
    enclosing-var $self._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable $self.sign
    var-kind field sign
    enclosing-var $self
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable $self
    var-kind variable
    dec-type Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable return._a
    var-kind field _a
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._a.is_defined
    var-kind field is_defined
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._a.is_empty
    var-kind field is_empty
    enclosing-var return._a
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e.sign
    var-kind field sign
    enclosing-var return._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._e.value
    var-kind field value
    enclosing-var return._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._e
    var-kind field _e
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m.sign
    var-kind field sign
    enclosing-var return._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._m.value
    var-kind field value
    enclosing-var return._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m
    var-kind field _m
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._p
    var-kind field _p
    enclosing-var return
    dec-type int
    rep-type int
    comparability 1
  variable return._p.is_defined
    var-kind field is_defined
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._p.is_empty
    var-kind field is_empty
    enclosing-var return._p
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return.sign
    var-kind field sign
    enclosing-var return
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return
    var-kind return
    dec-type Object
    rep-type hashcode
    comparability 22

ppt Math::BigFloat.import_s():::ENTER
  ppt-type enter
  variable $self
    var-kind variable
    dec-type String
    rep-type java.lang.String
    flags is_param
    comparability 22

ppt Math::BigFloat.import_s():::EXIT67
  ppt-type subexit
  variable $self
    var-kind variable
    dec-type String
    rep-type java.lang.String
    flags is_param
    comparability 22
  variable return.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22

ppt Math::BigFloat.is_one_s():::ENTER
  ppt-type enter

ppt Math::BigFloat.is_one_s():::EXIT56
  ppt-type subexit
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 22

ppt Math::BigFloat.is_one_s():::EXIT60
  ppt-type subexit
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 22

ppt Math::BigFloat.is_zero_s():::ENTER
  ppt-type enter

ppt Math::BigFloat.is_zero_s():::EXIT32
  ppt-type subexit
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 22

ppt Math::BigFloat.is_zero_s():::EXIT36
  ppt-type subexit
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 22

ppt Math::BigFloat.new_s():::ENTER
  ppt-type enter
  variable $class
    var-kind variable
    dec-type String
    rep-type java.lang.String
    flags is_param
    comparability 22
  variable $wanted
    var-kind variable
    dec-type String
    rep-type java.lang.String
    flags is_param
    comparability 22

ppt Math::BigFloat.new_s():::EXIT167
  ppt-type subexit
  variable $class
    var-kind variable
    dec-type String
    rep-type java.lang.String
    flags is_param
    comparability 22
  variable $wanted
    var-kind variable
    dec-type String
    rep-type java.lang.String
    flags is_param
    comparability 22
  variable return._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._f
    var-kind field _f
    enclosing-var return._e
    dec-type int
    rep-type int
    comparability 1
  variable return._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e.sign
    var-kind field sign
    enclosing-var return._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._e.value
    var-kind field value
    enclosing-var return._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._e
    var-kind field _e
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m._f
    var-kind field _f
    enclosing-var return._m
    dec-type int
    rep-type int
    comparability 1
  variable return._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m.sign
    var-kind field sign
    enclosing-var return._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._m.value
    var-kind field value
    enclosing-var return._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m
    var-kind field _m
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return.sign
    var-kind field sign
    enclosing-var return
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return
    var-kind return
    dec-type Object
    rep-type hashcode
    comparability 22

ppt Math::BigFloat.new_s():::EXIT253
  ppt-type subexit
  variable $class
    var-kind variable
    dec-type String
    rep-type java.lang.String
    flags is_param
    comparability 22
  variable $wanted
    var-kind variable
    dec-type String
    rep-type java.lang.String
    flags is_param
    comparability 22
  variable return._e._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e._f
    var-kind field _f
    enclosing-var return._e
    dec-type int
    rep-type int
    comparability 1
  variable return._e._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._e.sign
    var-kind field sign
    enclosing-var return._e
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._e.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._e.value
    var-kind field value
    enclosing-var return._e
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._e
    var-kind field _e
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m._a.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m._f
    var-kind field _f
    enclosing-var return._m
    dec-type int
    rep-type int
    comparability 1
  variable return._m._p.is_defined
    var-kind variable
    dec-type boolean
    rep-type boolean
    comparability 22
  variable return._m.sign
    var-kind field sign
    enclosing-var return._m
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return._m.value.deref[..]
    var-kind variable
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22[22]
  variable return._m.value
    var-kind field value
    enclosing-var return._m
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return._m
    var-kind field _m
    enclosing-var return
    dec-type Object
    rep-type hashcode
    comparability 22
  variable return.sign
    var-kind field sign
    enclosing-var return
    dec-type String
    rep-type java.lang.String
    comparability 22
  variable return
    var-kind return
    dec-type Object
    rep-type hashcode
    comparability 22
