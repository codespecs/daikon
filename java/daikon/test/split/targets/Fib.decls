decl-version 2.0

// Declaration file /scratch/tschantz/tests/daikon-tests/fib/misc/Fib.decls rewritten by ComparablePairsDescFileReader
// Wed Jun 04 23:19:41 EDT 2003

var-comparability implicit

// Declarations for misc/Fib.java
// Written Wed Jun  4 23:18:21 2003

ListImplementors
java.util.List

ppt misc.Fib.Fib():::ENTER
  ppt-type enter

ppt misc.Fib.Fib():::EXIT5
  ppt-type subexit
  parent parent misc.Fib:::OBJECT 1
  variable misc.Fib.a
    var-kind variable
    dec-type int
    rep-type int
    parent misc.Fib:::OBJECT 1
    comparability -2
  variable misc.Fib.b
    var-kind variable
    dec-type int
    rep-type int
    parent misc.Fib:::OBJECT 1
    comparability -2
  variable misc.Fib.c
    var-kind variable
    dec-type int
    rep-type int
    parent misc.Fib:::OBJECT 1
    comparability -2
  variable misc.Fib.STEPS
    var-kind variable
    dec-type int
    rep-type int
    constant 20
    parent misc.Fib:::OBJECT 1
    comparability -2

ppt misc.Fib.main(java.lang.String[]):::ENTER
  ppt-type enter
  parent parent misc.Fib:::OBJECT 1
  variable args
    var-kind variable
    dec-type java.lang.String[]
    rep-type hashcode
    flags is_param
    comparability 3
  variable args.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var args
    dec-type java.lang.Class
    rep-type java.lang.String
    comparability -1
  variable args[..]
    var-kind array
    enclosing-var args
    array 1
    dec-type java.lang.String[]
    rep-type java.lang.String[]
    comparability 2[3]
  variable args[..].toString
    var-kind field toString
    enclosing-var args[..]
    array 1
    dec-type java.lang.String[]
    rep-type java.lang.String[]
    comparability -1
  variable misc.Fib.a
    var-kind variable
    dec-type int
    rep-type int
    parent misc.Fib:::OBJECT 1
    comparability 0
  variable misc.Fib.b
    var-kind variable
    dec-type int
    rep-type int
    parent misc.Fib:::OBJECT 1
    comparability 0
  variable misc.Fib.c
    var-kind variable
    dec-type int
    rep-type int
    parent misc.Fib:::OBJECT 1
    comparability 0
  variable misc.Fib.STEPS
    var-kind variable
    dec-type int
    rep-type int
    constant 20
    parent misc.Fib:::OBJECT 1
    comparability 1

ppt misc.Fib.main(java.lang.String[]):::EXIT18
  ppt-type subexit
  parent parent misc.Fib:::OBJECT 1
  variable args
    var-kind variable
    dec-type java.lang.String[]
    rep-type hashcode
    flags is_param
    comparability 3
  variable args.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var args
    dec-type java.lang.Class
    rep-type java.lang.String
    comparability -1
  variable args[..]
    var-kind array
    enclosing-var args
    array 1
    dec-type java.lang.String[]
    rep-type java.lang.String[]
    comparability 2[3]
  variable args[..].toString
    var-kind field toString
    enclosing-var args[..]
    array 1
    dec-type java.lang.String[]
    rep-type java.lang.String[]
    comparability -1
  variable misc.Fib.a
    var-kind variable
    dec-type int
    rep-type int
    parent misc.Fib:::OBJECT 1
    comparability 0
  variable misc.Fib.b
    var-kind variable
    dec-type int
    rep-type int
    parent misc.Fib:::OBJECT 1
    comparability 0
  variable misc.Fib.c
    var-kind variable
    dec-type int
    rep-type int
    parent misc.Fib:::OBJECT 1
    comparability 0
  variable misc.Fib.STEPS
    var-kind variable
    dec-type int
    rep-type int
    constant 20
    parent misc.Fib:::OBJECT 1
    comparability 1

ppt misc.Fib.increment():::ENTER
  ppt-type enter
  parent parent misc.Fib:::OBJECT 1
  variable misc.Fib.a
    var-kind variable
    dec-type int
    rep-type int
    parent misc.Fib:::OBJECT 1
    comparability 0
  variable misc.Fib.b
    var-kind variable
    dec-type int
    rep-type int
    parent misc.Fib:::OBJECT 1
    comparability 0
  variable misc.Fib.c
    var-kind variable
    dec-type int
    rep-type int
    parent misc.Fib:::OBJECT 1
    comparability 0
  variable misc.Fib.STEPS
    var-kind variable
    dec-type int
    rep-type int
    constant 20
    parent misc.Fib:::OBJECT 1
    comparability 1

ppt misc.Fib.increment():::EXIT25
  ppt-type subexit
  parent parent misc.Fib:::OBJECT 1
  variable misc.Fib.a
    var-kind variable
    dec-type int
    rep-type int
    parent misc.Fib:::OBJECT 1
    comparability 0
  variable misc.Fib.b
    var-kind variable
    dec-type int
    rep-type int
    parent misc.Fib:::OBJECT 1
    comparability 0
  variable misc.Fib.c
    var-kind variable
    dec-type int
    rep-type int
    parent misc.Fib:::OBJECT 1
    comparability 0
  variable misc.Fib.STEPS
    var-kind variable
    dec-type int
    rep-type int
    constant 20
    parent misc.Fib:::OBJECT 1
    comparability 1

ppt DataStructures.QueueAr.QueueAr():::ENTER
  ppt-type enter

ppt misc.Fib:::CLASS
  ppt-type class
  variable misc.Fib.a
    var-kind variable
    dec-type int
    rep-type int
    comparability -2
  variable misc.Fib.b
    var-kind variable
    dec-type int
    rep-type int
    comparability -2
  variable misc.Fib.c
    var-kind variable
    dec-type int
    rep-type int
    comparability -2
  variable misc.Fib.STEPS
    var-kind variable
    dec-type int
    rep-type int
    constant 20
    comparability -2

ppt misc.Fib:::OBJECT
  ppt-type object
  variable this
    var-kind variable
    dec-type misc.Fib
    rep-type hashcode
    flags is_param
    comparability -2
  variable misc.Fib.a
    var-kind variable
    dec-type int
    rep-type int
    comparability -2
  variable misc.Fib.b
    var-kind variable
    dec-type int
    rep-type int
    comparability -2
  variable misc.Fib.c
    var-kind variable
    dec-type int
    rep-type int
    comparability -2
  variable misc.Fib.STEPS
    var-kind variable
    dec-type int
    rep-type int
    constant 20
    comparability -2
