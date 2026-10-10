decl-version 2.0

// Declaration file /scratch/mharder/tests/esc-experiments/StackAr/DataStructures/StackAr.decls rewritten by ComparablePairsDescFileReader
// Tue Feb 12 13:58:11 EST 2002

var-comparability implicit

// Declarations for DataStructures/StackAr.java
// Written Tue Feb 12 13:57:51 2002

ListImplementors
java.util.List

ppt DataStructures.StackAr.<init>(I)V:::ENTER
  ppt-type enter
  variable capacity
    var-kind variable
    dec-type int
    rep-type int
    comparability 0

ppt DataStructures.StackAr.<init>(I)V:::EXIT33
  ppt-type subexit
  parent parent DataStructures.StackAr:::OBJECT 1
  variable capacity
    var-kind variable
    dec-type int
    rep-type int
    comparability 1
  variable this.theArray
    var-kind variable
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.StackAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability 0[1]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.topOfStack
    var-kind variable
    dec-type int
    rep-type int
    parent DataStructures.StackAr:::OBJECT 1
    comparability 1

ppt DataStructures.StackAr.isEmpty()Z:::ENTER
  ppt-type enter
  parent parent DataStructures.StackAr:::OBJECT 1
  variable this.theArray
    var-kind variable
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.StackAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability 0[1]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.topOfStack
    var-kind variable
    dec-type int
    rep-type int
    parent DataStructures.StackAr:::OBJECT 1
    comparability 1

ppt DataStructures.StackAr.isEmpty()Z:::EXIT41
  ppt-type subexit
  parent parent DataStructures.StackAr:::OBJECT 1
  variable return
    var-kind return
    dec-type boolean
    rep-type boolean
    comparability 0
  variable this.theArray
    var-kind variable
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.StackAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability 1[2]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.topOfStack
    var-kind variable
    dec-type int
    rep-type int
    parent DataStructures.StackAr:::OBJECT 1
    comparability 2

ppt DataStructures.StackAr.isFull()Z:::ENTER
  ppt-type enter
  parent parent DataStructures.StackAr:::OBJECT 1
  variable this.theArray
    var-kind variable
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.StackAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability 0[1]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.topOfStack
    var-kind variable
    dec-type int
    rep-type int
    parent DataStructures.StackAr:::OBJECT 1
    comparability 1

ppt DataStructures.StackAr.isFull()Z:::EXIT50
  ppt-type subexit
  parent parent DataStructures.StackAr:::OBJECT 1
  variable return
    var-kind return
    dec-type boolean
    rep-type boolean
    comparability 0
  variable this.theArray
    var-kind variable
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.StackAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability 1[2]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.topOfStack
    var-kind variable
    dec-type int
    rep-type int
    parent DataStructures.StackAr:::OBJECT 1
    comparability 2

ppt DataStructures.StackAr.makeEmpty()V:::ENTER
  ppt-type enter
  parent parent DataStructures.StackAr:::OBJECT 1
  variable this.theArray
    var-kind variable
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.StackAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability 0[1]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.topOfStack
    var-kind variable
    dec-type int
    rep-type int
    parent DataStructures.StackAr:::OBJECT 1
    comparability 1

ppt DataStructures.StackAr.makeEmpty()V:::EXIT61
  ppt-type subexit
  parent parent DataStructures.StackAr:::OBJECT 1
  variable this.theArray
    var-kind variable
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.StackAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability 0[1]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.topOfStack
    var-kind variable
    dec-type int
    rep-type int
    parent DataStructures.StackAr:::OBJECT 1
    comparability 1

ppt DataStructures.StackAr.top()Ljava/lang/Object;:::ENTER
  ppt-type enter
  parent parent DataStructures.StackAr:::OBJECT 1
  variable this.theArray
    var-kind variable
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.StackAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability 0[1]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.topOfStack
    var-kind variable
    dec-type int
    rep-type int
    parent DataStructures.StackAr:::OBJECT 1
    comparability 1

ppt DataStructures.StackAr.top()Ljava/lang/Object;:::EXIT71
  ppt-type subexit
  parent parent DataStructures.StackAr:::OBJECT 1
  variable return
    var-kind return
    dec-type java.lang.Object
    rep-type hashcode
    comparability -2
  variable return.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var return
    dec-type java.lang.Class
    rep-type java.lang.String
    comparability -1
  variable this.theArray
    var-kind variable
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.StackAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability 0[1]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.topOfStack
    var-kind variable
    dec-type int
    rep-type int
    parent DataStructures.StackAr:::OBJECT 1
    comparability 1

ppt DataStructures.StackAr.top()Ljava/lang/Object;:::EXIT72
  ppt-type subexit
  parent parent DataStructures.StackAr:::OBJECT 1
  variable return
    var-kind return
    dec-type java.lang.Object
    rep-type hashcode
    comparability 0
  variable return.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var return
    dec-type java.lang.Class
    rep-type java.lang.String
    comparability -1
  variable this.theArray
    var-kind variable
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.StackAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability 0[1]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.topOfStack
    var-kind variable
    dec-type int
    rep-type int
    parent DataStructures.StackAr:::OBJECT 1
    comparability 1

ppt DataStructures.StackAr.pop()V:::ENTER
  ppt-type enter
  parent parent DataStructures.StackAr:::OBJECT 1
  variable this.theArray
    var-kind variable
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.StackAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability -2[-2]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.topOfStack
    var-kind variable
    dec-type int
    rep-type int
    parent DataStructures.StackAr:::OBJECT 1
    comparability -2

ppt DataStructures.StackAr.pop()V:::EXIT84
  ppt-type subexit
  parent parent DataStructures.StackAr:::OBJECT 1
  variable this.theArray
    var-kind variable
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.StackAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability -2[-2]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.topOfStack
    var-kind variable
    dec-type int
    rep-type int
    parent DataStructures.StackAr:::OBJECT 1
    comparability -2

ppt DataStructures.StackAr.push(Ljava/lang/Object;)V:::ENTER
  ppt-type enter
  parent parent DataStructures.StackAr:::OBJECT 1
  variable x
    var-kind variable
    dec-type java.lang.Object
    rep-type hashcode
    comparability 0
  variable x.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var x
    dec-type java.lang.Class
    rep-type java.lang.String
    comparability -1
  variable this.theArray
    var-kind variable
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.StackAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability 0[1]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.topOfStack
    var-kind variable
    dec-type int
    rep-type int
    parent DataStructures.StackAr:::OBJECT 1
    comparability 1

ppt DataStructures.StackAr.push(Ljava/lang/Object;)V:::EXIT96
  ppt-type subexit
  parent parent DataStructures.StackAr:::OBJECT 1
  variable x
    var-kind variable
    dec-type java.lang.Object
    rep-type hashcode
    comparability 0
  variable x.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var x
    dec-type java.lang.Class
    rep-type java.lang.String
    comparability -1
  variable this.theArray
    var-kind variable
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.StackAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability 0[1]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.topOfStack
    var-kind variable
    dec-type int
    rep-type int
    parent DataStructures.StackAr:::OBJECT 1
    comparability 1

ppt DataStructures.StackAr.topAndPop()Ljava/lang/Object;:::ENTER
  ppt-type enter
  parent parent DataStructures.StackAr:::OBJECT 1
  variable this.theArray
    var-kind variable
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.StackAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability 0[1]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.topOfStack
    var-kind variable
    dec-type int
    rep-type int
    parent DataStructures.StackAr:::OBJECT 1
    comparability 1

ppt DataStructures.StackAr.topAndPop()Ljava/lang/Object;:::EXIT105
  ppt-type subexit
  parent parent DataStructures.StackAr:::OBJECT 1
  variable return
    var-kind return
    dec-type java.lang.Object
    rep-type hashcode
    comparability -2
  variable return.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var return
    dec-type java.lang.Class
    rep-type java.lang.String
    comparability -1
  variable this.theArray
    var-kind variable
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.StackAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability 0[1]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.topOfStack
    var-kind variable
    dec-type int
    rep-type int
    parent DataStructures.StackAr:::OBJECT 1
    comparability 1

ppt DataStructures.StackAr.topAndPop()Ljava/lang/Object;:::EXIT108
  ppt-type subexit
  parent parent DataStructures.StackAr:::OBJECT 1
  variable return
    var-kind return
    dec-type java.lang.Object
    rep-type hashcode
    comparability 0
  variable return.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var return
    dec-type java.lang.Class
    rep-type java.lang.String
    comparability -1
  variable this.theArray
    var-kind variable
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.StackAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability 0[1]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.StackAr:::OBJECT 1
    comparability -1
  variable this.topOfStack
    var-kind variable
    dec-type int
    rep-type int
    parent DataStructures.StackAr:::OBJECT 1
    comparability 1

ppt DataStructures.StackAr:::OBJECT
  ppt-type object
  variable this.theArray
    var-kind variable
    dec-type java.lang.Object[]
    rep-type hashcode
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    comparability 0[1]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    comparability -1
  variable this.topOfStack
    var-kind variable
    dec-type int
    rep-type int
    comparability 1
