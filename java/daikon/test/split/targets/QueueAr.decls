decl-version 2.0

// Declaration file daikon-output/DataStructures/QueueAr.decls rewritten by ComparablePairsDescFileReader
// Wed Jun 04 20:58:04 EDT 2003

var-comparability implicit

// Declarations for DataStructures/QueueAr.java
// Written Wed Jun  4 20:57:04 2003

ListImplementors
java.util.List

ppt DataStructures.QueueAr.QueueAr():::ENTER
  ppt-type enter

ppt DataStructures.QueueAr.QueueAr():::EXIT30
  ppt-type subexit
  parent parent DataStructures.QueueAr:::OBJECT 1
  variable this
    var-kind variable
    dec-type DataStructures.QueueAr
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    flags is_param
    comparability 2
  variable this.theArray
    var-kind field theArray
    enclosing-var this
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 1[0]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.currentSize
    var-kind field currentSize
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0
  variable this.front
    var-kind field front
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0
  variable this.back
    var-kind field back
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0

ppt DataStructures.QueueAr.QueueAr(int):::ENTER
  ppt-type enter
  variable capacity
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 0

ppt DataStructures.QueueAr.QueueAr(int):::EXIT39
  ppt-type subexit
  parent parent DataStructures.QueueAr:::OBJECT 1
  variable capacity
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 0
  variable this
    var-kind variable
    dec-type DataStructures.QueueAr
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    flags is_param
    comparability 2
  variable this.theArray
    var-kind field theArray
    enclosing-var this
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 1[0]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.currentSize
    var-kind field currentSize
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0
  variable this.front
    var-kind field front
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0
  variable this.back
    var-kind field back
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0

ppt DataStructures.QueueAr.isEmpty():::ENTER
  ppt-type enter
  parent parent DataStructures.QueueAr:::OBJECT 1
  variable this
    var-kind variable
    dec-type DataStructures.QueueAr
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    flags is_param
    comparability 2
  variable this.theArray
    var-kind field theArray
    enclosing-var this
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 1[0]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.currentSize
    var-kind field currentSize
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0
  variable this.front
    var-kind field front
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0
  variable this.back
    var-kind field back
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0

ppt DataStructures.QueueAr.isEmpty():::EXIT47
  ppt-type subexit
  parent parent DataStructures.QueueAr:::OBJECT 1
  variable return
    var-kind return
    dec-type boolean
    rep-type boolean
    comparability 0
  variable this
    var-kind variable
    dec-type DataStructures.QueueAr
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    flags is_param
    comparability 3
  variable this.theArray
    var-kind field theArray
    enclosing-var this
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 2[1]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.currentSize
    var-kind field currentSize
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 1
  variable this.front
    var-kind field front
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 1
  variable this.back
    var-kind field back
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 1

ppt DataStructures.QueueAr.isFull():::ENTER
  ppt-type enter
  parent parent DataStructures.QueueAr:::OBJECT 1
  variable this
    var-kind variable
    dec-type DataStructures.QueueAr
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    flags is_param
    comparability 2
  variable this.theArray
    var-kind field theArray
    enclosing-var this
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 1[0]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.currentSize
    var-kind field currentSize
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0
  variable this.front
    var-kind field front
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0
  variable this.back
    var-kind field back
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0

ppt DataStructures.QueueAr.isFull():::EXIT56
  ppt-type subexit
  parent parent DataStructures.QueueAr:::OBJECT 1
  variable return
    var-kind return
    dec-type boolean
    rep-type boolean
    comparability 0
  variable this
    var-kind variable
    dec-type DataStructures.QueueAr
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    flags is_param
    comparability 3
  variable this.theArray
    var-kind field theArray
    enclosing-var this
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 2[1]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.currentSize
    var-kind field currentSize
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 1
  variable this.front
    var-kind field front
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 1
  variable this.back
    var-kind field back
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 1

ppt DataStructures.QueueAr.makeEmpty():::ENTER
  ppt-type enter
  parent parent DataStructures.QueueAr:::OBJECT 1
  variable this
    var-kind variable
    dec-type DataStructures.QueueAr
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    flags is_param
    comparability 2
  variable this.theArray
    var-kind field theArray
    enclosing-var this
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 1[0]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.currentSize
    var-kind field currentSize
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0
  variable this.front
    var-kind field front
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0
  variable this.back
    var-kind field back
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0

ppt DataStructures.QueueAr.makeEmpty():::EXIT67
  ppt-type subexit
  parent parent DataStructures.QueueAr:::OBJECT 1
  variable this
    var-kind variable
    dec-type DataStructures.QueueAr
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    flags is_param
    comparability 2
  variable this.theArray
    var-kind field theArray
    enclosing-var this
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 1[0]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.currentSize
    var-kind field currentSize
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0
  variable this.front
    var-kind field front
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0
  variable this.back
    var-kind field back
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0

ppt DataStructures.QueueAr.getFront():::ENTER
  ppt-type enter
  parent parent DataStructures.QueueAr:::OBJECT 1
  variable this
    var-kind variable
    dec-type DataStructures.QueueAr
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    flags is_param
    comparability -2
  variable this.theArray
    var-kind field theArray
    enclosing-var this
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -2[-2]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.currentSize
    var-kind field currentSize
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -2
  variable this.front
    var-kind field front
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -2
  variable this.back
    var-kind field back
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -2

ppt DataStructures.QueueAr.getFront():::EXIT77
  ppt-type subexit
  parent parent DataStructures.QueueAr:::OBJECT 1
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
  variable this
    var-kind variable
    dec-type DataStructures.QueueAr
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    flags is_param
    comparability -2
  variable this.theArray
    var-kind field theArray
    enclosing-var this
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -2[-2]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.currentSize
    var-kind field currentSize
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -2
  variable this.front
    var-kind field front
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -2
  variable this.back
    var-kind field back
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -2

ppt DataStructures.QueueAr.getFront():::EXIT78
  ppt-type subexit
  parent parent DataStructures.QueueAr:::OBJECT 1
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
  variable this
    var-kind variable
    dec-type DataStructures.QueueAr
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    flags is_param
    comparability -2
  variable this.theArray
    var-kind field theArray
    enclosing-var this
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -2[-2]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.currentSize
    var-kind field currentSize
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -2
  variable this.front
    var-kind field front
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -2
  variable this.back
    var-kind field back
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -2

ppt DataStructures.QueueAr.dequeue():::ENTER
  ppt-type enter
  parent parent DataStructures.QueueAr:::OBJECT 1
  variable this
    var-kind variable
    dec-type DataStructures.QueueAr
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    flags is_param
    comparability 2
  variable this.theArray
    var-kind field theArray
    enclosing-var this
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 1[0]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.currentSize
    var-kind field currentSize
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0
  variable this.front
    var-kind field front
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0
  variable this.back
    var-kind field back
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0

ppt DataStructures.QueueAr.dequeue():::EXIT88
  ppt-type subexit
  parent parent DataStructures.QueueAr:::OBJECT 1
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
  variable this
    var-kind variable
    dec-type DataStructures.QueueAr
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    flags is_param
    comparability 2
  variable this.theArray
    var-kind field theArray
    enclosing-var this
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 1[0]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.currentSize
    var-kind field currentSize
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0
  variable this.front
    var-kind field front
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0
  variable this.back
    var-kind field back
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0

ppt DataStructures.QueueAr.dequeue():::EXIT94
  ppt-type subexit
  parent parent DataStructures.QueueAr:::OBJECT 1
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
  variable this
    var-kind variable
    dec-type DataStructures.QueueAr
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    flags is_param
    comparability 2
  variable this.theArray
    var-kind field theArray
    enclosing-var this
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0[1]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.currentSize
    var-kind field currentSize
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 1
  variable this.front
    var-kind field front
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 1
  variable this.back
    var-kind field back
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 1

ppt DataStructures.QueueAr.enqueue(java.lang.Object):::ENTER
  ppt-type enter
  parent parent DataStructures.QueueAr:::OBJECT 1
  variable x
    var-kind variable
    dec-type java.lang.Object
    rep-type hashcode
    flags is_param
    comparability 1
  variable x.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var x
    dec-type java.lang.Class
    rep-type java.lang.String
    comparability -1
  variable this
    var-kind variable
    dec-type DataStructures.QueueAr
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    flags is_param
    comparability 2
  variable this.theArray
    var-kind field theArray
    enclosing-var this
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 1[0]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.currentSize
    var-kind field currentSize
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0
  variable this.front
    var-kind field front
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0
  variable this.back
    var-kind field back
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0

ppt DataStructures.QueueAr.enqueue(java.lang.Object):::EXIT109
  ppt-type subexit
  parent parent DataStructures.QueueAr:::OBJECT 1
  variable x
    var-kind variable
    dec-type java.lang.Object
    rep-type hashcode
    flags is_param
    comparability 1
  variable x.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var x
    dec-type java.lang.Class
    rep-type java.lang.String
    comparability -1
  variable this
    var-kind variable
    dec-type DataStructures.QueueAr
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    flags is_param
    comparability 2
  variable this.theArray
    var-kind field theArray
    enclosing-var this
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 1[0]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.currentSize
    var-kind field currentSize
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0
  variable this.front
    var-kind field front
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0
  variable this.back
    var-kind field back
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0

ppt DataStructures.QueueAr.increment(int):::ENTER
  ppt-type enter
  parent parent DataStructures.QueueAr:::OBJECT 1
  variable x
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 0
  variable this
    var-kind variable
    dec-type DataStructures.QueueAr
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    flags is_param
    comparability 2
  variable this.theArray
    var-kind field theArray
    enclosing-var this
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 1[0]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.currentSize
    var-kind field currentSize
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0
  variable this.front
    var-kind field front
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0
  variable this.back
    var-kind field back
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 0

ppt DataStructures.QueueAr.increment(int):::EXIT120
  ppt-type subexit
  parent parent DataStructures.QueueAr:::OBJECT 1
  variable x
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 1
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 1
  variable this
    var-kind variable
    dec-type DataStructures.QueueAr
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    flags is_param
    comparability 0
  variable this.theArray
    var-kind field theArray
    enclosing-var this
    dec-type java.lang.Object[]
    rep-type hashcode
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -2
  variable this.theArray.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray
    dec-type java.lang.Class
    rep-type java.lang.String
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.theArray[..]
    var-kind array
    enclosing-var this.theArray
    array 1
    dec-type java.lang.Object[]
    rep-type hashcode[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 2[1]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    parent DataStructures.QueueAr:::OBJECT 1
    comparability -1
  variable this.currentSize
    var-kind field currentSize
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 1
  variable this.front
    var-kind field front
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 1
  variable this.back
    var-kind field back
    enclosing-var this
    dec-type int
    rep-type int
    parent DataStructures.QueueAr:::OBJECT 1
    comparability 1

ppt DataStructures.QueueAr.main(java.lang.String[]):::ENTER
  ppt-type enter
  variable args
    var-kind variable
    dec-type java.lang.String[]
    rep-type hashcode
    flags is_param
    comparability 1
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
    comparability 0[1]
  variable args[..].toString
    var-kind field toString
    enclosing-var args[..]
    array 1
    dec-type java.lang.String[]
    rep-type java.lang.String[]
    comparability -1

ppt DataStructures.QueueAr.main(java.lang.String[]):::EXIT201
  ppt-type subexit
  variable args
    var-kind variable
    dec-type java.lang.String[]
    rep-type hashcode
    flags is_param
    comparability 1
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
    comparability 0[1]
  variable args[..].toString
    var-kind field toString
    enclosing-var args[..]
    array 1
    dec-type java.lang.String[]
    rep-type java.lang.String[]
    comparability -1

ppt DataStructures.QueueAr:::OBJECT
  ppt-type object
  variable this
    var-kind variable
    dec-type DataStructures.QueueAr
    rep-type hashcode
    flags is_param
    comparability 2
  variable this.theArray
    var-kind field theArray
    enclosing-var this
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
    comparability 1[0]
  variable this.theArray[..].getClass().getName()
    var-kind function getClass().getName()
    enclosing-var this.theArray[..]
    array 1
    dec-type java.lang.Class[]
    rep-type java.lang.String[]
    comparability -1
  variable this.currentSize
    var-kind field currentSize
    enclosing-var this
    dec-type int
    rep-type int
    comparability 0
  variable this.front
    var-kind field front
    enclosing-var this
    dec-type int
    rep-type int
    comparability 0
  variable this.back
    var-kind field back
    enclosing-var this
    dec-type int
    rep-type int
    comparability 0
