decl-version 2.0

// Declarations for MapQuick1/StreetNumberSet.java
// Written Wed Jun  4 22:50:17 2003

var-comparability none

ListImplementors
java.util.List

ppt MapQuick1.StreetNumberSet.StreetNumberSet(java.lang.String):::ENTER
  ppt-type enter
  variable numbers
    var-kind variable
    dec-type java.lang.String
    rep-type hashcode
    flags is_param
    comparability 22
  variable numbers.toString
    var-kind field toString
    enclosing-var numbers
    dec-type java.lang.String
    rep-type java.lang.String
    comparability 22

ppt MapQuick1.StreetNumberSet.StreetNumberSet(java.lang.String):::EXIT68
  ppt-type subexit
  parent parent MapQuick1.StreetNumberSet:::OBJECT 1
  variable numbers
    var-kind variable
    dec-type java.lang.String
    rep-type hashcode
    flags is_param
    comparability 22
  variable numbers.toString
    var-kind field toString
    enclosing-var numbers
    dec-type java.lang.String
    rep-type java.lang.String
    comparability 22
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22

ppt MapQuick1.StreetNumberSet.parityOf(int):::ENTER
  ppt-type enter
  variable i
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 22

ppt MapQuick1.StreetNumberSet.parityOf(int):::EXIT72
  ppt-type subexit
  variable i
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 22
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 22

ppt MapQuick1.StreetNumberSet.checkRep():::ENTER
  ppt-type enter
  parent parent MapQuick1.StreetNumberSet:::OBJECT 1
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22

ppt MapQuick1.StreetNumberSet.checkRep():::EXIT104
  ppt-type subexit
  parent parent MapQuick1.StreetNumberSet:::OBJECT 1
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22

ppt MapQuick1.StreetNumberSet.contains(int):::ENTER
  ppt-type enter
  parent parent MapQuick1.StreetNumberSet:::OBJECT 1
  variable n
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 22
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22

ppt MapQuick1.StreetNumberSet.contains(int):::EXIT118
  ppt-type subexit
  parent parent MapQuick1.StreetNumberSet:::OBJECT 1
  variable n
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 22
  variable return
    var-kind return
    dec-type boolean
    rep-type boolean
    comparability 22
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22

ppt MapQuick1.StreetNumberSet.contains(int):::EXIT123
  ppt-type subexit
  parent parent MapQuick1.StreetNumberSet:::OBJECT 1
  variable n
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 22
  variable return
    var-kind return
    dec-type boolean
    rep-type boolean
    comparability 22
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22

ppt MapQuick1.StreetNumberSet.orderStatistic(int):::ENTER
  ppt-type enter
  parent parent MapQuick1.StreetNumberSet:::OBJECT 1
  variable n
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 22
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22

ppt MapQuick1.StreetNumberSet.orderStatistic(int):::EXIT162
  ppt-type subexit
  parent parent MapQuick1.StreetNumberSet:::OBJECT 1
  variable n
    var-kind variable
    dec-type int
    rep-type int
    flags is_param
    comparability 22
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 22
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22

ppt MapQuick1.StreetNumberSet.size():::ENTER
  ppt-type enter
  parent parent MapQuick1.StreetNumberSet:::OBJECT 1
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22

ppt MapQuick1.StreetNumberSet.size():::EXIT181
  ppt-type subexit
  parent parent MapQuick1.StreetNumberSet:::OBJECT 1
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 22
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22

ppt MapQuick1.StreetNumberSet.isEmpty():::ENTER
  ppt-type enter
  parent parent MapQuick1.StreetNumberSet:::OBJECT 1
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22

ppt MapQuick1.StreetNumberSet.isEmpty():::EXIT190
  ppt-type subexit
  parent parent MapQuick1.StreetNumberSet:::OBJECT 1
  variable return
    var-kind return
    dec-type boolean
    rep-type boolean
    comparability 22
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22

ppt MapQuick1.StreetNumberSet.min():::ENTER
  ppt-type enter
  parent parent MapQuick1.StreetNumberSet:::OBJECT 1
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22

ppt MapQuick1.StreetNumberSet.min():::EXIT210
  ppt-type subexit
  parent parent MapQuick1.StreetNumberSet:::OBJECT 1
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 22
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22

ppt MapQuick1.StreetNumberSet.max():::ENTER
  ppt-type enter
  parent parent MapQuick1.StreetNumberSet:::OBJECT 1
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22

ppt MapQuick1.StreetNumberSet.max():::EXIT230
  ppt-type subexit
  parent parent MapQuick1.StreetNumberSet:::OBJECT 1
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 22
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22

ppt MapQuick1.StreetNumberSet.intersects(MapQuick1.StreetNumberSet):::ENTER
  ppt-type enter
  parent parent MapQuick1.StreetNumberSet:::OBJECT 1
  variable other
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    flags is_param
    comparability 22
  variable other.begins
    var-kind field begins
    enclosing-var other
    dec-type int[]
    rep-type hashcode
    comparability 22
  variable other.begins[..]
    var-kind array
    enclosing-var other.begins
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22
  variable other.ends
    var-kind field ends
    enclosing-var other
    dec-type int[]
    rep-type hashcode
    comparability 22
  variable other.ends[..]
    var-kind array
    enclosing-var other.ends
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22

ppt MapQuick1.StreetNumberSet.intersects(MapQuick1.StreetNumberSet):::EXIT239
  ppt-type subexit
  parent parent MapQuick1.StreetNumberSet:::OBJECT 1
  variable other
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    flags is_param
    comparability 22
  variable other.begins
    var-kind field begins
    enclosing-var other
    dec-type int[]
    rep-type hashcode
    comparability 22
  variable other.begins[..]
    var-kind array
    enclosing-var other.begins
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22
  variable other.ends
    var-kind field ends
    enclosing-var other
    dec-type int[]
    rep-type hashcode
    comparability 22
  variable other.ends[..]
    var-kind array
    enclosing-var other.ends
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22
  variable return
    var-kind return
    dec-type boolean
    rep-type boolean
    comparability 22
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22

ppt MapQuick1.StreetNumberSet.intersects(MapQuick1.StreetNumberSet):::EXIT240
  ppt-type subexit
  parent parent MapQuick1.StreetNumberSet:::OBJECT 1
  variable other
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    flags is_param
    comparability 22
  variable other.begins
    var-kind field begins
    enclosing-var other
    dec-type int[]
    rep-type hashcode
    comparability 22
  variable other.begins[..]
    var-kind array
    enclosing-var other.begins
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22
  variable other.ends
    var-kind field ends
    enclosing-var other
    dec-type int[]
    rep-type hashcode
    comparability 22
  variable other.ends[..]
    var-kind array
    enclosing-var other.ends
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22
  variable return
    var-kind return
    dec-type boolean
    rep-type boolean
    comparability 22
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22

ppt MapQuick1.StreetNumberSet.intersects(MapQuick1.StreetNumberSet):::EXIT253
  ppt-type subexit
  parent parent MapQuick1.StreetNumberSet:::OBJECT 1
  variable other
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    flags is_param
    comparability 22
  variable other.begins
    var-kind field begins
    enclosing-var other
    dec-type int[]
    rep-type hashcode
    comparability 22
  variable other.begins[..]
    var-kind array
    enclosing-var other.begins
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22
  variable other.ends
    var-kind field ends
    enclosing-var other
    dec-type int[]
    rep-type hashcode
    comparability 22
  variable other.ends[..]
    var-kind array
    enclosing-var other.ends
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22
  variable return
    var-kind return
    dec-type boolean
    rep-type boolean
    comparability 22
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22

ppt MapQuick1.StreetNumberSet.intersects(MapQuick1.StreetNumberSet):::EXIT257
  ppt-type subexit
  parent parent MapQuick1.StreetNumberSet:::OBJECT 1
  variable other
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    flags is_param
    comparability 22
  variable other.begins
    var-kind field begins
    enclosing-var other
    dec-type int[]
    rep-type hashcode
    comparability 22
  variable other.begins[..]
    var-kind array
    enclosing-var other.begins
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22
  variable other.ends
    var-kind field ends
    enclosing-var other
    dec-type int[]
    rep-type hashcode
    comparability 22
  variable other.ends[..]
    var-kind array
    enclosing-var other.ends
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22
  variable return
    var-kind return
    dec-type boolean
    rep-type boolean
    comparability 22
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22

ppt MapQuick1.StreetNumberSet.equals(java.lang.Object):::ENTER
  ppt-type enter
  parent parent MapQuick1.StreetNumberSet:::OBJECT 1
  variable o
    var-kind variable
    dec-type java.lang.Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable o.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var o
    dec-type java.lang.Class
    rep-type java.lang.String
    comparability 22
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22

ppt MapQuick1.StreetNumberSet.equals(java.lang.Object):::EXIT266
  ppt-type subexit
  parent parent MapQuick1.StreetNumberSet:::OBJECT 1
  variable o
    var-kind variable
    dec-type java.lang.Object
    rep-type hashcode
    flags is_param
    comparability 22
  variable o.getClass().getName()
    var-kind function getClass().getName()
    enclosing-var o
    dec-type java.lang.Class
    rep-type java.lang.String
    comparability 22
  variable return
    var-kind return
    dec-type boolean
    rep-type boolean
    comparability 22
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22

ppt MapQuick1.StreetNumberSet.equals(MapQuick1.StreetNumberSet):::ENTER
  ppt-type enter
  parent parent MapQuick1.StreetNumberSet:::OBJECT 1
  variable other
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    flags is_param
    comparability 22
  variable other.begins
    var-kind field begins
    enclosing-var other
    dec-type int[]
    rep-type hashcode
    comparability 22
  variable other.begins[..]
    var-kind array
    enclosing-var other.begins
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22
  variable other.ends
    var-kind field ends
    enclosing-var other
    dec-type int[]
    rep-type hashcode
    comparability 22
  variable other.ends[..]
    var-kind array
    enclosing-var other.ends
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22

ppt MapQuick1.StreetNumberSet.equals(MapQuick1.StreetNumberSet):::EXIT271
  ppt-type subexit
  parent parent MapQuick1.StreetNumberSet:::OBJECT 1
  variable other
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    flags is_param
    comparability 22
  variable other.begins
    var-kind field begins
    enclosing-var other
    dec-type int[]
    rep-type hashcode
    comparability 22
  variable other.begins[..]
    var-kind array
    enclosing-var other.begins
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22
  variable other.ends
    var-kind field ends
    enclosing-var other
    dec-type int[]
    rep-type hashcode
    comparability 22
  variable other.ends[..]
    var-kind array
    enclosing-var other.ends
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22
  variable return
    var-kind return
    dec-type boolean
    rep-type boolean
    comparability 22
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22

ppt MapQuick1.StreetNumberSet.equals(MapQuick1.StreetNumberSet):::EXIT272
  ppt-type subexit
  parent parent MapQuick1.StreetNumberSet:::OBJECT 1
  variable other
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    flags is_param
    comparability 22
  variable other.begins
    var-kind field begins
    enclosing-var other
    dec-type int[]
    rep-type hashcode
    comparability 22
  variable other.begins[..]
    var-kind array
    enclosing-var other.begins
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22
  variable other.ends
    var-kind field ends
    enclosing-var other
    dec-type int[]
    rep-type hashcode
    comparability 22
  variable other.ends[..]
    var-kind array
    enclosing-var other.ends
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22
  variable return
    var-kind return
    dec-type boolean
    rep-type boolean
    comparability 22
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22

ppt MapQuick1.StreetNumberSet.equals(MapQuick1.StreetNumberSet):::EXIT275
  ppt-type subexit
  parent parent MapQuick1.StreetNumberSet:::OBJECT 1
  variable other
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    flags is_param
    comparability 22
  variable other.begins
    var-kind field begins
    enclosing-var other
    dec-type int[]
    rep-type hashcode
    comparability 22
  variable other.begins[..]
    var-kind array
    enclosing-var other.begins
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22
  variable other.ends
    var-kind field ends
    enclosing-var other
    dec-type int[]
    rep-type hashcode
    comparability 22
  variable other.ends[..]
    var-kind array
    enclosing-var other.ends
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22
  variable return
    var-kind return
    dec-type boolean
    rep-type boolean
    comparability 22
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22

ppt MapQuick1.StreetNumberSet.equals(MapQuick1.StreetNumberSet):::EXIT281
  ppt-type subexit
  parent parent MapQuick1.StreetNumberSet:::OBJECT 1
  variable other
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    flags is_param
    comparability 22
  variable other.begins
    var-kind field begins
    enclosing-var other
    dec-type int[]
    rep-type hashcode
    comparability 22
  variable other.begins[..]
    var-kind array
    enclosing-var other.begins
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22
  variable other.ends
    var-kind field ends
    enclosing-var other
    dec-type int[]
    rep-type hashcode
    comparability 22
  variable other.ends[..]
    var-kind array
    enclosing-var other.ends
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22
  variable return
    var-kind return
    dec-type boolean
    rep-type boolean
    comparability 22
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22

ppt MapQuick1.StreetNumberSet.equals(MapQuick1.StreetNumberSet):::EXIT282
  ppt-type subexit
  parent parent MapQuick1.StreetNumberSet:::OBJECT 1
  variable other
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    flags is_param
    comparability 22
  variable other.begins
    var-kind field begins
    enclosing-var other
    dec-type int[]
    rep-type hashcode
    comparability 22
  variable other.begins[..]
    var-kind array
    enclosing-var other.begins
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22
  variable other.ends
    var-kind field ends
    enclosing-var other
    dec-type int[]
    rep-type hashcode
    comparability 22
  variable other.ends[..]
    var-kind array
    enclosing-var other.ends
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22
  variable return
    var-kind return
    dec-type boolean
    rep-type boolean
    comparability 22
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22

ppt MapQuick1.StreetNumberSet.equals(MapQuick1.StreetNumberSet):::EXIT286
  ppt-type subexit
  parent parent MapQuick1.StreetNumberSet:::OBJECT 1
  variable other
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    flags is_param
    comparability 22
  variable other.begins
    var-kind field begins
    enclosing-var other
    dec-type int[]
    rep-type hashcode
    comparability 22
  variable other.begins[..]
    var-kind array
    enclosing-var other.begins
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22
  variable other.ends
    var-kind field ends
    enclosing-var other
    dec-type int[]
    rep-type hashcode
    comparability 22
  variable other.ends[..]
    var-kind array
    enclosing-var other.ends
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22
  variable return
    var-kind return
    dec-type boolean
    rep-type boolean
    comparability 22
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22

ppt MapQuick1.StreetNumberSet.equals(MapQuick1.StreetNumberSet):::EXIT290
  ppt-type subexit
  parent parent MapQuick1.StreetNumberSet:::OBJECT 1
  variable other
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    flags is_param
    comparability 22
  variable other.begins
    var-kind field begins
    enclosing-var other
    dec-type int[]
    rep-type hashcode
    comparability 22
  variable other.begins[..]
    var-kind array
    enclosing-var other.begins
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22
  variable other.ends
    var-kind field ends
    enclosing-var other
    dec-type int[]
    rep-type hashcode
    comparability 22
  variable other.ends[..]
    var-kind array
    enclosing-var other.ends
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22
  variable return
    var-kind return
    dec-type boolean
    rep-type boolean
    comparability 22
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22

ppt MapQuick1.StreetNumberSet.hashCode():::ENTER
  ppt-type enter
  parent parent MapQuick1.StreetNumberSet:::OBJECT 1
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22

ppt MapQuick1.StreetNumberSet.hashCode():::EXIT295
  ppt-type subexit
  parent parent MapQuick1.StreetNumberSet:::OBJECT 1
  variable return
    var-kind return
    dec-type int
    rep-type int
    comparability 22
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    parent MapQuick1.StreetNumberSet:::OBJECT 1
    comparability 22

ppt MapQuick1.StreetNumberSet:::OBJECT
  ppt-type object
  variable this
    var-kind variable
    dec-type MapQuick1.StreetNumberSet
    rep-type hashcode
    flags is_param
    comparability 22
  variable this.begins
    var-kind field begins
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    comparability 22
  variable this.begins[..]
    var-kind array
    enclosing-var this.begins
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22
  variable this.ends
    var-kind field ends
    enclosing-var this
    dec-type int[]
    rep-type hashcode
    comparability 22
  variable this.ends[..]
    var-kind array
    enclosing-var this.ends
    array 1
    dec-type int[]
    rep-type int[]
    comparability 22
