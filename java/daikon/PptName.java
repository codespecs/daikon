package daikon;

import utilMDE.Assert;

import java.io.ObjectInputStream;
import java.io.Serializable;
import java.io.IOException;

/**
 * PptName is an immutable ADT that represents naming data associated with a
 * given program point, such as the class or method.
 * 
 * <p> Examples below are as if the full value of this PptName were
 * "DataStructures.StackAr.pop()Ljava/lang/Object;:::EXIT84"
 **/
public class PptName
  implements Serializable
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  // cannot be "final", because they must be re-interned upon deserialization
  private String fullname;   // interned full program point name
  private String fn_name;    // interned, nullable; derived from fullname
  private String cls;        // interned fully-qualified class name, nullable; derived from fullname
  private String method;     // interned method signature, including types, nullable; derived from fullname
  private String point;      // interned post-separator (separator is ":::"), nullable; derived from fullname

  // Represenatation invariant:
  //
  // Fullname is always present.  If fullname does not contain :::,
  // then all of the other fields are null.  Otherwise, fn_name is the
  // part of fullname before the ::: and point is the part after.  If
  // fn_name does not contain '(' then class is the same as fn_name
  // and method is full.  If fn_name does contain a '(' and a '.' then
  // comes before it, then class is the portion before the dot and
  // method if the portion after; otherwise (fn_name contains '(' but
  // no dot) class is null and method is the same as fn_name.

  // ==================== CONSTRUCTORS ====================

  /**
   * @param name non-null ppt name as given in the decls file
   **/
  public PptName( String name )
  {
    // If the name is well-formed, like "mvspc.setupGUI()V:::EXIT75",
    // then this constructor will extract the class and method names.
    // If not (eg for lisp code), it's okay because only the GUI uses
    // this class/method information.

    fullname = name.intern();
    int seperatorPosition = name.indexOf( FileIO.ppt_tag_separator );
    if (seperatorPosition == -1) {
      // probably a lisp program, which was instrumented differently
      cls = method = point = fn_name = null;
      return;			
    }
    fn_name = name.substring(0, seperatorPosition).intern();
    point = name.substring(seperatorPosition + FileIO.ppt_tag_separator.length()).intern();

    int dot = fn_name.lastIndexOf('.');
    int lparen = fn_name.indexOf('(');
    if (lparen == -1) {
      cls = fn_name;
      method = null;
      return;
    }
    if (dot == -1  ||  dot >= lparen) {
      // probably a lisp program, which was instrumented differently
      method = fn_name;
      cls = null;
      return;
    }
    // now 0 <= dot < lparen
    cls = fn_name.substring(0, dot).intern();
    method = fn_name.substring(dot + 1).intern();
  }

  /**
   * @param className fully-qualified class name
   **/
  public PptName(String className, String methodName, String pointName)
  {
    if ((className == null) && (methodName == null)) {
      throw new UnsupportedOperationException
	("One of class or method must be given");
    }
    // First set class name
    if (className != null) {
      cls = className.intern();
      fn_name = cls;
    }
    // Then add method name
    if (methodName != null) {
      method = methodName.intern();
      if (cls != null) {
	fn_name = (cls + "." + method).intern();
      } else {
	fn_name = method;
      }
    }
    // Then add point
    if (pointName != null) {
      point = pointName.intern();
      fullname = (fn_name + FileIO.ppt_tag_separator + point).intern();
    } else {
      point = null;
      fullname = fn_name;
    }
  }

  // ==================== OBSERVERS ====================

  /**
   * @return getName() [convenience accessor]
   * @see getName()
   **/
  public String name() {
    return getName();
  }

  /**
   * @return the complete program point name
   * e.g. "DataStructures.StackAr.pop()Ljava/lang/Object;:::EXIT84"
   **/
  public String getName() {
    return fullname;
  }

  /**
   * @return the fully-qualified class name, which uniquely identifies
   * a given class.
   * May be null.
   * e.g. "DataStructures.StackAr"
   **/
  public String getFullClassName()
  {
    return cls;
  }

  /**
   * @return the short name of the class, not including any
   * additional context, such as the package it is in.
   * May be null.
   * e.g. "StackAr"
   **/
  public String getShortClassName()
  {
    if (cls == null) return null;
    int pt = cls.lastIndexOf('.');
    if (pt == -1)
      return cls;
    else
      return cls.substring(pt+1);
  }

  /**
   * @return the full name which can uniquely identify a method within
   * a class.  The name includes symbols for the argument types and
   * return type.
   * May be null.
   * e.g. "pop()Ljava/lang/Object;"
   **/
  public String getFullMethodName()
  {
    return method;
  }

  /**
   * @return same as getFullMethodName(), except without the return
    * type information.
    * May be null.
    * e.g. "pop()"    
    **/
  public String getFullMethodNameWithoutReturn()
  {
    if (method == null) return null;
    int rparen = method.indexOf(')');
    Assert.assert(rparen >= 0);
    return method.substring(0, rparen+1);
  }

  /**
   * @return the name (identifier) of the method, not taking into
   * account any arguments, return values, etc.
   * May be null.
   * e.g. "pop"
   **/
  public String getShortMethodName()
  {
    if (method == null) return null;
    int lparen = method.indexOf('(');
    Assert.assert(lparen >= 0);
    return method.substring(0, lparen);
  }

  /**
   * @return the fully-qualified class and method name (and signature).
   * Does not include any point information (such as ENTER or EXIT).
   * Similar function lives in Ppt.fn_name(String)
   * May be null.
   * e.g. "DataStructures.StackAr.pop()Ljava/lang/Object;"
   **/
  public String getNameWithoutPoint() {
    if (cls == null && method == null) return null;
    if (cls == null) return method;
    if (method == null) return cls;
    return (cls + "." + method).intern();
  }

  /**
   * @return something interesting and descriptive about the point in
   * question, along the lines of "ENTER" or "EXIT" or somesuch.  The
   * semantics of this method are not yet decided, so don't try to do
   * aynthing useful with this result.
   * May be null.
   * e.g. "EXIT84"
   **/
  public String getPoint() {
    return point;
  }

  /**
   * @return a numberical subscript of the given point, or
   * Integer.MIN_VALUE if none exists.
   * e.g. "84"
   * @see exitLine
   **/
  public int getPointSubscript()
  {
    int result = Integer.MIN_VALUE;
    if (point != null) {
      // returns the largest substring [i..] which parses to an integer
      for (int i = 0; i < point.length(); i++) {
	char c = point.charAt(i);
	if (('0' <= c) && (c <= '9')) {
	  try {
	    result = Integer.parseInt(point.substring(i));
	    break;
	  } catch (NumberFormatException e) {
	  }
	}
      }
    }
    return result;
  }

  /**
   * @return true iff this name refers to a synthetic object instance
   * program point
   **/
  public boolean isObjectInstanceSynthetic()
  {
    return FileIO.object_suffix.equals(point);
  }

  /**
   * @return true iff this name refers to a synthetic class instance
   * program point
   **/
  public boolean isClassStaticSynthetic()
  {
    return FileIO.class_static_suffix.equals(point);
  }

  /**
   * @return true iff this name refers to a procedure exit point
   **/
  public boolean isExitPoint()
  {
    return (point != null) && point.startsWith(FileIO.exit_suffix);
  }

  /**
   * @return true iff this name refers to an abrupt completion point
   **/
  public boolean isThrowsPoint()
  {
    return (point != null) && point.startsWith(FileIO.throws_suffix);
  }

  /**
   * @return true iff this name refers to a combined (synthetic) procedure
   *         exit point
   **/
  public boolean isCombinedExitPoint()
  {
    return (point != null) && point.equals(FileIO.exit_suffix);
  }

  /**
   * @return true iff this name refers to a procedure exit point
   **/
  public boolean isEnterPoint()
  {
    return (point != null) && point.startsWith(FileIO.enter_suffix);
  }

  /**
   * @return a string containing the line number, if this is an exit point;
   *         otherwise, return null
   * @see getPointSubscript
   **/
  public String exitLine() {
    if (!isExitPoint())
      return "";
    int non_digit;
    for (non_digit=FileIO.exit_suffix.length(); non_digit<point.length(); non_digit++) {
      if (! Character.isDigit(point.charAt(non_digit)))
        break;
    }
    return point.substring(FileIO.exit_suffix.length(), non_digit);
  }

  /**
   * @return true iff this program point is a constructor entry or exit.
   **/
  public boolean isConstructor() {
    return (method != null)
      && (method.startsWith("<init>"));
  }

  // ==================== PRODUCERS ====================

  /**
   * @requires this.isExitPoint()
   * @return a name for the corresponding enter point
   **/
  public PptName makeEnter()
  {
    Assert.assert(isExitPoint(), fullname);
    Assert.assert(isExitPoint() || isThrowsPoint());
    return new PptName(cls, method, FileIO.enter_suffix);
  }

  /**
   * @requires this.isExitPoint() || this.isEnterPoint()
   * @return a name for the combined exit point
   **/
  public PptName makeExit()
  {
    Assert.assert(isExitPoint() || isEnterPoint(), fullname);
    return new PptName(cls, method, FileIO.exit_suffix);
  }

  /**
   * @requires this.isExitPoint() || this.isEnterPoint()
   * @return a name for the corresponding object invariant
   **/
  public PptName makeObject()
  {
    Assert.assert(isExitPoint() || isEnterPoint(), fullname);
    return new PptName(cls, null, FileIO.object_suffix);
  }

  /**
   * @requires this.isExitPoint() || this.isEnterPoint() || this.isObjectInstanceSynthetic()
   * @return a name for the corresponding class-static invariant
   **/
  public PptName makeClassStatic()
  {
    Assert.assert(isExitPoint() || isEnterPoint() || isObjectInstanceSynthetic(), fullname);
    return new PptName(cls, null, FileIO.class_static_suffix);
  }

  // ==================== OBJECT METHODS ====================

  /* @return interned string such that this.equals(new PptName(this.toString())) */
  public String toString()
  {
    return fullname;
  }

  public boolean equals(Object o)
  {
    return (o instanceof PptName) && equals((PptName) o);
  }

  public boolean equals(PptName o)
  {
    return (o != null) && (o.fullname == fullname);
  }

  public int hashCode()
  {
    return fullname.hashCode();
  }

  // Interning is lost when an object is serialized and deserialized.
  // Manually re-intern any interned fields upon deserialization.
  private void readObject(ObjectInputStream in)
    throws IOException, ClassNotFoundException
  {
    in.defaultReadObject();
    if (fullname != null)
      fullname = fullname.intern();
    if (fn_name != null)
      fn_name = fn_name.intern();
    if (cls != null)
      cls = cls.intern();
    if (method != null)
      method = method.intern();
    if (point != null)
      point = point.intern();
  }


}
