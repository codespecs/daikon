package daikon;

import java.io.Serializable;
import utilMDE.*;

/**
 * ADT which represents naming data associated with a given program
 * point, such as the class or method.
 **/
public class PptName
  implements Serializable
{

  // any of these can be null
  private final String cls;
  private final String method;
  private final String point;

  // ==================== CONSTRUCTORS ====================
  
  /**
   * @param name non-null ppt name as given in the decls file
   **/
  public PptName(String name)
  {
    int sep = name.indexOf(FileIO.ppt_tag_separator);
    Assert.assert(sep >= 0);
    String pre_sep = name.substring(0, sep);
    String post_sep = name.substring(sep + FileIO.ppt_tag_separator.length());

    int dot = pre_sep.lastIndexOf('.');
    int lparen = pre_sep.indexOf('(');
    if (lparen == -1) {
      cls = pre_sep.intern();
      method = null;
    } else {
      Assert.assert(dot < lparen);
      cls = pre_sep.substring(0, dot).intern();
      method = pre_sep.substring(dot + 1).intern();
    }
    point = post_sep.intern();
  }

  /**
   * @param className fully-qualified class name
   **/
  public PptName(String className, String methodName, String pointName)
  {
    cls = (className != null) ? className.intern() : null;
    method = (methodName != null) ? methodName.intern() : null;
    point = (pointName != null) ? pointName.intern() : null;
  }

  // ==================== OBSERVERS ====================

  /**
   * @return the full-qualified class name, which uniquely identifies
   * a given class.
   **/
  public String getFullClassName()
  {
    return cls;
  }

  /**
   * @return the short name of the method, not including any
   * additional context, such as the package it is in.
   **/
  public String getShortClassName()
  {
    if (cls == null) return null;
    int pt = cls.lastIndexOf('.');
    if (pt == -1)
      return cls;
    else
      return cls.substring(0, pt);
  }

  /**
   * @return the full name which can uniquely identify a method within
   * a class.  The name includes symbols for the argument types and
   * return type.
   **/
  public String getFullMethodName()
  {
    return method;
  }

  /**
   * @return same as getFullMethodName(), except without the return
    * type information
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
   **/
  public String getShortMethodName()
  {
    if (method == null) return null;
    int lparen = method.indexOf('(');
    Assert.assert(lparen >= 0);
    return method.substring(0, lparen);
  }

  /**
   * @return something interesting and descriptive about the point in
   * question, along the lines of "ENTER" or "EXIT" or somesuch.  The
   * semantics of this method are not yet decided, so don't try to do
   * aynthing useful with this result.
   **/
  public String getPoint() {
    return point;
  }

  /**
   * @return a numberical subscript of the given point, or
   * Integer.MIN_VALUE if none exists.
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
   * @return true iff this name refers to a procedure exit point
   **/
  public boolean isEnterPoint()
  {
    return (point != null) && point.startsWith(FileIO.enter_suffix);
  }
  
  // ==================== PRODUCERS ====================

  /**
   * @requires this.isExitPoint()
   * @return a name for the corresponding enter point
   **/
  public PptName makeEnter()
  {
    Assert.assert(isExitPoint());
    return new PptName(cls, method, FileIO.enter_suffix);
  }

  /**
   * @requires this.isExitPoint() || this.isEnterPoint()
   * @return a name for the corresponding object invariant
   **/
  public PptName makeObject()
  {
    Assert.assert(isExitPoint() || isEnterPoint());
    return new PptName(cls, null, FileIO.object_suffix);
  }

  /**
   * @requires this.isExitPoint() || this.isEnterPoint()
   * @return a name for the corresponding class-static invariant
   **/
  public PptName makeClassStatic()
  {
    Assert.assert(isExitPoint() || isEnterPoint());
    return new PptName(cls, null, FileIO.class_static_suffix);
  }

  // ==================== OBJECT METHODS ====================
  
  /* @return interned string such that this.equals(new PptName(this.toString())) */
  public String toString()
  {
    return (cls +
	    ((method == null) ? "" : ("." + method)) +
	    FileIO.ppt_tag_separator +
	    point).intern();
  }

  public boolean equals(Object o)
  {
    return (o instanceof PptName) && equals((PptName) o);
  }

  public boolean equals(PptName o)
  {
    return
      (o != null) &&
      (cls == o.cls) &&
      (method == o.method) &&
      (point == o.point) &&
      true;
  }

  public int hashCode()
  {
    // If the domains of the components overlap, we should multiply by
    // primes, but I think they are fairly disjoint
    return
      ((cls == null) ? 0 : cls.hashCode()) +
      ((method == null) ? 0 : method.hashCode()) +
      ((point == null) ? 0 : point.hashCode()) +
      0;
  }
  
}
