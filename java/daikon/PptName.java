package daikon;

/**
 * ADT which represents naming data associated with a given program
 * point, such as the class or method.
 **/
public class PptName
{

  // any of these can be null
  private final String cls;
  private final String method;
  private final String point;

  /**
   * @param className fully-qualified class name
   **/
  public PptName(String className, String methodName, String pointName)
  {
    cls = className;
    method = methodName;
    point = pointName;
  }

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
   * @return the name (identifier) of the method, not taking into
   * account any arguments, return values, etc.
   **/
  public String getShortMethodName()
  {
    if (method == null) return null;
    int lparen = method.indexOf('(');
    if (lparen == -1)
      return method;
    else
      return method.substring(0, lparen);
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

  public boolean equals(Object o)
  {
    return (o instanceof PptName) && equals((PptName) o);
  }

  public boolean equals(PptName o)
  {
    // check components in order of entropy, for speed
    return
      (o != null) &&
      ((o.method == null) ? (method == null) : (o.cls.equals(method))) &&
      ((o.cls    == null) ? (cls    == null) : (o.cls.equals(cls))) &&
      ((o.point  == null) ? (point  == null) : (o.cls.equals(point)))
      ;
  }

  public int hashCode()
  {
    // If the domains of the components overlap, we should multiply my
    // primes, but I think they are fairly distinct
    return
      ((cls == null) ? 0 : cls.hashCode()) +
      ((method == null) ? 0 : method.hashCode()) +
      ((point == null) ? 0 : point.hashCode()) +
      0;
  }
  
}
