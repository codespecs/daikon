// For tests of the entire utilMDE package, see class TestUtilMDE.

package utilMDE;

import java.io.*;
import java.util.*;
import java.util.zip.*;
import java.lang.reflect.*;
// import Assert;

// Yes, I know the name is very close to that of the package.
/** Utility functions that do not belong elsewhere in the utilMDE package. */
public final class UtilMDE {

  ///
  /// Array
  ///

  // For arrays, see ArraysMDE.java.

  ///
  /// BufferedFileReader
  ///

  /**
   * Returns an appropriate BufferedReader, accounting for the possibility
   * that the specified file is compressed.
   **/
  public static BufferedReader BufferedFileReader(String filename) throws FileNotFoundException, IOException {
    Reader file_reader;
    if (filename.endsWith(".gz")) {
      file_reader = new InputStreamReader(new GZIPInputStream(new FileInputStream(filename)));
    } else {
      file_reader = new FileReader(filename);
    }
    return new BufferedReader(file_reader);
  }


  /**
   * Returns an appropriate LineNumberReader, accounting for the possibility
   * that the specified file is compressed.
   **/
  public static LineNumberReader LineNumberFileReader(String filename) throws FileNotFoundException, IOException {
    Reader file_reader;
    if (filename.endsWith(".gz")) {
      file_reader = new InputStreamReader(new GZIPInputStream(new FileInputStream(filename)));
    } else {
      file_reader = new FileReader(filename);
    }
    return new LineNumberReader(file_reader);
  }

  ///
  /// Class
  ///

  private static HashMap primitiveClasses = new HashMap(8);
  static {
    primitiveClasses.put("boolean", Boolean.TYPE);
    primitiveClasses.put("byte", Byte.TYPE);
    primitiveClasses.put("char", Character.TYPE);
    primitiveClasses.put("double", Double.TYPE);
    primitiveClasses.put("float", Float.TYPE);
    primitiveClasses.put("int", Integer.TYPE);
    primitiveClasses.put("long", Long.TYPE);
    primitiveClasses.put("short", Short.TYPE);
  }

  /**
   * Like @link{Class.forName(String)}, but works when the string
   * represents a primitive type, too.
   **/
  public static Class classForName(String className) throws ClassNotFoundException {
    Object result = primitiveClasses.get(className);
    if (result != null)
      return (Class) result;
    else
      return Class.forName(className);
  }

  private static HashMap primitiveClassesJvm = new HashMap(8);
  static {
    primitiveClassesJvm.put("boolean", "Z");
    primitiveClassesJvm.put("byte", "B");
    primitiveClassesJvm.put("char", "C");
    primitiveClassesJvm.put("double", "D");
    primitiveClassesJvm.put("float", "F");
    primitiveClassesJvm.put("int", "I");
    primitiveClassesJvm.put("long", "L");
    primitiveClassesJvm.put("short", "S");
  }

  /** Convert a string of the form "java.lang.Object[]" to "[Ljava.lang.Object;" **/
  public static String classnameToJvm(String classname) {
    int dims = 0;
    while (classname.endsWith("[]")) {
      dims++;
      classname = classname.substring(0, classname.length()-2);
    }
    String result = (String) primitiveClassesJvm.get(classname);
    if (result == null) {
      result = "L" + classname + ";";
    }
    for (int i=0; i<dims; i++) {
      result = "[" + result;
    }
    return result;
  }

  private static HashMap primitiveClassesFromJvm = new HashMap(8);
  static {
    primitiveClassesFromJvm.put("Z", "boolean");
    primitiveClassesFromJvm.put("B", "byte");
    primitiveClassesFromJvm.put("C", "char");
    primitiveClassesFromJvm.put("D", "double");
    primitiveClassesFromJvm.put("F", "float");
    primitiveClassesFromJvm.put("I", "int");
    primitiveClassesFromJvm.put("L", "long");
    primitiveClassesFromJvm.put("S", "short");
  }

  /** Convert a string of the form "[Ljava.lang.Object;" to "java.lang.Object[]" **/
  public static String classnameFromJvm(String classname) {
    int dims = 0;
    while (classname.startsWith("[")) {
      dims++;
      classname = classname.substring(1);
    }
    String result;
    if (classname.startsWith("L") && classname.endsWith(";")) {
      result = classname.substring(1, classname.length() - 1);
    } else {
      result = (String) primitiveClassesFromJvm.get(classname);
      if (result == null) {
        throw new Error("Malformed base class: " + classname);
      }
    }
    for (int i=0; i<dims; i++) {
      result += "[]";
    }
    return result;
  }


  ///
  /// Classpath
  ///

  // Perhaps abstract out the simpler addToPath from this
  /** Add the directory to the system classpath. */
  public static void addToClasspath(String dir) {
    // If the dir isn't on CLASSPATH, add it.
    String pathSep = System.getProperty("path.separator");
    // what is the point of the "replace()" call?
    String cp = System.getProperty("java.class.path",".").replace('\\', '/');
    StringTokenizer tokenizer = new StringTokenizer(cp, pathSep, false);
    boolean found = false;
    while (tokenizer.hasMoreTokens() && !found) {
      found = tokenizer.nextToken().equals(dir);
    }
    if (!found) {
      System.setProperty("java.class.path", dir + pathSep + cp);
    }
  }

  ///
  /// File
  ///

  // Someone must have already written this.  Right?

  // Deals with exactly one "*" in name.
  public static final class WildcardFilter implements FilenameFilter {
    String prefix;
    String suffix;
    public WildcardFilter(String filename) {
      int astloc = filename.indexOf("*");
      if (astloc == -1)
        throw new Error("No asterisk in wildcard argument: " + filename);
      prefix = filename.substring(0, astloc);
      suffix = filename.substring(astloc+1);
      if (filename.indexOf("*") != -1)
        throw new Error("Multiple asterisks in wildcard argument: " + filename);
    }
    public boolean accept(File dir, String name) {
      return name.startsWith(prefix) && name.endsWith(suffix);
    }
  }


  ///
  /// HashMap
  ///

  // In Python, inlining this gave a 10x speed improvement.
  // Will the same be true for Java?
  /**
   * Increment the Integer which is indexed by key in the HashMap.
   * If the key isn't in the HashMap, it is added.
   * Throws an error if the key is in the HashMap but maps to a non-Integer.
   **/
  public Object incrementHashMap(HashMap hm, Object key, int count) {
    Object old = hm.get(key);
    int new_total;
    if (old == null) {
      new_total = count;
    } else {
      new_total = ((Integer) old).intValue() + count;
    }
    return hm.put(key, new Integer(new_total));
  }


  ///
  /// Iterator
  ///

  // Making these classes into functions didn't work because I couldn't get
  // their arguments into a scope that Java was happy with.

  /** Converts an Enumeration into an Iterator. */
  public static final class EnumerationIterator implements Iterator {
    Enumeration e;
    public EnumerationIterator(Enumeration e) { this.e = e; }
    public boolean hasNext() { return e.hasMoreElements(); }
    public Object next() { return e.nextElement(); }
    public void remove() { throw new UnsupportedOperationException(); }
  }

  /** Converts an Iterator into an Enumeration. */
  public static final class IteratorEnumeration implements Enumeration {
    Iterator itor;
    public IteratorEnumeration(Iterator itor) { this.itor = itor; }
    public boolean hasMoreElements() { return itor.hasNext(); }
    public Object nextElement() { return itor.next(); }
  }

  // This must already be implemented someplace else.  Right??
  /**
   * An Iterator that returns first the elements returned by its first
   * argument, then the elements returned by its second argument.
   **/
  public static final class MergedIterator2 implements Iterator {
    Iterator itor1, itor2;
    public MergedIterator2(Iterator itor1_, Iterator itor2_) {
      this.itor1 = itor1_; this.itor2 = itor2_;
    }
    public boolean hasNext() {
      return (itor1.hasNext() || itor2.hasNext());
    }
    public Object next() {
      if (itor1.hasNext())
        return itor1.next();
      else if (itor2.hasNext())
        return itor2.next();
      else
        throw new NoSuchElementException();
    }
    public void remove() {
      throw new UnsupportedOperationException();
    }
  }

  // This must already be implemented someplace else.  Right??
  /**
   * An Iterator that returns the elements in each of its argument
   * Iterators, in turn.  The argument is an Iterator of Iterators.
   **/
  public static final class MergedIterator implements Iterator {
    Iterator itorOfItors;
    public MergedIterator(Iterator itorOfItors) { this.itorOfItors = itorOfItors; }

    // an empty iterator to prime the pump
    Iterator current = Collections.EMPTY_SET.iterator();

    public boolean hasNext() {
      while ((!current.hasNext()) && (itorOfItors.hasNext())) {
        current = (Iterator) itorOfItors.next();
      }
      return current.hasNext();
    }

    public Object next() {
      hasNext();                // for side effect
      return current.next();
    }

    public void remove() {
      throw new UnsupportedOperationException();
    }
  }

  public static final class FilteredIterator implements Iterator {
    Iterator itor;
    Filter filter;

    public FilteredIterator(Iterator itor, Filter filter) {
      this.itor = itor; this.filter = filter;
    }

    Object current;
    boolean current_valid = false;

    public boolean hasNext() {
      while ((!current_valid) && itor.hasNext()) {
        current = itor.next();
        current_valid = filter.accept(current);
      }
      return current_valid;
    }

    public Object next() {
      if (hasNext()) {
        current_valid = false;
        return current;
      } else {
        throw new NoSuchElementException();
      }
    }

    public void remove() {
      throw new UnsupportedOperationException();
    }
  }


  ///
  /// Method
  ///

  // maps from a string of arg names to an array of Class objects.
  static HashMap args_seen = new HashMap();

  public static Method methodForName(String method)
    throws ClassNotFoundException, NoSuchMethodException, SecurityException {

    int oparenpos = method.indexOf('(');
    int dotpos = method.lastIndexOf('.', oparenpos);
    int cparenpos = method.indexOf(')', oparenpos);
    if ((dotpos == -1) || (oparenpos == -1) || (cparenpos == -1)) {
      throw new Error("malformed method name should contain a period, open paren, and close paren: " + method + " <<" + dotpos + "," + oparenpos + "," + cparenpos + ">>");
    }
    for (int i=cparenpos+1; i<method.length(); i++) {
      if (! Character.isWhitespace(method.charAt(i))) {
        throw new Error("malformed method name should contain only whitespace following close paren");
      }
    }

    String classname = method.substring(0,dotpos);
    String methodname = method.substring(dotpos+1, oparenpos);
    String all_argnames = method.substring(oparenpos+1, cparenpos).trim();
    Class[] argclasses = (Class[]) args_seen.get(all_argnames);;
    if (argclasses == null) {
      String[] argnames;
      if (all_argnames.equals("")) {
        argnames = new String[0];
      } else {
        argnames = split(all_argnames, ',');
      }

      argclasses = new Class[argnames.length];
      for (int i=0; i<argnames.length; i++) {
        String argname = argnames[i].trim();
        int numbrackets = 0;
        while (argname.endsWith("[]")) {
          argname = argname.substring(0, argname.length()-2);
          numbrackets++;
        }
        if (numbrackets > 0) {
          argname = "L" + argname + ";";
          while (numbrackets>0) {
            argname = "[" + argname;
            numbrackets--;
          }
        }
        // System.out.println("argname " + i + " = " + argname + " for method " + method);
        argclasses[i] = classForName(argname);
      }
      args_seen.put(all_argnames, argclasses);
    }
    return methodForName(classname, methodname, argclasses);
  }

  public static Method methodForName(String classname, String methodname, Class[] params)
    throws ClassNotFoundException, NoSuchMethodException, SecurityException {

    Class c = Class.forName(classname);
    Method m = c.getDeclaredMethod(methodname, params);
    return m;
  }



  ///
  /// Properties
  ///

  /**
   * Determines whether a property has value "true", "yes", or "1".
   * @see Properties#getProperty
   **/
  public static boolean propertyIsTrue(Properties p, String key) {
    String pvalue = p.getProperty(key);
    if (pvalue == null) {
      return false;
    }
    pvalue = pvalue.toLowerCase();
    return (pvalue.equals("true") || pvalue.equals("yes") || pvalue.equals("1"));
  }

  /**
   * Set the property to its previous value concatenated to the given value.
   * @see Properties#getProperty
   * @see Properties#setProperty
   **/
  public static String appendProperty(Properties p, String key, String value) {
    return (String)p.setProperty(key, p.getProperty(key, "") + value);
  }

  /**
   * Set the property only if it was not previously set.
   * @see Properties#getProperty
   * @see Properties#setProperty
   **/
  public static String setDefault(Properties p, String key, String value) {
    String currentValue = p.getProperty(key);
    if (currentValue == null) {
      p.setProperty(key, value);
    }
    return currentValue;
  }


  ///
  /// Stream
  ///

  /** Copy the contents of the input stream to the output stream. */
  public static void streamCopy(java.io.InputStream from, java.io.OutputStream to) {
    byte[] buffer = new byte[1024];
    int bytes;
    try {
      while (true) {
	bytes = from.read(buffer);
	if (bytes == -1) {
	  return;
	}
	to.write(buffer, 0, bytes);
      }
    } catch (java.io.IOException e) {
      e.printStackTrace();
      throw new Error("" + e);
    }
  }


  ///
  /// String
  ///

  /**
   * Return a new string which is the text of target with all instances of
   * oldStr replaced by newStr.
   **/
  public static String replaceString(String target, String oldStr, String newStr) {
    StringBuffer result = new StringBuffer();
    int lastend = 0;
    int pos;
    while ((pos = target.indexOf(oldStr, lastend)) != -1) {
      result.append(target.substring(lastend, pos));
      result.append(newStr);
      lastend = pos + oldStr.length();
    }
    result.append(target.substring(lastend));
    return result.toString();
  }

  // Consider writing another version of this that takes a String second
  // argument.  Little change to implementation should be required, since
  // indexOf can take a String as argument.
  /**
   * Return an array of Strings representing the characters between
   * successive instances of the delimiter character.
   **/
  public static String[] split(String s, char delim) {
    Vector result = new Vector();
    for (int delimpos = s.indexOf(delim); delimpos != -1; delimpos = s.indexOf(delim)) {
      result.add(s.substring(0, delimpos));
      s = s.substring(delimpos+1);
    }
    result.add(s);
    String[] result_array = new String[result.size()];
    result.copyInto(result_array);
    return result_array;
  }

  /**
   * Concatenate the string representations of the objects, placing the
   * delimiter between them.
   * @see{ArraysMDE.toString(int[])}
   **/
  public static String join(Object[] a, String delim) {
    if (a.length == 0) return "";
    if (a.length == 1) return a[0].toString();
    StringBuffer sb = new StringBuffer(a[0].toString());
    for (int i=1; i<a.length; i++)
      sb.append(delim).append(a[i]);
    return sb.toString();
  }

  /**
   * Concatenate the string representations of the objects, placing the
   * delimiter between them.
   * @see java.util.Vector#toString
   **/
  public static String join(Vector v, String delim) {
    if (v.size() == 0) return "";
    if (v.size() == 1) return v.elementAt(0).toString();
    StringBuffer sb = new StringBuffer(v.elementAt(0).toString());
    for (int i=1; i<v.size(); i++)
      sb.append(delim).append(v.elementAt(i));
    return sb.toString();
  }

  // Inspired by the function of the same name in Ajax (but independent code).
  /**
   * Quote \, ", \n, and \r characters in the target; return a new string.
   **/
  public static String quote(String orig) {
    StringBuffer sb = new StringBuffer();
    // The previous escape character was seen right before this position.
    int post_esc = 0;
    int orig_len = orig.length();
    for (int i=0; i<orig_len; i++) {
      char c = orig.charAt(i);
      switch (c) {
      case '\"':
      case '\\':
        if (post_esc < i) {
          sb.append(orig.substring(post_esc, i));
        }
        sb.append('\\');
        post_esc = i;
        break;
      case '\n':
        if (post_esc < i) {
          sb.append(orig.substring(post_esc, i));
        }
        sb.append("\\n");
        post_esc = i+1;
        break;
      case '\r':
        if (post_esc < i) {
          sb.append(orig.substring(post_esc, i));
        }
        sb.append("\\r");
        post_esc = i+1;
        break;
      }
    }
    if (sb.length() == 0)
      return orig;
    sb.append(orig.substring(post_esc));
    return sb.toString();
  }

  // The overhead of this is too high to call in quote(String)
  public static String quote(Character ch) {
    char c = ch.charValue();
    switch (c) {
    case '\"':
      return("\\\"");
    case '\\':
      return("\\\\");
    case '\n':
      return("\\n");
    case '\r':
      return("\\r");
    default:
      return new String(new char[] { c });
    }
  }

  /**
   * Replace "\\", "\"", "\n", and "\r" sequences by their one-character
   * equivalents.  All other backslashes are removed.
   **/
  public static String unquote(String orig) {
    StringBuffer sb = new StringBuffer();
    // The previous escape character was seen just before this position.
    int post_esc = 0;
    int this_esc = orig.indexOf('\\');
    while (this_esc != -1) {
      if (this_esc == orig.length()) {
        sb.append(orig.substring(post_esc, this_esc+1));
        post_esc = this_esc+1;
        break;
      }
      switch (orig.charAt(this_esc+1)) {
      case 'n':
        sb.append(orig.substring(post_esc, this_esc));
        sb.append('\n');
        post_esc = this_esc+2;
        break;
      case 'r':
        sb.append(orig.substring(post_esc, this_esc));
        sb.append('\r');
        post_esc = this_esc+2;
        break;
      case '\\':
        // This is not in the default case because the search would find
        // the quoted backslash.  Here we incluce the first backslash in
        // the output, but not the first.
        sb.append(orig.substring(post_esc, this_esc+1));
        post_esc = this_esc+2;
        break;
      default:
        sb.append(orig.substring(post_esc, this_esc));
        post_esc = this_esc+1;
        break;
      }
      this_esc = orig.indexOf('\\', post_esc);
    }
    if (post_esc == 0)
      return orig;
    sb.append(orig.substring(post_esc));
    return sb.toString();
  }

  // Use the built-in String.trim()!
  // /** Return the string with all leading and trailing whitespace stripped. */
  // public static String trimWhitespace(String s) {
  //   int len = s.length();
  //   if (len == 0)
  //     return s;
  //   int first_non_ws = 0;
  //   int last_non_ws = len-1;
  //   while ((first_non_ws < len) && Character.isWhitespace(s.charAt(first_non_ws)))
  //     first_non_ws++;
  //   if (first_non_ws == len)
  //     return "";
  //   while (Character.isWhitespace(s.charAt(last_non_ws)))
  //     last_non_ws--;
  //   if ((first_non_ws == 0) && (last_non_ws == len))
  //     return s;
  //   else
  //     return s.substring(first_non_ws, last_non_ws+1);
  // }
  // // // Testing:
  // // assert(UtilMDE.trimWhitespace("foo").equals("foo"));
  // // assert(UtilMDE.trimWhitespace(" foo").equals("foo"));
  // // assert(UtilMDE.trimWhitespace("    foo").equals("foo"));
  // // assert(UtilMDE.trimWhitespace("foo ").equals("foo"));
  // // assert(UtilMDE.trimWhitespace("foo    ").equals("foo"));
  // // assert(UtilMDE.trimWhitespace("  foo   ").equals("foo"));
  // // assert(UtilMDE.trimWhitespace("  foo  bar   ").equals("foo  bar"));
  // // assert(UtilMDE.trimWhitespace("").equals(""));
  // // assert(UtilMDE.trimWhitespace("   ").equals(""));


  // @return either "n noun" or "n nouns" depending on n
  public static String nplural(int n, String noun)
  {
    if (n == 1)
      return n + " " + noun;
    else if (noun.endsWith("s") || noun.endsWith("x") ||
             noun.endsWith("ch") || noun.endsWith("sh"))
      return n + " " + noun + "es";
    else
      return n + " " + noun + "s";
  }


  ///
  /// StringTokenizer
  ///

  /** Return a Vector of the Strings returned by @link{StringTokenizer(String,String,boolean)} with the given arguments. */
  public static Vector tokens(String str, String delim, boolean returnTokens) {
    return makeVector(new StringTokenizer(str, delim, returnTokens));
  }

  /** Return a Vector of the Strings returned by @link{StringTokenizer(String,String)} with the given arguments. */
  public static Vector tokens(String str, String delim) {
    return makeVector(new StringTokenizer(str, delim));
  }

  /** Return a Vector of the Strings returned by @link{StringTokenizer(String)} with the given arguments. */
  public static Vector tokens(String str) {
    return makeVector(new StringTokenizer(str));
  }



  ///
  /// Vector
  ///

  /** Returns a vector containing the elements of the enumeration. */
  public static Vector makeVector(Enumeration e) {
    Vector result = new Vector();
    while (e.hasMoreElements()) {
      result.addElement(e.nextElement());
    }
    return result;
  }

  // Rather than writing something like VectorToStringArray, use
  //   v.toArray(new String[0])


}
