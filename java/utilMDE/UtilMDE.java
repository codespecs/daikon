// For tests of the entire utilMDE package, see class TestUtilMDE.

package utilMDE;

import java.io.*;
import java.util.*;
import java.util.zip.*;
import java.lang.reflect.*;
// import Assert;

// The class name "UtilMDE" is very close to the package name "utilMDE".
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
   * Returns a BufferedReader for the file, accounting for the possibility
   * that the file is compressed.
   * <p>
   * Warning: The "gzip" program writes and reads files containing
   * concatenated gzip files.  As of Java 1.3, Java reads
   * just the first one:  it silently discards all characters (including
   * gzipped files) after the first gzipped file.
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
   * Returns a LineNumberReader for the file, accounting for the possibility
   * that the file is compressed.
   * <p>
   * Warning: The "gzip" program writes and reads files containing
   * concatenated gzip files.  As of Java 1.3, Java reads
   * just the first one:  it silently discards all characters (including
   * gzipped files) after the first gzipped file.
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

  /**
   * Returns a BufferedWriter for the file, accounting for the possibility
   * that the file is compressed.
   * <p>
   * Warning: The "gzip" program writes and reads files containing
   * concatenated gzip files.  As of Java 1.3, Java reads
   * just the first one:  it silently discards all characters (including
   * gzipped files) after the first gzipped file.
   **/
  public static BufferedWriter BufferedFileWriter(String filename) throws IOException{
    Writer file_writer;
    if (filename.endsWith(".gz")) {
      file_writer = new OutputStreamWriter(new GZIPOutputStream(new FileOutputStream(filename)));
    } else {
      file_writer = new FileWriter(filename);
    }
    return new BufferedWriter(file_writer);
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
    primitiveClassesJvm.put("long", "J");
    primitiveClassesJvm.put("short", "S");
  }

  /** Convert a string of the form "java.lang.Object[]" to "[Ljava/lang/Object;" **/
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
    return result.replace('.', '/');
  }

  public static String arglistToJvm(String arglist) {
    throw new Error("Not yet implemented");
  }

  private static HashMap primitiveClassesFromJvm = new HashMap(8);
  static {
    primitiveClassesFromJvm.put("Z", "boolean");
    primitiveClassesFromJvm.put("B", "byte");
    primitiveClassesFromJvm.put("C", "char");
    primitiveClassesFromJvm.put("D", "double");
    primitiveClassesFromJvm.put("F", "float");
    primitiveClassesFromJvm.put("I", "int");
    primitiveClassesFromJvm.put("J", "long");
    primitiveClassesFromJvm.put("S", "short");
  }

  /**
   * Convert a classname from JVML format to Java format.
   * For instance, convert "[Ljava/lang/Object;" to "java.lang.Object[]".
   **/
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
        // // throw new Error("Malformed base class: " + classname);
        // // For now, hope for the best.
        // result = classname;
	// Never hope for the best, because you're probably wrong
	return null;
      }
    }
    for (int i=0; i<dims; i++) {
      result += "[]";
    }
    return result.replace('/', '.');
  }

  /**
   * Convert an argument list from JVML format to Java format.
   * For instance, convert "([Ljava/lang/Integer;I[[Ljava/lang/Integer;)"
   * to "(java.lang.Integer[], int, java.lang.Integer[][])".
   **/
  public static String arglistFromJvm(String arglist) {
    if (! (arglist.startsWith("(") && arglist.endsWith(")"))) {
      throw new Error("Malformed arglist");
    }
    String result = "(";
    int pos = 1;
    while (pos < arglist.length()-1) {
      if (pos > 1)
        result += ", ";
      int nonarray_pos = pos;
      while (arglist.charAt(nonarray_pos) == '[') {
        nonarray_pos++;
      }
      char c = arglist.charAt(nonarray_pos);
      if (c == 'L') {
        int semi_pos = arglist.indexOf(";", nonarray_pos);
        result += classnameFromJvm(arglist.substring(pos, semi_pos+1));
        pos = semi_pos + 1;
      } else {
	String maybe = classnameFromJvm(arglist.substring(pos, nonarray_pos+1));
	if (maybe == null) {
	  return null;
	}
        result += maybe;
        pos = nonarray_pos+1;
      }
    }
    return result + ")";
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


  // Returns true if:
  // 1. The file exists and is writable
  // OR
  // 2. The file can be created
  public static boolean canCreateAndWrite(File file) {
    try {
      if (file.exists()) {
        return file.canWrite();
      } else {
        file.createNewFile();
        file.delete();
        return true;
      }
    } catch (IOException e) {
      return false;
    }
  }


  /**
   * Writes an Object to a File
   **/
  public static void writeObject(Object o, File file) throws IOException {
    // 8192 is the buffer size in BufferedReader
    OutputStream bytes =
      new BufferedOutputStream(new FileOutputStream(file), 8192);
    if (file.getName().endsWith(".gz")) {
      bytes = new GZIPOutputStream(bytes);
    }
    ObjectOutputStream objs = new ObjectOutputStream(bytes);
    objs.writeObject(o);
    objs.close();
  }


  /**
   * Reads an Object from a File
   **/
  public static Object readObject(File file) throws
  IOException, ClassNotFoundException {
    // 8192 is the buffer size in BufferedReader
    InputStream istream =
      new BufferedInputStream(new FileInputStream(file), 8192);
    if (file.getName().endsWith(".gz")) {
      istream = new GZIPInputStream(istream);
    }
    ObjectInputStream objs = new ObjectInputStream(istream);
    return objs.readObject();
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

  /**
   * Returns an iterator just like its argument, except that the first and
   * last elements are removed.  They can be accessed via the getFirst and
   * getLast methods.
   **/
  public static final class RemoveFirstAndLastIterator implements Iterator {
    Iterator itor;
    Object nothing = new Object();
    Object first = nothing;
    Object current = nothing;

    public RemoveFirstAndLastIterator(Iterator itor) {
      this.itor = itor;
      if (itor.hasNext()) {
        first = itor.next();
      }
      if (itor.hasNext()) {
        current = itor.next();
      }
    }

    public boolean hasNext() {
      return itor.hasNext();
    }

    public Object next() {
      if (! itor.hasNext()) {
        throw new NoSuchElementException();
      }
      Object tmp = current;
      current = itor.next();
      return tmp;
    }

    public Object getFirst() {
      if (first == nothing) {
        throw new NoSuchElementException();
      }
      return first;
    }

    public Object getLast() {
      if (itor.hasNext()) {
        throw new Error();
      }
      return current;
    }

    public void remove() {
      throw new UnsupportedOperationException();
    }
  }


  /**
   * Return an ArrayList containing num_elts randomly chosen
   * elements from the iterator, or all the elements of the iterator if
   * there are fewer.  It examines every element of the iterator, but does
   * not keep them all in memory.
   **/
  public static ArrayList randomElements(Iterator itor, int num_elts) {
    return randomElements(itor, num_elts, r);
  }
  private static Random r = new Random();

  /**
   * Return an ArrayList containing num_elts randomly chosen
   * elements from the iterator, or all the elements of the iterator if
   * there are fewer.  It examines every element of the iterator, but does
   * not keep them all in memory.
   **/
  public static ArrayList randomElements(Iterator itor, int num_elts, Random random) {
    // The elements are chosen with the following probabilities,
    // where n == num_elts:
    //   n n/2 n/3 n/4 n/5 ...

    RandomSelector rs = new RandomSelector (num_elts, random);

    while (itor.hasNext()) {
      rs.accept (itor.next());
    }
    return rs.getValues();


    /*
    ArrayList result = new ArrayList(num_elts);
    int i=1;
    for (int n=0; n<num_elts && itor.hasNext(); n++, i++) {
      result.add(itor.next());
    }
    for (; itor.hasNext(); i++) {
      Object o = itor.next();
      // test random < num_elts/i
      if (random.nextDouble() * i < num_elts) {
        // This element will replace one of the existing elements.
        result.set(random.nextInt(num_elts), o);
      }
    }
    return result;
    */
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
   * Always returns an array of length at least 1 (it might contain only the
   * empty string).
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
   * Return an array of Strings representing the characters between
   * successive instances of the delimiter String.
   * Always returns an array of length at least 1 (it might contain only the
   * empty string).
   **/
  public static String[] split(String s, String delim) {
    int delimlen = delim.length();
    if (delimlen == 0) {
      throw new Error("Second argument to split was empty.");
    }
    Vector result = new Vector();
    for (int delimpos = s.indexOf(delim); delimpos != -1; delimpos = s.indexOf(delim)) {
      result.add(s.substring(0, delimpos));
      s = s.substring(delimpos+delimlen);
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
    if (a.length == 1) return String.valueOf(a[0]);
    StringBuffer sb = new StringBuffer(String.valueOf(a[0]));
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
      if (this_esc == orig.length()-1) {
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
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
        // Here we should convert octal escapes into characters.
        // For now, fall through.
      default:
        // In the default case, retain the character following the backslash,
        // but discard the backslash itself.  "\*" is just a one-character string.
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


  /** Remove all whitespace before or after instances of delimiter. **/
  public static String removeWhitespaceAround(String arg, String delimiter) {
    arg = removeWhitespaceBefore(arg, delimiter);
    arg = removeWhitespaceAfter(arg, delimiter);
    return arg;
  }

  /** Remove all whitespace after instances of delimiter. **/
  public static String removeWhitespaceAfter(String arg, String delimiter) {
    // String orig = arg;
    int delim_len = delimiter.length();
    int delim_index = arg.indexOf(delimiter);
    while (delim_index > -1) {
      int non_ws_index = delim_index+delim_len;
      while ((non_ws_index < arg.length())
             && (Character.isWhitespace(arg.charAt(non_ws_index)))) {
        non_ws_index++;
      }
      // if (non_ws_index == arg.length()) {
      //   System.out.println("No nonspace character at end of: " + arg);
      // } else {
      //   System.out.println("'" + arg.charAt(non_ws_index) + "' not a space character at " + non_ws_index + " in: " + arg);
      // }
      if (non_ws_index != delim_index+delim_len) {
        arg = arg.substring(0, delim_index + delim_len) + arg.substring(non_ws_index);
      }
      delim_index = arg.indexOf(delimiter, delim_index+1);
    }
    return arg;
  }

  /** Remove all whitespace before instances of delimiter. **/
  public static String removeWhitespaceBefore(String arg, String delimiter) {
    // System.out.println("removeWhitespaceBefore(\"" + arg + "\", \"" + delimiter + "\")");
    // String orig = arg;
    int delim_len = delimiter.length();
    int delim_index = arg.indexOf(delimiter);
    while (delim_index > -1) {
      int non_ws_index = delim_index-1;
      while ((non_ws_index >= 0)
             && (Character.isWhitespace(arg.charAt(non_ws_index)))) {
        non_ws_index--;
      }
      // if (non_ws_index == -1) {
      //   System.out.println("No nonspace character at front of: " + arg);
      // } else {
      //   System.out.println("'" + arg.charAt(non_ws_index) + "' not a space character at " + non_ws_index + " in: " + arg);
      // }
      if (non_ws_index != delim_index-1) {
        arg = arg.substring(0, non_ws_index + 1) + arg.substring(delim_index);
      }
      delim_index = arg.indexOf(delimiter, non_ws_index+2);
    }
    return arg;
  }


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


  // Returns a string of the specified length, truncated if necessary,
  // and padded with spaces to the right if necessary.
  public static String rpad(String s, int length) {
    if (s.length() < length) {
      StringBuffer buf = new StringBuffer(s);
      for (int i = s.length(); i < length; i++) {
        buf.append(' ');
      }
      return buf.toString();
    } else {
      return s.substring(0, length);
    }
  }

  // Converts the int to a String, then formats it using rpad
  public static String rpad(int num, int length) {
    return rpad(String.valueOf(num), length);
  }

  // Converts the doubleto a String, then formats it using rpad
  public static String rpad(double num, int length) {
    return rpad(String.valueOf(num), length);
  }

  // Same as built-in String comparison, but accept null arguments,
  // and place them at the beginning.
  public static class NullableStringComparator
    implements Comparator
  {
    public int compare(Object o1, Object o2) {
      String s1 = (String) o1;
      String s2 = (String) o2;
      if (s1 == null && s2 == null) return 0;
      if (s1 == null && s2 != null) return 1;
      if (s1 != null && s2 == null) return -1;
      return s1.compareTo(s2);
    }
  }

  /** Return the number of times the character appears in the string. **/
  public static int count(String s, int ch) {
    int result = 0;
    int pos = s.indexOf(ch);
    while (pos > -1) {
      result++;
      pos = s.indexOf(ch, pos+1);
    }
    return result;
  }

  /** Return the number of times the second string appears in the first. **/
  public static int count(String s, String sub) {
    int result = 0;
    int pos = s.indexOf(sub);
    while (pos > -1) {
      result++;
      pos = s.indexOf(sub, pos+1);
    }
    return result;
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
  /// Collections
  ///

  /**
   * Returns the sorted version of the list.  Does not alter the list.
   **/
  public static List sortList (List l, Comparator c) {
    Object[] buf = new Object[l.size()];
    buf = l.toArray(buf);
    Arrays.sort (buf, c);
    return Arrays.asList(buf);
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
