// For tests of the entire utilMDE package, see class TestUtilMDE.

package utilMDE;

import java.util.*;
import java.io.*;
import java.util.zip.*;
// import Assert;

// Yes, I know the name is very close to that of the package.
/** Utility functions that do not belong elsewhere in the utilMDE package. */
public class UtilMDE {

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
   */
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
   */
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
  public static class WildcardFilter implements FilenameFilter {
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
  /// Iterator
  ///

  // Making these functions didn't work because I couldn't get their
  // arguments into a scope that Java was happy with.

  /** Converts an Enumeration into an Iterator. */
  public static class EnumerationIterator implements Iterator {
    Enumeration e;
    public EnumerationIterator(Enumeration e_) { e = e_; }
    public boolean hasNext() { return e.hasMoreElements(); }
    public Object next() { return e.nextElement(); }
    public void remove() { throw new UnsupportedOperationException(); }
  }

  /** Converts an Iterator into an Enumeration. */
  public static class IteratorEnumeration implements Enumeration {
    Iterator itor;
    public IteratorEnumeration(Iterator itor_) { itor = itor_; }
    public boolean hasMoreElements() { return itor.hasNext(); }
    public Object nextElement() { return itor.next(); }
  }

  // This must already be implemented someplace else.  Right??
  /**
   * An Iterator that returns first the elements returned by its first
   * argument, then the elements returned by its second argument.
   */
  public static class MergedIterator2 implements Iterator {
    Iterator itor1, itor2;
    public MergedIterator2(Iterator itor1_, Iterator itor2_) {
      itor1 = itor1_; itor2 = itor2_;
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
   */
  public static class MergedIterator implements Iterator {
    Iterator itorOfItors;
    public MergedIterator(Iterator itorOfItors_) {itorOfItors = itorOfItors_; }

    // an empty iterator to prime the pump
    Iterator current = Collections.EMPTY_SET.iterator();

    public boolean hasNext() {
      while ((!current.hasNext()) && (itorOfItors.hasNext())) {
        current = (Iterator) itorOfItors.next();
      }
      return current.hasNext();
    }

    public Object next() {
      return current.next();
    }

    public void remove() {
      throw new UnsupportedOperationException();
    }
  }

  public static class FilteredIterator implements Iterator {
    Iterator itor;
    Filter filter;

    public FilteredIterator(Iterator itor_, Filter filter_) {
      itor = itor_; filter = filter_;
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
  /// Properties
  ///

  /**
   * Determines whether a property has value "true", "yes", or "1".
   * @see Properties#getProperty
   */
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
   */
  public static String appendProperty(Properties p, String key, String value) {
    return (String)p.setProperty(key, p.getProperty(key, "") + value);
  }

  /**
   * Set the property only if it was not previously set.
   * @see Properties#getProperty
   * @see Properties#setProperty
   */
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
   */
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


  /**
   * Concatenate the string representations of the objects, placing the
   * delimiter between them.
   * @see{ArraysMDE.toString(int[])}
   */
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
   */
  public static String join(Vector v, String delim) {
    if (v.size() == 0) return "";
    if (v.size() == 1) return v.elementAt(0).toString();
    StringBuffer sb = new StringBuffer(v.elementAt(0).toString());
    for (int i=1; i<v.size(); i++)
      sb.append(delim).append(v.elementAt(i));
    return sb.toString();
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
