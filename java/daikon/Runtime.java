// Run-time support for dfej: Daikon Front End for Java.
// This is not really part of the Daikon invariant detection engine proper,
// but dfej and Daikon will be linked via it.

package daikon;

import java.util.List;
import java.util.zip.GZIPOutputStream;
import java.io.*;


public final class Runtime {

  // Constructor
  private Runtime() {
    throw new Error("Do not create instances of Runtime");
  }

  ////////////////////////////////////////////////////////////////////////
  /// Fresh (unique) classname used to disambiguate overloaded method
  /// calls during instrumentation; is not instantiated or used.
  public static class Unique {}
  public static final Unique unique = null;

  ////////////////////////////////////////////////////////////////////////
  /// The context-sensitive instrumentation pass creates bodies for
  /// abstract methods that throw this very object; that way we don't
  /// have to inspect their return type since they never return.
  /// Thanks to this global instance, they don't need to call "new" either.
  public static class AbstractException extends Error {
    static final long serialVersionUID = 20020130L;
  }
  public static final AbstractException abstractException =
    new AbstractException();

  ///////////////////////////////////////////////////////////////////////////
  /// Timestamps
  ///

  public static int time = 0;


  ///////////////////////////////////////////////////////////////////////////
  /// Printing
  ///

  // This flag is used to suppress output during reentrant
  // instrumentation; for example, when outputting the elements of an
  // instrumented List-derived object, we don't want to start
  // outputting the program points in the size()/get() methods of the
  // List object.
  public static int ps_count = 0;

  public static final class PrintStreamWithThrottle extends PrintStream
  {
    // Use "count <= 1" for compatibility with older dfej
    public void print(boolean b)   { if (count <= 1) super.print(b); }
    public void print(char c)      { if (count <= 1) super.print(c); }
    public void print(char[] s)    { if (count <= 1) super.print(s); }
    public void print(double d)    { if (count <= 1) super.print(d); }
    public void print(float f)     { if (count <= 1) super.print(f); }
    public void print(int i)       { if (count <= 1) super.print(i); }
    public void print(long l)      { if (count <= 1) super.print(l); }
    public void print(Object obj)  { if (count <= 1) super.print(obj); }
    public void print(String s)    { if (count <= 1) super.print(s); }
    public void println()          { if (count <= 1) super.println(); }
    public void println(boolean x) { if (count <= 1) super.println(x); }
    public void println(char x)    { if (count <= 1) super.println(x); }
    public void println(char[] x)  { if (count <= 1) super.println(x); }
    public void println(double x)  { if (count <= 1) super.println(x); }
    public void println(float x)   { if (count <= 1) super.println(x); }
    public void println(int x)     { if (count <= 1) super.println(x); }
    public void println(long x)    { if (count <= 1) super.println(x); }
    public void println(Object x)  { if (count <= 1) super.println(x); }
    public void println(String x)  { if (count <= 1) super.println(x); }

    // useful for emitting comments during debugging:
    public void printlnAlways(String s) { super.println(s); }

    // This flags is used to suppress output during reentrant
    // instrumentation; for example, when outputting the elements of an
    // instrumented List-derived object, we don't want to start
    // outputting the program points in the size()/get() methods of the
    // List object.
    public int count = 0;

    public PrintStreamWithThrottle(OutputStream out)
    { super(out); }
  }

  // It's convenient to have an entire run in one data trace file, so
  // probably don't bother to generalize this to put output from a single
  // run in different files depending on the class the information is
  // about.
  public static PrintStreamWithThrottle dtrace;

  public static void setDtrace(String filename, boolean append) {
    // System.out.println("calling setDtrace(" + filename + ")...");
    try {
      File file = new File(filename);
      File parent = file.getParentFile();
      if (parent != null) parent.mkdirs();
      OutputStream os = new FileOutputStream(filename, append);
      if (filename.endsWith(".gz")) {
        if (append)
          throw new Error("DTRACEAPPEND environment variable is set."
                          + "\nCannot append to gzipped dtrace file " + filename);
        os = new GZIPOutputStream(os);
      }
      // 8192 is the buffer size in BufferedReader
      dtrace = new PrintStreamWithThrottle(new BufferedOutputStream(os, 8192));
    } catch (Exception e) {
      e.printStackTrace();
      throw new Error("" + e);
    }
    if (supportsAddShutdownHook()) {
      addShutdownHook();
    } else {
      System.err.println("Warning: .dtrace file may be incomplete if program is aborted");
    }
    // System.out.println("...done calling setDtrace(" + filename + ")");
  }

  /**
   * If the current data trace file is not yet set, then set it.
   * The value of the DTRACEFILE environment variable is used;
   * if that environment variable is not set, then the argument
   * to this method is used instead.
   **/
  public static void setDtraceMaybe(String default_filename) {
    // System.out.println("setDtraceMaybe(" + default_filename + "); old = " + dtrace);
    if (dtrace == null) {
      // Jeremy used "daikon.dtrace.filename".
      String filename = System.getProperty("DTRACEFILE", default_filename);
      boolean append = System.getProperty("DTRACEAPPEND") != null;
      setDtrace(filename, append);
    }
  }

  private static boolean supportsAddShutdownHook() {
    try {
      Class rt = java.lang.Runtime.class;
      rt.getMethod("addShutdownHook", new Class[] {
	java.lang.Thread.class
      });
      return true;
    } catch (Exception e) {
      return false;
    }
  }

  // Add a shutdown hook to close the PrintStream when the program
  // exits
  private static void addShutdownHook() {
    java.lang.Runtime.getRuntime().addShutdownHook(new Thread() {
        public void run() {
          if (dtrace != null) {

	    // When the program being instrumented exits, the buffers
	    // of the "dtrace" (PrintStream) object are not flushed,
	    // so we miss the tail of the file.

	    synchronized (daikon.Runtime.dtrace)
	    {
	      dtrace.println();
	      dtrace.println("// EOF"); // this lets us know we didn't lose any
	      dtrace.close();
	    }
          }
        }
      });
  }

  // This is no longer necessary, as it was for Daikon-jtb
  // // This is a dummy method that can be called from Java code instead of
  // //   SomeClass.daikonPrint
  // // because daikonPrint doesn't (yet) exist in SomeClass.java.
  // // Later we will fix up all references to this.
  // public static void daikonPrint_dummy(Object x, PrintStream ps, int depth, String prefix, String target) {
  //   throw new Error("Unreplaced call to DaikonRuntime.daikonPrint_dummy(" + x + ", " + ps + ", " + depth + ", " + prefix + ", " + target + ")");
  // }


  // Some of these functions could be open-coded, but I don't want to get
  // into the business of writing lots of bytecodes; let the JIT inline
  // them.

  // The other advantage to dynamic generation is that it works for
  // arbitrary types, not just those hard-coded here.  That is a big
  // advantage.


  ///////////////////////////////////////////////////////////////////////////
  /// print
  ///

  // I used to have overloaded print and println methods (that called
  // print_Object and print_String respectively), but that could give me
  // unexpected results if the Object I was trying to print happened to be
  // a String.  So now I use different names to avoid that problem.

  public static final void print_Object(java.io.PrintStream ps, Object x) {
    if (x == null) {
      ps.print("null");
      return;
    }
    ps.print(java.lang.System.identityHashCode(x));
  }

  // augmentation of print_Object above
  public static final void print_class(java.io.PrintStream ps, Object x) {
    if (x == null) {
      ps.print("null");
    } else {
      print_String(ps, x.getClass().getName());
    }
  }

  // Avoid using this; prefer print_quoted_String instead, unless we can
  // guarantee that the string contains no character that need to be quoted.
  public static final void print_String(java.io.PrintStream ps, String x) {
    ps.print((x == null) ? "null" : "\"" + x + "\"");
  }

  public static final void print_quoted_String(java.io.PrintStream ps, String x) {
    ps.print((x == null) ? "null" : "\"" + quote(x) + "\"");
  }

  // Not yet used; but probably should be.
  public static final void print_quoted_Character(java.io.PrintStream ps, Character ch) {
    ps.print((ch == null) ? "null" : quote(ch));
  }

  // Lifted directly from utilMDE/UtilMDE.java, but repeated here to make
  // this class self-contained.
  /**
   * Quote \, ", \n, and \r characters in the target; return a new string.
   **/
  public static String quote(String orig) {
    StringBuffer sb = new StringBuffer();
    // The previous escape (or escaped) character was seen right before
    // this position.  Alternately:  from this character forward, the string
    // should be copied out verbatim (until the next escaped character).
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


  ///////////////////////////////////////////////////////////////////////////
  /// println
  ///

  public static final void println_Object(java.io.PrintStream ps, Object x) {
    print_Object(ps, x);
    ps.println();
  }

  public static final void println_class(java.io.PrintStream ps, Object x) {
    print_class(ps, x);
    ps.println();
  }

  // Avoid using this; prefer println_quoted_String instead.
  public static final void println_String(java.io.PrintStream ps, String x) {
    print_String(ps, x);
    ps.println();
  }

  public static final void println_quoted_String(java.io.PrintStream ps, String x) {
    print_quoted_String(ps, x);
    ps.println();
  }

  ///////////////////////////////////////////////////////////////////////////
  /// println_array
  ///

  // These are all cut-and-paste (the code is identical in some cases).

  ///
  /// Object
  ///

  public static final void println_array_Object(java.io.PrintStream ps, Object[] a) {
    if (a == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    if (a.length > 0) {
      print_Object(ps, a[0]);
      for (int i=1; i<a.length; i++) {
        ps.print(' ');
        print_Object(ps, a[i]);
      }
    }
    ps.println(']');
  }

  public static final void println_array_Object(java.io.PrintStream ps, List v) {
    if (v == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    int size = v.size();
    if (size > 0) {
      print_Object(ps, v.get(0));
      for (int i=1; i<size; i++) {
        ps.print(' ');
        print_Object(ps, v.get(i));
      }
    }
    ps.println(']');
  }

  // Print an array of the classes of the elements.
  public static final void println_array_Object_eltclass(java.io.PrintStream ps, Object[] a) {
    if (a == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    if (a.length > 0) {
      print_class(ps, a[0]);
      for (int i=1; i<a.length; i++) {
        ps.print(' ');
        print_class(ps, a[i]);
      }
    }
    ps.println(']');
  }

  // Print an array of the classes of the elements.
  public static final void println_array_Object_eltclass(java.io.PrintStream ps, List v) {
    if (v == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    int size = v.size();
    if (size > 0) {
      print_class(ps, v.get(0));
      for (int i=1; i<size; i++) {
        ps.print(' ');
        print_class(ps, v.get(i));
      }
    }
    ps.println(']');
  }

  // Print the lengths of the elements of the top-level array.
  // This is for Object[][] or for anything[][][], where "anything" may
  // be either Object or a base class.
  public static final void println_array_2d_size(java.io.PrintStream ps, Object[][] a) {
    if (a == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    if (a.length > 0) {
      ps.print((a[0]).length);
      for (int i=1; i<a.length; i++) {
        ps.print(' ');
        ps.print((a[i]).length);
      }
    }
    ps.println(']');
  }


  ///
  /// List
  ///

  // Print the lengths of the elements of a List[]

  public static final void println_array_List_size(java.io.PrintStream ps, List[] a) {
    if (a == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    if (a.length > 0) {
      ps.print(a[0].size());
      for (int i=1; i<a.length; i++) {
        ps.print(' ');
        ps.print(a[i].size());
      }
    }
    ps.println(']');
  }

  public static final void println_array_List_size(java.io.PrintStream ps, Object[] a) {
    if (a == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    if (a.length > 0) {
      ps.print(((List)a[0]).size());
      for (int i=1; i<a.length; i++) {
        ps.print(' ');
        ps.print(((List)a[i]).size());
      }
    }
    ps.println(']');
  }

  public static final void println_array_List_size(java.io.PrintStream ps, List v) {
    if (v == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    int size = v.size();
    if (size > 0) {
      ps.print(((List)v.get(0)).size());
      for (int i=1; i<size; i++) {
        ps.print(' ');
        ps.print(((List)v.get(i)).size());
      }
    }
    ps.println(']');
  }

  ///
  /// String
  ///

  public static final void println_array_String(java.io.PrintStream ps, String[] a) {
    if (a == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    if (a.length > 0) {
      print_quoted_String(ps, a[0]);
      for (int i=1; i<a.length; i++) {
        ps.print(' ');
        print_quoted_String(ps, a[i]);
      }
    }
    ps.println(']');
  }

  public static final void println_array_String(java.io.PrintStream ps, Object[] a) {
    if (a == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    if (a.length > 0) {
      print_quoted_String(ps, (String)a[0]);
      for (int i=1; i<a.length; i++) {
        ps.print(' ');
        print_quoted_String(ps, (String)a[i]);
      }
    }
    ps.println(']');
  }

  public static final void println_array_String(java.io.PrintStream ps, List v) {
    if (v == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    int size = v.size();
    if (size > 0) {
      print_quoted_String(ps, (String)v.get(0));
      for (int i=1; i<size; i++) {
        ps.print(' ');
        print_quoted_String(ps, (String)v.get(i));
      }
    }
    ps.println(']');
  }

  ///
  /// Primitive types (mostly numbers)
  ///

  // The primitive types are:
  //   boolean byte char double float int long short
  // Each of the sections should be identical up to renaming the primitive
  // types, so if one is changed, all the others should be, too.

  /// boolean

  public static final void println_array_boolean(java.io.PrintStream ps, boolean[] a) {
    if (a == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    if (a.length > 0) {
      ps.print(a[0]);
      for (int i=1; i<a.length; i++) {
        ps.print(' ');
        ps.print(a[i]);
      }
    }
    ps.println(']');
  }

  public static final void println_array_boolean(java.io.PrintStream ps, Object[] a) {
    if (a == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    if (a.length > 0) {
      ps.print(((Boolean)a[0]).booleanValue());
      for (int i=1; i<a.length; i++) {
        ps.print(' ');
        ps.print(((Boolean)a[i]).booleanValue());
      }
    }
    ps.println(']');
  }

  public static final void println_array_boolean(java.io.PrintStream ps, List v) {
    if (v == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    int size = v.size();
    if (size > 0) {
      ps.print(((Boolean)v.get(0)).booleanValue());
      for (int i=1; i<size; i++) {
        ps.print(' ');
        ps.print(((Boolean)v.get(i)).booleanValue());
      }
    }
    ps.println(']');
  }

  // Print the lengths of the elements of the top-level array.
  public static final void println_array_2d_size(java.io.PrintStream ps, boolean[][] a) {
    if (a == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    if (a.length > 0) {
      ps.print((a[0]).length);
      for (int i=1; i<a.length; i++) {
        ps.print(' ');
        ps.print((a[i]).length);
      }
    }
    ps.println(']');
  }

  /// byte

  public static final void println_array_byte(java.io.PrintStream ps, byte[] a) {
    if (a == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    if (a.length > 0) {
      ps.print(a[0]);
      for (int i=1; i<a.length; i++) {
        ps.print(' ');
        ps.print(a[i]);
      }
    }
    ps.println(']');
  }

  public static final void println_array_byte(java.io.PrintStream ps, Object[] a) {
    if (a == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    if (a.length > 0) {
      ps.print(((Byte)a[0]).byteValue());
      for (int i=1; i<a.length; i++) {
        ps.print(' ');
        ps.print(((Byte)a[i]).byteValue());
      }
    }
    ps.println(']');
  }

  public static final void println_array_byte(java.io.PrintStream ps, List v) {
    if (v == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    int size = v.size();
    if (size > 0) {
      ps.print(((Byte)v.get(0)).byteValue());
      for (int i=1; i<size; i++) {
        ps.print(' ');
        ps.print(((Byte)v.get(i)).byteValue());
      }
    }
    ps.println(']');
  }

  // Print the lengths of the elements of the top-level array.
  public static final void println_array_2d_size(java.io.PrintStream ps, byte[][] a) {
    if (a == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    if (a.length > 0) {
      ps.print((a[0]).length);
      for (int i=1; i<a.length; i++) {
        ps.print(' ');
        ps.print((a[i]).length);
      }
    }
    ps.println(']');
  }

  /// char

  public static final void println_array_char(java.io.PrintStream ps, char[] a) {
    println_array_char_as_String(ps, a);
  }

  public static final void println_array_char(java.io.PrintStream ps, Object[] a) {
    println_array_char_as_chars(ps, a);
  }

  public static final void println_array_char(java.io.PrintStream ps, List v) {
    println_array_char_as_chars(ps, v);
  }

  public static final void println_array_char_as_String(java.io.PrintStream ps, char[] a) {
    if (a == null) {
      ps.println("null");
      return;
    }
    println_quoted_String(ps, new String(a));
  }

  // Outputs a sequence of space-separated characters, with (only) return
  // and newline quoted.  (Should backslash also be quoted?)
  public static final void println_array_char_as_chars(java.io.PrintStream ps, Object[] a) {
    if (a == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    for (int i=0; i<a.length; i++) {
      if (i != 0)
        ps.print(' ');
      char c = ((Character)a[0]).charValue();
      if (c == '\r')
        ps.print("\\r");
      else if (c == '\n')
        ps.print("\\n");
      else
        ps.print(c);
    }
    ps.println(']');
  }

  // Outputs a sequence of space-separated characters, with (only) return
  // and newline quoted.  (Should backslash also be quoted?)
  public static final void println_array_char_as_chars(java.io.PrintStream ps, List v) {
    if (v == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    int size = v.size();
    for (int i=0; i<size; i++) {
      if (i != 0)
        ps.print(' ');
      char c = ((Character)v.get(i)).charValue();
      if (c == '\r')
        ps.print("\\r");
      else if (c == '\n')
        ps.print("\\n");
      else
        ps.print(c);
    }
    ps.println(']');
  }

  public static final void println_array_char_as_ints(java.io.PrintStream ps, char[] a) {
    if (a == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    if (a.length > 0) {
      ps.print(Character.getNumericValue(a[0]));
      for (int i=1; i<a.length; i++) {
        ps.print(' ');
        ps.print(Character.getNumericValue(a[i]));
      }
    }
    ps.println(']');
  }

  public static final void println_array_char_as_ints(java.io.PrintStream ps, Object[] a) {
    if (a == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    if (a.length > 0) {
      ps.print(Character.getNumericValue(((Character)a[0]).charValue()));
      for (int i=1; i<a.length; i++) {
        ps.print(' ');
        ps.print(Character.getNumericValue(((Character)a[i]).charValue()));
      }
    }
    ps.println(']');
  }

  public static final void println_array_char_as_ints(java.io.PrintStream ps, List v) {
    if (v == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    int size = v.size();
    if (size > 0) {
      ps.print(Character.getNumericValue(((Character)v.get(0)).charValue()));
      for (int i=1; i<size; i++) {
        ps.print(' ');
        ps.print(Character.getNumericValue(((Character)v.get(i)).charValue()));
      }
    }
    ps.println(']');
  }

  // I'm not sure if this is what I want -- I might prefer to view it as String[].
  // Print the lengths of the elements of the top-level array.
  public static final void println_array_2d_size(java.io.PrintStream ps, char[][] a) {
    if (a == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    if (a.length > 0) {
      ps.print((a[0]).length);
      for (int i=1; i<a.length; i++) {
        ps.print(' ');
        ps.print((a[i]).length);
      }
    }
    ps.println(']');
  }

  /// double

  public static final void println_array_double(java.io.PrintStream ps, double[] a) {
    if (a == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    if (a.length > 0) {
      ps.print(a[0]);
      for (int i=1; i<a.length; i++) {
        ps.print(' ');
        ps.print(a[i]);
      }
    }
    ps.println(']');
  }

  public static final void println_array_double(java.io.PrintStream ps, Object[] a) {
    if (a == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    if (a.length > 0) {
      ps.print(((Double)a[0]).doubleValue());
      for (int i=1; i<a.length; i++) {
        ps.print(' ');
        ps.print(((Double)a[i]).doubleValue());
      }
    }
    ps.println(']');
  }

  public static final void println_array_double(java.io.PrintStream ps, List v) {
    if (v == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    int size = v.size();
    if (size > 0) {
      ps.print(((Double)v.get(0)).doubleValue());
      for (int i=1; i<size; i++) {
        ps.print(' ');
        ps.print(((Double)v.get(i)).doubleValue());
      }
    }
    ps.println(']');
  }

  // Print the lengths of the elements of the top-level array.
  public static final void println_array_2d_size(java.io.PrintStream ps, double[][] a) {
    if (a == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    if (a.length > 0) {
      ps.print((a[0]).length);
      for (int i=1; i<a.length; i++) {
        ps.print(' ');
        ps.print((a[i]).length);
      }
    }
    ps.println(']');
  }

  /// float

  public static final void println_array_float(java.io.PrintStream ps, float[] a) {
    if (a == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    if (a.length > 0) {
      ps.print(a[0]);
      for (int i=1; i<a.length; i++) {
        ps.print(' ');
        ps.print(a[i]);
      }
    }
    ps.println(']');
  }

  public static final void println_array_float(java.io.PrintStream ps, Object[] a) {
    if (a == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    if (a.length > 0) {
      ps.print(((Float)a[0]).floatValue());
      for (int i=1; i<a.length; i++) {
        ps.print(' ');
        ps.print(((Float)a[i]).floatValue());
      }
    }
    ps.println(']');
  }

  public static final void println_array_float(java.io.PrintStream ps, List v) {
    if (v == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    int size = v.size();
    if (size > 0) {
      ps.print(((Float)v.get(0)).floatValue());
      for (int i=1; i<size; i++) {
        ps.print(' ');
        ps.print(((Float)v.get(i)).floatValue());
      }
    }
    ps.println(']');
  }

  // Print the lengths of the elements of the top-level array.
  public static final void println_array_2d_size(java.io.PrintStream ps, float[][] a) {
    if (a == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    if (a.length > 0) {
      ps.print((a[0]).length);
      for (int i=1; i<a.length; i++) {
        ps.print(' ');
        ps.print((a[i]).length);
      }
    }
    ps.println(']');
  }

  /// int

  public static final void println_array_int(java.io.PrintStream ps, int[] a) {
    if (a == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    if (a.length > 0) {
      ps.print(a[0]);
      for (int i=1; i<a.length; i++) {
        ps.print(' ');
        ps.print(a[i]);
      }
    }
    ps.println(']');
  }

  public static final void println_array_int(java.io.PrintStream ps, Object[] a) {
    if (a == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    if (a.length > 0) {
      ps.print(((Integer)a[0]).intValue());
      for (int i=1; i<a.length; i++) {
        ps.print(' ');
        ps.print(((Integer)a[i]).intValue());
      }
    }
    ps.println(']');
  }

  public static final void println_array_int(java.io.PrintStream ps, List v) {
    if (v == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    int size = v.size();
    if (size > 0) {
      ps.print(((Integer)v.get(0)).intValue());
      for (int i=1; i<size; i++) {
        ps.print(' ');
        ps.print(((Integer)v.get(i)).intValue());
      }
    }
    ps.println(']');
  }

  // Print the lengths of the elements of the top-level array.
  public static final void println_array_2d_size(java.io.PrintStream ps, int[][] a) {
    if (a == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    if (a.length > 0) {
      ps.print((a[0]).length);
      for (int i=1; i<a.length; i++) {
        ps.print(' ');
        ps.print((a[i]).length);
      }
    }
    ps.println(']');
  }

  /// long

  public static final void println_array_long(java.io.PrintStream ps, long[] a) {
    if (a == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    if (a.length > 0) {
      ps.print(a[0]);
      for (int i=1; i<a.length; i++) {
        ps.print(' ');
        ps.print(a[i]);
      }
    }
    ps.println(']');
  }

  public static final void println_array_long(java.io.PrintStream ps, Object[] a) {
    if (a == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    if (a.length > 0) {
      ps.print(((Long)a[0]).longValue());
      for (int i=1; i<a.length; i++) {
        ps.print(' ');
        ps.print(((Long)a[i]).longValue());
      }
    }
    ps.println(']');
  }

  public static final void println_array_long(java.io.PrintStream ps, List v) {
    if (v == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    int size = v.size();
    if (size > 0) {
      ps.print(((Long)v.get(0)).longValue());
      for (int i=1; i<size; i++) {
        ps.print(' ');
        ps.print(((Long)v.get(i)).longValue());
      }
    }
    ps.println(']');
  }

  // Print the lengths of the elements of the top-level array.
  public static final void println_array_2d_size(java.io.PrintStream ps, long[][] a) {
    if (a == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    if (a.length > 0) {
      ps.print((a[0]).length);
      for (int i=1; i<a.length; i++) {
        ps.print(' ');
        ps.print((a[i]).length);
      }
    }
    ps.println(']');
  }

  /// short

  public static final void println_array_short(java.io.PrintStream ps, short[] a) {
    if (a == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    if (a.length > 0) {
      ps.print(a[0]);
      for (int i=1; i<a.length; i++) {
        ps.print(' ');
        ps.print(a[i]);
      }
    }
    ps.println(']');
  }

  public static final void println_array_short(java.io.PrintStream ps, Object[] a) {
    if (a == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    if (a.length > 0) {
      ps.print(((Short)a[0]).shortValue());
      for (int i=1; i<a.length; i++) {
        ps.print(' ');
        ps.print(((Short)a[i]).shortValue());
      }
    }
    ps.println(']');
  }

  public static final void println_array_short(java.io.PrintStream ps, List v) {
    if (v == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    int size = v.size();
    if (size > 0) {
      ps.print(((Short)v.get(0)).shortValue());
      for (int i=1; i<size; i++) {
        ps.print(' ');
        ps.print(((Short)v.get(i)).shortValue());
      }
    }
    ps.println(']');
  }

  // Print the lengths of the elements of the top-level array.
  public static final void println_array_2d_size(java.io.PrintStream ps, short[][] a) {
    if (a == null) {
      ps.println("null");
      return;
    }
    ps.print('[');
    if (a.length > 0) {
      ps.print((a[0]).length);
      for (int i=1; i<a.length; i++) {
        ps.print(' ');
        ps.print((a[i]).length);
      }
    }
    ps.println(']');
  }


}
