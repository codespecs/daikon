package daikon.chicory;

import java.util.concurrent.CopyOnWriteArrayList;
import java.util.regex.Pattern;
import java.util.zip.GZIPOutputStream;
import java.io.*;
import java.net.*;
import java.net.Socket;
import java.util.*;

import daikon.Chicory;

/**
 * Runtime support for chicory daikon front end.
 * This class is a collection of methods.  It should
 * never be instantiated.
 */
public class Runtime
{
    /** Unique id for method entry/exit (so they can be matched up) **/
    public static int nonce = 0;

    /** debug flag **/
    public static final boolean debug = false;


    /**
     * List of classes recently transformed.  This list is examined in
     * each enter/exit and the decl information for any new classes are
     * printed out and the class is then removed from the list.
     */
    public static final List<ClassInfo> new_classes = new LinkedList<ClassInfo>();

    /** List of all instrumented classes **/
    public static final List<ClassInfo> all_classes = new ArrayList<ClassInfo>();

    /** flag that indicates when the first class has been processed**/
    static boolean first_class = true;

    /** List of all instrumented methods **/
    public static final List<MethodInfo> methods = new ArrayList<MethodInfo>();

    //
    // Control over what classes (ppts) are instrumented
    //
    /** Ppts to omit (regular expression) **/
    static List<Pattern> daikon_omit_regex = new ArrayList<Pattern>();

    /** Ppts to include (regular expression) **/
    static List<Pattern> daikon_include_regex = new ArrayList<Pattern>();

    //
    // Setups that control what information is written
    //
    /** Render linked lists as vectors **/
    static boolean linked_lists = true;

    /** Depth to wich to examine structure components **/
    static int nesting_depth = 2;

    //
    // Dtrace file vars
    //
    /** Max number of records in dtrace file **/
    static long dtraceLimit = Long.MAX_VALUE;

    /** Number of records printed to date **/
    static long printedRecords = 0;

    /** Terminate the program when the dtrace limit is reached **/
    static boolean dtraceLimitTerminate = false;

    /** Dtrace output stream **/
    static PrintStream dtrace;

    /** Set to true when the dtrace stream is closed **/
    static boolean dtrace_closed = false;

    /** True if no dtrace is being generated.  **/
    static boolean no_dtrace = false;

    /** Decl writer setup for writing to the trace file **/
    static DeclWriter decl_writer = null;

    /** Dtrace writer setup for writing to the trace file **/
    static DTraceWriter dtrace_writer = null;

    /** 
     * Which static initializers have been run.
     * Each element of the Set is a fully qualified class name.
     **/
    private static Set <String> initSet = new HashSet<String>();

    // Constructor
    private Runtime()
    {
        throw new Error("Do not create instances of Runtime");
    }

    /** Printf to dtrace file. **/
    final private static void printf(String format, Object... args)
    {
        if (!dtrace_closed)
            dtrace.printf(format, args);
    }

    /** Println to dtrace file. **/
    final private static void println(String msg)
    {
        if (!dtrace_closed)
            dtrace.println(msg);
    }

    /** Println to dtrace file. **/
    final private static void println(int val)
    {
        if (!dtrace_closed)
            dtrace.println(val);
    }

    /** Println to dtrace file. **/
    final private static void println(Object obj)
    {
        if (!dtrace_closed)
            dtrace.println(obj);
    }

    /** Println to dtrace file. **/
    final private static void println()
    {
        if (!dtrace_closed)
            dtrace.println();
    }

    /**
     * Thrown to indicate that main should not print a stack trace, but only
     * print the message itself to the user.
     * If the string is null, then this is normal termination, not an error.
     **/
    public static class TerminationMessage extends RuntimeException
    {

        public TerminationMessage(String s)
        {
            super(s);
        }

        public TerminationMessage()
        {
            super();
        }
    }


    /**
     * Called when a method is entered.
     *
     * @param obj - Receiver of the method that was entered.  Null if method is
     *              static
     * @param nonce - Nonce identifying which enter/exit pair this is
     * @param mi_index - Index in methods of the MethodInfo for this method
     * @param args - Array of arguments to method
     */
    public static void enter(Object obj, int nonce, int mi_index, Object[] args)
    {

        if (new_classes.size() > 0)
            process_new_classes();

        // System.out.println ("in enter");

         /*Throwable stack = new Throwable ("enter");
         stack.fillInStackTrace();
         StackTraceElement[] ste_arr = stack.getStackTrace();
         StackTraceElement ste = ste_arr[1];
         System.out.printf ("%s.%s():::ENTER%n%n", ste.getClassName(), ste.getMethodName());*/

        synchronized (all_classes)
        {
            MethodInfo mi = methods.get(mi_index);
            dtrace_writer.methodEntry(mi, nonce, obj, args);
        }

    }
    
    /**
     * Called when a method is exited.
     *
     * @param obj        -  Receiver of the method that was entered.  Null if method is
     *                      static
     * @param nonce       - Nonce identifying which enter/exit pair this is
     * @param mi_index    - Index in methods of the MethodInfo for this method
     * @param args        - Array of arguments to method
     * @param ret_val     - Return value of method.  null if method is void
     * @param exitLineNum - The line number at which this method exited
     */
    public static void exit(Object obj, int nonce, int mi_index, Object[] args, Object ret_val, int exitLineNum)
    {
        if (new_classes.size() > 0)
            process_new_classes();


        synchronized (all_classes)
        {
            MethodInfo mi = methods.get(mi_index);
            dtrace_writer.methodExit(mi, nonce, obj, args, ret_val, exitLineNum);
        }

    }

    /**
     * Called by classes when they have finished initialization
     * (ie, their static initializer has completed).
     * 
     * This functionality must be enabled by the flag Chicory.checkStaticInit.
     * When enabled, this method should only be called by the hooks created in the
     * RetTransform class.
     * 
     * @param className Fully qualified class name
     */
    public static void initNotify(String className)
    {
        assert !initSet.contains(className) : className + " already exists in initSet";
        
        //System.out.println("initialized ---> " + name);
        initSet.add(className);
    }

    /**
     * Return true iff the class with fully qualified name className
     * has been initialized.
     * 
     * @param className Fully qualified class name
     */
    public static boolean isInitialized(String className)
    {
        return initSet.contains(className);
    }

    /**
     * Writes out decl information for any new classes and removes
     * them from the list.
     */
    public static void process_new_classes() {

      // Processing of the new_classes list must be
      // very careful, as the call to get_reflection or printDeclClass
      // may load other classes (which then get added to the list).
      synchronized (new_classes) {
        // System.out.printf ("Processing %d new classes%n",
        //                 new_classes.size());
        while (new_classes.size() > 0) {
          ClassInfo class_info = new_classes.get (0);
          new_classes.remove (0);
          if (debug)
            System.out.printf ("processing class %s%n", class_info.class_name);
          if (first_class) {
            decl_writer.printHeaderInfo (class_info.class_name);
            first_class = false;
          }
          class_info.get_reflection();
          // class_info.dump (System.out);
          decl_writer.printDeclClass (class_info);

        }
      }
    }

    /** Increment the number of records that have been printed. **/
    public static void incrementRecords()
    {
        printedRecords++;

        // This should only print a percentage if dtraceLimit is not its
        // default value.
        // if(printedRecords%1000 == 0)
        //     System.out.printf("printed=%d, percent printed=%f%n", printedRecords, (float)(100.0*(float)printedRecords/(float)dtraceLimit));

        if (printedRecords >= dtraceLimit)
        {
            noMoreOutput();
        }
    }

    /** Indicates that no more output should be printed to the dtrace file.
     *  The file is closed and iff dtraceLimitTerminate is true the program
     * is terminated.
     */
    public static void noMoreOutput()
    {
        // The incrementRecords method (which calls this) is called inside a
        // synchronized block, but re-synchronize just to be sure, or in case
        // this is called from elsewhere.
        synchronized (Runtime.dtrace)
        {
            // The shutdown hook is synchronized on this, so close it up
            // ourselves, lest the call to System.exit cause deadlock.
            dtrace.println();
            dtrace.println("# EOF (added by no_more_output)");
            dtrace.close();

            // Don't set dtrace to null, because if we continue running, there will
            // be many attempts to synchronize on it.  (Is that a performance
            // bottleneck, if we continue running?)
            // dtrace = null;
            dtrace_closed = true;


            if (dtraceLimitTerminate)
            {
                System.out.println("Printed " + printedRecords + " records to dtrace file.  Exiting.");
                throw new TerminationMessage("Printed " + printedRecords + " records to dtrace file.  Exiting.");
                // System.exit(1);
            }
            else
            {
                // By default, no special output if the system continues to run.
                no_dtrace = true;
            }
        }
    }

    public static void setDtraceOnlineMode(int port)
    {
        dtraceLimit = Long.getLong("DTRACELIMIT", Integer.MAX_VALUE).longValue();
        dtraceLimitTerminate = Boolean.getBoolean("DTRACELIMITTERMINATE");

        Socket daikonSocket = null;
        try
        {
            daikonSocket = new Socket();
            daikonSocket.bind(null);
            //System.out.println("Attempting to connect to Daikon on port --- " + port);
            daikonSocket.connect(new InetSocketAddress(InetAddress.getLocalHost(), port), 5000);
        }
        catch (UnknownHostException e)
        {
            System.out.println("UnknownHostException connecting to Daikon : " + e.getMessage() + ". Exiting");
            System.exit(1);
        }
        catch (IOException e)
        {
            System.out.println("IOException, could not connect to Daikon : " + e.getMessage() + ". Exiting");
            System.exit(1);
        }

        try
        {
            dtrace = new PrintStream(daikonSocket.getOutputStream());
        }
        catch (IOException e)
        {
            System.out.println("IOException connecting to Daikon : " + e.getMessage() + ". Exiting");
            System.exit(1);
        }

        if (supportsAddShutdownHook())
        {
            addShutdownHook();
        }
        else
        {
            System.err.println("Warning: .dtrace file may be incomplete if program is aborted");
        }
    }

    /** Specify the dtrace file to which to write **/
    public static void setDtrace(String filename, boolean append)
    {
        // Copied from daikon.Runtime
        
        if (no_dtrace)
        {
            throw new Error("setDtrace called when no_dtrace was specified");
        }
        try
        {
            File file = new File(filename);
            File parent = file.getParentFile();
            if (parent != null)
                parent.mkdirs();
            OutputStream os = new FileOutputStream(filename, append);
            if (filename.endsWith(".gz"))
            {
                if (append)
                    throw new Error("DTRACEAPPEND environment variable is set, " + "Cannot append to gzipped dtrace file " + filename);
                os = new GZIPOutputStream(os);
            }
            dtraceLimit = Long.getLong("DTRACELIMIT", Integer.MAX_VALUE).longValue();
            dtraceLimitTerminate = Boolean.getBoolean("DTRACELIMITTERMINATE");

            //System.out.println("limit = " + dtraceLimit + " terminate " + dtraceLimitTerminate);

            // 8192 is the buffer size in BufferedReader
            BufferedOutputStream bos = new BufferedOutputStream(os, 8192);
            dtrace = new PrintStream(bos);
        }
        catch (Exception e)
        {
            e.printStackTrace();
            throw new Error("" + e);
        }
        if (supportsAddShutdownHook())
        {
            addShutdownHook();
        }
        else
        {
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
    public static void setDtraceMaybe(String default_filename)
    {
        // Copied from daikon.Runtime
        // System.out.println ("Setting dtrace maybe: " + default_filename);
        if ((dtrace == null) && (!no_dtrace))
        {
            String filename = System.getProperty("DTRACEFILE", default_filename);
            boolean append = System.getProperty("DTRACEAPPEND") != null;
            setDtrace(filename, append);
        }
    }

    private static boolean supportsAddShutdownHook()
    {
        // Copied from daikon.Runtime

        try
        {
            Class rt = java.lang.Runtime.class;
            rt.getMethod("addShutdownHook", new Class[]{java.lang.Thread.class});
            return true;
        }
        catch (Exception e)
        {
            return false;
        }
    }

    /**
     * Add a shutdown hook to close the PrintStream when the program
     * exits.
     */
    private static void addShutdownHook()
    {
        // Copied from daikon.Runtime
        
        java.lang.Runtime.getRuntime().addShutdownHook(new Thread()
        {

            public void run()
            {
                if (!dtrace_closed)
                {

                    // When the program being instrumented exits, the buffers
                    // of the "dtrace" (PrintStream) object are not flushed,
                    // so we miss the tail of the file.

                    synchronized (Runtime.dtrace)
                    {
                        dtrace.println();
                        // This lets us know we didn't lose any data.
                        dtrace.println("# EOF (added by Runtime.addShutdownHook)");
                        dtrace.close();
                    }


                }

            }
        });
    }

    private static Process chicory_proc;

    private static StreamRedirectThread err_thread;

    private static StreamRedirectThread out_thread;


    static void setDaikonInfo(StreamRedirectThread err, StreamRedirectThread out, Process proc)
    {
        chicory_proc = proc;
        err_thread = err;
        out_thread = out;
    }

    /**
     * Wait for Daikon to terminate
     */
    public static void endDaikon()
    {
        try
        {
            int status = chicory_proc.waitFor();
            System.out.println("daikon ended with status " + status);
        }
        catch (InterruptedException e1)
        {
            e1.printStackTrace();
        }

        try
        {
        err_thread.join();
        out_thread.join();
        }
        catch(InterruptedException e)
        {
        }

        System.out.println("Finished endDaikon");

    }

    /**
     * Gets the ClassInfo structure corresponding to type
     * @param type declaring class
     * @return ClassInfo structure corresponding to type
     */
    public static ClassInfo getClassInfoFromClass(Class type)
    {
        try
        {
        synchronized (Runtime.all_classes)
        {
            for (ClassInfo cinfo : Runtime.all_classes)
            {
                if (cinfo.clazz == null)
                    cinfo.get_reflection();

                if (cinfo.clazz.equals(type))
                    return cinfo;
            }
        }
        }
        catch(ConcurrentModificationException e)
        {
            // occurs if cinfo.get_reflection() causes a new class to be loaded
            // which causes all_classes to change
            return getClassInfoFromClass(type);
        }

        throw new RuntimeException("Unable to find class " + type.getName() + " in Runtime's class list");
    }


  ///////////////////////////////////////////////////////////////////////////
  /// Wrappers for the various primitive types.
  /// Used to distinguish wrappers created by user code
  /// from wrappers created by Chicory.

  public static interface PrimitiveWrapper
  {
  }

  /** wrapper used for boolean arguments **/
  public static class BooleanWrap implements PrimitiveWrapper{
    boolean val;
    public BooleanWrap (boolean val) { this.val = val; }
    public String toString() {return Boolean.toString(val);}
  }

  /** wrapper used for int arguments **/
  public static class ByteWrap implements PrimitiveWrapper{
    byte val;
    public ByteWrap (byte val) { this.val = val; }
    public String toString() {return Byte.toString(val);}
  }

  /** wrapper used for int arguments **/
  public static class CharWrap implements PrimitiveWrapper{
    char val;
    public CharWrap (char val) { this.val = val; }
    // Print characters as integers.
    public String toString() {return Integer.toString(val);}
  }

  /** wrapper used for int arguments **/
  public static class FloatWrap implements PrimitiveWrapper{
    float val;
    public FloatWrap (float val) { this.val = val; }
    public String toString() {return Float.toString(val);}
  }

  /** wrapper used for int arguments **/
  public static class IntWrap implements PrimitiveWrapper{
    int val;
    public IntWrap (int val) { this.val = val; }
    public String toString() {return Integer.toString(val);}
  }

  /** wrapper used for int arguments **/
  public static class LongWrap implements PrimitiveWrapper{
    long val;
    public LongWrap (long val) { this.val = val; }
    public String toString() {return Long.toString(val);}
  }

  /** wrapper used for int arguments **/
  public static class ShortWrap implements PrimitiveWrapper{
    short val;
    public ShortWrap (short val) { this.val = val; }
    public String toString() {return Short.toString(val);}
  }

  /** wrapper used for double arguments **/
  public static class DoubleWrap implements PrimitiveWrapper{
    double val;
    public DoubleWrap (double val) { this.val = val; }
    public String toString() {return Double.toString(val);}
  }


  ///////////////////////////////////////////////////////////////////////////
  /// Copied code
  ///

  // Lifted directly from utilMDE/UtilMDE.java, where it is called
  // escapeNonJava(), but repeated here to make this class self-contained.
  /** Quote \, ", \n, and \r characters in the target; return a new string. **/
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
      case '\n':                // not lineSep
        if (post_esc < i) {
          sb.append(orig.substring(post_esc, i));
        }
        sb.append("\\n");       // not lineSep
        post_esc = i+1;
        break;
      case '\r':
        if (post_esc < i) {
          sb.append(orig.substring(post_esc, i));
        }
        sb.append("\\r");
        post_esc = i+1;
        break;
      default:
        // Do nothing; i gets incremented.
      }
    }
    if (sb.length() == 0)
      return orig;
    sb.append(orig.substring(post_esc));
    return sb.toString();
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
   * For example, convert "[Ljava/lang/Object;" to "java.lang.Object[]".
   **/
  public static String classnameFromJvm(String classname) {

      //System.out.println(classname);

    int dims = 0;
    while (classname.startsWith("[")) {
      dims++;
      classname = classname.substring(1);
    }

    String result;
    //array of reference type
    if (classname.startsWith("L") && classname.endsWith(";")) {
      result = classname.substring(1, classname.length() - 1);
      result = result.replace('/', '.');
    }
    else {
        if(dims > 0) //array of primitives
            result = (String) primitiveClassesFromJvm.get(classname);
        else //just a primitive
            result = classname;

      if (result == null) {
        // As a failsafe, use the input; perhaps it is in Java, not JVML,
        // format.
        result = classname;
        // throw new Error("Malformed base class: " + classname);
      }
    }
    for (int i=0; i<dims; i++) {
      result += "[]";
    }
    return result;
  }

  ///////////////////////////////////////////////////////////////////////////
  /// end of copied code
  ///

}
