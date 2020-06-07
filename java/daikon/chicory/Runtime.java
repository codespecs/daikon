package daikon.chicory;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.SocketAddress;
import java.net.UnknownHostException;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.ConcurrentModificationException;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Pattern;
import java.util.zip.GZIPOutputStream;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.lock.qual.GuardedBy;
import org.checkerframework.checker.lock.qual.Holding;
import org.checkerframework.checker.nullness.qual.EnsuresNonNull;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.signature.qual.BinaryName;
import org.checkerframework.checker.signature.qual.ClassGetName;
import org.checkerframework.checker.signature.qual.FieldDescriptor;
import org.checkerframework.dataflow.qual.SideEffectFree;

/**
 * Runtime support for Chicory, the Daikon front end for Java. This class is a collection of
 * methods; it should never be instantiated.
 */
@SuppressWarnings({
  "nullness:initialization.static.fields.uninitialized", // library initialized in code added by
  // run-time instrumentation
  "JavaLangClash" // same class name as one in java.lang.
})
public class Runtime {
  /** Unique id for method entry/exit (so they can be matched up) */
  public static AtomicInteger nonce = new AtomicInteger();

  /** debug flag */
  public static boolean debug = false;

  /**
   * Flag indicating that a dtrace record is currently being written used to prevent a call to
   * instrumented code that occurs as part of generating a dtrace record (eg, toArray when
   * processing lists or pure functions) from generating a nested dtrace record.
   */
  public static boolean in_dtrace = false;

  /** True if ChicoryPremain was unable to load. */
  public static boolean chicoryLoaderInstantiationError = false;

  /** Flag that indicates when the first class has been processed. */
  static boolean first_class = true;

  //
  // Control over what classes (ppts) are instrumented
  //

  /** Ppts to omit (regular expression) */
  public static List<Pattern> ppt_omit_pattern = new ArrayList<>();

  /** Ppts to include (regular expression) */
  public static List<Pattern> ppt_select_pattern = new ArrayList<>();

  /** Comparability information (if any) */
  static @Nullable DeclReader comp_info = null;

  //
  // Setups that control what information is written
  //

  /** Depth to wich to examine structure components. */
  static int nesting_depth = 2;

  //
  // Dtrace file vars
  //

  /** Max number of records in dtrace file. */
  static long dtraceLimit = Long.MAX_VALUE;

  /** Number of records printed to date. */
  static long printedRecords = 0;

  /** Terminate the program when the dtrace limit is reached. */
  static boolean dtraceLimitTerminate = false;

  /** Dtrace output stream. Null if no_dtrace is true. */
  // Not annotated *@MonotonicNonNull* because initialization and use happen in generated
  // instrumentation code that cannot be type-checked by a source code checker.
  static @GuardedBy("<self>") PrintStream dtrace;

  /** Set to true when the dtrace stream is closed. */
  static boolean dtrace_closed = false;

  /** True if no dtrace is being generated. */
  static boolean no_dtrace = false;

  static String method_indent = "";

  /** Decl writer setup for writing to the trace file. */
  // Set in ChicoryPremain.premain().
  static DeclWriter decl_writer;

  /** Dtrace writer setup for writing to the trace file. */
  // Set in ChicoryPremain.premain().
  static @GuardedBy("Runtime.class") DTraceWriter dtrace_writer;

  /**
   * Which static initializers have been run. Each element of the Set is a fully qualified class
   * name.
   */
  private static Set<String> initSet = new HashSet<>();

  /** Class of information about each active call. */
  private static class CallInfo {
    /** nonce of call */
    int nonce;
    /** whether or not the call was captured on enter */
    boolean captured;

    @Holding("Runtime.class")
    public CallInfo(int nonce, boolean captured) {
      this.nonce = nonce;
      this.captured = captured;
    }
  }

  /** Stack of active methods. */
  private static @GuardedBy("Runtime.class") Map<Thread, Deque<CallInfo>> thread_to_callstack =
      new LinkedHashMap<>();

  /**
   * Sample count at a call site to begin sampling. All previous calls will be recorded. Sampling
   * starts at 10% and decreases by a factor of 10 each time another sample_start samples have been
   * recorded. If sample_start is 0, then all calls will be recorded.
   */
  public static int sample_start = 0;

  // Constructor
  private Runtime() {
    throw new Error("Do not create instances of Runtime");
  }

  /**
   * Thrown to indicate that main should not print a stack trace, but only print the message itself
   * to the user. If the string is null, then this is normal termination, not an error.
   */
  public static class TerminationMessage extends RuntimeException {
    static final long serialVersionUID = 20050923L;

    public TerminationMessage(String s) {
      super(s);
    }

    public TerminationMessage() {
      super();
    }
  }

  // Whenever a method call occurs in the target program, output
  // information about that call to the trace file.  However, if the
  // method is a pure method that is being called to create a value for
  // the trace file, don't record it.
  // TODO: invokingPure should be annotated with @GuardedByName("Runtime.class")
  // once that annotation is available.  Currently all the methods that access
  // invokingPure are annotated with @Holding("Runtime.class"), but annotating
  // the boolean would prevent any new methods from accessing it without holding
  // the lock.
  private static boolean invokingPure = false;

  @Holding("Runtime.class")
  public static boolean dontProcessPpts() {
    return invokingPure;
  }

  @Holding("Runtime.class")
  public static void startPure() {
    invokingPure = true;
  }

  @Holding("Runtime.class")
  public static void endPure() {
    invokingPure = false;
  }

  /**
   * Called when a method is entered.
   *
   * @param obj receiver of the method that was entered, or null if method is static
   * @param nonce nonce identifying which enter/exit pair this is
   * @param mi_index index in methods of the MethodInfo for this method
   * @param args array of arguments to method
   */
  public static synchronized void enter(
      @Nullable Object obj, int nonce, int mi_index, Object[] args) {

    MethodInfo mi = null;
    if (debug) {
      synchronized (SharedData.methods) {
        mi = SharedData.methods.get(mi_index);
      }
      System.out.printf(
          "%smethod_entry %s.%s%n", method_indent, mi.class_info.class_name, mi.method_name);
      method_indent = method_indent.concat("  ");
    }

    if (dontProcessPpts()) {
      return;
    }

    // Make sure that the in_dtrace flag matches the stack trace
    // check_in_dtrace();

    // Ignore this call if we are already processing a dtrace record
    if (in_dtrace) {
      return;
    }

    // Note that we are processing a dtrace record until we return
    in_dtrace = true;
    try {
      int num_new_classes = 0;
      synchronized (SharedData.new_classes) {
        num_new_classes = SharedData.new_classes.size();
      }
      if (num_new_classes > 0) {
        process_new_classes();
      }

      synchronized (SharedData.methods) {
        mi = SharedData.methods.get(mi_index);
      }
      mi.call_cnt++;

      // If sampling, check to see if we are capturing this sample
      boolean capture = true;
      if (sample_start > 0) {
        if (mi.call_cnt <= sample_start) {
          // nothing to do
        } else if (mi.call_cnt <= (sample_start * 10)) {
          capture = (mi.call_cnt % 10) == 0;
        } else if (mi.call_cnt <= (sample_start * 100)) {
          capture = (mi.call_cnt % 100) == 0;
        } else if (mi.call_cnt <= (sample_start * 1000)) {
          capture = (mi.call_cnt % 1000) == 0;
        } else {
          capture = (mi.call_cnt % 10000) == 0;
        }
        Thread t = Thread.currentThread();
        Deque<CallInfo> callstack = thread_to_callstack.get(t);
        if (callstack == null) {
          callstack = new ArrayDeque<CallInfo>();
          thread_to_callstack.put(t, callstack);
        }
        callstack.push(new CallInfo(nonce, capture));
      }

      if (capture) {
        mi.capture_cnt++;
        // long start = System.currentTimeMillis();
        if (mi.member == null) {
          dtrace_writer.clinitEntry(mi.class_info.class_name + ".<clinit>:::ENTER", nonce);
        } else {
          dtrace_writer.methodEntry(mi, nonce, obj, args);
        }
        // long duration = System.currentTimeMillis() - start;
        // System.out.println ("Enter " + mi + " " + duration + "ms"
        //                 + " " + mi.capture_cnt + "/" + mi.call_cnt);
      } else {
        // System.out.println ("skipped " + mi
        //                 + " " + mi.capture_cnt + "/" + mi.call_cnt);
      }
    } finally {
      in_dtrace = false;
    }
  }

  /**
   * Called when a method is exited.
   *
   * @param obj receiver of the method that was entered, or null if method is static
   * @param nonce nonce identifying which enter/exit pair this is
   * @param mi_index index in methods of the MethodInfo for this method
   * @param args array of arguments to method
   * @param ret_val return value of method, or null if method is void
   * @param exitLineNum the line number at which this method exited
   */
  public static synchronized void exit(
      @Nullable Object obj,
      int nonce,
      int mi_index,
      Object[] args,
      Object ret_val,
      int exitLineNum) {

    MethodInfo mi = null;
    if (debug) {
      synchronized (SharedData.methods) {
        mi = SharedData.methods.get(mi_index);
      }
      method_indent = method_indent.substring(2);
      System.out.printf(
          "%smethod_exit  %s.%s%n", method_indent, mi.class_info.class_name, mi.method_name);
    }

    if (dontProcessPpts()) {
      return;
    }

    // Make sure that the in_dtrace flag matches the stack trace
    // check_in_dtrace();

    // Ignore this call if we are already processing a dtrace record
    if (in_dtrace) {
      return;
    }

    // Note that we are processing a dtrace record until we return
    in_dtrace = true;
    try {

      int num_new_classes = 0;
      synchronized (SharedData.new_classes) {
        num_new_classes = SharedData.new_classes.size();
      }
      if (num_new_classes > 0) {
        process_new_classes();
      }

      // Skip this call if it was not sampled at entry to the method
      if (sample_start > 0) {
        CallInfo ci = null;
        @SuppressWarnings("nullness") // map: key was put in map by enter()
        @NonNull Deque<CallInfo> callstack = thread_to_callstack.get(Thread.currentThread());
        while (!callstack.isEmpty()) {
          ci = callstack.pop();
          if (ci.nonce == nonce) {
            break;
          }
        }
        if (ci == null) {
          synchronized (SharedData.methods) {
            mi = SharedData.methods.get(mi_index);
          }
          System.out.printf("no enter for exit %s%n", mi);
          return;
        } else if (!ci.captured) {
          return;
        }
      }

      // Write out the infromation for this method
      synchronized (SharedData.methods) {
        mi = SharedData.methods.get(mi_index);
      }
      // long start = System.currentTimeMillis();
      if (mi.member == null) {
        dtrace_writer.clinitExit(
            mi.class_info.class_name + ".<clinit>:::EXIT" + exitLineNum, nonce);
      } else {
        dtrace_writer.methodExit(mi, nonce, obj, args, ret_val, exitLineNum);
      }
      // long duration = System.currentTimeMillis() - start;
      // System.out.println ("Exit " + mi + " " + duration + "ms");
    } finally {
      in_dtrace = false;
    }
  }

  /**
   * Called by classes when they have finished initialization (i.e., their static initializer has
   * completed).
   *
   * <p>This functionality must be enabled by the flag Chicory.checkStaticInit. When enabled, this
   * method should only be called by the hooks created in the Instrument class.
   *
   * @param className fully qualified class name
   */
  public static void initNotify(String className) {
    if (initSet.contains(className)) {
      throw new Error("initNotify(" + className + ") when initSet already contains " + className);
    }

    // System.out.println("initialized ---> " + name);
    initSet.add(className);
  }

  /**
   * Return true iff the class with fully qualified name className has been initialized.
   *
   * @param className fully qualified class name
   */
  public static boolean isInitialized(String className) {
    return initSet.contains(className);
  }

  /**
   * Writes out decl information for any new classes (those in the new_classes field) and removes
   * them from that list.
   */
  @Holding("Runtime.class")
  public static void process_new_classes() {

    // Processing of the new_classes list must be
    // very careful, as the call to get_reflection or printDeclClass
    // may load other classes (which then get added to the list).
    while (true) {

      // Get the first class in the list (if any)
      ClassInfo class_info = null;
      synchronized (SharedData.new_classes) {
        if (SharedData.new_classes.size() > 0) {
          class_info = SharedData.new_classes.removeFirst();
        }
      }
      if (class_info == null) {
        break;
      }

      if (debug) System.out.println("processing class " + class_info.class_name);
      if (first_class) {
        decl_writer.printHeaderInfo(class_info.class_name);
        first_class = false;
      }
      class_info.initViaReflection();
      // class_info.dump (System.out);

      // Create tree structure for all method entries/exits in the class
      for (MethodInfo mi : class_info.method_infos) {
        mi.traversalEnter = RootInfo.enter_process(mi, Runtime.nesting_depth);
        mi.traversalExit = RootInfo.exit_process(mi, Runtime.nesting_depth);
      }

      decl_writer.printDeclClass(class_info, comp_info);
    }
  }

  /** Increment the number of records that have been printed. */
  public static void incrementRecords() {
    printedRecords++;

    // This should only print a percentage if dtraceLimit is not its
    // default value.
    // if (printedRecords%1000 == 0)
    //     System.out.printf("printed=%d, percent printed=%f%n", printedRecords,
    //                       (float)(100.0*(float)printedRecords/(float)dtraceLimit));

    if (printedRecords >= dtraceLimit) {
      noMoreOutput();
    }
  }

  /**
   * Indicates that no more output should be printed to the dtrace file. The file is closed and iff
   * dtraceLimitTerminate is true the program is terminated.
   */
  @SuppressWarnings("StaticGuardedByInstance")
  public static void noMoreOutput() {
    // The incrementRecords method (which calls this) is called inside a
    // synchronized block, but re-synchronize just to be sure, or in case
    // this is called from elsewhere.

    // Runtime.dtrace should be effectively final in that it refers
    // to the same value throughout the execution of the synchronized
    // block below (including the lock acquisition).
    // Unfortunately, the Lock Checker cannot verify this,
    // so a final local variable is used to satisfy the Lock Checker's
    // requirement that all variables used as locks be final or
    // effectively final.  If a bug exists whereby Runtime.dtrace
    // is not effectively final, this would unfortunately mask that error.
    final @GuardedBy("<self>") PrintStream dtrace = Runtime.dtrace;

    synchronized (dtrace) {
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

      if (dtraceLimitTerminate) {
        System.out.println("Printed " + printedRecords + " records to dtrace file.  Exiting.");
        throw new TerminationMessage(
            "Printed " + printedRecords + " records to dtrace file.  Exiting.");
        // System.exit(1);
      } else {
        // By default, no special output if the system continues to run.
        no_dtrace = true;
      }
    }
  }

  @EnsuresNonNull("dtrace")
  public static void setDtraceOnlineMode(int port) {
    dtraceLimit = Long.getLong("DTRACELIMIT", Integer.MAX_VALUE).longValue();
    dtraceLimitTerminate = Boolean.getBoolean("DTRACELIMITTERMINATE");

    Socket daikonSocket;
    try {
      daikonSocket = new Socket();
      @SuppressWarnings("nullness") // unannotated: java.net.Socket is not yet annotated
      @NonNull SocketAddress dummy = null;
      daikonSocket.bind(dummy);
      // System.out.println("Attempting to connect to Daikon on port --- " + port);
      daikonSocket.connect(new InetSocketAddress(InetAddress.getLocalHost(), port), 5000);
    } catch (UnknownHostException e) {
      System.out.println(
          "UnknownHostException connecting to Daikon : " + e.getMessage() + ". Exiting");
      System.exit(1);
      throw new Error("Unreachable control flow");
    } catch (IOException e) {
      System.out.println(
          "IOException, could not connect to Daikon : " + e.getMessage() + ". Exiting");
      System.exit(1);
      throw new Error("Unreachable control flow");
    }

    try {
      dtrace = new PrintStream(daikonSocket.getOutputStream());
    } catch (IOException e) {
      System.out.println("IOException connecting to Daikon : " + e.getMessage() + ". Exiting");
      System.exit(1);
    }

    if (supportsAddShutdownHook()) {
      addShutdownHook();
    } else {
      System.err.println("Warning: .dtrace file may be incomplete if program is aborted");
    }
  }

  // Copied from daikon.Runtime
  /** Specify the dtrace file to which to write. */
  @EnsuresNonNull("dtrace")
  public static void setDtrace(String filename, boolean append) {
    System.out.printf("entered daikon.chicory.Runtime.setDtrace(%s, %b)...%n", filename, append);

    if (no_dtrace) {
      throw new Error("setDtrace called when no_dtrace was specified");
    }
    try {
      File file = new File(filename);
      File parent = file.getParentFile();
      if (parent != null) parent.mkdirs();
      OutputStream os = new FileOutputStream(filename, append);
      if (filename.endsWith(".gz")) {
        if (append) {
          throw new Error(
              "DTRACEAPPEND environment variable is set, "
                  + "Cannot append to gzipped dtrace file "
                  + filename);
        }
        os = new GZIPOutputStream(os);
      }
      dtraceLimit = Long.getLong("DTRACELIMIT", Integer.MAX_VALUE).longValue();
      dtraceLimitTerminate = Boolean.getBoolean("DTRACELIMITTERMINATE");

      // System.out.println("limit = " + dtraceLimit + " terminate " + dtraceLimitTerminate);

      // 8192 is the buffer size in BufferedReader
      BufferedOutputStream bos = new BufferedOutputStream(os, 8192);
      dtrace = new PrintStream(bos);
    } catch (Exception e) {
      e.printStackTrace();
      throw new Error(e);
    }
    if (supportsAddShutdownHook()) {
      addShutdownHook();
    } else {
      System.err.println("Warning: .dtrace file may be incomplete if program is aborted");
    }
    // System.out.printf("exited daikon.chicory.Runtime.setDtrace(%s, %b)%n", filename, append);
  }

  /**
   * If the current data trace file is not yet set, then set it. The value of the DTRACEFILE
   * environment variable is used; if that environment variable is not set, then the argument to
   * this method is used instead.
   */
  public static void setDtraceMaybe(String default_filename) {
    // Copied from daikon.Runtime
    // System.out.println ("Setting dtrace maybe: " + default_filename);
    if ((dtrace == null) && !no_dtrace) {
      String filename = System.getProperty("DTRACEFILE", default_filename);
      boolean append = System.getProperty("DTRACEAPPEND") != null;
      setDtrace(filename, append);
    }
  }

  private static boolean supportsAddShutdownHook() {
    // Copied from daikon.Runtime

    try {
      Class<java.lang.Runtime> rt = java.lang.Runtime.class;
      rt.getMethod("addShutdownHook", new Class<?>[] {java.lang.Thread.class});
      return true;
    } catch (Exception e) {
      return false;
    }
  }

  /** Add a shutdown hook to close the PrintStream when the program exits. */
  private static void addShutdownHook() {
    // Copied from daikon.Runtime, then modified

    java.lang.Runtime.getRuntime()
        .addShutdownHook(
            new Thread() {
              @Override
              @SuppressWarnings("lock") // non-final field
              public void run() {
                if (!dtrace_closed) {
                  // When the program being instrumented exits, the buffers
                  // of the "dtrace" (PrintStream) object are not flushed,
                  // so we miss the tail of the file.

                  synchronized (Runtime.dtrace) {
                    dtrace.println();
                    // These are for debugging, I assume. -MDE
                    for (Pattern p : ppt_omit_pattern) {
                      dtrace.println("# ppt-omit-pattern: " + p);
                    }
                    for (Pattern p : ppt_select_pattern) {
                      dtrace.println("# ppt-select-pattern: " + p);
                    }
                    // This lets us know we didn't lose any data.
                    dtrace.println("# EOF (added by Runtime.addShutdownHook)");
                    dtrace.close();
                  }
                }

                if (chicoryLoaderInstantiationError) {
                  // Warning messages have already been printed.
                } else if (SharedData.all_classes.size() == 0) {
                  System.out.println("Chicory warning: No methods were instrumented.");
                  if (!ppt_select_pattern.isEmpty() || !ppt_omit_pattern.isEmpty()) {
                    System.out.println(
                        "Check the --ppt-select-pattern and --ppt-omit-pattern options");
                  }
                } else if (printedRecords == 0) {
                  System.out.println("Chicory warning: no records were printed");
                }
              }
            });
  }

  /**
   * Gets the ClassInfo structure corresponding to type. Returns null if the class was not
   * instrumented.
   *
   * @param type declaring class
   * @return ClassInfo structure corresponding to type
   */
  public static @Nullable ClassInfo getClassInfoFromClass(Class<?> type) {
    try {
      synchronized (SharedData.all_classes) {
        for (ClassInfo cinfo : SharedData.all_classes) {
          if (cinfo.clazz == null) {
            cinfo.initViaReflection();
          }
          if (cinfo.clazz.equals(type)) {
            return cinfo;
          }
        }
      }
    } catch (ConcurrentModificationException e) {
      // occurs if cinfo.get_reflection() causes a new class to be loaded
      // which causes all_classes to change
      return getClassInfoFromClass(type);
    }

    // throw new RuntimeException("Class " + type.getName() + " is not in Runtime's class list");
    return null;
  }

  ///////////////////////////////////////////////////////////////////////////
  /// Wrappers for the various primitive types.
  /// Used to distinguish wrappers created by user code
  /// from wrappers created by Chicory.

  public static interface PrimitiveWrapper {
    // returns corresponding java.lang wrapper
    public Object getJavaWrapper();

    public Class<?> primitiveClass();
  }

  /** wrapper used for boolean arguments */
  public static class BooleanWrap implements PrimitiveWrapper {
    boolean val;

    public BooleanWrap(boolean val) {
      this.val = val;
    }

    @SideEffectFree
    @Override
    public String toString(@GuardSatisfied BooleanWrap this) {
      return Boolean.toString(val);
    }

    @Override
    public Boolean getJavaWrapper() {
      return val;
    }

    @Override
    public Class<?> primitiveClass() {
      return boolean.class;
    }
  }

  /** wrapper used for int arguments */
  public static class ByteWrap implements PrimitiveWrapper {
    byte val;

    public ByteWrap(byte val) {
      this.val = val;
    }

    @SideEffectFree
    @Override
    public String toString(@GuardSatisfied ByteWrap this) {
      return Byte.toString(val);
    }

    @Override
    public Byte getJavaWrapper() {
      return val;
    }

    @Override
    public Class<?> primitiveClass() {
      return byte.class;
    }
  }

  /** wrapper used for int arguments */
  public static class CharWrap implements PrimitiveWrapper {
    char val;

    public CharWrap(char val) {
      this.val = val;
    }
    // Print characters as integers.
    @SideEffectFree
    @Override
    public String toString(@GuardSatisfied CharWrap this) {
      return Integer.toString(val);
    }

    @Override
    public Character getJavaWrapper() {
      return val;
    }

    @Override
    public Class<?> primitiveClass() {
      return char.class;
    }
  }

  /** wrapper used for int arguments */
  public static class FloatWrap implements PrimitiveWrapper {
    float val;

    public FloatWrap(float val) {
      this.val = val;
    }

    @SideEffectFree
    @Override
    public String toString(@GuardSatisfied FloatWrap this) {
      return Float.toString(val);
    }

    @Override
    public Float getJavaWrapper() {
      return val;
    }

    @Override
    public Class<?> primitiveClass() {
      return float.class;
    }
  }

  /** wrapper used for int arguments */
  public static class IntWrap implements PrimitiveWrapper {
    int val;

    public IntWrap(int val) {
      this.val = val;
    }

    @SideEffectFree
    @Override
    public String toString(@GuardSatisfied IntWrap this) {
      return Integer.toString(val);
    }

    @Override
    public Integer getJavaWrapper() {
      return val;
    }

    @Override
    public Class<?> primitiveClass() {
      return int.class;
    }
  }

  /** wrapper used for int arguments */
  public static class LongWrap implements PrimitiveWrapper {
    long val;

    public LongWrap(long val) {
      this.val = val;
    }

    @SideEffectFree
    @Override
    public String toString(@GuardSatisfied LongWrap this) {
      return Long.toString(val);
    }

    @Override
    public Long getJavaWrapper() {
      return val;
    }

    @Override
    public Class<?> primitiveClass() {
      return long.class;
    }
  }

  /** wrapper used for int arguments */
  public static class ShortWrap implements PrimitiveWrapper {
    short val;

    public ShortWrap(short val) {
      this.val = val;
    }

    @SideEffectFree
    @Override
    public String toString(@GuardSatisfied ShortWrap this) {
      return Short.toString(val);
    }

    @Override
    public Short getJavaWrapper() {
      return val;
    }

    @Override
    public Class<?> primitiveClass() {
      return short.class;
    }
  }

  /** wrapper used for double arguments */
  public static class DoubleWrap implements PrimitiveWrapper {
    double val;

    public DoubleWrap(double val) {
      this.val = val;
    }

    @SideEffectFree
    @Override
    public String toString(@GuardSatisfied DoubleWrap this) {
      return Double.toString(val);
    }

    @Override
    public Double getJavaWrapper() {
      return val;
    }

    @Override
    public Class<?> primitiveClass() {
      return double.class;
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  /// Copied code
  ///

  // Lifted directly from plume/UtilPlume.java, where it is called
  // escapeJava(), but repeated here to make this class self-contained.
  /** Quote \, ", \n, and \r characters in the target; return a new string. */
  public static String quote(String orig) {
    StringBuilder sb = new StringBuilder();
    // The previous escape (or escaped) character was seen right before
    // this position.  Alternately:  from this character forward, the string
    // should be copied out verbatim (until the next escaped character).
    int post_esc = 0;
    int orig_len = orig.length();
    for (int i = 0; i < orig_len; i++) {
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
        case '\n': // not lineSep
          if (post_esc < i) {
            sb.append(orig.substring(post_esc, i));
          }
          sb.append("\\n"); // not lineSep
          post_esc = i + 1;
          break;
        case '\r':
          if (post_esc < i) {
            sb.append(orig.substring(post_esc, i));
          }
          sb.append("\\r");
          post_esc = i + 1;
          break;
        default:
          // Do nothing; i gets incremented.
      }
    }
    if (sb.length() == 0) {
      return orig;
    }
    sb.append(orig.substring(post_esc));
    return sb.toString();
  }

  private static HashMap<String, String> primitiveClassesFromJvm = new HashMap<>(8);

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
   * Convert a classname from JVML format to Java format. For example, convert "[Ljava/lang/Object;"
   * to "java.lang.Object[]".
   *
   * <p>If the argument is not a field descriptor, returns it as is. This enables this method to be
   * used on the output of {@link Class#getName()}.
   */
  @Deprecated
  public static String classnameFromJvm(@FieldDescriptor String classname) {
    return fieldDescriptorToBinaryName(classname);
  }

  /**
   * Convert a classname from JVML format to Java format. For example, convert "[Ljava/lang/Object;"
   * to "java.lang.Object[]".
   *
   * <p>If the argument is not a field descriptor, returns it as is. This enables this method to be
   * used on the output of {@link Class#getName()}.
   */
  @SuppressWarnings("signature") // conversion routine
  public static String fieldDescriptorToBinaryName(@FieldDescriptor String classname) {

    // System.out.println(classname);

    int dims = 0;
    while (classname.startsWith("[")) {
      dims++;
      classname = classname.substring(1);
    }

    String result;
    // array of reference type
    if (classname.startsWith("L") && classname.endsWith(";")) {
      result = classname.substring(1, classname.length() - 1);
      result = result.replace('/', '.');
    } else {
      if (dims > 0) // array of primitives
      result = primitiveClassesFromJvm.get(classname);
      else {
        // just a primitive
        result = classname;
      }

      if (result == null) {
        // As a failsafe, use the input; perhaps it is in Java, not JVML,
        // format.
        result = classname;
        // throw new Error("Malformed base class: " + classname);
      }
    }
    for (int i = 0; i < dims; i++) {
      result += "[]";
    }
    return result;
  }

  @SuppressWarnings("signature") // conversion method
  public static final @BinaryName String classGetNameToBinaryName(@ClassGetName String cgn) {
    if (cgn.startsWith("[")) {
      return fieldDescriptorToBinaryName(cgn);
    } else {
      return cgn;
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  /// end of copied code
  ///

}
