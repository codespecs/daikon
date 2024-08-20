package daikon.dcomp;

import daikon.DynComp;
import daikon.plumelib.bcelutil.BcelUtil;
import daikon.plumelib.bcelutil.SimpleLog;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.lang.instrument.ClassFileTransformer;
import java.lang.instrument.IllegalClassFormatException;
import java.security.ProtectionDomain;
import org.apache.bcel.classfile.ClassParser;
import org.apache.bcel.classfile.JavaClass;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.signature.qual.InternalForm;
import org.checkerframework.dataflow.qual.Pure;

public class Instrument implements ClassFileTransformer {

  /** Directory for debug output. */
  File debug_dir;

  /** Directory for debug instrumented class output. */
  File debug_bin_dir;

  /** Directory for debug original class output. */
  File debug_orig_dir;

  /** Have we seen a class member of a known transformer? */
  static boolean transformer_seen = false;

  /**
   * Debug information about which classes and/or methods are transformed and why. Use
   * debugInstrument for actual instrumentation details.
   */
  private static SimpleLog debug_transform = new SimpleLog(false);

  /** Instrument class constructor. Setup debug directories, if needed. */
  public Instrument() {
    debug_transform.enabled =
        DynComp.debug || DynComp.debug_transform || Premain.debug_dcinstrument;
    daikon.chicory.Instrument.debug_transform.enabled = debug_transform.enabled;

    debug_dir = DynComp.debug_dir;
    debug_bin_dir = new File(debug_dir, "bin");
    debug_orig_dir = new File(debug_dir, "orig");

    if (DynComp.dump) {
      debug_bin_dir.mkdirs();
      debug_orig_dir.mkdirs();
    }
  }

  /** Debug code for printing the current run-time call stack. */
  public static void print_call_stack() {
    StackTraceElement[] stack_trace;
    stack_trace = Thread.currentThread().getStackTrace();
    // [0] is getStackTrace
    // [1] is print_call_stack
    for (int i = 2; i < stack_trace.length; i++) {
      System.out.printf("call stack: %s%n", stack_trace[i]);
    }
    System.out.println();
  }

  @Override
  @SuppressWarnings("nullness") // bug: java.lang.instrument is not yet annotated
  public byte @Nullable [] transform(
      ClassLoader loader,
      @InternalForm String className,
      Class<?> classBeingRedefined,
      ProtectionDomain protectionDomain,
      byte[] classfileBuffer)
      throws IllegalClassFormatException {

    debug_transform.log(
        "In dcomp.Instrument.transform(): class = %s, loader: %s%n", className, loader);

    if (className == null) {
      /*
      // debug code to display unnamed class
      try {
        // Parse the bytes of the classfile, die on any errors
        ClassParser parser = new ClassParser(new ByteArrayInputStream(classfileBuffer), className);
        JavaClass c = parser.parse();
        System.out.println(c.toString());
      } catch (Throwable e) {
        System.out.printf("Unexpected Error: %s%n", e);
        e.printStackTrace();
        throw new RuntimeException("Unexpected error", e);
      }
      */
      // most likely a lambda related class
      return null;
    }

    // See comments in Premain.java about meaning and use of in_shutdown.
    if (Premain.in_shutdown) {
      debug_transform.log("Skipping in_shutdown class %s%n", className);
      return null;
    }

    // If already instrumented, nothing to do
    // (This set will be empty if DCInstrument.jdk_instrumented is false)
    if (Premain.pre_instrumented.contains(className)) {
      debug_transform.log("Skipping pre_instrumented JDK class %s%n", className);
      return null;
    }

    boolean in_jdk = false;

    // Check if class is in JDK
    if (BcelUtil.inJdkInternalform(className)) {
      // If we are not using an instrumented JDK, then skip this class.
      if (!DCInstrument.jdk_instrumented) {
        debug_transform.log("Skipping JDK class %s%n", className);
        return null;
      }

      int lastSlashPos = className.lastIndexOf('/');
      if (lastSlashPos > 0) {
        String packageName = className.substring(0, lastSlashPos).replace('/', '.');
        if (Premain.problem_packages.contains(packageName)) {
          debug_transform.log("Skipping problem package %s%n", packageName);
          return null;
        }
      }

      if (BcelUtil.javaVersion > 8) {
        if (Premain.problem_classes.contains(className.replace('/', '.'))) {
          debug_transform.log("Skipping problem class %s%n", className);
          return null;
        }
      }

      if (className.contains("/$Proxy")) {
        debug_transform.log("Skipping proxy class %s%n", className);
        return null;
      }

      if (className.equals("java/lang/DCRuntime")) {
        debug_transform.log("Skipping special DynComp runtime class %s%n", className);
        return null;
      }

      in_jdk = true;
      debug_transform.log("Instrumenting JDK class %s%n", className);
    } else {

      // We're not in a JDK class
      // Don't instrument our own classes
      if (is_dcomp(className)) {
        debug_transform.log("Skipping is_dcomp class %s%n", className);
        return null;
      }

      // Don't instrument other byte code transformers
      if (is_transformer(className)) {
        debug_transform.log("Skipping is_transformer class %s%n", className);
        if (!transformer_seen) {
          transformer_seen = true;
          System.err.printf(
              "DynComp warning: This program uses a Java byte code transformer: %s%n", className);
          System.err.printf(
              "This may interfere with the DynComp transformer and cause DynComp to fail.%n");
        }
        return null;
      }
    }

    if (loader == null) {
      debug_transform.log("transforming class %s, loader %s%n", className, loader);
    } else {
      debug_transform.log(
          "transforming class %s, loader %s - %s%n", className, loader, loader.getParent());
    }

    // Parse the bytes of the classfile, die on any errors
    try (ByteArrayInputStream bais = new ByteArrayInputStream(classfileBuffer)) {
      ClassParser parser = new ClassParser(bais, className);

      JavaClass c = parser.parse();

      if (DynComp.dump) {
        c.dump(new File(debug_orig_dir, c.getClassName() + ".class"));
      }

      // Transform the file
      DCInstrument dci = new DCInstrument(c, in_jdk, loader);
      JavaClass njc = dci.instrument();

      if (njc == null) {
        debug_transform.log("Didn't instrument %s%n", c.getClassName());
        return null;
      } else {
        if (DynComp.dump) {
          System.out.printf("Dumping %s to %s%n", njc.getClassName(), debug_bin_dir);
          // write .class file
          njc.dump(new File(debug_bin_dir, njc.getClassName() + ".class"));
          // write .bcel file
          BcelUtil.dump(njc, debug_bin_dir);
        }
        return njc.getBytes();
      }
    } catch (Throwable e) {
      System.err.printf("Unexpected Error: %s%n", e);
      e.printStackTrace();
      throw new RuntimeException("Unexpected error", e);
    }
  }

  /**
   * Returns whether or not the specified class is part of dcomp itself (and thus should not be
   * instrumented). Some Daikon classes that are used by DynComp are included here as well.
   *
   * @param classname class to be checked
   * @return true if classname is a part of DynComp
   */
  @Pure
  private static boolean is_dcomp(String classname) {

    if (classname.startsWith("daikon/dcomp/") && !classname.startsWith("daikon/dcomp/DcompTest")) {
      return true;
    }
    if (classname.startsWith("daikon/chicory/")
        && !classname.equals("daikon/chicory/ChicoryTest")) {
      return true;
    }
    if (classname.equals("daikon/PptTopLevel$PptType")) {
      return true;
    }
    if (classname.startsWith("daikon/plumelib")) {
      return true;
    }
    return false;
  }

  /**
   * Returns whether or not the specified class is part of a tool known to do Java byte code
   * transformation. We need to warn the user that this may not work correctly.
   *
   * @param classname class to be checked
   * @return true if classname is a known transformer
   */
  @Pure
  protected static boolean is_transformer(String classname) {

    if (classname.startsWith("org/codehaus/groovy")) {
      return true;
    }
    if (classname.startsWith("groovy/lang")) {
      return true;
    }
    if (classname.startsWith("org/mockito")) {
      return true;
    }
    if (classname.startsWith("org/objenesis")) {
      return true;
    }
    if (classname.contains("ByMockito")) {
      return true;
    }
    return false;
  }
}
