package daikon.dcomp;

import daikon.DynComp;
import daikon.chicory.ClassInfo;
import daikon.plumelib.bcelutil.BcelUtil;
import daikon.plumelib.bcelutil.SimpleLog;
import daikon.plumelib.reflection.Signatures;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.lang.classfile.ClassFile;
import java.lang.classfile.ClassHierarchyResolver;
import java.lang.classfile.ClassModel;
import java.lang.classfile.ClassTransform;
import java.lang.instrument.ClassFileTransformer;
import java.lang.instrument.IllegalClassFormatException;
import java.nio.file.Files;
import java.security.ProtectionDomain;
import org.apache.bcel.classfile.ClassParser;
import org.apache.bcel.classfile.JavaClass;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.signature.qual.BinaryName;
import org.checkerframework.checker.signature.qual.InternalForm;
import org.checkerframework.dataflow.qual.Pure;

/**
 * This class is responsible for modifying another class's bytecodes. Specifically, its main task is
 * to add calls into the DynComp Runtime for instrumentation purposes. These added calls are
 * sometimes referred to as "hooks".
 *
 * <p>This class is loaded by Premain at startup. It is a ClassFileTransformer which means that its
 * {@code transform} method gets called each time the JVM loads a class.
 *
 * <p>Instrument24 and DCInstrument24 use Java's ({@code java.lang.classfile}) APIs for reading and
 * modifying .class files. Those APIs were added in JDK 24. Compared to BCEL, these APIs are more
 * complete and robust (no more fiddling with StackMaps) and are always up to date with any .class
 * file changes (since they are part of the JDK). (We will need to continue to support
 * Instrument.java using BCEL, as we anticipate our clients using JDK 21 or less for quite some
 * time.)
 *
 * <p>The entry point of {@link ClassFileTransformer} is {@link #transform}.
 */
public class Instrument24 implements ClassFileTransformer {

  /** Directory for debug output. */
  File debug_dir;

  /** Directory into which to dump debug-instrumented classes. */
  File debug_instrumented_dir;

  /** Directory into which to dump original classes. */
  File debug_uninstrumented_dir;

  /** Have we seen a class member of a known transformer? */
  private static boolean transformer_seen = false;

  /**
   * Debug information about which classes and/or methods are transformed and why. Use
   * debugInstrument for actual instrumentation details.
   */
  protected static SimpleLog debug_transform = new SimpleLog(false);

  /** Create an instrumenter. Setup debug directories, if needed. */
  public Instrument24() {
    debug_transform.enabled =
        DynComp.debug || DynComp.debug_transform || Premain.debug_dcinstrument || DynComp.verbose;
    daikon.chicory.Instrument24.debug_ppt_omit.enabled = DynComp.debug;

    debug_dir = DynComp.debug_dir;
    debug_instrumented_dir = new File(debug_dir, "instrumented");
    debug_uninstrumented_dir = new File(debug_dir, "uninstrumented");

    // temporary
    daikon.chicory.Instrument.debug_ppt_omit.enabled = DynComp.debug;
    // temporary to turn on DCInstrument.debug_transform
    daikon.dcomp.Instrument.debug_transform.enabled = debug_transform.enabled;

    if (DynComp.dump) {
      debug_instrumented_dir.mkdirs();
      debug_uninstrumented_dir.mkdirs();
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

  // TEMPORARY - REMOVE when DCInstrument24 is working
  /*
   * Output a .oclass file and a .obcel version of the class file.
   *
   * @param c the Java class to output
   * @param directory output location for the files
   * @param className the current class
   */
  private void outputOldDebugFiles(JavaClass c, File directory, @BinaryName String className) {
    try {
      debug_transform.log("Dumping .oclass and .obcel for %s to %s%n", className, directory);
      // Write the byte array to a .oclass file.
      c.dump(new File(directory, className + ".oclass"));
      // write .bcel file
      BcelUtil.dump(c, directory);
      // rename .bcel file to .obcel
      File outputFile = new File(directory, className + ".bcel");
      boolean ok = outputFile.renameTo(new File(directory, className + ".obcel"));
      if (!ok) {
        System.err.printf("Unexpected error renaming %s file for: %s%n", outputFile, className);
        // ignore the error, it shouldn't affect the instrumentation
      }
    } catch (Throwable t) {
      System.err.printf("Unexpected error %s dumping out debug files for: %s%n", t, className);
      t.printStackTrace();
      // ignore the error, it shouldn't affect the instrumentation
    }
  }

  /*
   * Output a .class file and a .bcel version of the class file.
   *
   * @param classBytes a byte array of the class file to output
   * @param directory output location for the files
   * @param className the current class
   */
  private void outputDebugFiles(byte[] classBytes, File directory, @BinaryName String className) {
    // Convert the classBytes to a BCEL JavaClass
    JavaClass c;
    try (ByteArrayInputStream bais = new ByteArrayInputStream(classBytes)) {
      ClassParser parser = new ClassParser(bais, className);
      c = parser.parse();
    } catch (Throwable t) {
      System.err.printf("Unexpected error %s while reading %s%n", t, className);
      t.printStackTrace();
      // ignore the error, it shouldn't affect the instrumentation
      return;
    }

    try {
      debug_transform.log("Dumping .class and .bcel for %s to %s%n", className, directory);
      // Write the byte array to a .class file.
      File outputFile = new File(directory, className + ".class");
      Files.write(outputFile.toPath(), classBytes);
      // Write a BCEL-like file.
      BcelUtil.dump(c, directory);
      // TEMPORARY - REMOVE when DCInstrument24 is working
      // rename .bcel file to .nbcel
      outputFile = new File(directory, className + ".bcel");
      boolean ok = outputFile.renameTo(new File(directory, className + ".nbcel"));
      if (!ok) {
        System.err.printf("Unexpected error renaming %s file for: %s%n", outputFile, className);
        // ignore the error, it shouldn't affect the instrumentation
      }
    } catch (Throwable t) {
      System.err.printf("Unexpected error %s dumping out debug files for: %s%n", t, className);
      t.printStackTrace();
      // ignore the error, it shouldn't affect the instrumentation
    }
  }

  /**
   * Given a class, return a transformed version of the class that contains instrumentation code.
   * Because DynComp is invoked as a javaagent, the transform method is called by the Java runtime
   * each time a new class is loaded. A return value of null leaves the byte codes unchanged.
   */
  @Override
  public byte @Nullable [] transform(
      @Nullable ClassLoader loader,
      @InternalForm String className,
      @Nullable Class<?> classBeingRedefined,
      ProtectionDomain protectionDomain,
      byte[] classfileBuffer)
      throws IllegalClassFormatException {

    // for debugging
    // new Throwable().printStackTrace();

    debug_transform.log("Entering dcomp.Instrument24.transform(): class = %s%n", className);

    @BinaryName String binaryClassName = Signatures.internalFormToBinaryName(className);

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
      debug_transform.log("Skipping in_shutdown class %s%n", binaryClassName);
      return null;
    }

    // If already instrumented, nothing to do
    // (This set will be empty if Premain.jdk_instrumented is false)
    if (Premain.pre_instrumented.contains(className)) {
      debug_transform.log("Skipping pre_instrumented JDK class %s%n", binaryClassName);
      return null;
    }

    boolean in_jdk = false;

    // Check if class is in JDK
    if (BcelUtil.inJdkInternalform(className)) {
      // If we are not using an instrumented JDK, then skip this class.
      if (!Premain.jdk_instrumented) {
        debug_transform.log("Skipping JDK class %s%n", binaryClassName);
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
        if (Premain.problem_classes.contains(binaryClassName)) {
          debug_transform.log("Skipping problem class %s%n", binaryClassName);
          return null;
        }
      }

      if (className.contains("/$Proxy")) {
        debug_transform.log("Skipping proxy class %s%n", binaryClassName);
        return null;
      }

      if (className.equals("java/lang/DCRuntime")) {
        debug_transform.log("Skipping special DynComp runtime class %s%n", binaryClassName);
        return null;
      }

      in_jdk = true;
      debug_transform.log("Instrumenting JDK class %s%n", binaryClassName);
    } else {

      // We're not in a JDK class
      // Don't instrument our own classes
      if (is_dcomp(className)) {
        debug_transform.log("Skipping is_dcomp class %s%n", binaryClassName);
        return null;
      }

      // Don't instrument other byte code transformers
      if (is_transformer(className)) {
        debug_transform.log("Skipping is_transformer class %s%n", binaryClassName);
        if (!transformer_seen) {
          transformer_seen = true;
          System.err.printf(
              "DynComp warning: This program uses a Java byte code transformer: %s%n",
              binaryClassName);
          System.err.printf(
              "This may interfere with the DynComp transformer and cause DynComp to fail.%n");
        }
        return null;
      }
    }

    ClassLoader cfLoader;
    if (loader == null) {
      cfLoader = ClassLoader.getSystemClassLoader();
      debug_transform.log("Transforming class %s, loader %s - %s%n", className, loader, cfLoader);
    } else {
      cfLoader = loader;
      debug_transform.log(
          "Transforming class %s, loader %s - %s%n", className, loader, loader.getParent());
    }

    // Do instrumentation both old and new way during development.
    // Following code is old way.

    // Parse the bytes of the classfile, die on any errors.
    JavaClass c;
    try (ByteArrayInputStream bais = new ByteArrayInputStream(classfileBuffer)) {
      ClassParser parser = new ClassParser(bais, className);
      c = parser.parse();
    } catch (Throwable t) {
      System.err.printf("Unexpected error %s while reading %s%n", t, binaryClassName);
      t.printStackTrace();
      // No changes to the bytecodes
      return null;
    }

    if (DynComp.dump) {
      outputOldDebugFiles(c, debug_uninstrumented_dir, binaryClassName);
    }

    // End of old code.

    // Start of new code.

    // Parse the bytes of the classfile, die on any errors.
    ClassFile classFile =
        ClassFile.of(
            ClassFile.ClassHierarchyResolverOption.of(
                ClassHierarchyResolver.ofResourceParsing(cfLoader)));

    ClassModel classModel;
    try {
      classModel = classFile.parse(classfileBuffer);
    } catch (Throwable t) {
      System.err.printf("Unexpected error %s reading in %s%n", t, binaryClassName);
      t.printStackTrace();
      // No changes to the bytecodes
      return null;
    }

    if (DynComp.dump) {
      outputDebugFiles(
          classFile.transformClass(classModel, ClassTransform.ACCEPT_ALL),
          debug_uninstrumented_dir,
          binaryClassName);
    }

    // Instrument the classfile, die on any errors.
    ClassInfo classInfo = new ClassInfo(binaryClassName, cfLoader);
    DCInstrument24 dci = new DCInstrument24(classFile, classModel, in_jdk);
    byte @Nullable [] newBytes = null;
    try {
      newBytes = dci.instrument(classInfo);
    } catch (Throwable t) {
      System.err.printf("Unexpected error %s in transform of %s%n", t, binaryClassName);
      t.printStackTrace();
      throw new RuntimeException("Unexpected error", t);
    }

    if (newBytes != null) {
      if (DynComp.dump) {
        outputDebugFiles(newBytes, debug_instrumented_dir, binaryClassName);
      }
      // return newBytes;
    } else {
      // debug_transform.log("Didn't instrument %s%n", binaryClassName);
      // No changes to the bytecodes
      // return null;
    }

    // End of new code.

    // Do instrumentation both old and new way during development.
    // Following code is old way.

    // Instrument the classfile, die on any errors
    JavaClass njc;
    try {
      DCInstrument dcio = new DCInstrument(c, in_jdk, loader);
      njc = dcio.instrument();
    } catch (Throwable t) {
      RuntimeException re =
          new RuntimeException(
              String.format("Unexpected error %s in transform of %s", t, binaryClassName), t);
      re.printStackTrace();
      throw re;
    }

    if (njc != null) {
      if (DynComp.dump) {
        outputOldDebugFiles(njc, debug_instrumented_dir, binaryClassName);
      }
      return njc.getBytes();
    } else {
      debug_transform.log("Didn't instrument %s%n", binaryClassName);
      // No changes to the bytecodes
      return null;
    }

    // End of old code.

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
