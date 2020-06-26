package daikon.dcomp;

import daikon.DynComp;
import daikon.plumelib.bcelutil.BcelUtil;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.lang.instrument.ClassFileTransformer;
import java.lang.instrument.IllegalClassFormatException;
import java.security.ProtectionDomain;
import org.apache.bcel.*;
import org.apache.bcel.classfile.*;
import org.apache.bcel.generic.*;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.signature.qual.InternalForm;
import org.checkerframework.dataflow.qual.Pure;

public class Instrument implements ClassFileTransformer {

  File debug_dir;
  File debug_bin_dir;
  File debug_orig_dir;

  public Instrument() {
    debug_dir = DynComp.debug_dir;
    debug_bin_dir = new File(debug_dir, "bin");
    debug_orig_dir = new File(debug_dir, "orig");

    if (DynComp.debug) {
      debug_bin_dir.mkdirs();
      debug_orig_dir.mkdirs();
    }
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

    if (DynComp.verbose) {
      System.out.printf("%ntransform on %s, loader: %s%n", className, loader);
    }

    // See comments in Premain.java about meaning and use of in_shutdown.
    if (Premain.in_shutdown) {
      if (DynComp.verbose) System.out.printf("Skipping in_shutdown class %s%n", className);
      return null;
    }

    if (BcelUtil.javaVersion > 8) {
      // If this class was pre-instrumented (via BuildJDK)
      // let DCInstrument know we need to correct the instrumentation.
      if (Premain.pre_instrumented.contains(className)) {
        DCInstrument.retransforming = true;
      } else {
        if (Premain.retransform_preloads) {
          // Nothing we can do about classes loaded before we got control
          // that were not pre-instrumented.
          if (DynComp.verbose) System.out.printf("Skipping pre-loaded class %s%n", className);
          return null;
        }
        DCInstrument.retransforming = false;
      }
    } else {
      // If already instrumented, nothing to do
      // (This set will be empty if DCInstrument.jdk_instrumented is false)
      if (Premain.pre_instrumented.contains(className)) {
        if (DynComp.verbose) {
          System.out.printf("Skipping pre_instrumented JDK class %s%n", className);
        }
        return null;
      }
    }

    boolean in_jdk = false;

    // Check if class is in JDK
    if (BcelUtil.inJdkInternalform(className)) {
      // If we are not using an instrumented JDK, then skip this class.
      if (!DCInstrument.jdk_instrumented) {
        if (DynComp.verbose) System.out.printf("Skipping JDK class %s%n", className);
        return null;
      }

      if (BcelUtil.javaVersion > 8) {
        int lastSlashPos = className.lastIndexOf('/');
        if (lastSlashPos > 0) {
          String packageName = className.substring(0, lastSlashPos).replace('/', '.');
          if (Premain.problem_packages.contains(packageName)) {
            if (DynComp.verbose) System.out.printf("Skipping problem package %s%n", packageName);
            return null;
          }
        }

        if (Premain.problem_classes.contains(className.replace('/', '.'))) {
          if (DynComp.verbose) System.out.printf("Skipping problem class %s%n", className);
          return null;
        }
      }

      in_jdk = true;
      if (DynComp.verbose) System.out.printf("Instrumenting JDK class %s%n", className);
    } else {

      // We're not in a JDK class
      // Don't instrument our own classes
      if (is_dcomp(className)) {
        if (DynComp.verbose) System.out.printf("Skipping is_dcomp class %s%n", className);
        return null;
      }
    }

    if (DynComp.verbose) {
      System.out.format("In dcomp.Instrument(): class = %s%n", className);
    }

    try {
      // Parse the bytes of the classfile, die on any errors
      ClassParser parser = new ClassParser(new ByteArrayInputStream(classfileBuffer), className);
      JavaClass c = parser.parse();

      if (DynComp.debug) {
        c.dump(new File(debug_orig_dir, c.getClassName() + ".class"));
      }

      // Transform the file
      DCInstrument dci = new DCInstrument(c, in_jdk, loader);
      JavaClass njc;
      if (DynComp.no_primitives) {
        njc = dci.instrument_refs_only();
      } else {
        njc = dci.instrument();
      }

      if (njc == null) {
        if (DynComp.verbose) System.out.printf("Didn't instrument %s%n", c.getClassName());
        return null;
      } else {
        if (DynComp.debug) {
          System.out.printf("Dumping %s to %s%n", njc.getClassName(), debug_bin_dir);
          njc.dump(new File(debug_bin_dir, njc.getClassName() + ".class"));
          BcelUtil.dump(njc, debug_bin_dir);
        }
        return (njc.getBytes());
      }
    } catch (Throwable e) {
      System.out.printf("Unexpected Error: %s%n", e);
      e.printStackTrace();
      throw new RuntimeException("Unexpected error", e);
    }
  }

  /**
   * Returns whether or not the specified class is part of dcomp itself (and thus should not be
   * instrumented). Some Daikon classes that are used by DynComp are included here as well.
   */
  @Pure
  private static boolean is_dcomp(String classname) {

    if ((classname.startsWith("daikon/dcomp/") && !classname.startsWith("daikon/dcomp/DcompTest"))
        || classname.startsWith("daikon/chicory/")) {
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
}
