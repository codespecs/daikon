package daikon.dcomp;

import daikon.DynComp;
import daikon.util.*;
import java.io.*;
import java.lang.instrument.*;
import java.security.*;
import org.apache.bcel.*;
import org.apache.bcel.classfile.*;
import org.apache.bcel.generic.*;

/*>>>
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.checker.signature.qual.*;
*/

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

  @SuppressWarnings("nullness") // bug: java.lang.instrument is not yet annotated
  public byte /*@Nullable*/ [] transform(
      ClassLoader loader,
      /*@InternalForm*/ String className,
      Class<?> classBeingRedefined,
      ProtectionDomain protectionDomain,
      byte[] classfileBuffer)
      throws IllegalClassFormatException {

    // System.out.printf ("transform on %s%n", className);

    // See comments in Premain.java about meaning and use of in_shutdown.
    if (Premain.in_shutdown) return null;

    // If already instrumented, nothing to do
    // (This set will be empty if --no-jdk)
    if (Premain.pre_instrumented.contains(className)) {
      return null;
    }

    boolean in_jdk = false;

    // Check if class is in JDK
    if (BCELUtil.in_jdk_internalform(className)) {
      // If --no-jdk option is active, then skip it.
      if (DynComp.no_jdk) {
        return null;
      }

      in_jdk = true;
      if (DynComp.verbose) System.out.printf("Instrumenting JDK class %s%n", className);
    } else {

      // We're not in a JDK class
      // Don't instrument our own classes
      if ((className.startsWith("daikon/dcomp/") && !className.startsWith("daikon/dcomp/Test"))
          || className.startsWith("daikon/chicory/")) {
        return null;
      }
    }

    if (DynComp.verbose) {
      System.out.format("In dcomp.Instrument(): class = %s\n", className);
    }

    try {
      // Parse the bytes of the classfile, die on any errors
      ClassParser parser = new ClassParser(new ByteArrayInputStream(classfileBuffer), className);
      JavaClass c = parser.parse();

      if (DynComp.debug) {
        c.dump(new File(debug_orig_dir, c.getClassName() + ".class"));
      }

      // Transform the file
      DCInstrument dci;
      if (DynComp.branch != null) {
        dci = new DFInstrument(c, in_jdk, loader);
      } else {
        dci = new DCInstrument(c, in_jdk, loader);
      }
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
          BCELUtil.dump(njc, debug_bin_dir);
        }
        return (njc.getBytes());
      }
    } catch (Throwable e) {
      System.out.printf("Unexpected Error: %s%n", e);
      e.printStackTrace();
      throw new RuntimeException("Unexpected error", e);
    }
  }
}
