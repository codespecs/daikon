package daikon.dcomp;

import java.lang.instrument.*;
import java.security.*;
import java.io.*;

import org.apache.bcel.*;
import org.apache.bcel.classfile.*;
import org.apache.bcel.generic.*;

import utilMDE.BCELUtil;

public class Premain {

  public static File debug_dir = new File ("/tmp", System.getenv ("USER"));
  public static File debug_bin_dir = new File (debug_dir, "bin");
  public static File debug_orig_dir = new File (debug_dir, "orig");
  public static boolean debug = true;

  public static void premain (String agentArgs, Instrumentation inst) {

    System.out.format ("In dcomp premain, agentargs ='%s', " +
                       "Instrumentation = '%s'\n", agentArgs, inst);

    debug_bin_dir.mkdirs();
    debug_orig_dir.mkdirs();

    Transform transformer = new Transform();
    inst.addTransformer (transformer);
  }

  static public class Transform implements ClassFileTransformer {

    public Transform() {
    }

    public byte[] transform (ClassLoader loader, String className,
                           Class<?> classBeingRedefined,
                           ProtectionDomain protectionDomain,
                           byte[] classfileBuffer)
                                  throws IllegalClassFormatException {

      // Don't instrument JDK classes (but allow instrumentation of the java
      // compiler)
      if ((className.startsWith ("java/") || className.startsWith ("com/")
           || className.startsWith ("sun/"))
          && !className.startsWith ("com/sun/tools/javac"))
        return (null);

      // Don't instrument our own classes
      if ((className.startsWith ("daikon/dcomp/")
           && !className.startsWith ("daikon/dcomp/Test"))
          || className.startsWith ("utilMDE")
          || className.startsWith ("daikon/chicory/"))
        return (null);

      System.out.format ("In Transform: class = %s\n", className);

      // Parse the bytes of the classfile, die on any errors
      JavaClass c = null;
      ClassParser parser = new ClassParser
        (new ByteArrayInputStream (classfileBuffer), className);
      try {
        c = parser.parse();
      } catch (Exception e) {
        throw new RuntimeException ("Unexpected error: " + e);
      }

      if (debug) {
        try {
          c.dump (new File (debug_orig_dir, c.getClassName() + ".class"));
        } catch (Exception e) {
          throw new Error ("can't dump " + c.getClassName(), e);
        }
      }

      // Transform the file
      DCInstrument dci = new DCInstrument (c, false, loader);
      JavaClass njc = null;
      try {
        njc = dci.instrument();
      } catch (Throwable t) {
        System.out.printf ("Unexected error: %s%n", t);
        t.printStackTrace();
        return (null);
      }
      if (debug) {
        try {
          System.out.printf ("Dumping to %s%n", debug_bin_dir);
          njc.dump (new File (debug_bin_dir, njc.getClassName() + ".class"));
          BCELUtil.dump (njc, debug_bin_dir);
        } catch (Exception e) {
          throw new Error ("can't dump " + njc.getClassName(), e);
        }
      }
      return (njc.getBytes());
    }
  }

}
