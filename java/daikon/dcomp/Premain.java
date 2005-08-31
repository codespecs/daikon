package daikon.dcomp;

import java.lang.instrument.*;
import java.security.*;
import java.io.*;
import java.util.*;
import java.util.regex.*;

import org.apache.bcel.*;
import org.apache.bcel.classfile.*;
import org.apache.bcel.generic.*;

import utilMDE.BCELUtil;

public class Premain {

  public static File debug_dir = new File ("/tmp", System.getenv ("USER"));
  public static File debug_bin_dir = new File (debug_dir, "bin");
  public static File debug_orig_dir = new File (debug_dir, "orig");
  public static boolean debug = true;
  public static List<Pattern> ppt_select_pattern = new ArrayList<Pattern>();
  public static List<Pattern> ppt_omit_pattern = new ArrayList<Pattern>();
  public static String compare_sets_file = null;

  public static void premain (String agentArgs, Instrumentation inst) {

    System.out.format ("In dcomp premain, agentargs ='%s', " +
                       "Instrumentation = '%s'\n", agentArgs, inst);

    String[] args = agentArgs.split ("  *");
    String error_msg = parse_args (args);
    if (error_msg != null) {
      usage (error_msg);
      System.exit (1);
    }

    debug_bin_dir.mkdirs();
    debug_orig_dir.mkdirs();

    // Setup the shutdown hook
    Thread shutdown_thread = new ShutdownThread();
    java.lang.Runtime.getRuntime().addShutdownHook (shutdown_thread);

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
        System.exit (1);
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

  static String parse_args (String[] args) {

    for (int ii = 0; ii < args.length; ii++) {

      String arg = args[ii];
      if (arg.startsWith ("--ppt-select-pattern=")) {
        String include = arg.substring ("--ppt-select-pattern=".length());
        if (include.length() == 0)
          return ("Empty ppt-select-pattern string");
        try {
          ppt_select_pattern.add (Pattern.compile (include));
        } catch (Exception e) {
          return String.format ("Can't compile pattern %s: %s%n", include, e);
        }
      } else if (arg.startsWith ("--ppt-omit-pattern=")) {
        String omit = arg.substring ("--ppt-omit-pattern=".length());
        if (omit.length() == 0)
          return ("Empty ppt-omit-pattern string");
        try {
          ppt_omit_pattern.add (Pattern.compile (omit));
        } catch (Exception e) {
          return String.format ("Can't compile pattern %s: %s%n", omit, e);
        }
      } else if (arg.startsWith ("--compare-sets-file=")) {
        compare_sets_file = arg.substring ("--compare-sets-file=".length());
      } else {
        return ("Unexpected argument " + arg);
      }
    }
    return (null);
  }

  public static void usage (String msg) {

    System.out.println (msg);
    System.out.println ("dcomp <options>");
    System.out.println ("Options:");
    System.out.println ("  --ppt-select-pattern=<regex>");
    System.out.println ("  --ppt-omit-pattern=<regex>");
  }

  /**
   * Shutdown thread that writes out the comparability results
   */
  public static class ShutdownThread extends Thread {

    public void run() {

      // If requested, write the comparability data to a file
      if (compare_sets_file != null) {
        PrintStream compare_out = open (compare_sets_file);
        DCRuntime.print_all_comparable (compare_out);
      }

      // Write comparability sets to standard out
      DCRuntime.print_all_comparable (System.out);

      // Write the decl file out
      String comp_out_fname = "/tmp/dcomp.decls";
      System.out.println("Writing comparability results to " + comp_out_fname);
      PrintStream comp_out = open (comp_out_fname);
      DCRuntime.print_decl_file (comp_out);
    }
  }

  public static PrintStream open (String filename) {
    try {
      return new PrintStream (filename);
    } catch (Exception e) {
      throw new Error ("Can't open " + filename, e);
    }
  }
}
