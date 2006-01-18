package daikon.dcomp;

import java.lang.instrument.*;
import java.security.*;
import java.io.*;
import java.util.*;
import java.util.regex.*;

import org.apache.bcel.*;
import org.apache.bcel.classfile.*;
import org.apache.bcel.generic.*;

import utilMDE.*;

public class Premain {

  public static File debug_dir = new File ("/tmp", System.getenv ("USER"));
  public static File debug_bin_dir = new File (debug_dir, "bin");
  public static File debug_orig_dir = new File (debug_dir, "orig");

  @Option("-v Print information about the classes being transformed")
  public static boolean verbose = false;

  @Option("-d Dump the instrumented classes to /tmp/$USER/bin")
  public static boolean debug = false;

  @Option("-f Output filename for Daikon decl file")
  public static File decl_file = new File ("comparability.decls");

  @Option("Only process program points matching the regex")
  public static List<Pattern> ppt_select_pattern = new ArrayList<Pattern>();

  @Option("Ignore program points matching the regex")
  public static List<Pattern> ppt_omit_pattern = new ArrayList<Pattern>();

  @Option("Output file for comarability sets")
  public static File compare_sets_file = null;

  @Option("Don't use an instrumented JDK")
  public static boolean no_jdk = false;

  public static String usage_synopsis
    = "java -javaagent:dcomp_premain.jar=[options]";

  public static void premain (String agentArgs, Instrumentation inst) {

    Options options = new Options (usage_synopsis, Premain.class);
    String[] args = options.parse_and_usage (agentArgs.split ("  *"));
    if (args.length > 0) {
      options.print_usage ("Unexpected argument %s", args[0]);
      System.exit (-1);
    }

    if (no_jdk)
      DCInstrument.jdk_instrumented = false;

    if (verbose) {
      System.out.format ("In dcomp premain, agentargs ='%s', " +
                       "Instrumentation = '%s'\n", agentArgs, inst);
      System.out.printf ("Options settings: %n%s%n", options.settings());
    }

    debug_bin_dir.mkdirs();
    debug_orig_dir.mkdirs();

    // Setup the shutdown hook
    Thread shutdown_thread = new ShutdownThread();
    java.lang.Runtime.getRuntime().addShutdownHook (shutdown_thread);

    Transform transformer = new Transform();
    inst.addTransformer (transformer);

    // Initialize the static tag array
    DCRuntime.init();
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

      if (verbose)
        System.out.format ("In Transform: class = %s\n", className);

      try {
        // Parse the bytes of the classfile, die on any errors
        ClassParser parser = new ClassParser
          (new ByteArrayInputStream (classfileBuffer), className);
        JavaClass c = parser.parse();


        if (debug) {
          c.dump (new File (debug_orig_dir, c.getClassName() + ".class"));
        }

        // Transform the file
        DCInstrument dci = new DCInstrument (c, false, loader);
        JavaClass njc = dci.instrument();
        if (njc == null) {
          if (verbose)
            System.out.printf ("Didn't instrument %s%n", c.getClassName());
          return (null);
        } else {
          if (debug) {
            System.out.printf ("Dumping to %s%n", debug_bin_dir);
            njc.dump (new File (debug_bin_dir, njc.getClassName() + ".class"));
            BCELUtil.dump (njc, debug_bin_dir);
          }
          return (njc.getBytes());
        }
      } catch (Throwable e) {
        System.out.printf ("Unexpected Error: %n");
        e.printStackTrace();
        throw new RuntimeException ("Unexpected error: " + e);
      }
    }
  }

  /**
   * Shutdown thread that writes out the comparability results
   */
  public static class ShutdownThread extends Thread {

    public void run() {

      // If requested, write the comparability data to a file
      if (compare_sets_file != null) {
        if (verbose)
          System.out.println ("Writing comparability sets to "
                              + compare_sets_file);
        PrintStream compare_out = open (compare_sets_file);
        DCRuntime.print_all_comparable (compare_out);
      }

      // Write comparability sets to standard out
      if (verbose && (compare_sets_file == null))
      DCRuntime.print_all_comparable (System.out);

      // Write the decl file out
      if (verbose)
        System.out.println("Writing comparability results to " + decl_file);
      PrintStream decl_fp = open (decl_file);
      DCRuntime.print_decl_file (decl_fp);
    }
  }

  public static PrintStream open (File filename) {
    try {
      return new PrintStream (filename);
    } catch (Exception e) {
      throw new Error ("Can't open " + filename, e);
    }
  }
}
