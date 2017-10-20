package daikon;

import daikon.chicory.StreamRedirectThread;
import daikon.dcomp.*;
import daikon.util.*;
import java.io.*;
import java.util.*;
import java.util.regex.Pattern;

/*>>>
import org.checkerframework.checker.nullness.qual.*;
*/

/**
 * This is the main class for DynComp. It uses the javaagent switch to java (which allows classes to
 * be instrumented as they are loaded). This class parses the command line arguments and starts java
 * with the javaagent switch on the target program. Code based largely on daikon.Chicory.
 */
public class DynComp {

  @Option("-v Print information about the classes being transformed")
  public static boolean verbose = false;

  @Option("-d Dump the instrumented classes to disk")
  public static boolean debug = false;

  @Option("Print detailed information on which classes are transformed")
  public static boolean debug_transform = false;

  @Option("Print detailed information on variables being observed")
  public static boolean debug_decl_print = false;

  @Option("Directory in which to create debug files")
  public static File debug_dir = new File("debug");

  @Option("Directory in which to create output files")
  public static File output_dir = new File(".");

  @Option("-f Output filename for Daikon decl file")
  public static /*@Nullable*/ String decl_file = null;

  @Option("Output file for comparability sets")
  // If null, do no output
  public static /*@Nullable*/ File comparability_file = null;

  @Option("Only process program points matching the regex")
  public static List<Pattern> ppt_select_pattern = new ArrayList<Pattern>();

  @Option("Ignore program points matching the regex")
  public static List<Pattern> ppt_omit_pattern = new ArrayList<Pattern>();

  @Option("Don't track primitives")
  public static boolean no_primitives = false;

  // Option("Don't use an instrumented JDK")
  // Flag is still used, but no longer exposed as an option.
  public static boolean no_jdk = false;

  @Option("jar file containing an instrumented JDK")
  public static /*@Nullable*/ File rt_file = null;

  @Option("use standard visibility")
  public static boolean std_visibility = false;

  @Option("variable nesting depth")
  public static int nesting_depth = 2;

  @Option("Display abridged variable names")
  public static boolean abridged_vars = false;

  @Option("Use faster but less precise algorithm on omitted ppts")
  public static boolean approximate_omitted_ppts = false;

  @Option("Don't continue after instrumentation error")
  public static boolean quit_if_error = false;

  @Option("Trace output file")
  // Null if shouldn't do output
  public static /*@Nullable*/ File trace_file = null;

  @Option("Depth of call hierarchy for line tracing")
  public static int trace_line_depth = 1;

  @Option("Output file for DataFlow information")
  // Null if shouldn't do output
  public static /*@Nullable*/ File dataflow_out = null;

  //  @Option("Enable tracing");
  //  public static boolean tracing_enabled = true;

  public static String usage_synopsis = "java daikon.DynComp [options]";

  /**
   * Path to java agent jar file that performs the transformation. The "main" procedure is
   * Premain.premain().
   *
   * @see Premain#premain
   */
  // Set by start_target()
  @Option("Path to the DynComp agent jar file (usually dcomp_premain.jar)")
  public static /*@MonotonicNonNull*/ File premain = null;

  // /** Thread that copies output from target to our output */
  // public static StreamRedirectThread out_thread;

  // /** Thread that copies stderr from target to our stderr */
  // public static StreamRedirectThread err_thread;

  /** starting time (msecs) */
  public static long start = System.currentTimeMillis();

  private static final SimpleLog basic = new SimpleLog(false);

  /** Synopsis for the dcomp command line */
  public static final String synopsis = "daikon.DynComp [options] target [target-args]";

  /**
   * Entry point of DynComp
   *
   * @param args see usage for argument descriptions
   */
  public static void main(String[] args) {

    // Parse our arguments
    Options options = new Options(synopsis, DynComp.class);
    // options.ignore_options_after_arg (true);
    String[] target_args = options.parse_or_usage(args);
    boolean ok = check_args(options, target_args);
    if (!ok) System.exit(1);

    // Turn on basic logging if the debug was selected
    basic.enabled = debug;
    basic.log("target_args = %s%n", Arrays.toString(target_args));

    // Start the target.  Pass the same options to the premain as
    // were passed here.

    DynComp dcomp = new DynComp();
    dcomp.start_target(options.get_options_str(), target_args);
  }

  /**
   * Check the resulting arguments for legality. Prints a message and returns false if there was an
   * error.
   */
  public static boolean check_args(Options options, String[] target_args) {

    // Make sure arguments have legal values
    if (nesting_depth < 0) {
      options.print_usage("nesting depth (%d) must not be negative", nesting_depth);
      return false;
    }
    if (target_args.length == 0) {
      options.print_usage("target program must be specified");
      return false;
    }
    if (rt_file != null && rt_file.getName().equalsIgnoreCase("NONE")) {
      no_jdk = true;
      rt_file = null;
    }
    if (!no_jdk && rt_file != null && !rt_file.exists()) {
      // if --rt-file was given, but doesn't exist
      options.print_usage("rt-file %s does not exist", rt_file);
      return false;
    }

    return true;
  }

  /**
   * Starts the target program with the java agent setup to do the transforms. All java agent
   * arguments are passed to it. Our classpath is passed to the new JVM.
   */
  /*TO DO: @PostNonNull("premain")*/
  void start_target(String premain_args, String[] target_args) {

    String target_class = target_args[0].replaceFirst(".*[/.]", "");

    // Default the decls file to <target-program-name>.decls-DynComp
    if (decl_file == null) {
      decl_file = String.format("%s.decls-DynComp", target_class);
      premain_args = "--decl-file=" + decl_file + " " + premain_args;
    }

    // Get the current classpath
    String cp = System.getProperty("java.class.path");
    basic.log("classpath = '%s'\n", cp);
    if (cp == null) cp = ".";

    // The the separator for items in the class path
    String path_separator = System.getProperty("path.separator");
    basic.log("path_separator = %s\n", path_separator);
    if (path_separator == null) {
      path_separator = ";"; //should work for windows at least...
    } else if (!RegexUtil.isRegex(path_separator)) {
      throw new Daikon.TerminationMessage(
          "Bad regexp "
              + path_separator
              + " for path.separator: "
              + RegexUtil.regexError(path_separator));
    }

    // Look for dcomp_premain.jar along the classpath
    if (premain == null) {
      String[] cpath = cp.split(path_separator);
      for (String path : cpath) {
        File poss_premain;
        if (path.endsWith("dcomp_premain.jar")) {
          poss_premain = new File(path);
        } else {
          poss_premain = new File(path, "dcomp_premain.jar");
        }
        // System.out.printf ("looking for file %s%n", poss_premain);
        if (poss_premain.canRead()) {
          premain = poss_premain;
          break;
        }
      }
    }

    // If not on the classpath look in ${DAIKONDIR}/java
    if (premain == null) {
      String daikon_dir = System.getenv("DAIKONDIR");
      if (daikon_dir != null) {
        String file_separator = System.getProperty("file.separator");
        File poss_premain = new File(daikon_dir + file_separator + "java", "dcomp_premain.jar");
        if (poss_premain.canRead()) premain = poss_premain;
      }
    }

    // If we didn't find a premain, give up
    if (premain == null) {
      System.err.printf("Can't find dcomp_premain.jar on the classpath\n");
      System.err.printf("or in $DAIKONDIR/java\n");
      System.err.printf("It should be found in directory where Daikon was " + "installed\n");
      System.err.printf("Use the --premain switch to specify its location\n");
      System.err.printf("or change your classpath to include it\n");
      System.exit(1);
    }

    // Look for rt-file
    if (!no_jdk) {
      // Look for dcomp_rt.jar along the classpath
      if (rt_file == null) {
        String[] cpath = cp.split(path_separator);
        for (String path : cpath) {
          File poss_rt;
          if (path.endsWith("dcomp_rt.jar")) {
            poss_rt = new File(path);
          } else {
            poss_rt = new File(path, "dcomp_rt.jar");
          }
          if (poss_rt.canRead()) {
            rt_file = poss_rt;
            break;
          }
        }
      }

      // If not on the classpath look in ${DAIKONDIR}/java
      if (rt_file == null) {
        String daikon_dir = System.getenv("DAIKONDIR");
        if (daikon_dir != null) {
          String file_separator = System.getProperty("file.separator");
          File poss_rt = new File(daikon_dir + file_separator + "java", "dcomp_rt.jar");
          if (poss_rt.canRead()) rt_file = poss_rt;
        }
      }

      // If we didn't find a rt-file, give up
      if (rt_file == null) {
        System.err.printf("Can't find dcomp_rt.jar on the classpath " + "or in $DAIKONDIR/java\n");
        System.err.printf("Probably you forgot to build it.\n");
        System.err.printf(
            "See the Daikon manual, section \"Instrumenting the "
                + "JDK with DynComp\" for help.\n");
        System.exit(1);
      }
    }

    // Build the command line to execute the target with the javaagent
    List<String> cmdlist = new ArrayList<String>();
    cmdlist.add("java");
    // cmdlist.add ("-verbose:class");
    cmdlist.add("-cp");
    cmdlist.add(cp);
    cmdlist.add("-ea");
    cmdlist.add("-esa");
    // get max memory given DynComp and pass on to dcomp_premain
    // rounded up to nearest G(igabyte)
    cmdlist.add(
        "-Xmx" + (int) Math.ceil(java.lang.Runtime.getRuntime().maxMemory() / 1073741824.0) + "G");
    if (!no_jdk) {
      // prepend to rather than replace bootclasspath
      cmdlist.add("-Xbootclasspath/p:" + rt_file + path_separator + cp);
    }

    cmdlist.add(String.format("-javaagent:%s=%s", premain, premain_args));

    for (String target_arg : target_args) {
      cmdlist.add(target_arg);
    }
    if (verbose) {
      System.out.printf("\nExecuting target program: %s\n", args_to_string(cmdlist));
    }
    String[] cmdline = cmdlist.toArray(new String[cmdlist.size()]);

    // Execute the command, sending all output to our streams
    java.lang.Runtime rt = java.lang.Runtime.getRuntime();
    Process dcomp_proc;
    try {
      dcomp_proc = rt.exec(cmdline);
    } catch (Throwable e) {
      System.out.printf("Exception '%s' while executing '%s'\n", e, cmdline);
      System.exit(1);
      throw new Error("Unreachable control flow");
    }
    int result = redirect_wait(dcomp_proc);

    // XXX check result!
  }

  /** Wait for stream redirect threads to complete */
  public int redirect_wait(Process p) {

    // Create the redirect theads and start them
    @SuppressWarnings("nullness") // didn't redirect stream, so getter returns non-null
    StreamRedirectThread err_thread =
        new StreamRedirectThread("stderr", p.getErrorStream(), System.err, true);

    @SuppressWarnings("nullness") // didn't redirect stream, so getter returns non-null
    StreamRedirectThread out_thread =
        new StreamRedirectThread("stdout", p.getInputStream(), System.out, true);

    err_thread.start();
    out_thread.start();

    // Wait for the process to terminate and return the results
    int result = -1;
    while (true) {
      try {
        result = p.waitFor();
        break;
      } catch (InterruptedException e) {
        System.out.printf("unexpected interrupt %s while waiting for " + "target to finish", e);
      }
    }

    // Make sure all output is forwarded before we finish
    try {
      err_thread.join();
      out_thread.join();
    } catch (InterruptedException e) {
      System.out.printf("unexpected interrupt %s while waiting for " + "threads to join", e);
    }

    return result;
  }

  /** Returns elapsed time as a String since the start of the program */
  public static String elapsed() {
    return ("[" + (System.currentTimeMillis() - start) + " msec]");
  }

  public static long elapsed_msecs() {
    return (System.currentTimeMillis() - start);
  }

  /** convert a list of arguments into a command line string */
  public String args_to_string(List<String> args) {
    String str = "";
    for (String arg : args) {
      str += arg + " ";
    }
    return (str.trim());
  }
}
