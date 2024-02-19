package daikon;

import daikon.chicory.StreamRedirectThread;
import daikon.plumelib.bcelutil.BcelUtil;
import daikon.plumelib.bcelutil.SimpleLog;
import daikon.plumelib.options.Option;
import daikon.plumelib.options.Options;
import daikon.plumelib.util.RegexUtil;
import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;
import org.checkerframework.checker.nullness.qual.Nullable;

/**
 * This is the main class for DynComp. It uses the -javaagent switch to Java (which allows classes
 * to be instrumented as they are loaded). This class parses the command line arguments and starts
 * Java with the -javaagent switch on the target program. Code based largely on daikon.Chicory.
 */
public class DynComp {

  /** Display usage information. */
  @Option("-h Display usage information")
  public static boolean help = false;

  /** Print information about the classes being transformed. */
  @Option("-v Print progress information")
  public static boolean verbose = false;

  /** Dump the instrumented classes to disk. */
  @Option("Dump the instrumented classes to disk")
  public static boolean dump = false;

  /** Output debugging information. */
  @Option("-d Output debugging information (implies --dump)")
  public static boolean debug = false;

  /** The directory in which to dump instrumented class files. */
  @Option("Directory in which to create debug files")
  public static File debug_dir = new File("debug");

  /** The directory in which to create output files. */
  @Option("Directory in which to create output files")
  public static File output_dir = new File(".");

  /** Output filename for .decls file suitable for input to Daikon. */
  @Option("-f Output filename for Daikon decl file")
  public static @Nullable String decl_file = null;

  /** Output filename for a more easily human-readable file summarizing comparability sets. */
  @Option("Output file for comparability sets")
  // If null, do no output
  public static @MonotonicNonNull File comparability_file = null;

  /** If specified, write a human-readable file showing some of the interactions that occurred. */
  @Option("Trace output file")
  // Null if shouldn't do output
  public static @MonotonicNonNull File trace_file = null;

  /** Controls size of the stack displayed in tracing the interactions that occurred. */
  @Option("Depth of call hierarchy for line tracing")
  public static int trace_line_depth = 1;

  /** Causes DynComp to abridge the variable names printed. */
  @Option("Display abridged variable names")
  public static boolean abridged_vars = false;

  /** Only emit program points that match regex. */
  @Option("Only process program points matching the regex")
  public static List<Pattern> ppt_select_pattern = new ArrayList<>();

  /** Suppress program points that match regex. */
  @Option("Ignore program points matching the regex")
  public static List<Pattern> ppt_omit_pattern = new ArrayList<>();

  /** Specifies the location of the instrumented JDK. */
  @Option("jar file containing an instrumented JDK")
  public static @Nullable File rt_file = null;

  /** Causes DynComp to traverse exactly those fields visible from a given program point. */
  @Option("use standard visibility")
  public static boolean std_visibility = false;

  /** Depth to which to examine structure components. */
  @Option("variable nesting depth")
  public static int nesting_depth = 2;

  /**
   * Path to Java agent .jar file that performs the transformation. The "main" procedure is {@code
   * Premain.premain()}.
   *
   * @see daikon.dcomp.Premain#premain
   */
  // Set by start_target()
  @Option("Path to the DynComp agent jar file (usually dcomp_premain.jar)")
  public static @Nullable File premain = null;

  /** Holds the path to "daikon.jar" or to "daikon/java:daikon/java/lib*". */
  // Set by start_target()
  public static @MonotonicNonNull String daikonPath = null;

  /** The current class path. */
  static @MonotonicNonNull String cp = null;

  /** The current path separator. */
  static @MonotonicNonNull String path_separator = null;

  /** The current file separator. */
  static @MonotonicNonNull String file_separator = null;

  /** Contains the expansion of java/lib/* if it is on the classpath. */
  static @Nullable String java_lib_path = null;

  /** The contents of DAIKONDIR environment setting. */
  static @Nullable String daikon_dir = null;

  // The following are internal debugging options primarily for use by the DynComp maintainers.
  // They are not documented in the Daikon User Manual.

  /** Print detailed information on which classes are transformed. */
  @Option("Print detailed information on which classes are transformed")
  public static boolean debug_transform = false;

  /** Print detailed information on variables being observed. */
  @Option("Print detailed information on variables being observed")
  public static boolean debug_decl_print = false;

  // Note that this is derived from the rt_file option.  There is no command-line argument that
  // corresponds to this variable.
  /** Do not use the instrumented JDK. */
  public static boolean no_jdk = false;

  /** starting time (msecs) */
  public static long start = System.currentTimeMillis();

  /** Log file if debug is enabled. */
  private static final SimpleLog basic = new SimpleLog(false);

  /** Synopsis for the DynComp command line. */
  public static final String synopsis = "daikon.DynComp [options] target [target-args]";

  /**
   * Locate where a class was loaded from.
   *
   * @param c class to look up
   * @return path to class or null if not found
   */
  public static @Nullable String whereFrom(Class<?> c) {
    if (c == null) {
      return null;
    }
    ClassLoader loader = c.getClassLoader();
    if (loader == null) {
      // Try the bootstrap classloader - obtained from the ultimate parent of the System Class
      // Loader.
      loader = ClassLoader.getSystemClassLoader();
      while (loader != null && loader.getParent() != null) {
        loader = loader.getParent();
      }
    }
    if (loader != null) {
      String name = c.getCanonicalName();
      if (name != null) {
        URL resource = loader.getResource(name.replace(".", "/") + ".class");
        if (resource != null) {
          return resource.toString();
        }
      }
    }
    return null;
  }

  /**
   * Entry point of DynComp.
   *
   * @param args see usage for argument descriptions
   */
  public static void main(String[] args) {

    // Parse our arguments
    Options options = new Options(synopsis, DynComp.class);
    options.setParseAfterArg(false);
    String[] target_args = options.parse(true, args);
    check_args(options, target_args);

    // Turn on basic logging if debug was selected
    basic.enabled = debug;
    basic.log("target_args = %s%n", Arrays.toString(target_args));

    // Start the target.  Pass the same options to the premain as
    // were passed here.

    DynComp dcomp = new DynComp();
    dcomp.start_target(options.getOptionsString(), target_args);
  }

  /**
   * Check the command-line arguments for legality. Prints a message and exits if there was an
   * error.
   *
   * @param options set of legal options to DynComp
   * @param target_args arguments being passed to the target program
   */
  public static void check_args(Options options, String[] target_args) {
    if (help) {
      options.printUsage();
      System.exit(1);
    }
    if (nesting_depth < 0) {
      System.out.printf("nesting depth (%d) must not be negative%n", nesting_depth);
      options.printUsage();
      System.exit(1);
    }
    if (target_args.length == 0) {
      System.out.println("target program must be specified");
      options.printUsage();
      System.exit(1);
    }
    if (rt_file != null && rt_file.getName().equalsIgnoreCase("NONE")) {
      no_jdk = true;
      rt_file = null;
    }
    if (!no_jdk && rt_file != null && !rt_file.exists()) {
      // if --rt-file was given, but doesn't exist
      System.out.printf("rt-file %s does not exist%n", rt_file);
      options.printUsage();
      System.exit(1);
    }
  }

  /**
   * Starts the target program with the Java agent setup to do the transforms. All Java agent
   * arguments are passed to it. Our classpath is passed to the new JVM.
   *
   * @param premain_args the Java agent argument list
   * @param target_args the test program name and its argument list
   */
  /*TO DO: @PostNonNull("premain")*/
  void start_target(String premain_args, String[] target_args) {

    // Default the decls file name to <target-program-name>.decls-DynComp
    if (decl_file == null) {
      String target_class = target_args[0].replaceFirst(".*[/.]", "");
      decl_file = String.format("%s.decls-DynComp", target_class);
      premain_args += " --decl-file=" + decl_file;
    }

    // Get the current classpath
    cp = System.getProperty("java.class.path");
    basic.log("classpath = '%s'%n", cp);
    if (cp == null) {
      cp = ".";
    }

    // Get location of DAIKONDIR, may be empty
    daikon_dir = System.getenv("DAIKONDIR");

    // The separator for items in the class path
    path_separator = System.getProperty("path.separator");
    basic.log("path_separator = %s%n", path_separator);
    if (!RegexUtil.isRegex(path_separator)) {
      throw new Daikon.UserError(
          "Bad regexp "
              + path_separator
              + " for path.separator: "
              + RegexUtil.regexError(path_separator));
    }

    file_separator = System.getProperty("file.separator");
    // Find where DynComp was loaded from.
    String wheresDynComp = whereFrom(DynComp.class);
    if (wheresDynComp != null) {
      if (wheresDynComp.startsWith("file:")) {
        wheresDynComp = wheresDynComp.substring(5);
        int end = wheresDynComp.indexOf("daikon" + file_separator + "DynComp.class");
        if (end != -1) {
          // We found DynComp on the classpath, not in daikon.jar. This occurs, for example,
          // when a developer is testing daikon changes prior to building a new daikon.jar.
          // In this case, we need to add either "java/lib/*" or "daikon.jar" to the classpath
          // as well. locateFile has a special case for "java/lib/*".
          File daikonPathFile = locateFile("daikon.jar");
          if (daikonPathFile != null) {
            // Empty file name is flag to use java_lib_path instead.
            if (daikonPathFile.getPath().length() == 0) {
              daikonPath = wheresDynComp.substring(0, end - 1) + java_lib_path;
            } else {
              daikonPath =
                  wheresDynComp.substring(0, end - 1) + path_separator + daikonPathFile.getPath();
            }
          }
        }
      } else if (wheresDynComp.startsWith("jar:file:")) {
        int end = wheresDynComp.indexOf('!');
        if (end != -1) {
          daikonPath = wheresDynComp.substring(9, end);
        }
      }
    }
    if (daikonPath == null) {
      System.err.printf("Cannot locate Daikon directory or daikon.jar.%n");
      System.exit(1);
    }
    // debuging
    System.out.println("daikonPath: " + daikonPath);

    // Look for location of dcomp_premain.jar
    if (premain == null) {
      premain = locateFile("dcomp_premain.jar");
    }

    // If we didn't find a premain, give up
    if (premain == null) {
      System.err.printf("Can't find dcomp_premain.jar on the classpath");
      if (daikon_dir == null) {
        System.err.printf(" and $DAIKONDIR is not set.%n");
      } else {
        System.err.printf(" or in $DAIKONDIR/java .%n");
      }
      System.err.printf("It should be found in the directory where Daikon was installed.%n");
      System.err.printf("Use the --premain switch to specify its location,%n");
      System.err.printf("or change your classpath to include it.%n");
      System.exit(1);
    }

    // Do we need rt-file?
    if (!no_jdk) {
      // Look for location of dcomp_rt.jar
      if (rt_file == null) {
        rt_file = locateFile("dcomp_rt.jar");
      }

      // If we didn't find a rt-file, give up
      if (rt_file == null) {
        System.err.printf("Can't find dcomp_rt.jar on the classpath");
        if (daikon_dir == null) {
          System.err.printf(" and $DAIKONDIR is not set.%n");
        } else {
          System.err.printf(" or in $DAIKONDIR/java .%n");
        }
        System.err.printf("Probably you forgot to build it.%n");
        System.err.printf(
            "See the Daikon manual, section \"Instrumenting the JDK with DynComp\" for help.%n");
        System.exit(1);
      }
    }

    // Build the command line to execute the target with the javaagent
    List<String> cmdlist = new ArrayList<>();
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

    if (BcelUtil.javaVersion <= 8) {
      if (!no_jdk) {
        // prepend to rather than replace bootclasspath
        cmdlist.add("-Xbootclasspath/p:" + rt_file + path_separator + daikonPath);
      }
    } else {
      // allow DCRuntime to make reflective access to java.land.Object.clone() without a warning
      cmdlist.add("--add-opens");
      cmdlist.add("java.base/java.lang=ALL-UNNAMED");
      if (!no_jdk) {
        // If we are processing JDK classes, then we need our code on the bootclasspath as well.
        // Otherwise, references to DCRuntime from the JDK would fail.
        cmdlist.add("-Xbootclasspath/a:" + rt_file + path_separator + daikonPath);
        // allow java.base to access daikon.jar (for instrumentation runtime)
        cmdlist.add("--add-reads");
        cmdlist.add("java.base=ALL-UNNAMED");
        // allow DCRuntime to make reflective access to sun.util.locale (equals_dcomp_instrumented)
        cmdlist.add("--add-exports");
        cmdlist.add("java.base/sun.util.locale=ALL-UNNAMED");
        // replace default java.base with our instrumented version
        cmdlist.add("--patch-module");
        cmdlist.add("java.base=" + rt_file);
      }
    }

    cmdlist.add(String.format("-javaagent:%s=%s", premain, premain_args));

    for (String target_arg : target_args) {
      cmdlist.add(target_arg);
    }
    if (verbose) {
      System.out.printf("%nExecuting target program: %s%n", args_to_string(cmdlist));
    }
    String[] cmdline = cmdlist.toArray(new String[cmdlist.size()]);

    // Execute the command, sending all output to our streams
    java.lang.Runtime rt = java.lang.Runtime.getRuntime();
    Process dcomp_proc;
    try {
      dcomp_proc = rt.exec(cmdline);
    } catch (Exception e) {
      System.out.printf("Exception '%s' while executing '%s'%n", e, cmdline);
      System.exit(1);
      throw new Error("Unreachable control flow");
    }

    int targetResult = redirect_wait(dcomp_proc);
    if (targetResult != 0) {
      System.out.printf("Warning: Target exited with %d status.%n", targetResult);
    }
    System.exit(targetResult);
  }

  /** Wait for stream redirect threads to complete. */
  public int redirect_wait(Process p) {

    // Create the redirect threads and start them.
    StreamRedirectThread in_thread =
        new StreamRedirectThread("stdin", System.in, p.getOutputStream(), false);
    StreamRedirectThread err_thread =
        new StreamRedirectThread("stderr", p.getErrorStream(), System.err, true);
    StreamRedirectThread out_thread =
        new StreamRedirectThread("stdout", p.getInputStream(), System.out, true);

    in_thread.start();
    err_thread.start();
    out_thread.start();

    // Wait for the process to terminate and return the results
    int result = -1;
    while (true) {
      try {
        result = p.waitFor();
        break;
      } catch (InterruptedException e) {
        System.out.printf("unexpected interrupt %s while waiting for target to finish", e);
      }
    }

    // Make sure all output is forwarded before we finish
    try {
      err_thread.join();
      out_thread.join();
    } catch (InterruptedException e) {
      System.out.printf("unexpected interrupt %s while waiting for threads to join", e);
    }

    return result;
  }

  /**
   * Returns elapsed time since the start of the program.
   *
   * @return elapsed time since the start of the program
   */
  public static String elapsed() {
    return "[" + (System.currentTimeMillis() - start) + " msec]";
  }

  /**
   * Returns number of milliseconds since the start of the program.
   *
   * @return number of milliseconds since the start of the program
   */
  public static long elapsed_msecs() {
    return System.currentTimeMillis() - start;
  }

  /**
   * Convert a list of arguments into a command-line string. Only used for debugging output.
   *
   * @param args the list of arguments
   * @return argument string
   */
  public String args_to_string(List<String> args) {
    String str = "";
    for (String arg : args) {
      if (arg.indexOf(" ") != -1) {
        arg = "'" + arg + "'";
      }
      str += arg + " ";
    }
    return str.trim();
  }

  /**
   * Search the current classpath for fileName. Return the path if found. If not found, try
   * ${DAIKONDIR}/java, then ${DAIKONDIR}. Return the path if found. Return null if not found.
   *
   * @param fileName name of file to look for
   * @return path to fileName or null
   */
  public @Nullable File locateFile(String fileName) {
    @SuppressWarnings("nullness") // start_target ensures cp and path_separator are non-null
    String[] cpath = cp.split(path_separator);
    File poss_file;
    String java_lib = "java" + file_separator + "lib" + file_separator;
    java_lib_path = "";
    int java_lib_count = 0;
    for (String path : cpath) {
      int index = path.indexOf(java_lib);
      if (index != -1) {
        // If the path contains "java/lib/" it is from the expansion of
        // "java/lib/*". However, this short hand is not allowed on the
        // bootclasspath so we must save the entire list.
        java_lib_count++;
        java_lib_path = java_lib_path + path_separator + path;
      }
      if (path.endsWith(fileName)) {
        poss_file = new File(path);
      } else {
        poss_file = new File(path, fileName);
      }
      if (poss_file.canRead()) {
        // We need to do a special case:
        // If "java/lib/*" appears on the classpath
        // before daikon.jar, use that instead.
        if (java_lib_count > 10 && fileName.equals("daikon.jar")) {
          // Return empty file name as flag to use java_lib_path instead.
          return new File("");
        }
        return poss_file;
      }
    }

    // Not on the class path - do special case:
    // If looking for daikon.jar and java/lib/* is on
    // classpath, then use that. 10 is arbitary value,
    // currently there are 14 jar files in java/lib.
    if (java_lib_count > 10 && fileName.equals("daikon.jar")) {
      // Return empty file name as flag to use java_lib_path instead.
      return new File("");
    }

    // If not on the classpath look in ${DAIKONDIR}/java, then ${DAIKONDIR}.
    if (daikon_dir != null) {
      poss_file = new File(new File(daikon_dir, "java"), fileName);
      if (poss_file.canRead()) {
        return poss_file;
      }
      poss_file = new File(daikon_dir, fileName);
      if (poss_file.canRead()) {
        return poss_file;
      }
    }
    // Couldn't find fileName
    return null;
  }
}
