package daikon;

import daikon.chicory.StreamRedirectThread;
import daikon.plumelib.bcelutil.BcelUtil;
import daikon.plumelib.bcelutil.SimpleLog;
import daikon.plumelib.options.Option;
import daikon.plumelib.options.Options;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.regex.Pattern;
import org.checkerframework.checker.nullness.qual.EnsuresNonNull;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.nullness.qual.RequiresNonNull;

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

  /**
   * Dump the instrumented classes to disk, for diagnostic purposes. The directory is specified by
   * {@code --debug-dir} (default {@code debug}).
   */
  @Option("Dump the instrumented classes to disk")
  public static boolean dump = false;

  /** Output debugging information. */
  @Option("-d Output debugging information (implies --dump)")
  public static boolean debug = false;

  /** The directory in which to dump instrumented class files. */
  @Option("Directory in which to create debug files")
  public static File debug_dir = new File("debug");

  /** The directory in which to create output files (i.e., Daikon input files). */
  @Option("Directory in which to create output files")
  public static File output_dir = new File(".");

  /** Output filename for .decls file suitable for input to Daikon. */
  @Option("-f Output filename for Daikon decl file")
  public static @MonotonicNonNull String decl_file = null;

  /** Output filename for a more easily human-readable file summarizing comparability sets. */
  @Option("Output file for comparability sets")
  // If null, do no output
  public static @MonotonicNonNull File comparability_file = null;

  /** If specified, write a human-readable file showing some of the interactions that occurred. */
  @Option("Trace output file")
  // Null if shouldn't do output
  public static @MonotonicNonNull File trace_file = null;

  /**
   * Controls size of the stack displayed in tracing the interactions that occurred. Used in {@code
   * daikon.dcomp.TagEntry}.
   */
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
  @Option("Use standard visibility")
  public static boolean std_visibility = false;

  /** Depth to which to examine structure components. */
  @Option("Variable nesting depth")
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

  /** Holds the path to "daikon.jar" or to "daikon/java:daikon/java/lib/*". */
  // Set by start_target()
  public static String daikonPath = "";

  /** The current class path. */
  static @MonotonicNonNull String cp = null;

  /** Contains the expansion of java/lib/* if it is on the classpath. */
  static @Nullable String java_lib_classpath = null;

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
   * Entry point of DynComp.
   *
   * @param args see usage for argument descriptions
   */
  public static void main(String[] args) {

    // Parse our arguments
    Options options = new Options(synopsis, DynComp.class);
    options.setParseAfterArg(false);
    String[] targetArgs = options.parse(true, args);
    check_args(options, targetArgs);

    // Turn on basic logging if debug was selected
    basic.enabled = debug;
    basic.log("targetArgs = %s%n", Arrays.toString(targetArgs));

    // Start the target.  Pass the same options to the premain as
    // were passed here.

    DynComp dcomp = new DynComp();
    dcomp.start_target(options.getOptionsString(), targetArgs);
  }

  /**
   * Check the command-line arguments for legality. Prints a message and exits if there was an
   * error.
   *
   * @param options set of legal options to DynComp
   * @param targetArgs arguments being passed to the target program
   */
  public static void check_args(Options options, String[] targetArgs) {
    if (help) {
      options.printUsage();
      System.exit(1);
    }
    if (nesting_depth < 0) {
      System.out.printf("nesting depth (%d) must not be negative%n", nesting_depth);
      options.printUsage();
      System.exit(1);
    }
    if (targetArgs.length == 0) {
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
   * Starts the target program with the Java agent set up to do the transforms. All Java agent
   * arguments are passed to it. The current classpath is passed to the new JVM.
   *
   * @param premain_args the Java agent argument list
   * @param targetArgs the test program name and its argument list
   */
  /*TO DO: @EnsuresNonNull("premain")*/
  @EnsuresNonNull("cp")
  void start_target(String premain_args, String[] targetArgs) {

    // Default the decls file name to <target-program-name>.decls-DynComp
    if (decl_file == null) {
      String target_class = targetArgs[0].replaceFirst(".*[/.]", "");
      decl_file = String.format("%s.decls-DynComp", target_class);
      premain_args += " --decl-file=" + decl_file;
    }

    // Get the current classpath
    cp = System.getProperty("java.class.path");
    basic.log("classpath = '%s'%n", cp);
    if (cp == null) {
      cp = ".";
    }

    // Get location of DAIKONDIR, may be null.
    daikon_dir = System.getenv("DAIKONDIR");

    // The separator for items in the class path.
    basic.log("File.pathSeparator = %s%n", File.pathSeparator);

    // Look for location of dcomp_premain.jar
    if (premain == null) {
      premain = locateFile("dcomp_premain.jar");
    }
    // If we didn't find a premain it's a fatal error.
    if (premain == null) {
      System.err.printf("Can't find dcomp_premain.jar on the classpath");
      if (daikon_dir == null) {
        System.err.printf(" and $DAIKONDIR is not set.%n");
      } else {
        System.err.printf(" or in %s/java .%n", daikon_dir);
      }
      System.err.printf("It should be found in the directory where Daikon was installed.%n");
      System.err.printf("Use the --premain switch to specify its location,%n");
      System.err.printf("or change your classpath to include it.%n");
      System.exit(1);
    }

    // Are we using the instrumented JDK?
    if (!no_jdk) {
      // Yes we are - We need to locate dcomp_rt.jar and add Daikon to the boot classpath.
      // Look for location of dcomp_rt.jar
      if (rt_file == null) {
        rt_file = locateFile("dcomp_rt.jar");
      }
      // If we didn't find a rt-file it's a fatal error.
      if (rt_file == null) {
        System.err.printf("Can't find dcomp_rt.jar on the classpath");
        if (daikon_dir == null) {
          System.err.printf(" and $DAIKONDIR is not set.%n");
        } else {
          System.err.printf(" or in %s/java .%n", daikon_dir);
        }
        System.err.printf("Probably you forgot to build it.%n");
        System.err.printf(
            "See the Daikon manual, section \"Instrumenting the JDK with DynComp\" for help.%n");
        System.exit(1);
      }
      // Add the location of Daikon to the boot classpath.  For each element of the classpath, if it
      // is part of Daikon, append it to the boot classpath.
      for (String path : cp.split(File.pathSeparator)) {
        if (isDaikonOnPath(path)) {
          daikonPath = daikonPath + File.pathSeparator + path;
        }
      }
      basic.log("daikonPath = '%s'%n", daikonPath);
    }

    // Build the command line to execute the target with the javaagent.
    List<String> cmdlist = new ArrayList<>();
    cmdlist.add("java");
    // cmdlist.add ("-verbose:class");
    cmdlist.add("-cp");
    cmdlist.add(cp);
    cmdlist.add("-ea");
    cmdlist.add("-esa");
    // Get max memory given DynComp and pass on to dcomp_premain rounded up to nearest gigabyte.
    cmdlist.add(
        "-Xmx" + (int) Math.ceil(java.lang.Runtime.getRuntime().maxMemory() / 1073741824.0) + "G");

    if (BcelUtil.javaVersion <= 8) {
      if (!no_jdk) {
        // prepend to rather than replace boot classpath
        // If daikonPath is nonempty, it starts with a pathSeparator.
        cmdlist.add("-Xbootclasspath/p:" + rt_file + daikonPath);
      }
    } else {
      // allow DCRuntime to make reflective access to java.land.Object.clone() without a warning
      cmdlist.add("--add-opens");
      cmdlist.add("java.base/java.lang=ALL-UNNAMED");
      if (!no_jdk) {
        // If we are processing JDK classes, then we need our code on the boot classpath as well.
        // Otherwise, references to DCRuntime from the JDK would fail.
        // If daikonPath is nonempty, it starts with a pathSeparator.
        cmdlist.add("-Xbootclasspath/a:" + rt_file + daikonPath);
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

    // A `for` loop is needed because targetArgs is an array and cmdlist is a list.
    for (String target_arg : targetArgs) {
      cmdlist.add(target_arg);
    }
    if (verbose) {
      System.out.printf("%nExecuting target program: %s%n", argsToString(cmdlist));
    }
    String[] cmdline = cmdlist.toArray(new String[0]);

    // Execute the command, sending all output to our streams.
    java.lang.Runtime rt = java.lang.Runtime.getRuntime();
    Process dcomp_proc;
    try {
      dcomp_proc = rt.exec(cmdline);
    } catch (Exception e) {
      System.out.printf("Exception '%s' while executing: %s%n", e, cmdline);
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
    return "[" + elapsedMsecs() + " msec]";
  }

  /**
   * Returns number of milliseconds since the start of the program.
   *
   * @return number of milliseconds since the start of the program
   */
  public static long elapsedMsecs() {
    return System.currentTimeMillis() - start;
  }

  /**
   * Convert a list of arguments into a command-line string. Only used for debugging output.
   *
   * @param args the list of arguments
   * @return argument string
   */
  public String argsToString(List<String> args) {
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
   * Returns true if Daikon or a Daikon library jar file is on the path argument. There are three
   * cases:
   *
   * <ul>
   *   <li>a jar file that contains "DynComp.class"
   *   <li>a path that ends in "java/lib/<em>something</em>.jar"
   *   <li>a path that leads to "daikon/DynComp.class"
   * </ul>
   *
   * @param path classpath element to inspect for Daikon
   * @return true if found
   */
  boolean isDaikonOnPath(String path) {
    if (path.endsWith(".jar")) {
      // path ends in ".jar".
      try (JarFile jar = new JarFile(path)) {
        JarEntry entry = jar.getJarEntry("daikon/DynComp.class");
        if (entry != null) {
          return true;
        }
      } catch (Exception e) {
        // do nothing, try next case
      }
      // check to see if path is .../java/lib/...
      String pathElements[] = path.split(Pattern.quote(File.separator));
      int pathLength = pathElements.length;
      if (pathLength == 0) {
        // Can never happen? Fatal error if it does.
        System.err.printf("classpath appears to be empty.%n");
        System.exit(1);
      }
      if (pathLength > 2) {
        if (pathElements[pathLength - 2].equals("lib")
            && pathElements[pathLength - 3].equals("java")) {
          File javaLibJarFile = new File(path);
          if (javaLibJarFile.canRead()) {
            return true;
          }
        }
      }
    } else {
      // path does not end in ".jar"
      File dyncompClassFile =
          new File(path + File.separator + "daikon" + File.separator + "DynComp.class");
      if (dyncompClassFile.canRead()) {
        return true;
      }
    }
    return false;
  }

  /**
   * Search for a file on the current classpath, then in ${DAIKONDIR}/java. Returns null if not
   * found.
   *
   * @param fileName the relative name of a file to look for
   * @return path to fileName or null
   */
  @RequiresNonNull("cp")
  public @Nullable File locateFile(String fileName) {
    File poss_file = findOnClasspath(fileName);
    if (poss_file != null) {
      return poss_file;
    }

    // If not on the classpath look in ${DAIKONDIR}/java.
    if (daikon_dir != null) {
      poss_file = new File(new File(daikon_dir, "java"), fileName);
      if (poss_file.canRead()) {
        return poss_file;
      }
    }
    // Couldn't find fileName
    return null;
  }

  /**
   * Search for a file on the current classpath. Returns null if not found.
   *
   * @param fileName the relative name of a file to look for
   * @return path to fileName or null
   */
  @RequiresNonNull("cp")
  public @Nullable File findOnClasspath(String fileName) {
    for (String path : cp.split(File.pathSeparator)) {
      File poss_file;
      if (path.endsWith(fileName)) {
        int start = path.indexOf(fileName);
        // There are three cases:
        //   path == fileName (start == 0)
        //   path == <something>/fileName (charAt(start-1) == separator)
        //   path == <something>/<something>fileName (otherwise)
        // The first two are good, the last is not what we are looking for.
        if (start == 0 || path.charAt(start - 1) == File.separatorChar) {
          poss_file = new File(path);
        } else {
          poss_file = new File(path, fileName);
        }
      } else {
        poss_file = new File(path, fileName);
      }
      if (poss_file.canRead()) {
        return poss_file;
      }
    }
    return null;
  }
}
