package daikon;

import static java.nio.charset.StandardCharsets.UTF_8;

import daikon.chicory.StreamRedirectThread;
import daikon.plumelib.bcelutil.SimpleLog;
import daikon.plumelib.options.Option;
import daikon.plumelib.options.Options;
import daikon.plumelib.util.RegexUtil;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;
import org.checkerframework.checker.nullness.qual.EnsuresNonNull;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.nullness.qual.RequiresNonNull;
import org.checkerframework.dataflow.qual.Pure;

/**
 * This is the main class for Chicory which transforms the class files of a program to instrument it
 * for Daikon. The instrumentation uses the javaagent switch to java (which allows classes to be
 * instrumented as they are loaded). This class parses the command line arguments, starts java with
 * the javaagent switch on the target program and if requested starts Daikon on the result.
 */
public class Chicory {

  /** Display usage information. */
  @Option("-h Display usage information")
  public static boolean help = false;

  /** Print information about the classes being transformed. */
  @Option("-v Print progress information")
  public static boolean verbose = false;

  /** Print debug information and save instrumented classes. */
  @Option("-d Print debug information and save instrumented classes")
  public static boolean debug = false;

  /** File in which to put dtrace output. */
  @Option("File in which to put dtrace output")
  public static @MonotonicNonNull File dtrace_file = null;

  /** Decl formatted file containing comparability information. */
  @Option("Decl formatted file containing comparability information")
  public static @Nullable File comparability_file = null;

  /** Directory in which to create output files. */
  @Option("Directory in which to create output files")
  public static File output_dir = new File(".");

  /** Directory in which to find configuration files. */
  @Option("Directory in which to find configuration files")
  public static @Nullable File config_dir = null;

  /** Run Daikon in a separate process after Chicory. */
  @Option("Run Daikon on the generated data trace file")
  public static boolean daikon = false;

  /** Send trace information to Daikon over a socket. */
  @Option("Send trace information to Daikon over a socket")
  public static boolean daikon_online = false;

  // TODO: splitting on whitespace is error-prone.
  /**
   * Specifies Daikon arguments to be used if Daikon is run on a generated trace file {@code
   * --daikon} or online via a socket {@code --daikon-online}. These arguments will be split on
   * whitespace.
   */
  @Option("Specify Daikon arguments for either --daikon or --daikon-online")
  public static String daikon_args = "";

  // Should perhaps permit specifying the heap for the target program and
  // for Daikon separately.
  /** Heap size for the target program, and for Daikon if Daikon is run. */
  @Option("Size of the heap for the target program, and for Daikon if it is run")
  public static String heap_size = "3600m";

  /**
   * Path to Java agent jar file that performs the transformation. The "main" procedure is {@link
   * daikon.chicory.ChicoryPremain#premain}.
   */
  @Option("Path to the Chicory agent jar file")
  public static @MonotonicNonNull File premain = null;

  /** Only emit program points that match the given regex. */
  @Option("Include only program points that match")
  public static List<Pattern> ppt_select_pattern = new ArrayList<>();

  /** Suppress program points that match the given regex. */
  @Option("Omit all program points that match")
  public static List<Pattern> ppt_omit_pattern = new ArrayList<>();

  /**
   * When this option is chosen, Chicory will record each program point until that program point has
   * been executed sample-cnt times. Chicory will then begin sampling. Sampling starts at 10% and
   * decreases by a factor of 10 each time another sample-cnt samples have been recorded. If
   * sample-cnt is 0, then all calls will be recorded.
   */
  @Option("Number of calls after which sampling will begin")
  public static int sample_start = 0;

  /** Treat classes that match the regex as boot classes (do not instrument). */
  @Option("Treat classes that match the regex as boot classes (do not instrument)")
  public static @Nullable Pattern boot_classes = null;

  /**
   * If true, no variable values are printed. Static variables are not initialized yet when the
   * routine is entered, and static variable are not necessarily initialized to their final values
   * when the routine is exited. These .dtrace entries are purely for the benefit of tools that use
   * Chicory for program tracing, to determine when methods are entered and exited.
   */
  @Option("Write static initializer program points")
  public static boolean instrument_clinit = false;

  /** Depth to examine structure components. */
  @Option("Depth to examine structure components")
  public static int nesting_depth = 2;

  /** Also see Daikon's {@code --var-omit-pattern} command-line argument. */
  @Option("Omit variables that match this regular expression.")
  public static @Nullable Pattern omit_var = null;

  /**
   * If false, every field in an instrumented class is visible. If true, use standard Java behavior
   * (if the field is in a class in a different package, it is only visible if public, etc.).
   */
  @Option("Only include variables that are visible under normal Java access rules")
  public static boolean std_visibility = false;

  /**
   * The name of the file to read for a list of pure methods. Should be 1 method per line. Each
   * method should be in the same format as output by the purity analysis.
   */
  @Option("File of pure methods to use as additional Daikon variables")
  public static @Nullable File purity_file;

  // The next three command-line options are internal debugging
  // options that are primarily for the use of the Daikon developers.

  /** Print detailed information on which classes are transformed. */
  @Option("Print detailed information on which classes are transformed")
  public static boolean debug_transform = false;

  /** Print detailed information on variables being observed. */
  @Option("Print detailed information on variables being observed")
  public static boolean debug_decl_print = false;

  /** Print information about each ppt name as it is created. */
  @Option("Print information about each ppt name as it is created")
  public static boolean debug_ppt_names = false;

  /** Daikon port number. Daikon writes this to stdout when it is started in online mode. */
  private static int daikon_port = -1;

  /** Thread that copies output from target to our output. */
  public static @MonotonicNonNull StreamRedirectThread out_thread;

  /** Thread that copies stderr from target to our stderr. */
  public static @MonotonicNonNull StreamRedirectThread err_thread;

  /** starting time (msecs) */
  public static long start = System.currentTimeMillis();

  /** daikon process for {@code --daikon} command-line option. */
  // non-null if either daikon==true or daikon_online==true
  public static @MonotonicNonNull Process daikon_proc;

  private static final String traceLimTermString = "DTRACELIMITTERMINATE";
  private static final String traceLimString = "DTRACELIMIT";

  /** Flag to use if we want to turn on the static initialization checks. */
  public static final boolean checkStaticInit = true;

  private static final boolean RemoteDebug = false;

  /** Flag to initiate a purity analysis and use results to create add vars. */
  private static boolean purityAnalysis = false;

  /** Log file if debug is enabled. */
  private static final SimpleLog basic = new SimpleLog(false);

  /** Synopsis for the Chicory command line. */
  public static final String synopsis = "daikon.Chicory [options] target [target-args]";

  /**
   * Entry point of Chicory.
   *
   * @param args see usage for argument descriptions
   */
  public static void main(String[] args) {

    // Parse our arguments
    Options options = new Options(synopsis, Chicory.class);
    options.setParseAfterArg(false);
    String[] target_args = options.parse(true, args);
    check_args(options, target_args);

    // Turn on basic logging if debug was selected
    basic.enabled = debug;
    basic.log("target_args = %s%n", Arrays.toString(target_args));

    // Start the target.  Pass the same options to the premain as
    // were passed here.

    Chicory chicory = new Chicory();
    chicory.start_target(options.getOptionsString(), target_args);
  }

  /**
   * Check the command-line arguments for legality. Prints a message and exits if there was an
   * error.
   *
   * @param options set of legal options to Chicory
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
    if (daikon && daikon_online) {
      System.out.printf("may not specify both daikon and daikon-onlne%n");
      options.printUsage();
      System.exit(1);
    }
    if (!daikon_args.trim().isEmpty() && !(daikon || daikon_online)) {
      System.out.printf("may not specify daikon-args without either daikon or daikon-onlne%n");
      options.printUsage();
      System.exit(1);
    }
  }

  /**
   * Return true iff argument was given to run a purity analysis.
   *
   * <p>You should only call this after parsing arguments.
   */
  public static boolean doPurity() {
    return purityAnalysis;
  }

  /** Return true iff a file name was specified to supply pure method names. */
  @Pure
  public static @Nullable File get_purity_file() {
    return purity_file;
  }

  /**
   * Starts the target program with the Java agent setup to do the transforms. All Java agent
   * arguments are passed to it. Our classpath is passed to the new JVM.
   *
   * @param premain_args the Java agent argument list
   * @param target_args the test program name and its argument list
   */
  void start_target(String premain_args, String[] target_args) {

    // Default the trace file name to <target-program-name>.dtrace.gz
    if (dtrace_file == null) {
      String target_class = target_args[0].replaceFirst(".*[/.]", "");
      dtrace_file = new File(String.format("%s.dtrace.gz", target_class));
      premain_args += " --dtrace-file=" + dtrace_file;
    }

    // Get the current classpath
    String cp = System.getProperty("java.class.path");
    basic.log("classpath = '%s'%n", cp);
    if (cp == null) {
      cp = ".";
    }

    // The separator for items in the class path
    basic.log("File.pathSeparator = %s%n", File.pathSeparator);
    if (!RegexUtil.isRegex(File.pathSeparator)) {
      // This can't happen, at least on Unix & Windows.
      throw new Daikon.UserError(
          "Bad regexp "
              + File.pathSeparator
              + " for path.separator: "
              + RegexUtil.regexError(File.pathSeparator));
    }

    // Look for ChicoryPremain.jar along the classpath
    if (premain == null) {
      String[] cpath = cp.split(File.pathSeparator);
      for (String path : cpath) {
        File poss_premain = new File(path, "ChicoryPremain.jar");
        if (poss_premain.canRead()) {
          premain = poss_premain;
          break;
        }
      }
    }

    // If not on the classpath look in ${DAIKONDIR}/java
    String daikon_dir = System.getenv("DAIKONDIR");
    if (premain == null) {
      if (daikon_dir != null) {
        File poss_premain = new File(new File(daikon_dir, "java"), "ChicoryPremain.jar");
        if (poss_premain.canRead()) {
          premain = poss_premain;
        }
      }
    }

    // If not found, try the daikon.jar file itself
    if (premain == null) {
      for (String path : cp.split(File.pathSeparator)) {
        File poss_premain = new File(path);
        if (poss_premain.getName().equals("daikon.jar")) {
          if (poss_premain.canRead()) {
            premain = poss_premain;
          }
        }
      }
    }

    // If we didn't find a premain, give up
    if (premain == null) {
      System.err.printf("Can't find ChicoryPremain.jar or daikon.jar on the classpath");
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

    String dtraceLim, terminate;
    dtraceLim = System.getProperty(traceLimString);
    terminate = System.getProperty(traceLimTermString);

    // Run Daikon if we're in online mode
    StreamRedirectThread daikon_err = null;
    StreamRedirectThread daikon_out = null;
    if (daikon_online) {
      runDaikon();

      StreamRedirectThread tmp_daikon_err =
          new StreamRedirectThread("stderr", daikon_proc.getErrorStream(), System.err);
      daikon_err = tmp_daikon_err;
      daikon_err.start();

      @NonNull InputStream daikonStdOut = daikon_proc.getInputStream();
      // daikonReader escapes, so it is not closed in this method.
      BufferedReader daikonReader = new BufferedReader(new InputStreamReader(daikonStdOut, UTF_8));

      // Examine up to 100 lines of Daikon output, looking for
      // the "DaikonChicoryOnlinePort=" line.  Note that if file progress
      // is turned on in Daikon, it may be preceded by a timestamp.
      for (int i = 0; i < 100; i++) {
        String line;
        try {
          line = daikonReader.readLine();
        } catch (IOException e1) {
          System.out.printf("Exception reading output from Daikon: %s%n", e1);
          line = null;
        }

        if (line == null) {
          throw new RuntimeException("Did not receive socket port from Daikon!");
        } else {
          System.out.println(line);

          if (line.contains("DaikonChicoryOnlinePort=")) {
            String portStr = line.replaceFirst(".*DaikonChicoryOnlinePort=", "");
            daikon_port = Integer.decode(portStr);
            System.out.println("GOT PORT STRING " + daikon_port);
            break;
          }
        }
      }

      if (daikon_port == -1) {
        throw new RuntimeException("After 100 lines of output, Daikon port not received");
      }

      // continue reading daikon output in separate thread
      daikon_out = new StreamRedirectThread("stdout", daikonStdOut, System.out);
      daikon_out.start();
    }

    // Build the command line to execute the target with the javaagent
    List<String> cmdlist = new ArrayList<>();
    cmdlist.add("java");

    if (RemoteDebug) {
      cmdlist.add("-Xdebug");

      cmdlist.add("-Xrunjdwp:server=n,transport=dt_socket,address=8000,suspend=y");
      // cmdlist.add("-Xrunjdwp:server=y,transport=dt_socket,address=4142,suspend=n");

      // cmdlist.add("-Xnoagent");
      // cmdlist.add("-Xrunjdwp:server=n,transport=dt_socket,address=8000,suspend=n");
      // cmdlist.add("-Djava.compiler=NONE");
    }

    cmdlist.add("-cp");
    cmdlist.add(cp);
    cmdlist.add("-ea");
    cmdlist.add("-esa");
    cmdlist.add("-Xmx" + heap_size);
    // cmdlist.add ("-verbose");

    if (dtraceLim != null) {
      cmdlist.add("-D" + traceLimString + "=" + dtraceLim);
    }
    if (terminate != null) {
      cmdlist.add("-D" + traceLimTermString + "=" + terminate);
    }

    // Specify the port to use to talk to Daikon if in online mode
    if (daikon_online) {
      assert daikon_port != -1 : daikon_port;
      premain_args += " --daikon-port " + daikon_port;
    }

    cmdlist.add(String.format("-javaagent:%s=%s", premain, premain_args));

    for (String target_arg : target_args) {
      cmdlist.add(target_arg);
    }
    if (verbose) {
      System.out.printf("%nExecuting target program: %s%n", args_to_string(cmdlist));
    }
    String[] cmdline = cmdlist.toArray(new String[0]);

    // Execute the command, sending all output to our streams
    java.lang.Runtime rt = java.lang.Runtime.getRuntime();
    Process chicory_proc;
    try {
      chicory_proc = rt.exec(cmdline);
    } catch (Exception e) {
      System.out.printf("Exception '%s' while executing '%s'%n", e, cmdline);
      System.exit(1);
      throw new Error("Unreachable control flow");
    }

    int targetResult = redirect_wait(chicory_proc);

    if (daikon) {
      // Terminate if target didn't end properly
      if (targetResult != 0) {
        System.out.printf(
            "Warning: Did not run Daikon because target exited with %d status%n", targetResult);
        System.exit(targetResult);
      }

      runDaikon();
      int daikonResult = waitForDaikon();
      System.exit(daikonResult);
    } else if (daikon_online) {
      assert daikon_proc != null
          : "@AssumeAssertion(nullness): conditional: just tested daikon_online, and ran"
              + " runDaikon() earlier in this method";
      if (targetResult != 0) {
        System.out.printf("Warning: Target exited with %d status%n", targetResult);
      }

      // Wait for the process to terminate and return the results
      int daikonResult = 0; // initialized to nonsense value to suppress compiler warning
      while (true) {
        try {
          daikonResult = daikon_proc.waitFor();
          break;
        } catch (InterruptedException e) {
          System.out.printf("unexpected interrupt %s while waiting for target to finish", e);
        }
      }

      // Make sure all output is forwarded before we finish
      try {
        assert daikon_err != null
            : "@AssumeAssertion(nullness): dependent: because daikon_online is true";
        assert daikon_out != null
            : "@AssumeAssertion(nullness): dependent: because daikon_online is true";
        daikon_err.join();
        daikon_out.join();
      } catch (InterruptedException e) {
        System.out.printf("unexpected interrupt %s while waiting for threads to join", e);
      }

      if (daikonResult != 0) {
        System.out.printf("Warning: Daikon exited with %d status%n", daikonResult);
      }
      System.exit(daikonResult);
    } else {
      // No daikon command specified, so just exit
      if (targetResult != 0) {
        System.out.printf("Warning: Target exited with %d status%n", targetResult);
      }
      System.exit(targetResult);
    }
  }

  /** Runs daikon either online or on the generated trace file. */
  @EnsuresNonNull("daikon_proc")
  public void runDaikon() {

    java.lang.Runtime rt = java.lang.Runtime.getRuntime();

    // Get the current classpath
    String cp = System.getProperty("java.class.path");
    if (cp == null) {
      cp = ".";
    }

    List<String> cmd = new ArrayList<>();
    cmd.add("java");
    cmd.add("-Xmx" + heap_size);
    cmd.add("-cp");
    cmd.add(cp);
    cmd.add("-ea");
    cmd.add("daikon.Daikon");
    if (!daikon_args.trim().isEmpty()) {
      for (String arg : daikon_args.split(" +")) {
        cmd.add(arg);
      }
    }
    if (daikon_online) {
      cmd.add("+");
    } else {
      cmd.add(output_dir + File.separator + dtrace_file);
    }

    // System.out.println("daikon command cmd " + cmd);

    if (verbose) {
      System.out.printf("%nExecuting daikon: %s%n", cmd);
    }

    try {
      daikon_proc = rt.exec(cmd.toArray(new String[0]));
    } catch (Exception e) {
      System.out.printf("Exception '%s' while executing '%s'%n", e, cmd);
      System.exit(1);
    }
  }

  /**
   * Wait for daikon to complete and return its exit status.
   *
   * @return Daikon's exit status
   */
  @RequiresNonNull("daikon_proc")
  private int waitForDaikon() {
    return redirect_wait(daikon_proc);
  }

  /**
   * Wait for stream redirect threads to complete and return their exit status.
   *
   * @param p the process to wait for completion
   * @return process result
   */
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
   * Returns string representation of elapsed time since the start of the program.
   *
   * @return string representation of elapsed time since the start of the program
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

  /** Convert a list of arguments into a command-line string. Only used for debugging output. */
  public String args_to_string(List<String> args) {
    String str = "";
    for (String arg : args) {
      if (arg.indexOf(" ") != -1) {
        str = "'" + str + "'";
      }
      str += arg + " ";
    }
    return str.trim();
  }
}
