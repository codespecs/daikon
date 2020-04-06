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
import java.io.PrintWriter;
import java.lang.reflect.Method;
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

  @Option("File in which to put dtrace output")
  public static @MonotonicNonNull File dtrace_file = null;

  /** Also see Daikon's {@code --var-omit-pattern} command-line argument. */
  @Option("Omit variables that match this regular expression.")
  public static @Nullable Pattern omit_var = null;

  @Option("Directory in which to create output files")
  public static File output_dir = new File(".");

  @Option("Depth to examine structure components")
  public static int nesting_depth = 2;

  @Option("Omit all program points that match")
  public static List<Pattern> ppt_omit_pattern = new ArrayList<>();

  @Option("Include only program points that match")
  public static List<Pattern> ppt_select_pattern = new ArrayList<>();

  @Option("Decl formatted file containing comparability information")
  public static @Nullable File comparability_file = null;

  /**
   * If true, no variable values are printed. Static variables are not initialized yet when the
   * routine is entered, and static variable are not necessarily initialized to their final values
   * when the routine is exited. These .dtrace entries are purely for the benefit of tools that use
   * Chicory for program tracing, to determine when methods are entered and exited.
   */
  @Option("Write static initializer program points")
  public static boolean instrument_clinit = false;

  @Option("Include variables that are visible under normal java access rules")
  public static boolean std_visibility = false;

  @Option("Print progress information")
  public static boolean verbose = false;

  @Option("Print debug information and save instrumented classes")
  public static boolean debug = false;

  @Option("Print detailed information on which classes are transformed")
  public static boolean debug_transform = false;

  @Option("Print detailed information on variables being observed")
  public static boolean debug_decl_print = false;

  @Option("Treat classes that match the regex as boot classes (do not instrument)")
  public static @Nullable Pattern boot_classes = null;

  // Should perhaps permit specifying the heap for the target program and
  // for Daikon separately.
  @Option("Size of the heap for the target program, and for Daikon if it is run")
  public static String heap_size = "1000m";

  @Option("Print information about each ppt name as it is created")
  public static boolean debug_ppt_names = false;

  @Option("Create the new declaration record format")
  public static boolean new_decl_format = true;

  /**
   * Path to java agent jar file that performs the transformation. The "main" procedure is {@link
   * daikon.chicory.ChicoryPremain#premain}.
   */
  @Option("Path to the Chicory agent jar file")
  public static @MonotonicNonNull File premain = null;

  /**
   * The name of the file to read for a list of pure methods. Should be 1 method per line. Each
   * method should be in the same format as format output by the purity analysis.
   */
  @Option("File of pure methods to use as additional Daikon variables")
  public static @Nullable File purity_file;

  @Option("Directory in which to find configuration files")
  public static @Nullable File config_dir = null;

  // Daikon is run in a separate process
  @Option("Run Daikon on the generated data trace file")
  public static boolean daikon = false;

  @Option("Send trace information to Daikon over a socket")
  public static boolean daikon_online = false;

  /**
   * Specifies Daikon arguments to be used if Daikon is run on a generated trace file or online via
   * a socket. If neither {@code --daikon} or {@code --daikon-online} is chosen, this option will
   * select {@code --daikon}.
   */
  @Option("Specify Daikon arguments for either --daikon or --daikon-online")
  public static String daikon_args = "";

  /**
   * When this option is chosen, Chicory will record each program point until that program point has
   * been executed sample-cnt times. Chicory will then begin sampling. Sampling starts at 10% and
   * decreases by a factor of 10 each time another sample-cnt samples have been recorded. If
   * sample-cnt is 0, then all calls will be recorded.
   */
  @Option("Number of calls after which sampling will begin")
  public static int sample_start = 0;

  /** Daikon port number. Daikon writes this to stdout when it is started in online mode. */
  private static int daikon_port = -1;

  /** Thread that copies output from target to our output. */
  public static @MonotonicNonNull StreamRedirectThread out_thread;

  /** Thread that copies stderr from target to our stderr. */
  public static @MonotonicNonNull StreamRedirectThread err_thread;

  /** starting time (msecs) */
  public static long start = System.currentTimeMillis();

  /** daikon process for {@code --daikon} command-line option */
  // non-null if either daikon==true or daikon_online==true
  public static @MonotonicNonNull Process daikon_proc;

  private static final String traceLimTermString = "DTRACELIMITTERMINATE";
  private static final String traceLimString = "DTRACELIMIT";

  /** flag to use if we want to turn on the static initialization checks */
  public static final boolean checkStaticInit = true;

  private static final boolean RemoteDebug = false;

  /** Flag to initiate a purity analysis and use results to create add vars. */
  private static boolean purityAnalysis = false;

  private static final SimpleLog basic = new SimpleLog(false);

  /** Synopsis for the chicory command line. */
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

    // Turn on basic logging if the debug was selected
    basic.enabled = debug;
    basic.log("target_args = %s%n", Arrays.toString(target_args));

    // Start the target.  Pass the same options to the premain as
    // were passed here.

    Chicory chicory = new Chicory();
    chicory.start_target(getOptionsString(options), target_args);
  }

  // Gross hack, undo when Options package makes the `getOptionsString` method public.
  @SuppressWarnings("nullness")
  private static String getOptionsString(Options options) {
    try {
      Method method = options.getClass().getDeclaredMethod("getOptionsString");
      method.setAccessible(true);
      return (String) method.invoke(options);
    } catch (Throwable e) {
      throw new Error(e);
    }
  }

  /**
   * Check the command-line arguments for legality. If not legal, prints a message and exits the
   * JVM.
   *
   * @param options set of legal options to Chicory
   * @param target_args arguments being passed to the target program
   */
  public static void check_args(Options options, String[] target_args) {
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
   * Starts the target program with the java agent setup to do the transforms. All java agent
   * arguments are passed to it. Our classpath is passed to the new JVM.
   */
  void start_target(String premain_args, String[] target_args) {

    // Default the trace file name to the <target-program-name>.dtrace.gz
    if (dtrace_file == null) {
      String target_class = target_args[0].replaceFirst(".*[/.]", "");
      dtrace_file = new File(String.format("%s.dtrace.gz", target_class));
      premain_args += " --dtrace-file=" + dtrace_file;
    }

    // Get the current classpath
    String cp = System.getProperty("java.class.path");
    basic.log("classpath = '%s'\n", cp);
    if (cp == null) cp = ".";

    // The the separator for items in the class path
    String path_separator = System.getProperty("path.separator");
    basic.log("path_separator = %s\n", path_separator);
    if (!RegexUtil.isRegex(path_separator)) {
      throw new Daikon.UserError(
          "Bad regexp "
              + path_separator
              + " for path.separator: "
              + RegexUtil.regexError(path_separator));
    }

    // Look for ChicoryPremain.jar along the classpath
    if (premain == null) {
      String[] cpath = cp.split(path_separator);
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
        String file_separator = System.getProperty("file.separator");
        File poss_premain = new File(daikon_dir + file_separator + "java", "ChicoryPremain.jar");
        if (poss_premain.canRead()) {
          premain = poss_premain;
        }
      }
    }

    // If not found, try the daikon.jar file itself
    if (premain == null) {
      for (String path : cp.split(path_separator)) {
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
      System.err.printf("Can't find ChicoryPremain.jar on the classpath");
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
      // -Xdebug -Xrunjdwp:server=y,transport=dt_socket,address=4142,suspend=n
      cmdlist.add("-Xdebug -Xrunjdwp:server=n,transport=dt_socket,address=8000,suspend=y");
      // cmdlist.add("-Xdebug -Xnoagent
      // -Xrunjdwp:transport=dt_socket,server=n,suspend=n,address=8000 -Djava.compiler=NONE");
    }

    cmdlist.add("-cp");
    cmdlist.add(cp);
    cmdlist.add("-ea");
    cmdlist.add("-Xmx" + heap_size);
    // cmdlist.add ("-verbose");

    if (dtraceLim != null) cmdlist.add("-D" + traceLimString + "=" + dtraceLim);
    if (terminate != null) cmdlist.add("-D" + traceLimTermString + "=" + terminate);

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
    String[] cmdline = cmdlist.toArray(new String[cmdlist.size()]);

    // Execute the command, sending all output to our streams
    java.lang.Runtime rt = java.lang.Runtime.getRuntime();
    Process chicory_proc = null;
    try {
      chicory_proc = rt.exec(cmdline);
    } catch (Exception e) {
      System.out.printf("Exception '%s' while executing '%s'%n", e, cmdline);
      System.exit(1);
    }

    StreamRedirectThread stdin_thread =
        new StreamRedirectThread("stdin", System.in, chicory_proc.getOutputStream(), false);
    stdin_thread.start();

    int targetResult = redirect_wait(chicory_proc);

    if (daikon) {
      // Terminate if target didn't end properly
      if (targetResult != 0) {
        System.out.printf(
            "Warning: Did not run Daikon because target exited with %d status\n", targetResult);
        System.exit(targetResult);
      }

      runDaikon();
      int daikonResult = waitForDaikon();
      System.exit(daikonResult);
    } else if (daikon_online) {
      assert daikon_proc != null
          : "@AssumeAssertion(nullness): conditional: just tested daikon_online, and ran runDaikon() earlier in this method";
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
    if (cp == null) cp = ".";

    String cmdstr;
    if (daikon_online) {
      cmdstr =
          String.format("java -Xmx%s -cp %s -ea daikon.Daikon %s +", heap_size, cp, daikon_args);
    } else {
      cmdstr =
          String.format(
              "java -Xmx%s -cp %s -ea daikon.Daikon %s %s/%s",
              heap_size, cp, daikon_args, output_dir, dtrace_file);
    }

    // System.out.println("daikon command is " + daikon_cmd);
    // System.out.println("daikon command cmdstr " + cmdstr);

    if (verbose) System.out.printf("%nExecuting daikon: %s%n", cmdstr);

    try {
      daikon_proc = rt.exec(cmdstr);
    } catch (Exception e) {
      System.out.printf("Exception '%s' while executing '%s'%n", e, cmdstr);
      System.exit(1);
    }
  }

  /** Wait for daikon to complete and return its exit status. */
  @RequiresNonNull("daikon_proc")
  private int waitForDaikon() {
    int result = redirect_wait(daikon_proc);
    return result;
  }

  /** Wait for stream redirect threads to complete and return its exit status. */
  public int redirect_wait(Process p) {

    // Create the redirect theads and start them
    StreamRedirectThread err_thread =
        new StreamRedirectThread("stderr", p.getErrorStream(), System.err);

    StreamRedirectThread out_thread =
        new StreamRedirectThread("stdout", p.getInputStream(), System.out);

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

  public PrintWriter openFileInDirectory(String fileName, String dirName) {
    PrintWriter outFile = null;
    try {
      if (dirName != null) {
        File directory = new File(dirName);

        // make the output directory if non-existent
        if (!directory.exists()) directory.mkdir();
      }

      outFile = new PrintWriter(new File(dirName, fileName), UTF_8.name());
    } catch (IOException e) {
      if (outFile == null) {
        throw new RuntimeException("This can't happen.");
      }
      outFile.close();

      throw new Error("File creation of file " + fileName + " failed", e);
    }
    return outFile;
  }

  /** Returns elapsed time as a String since the start of the program. */
  public static String elapsed() {
    return ("[" + (System.currentTimeMillis() - start) + " msec]");
  }

  public static long elapsed_msecs() {
    return (System.currentTimeMillis() - start);
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
    return (str.trim());
  }

  // parses the single string into arguments
  public String[] parseDaikonArgs(String arg) {
    // TODO deal with quotation marks...
    return arg.split(" ");
  }
}
