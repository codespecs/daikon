package daikon;

import java.lang.*;
import java.io.*;
import java.util.*;

import daikon.chicory.*;

/**
 * This is the main class for Chicory which transforms the class files
 * of a program to instrument it for Daikon.  The instrumentation uses
 * the javaagent switch to java (which allows classes to be instrumented
 * as they are loaded).  This class parses the command line arguments,
 * starts java with the javaagent switch on the target program and
 * if requested starts Daikon on the result.
 */
public class Chicory {

  /** File in which to put dtrace output **/
  public String trace_file_name = null;

  /** Directory in which to create output files **/
  public File output_dir = new File(".");

  /** Depth to wich to examine structure components **/
  public int nesting_depth = 2;

  /** Ppts to omit (regular expression) **/
  public List<String> daikon_omit_regex = new ArrayList<String>();

  /** Ppts to include (regular expression) **/
  public List<String> daikon_include_regex = new ArrayList<String>();

  /** Print progress information **/
  public static boolean verbose = true;

  /** Main debug switch **/
  public boolean debug = false;

  /** Path to java agent jar file that performs the transformation **/
  File premain_path = null;

  /** Arguments to be passed to the java agent **/
  public List<String> premain_args = new ArrayList<String>();

  /** Render linked lists as vectors **/
  public boolean linked_lists = true;

  /** Daikon command line.  If null, Daikon is not run **/
  public String daikon_cmd = null;

  /** Target program name **/
  public String target_program = null;

  /** Target program and target program args **/
  public List<String> target_args = new ArrayList<String>();

  /** Thread that copies output from target to our output **/
  public StreamRedirectThread out_thread;

  /** Thread that copies stderr from target to our stderr **/
  public StreamRedirectThread err_thread;

  /** starting time (msecs) **/
  public static long start = System.currentTimeMillis();

  /**
   * Entry point of Chicory <p>
   * @param args see usage for argument descriptions
   */
  public static void main(String[] args) {
    Chicory chicory = new Chicory();
    chicory.parse_args(args, false);

    chicory.start_target();
  }

  /** debug print **/
  public void log(String format, Object... args) {
    if (!debug)
      return;
    System.out.printf(format, args);
  }

  /**
   * Parse the command line arguments, setting fields accordingly.  If
   * agent is true, only allows agent arguments
   */
  public void parse_args(String[] args, boolean agent) {

    int inx;
    for (inx = 0; inx < args.length; ++inx) {

      String arg = args[inx];

      if (arg.equals("--debug")) {
        debug = true;

      } else if (arg.startsWith("--nesting-depth=")) {
        String depthStr = arg.substring("--nesting-depth=".length());
        try {
          nesting_depth = Integer.parseInt(depthStr);
        } catch (NumberFormatException e) {
          usage("Invalid depth: " + depthStr);
          System.exit(1);
        }
        if (nesting_depth <= 0) {
          usage("Daikon depth " + depthStr + " must be positive");
          System.exit(1);
        }
        premain_args.add(arg);

      } else if (arg.equals("--linked-lists")) {
        linked_lists = true;
        premain_args.add(arg);

      } else if (arg.equals("--no-linked-lists")) {
        linked_lists = false;
        premain_args.add(arg);

      } else if (arg.startsWith("--ppt-omit-pattern=")) {
        String omit = arg.substring("--ppt-omit-pattern=".length());
        if (daikon_include_regex.size() > 0) {
          usage("Cannot use both --ppt-select-pattern and --ppt-omit-pattern");
          System.exit(1);
        }
        if (omit.length() == 0) {
          usage("Empty omit string");
          System.exit(1);
        }
        daikon_omit_regex.add(omit);
        premain_args.add(arg);

      } else if (arg.startsWith("--ppt-select-pattern=")) {
        String include = arg.substring("--ppt-select-pattern=".length());
        if (daikon_omit_regex.size() > 0) {
          usage("Cannot use both --ppt-select-pattern and --ppt-omit-pattern");
          System.exit(1);
        }
        if (include.length() == 0) {
          usage("Empty select string");
          System.exit(1);
        }
        daikon_include_regex.add(include);
        premain_args.add(arg);

      } else if (arg.startsWith("--dtrace-file=")) {
        trace_file_name = arg.substring("--dtrace-file=".length());
        if (trace_file_name.length() == 0) {
          usage("invalid trace file name: " + trace_file_name);
          System.exit(1);
        }
        premain_args.add(arg);

      } else if (arg.startsWith("--premain=")) {
        String premain_name = arg.substring("--premain=".length());
        premain_path = new File(premain_name);
        if (!premain_path.canRead()) {
          usage("premain is not readable: " + premain_path);
          System.exit(1);
        }

      } else if (arg.startsWith("--output-dir=")) {
        String dir_name = arg.substring("--output-dir=".length());
        output_dir = new File(dir_name);
        if (!output_dir.canWrite())
          usage("output directory not writable: " + output_dir);
        premain_args.add(arg);

      } else if (arg.equals("--help")) {
        usage();
        System.exit(0);

      } else if (arg.equals("--daikon")) {
        daikon_cmd = "daikon.Daikon";

      } else if (arg.startsWith ("--daikon=")) {
        daikon_cmd = "daikon.Daikon " + arg.substring ("--daikon=".length());

      } else if (arg.equals("--verbose")) {
        verbose = true;
        premain_args.add(arg);

      } else if (arg.equals("--no-verbose")) {
        verbose = false;
        premain_args.add(arg);

      } else if (arg.startsWith("-")) {
        usage("Unexpected argument: " + arg);
        System.exit(1);

      } else { // must be the start of the target program arguments

        target_program = arg;
        for (; inx < args.length; inx++)
          target_args.add(args[inx]);
      }
    }

    // Make sure a target program was specified
    if (!agent && target_args.size() == 0) {
      usage("Error: No target program specified");
      System.exit(1);
    }

    // Default the trace file name to the <target-program-name>.dtrace.gz
    if (trace_file_name == null) {
      String target_class = target_program.replaceFirst (".*[/.]", "");
      trace_file_name = String.format ("%s.dtrace.gz", target_class);
      premain_args.add ("--dtrace-file=" + trace_file_name);
    }

  }

  /*
   * Starts the target program with the java agent setup to do the
   * transforms.  All java agent arguments are passed to it.  Our
   * classpath is passed to the new jvm
   */
  void start_target() {

    // Get the current classpath
    String cp = System.getProperty("java.class.path");
    log("classpath = '%s'\n", cp);
    if (cp == null)
      cp = ".";

    // The the separator for items in the class path
    String separator = System.getProperty("path.separator");
    log("separator = %s\n", separator);
    if (separator == null)
      separator = ";"; //should work for windows at least...

    // Look for ChicoryPremain.jar along the classpath
    if (premain_path == null)
      {
        String[] cpath = cp.split(separator);
        for (String path : cpath)
          {
            File poss_premain = new File(path, "ChicoryPremain.jar");
            if (poss_premain.canRead())
              premain_path = poss_premain;
          }
      }

    // Build the command line to execute the target with the javaagent
    List<String> cmdlist = new ArrayList<String>();
    cmdlist.add ("java");
    cmdlist.add ("-cp");
    cmdlist.add (cp);
    cmdlist.add ("-ea");
    cmdlist.add (String.format("-javaagent:%s=%s", premain_path,
                               args_to_string(premain_args)));
    for (String target_arg : target_args)
      cmdlist.add (target_arg);
    if (verbose)
      System.out.printf ("\nExecuting target program: %s\n",
                         args_to_string(cmdlist));
    String[] cmdline = new String[cmdlist.size()];
    cmdline = cmdlist.toArray(cmdline);

    // Execute the command, sending all output to our streams
    java.lang.Runtime rt = java.lang.Runtime.getRuntime();
    Process chicory_proc = null;
    try {
      chicory_proc = rt.exec(cmdline);
    }
    catch (Exception e) {
      System.out.printf("Exception '%s' while executing '%s'\n", e,
                        cmdline);
      System.exit(1);
    }
    int result = redirect_wait (chicory_proc);

    // If no daikon command specified, show results and exit
    if (daikon_cmd == null) {
      if (result != 0)
        System.out.printf ("Warning: Target exited with %d status\n", result);
      System.exit (result);
    }

    // Terminate if target didn't end properly
    if (result != 0) {
      System.out.printf ("Warning: Target exited with %d status\n", result);
      System.out.printf ("Daikon not run\n");
      System.exit (result);
    }

    // Run Daikon on the results
    String cmdstr = String.format ("java -Xmx500m -cp %s -ea %s %s/%s",
                                   cp, daikon_cmd, output_dir, trace_file_name);
    if (verbose)
      System.out.printf ("\nExecuting daikon: %s\n", cmdstr);

    Process daikon_proc = null;
    try {
      daikon_proc = rt.exec(cmdstr);
    } catch (Exception e) {
      System.out.printf("Exception '%s' while executing '%s'\n", e, cmdstr);
      System.exit(1);
    }
    result = redirect_wait (daikon_proc);
    System.exit (result);
  }

  public int redirect_wait (Process p) {

    // Create the redirect theads and start them
    StreamRedirectThread err_thread
      = new StreamRedirectThread("stderr", p.getErrorStream(), System.err);
    StreamRedirectThread out_thread
      = new StreamRedirectThread("stdout", p.getInputStream(), System.out);
    err_thread.start();
    out_thread.start();

    // Wait for the process to terminate and return the results
    int result = -1;
    while (true) {
      try {
        result = p.waitFor();
        break;
      } catch (InterruptedException e) {
        System.out.printf ("unexpected interrupt %s while waiting for "
                           + "target to finish", e);
      }
    }

    // Make sure all output is forwarded before we finish
    try {
      err_thread.join();
      out_thread.join();
    } catch (InterruptedException e) {
      System.out.printf ("unexpected interrupt %s while waiting for "
                           + "threads to join", e);
    }

    return (result);
  }

  public PrintWriter openFileInDirectory(String fileName, String dirName)
  {
      PrintWriter outFile = null;
      try
      {
          if (dirName != null)
          {
              File directory = new File(dirName);

              //make the output directory if non-existent
              if (!directory.exists())
                  directory.mkdir();
          }

          outFile = new PrintWriter(new File(dirName, fileName));
      }
      catch (IOException e)
      {
          if (outFile != null)
              outFile.close();

          throw new Error("File creation of file " + fileName + " failed with exception " + e.getMessage());
      }
      return outFile;
  }

  void usage()
  {
      usage(null);
  }

  /**
   * Print command line usage help
   */
  void usage(String msg)
  {
      if (msg != null)
          System.err.println(msg);
      System.err.println("Usage: java daikon.Chicory <options> <class> <args>");
      System.err.println("<options> are:");
      //System.err.println("  -all                             Include system classes in output");
      System.err.println("  --nesting-depth=<integer>          Specify Daikon depth (default 2)");
      System.err.println("  --debug");
      System.err.println("  --linked-lists                     Instrument implicit linked lists as vectors (default)");
      System.err.println("  --no-linked-lists                  Don't instrument implicit linked lists as vectors");
      System.err.println("  --ppt-omit-pattern=<regex>         Specifies an include-only class for instrumentation.  Cannot be used with ppt-select-pattern");
      System.err.println("  --ppt-select-pattern=<regex>       Specifies an include-only class for instrumentation.  Cannot be used with ppt-omit-pattern");
      System.err.println("  --dtrace-file=<file>               Write the dtrace file to this filename.  Usually ends with .dtrace");
      System.err.println("  --output-dir=<directory>           Write the dtrace files to this directory (default is current directory)");
      System.err.println("  --daikon[=<daikon-args>]           Run daikon with no additional args");
      System.err.println("  --help                             Print this help message");
      System.err.println("<class> is the program to trace.  Must exist in the classpath given");
      System.err.println("<args> are the arguments to <class>");
  }

  /** Returns elapsed time as a String since the start of the program **/
  public static String elapsed()
  {
      return ("[" + (System.currentTimeMillis() - start) + " msec]");
  }

  public static long elapsed_msecs()
  {
      return (System.currentTimeMillis() - start);
  }

  public void runDaikon(String daikonArgs)
  {
      /*
       String tFileName;
       String dFileName;
       try
       {
       tFileName = (new File(outputDir, traceFileName)).getCanonicalPath();
       dFileName = (new File(outputDir, declFileName)).getCanonicalPath();
       }
       catch (IOException e)
       {
       throw new Error("Could not find decls or dtrace file: " + e);
       }

       String daikonArgArr[];

       if (daikonArgs == null)
       daikonArgArr = new String[0];
       else
       daikonArgArr = parseDaikonArgs(daikonArgs);

       try
       {
       String commands[] = new String[4 + daikonArgArr.length];

       int length = commands.length;

       commands[0] = "java";
       commands[1] = "daikon.Daikon";

       System.arraycopy(daikonArgArr, 0, commands, 2, daikonArgArr.length);

       commands[length - 2] = dFileName;
       commands[length - 1] = tFileName;

       System.out.println("Executing Daikon...");
       for (int i = 0; i < commands.length; i++)
       System.out.print(commands[i] + " ");
       System.out.println();

       Process ls_proc = Runtime.getRuntime().exec(commands);
       //Process ls_proc = Runtime.getRuntime().exec("cmd");

       StreamRedirectThread daikonErrThread = new StreamRedirectThread("daikon error reader", ls_proc.getErrorStream(), System.err);
       StreamRedirectThread daikonOutThread = new StreamRedirectThread("daikon output reader", ls_proc.getInputStream(), System.out);

       daikonErrThread.start();
       daikonOutThread.start();

       // Shutdown begins when event thread terminates
       try
       {
       daikonErrThread.join(); // Make sure output is forwarded
       daikonOutThread.join(); // before we exit

       //System.out.println("\nDaikon completed!");
       }
       catch (InterruptedException exc)
       {
       // we don't interrupt
       }

       }
       catch (IOException e1)
       {
       System.out.println("could not run Daikon: " + e1);
       }
       */
  }

  /** convert a list of arguments into a command line string **/
  public String args_to_string(List<String> args)
  {
      String str = "";
      for (String arg : args)
          str += arg + " ";
      return (str.trim());
  }

  //parses the single string into arguments
  public String[] parseDaikonArgs(String arg)
  {
      //TODO deal with quotation marks...
      return arg.split(" ");
  }

}
