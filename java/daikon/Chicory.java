package daikon;

import java.lang.*;
import java.io.*;
import java.util.*;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

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
  public List<Pattern> daikon_omit_regex = new ArrayList<Pattern>();

  /** Ppts to include (regular expression) **/
  public List<Pattern> daikon_include_regex = new ArrayList<Pattern>();

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

  /**Daikon command line for online mode.  Daikon not run in online mode if null **/
  public String daikon_cmd_online = null;

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

  /** daikon process for --daikon switch **/
  public Process daikon_proc;

  // private List<String> cmdList;

  private static final String traceLimTermString = "DTRACELIMITTERMINATE";
  private static final String traceLimString = "DTRACELIMIT";

  /**socket port to communicate with Daikon**/
  private int daikonPort = -1;

  /** flag to use if we want to turn on the static initialization checks**/
  public static final boolean checkStaticInit = false;

  private static final boolean RemoteDebug = false;

  /**
   * Entry point of Chicory <p>
   * @param args see usage for argument descriptions
   */
  public static void main(String[] args) {

      parseAndLoadTarget(args);
  }

  public static void parseAndLoadTarget(String[] args)
  {
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

      }
      else if (arg.startsWith("--daikon-port="))
            {
                String portStr = arg.substring("--daikon-port=".length());
                try
                {
                    daikonPort  = Integer.parseInt(portStr);
                }
                catch (NumberFormatException e)
                {
                    usage("Invalid port: " + portStr);
                    System.exit(1);
                }

            }
          else if (arg.equals("--linked-lists")) {
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
        
        try
        {
            daikon_omit_regex.add(Pattern.compile(omit));
        }
        catch (PatternSyntaxException e)
        {
            System.out.println("WARNING: Error during regular expression compilation: " + e.getMessage());
        }
        
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
        
        try
        {
            daikon_include_regex.add(Pattern.compile(include));
        }
        catch (PatternSyntaxException e)
        {
            System.out.println("WARNING: Error during regular expression compilation: " + e.getMessage());
        }
        
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

      }
      else if (arg.equals("--daikon")) {
          daikon_cmd = "daikon.Daikon";

        }
      else if (arg.startsWith ("--daikon=")) {
          daikon_cmd = "daikon.Daikon " + arg.substring ("--daikon=".length());
      }

         else if (arg.equals("--daikon-online")) {
        daikon_cmd_online = "daikon.Daikon +";

      }
      else if (arg.startsWith ("--daikon-online=")) {
        daikon_cmd_online = "daikon.Daikon " + arg.substring ("--daikon-online=".length()) + " +";

      }else if (arg.equals("--verbose")) {
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

    // If not on the classpath look in $(DAIKONDIR)/java
    if (premain_path == null) {
      String daikon_dir = System.getenv ("DAIKONDIR");
      if (daikon_dir != null) {
        String file_separator = System.getProperty ("file.separator");
        File poss_premain = new File (daikon_dir + file_separator + "java",
                                      "ChicoryPremain.jar");
        if (poss_premain.canRead())
          premain_path = poss_premain;
      }
    }

    // If not found, try the daikon.jar file itself
    if (premain_path == null) {
      for (String path : cp.split(separator)) {
        File poss_premain = new File(path);
        if (poss_premain.getName().equals ("daikon.jar"))
          if (poss_premain.canRead())
            premain_path = poss_premain;
        }
    }

    // If we didn't find a premain, give up
    if (premain_path == null) {
      System.err.printf ("Can't find ChicoryPremain.jar on the classpath\n");
      System.err.printf ("or in $DAIKONDIR/java\n");
      System.err.printf ("It should be find in directory where Daikon was "
                         + " installed\n");
      System.err.printf ("Use the --premain switch to specify its location\n");
      System.err.printf ("or change your classpath to include it\n");
      System.exit (1);
    }

    String dtraceLim, terminate;
    dtraceLim = System.getProperty(traceLimString);
    terminate = System.getProperty(traceLimTermString);

        //run Daikon if we're in online mode
        StreamRedirectThread daikon_err = null, daikon_out = null;
        if (daikon_cmd_online != null)
        {
            runDaikon(true);

            daikon_err = new StreamRedirectThread("stderr", daikon_proc.getErrorStream(), System.err);
            daikon_err.start();

            InputStream daikonStdOut = daikon_proc.getInputStream();
            BufferedReader daikonReader = new BufferedReader(new InputStreamReader(daikonStdOut));

            //online mode code
            for (int i = 0; i < 100; i++)
            {
                String line;
                try
                {
                    line = daikonReader.readLine();
                }
                catch (IOException e1)
                {
                    line = null;
                }

                if (line == null)
                {
                    throw new RuntimeException("Did not receive socket port from Daikon!");
                }
                else
                {
                    //if (!line.startsWith("DaikonChicoryOnlinePort="))
                        System.out.println(line);

                    if (line.startsWith("DaikonChicoryOnlinePort="))
                    {
                        String portStr = line.substring("DaikonChicoryOnlinePort=".length());
                        //System.out.println("GOT PORT STRING " + portStr);
                        premain_args.add("--daikon-port=" + portStr);
                        break;
                    }
                }
            }

            //continue reading daikon output in separate thread
            daikon_out = new StreamRedirectThread("stdout", daikonStdOut, System.out);
            daikon_out.start();
        }



    // Build the command line to execute the target with the javaagent
    List<String> cmdlist = new ArrayList<String>();
    cmdlist.add ("java");

    if(RemoteDebug)
    {
        //-Xdebug -Xrunjdwp:server=y,transport=dt_socket,address=4142,suspend=n
        cmdlist.add("-Xdebug -Xrunjdwp:server=n,transport=dt_socket,address=8000,suspend=y");
        //cmdlist.add("-Xdebug -Xnoagent -Xrunjdwp:transport=dt_socket,server=n,suspend=n,address=8000 -Djava.compiler=NONE");
    }

    cmdlist.add ("-cp");
    cmdlist.add (cp);
    cmdlist.add ("-ea");

    if(dtraceLim != null)
        cmdlist.add("-D" + traceLimString + "=" + dtraceLim);
    if(terminate != null)
        cmdlist.add("-D" + traceLimTermString + "=" + terminate );
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

    if(daikon_cmd != null)
    {
    // Terminate if target didn't end properly
    if (result != 0) {
      System.out.printf ("Warning: Target exited with %d status\n", result);
      System.out.printf ("Daikon not run\n");
      System.exit (result);
    }

    runDaikon(false);
    result = waitForDaikon();
    System.exit(result);
    }
    else if (daikon_cmd_online != null)
    {
        // Wait for the process to terminate and return the results
        result = -1;
        while (true) {
          try {
            result = daikon_proc.waitFor();
            break;
          } catch (InterruptedException e) {
            System.out.printf ("unexpected interrupt %s while waiting for "
                               + "target to finish", e);
          }
        }

        // Make sure all output is forwarded before we finish
        try {
            daikon_err.join();
            daikon_out.join();
        } catch (InterruptedException e) {
          System.out.printf ("unexpected interrupt %s while waiting for "
                               + "threads to join", e);
        }

        System.exit(result);
    }
    else
    {
        // If no daikon command specified, show results and exit
        if (result != 0)
            System.out.printf ("Warning: Target exited with %d status\n", result);
          System.exit (result);
    }
  }


  public void runDaikon(boolean isOnline)
  {
      java.lang.Runtime rt = java.lang.Runtime.getRuntime();

      // Get the current classpath
      String cp = System.getProperty("java.class.path");
      if (cp == null)
        cp = ".";

      String cmdstr;
        if (isOnline)
        {
            cmdstr = String.format("java -Xmx500m -cp %s -ea %s", cp, daikon_cmd_online);
        }
        else
        {
            cmdstr = String.format("java -Xmx500m -cp %s -ea %s %s/%s", cp, daikon_cmd, output_dir, trace_file_name);
        }

    //cmdstr = "java -Xmx500m -cp " + cp + " -ea " + daikon_cmd + " " + output_dir+"/"+trace_file_name;


    //System.out.println("daikon command is " + daikon_cmd);
    //System.out.println("daikon command cmdstr " + cmdstr);

    if (verbose)
      System.out.printf ("\nExecuting daikon: %s\n", cmdstr);

    daikon_proc = null;
    try {
      daikon_proc = rt.exec(cmdstr);
    } catch (Exception e) {
      System.out.printf("Exception '%s' while executing '%s'\n", e, cmdstr);
      System.exit(1);
    }
  }

  private int waitForDaikon()
  {
    int result = redirect_wait (daikon_proc);
    return result;
  }


  /** Wait for stream redirect threads to complete **/
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
          if (outFile == null) { throw new RuntimeException("This can't happen."); }
          outFile.close();

          throw new Error("File creation of file " + fileName + " failed", e);
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
      System.err.println("  --daikon-online[=<daikon-args>]    Run daikon with no additional args in online mode via socket communication");
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

  /**
  * @return The local socket port on which Daikon is listening
  */
  public int getDaikonPort()
  {
    return daikonPort;
  }

}
