package daikon.tools;

import daikon.*;
import daikon.inv.*;
import utilMDE.*;

import java.util.*;
import java.io.*;
import gnu.getopt.*;
import java.util.logging.Logger;
import java.util.logging.Level;

/**
 * InvariantChecker reads an invariant file and trace file.  It prints errors
 * for any invariants that are violated by the trace file.
 */
public class InvariantChecker {
  private InvariantChecker() { throw new Error("do not instantiate"); }

  public static final Logger debug
    = Logger.getLogger ("daikon.tools.InvariantChecker");

  public static final Logger debug_detail
    = Logger.getLogger ("daikon.tools.InvariantCheckerDetail");


  private static final String output_SWITCH = "output";
  private static final String dir_SWITCH = "dir";

  private static String usage =
    UtilMDE.joinLines(
      "Usage: java daikon.InvariantChecker [OPTION]... <inv_file> "
        + "<dtrace_file>",
      "  -h, --" + Daikon.help_SWITCH,
      "      Display this usage message",
      "  --" + output_SWITCH + " output file",
      "  --" + dir_SWITCH + " directory with invariant and dtrace files. We output a matrix saying how many invariants failed for each invariant file and each dtrace file.",
      "  --" + Daikon.config_option_SWITCH + " config_var=val",
      "      Sets the specified configuration variable.  ",
      "  --" + Daikon.debugAll_SWITCH,
      "      Turns on all debug flags (voluminous output)",
      "  --" + Daikon.debug_SWITCH + " logger",
      "      Turns on the specified debug logger",
      "  --" + Daikon.track_SWITCH + " class<var1,var2,var3>@ppt",
      "      Print debug info on the specified invariant class, vars, and ppt"
      );

  public static File inv_file = null;
  public static List<String> dtrace_files = new ArrayList<String>();
  static File output_file;
  static PrintStream output_stream = System.out;
  static int error_cnt = 0;

  static File dir_file; //Yoav added
  static Set<String> failedInvariants; //Yoav added
  static Set<String> allFailedInvariants; //Yoav added
  static LinkedHashSet<String> outputMatrix; //Yoav added

  public static void main(String[] args)
    throws FileNotFoundException, StreamCorruptedException,
           OptionalDataException, IOException, ClassNotFoundException {
    try {
      mainHelper(args);
    } catch (Daikon.TerminationMessage e) {
      System.err.println(e.getMessage());
      System.exit(1);
    }
    // Any exception other than Daikon.TerminationMessage gets propagated.
    // This simplifies debugging by showing the stack trace.
  }

  /**
   * This does the work of main, but it never calls System.exit, so it
   * is appropriate to be called progrmmatically.
   * Termination of the program with a message to the user is indicated by
   * throwing Daikon.TerminationMessage.
   * @see #main(String[])
   * @see daikon.Daikon.TerminationMessage
   **/
  public static void mainHelper(final String[] args)
    throws FileNotFoundException, StreamCorruptedException,
           OptionalDataException, IOException, ClassNotFoundException {
    daikon.LogHelper.setupLogs(daikon.LogHelper.INFO);

    LongOpt[] longopts = new LongOpt[] {
      new LongOpt(Daikon.config_option_SWITCH, LongOpt.REQUIRED_ARGUMENT,
                  null, 0),
      new LongOpt(output_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
      new LongOpt(dir_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
      new LongOpt(Daikon.debugAll_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.debug_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
      new LongOpt(Daikon.ppt_regexp_SWITCH, LongOpt.REQUIRED_ARGUMENT, null,
                  0),
      new LongOpt(Daikon.track_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
    };
    Getopt g = new Getopt("daikon.PrintInvariants", args, "h", longopts);
    int c;
    while ((c = g.getopt()) != -1) {
      switch(c) {
      case 0:
        // got a long option
        String option_name = longopts[g.getLongind()].getName();
        if (Daikon.help_SWITCH.equals(option_name)) {
          System.out.println(usage);
          throw new Daikon.TerminationMessage();
        } else if (dir_SWITCH.equals (option_name)) {
          dir_file = new File (g.getOptarg());
          if (!dir_file.exists() || !dir_file.isDirectory())
             throw new Daikon.TerminationMessage ("Error reading the directory "+dir_file);

        } else if (output_SWITCH.equals (option_name)) {
          output_file = new File (g.getOptarg());
          output_stream = new PrintStream (new FileOutputStream (output_file));
        } else if (Daikon.config_option_SWITCH.equals(option_name)) {
          String item = g.getOptarg();
          daikon.config.Configuration.getInstance().apply(item);
          break;
        } else if (Daikon.debugAll_SWITCH.equals(option_name)) {
          Global.debugAll = true;
        } else if (Daikon.debug_SWITCH.equals(option_name)) {
          LogHelper.setLevel(g.getOptarg(), LogHelper.FINE);
        } else if (Daikon.track_SWITCH.equals (option_name)) {
          LogHelper.setLevel("daikon.Debug", LogHelper.FINE);
          String error = Debug.add_track (g.getOptarg());
          if (error != null) {
            throw new Daikon.TerminationMessage ("Error parsing track argument '"
                                + g.getOptarg() + "' - " + error);
          }
        } else {
          throw new RuntimeException("Unknown long option received: " +
                                     option_name);
        }
        break;
      case 'h':
        System.out.println(usage);
        throw new Daikon.TerminationMessage();
      case '?':
        break; // getopt() already printed an error
      default:
        System.out.println("getopt() returned " + c);
        break;
      }
    }

    // Loop through each filename specified
    for (int i=g.getOptind(); i<args.length; i++) {

      // Get the file and make sure it exists
      File file = new File(args[i]);
      if (! file.exists()) {
        throw new Error("File " + file + " not found.");
      }

      // These aren't "endsWith()" because there might be a suffix on the end
      // (eg, a date).
      String filename = file.toString();
      if (filename.indexOf(".inv") != -1) {
        if (inv_file != null) {
          throw new Daikon.TerminationMessage ("multiple inv files specified", usage);
        }
        inv_file = file;
      } else if (filename.indexOf(".dtrace") != -1) {
        dtrace_files.add(filename);
      } else {
        throw new Error("Unrecognized argument: " + file);
      }
    }
    if (dir_file==null) {
      checkInvariants();
      return;
    }

    // Yoav additions:
    failedInvariants = new HashSet<String>();
    allFailedInvariants = new HashSet<String>();
    outputMatrix = new LinkedHashSet<String>();
    File[] filesInDir = dir_file.listFiles();
    if (filesInDir == null || filesInDir.length==0)
          throw new Daikon.TerminationMessage("The directory "+dir_file+" is empty", usage);
    ArrayList<File> invariants = new ArrayList<File>();
    for(File f: filesInDir)
       if (f.toString().indexOf(".inv") != -1) invariants.add(f);
    if (invariants.size()==0)
          throw new Daikon.TerminationMessage("Did not find any invariant files in the directory "+dir_file, usage);
    ArrayList<File> dtraces = new ArrayList<File>();
    for(File f: filesInDir)
       if (f.toString().indexOf(".dtrace") != -1) dtraces.add(f);
    if (dtraces.size()==0)
          throw new Daikon.TerminationMessage("Did not find any dtrace files in the directory "+dir_file, usage);

    output_stream.println("Building a matrix for invariants files "+invariants+" and dtrace files "+dtraces);

    for (File inFile : invariants) {
      inv_file = inFile;
      int invNum = 0;
      for (File dtrace : dtraces) {
        dtrace_files.clear();
        failedInvariants.clear();
        dtrace_files.add(dtrace.toString());
        int invNum2 = checkInvariants();
        if (invNum==0) invNum = invNum2;
        assert invNum==invNum2;
        int failedCount = failedInvariants.size();
        allFailedInvariants.addAll(failedInvariants);
        outputMatrix.add(inv_file+" - "+dtrace+": "+failedCount+" false positives, which is "+toPercentage(failedCount, invNum)+".");
        error_cnt = 0;
      }
      int failedCount = allFailedInvariants.size();
      outputMatrix.add(inv_file+": "+failedCount+" false positives, out of "+invNum+", which is "+toPercentage(failedCount, invNum)+".");
      allFailedInvariants.clear();
    }
    output_stream.println();
    for (String output : outputMatrix)
      output_stream.println(output);
  }
  private static String toPercentage(int portion, int total) {
    double s = portion * 100;
    return String.format("%.2f",s /total)+"%";
  }
  private static int checkInvariants() throws IOException {
    // Read the invariant file
    PptMap ppts = FileIO.read_serialized_pptmap (inv_file, true );

    // Read and process the data trace files
    FileIO.Processor processor = new InvariantCheckProcessor();

    Daikon.FileIOProgress progress = new Daikon.FileIOProgress();
    progress.start();
    progress.clear();
    FileIO.read_data_trace_files (dtrace_files, ppts, processor, false);
    progress.shouldStop = true;
    System.out.println ();
    System.out.println ("" + error_cnt + " Errors Found, out of "+ppts.size()+" invariants" );
    return ppts.size();
  }

  /** Class to track matching ppt and its values. */
  static final class EnterCall {

    public PptTopLevel ppt;
    public ValueTuple vt;

    public EnterCall (PptTopLevel ppt, ValueTuple vt) {

      this.ppt = ppt;
      this.vt = vt;
    }
  }

  public static class InvariantCheckProcessor extends FileIO.Processor {

    PptMap all_ppts = null;

    Map<Integer,EnterCall> call_map = new LinkedHashMap<Integer,EnterCall>();

    /**
     * process the sample by checking it against each existing invariant
     * and issuing an error if any invariant is falsified or weakened.
     */
    public void process_sample (PptMap all_ppts, PptTopLevel ppt,
                                ValueTuple vt, Integer nonce) {

      this.all_ppts = all_ppts;

      debug.fine ("processing sample from: " + ppt.name);

      // Add orig and derived variables
      FileIO.add_orig_variables(ppt, vt.vals, vt.mods, nonce);
      FileIO.add_derived_variables(ppt, vt.vals, vt.mods);

      // Intern the sample
      vt = new ValueTuple(vt.vals, vt.mods);

      // If this is an enter point, just remember it for later
      if (ppt.ppt_name.isEnterPoint()) {
        Assert.assertTrue (nonce != null);
        Assert.assertTrue (call_map.get (nonce) == null);
        call_map.put (nonce, new EnterCall (ppt, vt));
        debug.fine ("Skipping enter sample");
        return;
      }

      // If this is an exit point, process the saved enter point
      if (ppt.ppt_name.isExitPoint()) {
        Assert.assertTrue (nonce != null);
        EnterCall ec = call_map.get (nonce);
        call_map.remove (nonce);
        debug.fine ("Processing enter sample from " + ec.ppt.name);
        add (ec.ppt, ec.vt);
      }

      add (ppt, vt);
    }

    private void add (PptTopLevel ppt, ValueTuple vt) {

      // Add the sample to any splitters
      if (ppt.has_splitters()) {
        for (PptSplitter ppt_split : ppt.splitters) {
          PptConditional ppt_cond = ppt_split.choose_conditional (vt);
          if (ppt_cond != null)
            add (ppt_cond, vt);
          else
            debug.fine (": sample doesn't pick conditional");
        }
      }

      // if this is a numbered exit, apply to the combined exit as well
      if (!(ppt instanceof PptConditional)
          && ppt.ppt_name.isNumberedExitPoint()) {
        PptTopLevel parent = all_ppts.get (ppt.ppt_name.makeExit());
        if (parent != null) {
          parent.get_missingOutOfBounds (ppt, vt);
          add (parent, vt);
        }
      }

      // If the point has no variables, skip it
      if (ppt.var_infos.length == 0)
        return;

      // We should have received sample here before, or there is nothing
      // to check.
      // Yoav added: It can be that the different dtrace and inv files have different program points
      if (false && ppt.num_samples() <= 0)
        Assert.assertTrue (ppt.num_samples() > 0, "ppt " + ppt.name
                            + " has 0 samples and "
                            + ppt.var_infos.length + " variables");

      // Loop through each slice
      slice_loop:
      for (Iterator<PptSlice> i = ppt.views_iterator(); i.hasNext(); ) {
        PptSlice slice = i.next();
        if (debug_detail.isLoggable (Level.FINE))
          debug_detail.fine (": processing slice " + slice + "vars: "
                           + Debug.toString (slice.var_infos, vt));

        // If any variables are missing, skip this slice
        for (int j = 0; j < slice.var_infos.length; j++) {
          VarInfo v = slice.var_infos[j];
          int mod = vt.getModified (v);
          if (v.isMissing (vt)) {
            if (debug_detail.isLoggable (Level.FINE))
              debug_detail.fine (": : Skipping slice, " + v.name()
                               + " missing");
            continue slice_loop;
          }
          if (v.missingOutOfBounds()) {
            if (debug_detail.isLoggable (Level.FINE))
              debug.fine (": : Skipping slice, " + v.name()
                          + " out of bounds");
            continue slice_loop;
          }
        }

        // Loop through each invariant
        for (Invariant inv : slice.invs) {
          if (debug_detail.isLoggable (Level.FINE))
            debug_detail.fine (": : Processing invariant: " + inv);
          if (!inv.isActive()) {
            if (debug_detail.isLoggable (Level.FINE))
              debug_detail.fine (": : skipped non-active " + inv);
            continue;
          }
          Invariant pre_inv = (Invariant) inv.clone();
          InvariantStatus status = inv.add_sample (vt, 1);
          if (status != InvariantStatus.NO_CHANGE) {
            // Invariant pre_inv = inv;
            LineNumberReader lnr = FileIO.data_trace_state.reader;
            String line = (lnr == null) ? "?"
                        : String.valueOf(lnr.getLineNumber());
            String invName = pre_inv.format();
            output_stream.println ("At ppt " + ppt.name + ", Invariant '"
                 + invName + "' invalidated by sample "
                 + Debug.toString (slice.var_infos, vt) + "at line " + line);
            if (dir_file!=null) failedInvariants.add(invName);
            error_cnt++;
          }
        }
      }
    }
  }
}
