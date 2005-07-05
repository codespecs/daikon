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

  private static final Daikon.FileIOProgress progress
    = new Daikon.FileIOProgress();


  private static final String output_SWITCH = "output";

  private static String usage =
    UtilMDE.joinLines(
      "Usage: java daikon.PrintInvariants [OPTION]... <inv_file> "
        + "<dtrace_file>",
      "  -h, --" + Daikon.help_SWITCH,
      "      Display this usage message",
      "  --" + output_SWITCH + " output file",
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
  public static List dtrace_files = new ArrayList();
  static File output_file;
  static PrintStream output_stream = System.out;
  static int error_cnt = 0;

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

    // Read the invariant file
    PptMap ppts = FileIO.read_serialized_pptmap (inv_file, true );

    // Read and process the data trace files
    FileIO.Processor processor = new InvariantCheckProcessor();
    progress.start();
    progress.clear();
    FileIO.read_data_trace_files (dtrace_files, ppts, processor);
    progress.shouldStop = true;
    System.out.println ();
    System.out.println ("" + error_cnt + " Errors Found");
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

    /** nonce -> EnterCall **/
    Map call_map = new LinkedHashMap();

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
        EnterCall ec = (EnterCall) call_map.get (nonce);
        call_map.remove (nonce);
        debug.fine ("Processing enter sample from " + ec.ppt.name);
        add (ec.ppt, ec.vt);
      }

      add (ppt, vt);
    }

    private void add (PptTopLevel ppt, ValueTuple vt) {

      // Add the sample to any splitters
      if (ppt.has_splitters()) {
        for (Iterator ii = ppt.splitters.iterator(); ii.hasNext(); ) {
          PptSplitter ppt_split = (PptSplitter) ii.next();
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
      if (ppt.num_samples() <= 0)
        Assert.assertTrue (ppt.num_samples() > 0, "ppt " + ppt.name
                            + " has 0 samples and "
                            + ppt.var_infos.length + " variables");

      // Loop through each slice
      slice_loop:
      for (Iterator i = ppt.views_iterator(); i.hasNext(); ) {
        PptSlice slice = (PptSlice) i.next();
        if (debug_detail.isLoggable (Level.FINE))
          debug_detail.fine (": processing slice " + slice + "vars: "
                           + Debug.toString (slice.var_infos, vt));

        // If any variables are missing, skip this slice
        for (int j = 0; j < slice.var_infos.length; j++) {
          VarInfo v = slice.var_infos[j];
          int mod = vt.getModified (v);
          if (v.isMissing (vt)) {
            if (debug_detail.isLoggable (Level.FINE))
              debug_detail.fine (": : Skipping slice, " + v.name.name()
                               + " missing");
            continue slice_loop;
          }
          if (v.missingOutOfBounds()) {
            if (debug_detail.isLoggable (Level.FINE))
              debug.fine (": : Skipping slice, " + v.name.name()
                          + " out of bounds");
            continue slice_loop;
          }
        }

        // Loop through each invariant
        for (Iterator j = slice.invs.iterator(); j.hasNext(); ) {
          Invariant inv = (Invariant) j.next();
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
            output_stream.println ("At ppt " + ppt.name + ", Invariant '"
                 + pre_inv.format() + "' invalidated by sample "
                 + Debug.toString (slice.var_infos, vt) + "at line " + line);
            error_cnt++;
          }
        }
      }
    }
  }
}
