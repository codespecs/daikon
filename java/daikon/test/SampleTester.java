package daikon.test;

import daikon.*;
import daikon.inv.*;
import utilMDE.*;

import gnu.getopt.*;
import java.io.*;
import java.lang.reflect.*;
import java.net.URL;
import java.util.*;
import java.util.logging.Logger;
import java.util.logging.Level;

import junit.framework.*;


/**
 * This tests Daikon's state as samples are processed.  A standard
 * decl file specifies the ppts.  A sample input file specifies the
 * samples and assertions that should be true at various points while
 * processing.
 *
 * The input file format is documented in the developer manual.
 **/
public class SampleTester extends TestCase {

  public static final String lineSep = Global.lineSep;

  public static final Logger debug
                                = Logger.getLogger("daikon.test.SampleTester");
  String fname;
  LineNumberReader fp;
  PptMap all_ppts;
  PptTopLevel ppt;
  VarInfo vars[];

  private static String usage =
    UtilMDE.join(new String[] {
      "Usage: java daikon.PrintInvariants [OPTION]... FILE",
      "  -h, --" + Daikon.help_SWITCH,
      "      Display this usage message",
      "  --" + Daikon.config_option_SWITCH,
      "      Specify a configuration option ",
      "  --" + Daikon.debug_SWITCH,
      "      Specify a logger to enable",
      "  --" + Daikon.track_SWITCH,
      "      Specify a class, varinfos, and ppt to debug track.  Format"
             + "is class<var1,var2,var3>@ppt",
      },
      lineSep);

  public static void main(String[] args) throws IOException {

    LongOpt[] longopts = new LongOpt[] {
      new LongOpt(Daikon.config_option_SWITCH, LongOpt.REQUIRED_ARGUMENT,
                  null, 0),
      new LongOpt(Daikon.debugAll_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(Daikon.debug_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
      new LongOpt(Daikon.track_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
    };

    Getopt g = new Getopt("daikon.test.SampleTester", args, "h:", longopts);
    int c;
    while ((c = g.getopt()) != -1) {
      switch(c) {

      // long option
      case 0:
        String option_name = longopts[g.getLongind()].getName();
        if (Daikon.help_SWITCH.equals(option_name)) {
          System.out.println(usage);
          System.exit(1);

        } else if (Daikon.config_option_SWITCH.equals(option_name)) {
          String item = g.getOptarg();
          daikon.config.Configuration.getInstance().apply(item);
          break;

        } else if (Daikon.debugAll_SWITCH.equals(option_name)) {
          Global.debugAll = true;

        } else if (Daikon.debug_SWITCH.equals(option_name)) {
          LogHelper.setLevel(g.getOptarg(), LogHelper.FINE);
        } else if (Daikon.track_SWITCH.equals(option_name)) {
          LogHelper.setLevel("daikon.Debug", LogHelper.FINE);
          String error = Debug.add_track (g.getOptarg());
          if (error != null) {
            System.out.println ("Error parsing track argument '"
                                + g.getOptarg() + "' - " + error);
            System.exit(1);
          }
        } else {
          throw new RuntimeException("Unknown long option received: " +
                                     option_name);
        }
        break;

      case 'h':
        System.out.println(usage);
        System.exit(1);
        break;

      case '?':
        break; // getopt() already printed an error

      default:
        System.out.println("getopt() returned " + c);
        break;
      }
    }

    daikon.LogHelper.setupLogs(Global.debugAll ? LogHelper.FINE
                               : LogHelper.INFO);

    String input_file = find_file ("daikon/test/SampleTester.commands");
    if (input_file == null)
      fail ("Input file SampleTester.commands missing." +
           " (Should be in daikon.test and it must be within the classpath)");

    SampleTester ts = new SampleTester();
    ts.proc_sample_file (input_file);
    Fmt.pf ("Test Passes");
  }

  private static String find_file (String fname) {

    URL input_file_location =
      ClassLoader.getSystemClassLoader().getSystemResource (fname);

    if (input_file_location == null)
      return (null);
    else
      return (input_file_location.getFile());
  }

  /**
   * This function is the actual function performed when this class is
   * run through JUnit.
   **/
  public void test_samples () throws IOException {

    String input_file = find_file ("daikon/test/SampleTester.commands");
    if (input_file == null)
      fail ("Input file SampleTester.commands missing." +
           " (Should be in daikon.test and it must be within the classpath)");

    SampleTester ts = new SampleTester();
    ts.proc_sample_file (input_file);
  }

  public void proc_sample_file (String fname) throws IOException {

    this.fname = fname;
    fp = UtilMDE.LineNumberFileReader(fname);
    for (String line = fp.readLine(); line != null; line = fp.readLine()) {

      // Remove comments and skip blank lines
      line = line.replaceAll ("#.*", "");
      line = line.trim();
      if (line.length() == 0)
        continue;

      // Get the line type and the remainder of the line
      String[] sa = line.split (": *", 2);
      if (sa.length != 2)
        parse_error ("No line type specified");
      String ltype = sa[0].trim();
      String cmd = sa[1].trim();
      if (cmd.length() == 0)
        parse_error ("no command specified");

      // Process the specified type of command
      if (ltype.equals ("decl"))
        proc_decl (cmd);
      else if (ltype.equals ("ppt"))
        proc_ppt (cmd);
      else if (ltype.equals ("vars"))
        proc_vars (cmd);
      else if (ltype.equals ("data"))
        proc_data (cmd);
      else if (ltype.equals ("assert"))
        proc_assert (cmd);
      else
        parse_error ("unknown line type: " + ltype);
    }

  }

  /**
   * Reads in the specified decl file and sets all_ppts accordingly
   */
  private void proc_decl (String decl_file) throws IOException {

    // Read in the specified file
    Set decl_files = new HashSet(1);
    decl_files.add (new File(decl_file));
    all_ppts = FileIO.read_declaration_files (decl_files);

    // Setup everything to run
    Dataflow.init_partial_order (all_ppts);
    PptTopLevel.init (all_ppts);
    Daikon.setupEquality (all_ppts);

    ppt = null;
  }

  /**
   * Looks up the specified ppt name and set ppt accordingly
   */
  private void proc_ppt (String ppt_name) {

    if (all_ppts == null)
      parse_error ("decl file must be specified before ppt");
    ppt = all_ppts.get(ppt_name);
    if (ppt == null)
      parse_error ("ppt name " + ppt_name + " not found in decl file");
    vars = null;
  }

  /**
   * Processes a variable list.  Sets up the vars[] array to point to the
   * matching variables in the ppt.  The ppt must have been previously
   * specified.  Variables are separated by spaces
   */
  private void proc_vars (String var_names) {

    if (ppt == null)
      parse_error ("ppt must be specified first");

    // Variable names are separated by blanks
    String[] var_arr = var_names.split ("  *");

    // The var array contains the variables in the ppt that correspond
    // to each name specified
    vars = new VarInfo[var_arr.length];

    // Loop through each variable name and find it in the variable list
    for (int i = 0; i < var_arr.length; i++) {
      String vname = var_arr[i];
      for (int j = 0; j < ppt.var_infos.length; j++) {
        if (vname.equals (ppt.var_infos[j].name.name()))
          vars[i] = ppt.var_infos[j];
      }
      if (vars[i] == null)
        parse_error ("Variable " + vname + " not found in ppt " + ppt.name());
    }
  }

  /**
   * Processes a line of sample data.  There should be one item of
   * data for each previously specified variable.  Each data item is
   * separated by spaces.  Spaces cannot be included within a single
   * item (ie, strings and arrays can't include spaces). Missing items
   * are indicated with a dash (-).  Any variables not specifically
   * mentioned in the variable string are set to missing as well.
   *
   * Neither orig nor derived variables are added.
   */
  private void proc_data (String data) {

    if (vars == null)
      parse_error ("vars must be specified before data");
    String[] da = data.split ("  *");
    if (da.length != vars.length)
      parse_error ("number of data elements doesn't match var elements");

    VarInfo[] vis = ppt.var_infos;
    int vals_array_size = vis.length;
    Object[] vals = new Object[vals_array_size];
    int[] mods = new int[vals_array_size];

    // initially all variables are missing
    for (int i = 0; i < vals_array_size; i++) {
      vals[i] = null;
      mods[i] = ValueTuple.parseModified ("2");
    }

    // Parse and enter the specified variables, - indicates a missing value
    for (int i = 0; i < vars.length; i++) {
      if (da[i].equals("-"))
        continue;
      VarInfo vi = vars[i];
      vals[vi.value_index] = vi.rep_type.parse_value (da[i]);
      mods[vi.value_index] = ValueTuple.parseModified ("1");
    }

    ValueTuple vt = ValueTuple.makeUninterned (vals, mods);

    // We might want to add the following at some point.  Certainly the
    // derived variables.  The orig variables force us to deal with matching
    // enter and exit which I really don't want to do.  Perhaps we can always
    // give them the same value at enter and exit.  Both of these calls
    // are in FileIO

    // add_orig_variables (ppt, vt.vals, vt.mods, nonce);
    // add_derived_variables (ppt, vt.vals, vt.mods);

    // Causes interning
    vt = new ValueTuple (vt.vals, vt.mods);

    ppt.add_global_bottom_up (vt, 1);
  }

  /**
   * Processes a string of possibly multiple assertions.  If any are false,
   * throws an error
   */
  private void proc_assertions (String assertions) {

    String[] aa = assertions.split ("\\) *");
    for (int i = 0; i < aa.length; i++) {
      proc_assert (aa[i]);
    }
  }

  /**
   * Processes a single assertion.  If the assertion is false, throws
   * an error
   */
  private void proc_assert (String assertion) {

    // Look for negation
    boolean negate = false;
    String assert_string = assertion;
    if (assertion.indexOf('!') == 0) {
      negate = true;
      assert_string = assert_string.substring(1);
    }

    // Get the assertion name and arguments
    assert_string = assert_string.replaceFirst ("\\)$", "");
    String[] aa = assert_string.split (" *\\( *");
    if (aa.length != 2)
      parse_error ("invalid assertion");
    String name = aa[0];
    String[] args = aa[1].trim().split (", *");

    // process the specific assertion
    boolean result = false;
    if (name.equals ("inv")) {
      result = proc_inv_assert (args);
      if (!result && !negate) {
        debug.setLevel (Level.FINE);
        proc_inv_assert (args);
      }
    } else
      parse_error ("unknown assertion: " + name);

    if (negate) result = !result;

    if (!result) {
      fail (Fmt.spf ("Assertion %s fails in file %s at line %s", assertion,
                     fname, Fmt.i(fp.getLineNumber())));
    }
  }

  /**
   * Processes an invariant existence assertion and returns true if it is
   * found.  The first argument should be the invariant class.  The remaining
   * arguments are the variables.  This needs to be expanded to specify
   * more information for invariants with state.
   */
  private boolean proc_inv_assert (String[] args) {

    if ((args.length < 2) || (args.length > 4))
      parse_error ("bad argument count for invariant assertion");

    Class cls = null;
    String format = null;

    // If the first argument is a string
    if (args[0].startsWith ("\"")) {
      format = args[0].substring(1,args[0].length()-1);
      debug.fine (Fmt.spf ("Looking for format: '%s'", format));
    } else { // must be a classname
      try {
        cls = Class.forName (args[0]);
      } catch (Exception e) {
        throw new RuntimeException ("Can't find class " + args[0] + " - " + e);
      }
      debug.fine ("Looking for " + cls);
    }
    // Build a vis to match the specified variables
    VarInfo[] vis = new VarInfo[args.length-1];
    for (int i = 0; i < vis.length; i++) {
      vis[i] = ppt.find_var_by_name (args[i+1]);
      if (vis[i] == null)
        parse_error (Fmt.spf ("Variable '%s' not found at ppt %s",
                              args[i+1], ppt.name()));
    }
    PptSlice slice = ppt.findSlice (vis);
    if (slice == null)
      return (false);

    // Look for a matching invariant in the slices invariant list
    for (Iterator i = slice.invs.iterator(); i.hasNext(); ) {
      Invariant inv = (Invariant) i.next();
      if (inv.getClass() == cls)
        return (true);
      if ((format != null) && format.equals (inv.format()))
        return (true);
      debug.fine (Fmt.spf ("trace %s: %s", inv.getClass(), inv.format()));
    }
    return (false);
  }

  private void parse_error (String msg) {

    fail (Fmt.spf ("Error parsing %s at line %s: %s",
                                fname, Fmt.i(fp.getLineNumber()), msg));
  }

}
