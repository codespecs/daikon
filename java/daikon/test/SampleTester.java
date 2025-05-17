package daikon.test;

import static java.io.StreamTokenizer.TT_WORD;
import static java.nio.charset.StandardCharsets.UTF_8;
import static java.util.logging.Level.FINE;
import static java.util.logging.Level.INFO;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import daikon.Daikon;
import daikon.Debug;
import daikon.FileIO;
import daikon.Global;
import daikon.PptMap;
import daikon.PptSlice;
import daikon.PptTopLevel;
import daikon.PrintInvariants;
import daikon.ValueTuple;
import daikon.VarInfo;
import daikon.inv.Invariant;
import gnu.getopt.Getopt;
import gnu.getopt.LongOpt;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.StreamTokenizer;
import java.io.StringReader;
import java.io.UncheckedIOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.StringJoiner;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.signature.qual.ClassGetName;
import org.junit.Test;
import org.plumelib.util.IPair;
import org.plumelib.util.StringsPlume;

/**
 * This tests Daikon's state as samples are processed. A standard decl file specifies the ppts. A
 * sample input file specifies the samples and assertions that should be true at various points
 * while processing.
 *
 * <p>The input file format is documented in the developer manual.
 */
@SuppressWarnings({"nullness", "builder"}) // test code
public class SampleTester {

  public static final Logger debug = Logger.getLogger("daikon.test.SampleTester");
  public static final Logger debug_progress = Logger.getLogger("daikon.test.SampleTester.progress");

  static boolean first_decl = true;
  String fname;
  LineNumberReader fp;
  PptMap all_ppts;
  PptTopLevel ppt;
  VarInfo[] vars;

  /** The usage message for this program. */
  private static String usage =
      StringsPlume.joinLines(
          "Usage: java daikon.PrintInvariants [OPTION]... FILE",
          "  -h, --" + Daikon.help_SWITCH,
          "      Display this usage message",
          "  --" + Daikon.config_option_SWITCH,
          "      Specify a configuration option ",
          "  --" + Daikon.debug_SWITCH,
          "      Specify a logger to enable",
          "  --" + Daikon.track_SWITCH,
          "      Specify a class, varinfos, and ppt to debug track.",
          "      Format is class<var1,var2,var3>@ppt");

  public static void main(String[] args) throws IOException {

    LongOpt[] longopts =
        new LongOpt[] {
          new LongOpt(Daikon.config_option_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
          new LongOpt(Daikon.debugAll_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
          new LongOpt(Daikon.debug_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
          new LongOpt(Daikon.track_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
        };

    Getopt g = new Getopt("daikon.test.SampleTester", args, "h:", longopts);
    int c;
    while ((c = g.getopt()) != -1) {
      switch (c) {

        // long option
        case 0:
          String option_name = longopts[g.getLongind()].getName();
          if (Daikon.help_SWITCH.equals(option_name)) {
            System.out.println(usage);
            throw new Daikon.NormalTermination();

          } else if (Daikon.config_option_SWITCH.equals(option_name)) {
            String item = Daikon.getOptarg(g);
            daikon.config.Configuration.getInstance().apply(item);
            break;

          } else if (Daikon.debugAll_SWITCH.equals(option_name)) {
            Global.debugAll = true;

          } else if (Daikon.debug_SWITCH.equals(option_name)) {
            daikon.LogHelper.setLevel(Daikon.getOptarg(g), FINE);
          } else if (Daikon.track_SWITCH.equals(option_name)) {
            daikon.LogHelper.setLevel("daikon.Debug", FINE);
            String error = Debug.add_track(Daikon.getOptarg(g));
            if (error != null) {
              throw new Daikon.UserError(
                  "Error parsing track argument '" + Daikon.getOptarg(g) + "' - " + error);
            }
          } else {
            throw new RuntimeException("Unknown long option received: " + option_name);
          }
          break;

        case 'h':
          System.out.println(usage);
          throw new Daikon.NormalTermination();

        case '?':
          break; // getopt() already printed an error

        default:
          System.out.println("getopt() returned " + c);
          break;
      }
    }

    daikon.LogHelper.setupLogs(Global.debugAll ? FINE : INFO);

    new SampleTester().test_samples();
    System.out.println("Test Passes");
  }

  private static @Nullable String find_file(String fname) {

    URL input_file_location = ClassLoader.getSystemResource(fname);

    if (input_file_location == null) {
      return null;
    } else {
      return input_file_location.toExternalForm();
    }
  }

  /** Test {@link #splitData}. */
  @Test
  public void test_splitData() {
    assertEquals(2, splitData("\"foo@bar.com\" [\"foo@bar.com\"]").size());
    assertEquals(2, splitData("\"baz@a.b.edu\" [\"baz@a.b.edu\" \"baz@a.b.edu\"]").size());
    assertEquals(2, splitData("\"a@b.c.biz\" []").size());
    assertEquals(2, splitData("\"b@b.c.biz\" [\"a@b.c.biz\"]").size());
    assertEquals(2, splitData("\"c@b.c.biz\" [\"c@b.c.biz\" \"c@b.c.biz\" \"c@b.c.biz\"]").size());
    assertEquals(2, splitData("- []").size());
  }

  /**
   * This function is the actual function performed when this class is run through JUnit.
   *
   * @throws IOException if there in a problem with I/O
   */
  @Test
  public void test_samples() throws IOException {
    test_samples("SampleTester.commands");
    test_samples("SampleTester.commands_linear_ternary");
  }

  /**
   * This function is the actual function performed when this class is run through JUnit.
   *
   * @param commandFile the file containing the commands
   * @throws IOException if there in a problem with I/O
   */
  public void test_samples(String commandFile) throws IOException {

    FileIO.new_decl_format = null;

    try (InputStream commands = getClass().getResourceAsStream(commandFile)) {
      if (commands == null) {
        fail(
            "Missing input file: "
                + commandFile
                + " (Should be in daikon.test and it must be within the classpath)");
      }

      SampleTester ts = new SampleTester();
      ts.proc_sample_file(commands, commandFile);
    }
  }

  public void proc_sample_file(InputStream commands, String filename) throws IOException {

    if (PrintInvariants.dkconfig_print_inv_class) {
      System.out.println("Warning: turning off PrintInvariants.dkconfig_print_inv_class");
      PrintInvariants.dkconfig_print_inv_class = false;
    }

    // Enable expected invariants
    daikon.inv.ternary.threeScalar.FunctionBinary.dkconfig_enabled = true;
    daikon.inv.ternary.threeScalar.FunctionBinaryFloat.dkconfig_enabled = true;

    this.fname = filename;
    fp = new LineNumberReader(new InputStreamReader(commands, UTF_8));

    for (String line = fp.readLine(); line != null; line = fp.readLine()) {

      // Remove comments and skip blank lines
      line = line.replaceAll("#.*", "");
      line = line.trim();
      if (line.length() == 0) {
        continue;
      }

      // Get the line type and the remainder of the line
      String[] sa = line.split(": *", 2);
      if (sa.length != 2) parse_error("No line type specified");
      String ltype = sa[0].trim();
      String cmd = sa[1].trim();
      if (cmd.length() == 0) {
        parse_error("no command specified");
      }

      // Process the specified type of command
      if (ltype.equals("decl")) proc_decl(cmd);
      else if (ltype.equals("ppt")) proc_ppt(cmd);
      else if (ltype.equals("vars")) proc_vars(cmd);
      else if (ltype.equals("data")) proc_data(cmd, fp, filename);
      else if (ltype.equals("assert")) proc_assert(cmd);
      else {
        parse_error("unknown line type: " + ltype);
      }
    }
  }

  /** Reads in the specified decl file and sets all_ppts accordingly. */
  private void proc_decl(String decl_file) throws IOException {

    debug_progress.fine("Processing " + decl_file);

    // Read in the specified file
    Set<File> decl_files = new HashSet<>(1);
    String absolute_decl_file = find_file(decl_file);
    if (absolute_decl_file == null) {
      fail("Decl file " + decl_file + " not found.");
    }

    if (absolute_decl_file.startsWith("file:")) {
      absolute_decl_file = absolute_decl_file.substring(5);
    }

    decl_files.add(new File(absolute_decl_file));
    all_ppts = FileIO.read_declaration_files(decl_files);

    // Setup everything to run
    if (first_decl) {
      Daikon.setup_proto_invs();
      Daikon.setup_NISuppression();
      first_decl = false;
    }

    ppt = null;
  }

  /** Looks up the specified ppt name and set ppt accordingly. */
  private void proc_ppt(String ppt_name) {

    if (all_ppts == null) parse_error("decl file must be specified before ppt");
    ppt = all_ppts.get(ppt_name);
    if (ppt == null) {
      parse_error("ppt name " + ppt_name + " not found in decl file");
    }
    vars = null;
  }

  /**
   * Processes a variable list. Sets up the vars[] array to point to the matching variables in the
   * ppt. The ppt must have been previously specified.
   *
   * @param var_names the variable names, separated by spaces
   */
  private void proc_vars(String var_names) {

    if (ppt == null) {
      parse_error("ppt must be specified first");
    }

    // Variable names are separated by blanks
    String[] var_arr = var_names.split("  *");

    // The var array contains the variables in the ppt that correspond
    // to each name specified
    vars = new VarInfo[var_arr.length];

    // Loop through each variable name and find it in the variable list
    for (int i = 0; i < var_arr.length; i++) {
      String vname = var_arr[i];
      for (int j = 0; j < ppt.var_infos.length; j++) {
        if (vname.equals(ppt.var_infos[j].name())) vars[i] = ppt.var_infos[j];
      }
      if (vars[i] == null) parse_error("Variable " + vname + " not found in ppt " + ppt.name());
    }
  }

  /**
   * Processes a line of sample data. There should be one item of data for each previously specified
   * variable. Each data item is separated by spaces. Spaces cannot be included within a single item
   * (i.e., strings and arrays can't include spaces). Missing items are indicated with a dash (-).
   * Any variables not specifically mentioned in the variable string are set to missing as well.
   *
   * <p>Neither orig nor derived variables are added.
   */
  private void proc_data(String data, LineNumberReader reader, String filename) {

    if (vars == null) parse_error("vars must be specified before data");
    List<String> da = splitData(data);
    if (da.size() != vars.length)
      parse_error(String.format("%d vars but %d data elements: %s", vars.length, da.size(), data));
    debug_progress.fine("data: " + Debug.toString(da));

    VarInfo[] vis = ppt.var_infos;
    int vals_array_size = vis.length;
    Object[] vals = new Object[vals_array_size];
    int[] mods = new int[vals_array_size];

    // initially all variables are missing
    for (int i = 0; i < vals_array_size; i++) {
      vals[i] = null;
      mods[i] = ValueTuple.parseModified("2");
    }

    // Parse and enter the specified variables, - indicates a missing value
    for (int i = 0; i < vars.length; i++) {
      String val = da.get(i);
      if (val.equals("-")) {
        continue;
      }
      VarInfo vi = vars[i];
      vals[vi.value_index] = vi.rep_type.parse_value(val, reader, filename);
      mods[vi.value_index] = ValueTuple.parseModified("1");
    }

    ValueTuple vt = ValueTuple.makeUninterned(vals, mods);

    // We might want to add the following at some point.  Certainly the
    // derived variables.  The orig variables force us to deal with matching
    // enter and exit which I really don't want to do.  Perhaps we can always
    // give them the same value at enter and exit.  Both of these calls
    // are in FileIO

    // compute_orig_variables (ppt, vt.vals, vt.mods, nonce);
    // compute_derived_variables (ppt, vt.vals, vt.mods);

    // Causes interning
    vt = new ValueTuple(vt.vals, vt.mods);

    ppt.add_bottom_up(vt, 1);
  }

  /**
   * Splits a "data:" line of a SampleTester input file.
   *
   * @param s the "data:" line of a SampleTester input file
   * @return the components of a "data:" line of a SampleTester input file
   */
  private List<String> splitData(String s) {
    s = s.trim();
    List<String> result = new ArrayList<>();
    while (!s.isEmpty()) {
      IPair<String, String> p = readValueFromBeginning(s);
      result.add(p.first);
      s = p.second;
    }
    return result;
  }

  /**
   * Reads a value from the beginning of a string.
   *
   * @param s a string containing a sequence of Daikon dtrace values, separated by spaces
   * @return the first value and the remaining string
   */
  private IPair<String, String> readValueFromBeginning(String s) {
    s = s.trim();
    if (s.isEmpty()) {
      throw new Error("Cannot read a value from an empty string");
    }
    switch (s.charAt(0)) {
      case '"':
        for (int i = 1; i < s.length(); i++) {
          switch (s.charAt(i)) {
            case '"':
              return IPair.of(s.substring(0, i + 1), s.substring(i + 1).trim());
            case '\\':
              i++;
              break;
            default:
              break;
          }
        }
        throw new Error("Unterminated string: " + s);
      case '[':
        StringJoiner sj = new StringJoiner(" ", "[", "]");
        s = s.substring(1);
        while (!s.startsWith("]")) {
          IPair<String, String> p = readValueFromBeginning(s);
          sj.add(p.first);
          s = p.second;
        }
        return IPair.of(sj.toString(), s.substring(1).trim());
      default:
        Matcher m = spaceOrCloseBracket.matcher(s);
        int pos = m.find() ? m.start() : s.length();
        return IPair.of(s.substring(0, pos), s.substring(pos).trim());
    }
  }

  /** A regular expression that matches a space or a close square bracket. */
  Pattern spaceOrCloseBracket = Pattern.compile("[] ]");

  /**
   * Requires that the StreamTokenizer has just read a word. Returns that word.
   *
   * @param stok a StreamTokenizer that has just read a word
   * @return the word that the StreamTokenizer just read
   */
  private String readString(StreamTokenizer stok) {
    int ttype;
    try {
      ttype = stok.nextToken();
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
    if (ttype == TT_WORD || ttype == '"') {
      return stok.sval;
    } else if (ttype > 0) {
      return String.valueOf((char) ttype);
    } else {
      throw new Error("Expected string.  Got ttype = " + ttype);
    }
  }

  /** Processes a single assertion. If the assertion is false, throws an error. */
  private void proc_assert(String assertion) throws IOException {

    // Look for negation
    boolean negate = false;
    String assert_string = assertion;
    if (assertion.indexOf('!') == 0) {
      negate = true;
      assert_string = assert_string.substring(1);
    }

    boolean result = false;

    try {

      // Create a tokenizer over the assertion string
      StreamTokenizer stok = new StreamTokenizer(new StringReader(assert_string));
      stok.wordChars('_', '_');
      stok.commentChar('#');
      stok.quoteChar('"');

      int ttype;

      // Get the assertion name
      ttype = stok.nextToken();
      assertEquals(TT_WORD, ttype);
      String name = stok.sval;

      // Get the arguments (enclosed in parens, separated by commas)
      String delimiter = readString(stok);
      if (!"(".equals(delimiter)) {
        throw new Error("Expected \"(\", got: " + delimiter);
      }

      List<String> args = new ArrayList<>(10);
      do {
        String arg = readString(stok);
        delimiter = readString(stok);
        while (delimiter.equals("[") || delimiter.equals("]")) {
          arg += delimiter;
          delimiter = readString(stok);
        }
        args.add(arg);
      } while (delimiter.equals(","));
      if (!delimiter.equals(")")) {
        parse_error(String.format("%s found where ')' expected", delimiter));
      }

      // process the specific assertion
      if (name.equals("inv")) {
        result = proc_inv_assert(args);
        if (!result && !negate) {
          daikon.LogHelper.setLevel(debug, FINE);
          proc_inv_assert(args);
        }
      } else if (name.equals("show_invs")) {
        result = proc_show_invs_assert(args);
      } else if (name.equals("constant")) {
        result = proc_constant_assert(args);
      } else {
        parse_error("unknown assertion: " + name);
      }

      if (negate) {
        result = !result;
      }
    } catch (Throwable t) {
      throw new Error(
          String.format(
              "Problem in file %s, line %d, assertion: %s", fname, fp.getLineNumber(), assertion),
          t);
    }

    if (!result) {
      fail(
          String.format(
              "Assertion %s fails in file %s at line %d", assertion, fname, fp.getLineNumber()));
    }
  }

  /**
   * Processes an invariant existence assertion and returns true if it is found. The first argument
   * should be the invariant class. The remaining arguments are the variables. This needs to be
   * expanded to specify more information for invariants with state.
   */
  private boolean proc_inv_assert(List<String> args) {

    if ((args.size() < 2) || (args.size() > 4)) {
      parse_error("bad argument count (" + args.size() + ") for invariant assertion");
    }

    Class<?> cls = null;
    String format = null;

    // First argument is a classname or a quoted string
    String arg0 = args.get(0);
    try {
      debug.fine("Looking for " + arg0);
      @SuppressWarnings("signature") // user input (?); throws exception if fails
      @ClassGetName String arg0_cgn = arg0;
      cls = Class.forName(arg0_cgn);
    } catch (Exception e) {
      format = arg0;
      debug.fine(String.format("Looking for format: '%s' in ppt %s", format, ppt));
    }

    // Build a vis to match the specified variables
    VarInfo[] vis = new VarInfo[args.size() - 1];
    for (int i = 0; i < vis.length; i++) {
      vis[i] = ppt.find_var_by_name(args.get(i + 1));
      if (vis[i] == null) {
        parse_error(
            String.format("Variable '%s' not found at ppt %s", args.get(i + 1), ppt.name()));
      }
    }
    PptSlice slice = ppt.findSlice(vis);
    if (slice == null) {
      return false;
    }

    // Look for a matching invariant in the slices invariant list
    for (Invariant inv : slice.invs) {
      if (inv.getClass() == cls) {
        return true;
      }
      if ((format != null) && format.equals(inv.format())) {
        return true;
      }
      debug.fine(String.format("trace %s: '%s'", inv.getClass(), inv.format()));
    }
    return false;
  }

  /**
   * Prints all of the invariants in the given slice identified by the arguments (each of which
   * should be a valid variable name for this ppt).
   *
   * @param varNames a list of variable names
   * @return true
   */
  private boolean proc_show_invs_assert(List<String> varNames) {

    if ((varNames.size() < 1) || (varNames.size() > 3)) {
      parse_error("bad argument count (" + varNames.size() + ") for show_invs");
    }

    // Build a vis to match the specified variables
    VarInfo[] vis = new VarInfo[varNames.size()];
    for (int i = 0; i < vis.length; i++) {
      vis[i] = ppt.find_var_by_name(varNames.get(i));
      if (vis[i] == null) {
        parse_error(
            String.format("Variable '%s' not found at ppt %s", varNames.get(i), ppt.name()));
      }
    }
    PptSlice slice = ppt.findSlice(vis);
    if (slice == null) {
      System.out.println("No invariants found for vars: " + Arrays.toString(vis));
      return true;
    }

    // Diagnostics.
    if (false) {
      System.out.printf(
          "SampleTester %s show_invs: %d invariants%n", Arrays.toString(vis), slice.invs.size());
      for (Invariant inv : slice.invs) {
        System.out.printf("  %s: %s%n", inv.getClass(), inv.format());
      }
    }

    return true;
  }

  /**
   * The constant assertion returns true if all of its arguments are constants.
   *
   * @param varNames variables; must be non-empty
   * @return true if all of the given variables are constants
   */
  private boolean proc_constant_assert(List<String> varNames) {

    if (varNames.size() < 1) {
      parse_error("Must be at least one argument for constant assertion");
    }

    for (String arg : varNames) {
      VarInfo v = ppt.find_var_by_name(arg);
      if (v == null) {
        parse_error(String.format("Variable '%s' not found at ppt %s", arg, ppt.name()));
      }
      if (!ppt.constants.is_constant(v)) {
        return false;
      }
    }
    return true;
  }

  private void parse_error(String msg) {

    fail(String.format("Error parsing %s at line %d: %s", fname, fp.getLineNumber(), msg));
  }
}
