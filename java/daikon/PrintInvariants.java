package daikon;

import static java.nio.charset.StandardCharsets.UTF_8;
import static java.util.logging.Level.FINE;
import static java.util.logging.Level.INFO;

import daikon.FileIO.ParentRelation;
import daikon.PptRelation.PptRelationType;
import daikon.VarInfo.VarFlags;
import daikon.config.Configuration;
import daikon.inv.DiscardCode;
import daikon.inv.DiscardInfo;
import daikon.inv.Equality;
import daikon.inv.GuardingImplication;
import daikon.inv.Implication;
import daikon.inv.Invariant;
import daikon.inv.InvariantInfo;
import daikon.inv.Joiner;
import daikon.inv.OutputFormat;
import daikon.inv.filter.InvariantFilter;
import daikon.inv.filter.InvariantFilters;
import daikon.inv.filter.ObviousFilter;
import daikon.inv.filter.UnjustifiedFilter;
import daikon.split.PptSplitter;
import daikon.suppress.NIS;
import daikon.suppress.NISuppressionSet;
import gnu.getopt.Getopt;
import gnu.getopt.LongOpt;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OptionalDataException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.StreamCorruptedException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.TreeSet;
import java.util.logging.Logger;
import java.util.regex.Pattern;
import org.checkerframework.checker.mustcall.qual.Owning;
import org.checkerframework.checker.nullness.qual.KeyFor;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.nullness.qual.RequiresNonNull;
import org.plumelib.util.RegexUtil;
import org.plumelib.util.StringsPlume;

/**
 * PrintInvariants prints a set of invariants from a {@code .inv} file. For documentation, see
 * section "Printing Invariants" in the Daikon manual. Invoke the program as follows:
 *
 * <pre>java daikon.PrintInvariants [flags] inv-file</pre>
 *
 * For a list of cammand-line options, pass the "-h" flag to this program.
 */
public final class PrintInvariants {

  private PrintInvariants() {
    throw new Error("do not instantiate");
  }

  /**
   * See dkconfig_replace_prestate.
   *
   * <p>Resets the prestate expression mapping. This is done between printing of each different
   * program point.
   */
  public static void resetPrestateExpressions() {
    varNameCounter = 0;
    exprToVar = new HashMap<>();
  }

  // Used to create distinct variable names (see See dkconfig_replace_prestate).
  private static int varNameCounter = 0;

  // Maps prestate expressions to variable names (see See dkconfig_replace_prestate)>
  private static Map<String, String> exprToVar = new HashMap<>();

  /**
   * See dkconfig_replace_prestate.
   *
   * <p>Return the variable name corresponding to expr. Create a new varname and an expr &rarr;
   * varname mapping if there is not already one.
   */
  public static String addPrestateExpression(String expr) {
    if (expr == null) {
      throw new IllegalArgumentException(expr);
    }
    if (exprToVar.containsKey(expr)) {
      return exprToVar.get(expr);
    }
    String v = "v" + Integer.toString(varNameCounter++);
    exprToVar.put(expr, v);
    return v;
  }

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.

  /**
   * This option must be given with "--format Java" option.
   *
   * <p>Instead of outputting prestate expressions as "\old(E)" within an invariant, output a
   * variable name (e.g. `v1'). At the end of each program point, output the list of
   * variable-to-expression mappings. For example: with this option set to false, a program point
   * might print like this:
   *
   * <pre>
   * foo.bar.Bar(int):::EXIT
   * \old(capacity) == sizeof(this.theArray)
   * </pre>
   *
   * With the option set to true, it would print like this:
   *
   * <pre>
   * foo.bar.Bar(int):::EXIT
   * v0 == sizeof(this.theArray)
   * prestate assignment: v0=capacity
   * </pre>
   */
  public static boolean dkconfig_replace_prestate = true;

  /**
   * Print invariant classname with invariants in output of {@code format()} method. Normally used
   * only for debugging output rather than ordinary printing of invariants. This is especially
   * useful when two different invariants have the same printed representation.
   */
  public static boolean dkconfig_print_inv_class = false;

  /** If true, print all invariants without any filtering. */
  public static boolean dkconfig_print_all = false;

  /**
   * If true, print the total number of true invariants. This includes invariants that are redundant
   * and would normally not be printed or even created due to optimizations.
   */
  public static boolean dkconfig_true_inv_cnt = false;

  /**
   * If true, remove as many variables as possible that need to be indicated as 'post'. Post
   * variables occur when the subscript for a derived variable with an orig sequence is not orig.
   * For example: orig(a[post(i)]) An equivalent expression involving only orig variables is
   * substitued for the post variable when one exists.
   */
  public static boolean dkconfig_remove_post_vars = false;

  /**
   * In the new decl format, print array names as 'a[]' as opposed to 'a[..]' This creates names
   * that are more compatible with the old output. This option has no effect in the old decl format.
   */
  public static boolean dkconfig_old_array_names = true;

  /**
   * This enables a different way of treating static constant variables. They are not created into
   * invariants into slices. Instead, they are examined during print time. If a unary invariant
   * contains a value which matches the value of a static constant varible, the value will be
   * replaced by the name of the variable, "if it makes sense". For example, if there is a static
   * constant variable a = 1. And if there exists an invariant x &le; 1, x &le; a would be the
   * result printed.
   */
  public static boolean dkconfig_static_const_infer = false;

  /**
   * If false, don't print entry method program points for methods that override or implement
   * another method (i.e., entry program points that have a parent that is a method). Microsoft Code
   * Contracts does not allow contracts on such methods.
   */
  public static boolean dkconfig_print_implementer_entry_ppts = true;

  /** Main debug tracer for PrintInvariants (for things unrelated to printing). */
  public static final Logger debug = Logger.getLogger("daikon.PrintInvariants");

  /** Debug tracer for printing. */
  public static final Logger debugRepr = Logger.getLogger("daikon.PrintInvariants.repr");

  /** Debug tracer for printing. */
  public static final Logger debugPrint = Logger.getLogger("daikon.print");

  /** Debug tracer for printing modified variables in ESC/JML/DBC output. */
  public static final Logger debugPrintModified = Logger.getLogger("daikon.print.modified");

  /** Debug tracer for printing equality. */
  public static final Logger debugPrintEquality = Logger.getLogger("daikon.print.equality");

  /** Debug tracer for filtering. */
  public static final Logger debugFiltering = Logger.getLogger("daikon.filtering");

  /** Debug tracer for variable bound information. */
  public static final Logger debugBound = Logger.getLogger("daikon.bound");

  private static final String lineSep = Global.lineSep;

  /** Regular expression that ppts must match to be printed. */
  private static @Nullable Pattern ppt_regexp;

  /**
   * Switch for whether to print discarded Invariants or not, default is false. True iff {@code
   * --disc_reason} switch was supplied on the command line.
   */
  public static boolean print_discarded_invariants = false;

  /**
   * If true, then each invariant is printed using the current OutputFormat, but it's wrapped inside
   * xml tags, along with other information about the invariant. For example, if this switch is true
   * and if the output format is JAVA, and the invariant prints as "x == null", the results of
   * print_invariant would look something like:
   *
   * <pre>{@code
   * <INVINFO>
   * <INV> x == null </INV>
   * <SAMPLES> 100 </SAMPLES>
   * <DAIKON> x == null </DAIKON>
   * <DAIKONCLASS> daikon.inv.unary.scalar.NonZero </DAIKONCLASS>
   * <METHOD> foo() </METHOD>
   * </INVINFO>
   * }</pre>
   *
   * The above output is actually all in one line, although in this comment it's broken up into
   * multiple lines for clarity.
   *
   * <p>Note the extra information printed with the invariant: the number of samples from which the
   * invariant was derived, the daikon representation (i.e. the Daikon output format), the Java
   * class that the invariant corresponds to, and the method that the invariant belongs to ("null"
   * for object invariants).
   */
  public static boolean wrap_xml = false;

  /** {@code --output} flag to redirect output to a specified file. */
  private static String output_SWITCH = "output";

  /**
   * {@code --print_csharp_metadata} flag to print extra data for contracts when the format is
   * CSHARP_CONTRACT.
   */
  private static String print_csharp_metadata_SWITCH = "print_csharp_metadata";

  /** Stores the output file stream if --output is specified. Null means System.out. */
  private static @Owning @Nullable OutputStream out_stream = null;

  /** If true, print C# metadata. */
  private static boolean print_csharp_metadata = false;

  // Fields that will be used if the --disc_reason switch is used (in other
  // words, if print_discarded_invariants == true).  But they can be null
  // even in that case, which means to output a discard-reason for every
  // invariant.
  /** Output discard reasons for this class. If null, output discard reasons for all classes. */
  private static @MonotonicNonNull String discClass = null;

  /**
   * Comma-separated variable names. Output discard reasons if those are the variables. If null,
   * output discard reasons for all variable tuples.
   */
  private static @MonotonicNonNull String discVars = null;

  /**
   * Output discard reasons for this program point. If null, output discard reasons fro all program
   * points.
   */
  private static @MonotonicNonNull String discPpt = null;

  /** The usage message for this program. */
  private static String usage =
      StringsPlume.joinLines(
          "Usage: java daikon.PrintInvariants [OPTION]... FILE",
          "  -h, --" + Daikon.help_SWITCH,
          "      Display this usage message",
          "  --" + Daikon.format_SWITCH + " format_name",
          "      Write output in the given format.",
          "  --" + PrintInvariants.output_SWITCH + " output_file",
          "      Write output to the given file.",
          "  --" + Daikon.suppress_redundant_SWITCH,
          "      Suppress display of logically redundant invariants.",
          "  --" + Daikon.output_num_samples_SWITCH,
          "      Output number of values and samples for invariants and ppts; for debugging.",
          "  --" + Daikon.config_option_SWITCH + " config_var=val",
          "      Sets the specified configuration variable.  ",
          "  --" + Daikon.debugAll_SWITCH,
          "      Turns on all debug flags (voluminous output)",
          "  --" + Daikon.debug_SWITCH + " logger",
          "      Turns on the specified debug logger",
          "  --" + Daikon.track_SWITCH + " class<var1,var2,var3>@ppt",
          "      Print debug info on the specified invariant class, vars, and ppt",
          "  --" + Daikon.wrap_xml_SWITCH,
          "      Print extra info about invariants, and wrap in XML tags");

  /** See the documentation for this class. */
  public static void main(final String[] args)
      throws FileNotFoundException,
          StreamCorruptedException,
          OptionalDataException,
          IOException,
          ClassNotFoundException {
    try {
      mainHelper(args);
    } catch (Configuration.ConfigException e) {
      System.err.println(e.getMessage());
    } catch (Daikon.DaikonTerminationException e) {
      Daikon.handleDaikonTerminationException(e);
    }
  }

  /**
   * This does the work of {@link #main(String[])}, but it never calls System.exit, so it is
   * appropriate to be called progrmmatically.
   */
  public static void mainHelper(String[] args)
      throws FileNotFoundException,
          StreamCorruptedException,
          OptionalDataException,
          IOException,
          ClassNotFoundException {

    LongOpt[] longopts =
        new LongOpt[] {
          new LongOpt(Daikon.help_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
          new LongOpt(Daikon.format_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
          new LongOpt(Daikon.suppress_redundant_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
          new LongOpt(Daikon.output_num_samples_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
          new LongOpt(Daikon.config_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
          new LongOpt(Daikon.config_option_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
          new LongOpt(Daikon.debugAll_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
          new LongOpt(Daikon.debug_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
          new LongOpt(Daikon.ppt_regexp_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
          new LongOpt(Daikon.track_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
          new LongOpt(Daikon.wrap_xml_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
          new LongOpt(PrintInvariants.output_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
          new LongOpt(
              PrintInvariants.print_csharp_metadata_SWITCH, LongOpt.OPTIONAL_ARGUMENT, null, 0),
        };
    Getopt g = new Getopt("daikon.PrintInvariants", args, "h", longopts);
    int c;
    while ((c = g.getopt()) != -1) {
      switch (c) {
        case 0:
          // got a long option
          String option_name = longopts[g.getLongind()].getName();
          if (Daikon.help_SWITCH.equals(option_name)) {
            System.out.println(usage);
            throw new Daikon.NormalTermination();
          } else if (Daikon.ppt_regexp_SWITCH.equals(option_name)) {
            if (ppt_regexp != null) {
              throw new Error(
                  "multiple --"
                      + Daikon.ppt_regexp_SWITCH
                      + " regular expressions supplied on command line");
            }
            String regexp_string = Daikon.getOptarg(g);
            if (!RegexUtil.isRegex(regexp_string)) {
              throw new Daikon.UserError(
                  "Bad regexp "
                      + regexp_string
                      + " for "
                      + Daikon.ppt_regexp_SWITCH
                      + ": "
                      + RegexUtil.regexError(regexp_string));
            }
            regexp_string =
                RegexUtil.asRegex(regexp_string); // @SuppressWarnings("regex") // flow-sensitivity
            ppt_regexp = Pattern.compile(regexp_string);
          } else if (Daikon.disc_reason_SWITCH.equals(option_name)) {
            try {
              PrintInvariants.discReasonSetup(Daikon.getOptarg(g));
            } catch (IllegalArgumentException e) {
              assert e.getMessage() != null
                  : "@AssumeAssertion(nullness):  application invariant:  if discReasonSetup"
                      + " throws IllegalArgumentException, its message is non-null";
              throw new Daikon.UserError(e.getMessage());
            }
          } else if (Daikon.suppress_redundant_SWITCH.equals(option_name)) {
            Daikon.suppress_redundant_invariants_with_simplify = true;
          } else if (Daikon.format_SWITCH.equals(option_name)) {
            String format_name = Daikon.getOptarg(g);
            Daikon.output_format = OutputFormat.get(format_name);
            if (Daikon.output_format == null) {
              throw new Daikon.UserError("Unknown output format:  --format " + format_name);
            }
          } else if (PrintInvariants.print_csharp_metadata_SWITCH.equals(option_name)) {
            PrintInvariants.print_csharp_metadata = true;
          } else if (Daikon.output_num_samples_SWITCH.equals(option_name)) {
            Daikon.output_num_samples = true;
          } else if (Daikon.config_SWITCH.equals(option_name)) {
            String config_file = Daikon.getOptarg(g);
            try (InputStream stream = new FileInputStream(config_file)) {
              Configuration.getInstance().apply(stream);
            } catch (IOException e) {
              throw new RuntimeException("Could not open config file " + config_file);
            }
            break;
          } else if (PrintInvariants.output_SWITCH.equals(option_name)) {
            String output_file = Daikon.getOptarg(g);
            try {
              out_stream = new FileOutputStream(output_file);
            } catch (IOException e) {
              out_stream = null;
              throw new RuntimeException("Could not create output file " + output_file);
            }
            break;
          } else if (Daikon.config_option_SWITCH.equals(option_name)) {
            String item = Daikon.getOptarg(g);
            daikon.config.Configuration.getInstance().apply(item);
            break;
          } else if (Daikon.debugAll_SWITCH.equals(option_name)) {
            Global.debugAll = true;
          } else if (Daikon.debug_SWITCH.equals(option_name)) {
            LogHelper.setLevel(Daikon.getOptarg(g), FINE);
          } else if (Daikon.track_SWITCH.equals(option_name)) {
            LogHelper.setLevel("daikon.Debug", FINE);
            String error = Debug.add_track(Daikon.getOptarg(g));
            if (error != null) {
              throw new Daikon.UserError(
                  "Error parsing track argument '" + Daikon.getOptarg(g) + "' - " + error);
            }
          } else if (Daikon.wrap_xml_SWITCH.equals(option_name)) {
            wrap_xml = true;
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

    // Set up debug traces; note this comes after reading command line options.
    LogHelper.setupLogs(Global.debugAll ? FINE : INFO);

    validateGuardNulls();

    // The index of the first non-option argument -- the name of the file
    int fileIndex = g.getOptind();
    if (args.length - fileIndex != 1) {
      System.out.println(usage);
      throw new Daikon.UserError("Wrong number of arguments (expected 1)");
    }

    // Read in the invariants
    String filename = args[fileIndex];
    PptMap ppts =
        FileIO.read_serialized_pptmap(
            new File(filename), true // use saved config
            );
    // Setup the list of prototype invariants and initialize NIS suppressions
    Daikon.setup_proto_invs();
    Daikon.setup_NISuppression();

    // Make sure ppts' rep invariants hold
    ppts.repCheck();

    // If requested, just print the number of true invariants
    if (dkconfig_true_inv_cnt) {
      print_true_inv_cnt(ppts);
      return;
    }

    validateGuardNulls();
    //     if ((Daikon.dkconfig_guardNulls == "always") // interned
    //         || (Daikon.dkconfig_guardNulls == "missing")) { // interned
    //       Daikon.guardInvariants(ppts);
    //     }

    // Debug print the hierarchy is a more readable manner
    if (debug.isLoggable(FINE)) {
      debug.fine("Printing PPT Hierarchy");
      for (PptTopLevel my_ppt : ppts.pptIterable()) {
        if (my_ppt.parents.size() == 0) {
          my_ppt.debug_print_tree(debug, 0, null);
        }
      }
    }

    print_invariants(ppts);

    // Close the output stream if --output was specified.
    if (out_stream != null) {
      out_stream.flush();
      assert out_stream != null
          : "@AssumeAssertion(nullness): flush() does not affect any global variables";
      out_stream.close();
    }
  }

  /**
   * Prints out all the discardCodes and discardStrings of the Invariants that will not be printed
   * if the {@code --disc_reason} command-line option is used.
   */
  public static void print_reasons(PptMap ppts) {
    if (!print_discarded_invariants || Daikon.no_text_output) {
      return;
    }

    System.out.println();
    System.out.println("DISCARDED INVARIANTS:");
    // DiscReasonMap.debug(discPpt);

    // Makes things faster if a ppt is specified
    if (discPpt != null) {
      PptTopLevel ppt = ppts.get(discPpt);
      if (ppt == null) {
        System.out.println("No such ppt found: " + discPpt);
      } else {
        String toPrint = "";
        toPrint += print_reasons_from_ppt(ppt, ppts);

        StringTokenizer st = new StringTokenizer(toPrint, lineSep);
        if (st.countTokens() > 2) {
          System.out.print(toPrint);
        } else {
          String matching = "";
          if (discVars != null || discClass != null) {
            matching = " matching ";
          }
          System.out.println("No" + matching + "discarded Invariants found in " + ppt.name());
        }
      }
      return;
    }

    // Uses the custom comparator to get the Ppt objects in sorted order
    Comparator<PptTopLevel> comparator = new Ppt.NameComparator();
    TreeSet<PptTopLevel> ppts_sorted = new TreeSet<>(comparator);
    ppts_sorted.addAll(ppts.asCollection());

    // Iterate over the PptTopLevels in ppts
    for (PptTopLevel ppt : ppts_sorted) {
      StringBuilder toPrint = new StringBuilder();
      toPrint.append(print_reasons_from_ppt(ppt, ppts));

      // A little hack so that PptTopLevels without discarded Invariants of
      // interest don't get their names printed
      StringTokenizer st = new StringTokenizer(toPrint.toString(), lineSep);
      if (st.countTokens() > 2) {
        System.out.print(toPrint.toString());
      }
    }
  }

  /** Validate guardNulls config option. */
  public static void validateGuardNulls() {
    Daikon.dkconfig_guardNulls = Daikon.dkconfig_guardNulls.intern();
    // Complicated default!
    if (Daikon.dkconfig_guardNulls == "default") { // interned
      if (Daikon.output_format == OutputFormat.JML
          || Daikon.output_format == OutputFormat.ESCJAVA) {
        Daikon.dkconfig_guardNulls = "missing";
      } else {
        Daikon.dkconfig_guardNulls = "never";
      }
    }
    if (!((Daikon.dkconfig_guardNulls == "always") // interned
        || (Daikon.dkconfig_guardNulls == "never") // interned
        || (Daikon.dkconfig_guardNulls == "missing")) // interned
    ) {
      throw new Error(
          "Bad guardNulls config option \""
              + Daikon.dkconfig_guardNulls
              + "\", should be one of \"always\", \"never\", or \"missing\"");
    }
  }

  /** Add discard reasons for invariants that are filtered out. */
  private static void add_filter_reasons(PptTopLevel ppt) {
    InvariantFilters fi = InvariantFilters.defaultFilters();
    for (Iterator<Invariant> fullInvItor = ppt.invariants_iterator(); fullInvItor.hasNext(); ) {
      Invariant nextInv = fullInvItor.next();
      InvariantFilter varFilter = fi.shouldKeepVarFilters(nextInv);
      if (varFilter != null) {
        DiscReasonMap.put(nextInv, DiscardCode.findCode(varFilter), varFilter.getDescription());
      } else {
        InvariantFilter propFilter = fi.shouldKeepPropFilters(nextInv);
        if (propFilter != null) {
          DiscardInfo di;
          if (propFilter instanceof ObviousFilter) {
            di = nextInv.isObvious();
            assert di != null : "@AssumeAssertion(nullness)";
            if (Invariant.logOn()) {
              nextInv.log("DiscardInfo's stuff: %s%s%s", di.className(), lineSep, di.format());
            }
          } else if (propFilter instanceof UnjustifiedFilter) {
            di =
                new DiscardInfo(
                    nextInv,
                    DiscardCode.bad_confidence,
                    "Had confidence: " + nextInv.getConfidence());
          } else {
            di =
                new DiscardInfo(
                    nextInv, DiscardCode.findCode(propFilter), propFilter.getDescription());
          }
          DiscReasonMap.put(nextInv, di);
        }
      }
    }
  }

  private static String print_reasons_from_ppt(PptTopLevel ppt, PptMap ppts) {
    // Add all the reasons that would come from filtering to the DiscReasonMap
    add_filter_reasons(ppt);

    String toPrint = "";
    String dashes =
        "---------------------------------------------------------------------------" + lineSep;

    if (!(ppt instanceof PptConditional)) {
      toPrint +=
          "===========================================================================" + lineSep;
      toPrint += (ppt.name() + lineSep);
    }

    StringBuilder sb = new StringBuilder();
    for (DiscardInfo nextInfo :
        DiscReasonMap.returnMatches_from_ppt(new InvariantInfo(ppt.name(), discVars, discClass))) {
      sb.append(dashes + nextInfo.format() + lineSep);
    }

    // In case the user is interested in conditional ppt's
    if (Daikon.dkconfig_output_conditionals
        && (Daikon.output_format == OutputFormat.DAIKON
            || Daikon.output_format == OutputFormat.CSHARPCONTRACT)) {
      for (PptConditional pcond : ppt.cond_iterable()) {
        sb.append(print_reasons_from_ppt(pcond, ppts));
      }
    }
    return toPrint + sb.toString();
  }

  /**
   * Method used to setup fields if the {@code --disc_reason} command-line option is used if
   * (arg==null) then show all discarded Invariants, otherwise just show the ones specified in arg,
   * where arg = <em>class-name</em>&lt;<em>var1</em>,<em>var2</em>,...&gt;@<em>ppt.name</em> e.g.:
   * OneOf&lt;x&gt;@foo():::ENTER would only show OneOf Invariants that involve x at the program
   * point foo:::ENTER (any of the 3 params can be omitted, e.g. OneOf@foo:::ENTER)
   *
   * @throws IllegalArgumentException if arg is not of the proper syntax
   */
  public static void discReasonSetup(String arg) {
    print_discarded_invariants = true;
    usage =
        "Usage: <class-name><<var1>,<var2>,,,,>@<ppt.name()>"
            + lineSep
            + "or use --disc_reason \"all\" to show all discarded Invariants"
            + lineSep
            + "e.g.: OneOf<x>@foo():::ENTER"
            + lineSep;

    // Will print all discarded Invariants in this case
    if (arg == null || arg.length() == 0 || arg.equals("all")) {
      return;
    }

    // User wishes to specify a classname for the discarded Invariants of
    // interest
    char firstChar = arg.charAt(0);
    // This temp is used later as a way of "falling through" the cases
    String temp = arg;
    if (firstChar != '@' && firstChar != '<') {
      StringTokenizer splitArg = new StringTokenizer(arg, "@<");
      discClass = splitArg.nextToken();
      if ((arg.indexOf('<') != -1)
          && (arg.indexOf('@') != -1)
          && (arg.indexOf('@') < arg.indexOf('<')))
        temp = arg.substring(arg.indexOf('@')); // in case the pptname has a < in it
      else if (arg.indexOf('<') != -1) {
        temp = arg.substring(arg.indexOf('<'));
      } else if (arg.indexOf('@') != -1) {
        temp = arg.substring(arg.indexOf('@'));
      } else {
        return;
      }
    }
    firstChar = temp.charAt(0);

    // User wants to specify the variable names of interest
    if (firstChar == '<') {
      if (temp.length() < 2) {
        throw new IllegalArgumentException("Missing '>'" + lineSep + usage);
      }
      if (temp.indexOf('>', 1) == -1) {
        throw new IllegalArgumentException("Missing '>'" + lineSep + usage);
      }
      StringTokenizer parenTokens = new StringTokenizer(temp, "<>");
      if ((temp.indexOf('@') == -1 && parenTokens.countTokens() > 0)
          || (temp.indexOf('@') > -1 && parenTokens.countTokens() > 2))
        throw new IllegalArgumentException("Too many brackets" + lineSep + usage);
      StringTokenizer vars = new StringTokenizer(parenTokens.nextToken(), ",");
      if (vars.hasMoreTokens()) {
        discVars = vars.nextToken();
        while (vars.hasMoreTokens()) discVars += "," + vars.nextToken();
        // Get rid of *all* spaces since we know varnames can't have them
        discVars = discVars.replaceAll(" ", "");
      }
      if (temp.endsWith(">")) {
        return;
      } else if (temp.charAt(temp.indexOf('>') + 1) != '@') {
        throw new IllegalArgumentException("Must have '@' after '>'" + lineSep + usage);
      } else {
        temp = temp.substring(temp.indexOf('>') + 1);
      }
    }

    // If it made it this far, the first char of temp has to be '@'
    assert temp.charAt(0) == '@';
    if (temp.length() == 1) {
      throw new IllegalArgumentException("Must provide ppt name after '@'" + lineSep + usage);
    }
    discPpt = temp.substring(1);
  }

  // The following code is a little odd because it is trying to match the
  // output format of V2.  In V2, combined exit points are printed after
  // the original exit points (rather than before as they are following
  // the PptMap sort order).
  //
  // Also, V2 only prints out a single ppt when there is only one
  // exit point.  This seems correct.  Probably a better solution to
  // this would be to not create the combined exit point at all when there
  // is only a single exit.  Its done here instead so as not to futz with
  // the partial order stuff.
  //
  // All of this can (and should be) improved when V2 is dropped.

  @RequiresNonNull("FileIO.new_decl_format")
  public static void print_invariants(PptMap all_ppts) {

    if (out_stream == null) {
      out_stream = System.out;
    }
    PrintWriter pw =
        new PrintWriter(new BufferedWriter(new OutputStreamWriter(out_stream, UTF_8)), true);
    if (wrap_xml) {
      pw.println("<INVARIANTS>");
    }

    PptTopLevel combined_exit = null;
    boolean enable_exit_swap = true; // !Daikon.dkconfig_df_bottom_up;

    if (Daikon.no_text_output) {
      return;
    }

    // Retrieve Ppt objects in sorted order.  Put them in an array list
    // so that it is easier to look behind and ahead.
    PptTopLevel[] ppts = new PptTopLevel[all_ppts.size()];
    int ii = 0;
    for (PptTopLevel ppt : all_ppts.pptIterable()) {
      ppts[ii++] = ppt;
      // System.out.printf("considering ppt %s%n", ppts[ii-1].name());
    }

    for (int i = 0; i < ppts.length; i++) {
      PptTopLevel ppt = ppts[i];

      if (debug.isLoggable(FINE)) {
        debug.fine("Looking at point " + ppt.name());
      }

      // If this point is not an exit point, print out any retained combined
      // exit point
      if (enable_exit_swap && !ppt.ppt_name.isExitPoint()) {
        if (combined_exit != null) {
          print_invariants_maybe(combined_exit, pw, all_ppts);
        }
        combined_exit = null;
      }

      // Just cache the combined exit point for now, print it after the
      // EXITnn points.
      if (enable_exit_swap && ppt.ppt_name.isCombinedExitPoint()) {
        combined_exit = ppt;
        continue;
      }

      // If there is only one exit point, just show the combined one (since
      // the EXITnn point will be empty)  This is accomplished by skipping this
      // point if it is an EXITnn point and the previous point was a combined
      // exit point and the next one is not an EXITnn point.  But don't skip
      // any conditional ppts attached to the skipped ppt.
      if (enable_exit_swap && (i > 0) && ppt.ppt_name.isExitPoint()) {
        if (ppts[i - 1].ppt_name.isCombinedExitPoint()) {
          if (((i + 1) >= ppts.length) || !ppts[i + 1].ppt_name.isExitPoint()) {
            //             if (Daikon.dkconfig_output_conditionals
            //                 && Daikon.output_format == OutputFormat.DAIKON) {
            //               for (PptConditional pcond : ppt.cond_iterable()) {
            //                 print_invariants_maybe(pcond, pw, all_ppts);
            //               }
            //             }
            continue;
          }
        }
      }

      if (false) {
        VarInfo v = ppt.find_var_by_name("size(/map.tiles[])");
        System.out.printf("Found variable %s%n", v);
        if (v != null) {
          List<Invariant> invs = ppt.find_assignment_inv(v);
          System.out.printf("assignment invs = %s%n", invs);
        }
      }

      print_invariants_maybe(ppt, pw, all_ppts);
    }

    // print a last remaining combined exit point (if any)
    if (enable_exit_swap && combined_exit != null) {
      print_invariants_maybe(combined_exit, pw, all_ppts);
    }

    if (wrap_xml) {
      pw.println("</INVARIANTS>");
    }

    pw.flush();
  }

  /**
   * Print invariants for a single program point and its conditionals. Does no output if no samples
   * or no views.
   */
  @RequiresNonNull("FileIO.new_decl_format")
  public static void print_invariants_maybe(PptTopLevel ppt, PrintWriter out, PptMap all_ppts) {

    debugPrint.fine("Considering printing ppt " + ppt.name() + ", samples = " + ppt.num_samples());

    // Skip this ppt if it doesn't match ppt regular expression
    if ((ppt_regexp != null) && !ppt_regexp.matcher(ppt.name()).find()) {
      return;
    }

    // Skip this ppt if it is an ENTER ppt with a non-object parent
    if (!dkconfig_print_implementer_entry_ppts && ppt.is_enter()) {
      if (ppt.parent_relations != null) {
        for (ParentRelation parent : ppt.parent_relations) {
          if (parent != null) {
            // TODO:  This assertion is necessary because the KeyFor
            // Checker does not track @KeyFor except for Java Maps; it
            // should support KeyFor for PptMap which is not a subtype of
            // Map.  Alternately, PptMap could be made a subtype of Map.
            assert all_ppts.get(parent.parent_ppt_name) != null
                : "@AssumeAssertion(nullness)"; // parent.parent_ppt_name is a key in all_ppts
            if (parent.rel_type == PptRelationType.PARENT
                && all_ppts.get(parent.parent_ppt_name).is_enter()) {
              return;
            }
          }
        }
      }
    }

    // Skip this ppt if it never saw any samples.
    // (Maybe this test isn't even necessary, but will be subsumed by others,
    // as all the invariants will be unjustified.)
    if (ppt.num_samples() == 0) {
      if (debugPrint.isLoggable(FINE)) {
        debugPrint.fine("[No samples for " + ppt.name() + "]");
      }
      if (Daikon.output_num_samples) {
        out.println("[No samples for " + ppt.name() + "]");
      }
      return;
    }
    if ((ppt.numViews() == 0) && (ppt.joiner_view.invs.size() == 0)) {
      if (debugPrint.isLoggable(FINE)) {
        debugPrint.fine("[No views for " + ppt.name() + "]");
      }
      if (!(ppt instanceof PptConditional)) {
        // Presumably all the views that were originally there were deleted
        // because no invariants remained in any of them.
        if (Daikon.output_num_samples) {
          out.println("[No views for " + ppt.name() + "]");
        }
        return;
      }
    }

    if (false) {
      for (PptSlice slice : ppt.viewsAsCollection()) {
        System.out.printf("slice = %s, inv cnt = %d%n", slice, slice.invs.size());
      }
    }
    // out.println("This = " + this + ", Name = " + name + " = " + ppt_name);

    String DASHES = "===========================================================================";

    if (wrap_xml) {
      out.println("<!-- " + DASHES + " -->");
    } else {
      out.println(DASHES);
    }

    print_invariants(ppt, out, all_ppts);

    if (Daikon.dkconfig_output_conditionals
        && (Daikon.output_format == OutputFormat.DAIKON
            || Daikon.output_format == OutputFormat.CSHARPCONTRACT)) {
      for (PptConditional pcond : ppt.cond_iterable()) {
        print_invariants_maybe(pcond, out, all_ppts);
      }
    }
  }

  /**
   * Prints the program point name. If Daikon.output_num_samples is enabled, prints the number of
   * samples for the specified ppt. Also prints all of the variables for the ppt if
   * Daikon.output_num_samples is enabled or the format is ESCJAVA, JML, or DBCJAVA.
   */
  @RequiresNonNull("FileIO.new_decl_format")
  public static void print_sample_data(PptTopLevel ppt, PrintWriter out) {

    if (!wrap_xml) {
      out.print(ppt.name());
    } else {
      out.println("<PPT>");
      printXmlTagged(out, "PPTNAME", ppt.name());
    }
    if (Daikon.output_num_samples) {
      out.print("  ");
      if (!wrap_xml) {
        out.print(StringsPlume.nplural(ppt.num_samples(), "sample"));
      } else {
        printXmlTagged(out, "SAMPLES", ppt.num_samples());
      }
    }
    out.println();

    // Note that this code puts out the variable list using daikon formatting
    // for the names and not the output specific format.  It also includes
    // both front end and daikon derived variables which are probably not
    // appropriate
    if (Daikon.output_num_samples
        || (Daikon.output_format == OutputFormat.ESCJAVA)
        || (Daikon.output_format == OutputFormat.JML)
        || (Daikon.output_format == OutputFormat.DBCJAVA)) {
      out.print("    Variables:");
      for (int i = 0; i < ppt.var_infos.length; i++) {
        if (dkconfig_old_array_names && FileIO.new_decl_format) {
          out.print(" " + ppt.var_infos[i].name().replace("[..]", "[]"));
        } else {
          out.print(" " + ppt.var_infos[i].name());
        }
      }
      out.println();
    }
  }

  /** Prints all variables that were modified if the format is ESCJAVA or DBCJAVA. */
  public static void print_modified_vars(PptTopLevel ppt, PrintWriter out) {

    debugPrintModified.fine("Doing print_modified_vars for: " + ppt.name());

    List<VarInfo> modified_vars = new ArrayList<>();
    List<VarInfo> reassigned_parameters = new ArrayList<>();
    List<VarInfo> unmodified_vars = new ArrayList<>();

    // Loop through each variable at this ppt
    for (VarInfo vi : ppt.var_infos) {

      // Skip any orig variables
      if (vi.isPrestate()) {
        debugPrintModified.fine("  skipping " + vi.name() + ": is prestate");
        continue;
      }
      debugPrintModified.fine("  Considering var: " + vi.name());

      // Get the orig version of this variable.  If none is found then this
      // isn't a variable about which it makes sense to consider modifiability.
      VarInfo vi_orig = ppt.find_var_by_name(vi.prestate_name());
      if (vi_orig == null) {
        debugPrintModified.fine("  skipping " + vi.name() + ": no orig variable");
        continue;
      }

      // TODO: When we can get information from the decl file that
      // indicates if a variable is 'final', we should add such a test
      // here.  For now we use the equality invariant between the
      // variable and its orig variable to determine if it has been
      // modified.

      if (ppt.is_equal(vi, vi_orig)) {
        debugPrintModified.fine("  " + vi.name() + " = " + vi_orig.name());
        unmodified_vars.add(vi);
      } else { // variables are not equal
        if (vi.isParam()) {
          reassigned_parameters.add(vi);
        } else {
          modified_vars.add(vi);
        }
      }
    }

    if (Daikon.output_num_samples
        || (Daikon.output_format == OutputFormat.ESCJAVA)
        || (Daikon.output_format == OutputFormat.DBCJAVA)) {
      if (modified_vars.size() > 0) {
        out.print("      Modified variables:");
        for (VarInfo vi : modified_vars) {
          out.print(" " + vi.old_var_name());
        }
        out.println();
      }
      if (reassigned_parameters.size() > 0) {
        // out.print("      Reassigned parameters:");
        out.print("      Modified primitive arguments:");
        for (VarInfo vi : reassigned_parameters) {
          out.print(" " + vi.old_var_name());
        }
        out.println();
      }
      if (unmodified_vars.size() > 0) {
        out.print("      Unmodified variables:");
        for (VarInfo vi : unmodified_vars) {
          out.print(" " + vi.old_var_name());
        }
        out.println();
      }
    }

    // Remove non-variables from the assignable output
    if (Daikon.output_format == OutputFormat.ESCJAVA || Daikon.output_format == OutputFormat.JML) {
      List<VarInfo> mods = new ArrayList<>();
      for (VarInfo vi : modified_vars) {
        if (!vi.is_assignable_var()) {
          continue;
        }
        mods.add(vi);
      }

      // Print out the modifies/assignable list
      if (mods.size() > 0) {
        if (Daikon.output_format == OutputFormat.ESCJAVA) {
          out.print("modifies ");
        } else {
          out.print("assignable ");
        }
        int inserted = 0;
        for (VarInfo vi : mods) {
          String name = vi.old_var_name();
          if (!name.equals("this")) {
            if (inserted > 0) {
              out.print(", ");
            }
            if (name.endsWith("[]")) {
              name = name.substring(0, name.length() - 1) + "*]";
            }
            out.print(name);
            inserted++;
          }
        }
        out.println();
      }
    }
  }

  /** Count statistics (via Global) on variables (canonical, missing, etc.) */
  public static void count_global_stats(PptTopLevel ppt) {
    for (int i = 0; i < ppt.var_infos.length; i++) {
      if (ppt.var_infos[i].isDerived()) {
        Global.derived_variables++;
      }
    }
  }

  /** Prints the specified invariant to out. */
  @RequiresNonNull("FileIO.new_decl_format")
  public static void print_invariant(
      Invariant inv, PrintWriter out, int invCounter, PptTopLevel ppt) {

    String inv_rep = inv.format_using(Daikon.output_format);
    assert inv_rep != null : String.format("Null format (%s): %s", inv.getClass(), inv);

    if ((Daikon.output_format == OutputFormat.ESCJAVA) && !inv.isValidEscExpression()) {
      String class_name = ((inv instanceof Equality) ? "'equality'" : inv.getClass().getName());
      inv_rep =
          "warning: method "
              + class_name
              + ".format(OutputFormat:ESC/Java) needs to be implemented: "
              + inv.format();
    }

    // TODO: Remove once we revise OutputFormat
    if (Daikon.output_format == OutputFormat.JAVA) {
      // if there is a $pre string in the format, then it contains
      // the orig variable and should not be printed.
      if (inv_rep.indexOf("$pre") != -1) {
        return;
      }
    }

    // Addditional information about C# (C Sharp) contracts.
    if (Daikon.output_format == OutputFormat.CSHARPCONTRACT) {

      String csharp = inv.format_using(OutputFormat.CSHARPCONTRACT);
      String daikon = inv.format_using(OutputFormat.DAIKON);
      String invType = get_csharp_inv_type(inv);

      boolean postAndOrig = daikon.contains("post") && daikon.contains("orig");
      // boolean hasOnlyOneValue = daikon.contains("has only one value");

      if (!postAndOrig) {
        Set<String> sortedVariables = new HashSet<>();
        String sortedVars = "";
        Set<String> variables = new HashSet<>();
        String vars = "";

        get_csharp_invariant_variables(inv, sortedVariables, true);
        get_csharp_invariant_variables(inv, variables, false);

        for (String s : sortedVariables) {
          sortedVars += s + " ";
        }
        for (String s : variables) {
          vars += s + " ";
        }

        out.println(csharp);

        // Only print additional meta data information for Scout use if the flag is supplied.
        if (PrintInvariants.print_csharp_metadata) {
          out.println(daikon);
          out.println(invType);
          out.println(sortedVars);
          out.println(vars);
          out.println("*");
        }
      }
      return;
    }

    // Returns "" unless PrintInvariants.dkconfig_print_inv_class is set.
    inv_rep += inv.format_classname();

    if (Daikon.output_num_samples) {
      int inv_num_samps = inv.ppt.num_samples();
      String num_values_samples = "\t\t(" + StringsPlume.nplural(inv_num_samps, "sample") + ")";
      inv_rep += num_values_samples;
    }

    if (debugRepr.isLoggable(FINE)) {
      debugRepr.fine("Printing: [" + inv.repr_prob() + "]");
    } else if (debugPrint.isLoggable(FINE)) {
      debugPrint.fine("Printing: [" + inv.repr_prob() + "]");
    }

    if (dkconfig_old_array_names && FileIO.new_decl_format) {
      inv_rep = inv_rep.replace("[..]", "[]");
    }

    if (wrap_xml) {
      out.print("<INVINFO>");
      out.print(" ");
      printXmlTagged(out, "PARENT", inv.ppt.parent.ppt_name.getPoint());
      out.print(" ");
      printXmlTagged(out, "INV", inv_rep);
      out.print(" ");
      printXmlTagged(out, "SAMPLES", Integer.toString(inv.ppt.num_samples()));
      out.print(" ");
      printXmlTagged(out, "DAIKON", inv.format_using(OutputFormat.DAIKON));
      out.print(" ");
      printXmlTagged(out, "DAIKONCLASS", inv.getClass().toString());
      out.print(" ");
      printXmlTagged(out, "METHOD", inv.ppt.parent.ppt_name.getSignature());
      out.print(" ");
      out.print("</INVINFO>");
      out.println();
    } else {
      out.println(inv_rep);
    }
    if (debug.isLoggable(FINE)) {
      debug.fine(inv.repr());
    }
  }

  /**
   * Given an arbitrary string or object, prints an XML start tag, the string (or the object's
   * {@code toString()}) XML-quoted, and then the XML end tag, all on one line.
   *
   * <p>If the content is null, prints nothing.
   */
  private static void printXmlTagged(PrintWriter out, String tag, @Nullable Object content) {
    if (content == null) {
      return;
    }
    out.print("<");
    out.print(tag);
    out.print(">");
    String quoted =
        content
            .toString()
            .replace("&", "&amp;")
            .replace("<", "&lt;")
            .replace(">", "&gt;")
            .replace("\"", "&quot;")
            .replace("'", "&apos;");
    out.print(quoted);
    out.print("</");
    out.print(tag);
    out.print(">");
  }

  /**
   * Parses the variables from varInfo.
   *
   * @param varInfo the Daikon variable representation to parse
   * @param sort true to parse as a grouping variable, false to parse as a filtering variable
   * @return the parsed variable string
   */
  public static String parse_csharp_invariant_variable(VarInfo varInfo, boolean sort) {
    // Do not ever want to sort by old value.
    if (varInfo.postState != null) {
      return parse_csharp_invariant_variable(varInfo.postState, sort);
    }

    // Do not ever want to sort by function.
    if (varInfo.var_kind == VarInfo.VarKind.FUNCTION
        && !varInfo.var_flags.contains(VarFlags.IS_PROPERTY)) {
      // The assertion says that currently, Celeriac only visits a static
      // method when the method has a single parameter with the same type
      // as the declaring class. For example: string.NullOrEmpty(string
      // arg). In these cases, Celeriac will output arg as being the
      // enclosing var. The DECLs would look something like:
      // var fieldA
      // var string.NullOrEmpty(fieldA)
      //   parent fieldA
      assert (varInfo.enclosing_var != null)
          : "@AssumeAssertion(nullness)"; // application invariant; see comment above
      return parse_csharp_invariant_variable(varInfo.enclosing_var, sort);
    }

    if (!sort) {
      String r = varInfo.name_using(OutputFormat.CSHARPCONTRACT);
      int a = r.indexOf("[");
      int b = r.indexOf("]");
      if (a != 1 && b != 1 && a < b) {
        String middle = r.substring(a + 1, b);
        if (middle.equals("..")) {
          r = r.substring(0, a + 1) + ".." + r.substring(b, r.length());
        } else {
          r = r.substring(0, a + 1) + "i" + r.substring(b, r.length());
        }
      }

      return r;
    }

    if ((varInfo.var_kind == VarInfo.VarKind.FIELD || varInfo.var_kind == VarInfo.VarKind.FUNCTION)
        && varInfo.enclosing_var != null
        && !varInfo.enclosing_var.csharp_name().equals("this")) {

      return parse_csharp_invariant_variable(varInfo.enclosing_var, sort);
    } else {
      return varInfo.csharp_name();
    }
  }

  /**
   * Parses the variables from vars.
   *
   * @param vars an array of Daikon variable representations
   * @param variables the set to store the parsed variables
   * @param sort true to parse as group variables, false to parse as filtering variables
   */
  public static void get_csharp_invariant_variables(
      VarInfo[] vars, Set<String> variables, boolean sort) {
    for (VarInfo v : vars) {
      String add = parse_csharp_invariant_variable(v, sort);
      variables.add(add);
    }
  }

  /**
   * Parses the invariant variables of invariant and stores them in variables If group is true the
   * invariant's grouping variables are parsed (the variables which the invariant is grouped by in
   * the contract list view). If group is false the invariant's filtering variables are parsed (the
   * variables for which this invariant can be filtered by). In the case of implications, only
   * variables on the right side of the implication are parsed.
   *
   * @param invariant the invariant to parse
   * @param variables the set to store the parsed variables
   * @param group true to parse group variables, false to parse filtering variables
   */
  public static void get_csharp_invariant_variables(
      Invariant invariant, Set<String> variables, boolean group) {

    if (invariant instanceof GuardingImplication) {
      GuardingImplication gi = (GuardingImplication) invariant;
      get_csharp_invariant_variables(gi.right, variables, group);
    }
    if (invariant instanceof Implication) {
      Implication implication = ((Implication) invariant);
      get_csharp_invariant_variables(implication.right, variables, group);
    } else if (invariant instanceof Joiner) {
      Joiner joiner = ((Joiner) invariant);
      // Get the variables on each side of the joiner.
      get_csharp_invariant_variables(joiner.left, variables, group);
      get_csharp_invariant_variables(joiner.right, variables, group);
    } else {
      get_csharp_invariant_variables(invariant.ppt.var_infos, variables, group);
    }
  }

  /**
   * Gets the invariant type string (i.e. daikon.inv.binary.inv) of a Daikon invariant.
   *
   * @param invariant the Daikon invariant
   * @return the invariant type string of the invariant
   */
  public static String get_csharp_inv_type(Invariant invariant) {
    if (invariant instanceof GuardingImplication) {
      GuardingImplication gi = ((GuardingImplication) invariant);
      return get_csharp_inv_type(gi.right);
    } else if (invariant instanceof Implication) {
      Implication implication = ((Implication) invariant);
      return get_csharp_inv_type(implication.right);
    } else if (invariant instanceof Joiner) {
      Joiner joiner = ((Joiner) invariant);
      return get_csharp_inv_type(joiner.right);
    } else {
      String invType = invariant.getClass().toString();
      invType = invType.split(" ")[1];
      return invType;
    }
  }

  /**
   * Takes a list of Invariants and returns a list of Invariants that is sorted according to
   * PptTopLevel.icfp.
   *
   * @param invs a list of Invariants
   * @return a sorted list of the Invariants
   */
  public static List<Invariant> sort_invariant_list(List<Invariant> invs) {
    List<Invariant> result = new ArrayList<>(invs);
    result.sort(PptTopLevel.icfp);
    return result;
  }

  /** Print invariants for a single program point, once we know that this ppt is worth printing. */
  @RequiresNonNull("FileIO.new_decl_format")
  public static void print_invariants(PptTopLevel ppt, PrintWriter out, PptMap ppt_map) {

    // make names easier to read before printing
    ppt.simplify_variable_names();

    print_sample_data(ppt, out);
    print_modified_vars(ppt, out);

    // Dump some debugging info, if enabled
    if (debugPrint.isLoggable(FINE)) {
      debugPrint.fine("Variables for ppt " + ppt.name());
      for (int i = 0; i < ppt.var_infos.length; i++) {
        VarInfo vi = ppt.var_infos[i];
        // PptTopLevel ppt_tl = vi.ppt;
        // PptSlice slice1 = ppt_tl.findSlice(vi);
        debugPrint.fine("      " + vi.name());
      }
      debugPrint.fine("Equality set: ");
      debugPrint.fine((ppt.equality_view == null) ? "null" : ppt.equality_view.toString());
    }
    if (debugFiltering.isLoggable(FINE)) {
      debugFiltering.fine(
          "---------------------------------------------------------------------------");
      debugFiltering.fine(ppt.name());
    }

    // Count statistics (via Global) on variables (canonical, missing, etc.)
    count_global_stats(ppt);

    // I could instead sort the PptSlice objects, then sort the invariants
    // in each PptSlice.  That would be more efficient, but this is
    // probably not a bottleneck anyway.
    List<Invariant> invs_vector = new ArrayList<>(ppt.getInvariants());

    if (PptSplitter.debug.isLoggable(FINE)) {
      PptSplitter.debug.fine("Joiner View for ppt " + ppt.name);
      for (Invariant inv : ppt.joiner_view.invs) {
        PptSplitter.debug.fine("-- " + inv.format());
      }
    }

    if (debugBound.isLoggable(FINE)) {
      ppt.debug_unary_info(debugBound);
    }

    Invariant[] invs_array = invs_vector.toArray(new Invariant[0]);
    Arrays.sort(invs_array, PptTopLevel.icfp);

    Global.non_falsified_invariants += invs_array.length;

    List<Invariant> accepted_invariants = new ArrayList<>();

    for (int i = 0; i < invs_array.length; i++) {
      Invariant inv = invs_array[i];

      if (Invariant.logOn()) {
        inv.log("Considering Printing");
      }
      assert !(inv instanceof Equality);
      for (int j = 0; j < inv.ppt.var_infos.length; j++) {
        assert !inv.ppt.var_infos[j].missingOutOfBounds()
            : "var '" + inv.ppt.var_infos[j].name() + "' out of bounds in " + inv.format();
      }
      InvariantFilters fi = InvariantFilters.defaultFilters();

      boolean fi_accepted = true;
      {
        InvariantFilter filter_result = null;
        if (!dkconfig_print_all) {
          filter_result = fi.shouldKeep(inv);
          fi_accepted = (filter_result == null);
        }
        if ((inv instanceof Implication) && PptSplitter.debug.isLoggable(FINE)) {
          PptSplitter.debug.fine("filter result = " + filter_result + " for inv " + inv);
        }
      }

      if (Invariant.logOn()) {
        inv.log("Filtering, accepted = %s", fi_accepted);
      }

      // Never print the guarding predicates themselves, they should only
      // print as part of GuardingImplications
      if (fi_accepted && !inv.isGuardingPredicate) {
        Global.reported_invariants++;
        accepted_invariants.add(inv);
      } else {
        if (Invariant.logOn() || debugPrint.isLoggable(FINE)) {
          inv.log(
              debugPrint,
              "fi_accepted = "
                  + fi_accepted
                  + " inv.isGuardingPredicate = "
                  + inv.isGuardingPredicate
                  + " not printing "
                  + inv.repr());
        }
      }
    }

    accepted_invariants = InvariantFilters.addEqualityInvariants(accepted_invariants);

    if (debugFiltering.isLoggable(FINE)) {
      for (Invariant current_inv : accepted_invariants) {
        if (current_inv instanceof Equality) {
          debugFiltering.fine("Found Equality that says " + current_inv.format());
        }
      }
    }

    if (debugFiltering.isLoggable(FINE)) {
      for (int i = 0; i < ppt.var_infos.length; i++) {
        // VarInfo vi = ppt.var_infos[i];
        // ... print it
      }
    }
    finally_print_the_invariants(accepted_invariants, out, ppt);
    // if (ppt.constants != null) {
    //   ppt.constants.print_missing(out);
    // }
  }

  /** Does the actual printing of the invariants. */
  @RequiresNonNull("FileIO.new_decl_format")
  private static void finally_print_the_invariants(
      List<Invariant> invariants, PrintWriter out, PptTopLevel ppt) {
    // System.out.printf("Ppt %s%n", ppt.name());
    // for (VarInfo vi : ppt.var_infos)
    // System.out.printf("  var %s canbemissing = %b%n", vi, vi.canBeMissing);

    int index = 0;
    for (Invariant inv : invariants) {
      index++;
      Invariant guarded = inv.createGuardedInvariant(false);
      if (guarded != null) {
        inv = guarded;
      }
      print_invariant(inv, out, index, ppt);
    }
    if (wrap_xml) {
      out.println("</PPT>");
    }

    if (dkconfig_replace_prestate) {
      for (Map.Entry<@KeyFor("exprToVar") String, String> e : exprToVar.entrySet()) {
        out.println("prestate assignment: " + e.getValue() + "=" + e.getKey());
      }
      resetPrestateExpressions();
    }
  }

  /**
   * Prints all invariants for ternary slices (organized by slice) and all of the unary and binary
   * invariants over the same variables. The purpose of this is to look for possible
   * ni-suppressions. It's not intended as a normal output mechanism.
   */
  public static void print_all_ternary_invs(PptMap all_ppts) {

    // loop through each ppt
    for (PptTopLevel ppt : all_ppts.pptIterable()) {

      // if (ppt.num_samples() == 0)
      //  continue;

      // First figure out how many ternary invariants/slices there are
      int lt_cnt = 0;
      int slice_cnt = 0;
      int inv_cnt = 0;
      int total_slice_cnt = 0;
      int total_inv_cnt = 0;
      for (PptSlice slice : ppt.views_iterable()) {
        total_slice_cnt++;
        total_inv_cnt += slice.invs.size();
        if (slice.arity() != 3) {
          continue;
        }
        slice_cnt++;
        inv_cnt += slice.invs.size();
        for (Invariant inv : slice.invs) {
          if (inv.getClass().getName().indexOf("Ternary") > 0) {
            lt_cnt++;
          }
        }
      }

      System.out.println();
      System.out.printf(
          "%s - %d samples, %d slices, %d invariants (%d linearternary)%n",
          ppt.name(), ppt.num_samples(), slice_cnt, inv_cnt, lt_cnt);
      System.out.println(
          "    total slice count = " + total_slice_cnt + ", total_inv_cnt = " + total_inv_cnt);

      // Loop through each ternary slice
      for (PptSlice slice : ppt.views_iterable()) {
        if (slice.arity() != 3) {
          continue;
        }
        VarInfo[] vis = slice.var_infos;

        String var_str = "";
        for (int i = 0; i < vis.length; i++) {
          var_str += vis[i].name() + " ";
          if (ppt.is_constant(vis[i])) {
            var_str += "[" + Debug.toString(ppt.constants.constant_value(vis[i])) + "] ";
          }
        }
        System.out.printf("  Slice %s - %d invariants%n", var_str, slice.invs.size());

        // Loop through each invariant (skipping ternary ones)
        for (Invariant inv : slice.invs) {
          if (inv.getClass().getName().indexOf("Ternary") > 0) {
            continue;
          }

          // Check to see if the invariant should be suppressed
          String suppress = "";
          NISuppressionSet ss = inv.get_ni_suppressions();
          if ((ss != null) && ss.suppressed(slice)) {
            suppress = "ERROR: Should be suppressed by " + ss;
          }

          // Print the invariant
          System.out.printf(
              "    %s [%s] %s%n", inv.format(), inv.getClass().getSimpleName(), suppress);

          // Print all unary and binary invariants over the same variables
          for (int i = 0; i < vis.length; i++) {
            System.out.printf("      %s is %s%n", vis[i].name(), vis[i].file_rep_type);
            print_all_invs(ppt, vis[i], "      ");
          }
          print_all_invs(ppt, vis[0], vis[1], "      ");
          print_all_invs(ppt, vis[1], vis[2], "      ");
          print_all_invs(ppt, vis[0], vis[2], "      ");
        }
      }
    }
  }

  /** Prints all of the unary invariants over the specified variable. */
  public static void print_all_invs(PptTopLevel ppt, VarInfo vi, String indent) {
    String name = String.format("%s [%s]", vi.name(), vi.file_rep_type);
    if (ppt.is_missing(vi)) {
      System.out.printf("%s%s%n missing%n", indent, name);
    } else if (ppt.is_constant(vi)) {
      System.out.printf(
          "%s%s = %s%n", indent, name, Debug.toString(ppt.constants.constant_value(vi)));
    } else {
      PptSlice slice = ppt.findSlice(vi);
      if (slice != null) {
        print_all_invs(slice, indent);
      }

      if (slice == null) {
        System.out.printf("%s%s has %d values%n", indent, name, ppt.num_values(vi));
      }
    }
  }

  /** Prints all of the binary invariants over the specified variables. */
  public static void print_all_invs(PptTopLevel ppt, VarInfo v1, VarInfo v2, String indent) {
    // Get any invariants in the local slice
    PptSlice slice = ppt.findSlice(v1, v2);
    print_all_invs(slice, indent);
  }

  /** Prints all of the invariants in the specified slice. */
  public static void print_all_invs(@Nullable PptSlice slice, String indent) {

    if (slice == null) {
      return;
    }

    for (Invariant inv : slice.invs) {
      System.out.printf("%s%s [%s]%n", indent, inv.format(), inv.getClass().getSimpleName());
    }
  }

  /** Prints how many invariants are filtered by each filter. */
  public static void print_filter_stats(Logger log, PptTopLevel ppt, PptMap ppt_map) {

    boolean print_invs = false;

    List<Invariant> invs_vector = new ArrayList<>(ppt.getInvariants());
    Invariant[] invs_array = invs_vector.toArray(new Invariant[0]);

    // Not Map, because keys are nullable
    HashMap<@Nullable Class<? extends InvariantFilter>, Map<Class<? extends Invariant>, Integer>>
        filter_map =
            new LinkedHashMap<
                @Nullable Class<? extends InvariantFilter>,
                Map<Class<? extends Invariant>, Integer>>();

    if (print_invs) {
      debug.fine(ppt.name());
    }

    for (int i = 0; i < invs_array.length; i++) {
      Invariant inv = invs_array[i];

      InvariantFilters fi = InvariantFilters.defaultFilters();
      InvariantFilter filter = fi.shouldKeep(inv);
      Class<? extends InvariantFilter> filter_class = null;
      if (filter != null) {
        filter_class = filter.getClass();
      }
      Map<Class<? extends Invariant>, Integer> inv_map =
          filter_map.computeIfAbsent(filter_class, __ -> new LinkedHashMap<>());
      Integer cnt = inv_map.get(inv.getClass());
      if (cnt == null) {
        cnt = 1;
      } else {
        cnt = cnt.intValue() + 1;
      }
      inv_map.put(inv.getClass(), cnt);

      if (print_invs) {
        log.fine(" : " + filter_class + " : " + inv.format());
      }
    }

    log.fine(ppt.name() + ": " + invs_array.length);

    for (Map.Entry<
            @Nullable @KeyFor("filter_map") Class<? extends InvariantFilter>,
            Map<Class<? extends Invariant>, Integer>>
        entry : filter_map.entrySet()) {
      Class<? extends InvariantFilter> filter_class = entry.getKey();
      Map<Class<? extends Invariant>, Integer> inv_map = entry.getValue();
      int total = 0;
      for (Integer cnt : inv_map.values()) {
        total += cnt.intValue();
      }
      if (filter_class == null) {
        log.fine(" : Accepted Invariants : " + total);
      } else {
        log.fine(" : " + filter_class.getName() + ": " + total);
      }
      for (Map.Entry<@KeyFor("inv_map") Class<? extends Invariant>, Integer> entry2 :
          inv_map.entrySet()) {
        Class<? extends Invariant> inv_class = entry2.getKey();
        Integer cnt = entry2.getValue();
        log.fine(" : : " + inv_class.getName() + ": " + cnt.intValue());
      }
    }
  }

  @RequiresNonNull({"daikon.suppress.NIS.all_suppressions", "daikon.suppress.NIS.suppressor_map"})
  public static void print_true_inv_cnt(PptMap ppts) {

    // Count printable invariants
    long inv_cnt = 0;
    for (PptTopLevel ppt : ppts.pptIterable()) {
      for (Invariant inv : ppt.getInvariants()) {
        InvariantFilters fi = InvariantFilters.defaultFilters();
        if (fi.shouldKeep(inv) == null) {
          inv_cnt++;
        }
      }
    }
    System.out.printf("%d printable invariants%n", inv_cnt);

    // Count all of the stored invariants
    inv_cnt = 0;
    for (PptTopLevel ppt : ppts.pptIterable()) {
      inv_cnt += ppt.invariant_cnt();
    }
    System.out.printf("%d physical invariants%n", inv_cnt);

    // undo suppressions
    for (PptTopLevel ppt : ppts.pptIterable()) {
      NIS.create_suppressed_invs(ppt);
    }

    // Recount with suppressions removed
    inv_cnt = 0;
    for (PptTopLevel ppt : ppts.pptIterable()) {
      inv_cnt += ppt.invariant_cnt();
    }
    System.out.printf("%d invariants with suppressions removed%n", inv_cnt);

    // Count invariants again, adjusting the count for equality sets
    inv_cnt = 0;
    for (PptTopLevel ppt : ppts.pptIterable()) {
      List<Invariant> invs = ppt.getInvariants();
      for (Invariant inv : invs) {
        int cnt = 1;
        VarInfo[] vis = inv.ppt.var_infos;
        for (VarInfo vi : vis) {
          cnt = cnt * vi.get_equalitySet_size();
        }
        inv_cnt += cnt;
      }
    }
    System.out.printf("%d invariants with equality removed%n", inv_cnt);
  }
}
