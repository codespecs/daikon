// Global variables

package daikon;

import static java.util.logging.Level.FINE;
import static java.util.logging.Level.INFO;

import daikon.inv.Invariant;
import daikon.simplify.Lemma;
import daikon.split.Splitter;
import daikon.split.SplitterObject;
import java.io.PrintWriter;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.logging.Logger;
import java.util.regex.Pattern;
import org.checkerframework.checker.mustcall.qual.Owning;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;
import org.plumelib.util.FuzzyFloat;

public final class Global {

  // There are some other variables in the Daikon class.  Probably move
  // them here eventually.

  static {
    daikon.config.Configuration.getInstance();
  }

  // Don't permit this class to be instantiated
  private Global() {}

  ///////////////////////////////////////////////////////////////////////////
  /// Constants
  ///

  /** The system-specific line separator. */
  public static final String lineSep = System.lineSeparator();

  /** Matches white space. */
  public static final java.util.regex.Pattern ws_regexp = Pattern.compile("\\s+");

  /** The source of randomness. */
  public static final Random random = new Random();

  /** An empty Invariant array, useful for passing to calls to List.toArray(). */
  public static final Invariant[] emptyInvariantArray = new Invariant[0];

  /** An empty Lemma array, useful for passing to calls to List.toArray(). */
  public static final Lemma[] emptyLemmaArray = new Lemma[0];

  /** An empty PptSlice array, useful for passing to calls to List.toArray(). */
  public static final PptSlice[] emptyPptSliceArray = new PptSlice[0];

  /** An empty Splitter array, useful for passing to calls to List.toArray(). */
  public static final Splitter[] emptySplitterArray = new Splitter[0];

  /** An empty SplitterObject array, useful for passing to calls to List.toArray(). */
  public static final SplitterObject[] emptySplitterObjectArray = new SplitterObject[0];

  /** An empty String array, useful for passing to calls to List.toArray(). */
  public static final String[] emptyStringArray = new String[0];

  /** An empty VarInfo array, useful for passing to calls to List.toArray(). */
  public static final VarInfo[] emptyVarInfoArray = new VarInfo[0];

  ///////////////////////////////////////////////////////////////////////////
  /// Variables
  ///

  // Perhaps I shouldn't have anything in this category (i.e., no global
  // variables)?

  ///////////////////////////////////////////////////////////////////////////
  /// Statistics-gathering
  ///

  // All these different variables is a little out of control, true.
  // Maybe turn it into a structure or an array of integers (which is
  // easier to output and to initialize, both by looping).

  public static final boolean output_statistics = true;

  /// Invariant inference or variable derivation
  // These I will compute from a final postpass over each Ppt.
  public static int non_canonical_variables = 0;
  public static int can_be_missing_variables = 0;
  public static int canonical_variables = 0;

  /// Variable derivation
  public static int nonsensical_suppressed_derived_variables = 0;
  public static int tautological_suppressed_derived_variables = 0;
  // Can be set by a postpass.  (Might be instructive to compute on the
  // fly, too, to see what I missed.)
  public static int derived_variables = 0;

  /// Invariant inference
  public static int implied_noninstantiated_invariants = 0;
  public static int implied_false_noninstantiated_invariants = 0;
  public static int subexact_noninstantiated_invariants = 0;
  // These also appear in falsified_invariants or non_falsified_invariants;
  // they shouldn't be added to other things.
  public static int partially_implied_invariants = 0;
  // instantiated_invariants == falsified_invariants + non_falsified_invariants
  public static int instantiated_invariants = 0;
  public static int falsified_invariants = 0;
  // non_falsified_invariants should be the sum of all the below
  public static int non_falsified_invariants = 0;
  public static int too_few_samples_invariants = 0;
  public static int non_canonical_invariants = 0;
  public static int obvious_invariants = 0;
  public static int unjustified_invariants = 0;
  public static int reported_invariants = 0;

  public static void output_statistics() {
    if (!output_statistics) {
      return;
    }

    System.out.println(
        "===========================================================================");
    System.out.println("Variables:");
    System.out.println("  non_canonical_variables = " + non_canonical_variables);
    System.out.println("  can_be_missing_variables = " + can_be_missing_variables);
    System.out.println("  canonical_variables = " + canonical_variables);
    System.out.println(
        "  total variables = "
            + (non_canonical_variables + can_be_missing_variables + canonical_variables));

    System.out.println("Derivation:");
    System.out.println("  derived_variables = " + derived_variables);
    System.out.println(
        "  suppressed derived variables = "
            + (nonsensical_suppressed_derived_variables
                + tautological_suppressed_derived_variables));
    System.out.println(
        "    nonsensical_suppressed_derived_variables = "
            + nonsensical_suppressed_derived_variables);
    System.out.println(
        "    tautological_suppressed_derived_variables = "
            + tautological_suppressed_derived_variables);

    System.out.println("Inference:");
    System.out.println(
        "Non-instantiated: "
            + ((implied_noninstantiated_invariants + subexact_noninstantiated_invariants)
                + (implied_false_noninstantiated_invariants + partially_implied_invariants)));
    System.out.println(
        "  true = " + (implied_noninstantiated_invariants + subexact_noninstantiated_invariants));
    System.out.println(
        "    implied_noninstantiated_invariants = " + implied_noninstantiated_invariants);
    System.out.println(
        "    subexact_noninstantiated_invariants = " + subexact_noninstantiated_invariants);
    System.out.println(
        "  false = " + (implied_false_noninstantiated_invariants + partially_implied_invariants));
    System.out.println(
        "    implied_false_noninstantiated_invariants = "
            + implied_false_noninstantiated_invariants);
    System.out.println("    partially_implied_invariants = " + partially_implied_invariants);
    System.out.println(
        "Instantiated: "
            + instantiated_invariants
            + " = "
            + (falsified_invariants + non_falsified_invariants));
    System.out.println("  falsified_invariants = " + falsified_invariants);
    System.out.println(
        "  non_falsified_invariants = "
            + non_falsified_invariants
            + " = "
            + ((too_few_samples_invariants + unjustified_invariants)
                + (non_canonical_invariants + obvious_invariants)
                + reported_invariants));
    System.out.println(
        "    unjustified = " + (too_few_samples_invariants + unjustified_invariants));
    System.out.println("      too_few_samples_invariants = " + too_few_samples_invariants);
    System.out.println("      unjustified_invariants = " + unjustified_invariants);
    System.out.println("    implied = " + (non_canonical_invariants + obvious_invariants));
    System.out.println("      non_canonical_invariants = " + non_canonical_invariants);
    System.out.println("      obvious_invariants = " + obvious_invariants);
    System.out.println("    reported_invariants = " + reported_invariants);
  }

  ///////////////////////////////////////////////////////////////////////////
  /// Debugging
  /// Anything that's commented in the false section is now implemented
  /// via the logger.

  public static boolean debugAll = false;

  static {
    // Set up debug traces.
    // Better to do this here than in each separate program.
    LogHelper.setupLogs(debugAll ? FINE : INFO);
  }

  /** Debug tracer for debugging statistics output. */
  public static final Logger debugStatistics = Logger.getLogger("daikon.statistics");

  /** Debug tracer for debugging Simplify output. */
  public static final Logger debugSimplify = Logger.getLogger("daikon.simplify");

  /** Debug tracer for debugging derived vars. */
  public static Logger debugDerive = Logger.getLogger("daikon.derive");

  /** Debug tracer for debugging splitting. */
  public static Logger debugSplit = Logger.getLogger("daikon.split");

  /** Debug tracer for debugging general invariant inference. */
  public static Logger debugInfer = Logger.getLogger("daikon.infer");

  /** Debug tracer for debugging invariant suppression. */
  public static Logger debugSuppress = Logger.getLogger("daikon.suppress");

  /** Debug tracer for debugging invariant suppression by using parameters. */
  public static Logger debugSuppressParam = Logger.getLogger("daikon.suppress.param");

  /** Debug tracer for debugging invariant printing. */
  public static Logger debugPrint = Logger.getLogger("daikon.print");

  /** If true, print logging information about printing of dtrace files. */
  public static final boolean debugPrintDtrace = false;

  /** Used only if debugPrintDtrace is true. Users need not set this. */
  public static @Owning @MonotonicNonNull PrintWriter dtraceWriter = null;

  // Global Fuzzy Float comparator to use
  public static FuzzyFloat fuzzy = new FuzzyFloat();

  /* Map of statistics for each ppt. */
  public static Map<PptTopLevel, List<PptTopLevel.Stats>> stats_map = new LinkedHashMap<>();
}
