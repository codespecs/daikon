// Global variables

package daikon;

import java.util.*;
import com.oroinc.text.regex.*;

public final class Global {

  // There are some other variables in the Daikon class.  Probably move
  // them here eventually.


  // Don't permit this class to be instantiated
  private Global() { }

  ///////////////////////////////////////////////////////////////////////////
  /// Constants
  ///

  // Regular expressions
  public final static PatternCompiler regexp_compiler;
  public final static PatternMatcher regexp_matcher;
  public final static Pattern ws_regexp;

  static {
    regexp_compiler = new Perl5Compiler();
    regexp_matcher = new Perl5Matcher();
    try {
      ws_regexp = regexp_compiler.compile("[ \\t]+");
    } catch (Exception e) {
      throw new Error(e.toString());
    }
  }

  public final static Random random = new Random();


  ///////////////////////////////////////////////////////////////////////////
  /// Variables
  ///

  // Perhaps I shouldn't have anything in this category (ie, no global
  // variables)?


  ///////////////////////////////////////////////////////////////////////////
  /// Statistics-gathering
  ///

  // All these different variables is a little out of control, true.
  // Maybe turn it into a structure or an array of integers (which is
  // easier to output and to initialize, both by looping).

  public final static boolean output_statistics = true;
  public final static boolean debugStatistics = false;

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
    if (! output_statistics)
      return;

    System.out.println("Variables:");
    System.out.println("  non_canonical_variables = " + non_canonical_variables);
    System.out.println("  can_be_missing_variables = " + can_be_missing_variables);
    System.out.println("  canonical_variables = " + canonical_variables);
    System.out.println("  total variables = "
                       + (non_canonical_variables
                          + can_be_missing_variables
                          + canonical_variables));

    System.out.println("Derivation:");
    System.out.println("  derived_variables = " + derived_variables);
    System.out.println("  suppressed derived variables = " +
                       (nonsensical_suppressed_derived_variables
                        + tautological_suppressed_derived_variables));
    System.out.println("    nonsensical_suppressed_derived_variables = " + nonsensical_suppressed_derived_variables);
    System.out.println("    tautological_suppressed_derived_variables = " + tautological_suppressed_derived_variables);


    System.out.println("Inference:");
    System.out.println("Non-instantiated: "
                       + ((implied_noninstantiated_invariants
                           + subexact_noninstantiated_invariants)
                          + (implied_false_noninstantiated_invariants
                             + partially_implied_invariants)));
    System.out.println("  true = " + (implied_noninstantiated_invariants
                                      + subexact_noninstantiated_invariants));
    System.out.println("    implied_noninstantiated_invariants = " + implied_noninstantiated_invariants);
    System.out.println("    subexact_noninstantiated_invariants = " + subexact_noninstantiated_invariants);
    System.out.println("  false = " + (implied_false_noninstantiated_invariants
                                       + partially_implied_invariants));
    System.out.println("    implied_false_noninstantiated_invariants = " + implied_false_noninstantiated_invariants);
    System.out.println("    partially_implied_invariants = " + partially_implied_invariants);
    System.out.println("Instantiated: " + instantiated_invariants
                       + " = "
                       + (falsified_invariants + non_falsified_invariants));
    System.out.println("  falsified_invariants = " + falsified_invariants);
    System.out.println("  non_falsified_invariants = " + non_falsified_invariants
                       + " = "
                       + ((too_few_samples_invariants
                          + unjustified_invariants)
                       + (non_canonical_invariants
                          + obvious_invariants)
                       + reported_invariants));
    System.out.println("    unjustified = " + (too_few_samples_invariants
                                               + unjustified_invariants));
    System.out.println("      too_few_samples_invariants = " + too_few_samples_invariants);
    System.out.println("      unjustified_invariants = " + unjustified_invariants);
    System.out.println("    implied = " + (non_canonical_invariants
                                           + obvious_invariants));
    System.out.println("      non_canonical_invariants = " + non_canonical_invariants);
    System.out.println("      obvious_invariants = " + obvious_invariants);
    System.out.println("    reported_invariants = " + reported_invariants);
  }


  ///////////////////////////////////////////////////////////////////////////
  /// Debugging
  ///

  public final static boolean debugRead = false;
  public final static boolean debugPptTopLevel = false;
  public final static boolean debugDerive = false;
  public final static boolean debugInfer = false;
  public final static boolean debugPptSplit = false;
  public final static boolean debugPrintInvariants = false;
  public final static boolean debugPptSlice = false;
  public final static boolean debugPptSliceGeneric = false;
  public final static boolean debugPptSliceSpecific = false;

  // public final static boolean debugRead = true;
  // public final static boolean debugPptTopLevel = true;
  // public final static boolean debugDerive = true;
  // public final static boolean debugInfer = true;
  // public final static boolean debugPptSplit = true;
  // public final static boolean debugPrintInvariants = true;
  // public final static boolean debugPptSlice = true;
  // public final static boolean debugPptSliceGeneric = true;
  // public final static boolean debugPptSliceSpecific = true;

  // Used only if debugPptSliceSpecific is set.
  // Variables must appear in the correct order.
  public final static String[][] debuggedPptSliceSpecific
    = { { "arg", "orig(arg)" },
        { "inCentralCommandMediator.mAspectLegend", "inCentralCommandMediator.mAspectBrowser" },
        { "list.class" },
        { "this.theArray[orig(this.front)+1..]", "this.theArray[this.front..]" },
        { "this.theArray[this.front..]", "this.theArray[orig(this.front)+1..]" },
    };

  // This may be expensive and so should only be called infrequently.
  public final static boolean isDebuggedPptSlice(PptSlice slice) {
    String[][] dpss = debuggedPptSliceSpecific;
  outer:
    for (int i=0; i<dpss.length; i++) {
      if (dpss[i].length == slice.arity) {
        for (int j=0; j<slice.arity; j++) {
          if (dpss[i][j] != slice.var_infos[j].name)
            continue outer;
        }
        return true;
      }
    }
    return false;
  }


}
