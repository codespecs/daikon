package daikon.inv;

import static daikon.tools.nullness.NullnessUtil.castNonNullDeep;

import daikon.Daikon;
import daikon.Debug;
import daikon.FileIO;
import daikon.PptSlice;
import daikon.PptSlice1;
import daikon.PptSlice2;
import daikon.PrintInvariants;
import daikon.ValueTuple;
import daikon.VarComparability;
import daikon.VarComparabilityImplicit;
import daikon.VarInfo;
import daikon.inv.binary.BinaryInvariant;
import daikon.inv.filter.InvariantFilters;
import daikon.inv.ternary.TernaryInvariant;
import daikon.inv.unary.OneOf;
import daikon.inv.unary.UnaryInvariant;
import daikon.simplify.LemmaStack;
import daikon.simplify.SimpUtil;
import daikon.suppress.NISuppressionSet;
import java.io.Serializable;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.StringJoiner;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;
import org.checkerframework.checker.formatter.qual.FormatMethod;
import org.checkerframework.checker.initialization.qual.UnderInitialization;
import org.checkerframework.checker.initialization.qual.UnknownInitialization;
import org.checkerframework.checker.interning.qual.UsesObjectEquals;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.EnsuresNonNullIf;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;
import org.checkerframework.framework.qual.Unused;
import org.plumelib.util.ArraysPlume;
import org.plumelib.util.CollectionsPlume;
import org.plumelib.util.MathPlume;
import typequals.prototype.qual.NonPrototype;
import typequals.prototype.qual.Prototype;

/**
 * Base implementation for Invariant objects. Intended to be subclassed but not to be directly
 * instantiated. Rules/assumptions for invariants:
 *
 * <p>For each program point's set of VarInfos, there exists no more than one invariant of its type.
 * For example, between variables a and b at PptTopLevel T, there will not be two instances of
 * invariant I(a, b).
 */
@UsesObjectEquals
@Prototype
public abstract class Invariant implements Serializable, Cloneable // but don't YOU clone it
{
  static final long serialVersionUID = 20040921L;

  /** General debug tracer. */
  public static final Logger debug = Logger.getLogger("daikon.inv.Invariant");

  /** Debug tracer for printing invariants. */
  public static final Logger debugPrint = Logger.getLogger("daikon.print");

  /** Debug tracer for invariant flow. */
  public static final Logger debugFlow = Logger.getLogger("daikon.flow.flow");

  /** Debug tracer for printing equality invariants. */
  public static final Logger debugPrintEquality = Logger.getLogger("daikon.print.equality");

  /** Debug tracer for isWorthPrinting() checks. */
  public static final Logger debugIsWorthPrinting =
      Logger.getLogger("daikon.print.isWorthPrinting");

  /** Debug tracer for guarding. */
  public static final Logger debugGuarding = Logger.getLogger("daikon.guard");

  /** Debug tracer for isObvious checks. */
  public static final Logger debugIsObvious = Logger.getLogger("daikon.inv.Invariant.isObvious");

  /**
   * Floating-point number between 0 and 1. Invariants are displayed only if the confidence that the
   * invariant did not occur by chance is greater than this. (May also be set via the {@code
   * --conf_limit} command-line option to Daikon; refer to manual.)
   */
  public static double dkconfig_confidence_limit = .99;

  /**
   * A boolean value. If true, Daikon's Simplify output (printed when the {@code --format simplify}
   * flag is enabled, and used internally by {@code --suppress_redundant}) will include new
   * predicates representing some complex relationships in invariants, such as lexical ordering
   * among sequences. If false, some complex relationships will appear in the output as complex
   * quantified formulas, while others will not appear at all. When enabled, Simplify may be able to
   * make more inferences, allowing {@code --suppress_redundant} to suppress more redundant
   * invariants, but Simplify may also run more slowly.
   */
  public static boolean dkconfig_simplify_define_predicates = false;

  /**
   * Floating-point number between 0 and 0.1, representing the maximum relative difference between
   * two floats for fuzzy comparisons. Larger values will result in floats that are relatively
   * farther apart being treated as equal. A value of 0 essentially disables fuzzy comparisons.
   * Specifically, if {@code abs(1 - f1/f2)} is less than or equal to this value, then the two
   * doubles ({@code f1} and {@code f2}) will be treated as equal by Daikon.
   */
  public static double dkconfig_fuzzy_ratio = 0.0001;

  /** The default for dkconfig_enabled in each subclass of Invariant. */
  public static boolean invariantEnabledDefault = true;

  /**
   * The program point for this invariant; includes values, number of samples, VarInfos, etc. Can be
   * null for a "prototype" invariant.
   */
  @Unused(when = Prototype.class)
  public PptSlice ppt;

  // Has to be public so wrappers can read it.
  /**
   * True exactly if the invariant has been falsified: it is guaranteed never to hold (and should be
   * either in the process of being destroyed or about to be destroyed). This should never be set
   * directly; instead, call destroy().
   */
  protected boolean falsified = false;

  // Whether an invariant is a guarding predicate, that is, creately solely
  // for the purpose of ensuring invariants with variables that can be
  // missing do not cause exceptions when tested.  If this is true, then
  // the invariant itself does not hold over the observed data.
  public boolean isGuardingPredicate = false;

  /**
   * The probability that this could have happened by chance alone. <br>
   * 1 = could never have happened by chance; that is, we are fully confident that this invariant is
   * a real invariant
   */
  public static final double CONFIDENCE_JUSTIFIED = 1;

  /**
   * (0..1) = greater to lesser likelihood of coincidence <br>
   * 0 = must have happened by chance
   */
  public static final double CONFIDENCE_UNJUSTIFIED = 0;

  /** -1 = delete this invariant; we know it's not true */
  public static final double CONFIDENCE_NEVER = -1;

  /**
   * The probability that this could have happened by chance alone. <br>
   * 0 = could never have happened by chance; that is, we are fully confident that this invariant is
   * a real invariant
   */
  public static final double PROBABILITY_JUSTIFIED = 0;

  /**
   * (0..1) = lesser to greater likelihood of coincidence <br>
   * 1 = must have happened by chance
   */
  public static final double PROBABILITY_UNJUSTIFIED = 1;

  /** 3 = delete this invariant; we know it's not true */
  public static final double PROBABILITY_NEVER = 3;

  /**
   * Return Invariant.CONFIDENCE_JUSTIFIED if x&ge;goal. Return Invariant.CONFIDENCE_UNJUSTIFIED if
   * x&le;1. For intermediate inputs, the result gives confidence that grades between the two
   * extremes. See the discussion of gradual vs. sudden confidence transitions.
   *
   * @param x the greater value
   * @param goal the lesser value
   * @return CONFIDENCE_JUSTIFIED if x&ge;goal, Invariant.CONFIDENCE_UNJUSTIFIED if x&le;1, other
   *     values otherwise
   */
  public static final double conf_is_ge(double x, double goal) {
    if (x >= goal) {
      return 1;
    }
    if (x <= 1) {
      return 0;
    }
    double result = 1 - (goal - x) / (goal - 1);
    assert 0 <= result && result <= 1
        : "conf_is_ge: bad result = " + result + " for (x=" + x + ", goal=" + goal + ")";
    return result;
  }

  /**
   * Return Invariant.PROBABILITY_JUSTIFIED if x&ge;goal. Return Invariant.PROBABILITY_UNJUSTIFIED
   * if x&le;1. For intermediate inputs, the result gives probability that grades between the two
   * extremes. See the discussion of gradual vs. sudden probability transitions.
   *
   * @param x the greater value
   * @param goal the lesser value
   * @return if x&ge;goal: invariant.PROBABILITY_JUSTIFIED; if x&le;1:
   *     Invariant.PROBABILITY_UNJUSTIFIED; otherwise: other values
   */
  public static final double prob_is_ge(double x, double goal) {
    if (x >= goal) {
      return 0;
    }
    if (x <= 1) {
      return 1;
    }
    double result = (goal - x) / (goal - 1);
    assert 0 <= result && result <= 1
        : "prob_is_ge: bad result = " + result + " for (x=" + x + ", goal=" + goal + ")";
    return result;
  }

  /**
   * Return the "and" of the given confidences. This is the confidence that multiple conditions
   * (whose confidences are given) are all satisfied.
   *
   * @param c1 the confidence of the first condition
   * @param c2 the confidence of the second condition
   * @return the "and" of the two condidences
   */
  public static final double confidence_and(double c1, double c2) {
    assert 0 <= c1 && c1 <= 1 : "confidence_and: bad c1 = " + c1;
    assert 0 <= c2 && c2 <= 1 : "confidence_and: bad c2 = " + c2;

    double result = c1 * c2;

    assert 0 <= result && result <= 1 : "confidence_and: bad result = " + result;
    return result;
  }

  /**
   * Return the "and" of the given confidences. This is the confidence that multiple conditions
   * (whose confidences are given) are all satisfied.
   *
   * @param c1 the confidence of the first condition
   * @param c2 the confidence of the second condition
   * @param c3 the confidence of the third condition
   * @return the "and" of the two condidences
   */
  public static final double confidence_and(double c1, double c2, double c3) {
    assert 0 <= c1 && c1 <= 1 : "confidence_and: bad c1 = " + c1;
    assert 0 <= c2 && c2 <= 1 : "confidence_and: bad c2 = " + c1;
    assert 0 <= c3 && c3 <= 1 : "confidence_and: bad c3 = " + c1;

    double result = c1 * c2 * c3;

    assert 0 <= result && result <= 1 : "confidence_and: bad result = " + result;
    return result;
  }

  /**
   * Return the "or" of the given confidences. This is the confidence that at least one of multiple
   * conditions (whose confidences are given) is satisfied.
   *
   * @param c1 the confidence of the first condition
   * @param c2 the confidence of the second condition
   * @return the "or" of the two condidences
   */
  public static final double confidence_or(double c1, double c2) {
    // Not "1-(1-c1)*(1-c2)" because that can produce a value too large; we
    // don't want the result to be larger than the larger argument.
    return Math.max(c1, c2);
  }

  /**
   * Return the "and" of the given probabilities. This is the probability that multiple conditions
   * (whose probabilities are given) are all satisfied.
   *
   * @param p1 the probability of the first condition
   * @param p2 the probability of the second condition
   * @return the "and" of the two condidences
   */
  public static final double prob_and(double p1, double p2) {
    assert 0 <= p1 && p1 <= 1 : "prob_and: bad p1 = " + p1;
    assert 0 <= p2 && p2 <= 1 : "prob_and: bad p2 = " + p2;

    // 1 - (1-p1)*(1-p2)
    double result = p1 + p2 - p1 * p2;

    assert 0 <= result && result <= 1 : "prob_and: bad result = " + result;
    return result;
  }

  /**
   * Return the "and" of the given probabilities. This is the probability that multiple conditions
   * (whose probabilities are given) are all satisfied.
   *
   * @param p1 the probability of the first condition
   * @param p2 the probability of the second condition
   * @param p3 the probability of the third condition
   * @return the "and" of the two condidences
   */
  public static final double prob_and(double p1, double p2, double p3) {
    assert 0 <= p1 && p1 <= 1 : "prob_and: bad p1 = " + p1;
    assert 0 <= p2 && p2 <= 1 : "prob_and: bad p2 = " + p1;
    assert 0 <= p3 && p3 <= 1 : "prob_and: bad p3 = " + p1;

    double result = 1 - (1 - p1) * (1 - p2) * (1 - p3);

    assert 0 <= result && result <= 1 : "prob_and: bad result = " + result;
    return result;
  }

  /**
   * Return the "or" of the given probabilities. This is the probability that at least one of
   * multiple conditions (whose probabilities are given) is satisfied.
   *
   * @param p1 the probability of the first condition
   * @param p2 the probability of the second condition
   * @return the "or" of the two condidences
   */
  public static final double prob_or(double p1, double p2) {
    // Not "p1*p2" because that can produce a value too small; we don't
    // want the result to be smaller than the smaller argument.
    return Math.min(p1, p2);
  }

  // Subclasses should set these; Invariant never does.

  /**
   * At least this many samples are required, or else we don't report any invariant at all. (Except
   * that OneOf invariants are treated differently.)
   */
  public static final int min_mod_non_missing_samples = 5;

  /**
   * Returns true if the invariant has enough samples to have its computed constants well-formed. Is
   * overridden in classes like LinearBinary/Ternary and Upper/LowerBound.
   *
   * @return true if the invariant has enough samples to have its computed constants well-formed
   */
  public boolean enoughSamples(@GuardSatisfied @NonPrototype Invariant this) {
    return true;
  }

  // The confidence routines (getConfidence and internal helper
  // computeConfidence) use ModBitTracker information to compute
  // justification.

  // There are three confidence routines:
  //  justified() is what most clients should call
  //  getConfidence() gives the actual confidence.  It used to cache
  //    results, but it does not do so any longer.
  //  computeConfidence() is an internal helper method that does the
  //    actual work, but it should not be called externally, only by
  //    getConfidence.  It ignores whether the invariant is falsified.

  // There are two general approaches to computing confidence
  // when there is a threshold (such as needing to see 10 samples):
  //  * Make the confidence typically either 0 or 1, transitioning
  //    suddenly between the two as soon as the 10th sample is observed.
  //  * Make the confidence transition more gradually; for instance, each
  //    sample changes the confidence by 10%.
  // The gradual approach has advantages and disadvantages:
  //  + Users can set the confidence limit to see invariants earlier; this
  //    is simpler than figuring out all the thresholds to set.
  //  + Tools such as the operational difference for test suite generation
  //    are assisted by knowing whether they are getting closer to
  //    justification.
  //  - The code is a bit more complicated.

  /**
   * A wrapper around getConfidence() or getConfidence().
   *
   * @return true if this invariant's confidence is greater than the global confidence limit
   */
  public final boolean justified(@NonPrototype Invariant this) {
    boolean just = (!falsified && (getConfidence() >= dkconfig_confidence_limit));
    if (logOn()) {
      log(
          "justified = %s, confidence = %s, samples = %s",
          just, getConfidence(), ppt.num_samples());
    }
    return just;
  }

  // If confidence == CONFIDENCE_NEVER, then this invariant can be eliminated.
  /**
   * Given that this invariant has been true for all values seen so far, this method returns the
   * confidence that that situation has occurred by chance alone.
   *
   * <p>Returns the probability that the observed data did not happen by chance alone. The result is
   * a value between 0 and 1 inclusive. 0 means that this invariant could never have occurred by
   * chance alone; we are fully confident that its truth is no coincidence. 1 means that the
   * invariant is certainly a happenstance, so the truth of the invariant is not relevant and it
   * should not be reported. Values between 0 and 1 give differing confidences in the invariant. The
   * method may also return {@link #CONFIDENCE_NEVER}, meaning the invariant has been falsified.
   *
   * <p>An invariant is printed only if its probability of not occurring by chance alone is large
   * enough (by default, greater than .99; see Daikon's --conf_limit command-line option.
   *
   * <p>As an example, consider the invariant "x!=0". If only one value, 22, has been seen for x,
   * then the conclusion "x!=0" is not justified. But if there have been 1,000,000 values, and none
   * of them were 0, then we may be confident that the property "x!=0" is not a coincidence.
   *
   * <p>This method is a wrapper around {@link #computeConfidence()}, which does the actual work.
   *
   * <p>Consider the invariant is 'x is even', which has a 50% chance of being true by chance for
   * each sample. Then a reasonable body for {@link #computeConfidence} would be
   *
   * <pre>return 1 - Math.pow(.5, ppt.num_samples());</pre>
   *
   * If 5 values had been seen, then this implementation would return 31/32, which is the likelihood
   * that all 5 values seen so far were even not purely by chance.
   *
   * @return confidence of this invariant
   * @see #computeConfidence()
   */
  public final double getConfidence(@NonPrototype Invariant this) {
    assert !falsified;
    // if (falsified)
    //   return CONFIDENCE_NEVER;
    double result = computeConfidence();
    // System.out.println("getConfidence: " + getClass().getName() + " " + ppt.varNames());
    if (!((result == CONFIDENCE_JUSTIFIED)
        || (result == CONFIDENCE_UNJUSTIFIED)
        || (result == CONFIDENCE_NEVER)
        || ((0 <= result) && (result <= 1)))) {
      // Can't print this.repr_prob(), as it may compute the confidence!
      System.out.println(
          "getConfidence: " + getClass().getName() + " " + ppt.varNames() + " => " + result);
      System.out.println("  " + this.format() + "; " + repr());
    }
    assert ((0 <= result) && (result <= 1))
            || (result == CONFIDENCE_JUSTIFIED)
            || (result == CONFIDENCE_UNJUSTIFIED)
            || (result == CONFIDENCE_NEVER)
        : "unexpected conf value: " + getClass().getName() + ": " + repr() + ", result=" + result;
    return result;
  }

  /**
   * This method computes the confidence that this invariant occurred by chance. Clients should call
   * {@link #getConfidence()} instead.
   *
   * <p>This method need not check the value of field "falsified", as the caller does that.
   *
   * @return confidence of this invariant
   * @see #getConfidence()
   */
  protected abstract double computeConfidence(@NonPrototype Invariant this);

  /**
   * Subclasses should override. An exact invariant indicates that given all but one variable value,
   * the last one can be computed. (I think that's correct, anyway.) Examples are IntComparison
   * (when only equality is possible), LinearBinary, FunctionUnary. OneOf is treated differently, as
   * an interface. The result of this method does not depend on whether the invariant is justified,
   * destroyed, etc.
   *
   * @return true if any variable value can be computed from all the others
   */
  @Pure
  public boolean isExact(@Prototype Invariant this) {
    return false;
  }

  // Implementations of this need to examine all the data values already
  // in the ppt.  Or, don't put too much work in the constructor and instead
  // have the caller do that.
  // The "ppt" argument can be null if this is a prototype invariant.
  protected Invariant(PptSlice ppt) {
    this.ppt = ppt;
    checkMergeOverridden();
  }

  @SuppressWarnings("nullness") // weakness in @Unused checking
  protected @Prototype Invariant() {
    this.ppt = null;
    checkMergeOverridden();
  }

  @SuppressWarnings("unused")
  @Pure
  private boolean isPrototype() {
    return this.ppt == null;
  }

  /**
   * Marks the invariant as falsified. Should always be called rather than just setting the flag so
   * that we can track when this happens.
   */
  public void falsify(@NonPrototype Invariant this) {
    falsified = true;
    if (logOn()) {
      log("Destroyed %s", format());
    }
  }

  /** Clear the falsified flag. */
  public void clear_falsified(@NonPrototype Invariant this) {
    falsified = false;
  }

  /**
   * Returns whether or not this invariant has been falsified.
   *
   * @return true if this invariant has been falsified
   */
  @Pure
  public boolean is_false(@NonPrototype Invariant this) {
    return falsified;
  }

  /** Do nothing special, Overridden to remove exception from declaration. */
  @SideEffectFree
  @Override
  public Invariant clone(@GuardSatisfied @NonPrototype Invariant this) {
    try {
      Invariant result = (Invariant) super.clone();
      return result;
    } catch (CloneNotSupportedException e) {
      throw new Error(); // can never happen
    }
  }

  /**
   * Make a copy of this invariant and transfer it into a new PptSlice.
   *
   * @param new_ppt must have the same arity and types
   * @param permutation gives the varinfo array index mapping in the new ppt
   * @return a copy of the invariant, on a different slice
   */
  public Invariant transfer(@NonPrototype Invariant this, PptSlice new_ppt, int[] permutation) {
    // Check some sanity conditions
    assert new_ppt.arity() == ppt.arity();
    assert permutation.length == ppt.arity();
    for (int i = 0; i < ppt.arity(); i++) {
      VarInfo oldvi = ppt.var_infos[i];
      VarInfo newvi = new_ppt.var_infos[permutation[i]];
      // We used to check that all 3 types were equal, but we can't do
      // that anymore, because with equality, invariants may get
      // transferred between old and new VarInfos of different types.
      // They are, however, comparable
      assert oldvi.comparableNWay(newvi);
    }

    Invariant result;
    // Clone it
    result = this.clone();

    // Fix up the fields
    result.ppt = new_ppt;
    // Let subclasses fix what they need to
    result = result.resurrect_done(permutation);

    if (logOn()) {
      result.log(
          "Created %s:%s via transfer from %s:%s using permutation %s old_ppt = %s new_ppt = %s",
          result.getClass().getName(),
          result.format(),
          getClass().getName(),
          format(),
          Arrays.toString(permutation),
          ppt,
          new_ppt);
      // result.log (UtilPlume.backTrace());
    }
    // if (debug.isLoggable(Level.FINE))
    //    debug.fine ("Invariant.transfer to " + new_ppt.name() + " "
    //                 + result.repr());

    return result;
  }

  /**
   * Clones the invariant and then permutes it as specified. Normally used to make child invariant
   * match the variable order of the parent when merging invariants bottom up.
   */
  public Invariant clone_and_permute(@NonPrototype Invariant this, int[] permutation) {

    Invariant result = this.clone();
    result = result.resurrect_done(permutation);

    if (logOn()) {
      result.log(
          "Created %s via clone_and_permute from %s using permutation %s old_ppt = %s",
          result.format(), format(), Arrays.toString(permutation), Arrays.toString(ppt.var_infos)
          // + " new_ppt = " + Arrays.toString (new_ppt.var_infos)
          );
    }

    return result;
  }

  /**
   * Take a falsified invariant and resurrect it in a new PptSlice.
   *
   * @param new_ppt must have the same arity and types
   * @param permutation gives the varinfo array index mapping
   * @return the resurrected invariant, in a new PptSlice
   */
  public Invariant resurrect(@NonPrototype Invariant this, PptSlice new_ppt, int[] permutation) {
    // Check some sanity conditions
    assert falsified;
    assert new_ppt.arity() == ppt.arity();
    assert permutation.length == ppt.arity();
    for (int i = 0; i < ppt.arity(); i++) {
      VarInfo oldvi = ppt.var_infos[i];
      VarInfo newvi = new_ppt.var_infos[permutation[i]];
      // We used to check that all 3 types were equal, but we can't do
      // that anymore, because with equality, invariants may get
      // resurrected between old and new VarInfos of different types.
      // They are, however, comparable
      assert oldvi.comparableNWay(newvi);
    }

    Invariant result;
    // Clone it
    result = this.clone();

    // Fix up the fields
    result.falsified = false;
    result.ppt = new_ppt;
    // Let subclasses fix what they need to
    result = result.resurrect_done(permutation);

    if (logOn()) {
      result.log(
          "Created %s via resurrect from %s using permutation %s old_ppt = %s new_ppt = %s",
          result.format(),
          format(),
          Arrays.toString(permutation),
          Arrays.toString(ppt.var_infos),
          Arrays.toString(new_ppt.var_infos));
    }

    return result;
  }

  /**
   * Returns a single VarComparability that describes the set of variables used by this invariant.
   * Since all of the variables in an invariant must be comparable, this can usually be the
   * comparability information for any variable. The exception is when one or more variables is
   * always comparable (comparable to everything else). An always comparable VarComparability is
   * returned only if all of the variables involved are always comparable. Otherwise the
   * comparability information from one of the non always-comparable variables is returned.
   *
   * @return a VarComparability that describes any (and all) of this invariant's variables
   */
  public VarComparability get_comparability(@NonPrototype Invariant this) {

    // assert ppt != null : "class " + getClass();

    // Return the first variable that is not always-comparable
    for (int i = 0; i < ppt.var_infos.length; i++) {
      VarComparability vc = ppt.var_infos[i].comparability;
      if (!vc.alwaysComparable()) {
        return vc;
      }
    }

    // All the variables are always-comparable, just return the first one
    // return ppt.var_infos[0].comparability;
    return VarComparabilityImplicit.unknown;
  }

  /**
   * Merge the invariants in invs to form a new invariant. This implementation merely returns a
   * clone of the first invariant in the list. This is correct for simple invariants whose equation
   * or statistics don't depend on the actual samples seen. It should be overriden for more complex
   * invariants (eg, bound, oneof, linearbinary, etc).
   *
   * @param invs list of invariants to merge. The invariants must all be of the same type and should
   *     come from the children of parent_ppt. They should also all be permuted to match the
   *     variable order in parent_ppt.
   * @param parent_ppt slice that will contain the new invariant
   * @return the merged invariant or null if the invariants didn't represent the same invariant
   */
  public @Nullable @NonPrototype Invariant merge(
      @Prototype Invariant this, List<@NonPrototype Invariant> invs, PptSlice parent_ppt) {

    Invariant first = invs.get(0);
    Invariant result = first.clone();
    result.ppt = parent_ppt;
    result.log(
        "Merged '%s' from %s child invariants",
        result.format(), invs.size() /*, first.ppt.name() */);

    // Make sure that each invariant was really of the same type
    boolean assert_enabled = false;
    assert (assert_enabled = true);
    // Now, assert_enabled is true if the JVM was started with the "-ea" command-line argument.
    if (assert_enabled) {
      Match m = new Match(result);
      for (int i = 1; i < invs.size(); i++) {
        assert m.equals(new Match(invs.get(i)));
      }
    }

    return result;
  }

  /**
   * Permutes the invariant as specified. Often creates a new invariant (with a different class).
   *
   * @param permutation the permutation
   * @return the permuted invariant
   */
  public @NonPrototype Invariant permute(@NonPrototype Invariant this, int[] permutation) {
    return resurrect_done(permutation);
  }

  /**
   * Called on the new invariant just before resurrect() returns it to allow subclasses to fix any
   * information they might have cached from the old Ppt and VarInfos.
   *
   * @param permutation the permutation
   * @return the permuted invariant
   */
  protected abstract Invariant resurrect_done(@NonPrototype Invariant this, int[] permutation);

  // Regrettably, I can't declare a static abstract method.
  // // The return value is probably ignored.  The new Invariant installs
  // // itself on the PptSlice, and that's what really matters (right?).
  // public static abstract Invariant instantiate(PptSlice ppt);

  /** Return true if this invariant uses the given variable. */
  public boolean usesVar(@NonPrototype Invariant this, VarInfo vi) {
    return ppt.usesVar(vi);
  }

  /** Return true if this invariant uses the given variable. */
  public boolean usesVar(@NonPrototype Invariant this, String name) {
    return ppt.usesVar(name);
  }

  /** Return true if this invariant uses the given variable or any variable derived from it. */
  public boolean usesVarDerived(@NonPrototype Invariant this, String name) {
    return ppt.usesVarDerived(name);
  }

  // Not used as of 1/31/2000
  // // For use by subclasses.
  // /** Put a string representation of the variable names in the StringBuilder. */
  // public void varNames(StringBuilder sb) {
  //   // sb.append(this.getClass().getName());
  //   ppt.varNames(sb);
  // }

  /**
   * Return a string representation of the variable names.
   *
   * @return a string representation of the variable names
   */
  public final String varNames(@GuardSatisfied @NonPrototype Invariant this) {
    return ppt.varNames();
  }

  // repr()'s output should not include result of getConfidence, because
  // repr() may be called from computeConfidence or elsewhere for
  // debugging purposes.
  /**
   * For printing invariants, there are two interfaces: repr gives a low-level representation
   * ({@link #repr_prob} also prints the confidence), and {@link #format} gives a high-level
   * representation for user output.
   *
   * @return a string representation of this
   */
  public String repr(@GuardSatisfied @NonPrototype Invariant this) {
    // A better default would be to use reflection and print out all
    // the variable names.
    return getClass() + varNames() + ": " + format();
  }

  /**
   * For printing invariants, there are two interfaces: {@link #repr} gives a low-level
   * representation (repr_prob also prints the confidence), and {@link #format} gives a high-level
   * representation for user output.
   *
   * @return {@link #repr()}, but with the confidence as well
   */
  public String repr_prob(@NonPrototype Invariant this) {
    return repr() + "; confidence = " + getConfidence();
  }

  /**
   * Returns a high-level printed representation of the invariant, for user output. {@code format}
   * produces normal output, while the {@link #repr} formatting routine produces low-level, detailed
   * output for debugging, and {@link #repr_prob} also prints the confidence.
   *
   * @return a string representation of this
   */
  // Does not respect PrintInvariants.dkconfig_print_inv_class; PrintInvariants does so.
  // Receiver must be fully-initialized because subclasses read their fields.
  @SideEffectFree
  public String format(@GuardSatisfied @NonPrototype Invariant this) {
    return format_using(OutputFormat.DAIKON);
  }

  /**
   * Returns the class name of the invariant, for use in debugging output. Returns "" if {@link
   * PrintInvariants#dkconfig_print_inv_class} is false.
   *
   * @return a string representation of the class name of the invariant, or ""
   */
  public String format_classname() {
    if (!PrintInvariants.dkconfig_print_inv_class) {
      return "";
    }
    String classname = getClass().getName();
    int index = classname.lastIndexOf('.');
    classname = classname.substring(index + 1);
    return " [" + classname + "]";
  }

  /** Return a printed representation of this invariant, in the given format. */
  @SideEffectFree
  public abstract String format_using(
      @GuardSatisfied @NonPrototype Invariant this, OutputFormat format);

  /**
   * Returns a conjuction of mapping the same function of our expresssions's VarInfos, in general.
   * Subclasses may override if they are able to handle generally-inexpressible properties in
   * special-case ways.
   *
   * @return conjuction of mapping the same function of our expresssions's VarInfos
   * @see VarInfo#isValidEscExpression
   */
  @Pure
  public boolean isValidEscExpression(@NonPrototype Invariant this) {
    for (int i = 0; i < ppt.var_infos.length; i++) {
      if (!ppt.var_infos[i].isValidEscExpression()) {
        return false;
      }
    }
    return true;
  }

  /** A "\type(...)" construct where the "..." contains a "$". */
  private static Pattern anontype_pat = Pattern.compile("\\\\type\\([^\\)]*\\$");

  /**
   * Returns true if this Invariant can be properly formatted for the given output format.
   *
   * @param format the expected output format
   * @return true if this Invariant can be properly formatted for the given output format
   */
  @Pure
  public boolean isValidExpression(@NonPrototype Invariant this, OutputFormat format) {
    if ((format == OutputFormat.ESCJAVA) && !isValidEscExpression()) {
      return false;
    }

    String s = format_using(format);

    if ((format == OutputFormat.ESCJAVA) || format.isJavaFamily()) {
      // This list should get shorter as we improve the formatting.
      if ((s.indexOf(" needs to be implemented: ") != -1)
          || (s.indexOf("<<") != -1)
          || (s.indexOf(">>") != -1)
          || (s.indexOf("warning: ") != -1)
          || (s.indexOf('~') != -1)
          || (s.indexOf("\\new") != -1)
          || (s.indexOf(".toString ") != -1)
          || s.endsWith(".toString")
          || (s.indexOf(".typeArray") != -1)
          || (s.indexOf("warning: method") != -1)
          || (s.indexOf("inexpressible") != -1)
          || (s.indexOf("unimplemented") != -1)
          || (s.indexOf("Infinity") != -1)
          || anontype_pat.matcher(s).find()) {
        return false;
      }
    }
    return true;
  }

  /**
   * Returns standard "format needs to be implemented" for the given requested format. Made public
   * so cores can call it.
   *
   * @param format the requested output format
   * @return standard "format needs to be implemented" for the given requested format
   */
  public String format_unimplemented(
      @GuardSatisfied @NonPrototype Invariant this, OutputFormat format) {
    String classname = this.getClass().getName();
    return "warning: method "
        + classname
        + ".format("
        + format
        + ")"
        + " needs to be implemented: "
        + format();
  }

  /**
   * Returns standard "too few samples for to have interesting invariant" for the requested format.
   * For machine-readable formats, this is just "true". An optional string argument, if supplied, is
   * a human-readable description of the invariant in its uninformative state, which will be added
   * to the message.
   *
   * @param format the requested output format
   * @return standard "too few samples for to have interesting invariant" for the requested format
   */
  public String format_too_few_samples(
      @GuardSatisfied @NonPrototype Invariant this, OutputFormat format, @Nullable String attempt) {
    if (format == OutputFormat.SIMPLIFY) {
      return "(AND)";
    } else if (format == OutputFormat.JAVA
        || format == OutputFormat.ESCJAVA
        || format == OutputFormat.JML
        || format == OutputFormat.DBCJAVA) {
      return "true";
    }
    String classname = this.getClass().getName();
    if (attempt == null) {
      attempt = varNames();
    }
    return "warning: too few samples for " + classname + " invariant: " + attempt;
  }

  /**
   * Convert a floating point value into the weird Modula-3-like floating point format that the
   * Simplify tool requires.
   *
   * @param d the number to print
   * @return a printed representation of the number, for Simplify
   */
  public static String simplify_format_double(double d) {
    String s = d + "";
    if (s.indexOf('E') != -1) {
      // 1E6 -> 1d6
      // 1.43E6 -> 1.43d6
      s = s.replace('E', 'd');
    } else if (s.indexOf('.') != -1) {
      // 3.14 -> 3.14d0
      s = s + "d0";
    } else if (s.equals("-Infinity")) {
      // -Infinity -> NegativeInfinity
      s = "NegativeInfinity";
    }
    // 5 -> 5
    // NaN -> NaN
    // Infinity -> Infinity
    return s;
  }

  /**
   * Conver a long integer value into a format that Simplify can use. If the value is too big, we
   * have to print it in a weird way, then tell Simplify about its properties specially.
   *
   * @param l the number to print
   * @return a printed representation of the number, for Simplify
   */
  public static String simplify_format_long(long l) {
    LemmaStack.noticeInt(l);
    if (l > -LemmaStack.SMALL_INTEGER && l < LemmaStack.SMALL_INTEGER) {
      // Simplify's internal numeric representation is based on
      // (ratios of) signed 32-bit integers, so it can't be both sound
      // and precise with respect to integer overflow. In its default
      // configuration it crashes with an internal error when any
      // internal computation overflows. To work around this, we print
      // large integers in an abstract format, and then explicitly
      // give the ordering among those values. This loses other
      // arithmetic facts, but is good enough for the way we use it.
      // Examples of inputs containing large integers that crash:
      // > (BG_PUSH (< 2147483647 n))
      // (assertion in negative argument to GCD)
      // > (BG_PUSH (>= x -1073741825))
      // > (BG_PUSH (<= x 1073741825))
      // > (OR)
      // (assertion: unexpected negative value in the Simplex engine)
      // > (BG_PUSH (EQ x 56312))
      // > (BG_PUSH (EQ y (* y x)))
      // (assertion: negative denomniator in rational normalize)
      // The value 32000 for SMALL_INTEGER is semi-empirically derived
      // as "2**15 - some slop" in hopes of working around as many
      // potential overflows in Simplify as possible, while still
      // letting it do concrete arithmetic on small integers. It may
      // be that there's no bound that would work with arbitrary
      // formulas, but this one seems to work OK for formulas
      // generated by Daikon.
      return "" + l;
    } else {
      return SimpUtil.formatInteger(l);
    }
  }

  /**
   * Convert a string value into the weird |-quoted format that the Simplify tool requires. (Note
   * that Simplify doesn't distinguish between variables, symbolic constants, and strings, so we
   * prepend "_string_" to avoid collisions with variables and other symbols).
   *
   * @param s the number to print
   * @return a printed representation of the string, for Simplify
   */
  public static String simplify_format_string(String s) {
    if (s == null) {
      return "null";
    }
    StringBuilder buf = new StringBuilder("|_string_");
    if (s.length() > 150) {
      // Simplify can't handle long strings (its input routines have a
      // 4000-character limit for |...| identifiers, but it gets an
      // internal array overflow for ones more than about 195
      // characters), so replace all but the beginning and end of a
      // long string with a hashed summary.
      int summ_length = s.length() - 100;
      int p1 = 50 + summ_length / 4;
      int p2 = 50 + summ_length / 2;
      int p3 = 50 + 3 * summ_length / 4;
      int p4 = 50 + summ_length;
      StringBuilder summ_buf = new StringBuilder(s.substring(0, 50));
      summ_buf.append("...");
      summ_buf.append(Integer.toHexString(s.substring(50, p1).hashCode()));
      summ_buf.append(Integer.toHexString(s.substring(p1, p2).hashCode()));
      summ_buf.append(Integer.toHexString(s.substring(p2, p3).hashCode()));
      summ_buf.append(Integer.toHexString(s.substring(p3, p4).hashCode()));
      summ_buf.append("...");
      summ_buf.append(s.substring(p4));
      s = summ_buf.toString();
    }
    for (int i = 0; i < s.length(); i++) {
      char c = s.charAt(i);
      // The following line uses '\n' rather than linesep.
      if (c == '\n') buf.append("\\n");
      else if (c == '\r') buf.append("\\r");
      else if (c == '\t') buf.append("\\t");
      else if (c == '\f') buf.append("\\f");
      else if (c == '\\') buf.append("\\\\");
      else if (c == '|') buf.append("\\|");
      else if (c >= ' ' && c <= '~') buf.append(c);
      else {
        buf.append("\\");
        // AFAIK, Simplify doesn't glork Unicode, so lop off all but
        // the low 8 bits
        String octal = Integer.toOctalString(c & 0xff);
        // Also, Simplify only accepts octal escapes with exactly 3 digits
        while (octal.length() < 3) octal = "0" + octal;
        buf.append(octal);
      }
    }
    buf.append("|");
    return buf.toString();
  }

  // This should perhaps be merged with some kind of PptSlice comparator.
  /** Compare based on arity, then printed representation. */
  public static final class InvariantComparatorForPrinting implements Comparator<Invariant> {
    @Pure
    @Override
    public int compare(@NonPrototype Invariant inv1, @NonPrototype Invariant inv2) {
      if (inv1 == inv2) {
        return 0;
      }

      // Guarding implications should compare as if they were without the
      // guarding predicate

      if (inv1 instanceof GuardingImplication) {
        inv1 = ((GuardingImplication) inv1).right;
      }
      if (inv2 instanceof GuardingImplication) {
        inv2 = ((GuardingImplication) inv2).right;
      }

      // Put equality invariants first
      if ((inv1 instanceof EqualityComparison) && !(inv2 instanceof EqualityComparison)) {
        return -1;
      }
      if (!(inv1 instanceof EqualityComparison) && (inv2 instanceof EqualityComparison)) {
        return 1;
      }

      // assert inv1.ppt.parent == inv2.ppt.parent;
      VarInfo[] vis1 = inv1.ppt.var_infos;
      VarInfo[] vis2 = inv2.ppt.var_infos;
      int arity_cmp = vis1.length - vis2.length;
      if (arity_cmp != 0) {
        return arity_cmp;
      }
      // Comparing on variable index is wrong in general:  variables of the
      // same name may have different indices at different program points.
      // However, it's safe if the invariants are from the same program
      // point.  Also, it is nice to avoid changing the order of variables
      // from that of the data trace file.

      if (inv1.ppt.parent == inv2.ppt.parent) {
        for (int i = 0; i < vis1.length; i++) {
          int tmp = vis1[i].varinfo_index - vis2[i].varinfo_index;
          if (tmp != 0) {
            return tmp;
          }
        }
      } else {
        // // Debugging
        // System.out.println("ICFP: different parents for " + inv1.format() + ", " +
        // inv2.format());

        for (int i = 0; i < vis1.length; i++) {
          String name1 = vis1[i].name();
          String name2 = vis2[i].name();
          if (name1.equals(name2)) {
            continue;
          }
          int name1in2 = inv2.ppt.parent.indexOf(name1);
          int name2in1 = inv1.ppt.parent.indexOf(name2);
          int cmp1 = (name1in2 == -1) ? 0 : vis1[i].varinfo_index - name1in2;
          int cmp2 = (name2in1 == -1) ? 0 : vis2[i].varinfo_index - name2in1;
          int cmp = MathPlume.sign(cmp1) + MathPlume.sign(cmp2);
          if (cmp != 0) {
            return cmp;
          }
        }
      }

      // Sort OneOf invariants earlier than others
      if ((inv1 instanceof OneOf) && !(inv2 instanceof OneOf)) {
        return -1;
      }
      if (!(inv1 instanceof OneOf) && (inv2 instanceof OneOf)) {
        return 1;
      }

      // System.out.println("ICFP: default rule yields "
      //                    + inv1.format().compareTo(inv2.format())
      //                    + " for " + inv1.format() + ", " + inv2.format());
      // (Actually, FileIO.new_decl_format should always be non-null here.)
      if (PrintInvariants.dkconfig_old_array_names
          && FileIO.new_decl_format != null
          && FileIO.new_decl_format) {
        return inv1.format().replace("[..]", "[]").compareTo(inv2.format().replace("[..]", "[]"));
      } else {
        return inv1.format().compareTo(inv2.format());
      }
    }
  }

  /**
   * Returns true iff the two invariants represent the same mathematical formula. Does not consider
   * the context such as variable names, confidences, sample counts, value counts, or related
   * quantities. As a rule of thumb, if two invariants format the same, this method returns true.
   * Furthermore, in many cases, if an invariant does not involve computed constants (as "x&gt;c"
   * and "y=ax+b" do for constants a, b, and c), then this method vacuously returns true.
   *
   * @param other the invariant to compare to this one
   * @return true iff the two invariants represent the same mathematical formula. Does not consider
   * @exception RuntimeException if other.getClass() != this.getClass()
   */
  public abstract boolean isSameFormula(@Prototype Invariant this, Invariant other);

  /**
   * Returns whether or not it is possible to merge invariants of the same class but with different
   * formulas when combining invariants from lower ppts to build invariants at upper program points.
   * Invariants that have this characteristic (eg, bound, oneof) should override this function. Note
   * that invariants that can do this, normally need special merge code as well (to merge the
   * different formulas into a single formula at the upper point.
   *
   * @return true if invariants with different formulas can be merged
   */
  public boolean mergeFormulasOk(@Prototype Invariant this) {
    return false;
  }

  /**
   * Returns true iff the argument is the "same" invariant as this. Same, in this case, means a
   * matching type, formula, and variable names.
   *
   * @param inv2 the other invariant to compare to this one
   * @return true iff the argument is the "same" invariant as this
   */
  @Pure
  public boolean isSameInvariant(@NonPrototype Invariant this, Invariant inv2) {
    // return isSameInvariant(inv2, defaultIsSameInvariantNameExtractor);

    Invariant inv1 = this;

    // Can't be the same if they aren't the same type
    if (!inv1.getClass().equals(inv2.getClass())) {
      return false;
    }

    // Can't be the same if they aren't the same formula
    if (!inv1.isSameFormula(inv2)) {
      return false;
    }

    // The variable names much match up, in order
    VarInfo[] vars1 = inv1.ppt.var_infos;
    VarInfo[] vars2 = inv2.ppt.var_infos;

    // due to inv type match already
    assert vars1.length == vars2.length;

    for (int i = 0; i < vars1.length; i++) {
      VarInfo var1 = vars1[i];
      VarInfo var2 = vars2[i];
      if (!var1.name().equals(var2.name())) {
        return false;
      }
    }

    return true;
  }

  /**
   * Returns true iff the two invariants represent mutually exclusive mathematical formulas -- that
   * is, if one of them is true, then the other must be false. This method does not consider the
   * context such as variable names, confidences, sample counts, value counts, or related
   * quantities.
   *
   * @param other the other invariant to compare to this one
   * @return true iff the two invariants represent mutually exclusive mathematical formulas
   */
  @Pure
  public boolean isExclusiveFormula(@NonPrototype Invariant this, Invariant other) {
    return false;
  }

  /**
   * Look up a previously instantiated Invariant.
   *
   * @param invclass the class of the invariant to search for
   * @param ppt the program point in which to look for the invariant
   * @return the invariant of class invclass, or null if none was found
   */
  // This implementation should be made more efficient, because it's used in
  // suppression.  We should somehow index invariants by their type.
  public static @Nullable Invariant find(Class<? extends Invariant> invclass, PptSlice ppt) {
    for (Invariant inv : ppt.invs) {
      if (inv.getClass() == invclass) {
        return inv;
      }
    }
    return null;
  }

  /**
   * Returns the set of non-instantiating suppressions for this invariant. May return null instead
   * of an empty set. Should be overridden by subclasses with non-instantiating suppressions.
   *
   * @return the set of non-instantiating suppressions for this invariant
   */
  @Pure
  public @Nullable NISuppressionSet get_ni_suppressions(@Prototype Invariant this) {
    return null;
  }

  /**
   * Returns whether or not this invariant is ni-suppressed.
   *
   * @return true if this invariant is ni-suppressed
   */
  @SuppressWarnings(
      "nullness") // tricky control flow, need to mark get_ni_suppressions as @Pure if that's true
  @EnsuresNonNullIf(result = true, expression = "get_ni_suppressions()")
  @Pure
  public boolean is_ni_suppressed() {

    NISuppressionSet ss = get_ni_suppressions();
    if (ss == null) {
      return false;
    }
    boolean suppressed = ss.suppressed(ppt);
    if (suppressed && Debug.logOn() && (Daikon.current_inv != null)) {
      Daikon.current_inv.log("inv %s suppressed: %s", format(), ss);
    }
    if (Debug.logDetail()) {
      log("suppressed = %s suppression set = %s", suppressed, ss);
    }

    return suppressed;
  }

  // ///////////////////////////////////////////////////////////////////////////
  // Tests about the invariant (for printing)
  //

  // DO NOT OVERRIDE.  Should be declared "final", but the "final" is
  // omitted to allow for easier testing.
  @Pure
  public boolean isWorthPrinting(@NonPrototype Invariant this) {
    return InvariantFilters.defaultFilters().shouldKeep(this) == null;
  }

  // ////////////////////////////////////////////////////////////////////////////
  // Static and dynamic checks for obviousness

  /**
   * Return true if this invariant is necessarily true from a fact that can be determined statically
   * from the decls files. (An example is being from a certain derivation.) Intended to be
   * overridden by subclasses.
   *
   * <p>This method is final because children of Invariant should be extending
   * isObviousStatically(VarInfo[]) because it is more general.
   */
  @Pure
  public final @Nullable DiscardInfo isObviousStatically(@NonPrototype Invariant this) {
    return isObviousStatically(this.ppt.var_infos);
  }

  /**
   * Return true if this invariant is necessarily true from a fact that can be determined statically
   * -- for the given varInfos rather than the varInfos of this. Conceptually, this means "is this
   * invariant statically obvious if its VarInfos were switched with vis?" Intended to be overridden
   * by subclasses. Should only do static checking.
   *
   * <p>Precondition: vis.length == this.ppt.var_infos.length
   *
   * @param vis the VarInfos this invariant is obvious over. The position and data type of the
   *     variables is the *same* as that of this.ppt.var_infos.
   */
  @Pure
  public @Nullable DiscardInfo isObviousStatically(@Prototype Invariant this, VarInfo[] vis) {
    return null;
  }

  /**
   * Return true if this invariant and all equality combinations of its member variables are
   * necessarily true from a fact that can be determined statically (i.e., the decls files). For
   * example, a == b, and f(a) is obvious, but f(b) is not. In that case, this method on f(a) would
   * return false. If f(b) is also obvious, then this method would return true.
   */
  // This is used because we cannot decide to non-instantiate some
  // invariants just because isObviousStatically is true, since some
  // of the member variables may be equal to non-obvious varInfos.  If
  // we were to non-instantiate, we could not copy an invariant to the
  // non-obvious VarInfos should they split off from the obvious one.
  // Of course, it's expensive to examine every possible permutation
  // of VarInfos and their equality set, so a possible conservative
  // approximation is to simply return false.
  @Pure
  public boolean isObviousStatically_AllInEquality(@NonPrototype Invariant this) {
    // If the leaders aren't statically obvious, then clearly not all
    // combinations are.
    if (isObviousStatically() == null) {
      return false;
    }

    for (int i = 0; i < ppt.var_infos.length; i++) {
      if (ppt.var_infos[i].equalitySet.getVars().size() > 1) {
        return false;
      }
    }
    return true;
  }

  /**
   * Return true if this invariant and some equality combinations of its member variables are
   * statically obvious. For example, if a == b, and f(a) is obvious, then so is f(b). We use the
   * someInEquality (or least interesting) method during printing so we only print an invariant if
   * all its variables are interesting, since a single, static, non interesting occurance means all
   * the equality combinations aren't interesting.
   *
   * @return the VarInfo array that contains the VarInfos that showed this invariant to be obvious.
   *     The contains variables that are elementwise in the same equality set as this.ppt.var_infos.
   *     Can be null if no such assignment exists.
   */
  @Pure
  public @Nullable DiscardInfo isObviousStatically_SomeInEquality(@NonPrototype Invariant this) {
    DiscardInfo result = isObviousStatically();
    if (result != null) {
      return result;
    }
    return isObviousStatically_SomeInEqualityHelper(
        this.ppt.var_infos, new /*NNC:@MonotonicNonNull*/ VarInfo[this.ppt.var_infos.length], 0);
  }

  // TODO: finish this comment.
  /** Recurse through vis and generate the cartesian product of ... */
  @Pure
  protected @Nullable DiscardInfo isObviousStatically_SomeInEqualityHelper(
      @NonPrototype Invariant this,
      VarInfo[] vis,
      /*NNC:@MonotonicNonNull*/ VarInfo[] assigned,
      int position) {
    if (position == vis.length) {
      if (debugIsObvious.isLoggable(Level.FINE)) {
        StringBuilder sb = new StringBuilder();
        sb.append("  isObviousStatically_SomeInEquality: ");
        for (int i = 0; i < vis.length; i++) {
          sb.append(assigned[i].name() + " ");
        }
        debugIsObvious.fine(sb.toString());
      }

      assigned = castNonNullDeep(assigned); // https://tinyurl.com/cfissue/986
      return isObviousStatically(assigned);
    } else {
      for (VarInfo vi : vis[position].get_equalitySet_vars()) {
        assigned[position] = vi;
        DiscardInfo temp = isObviousStatically_SomeInEqualityHelper(vis, assigned, position + 1);
        if (temp != null) {
          return temp;
        }
      }
      return null;
    }
  }

  /**
   * Return true if this invariant is necessarily true from a fact that can be determined statically
   * (i.e., the decls files) or dynamically (after checking data). Intended not to be overriden,
   * because sub-classes should override isObviousStatically or isObviousDynamically. Wherever
   * possible, suppression, rather than this, should do the dynamic checking.
   */
  @Pure
  public final @Nullable DiscardInfo isObvious(@NonPrototype Invariant this) {
    // Actually actually, we'll eliminate invariants as they become obvious
    // rather than on output; the point of this is to speed up computation.
    // // Actually, we do need to check isObviousDerived after all because we
    // // add invariants that might be obvious, but might also turn out to be
    // // even stronger (and so not obvious).  We don't know how the invariant
    // // turns out until after testing it.
    // // // We don't need to check isObviousDerived because we won't add
    // // // obvious-derived invariants to lists in the first place.
    DiscardInfo staticResult = isObviousStatically_SomeInEquality();
    if (staticResult != null) {
      if (debugPrint.isLoggable(Level.FINE)) {
        debugPrint.fine("  [obvious:  " + repr_prob() + " ]");
      }
      return staticResult;
    } else {
      DiscardInfo dynamicResult = isObviousDynamically_SomeInEquality();
      if (dynamicResult != null) {
        if (debugPrint.isLoggable(Level.FINE)) {
          debugPrint.fine("  [obvious:  " + repr_prob() + " ]");
        }
        return dynamicResult;
      } else {
        return null;
      }
    }
  }

  /**
   * Return non-null if this invariant is necessarily true from a fact that can be determined
   * dynamically (after checking data) -- for the given varInfos rather than the varInfos of this.
   * Conceptually, this means, "Is this invariant dynamically obvious if its VarInfos were switched
   * with vis?" Intended to be overriden by subclasses so they can filter invariants after checking;
   * the overriding method should first call "super.isObviousDynamically(vis)". Since this method is
   * dynamic, it should only be called after all processing.
   */
  public @Nullable DiscardInfo isObviousDynamically(@NonPrototype Invariant this, VarInfo[] vis) {
    assert !Daikon.isInferencing;
    assert vis.length <= 3 : "Unexpected more-than-ternary invariant";
    if (!ArraysPlume.hasNoDuplicates(vis)) {
      log("Two or more variables are equal %s", format());
      return new DiscardInfo(this, DiscardCode.obvious, "Two or more variables are equal");
    }
    // System.out.println("Passed Invariant.isObviousDynamically(): " + format());
    return null;
  }

  /**
   * Return true if more than one of the variables in the invariant are the same variable. We create
   * such invariants for the purpose of equality set processing, but they aren't intended for
   * printing; there should be invariants with the same meaning but lower arity instead. For
   * instance, we don't need "x = x + x" because we have "x = 0" instead.
   *
   * <p>Actually, this isn't strictly true: we don't have an invariant "a[] is a palindrome"
   * corresponding to "a[] is the reverse of a[]", for instance.
   *
   * @return true if more than one of the variables in the invariant are the same variable
   */
  @Pure
  public boolean isReflexive(@NonPrototype Invariant this) {
    return !ArraysPlume.hasNoDuplicates(ppt.var_infos);
  }

  /**
   * Return true if this invariant is necessarily true from a fact that can be determined
   * dynamically (after checking data, based on other invariants that were inferred). Since this
   * method is dynamic, it should only be called after all processing.
   *
   * <p>If a test does not look up other inferred invariants (that is, it relies only on information
   * in the decls file), then it should be written as an isObviousStatically test, not an
   * isObviousDynamically test.
   *
   * <p>This method is final because subclasses should extend isObviousDynamically(VarInfo[]) since
   * that method is more general.
   */
  public final @Nullable DiscardInfo isObviousDynamically(@NonPrototype Invariant this) {
    assert !Daikon.isInferencing;
    return isObviousDynamically(ppt.var_infos);
  }

  /**
   * Return true if this invariant and some equality combinations of its member variables are
   * dynamically obvious. For example, a == b, and f(a) is obvious, so is f(b). We use the
   * someInEquality (or least interesting) method during printing so we only print an invariant if
   * all its variables are interesting, since a single, dynamic, non interesting occurance means all
   * the equality combinations aren't interesting.
   *
   * @return the VarInfo array that contains the VarInfos that showed this invariant to be obvious.
   *     The contains variables that are elementwise in the same equality set as this.ppt.var_infos.
   *     Can be null if no such assignment exists.
   */
  @Pure
  public @Nullable DiscardInfo isObviousDynamically_SomeInEquality(@NonPrototype Invariant this) {
    DiscardInfo result = isObviousDynamically();
    if (result != null) {
      return result;
    }
    return isObviousDynamically_SomeInEqualityHelper(
        this.ppt.var_infos, new /*NNC:@MonotonicNonNull*/ VarInfo[this.ppt.var_infos.length], 0);
  }

  /**
   * Recurse through vis (an array of leaders) and generate the cartesian product of their equality
   * sets; in other words, every combination of one element from each equality set. For each such
   * combination, test isObviousDynamically; if any test is true, then return that combination. The
   * combinations are generated via recursive calls to this routine.
   */
  protected @Nullable DiscardInfo isObviousDynamically_SomeInEqualityHelper(
      @NonPrototype Invariant this, VarInfo[] vis, VarInfo[] assigned, int position) {
    if (position == vis.length) {
      // base case
      if (debugIsObvious.isLoggable(Level.FINE)) {
        StringBuilder sb = new StringBuilder();
        sb.append("  isObviousDynamically_SomeInEquality: ");
        for (int i = 0; i < vis.length; i++) {
          sb.append(assigned[i].name() + " ");
        }
        debugIsObvious.fine(sb.toString());
      }
      return isObviousDynamically(assigned);
    } else {
      // recursive case
      for (VarInfo vi : vis[position].get_equalitySet_vars()) {
        assigned[position] = vi;
        DiscardInfo temp = isObviousDynamically_SomeInEqualityHelper(vis, assigned, position + 1);
        if (temp != null) {
          return temp;
        }
      }
      return null;
    }
  }

  /**
   * Returns true if this invariant is only over prestate variables.
   *
   * @return true if this invariant is only over prestate variables
   */
  @Pure
  public boolean isAllPrestate(@NonPrototype Invariant this) {
    return ppt.allPrestate();
  }

  /**
   * An invariant that includes an uninteresting constant (say, "size(x[]) &lt; 237") is likely to
   * be an artifact of the way the program was tested, rather than a statement that would in fact
   * hold over all possible executions.
   */
  public boolean hasUninterestingConstant(@NonPrototype Invariant this) {
    return false;
  }

  // Orders invariants by class, then by variable names.  If the
  // invariants are both of class Implication, they are ordered by
  // comparing the predicate, then the consequent.
  public static final class ClassVarnameComparator implements Comparator<Invariant> {
    @Pure
    @Override
    public int compare(Invariant inv1, Invariant inv2) {

      if (inv1 instanceof Implication && inv2 instanceof Implication) {
        return compareImplications((Implication) inv1, (Implication) inv2);
      }

      int compareClass = compareClass(inv1, inv2);
      if (compareClass != 0) {
        return compareClass;
      }

      return compareVariables(inv1, inv2);
    }

    // Returns 0 if the invariants are of the same class.  Else,
    // returns the comparison of the class names.
    private int compareClass(Invariant inv1, Invariant inv2) {
      if (inv1.getClass().equals(inv2.getClass())) {
        if (inv1 instanceof DummyInvariant) {
          // This special case is needed because the other code
          // assumes that all invariants of a given class have the
          // same arity.
          String df1 = inv1.format();
          String df2 = inv2.format();
          int cmp = df1.compareTo(df2);
          if (cmp != 0) {
            return cmp;
          }
          return inv1.ppt.var_infos.length - inv2.ppt.var_infos.length;
        }
        return 0;
      } else {
        String classname1 = inv1.getClass().getName();
        String classname2 = inv2.getClass().getName();
        return classname1.compareTo(classname2);
      }
    }

    // Returns 0 if the invariants have the same variable names.
    // Else, returns the comparison of the first variable names that
    // differ.  Requires that the invariants be of the same class.
    private int compareVariables(Invariant inv1, Invariant inv2) {
      VarInfo[] vars1 = inv1.ppt.var_infos;
      VarInfo[] vars2 = inv2.ppt.var_infos;

      // due to inv type match already
      assert vars1.length == vars2.length
          : "Bad type match: " + inv1.format() + " vs. " + inv2.format();

      for (int i = 0; i < vars1.length; i++) {
        VarInfo var1 = vars1[i];
        VarInfo var2 = vars2[i];
        int compare = var1.name().compareTo(var2.name());
        if (compare != 0) {
          return compare;
        }
      }

      // All the variable names matched
      return 0;
    }

    private int compareImplications(Implication inv1, Implication inv2) {
      int comparePredicate = compare(inv1.predicate(), inv2.predicate());
      if (comparePredicate != 0) {
        return comparePredicate;
      }

      return compare(inv1.consequent(), inv2.consequent());
    }
  }

  /**
   * Orders invariants by class, then variable names, then formula. If the formulas are the same,
   * compares the printed representation obtained from the format() method.
   */
  public static final class ClassVarnameFormulaComparator implements Comparator<Invariant> {

    Comparator<Invariant> classVarnameComparator = new ClassVarnameComparator();

    @Pure
    @Override
    public int compare(@NonPrototype Invariant inv1, @NonPrototype Invariant inv2) {
      int compareClassVarname = classVarnameComparator.compare(inv1, inv2);

      if (compareClassVarname != 0) {
        return compareClassVarname;
      }

      if (inv1.isSameInvariant(inv2)) {
        return 0;
      }

      int result = inv1.format().compareTo(inv2.format());

      // The purpose of the assertion below would seem to be to insist that
      // anything that doesn't return true to isSameInvariant() will not
      // produce the same written formula.  This can happen, however, if the
      // variables have a different order (as in function binary), but the
      // swapped variables are actually the same (since we create invariants
      // of the form f(a, a, a) because of equality sets.
      // assert result != 0
      //                   : "isSameInvariant() returned false "
      //                   + "(isSameFormula returned " + inv1.isSameFormula(inv2) + ")," + lineSep
      //                   + "but format().compareTo() returned 0:" + lineSep
      //                   + "  " + inv1.format() + lineSep + "      "  + inv1.repr() + lineSep
      //                   + "    " + inv1.ppt.parent.name + lineSep
      //                   + "  " + inv2.format() + lineSep + "      "  + inv2.repr() + lineSep
      //                   + "    " + inv1.ppt.parent.name + lineSep
      //                  ;

      return result;
    }
  }

  /**
   * Class used as a key to store invariants in a MAP where their equality depends on the invariant
   * representing the same invariant (i.e., their class is the same) and the same internal state
   * (when multiple invariants with the same class are possible).
   *
   * <p>Note that this is based on the Invariant type (i.e., class) and the internal state and not
   * on what ppt the invariant is in or what variables it is over. Thus, invariants from different
   * ppts are the same if they represent the same type of invariant.
   */
  public static class Match {

    public Invariant inv;

    public Match(Invariant inv) {
      this.inv = inv;
    }

    @EnsuresNonNullIf(result = true, expression = "#1")
    @Pure
    @Override
    public boolean equals(@GuardSatisfied Match this, @GuardSatisfied @Nullable Object obj) {
      if (!(obj instanceof Match)) {
        return false;
      }

      Match ic = (Match) obj;
      return ic.inv.match(inv);
    }

    @Pure
    @Override
    public int hashCode(@GuardSatisfied Match this) {
      return inv.getClass().hashCode();
    }
  }

  /**
   * Returns whether or not two invariants are of the same type. To be of the same type, invariants
   * must be of the same class. Some invariant classes represent multiple invariants (such as
   * FunctionBinary). They must also be the same formula. Note that invariants with different
   * formulas based on their samples (LinearBinary, Bounds, etc) will still match as long as the
   * mergeFormulaOk() method returns true.
   */
  public boolean match(@Prototype Invariant inv) {

    if (inv.getClass() == getClass()) {
      return inv.mergeFormulasOk() || isSameFormula(inv);
    } else {
      return false;
    }
  }

  /**
   * Returns whether or not the invariant matches the specified state. Must be overriden by
   * subclasses that support this. Otherwise, it returns true only if the state is null.
   */
  public boolean state_match(@NonPrototype Invariant this, Object state) {
    return (state == null);
  }

  /** Create a guarding predicate for a given invariant. Returns null if no guarding is needed. */
  public @Nullable @NonPrototype Invariant createGuardingPredicate(boolean install) {
    if (debugGuarding.isLoggable(Level.FINE)) {
      debugGuarding.fine("Guarding predicate being created for: ");
      debugGuarding.fine("  " + this.format());
    }

    // Find which VarInfos must be guarded
    List<VarInfo> mustBeGuarded = getGuardingList();

    if (mustBeGuarded.isEmpty()) {
      if (debugGuarding.isLoggable(Level.FINE)) {
        debugGuarding.fine("No guarding is needed, returning");
      }
      return null;
    }

    // This conjunction would look better if it was built up right-to-left.
    Invariant guardingPredicate = null;
    for (VarInfo vi : mustBeGuarded) {
      Invariant currentGuard = vi.createGuardingPredicate(install);
      if (currentGuard == null) {
        continue;
      }
      debugGuarding.fine(String.format("VarInfo %s guard is %s", vi, currentGuard));
      if (guardingPredicate == null) {
        guardingPredicate = currentGuard;
      } else {
        guardingPredicate = new AndJoiner(ppt.parent, guardingPredicate, currentGuard);
      }
      debugGuarding.fine(String.format("  predicate so far: %s", guardingPredicate));
    }

    // If the guarding predicate has been previously constructed, return it.
    // Otherwise, we will return the newly constructed one.
    // This algorithm is inefficient.
    if (guardingPredicate != null) { // equivalently: mustBeGuarded.size() > 1
      List<Invariant> joinerViewInvs = ppt.parent.joiner_view.invs;
      for (Invariant currentInv : joinerViewInvs) {
        if (currentInv.isSameInvariant(guardingPredicate)) {
          return currentInv;
        }
      }
    }
    return guardingPredicate;
  }

  /**
   * Return a list of all the variables that must be non-null in order for this invariant to be
   * evaluated. For instance, it this invariant is "a.b.c &gt; d.e" (where c and e are of integer
   * type), then it doesn't make sense to evaluate the invariant unless "a" is non-null, "a.b" is
   * non-null, and "d" is non-null. So, another way to write the invariant (in "guarded" form) would
   * be "a != null &amp;&amp; a.b != null &amp;&amp; d != null &amp;&amp; a.b.c &gt; d.e".
   */
  public List<VarInfo> getGuardingList(@NonPrototype Invariant this) {
    return getGuardingList(ppt.var_infos);
  }

  /**
   * Returns the union of calling VarInfo.getGuardingList on each element of the argument.
   *
   * @param varInfos an array of VarInfo
   * @return the union of calling VarInfo.getGuardingList on each element of the argument
   */
  public static List<VarInfo> getGuardingList(VarInfo[] varInfos) {
    List<VarInfo> guardingList = new ArrayList<>();

    for (int i = 0; i < varInfos.length; i++) {
      // debugGuarding.fine (varInfos[i]);
      guardingList.addAll(varInfos[i].getGuardingList());
      // debugGuarding.fine (guardingSet.toString());
    }

    return CollectionsPlume.withoutDuplicates(guardingList);
  }

  // This is called only from finally_print_the_invariants().
  // Nothing is ever done with the result except print it and discard it.
  /**
   * This procedure guards one invariant and returns the resulting guarded invariant (implication),
   * without placing it in any slice and without modifying the original invariant. Returns null if
   * the invariant does not need to be guarded.
   */
  public @Nullable @NonPrototype Invariant createGuardedInvariant(boolean install) {
    if (Daikon.dkconfig_guardNulls == "never") { // interned
      return null;
    }

    if (debugGuarding.isLoggable(Level.FINE)) {
      debugGuarding.fine("  Trying to add guard for: " + this + "; repr = " + repr());
    }
    if (isGuardingPredicate) {
      debugGuarding.fine("  Do not guard: this is a guarding predicate");
      return null;
    }
    Invariant guardingPredicate = createGuardingPredicate(install);
    if (debugGuarding.isLoggable(Level.FINE)) {
      if (guardingPredicate != null) {
        debugGuarding.fine("  Predicate: " + guardingPredicate.format());
        debugGuarding.fine("  Consequent: " + format());
      } else {
        debugGuarding.fine("  No implication needed");
      }
    }

    if (guardingPredicate == null) {
      return null;
    }

    Implication guardingImplication =
        GuardingImplication.makeGuardingImplication(ppt.parent, guardingPredicate, this, false);

    if (install) {
      if (!ppt.parent.joiner_view.hasImplication(guardingImplication)) {
        ppt.parent.joiner_view.addInvariant(guardingImplication);
      }
    }
    return guardingImplication;
  }

  /**
   * Instantiates (creates) an invariant of the same class on the specified slice. Must be
   * overridden in each class. Must be used rather than {@link #clone} so that checks in {@link
   * #instantiate} for reasonable invariants are done.
   *
   * <p>The implementation of this method is almost always {@code return new
   * <em>InvName</em>(slice);}
   *
   * @return the new invariant
   */
  protected abstract @NonPrototype Invariant instantiate_dyn(
      @Prototype Invariant this, PptSlice slice);

  /**
   * Returns whether or not this class of invariants is currently enabled.
   *
   * <p>Its implementation is almost always {@code return dkconfig_enabled;}.
   */
  public abstract boolean enabled(@Prototype Invariant this);

  /**
   * Returns whether or not the invariant is valid over the basic types in vis. This only checks
   * basic types (scalar, string, array, etc) and should match the basic superclasses of invariant
   * (SingleFloat, SingleScalarSequence, ThreeScalar, etc). More complex checks that depend on
   * variable details can be implemented in instantiate_ok().
   *
   * @see #instantiate_ok(VarInfo[])
   */
  public abstract boolean valid_types(@Prototype Invariant this, VarInfo[] vis);

  /**
   * Returns true if it makes sense to instantiate this invariant over the specified variables.
   * Checks details beyond what is provided by {@link #valid_types}. This should never be called
   * without calling valid_types() first (implementations should be able to presume that
   * valid_types() returns true).
   *
   * <p>This method does not have to be overridden; the default implementation in Invariant returns
   * true.
   *
   * @see #valid_types(VarInfo[])
   */
  public boolean instantiate_ok(@Prototype Invariant this, VarInfo[] vis) {
    return true;
  }

  // Every Invariant is either a regular Invariant, or a "prototype"
  // Invariant.  A prototype invariant is really a factory.  The
  // "instantiate" method should only be called on a prototype invariant,
  // and many methods should only be called on non-prototype methods.
  // Another (arguably better, though less convenient in certain ways)
  // design would not represent the factory as an Invariant object.  An
  // object never transitions at run time between being a factory/prototype
  // and being a normal invariant.
  //
  // Could we just use the class, such as Positive.class, as (a proxy for)
  // the factory?
  //  * That would require use of reflection to call the constructor, which
  //    is ugly.
  //  * The factory needs at least some state is needed, for example to
  //    distinguish between creating a division operator "a/b" vs. "b/a".
  //
  // Could the factories be represented by a separate class, unrelated in
  // the type hierarchy to Invariant?
  //  * That would probably be a better design.
  //  * instantiate_dyn would have to have more than just the single line that
  //    it is right now; longer code is more error-prone.
  //  * Not all the code for an invariant would be in a single class any
  //    more; but it could still all be in the same file, for example.
  //  * There are probably other difficulties that escape me at the moment.

  /**
   * Instantiates this invariant over the specified slice. The slice must not be null and its
   * variables must be valid for this type of invariant. Returns null if the invariant is not
   * enabled or if the invariant is not reasonable over the specified variables. Otherwise returns
   * the new invariant.
   */
  public @Nullable Invariant instantiate(@Prototype Invariant this, PptSlice slice) {

    assert isPrototype(); // receiver should be a "prototype" invariant
    assert slice != null;
    if (!valid_types(slice.var_infos)) {
      System.out.printf("this.getClass(): %s%n", this.getClass());
      System.out.printf("slice: %s%n", slice);
      System.out.printf(
          "slice.var_infos (length %d): %s%n", slice.var_infos.length, (Object) slice.var_infos);
      for (VarInfo vi : slice.var_infos) {
        System.out.printf("  var_info: %s %s%n", vi, vi.type);
      }
    }
    assert valid_types(slice.var_infos)
        : String.format("valid_types(%s) = false for %s", Arrays.toString(slice.var_infos), this);
    if (!enabled() || !instantiate_ok(slice.var_infos)) {
      return null;
    }
    Invariant inv = instantiate_dyn(slice);
    assert inv != null;
    if (inv.ppt == null) {
      // Avoid creating the message if the check succeeds
      assert inv.ppt != null : "invariant class " + inv.getClass();
    }
    return inv;
  }

  /**
   * Adds the specified sample to the invariant and returns the result.
   *
   * @param vt the sample to add to this invariant
   * @param count the number of occurrences of the sample to add to this invariant
   * @return the result of adding the samples to this invariant
   */
  public InvariantStatus add_sample(@NonPrototype Invariant this, ValueTuple vt, int count) {

    if (ppt instanceof PptSlice1) {

      VarInfo v = ppt.var_infos[0];
      UnaryInvariant unary_inv = (UnaryInvariant) this;
      return unary_inv.add(vt.getValue(v), vt.getModified(v), count);

    } else if (ppt instanceof PptSlice2) {

      VarInfo v1 = ppt.var_infos[0];
      VarInfo v2 = ppt.var_infos[1];
      BinaryInvariant bin_inv = (BinaryInvariant) this;
      return bin_inv.add_unordered(vt.getValue(v1), vt.getValue(v2), vt.getModified(v1), count);

    } else /* must be ternary */ {

      VarInfo v1 = ppt.var_infos[0];
      VarInfo v2 = ppt.var_infos[1];
      VarInfo v3 = ppt.var_infos[2];
      assert (this instanceof TernaryInvariant)
          : "invariant '" + format() + "' in slice " + ppt.name() + " is not ternary";
      TernaryInvariant ternary_inv = (TernaryInvariant) this;
      return ternary_inv.add(
          vt.getValue(v1), vt.getValue(v2), vt.getValue(v3), vt.getModified(v1), count);
    }
  }

  /** Check the rep invariants of this. */
  public void repCheck(@Prototype Invariant this) {}

  /**
   * Returns whether or not the invariant is currently active. This is used to identify those
   * invariants that require a certain number of points before they actually do computation (eg,
   * LinearBinary)
   *
   * <p>This is used during suppresion. Any invariant that is not active cannot suppress another
   * invariant.
   *
   * @return true if this invariant is currently active
   */
  @Pure
  public boolean isActive(@NonPrototype Invariant this) {
    return true;
  }

  // TODO: The logDetail and (especially) logOn methods are misleading,
  // because they are static but are very often called with an instance as
  // the receiver, suggesting that they have something to do with the
  // receiver.  This should be corrected.  -MDE

  /**
   * Returns true if detailed logging is on. Note that this check is not performed inside the
   * logging calls themselves, it must be performed by the caller.
   *
   * @return true if detailed logging is on
   * @see daikon.Debug#logDetail()
   * @see daikon.Debug#logOn()
   * @see daikon.Debug#log(Logger, Class, Ppt, String)
   */
  public static boolean logDetail() {
    return Debug.logDetail();
  }

  /**
   * Returns whether or not logging is on.
   *
   * @see daikon.Debug#logOn()
   */
  public static boolean logOn() {
    return Debug.logOn();
  }

  // Using `@link` leads to javadoc -Xdoclint:all crashing with:
  // "com.sun.tools.javac.code.Type$AnnotatedType cannot be cast to
  // com.sun.tools.javac.code.Type$ClassType"
  /**
   * Logs a description of the invariant and the specified msg via the logger as described in {@code
   * daikon.Debug#log(Logger, Class, Ppt, VarInfo[], String)}.
   *
   * @param log where to log the message
   * @param msg the message to log
   */
  // receiver needs to be initialized because subclass implementations will read their own fields
  public void log(@NonPrototype Invariant this, Logger log, String msg) {

    if (Debug.logOn()) {
      Debug.log(log, getClass(), ppt, msg);
    }
  }

  // Using `@link` leads to javadoc -Xdoclint:all crashing with:
  // "com.sun.tools.javac.code.Type$AnnotatedType cannot be cast to
  // com.sun.tools.javac.code.Type$ClassType"
  /**
   * Logs a description of the invariant and the specified msg via the logger as described in {@code
   * daikon.Debug#log(Logger, Class, Ppt, VarInfo[], String)}.
   *
   * @param format a format string
   * @param args the argumnts to the format string
   * @return whether or not it logged anything
   */
  @FormatMethod
  public boolean log(
      @UnknownInitialization(Invariant.class) @NonPrototype Invariant this,
      String format,
      @Nullable Object... args) {
    if (ppt != null) {
      String msg = format;
      if (args.length > 0) {
        msg = String.format(format, args);
      }
      return Debug.log(getClass(), ppt, msg);
    } else {
      return false;
    }
  }

  // Receiver must be fully initialized
  @SideEffectFree
  @Override
  public String toString(@GuardSatisfied Invariant this) {
    return format();
  }

  /**
   * Return a string representation of the given invariants.
   *
   * @param invs the invariants to get a string representation of
   * @return a string representation of the given invariants
   */
  public static String toString(@NonPrototype Invariant[] invs) {

    ArrayList<String> strings = new ArrayList<>(invs.length);
    for (int i = 0; i < invs.length; i++) {
      if (invs[i] == null) {
        strings.add("null");
      } else {
        strings.add(invs[i].format());
      }
    }
    return String.join(", ", strings);
  }

  /**
   * Used throughout Java family formatting of invariants.
   *
   * <p>Returns
   *
   * <p>"plume.FuzzyFloat.method(v1_name, v2_name)"
   *
   * <p>Where v1_name and v2_name are the properly formatted varinfos v1 and v2, under the given
   * format.
   */
  // [[ This method doesn't belong here. But where? ]]
  public static String formatFuzzy(String method, VarInfo v1, VarInfo v2, OutputFormat format) {

    StringBuilder results = new StringBuilder();
    return results
        .append("daikon.Quant.fuzzy.")
        .append(method)
        .append("(")
        .append(v1.name_using(format))
        .append(", ")
        .append(v2.name_using(format))
        .append(")")
        .toString();
  }

  @SuppressWarnings("unchecked") // casting method
  public static Class<? extends Invariant> asInvClass(Object x) {
    return (Class<? extends Invariant>) x;
  }

  /**
   * Returns true if this is an equality comparison. That is, returns true if this Invariant
   * satisfies the following conditions:
   *
   * <ul>
   *   <li>the Invariant is an EqualityComparison (its relationship is =, not &lt;, &le;, &gt;, or
   *       &ge;).
   *   <li>the invariant is statistically satisfied (its confidence is above the limit)
   * </ul>
   *
   * This does not consider PairwiseIntComparison to be an equality invariant.
   */
  public boolean isEqualityComparison() {
    if (!(this instanceof EqualityComparison)) {
      return false;
    }
    double chance_conf = ((EqualityComparison) this).eq_confidence();
    return chance_conf > Invariant.dkconfig_confidence_limit;
  }

  /**
   * Throws an exception if this object is invalid.
   *
   * @exception RuntimeException if representation invariant on this is broken
   */
  public void checkRep() {
    // very partial initial implementation
    for (VarInfo vi : ppt.var_infos) {
      vi.checkRep();
    }
  }

  /** Classes for which {@link #checkMergeOverridden} has been called. */
  public static IdentityHashMap<Class<?>, Boolean> checkedMergeOverridden = new IdentityHashMap<>();

  /**
   * Throws an exception if the class directly defines fields but does not override {@link #merge}.
   */
  private void checkMergeOverridden(
      @UnderInitialization(daikon.inv.Invariant.class) Invariant this) {
    Class<?> thisClass = getClass();
    if (!checkedMergeOverridden.containsKey(thisClass)) {
      checkedMergeOverridden.put(thisClass, true);

      // TODO: Could look at all fields and compare them to the fields of Invariant.class.
      Field[] declaredFields = thisClass.getDeclaredFields();
      List<Field> statefulFields = new ArrayList<>(4);
      for (Field declaredField : declaredFields) {
        if (Modifier.isStatic(declaredField.getModifiers())) {
          continue;
        }

        String fieldName = declaredField.getName();
        if (fieldName.equals("serialVersionUID")) {
          continue;
        }
        if (fieldName.startsWith("$")) {
          continue;
        }
        if (fieldName.startsWith("dkconfig_")) {
          continue;
        }
        if (fieldName.startsWith("debug")) {
          continue;
        }
        if (fieldName.endsWith("Cache")) {
          continue;
        }

        statefulFields.add(declaredField);
      }

      if (statefulFields.isEmpty()) {
        return;
      }

      try {
        @SuppressWarnings(
            "UnusedVariable" // Method is called for side effect, ignore return value, but give the
        // unused variable a name for documentation purposes.
        )
        Method mergeMethod = thisClass.getDeclaredMethod("merge", List.class, PptSlice.class);
        // `mergeMethod` is non-null, or else `NoSuchMethodException` was thrown.
      } catch (NoSuchMethodException e) {
        StringJoiner fields = new StringJoiner(", ");
        for (Field f : statefulFields) {
          fields.add(f.getName());
        }
        throw new Error(
            thisClass.getSimpleName()
                + " does not override `merge(List, PptTopLevel)`,"
                + " but these fields might store state: "
                + fields);
      }
    }
  }
}
