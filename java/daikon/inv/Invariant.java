package daikon.inv;

import daikon.*;
import daikon.Debug;
import daikon.inv.unary.sequence.EltOneOf;
import daikon.inv.unary.stringsequence.EltOneOfString;
import daikon.inv.DiscardInfo;
import daikon.inv.filter.*;
import daikon.suppress.*;

import utilMDE.*;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.*;
import java.io.Serializable;

/**
 * Base implementation for Invariant objects.
 * Intended to be subclassed but not to be directly instantiated.
 * Rules/assumptions for invariants:
 *
 * <li> For each program point's set of VarInfos, there exists exactly
 * no more than one invariant of its type.  For example, between
 * variables a and b at PptTopLevel T, there will not be two instances
 * of invariant I(a, b).
 **/
public abstract class Invariant
  implements Serializable, Cloneable // but don't YOU clone it
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20030822L;

  /**
   * General debug tracer.
   **/
  public static final Logger debug = Logger.getLogger("daikon.inv.Invariant");

  /**
   * Debug tracer for printing invariants
   **/
  public static final Logger debugPrint = Logger.getLogger ("daikon.print");

  /**
   * Debug tracer for invariant flow.
   **/
  public static final Logger debugFlow = Logger.getLogger ("daikon.flow.flow");

  /**
   * Debug tracer for printing equality invariants
   **/
  public static final Logger debugPrintEquality = Logger.getLogger ("daikon.print.equality");

  /**
   * Debug tracer for isWorthPrinting() checks.
   **/
  public static final Logger debugIsWorthPrinting = Logger.getLogger("daikon.print.isWorthPrinting");

  /**
   * Debug tracer for guarding.
   **/
  public static final Logger debugGuarding = Logger.getLogger("daikon.guard");

  /**
   * Debug tracer for isObvious checks.
   **/
  public static final Logger debugIsObvious = Logger.getLogger("daikon.inv.Invariant.isObvious");

  /**
   * Real number between 0 and 1.  The invariant is displayed only if
   * the probability that the invariant occurred by chance is
   * less than this.  (May also be set
   * via --prob_limit switch to Daikon; refer to manual.)
   * Enabled only if configuration variable use_confidence is false.
   **/
  public static double dkconfig_probability_limit = .01;

  /**
   * Real number between 0 and 1.  The invariant is displayed only if
   * the confidence that the invariant did not occur by chance is
   * greater than this.  (May also be set
   * via --conf_limit switch to Daikon; refer to manual.)
   * Enabled only if configuration variable use_confidence is true.
   **/
  public static double dkconfig_confidence_limit = .99;

  /**
   * If true, use confidence-style filtering (computed from ModBitTracker).
   * Otherwise, use probability-style filtering (computed from ValueTracker).
   **/
  public static boolean dkconfig_use_confidence = false;

  /**
   * A boolean value. If true, Daikon's Simplify output (printed when
   * the --simplify_output flag is enabled, and used internally by
   * --suppress_redundant) will include new predicates representing
   * some complex relationships in invariants, such as lexical
   * ordering among sequences. If false, some complex relationships
   * will appear in the output as complex quantified formulas, while
   * others will not appear at all. When enabled, Simplify may be able
   * to make more inferences, allowing --suppress_redundant to
   * suppress more redundant invariants, but Simplify may also run
   * more slowly.
   **/
  public static boolean dkconfig_simplify_define_predicates = false;

  /**
   * Real number between 0 and 0.1.  The maximum relative difference
   * between two floats for fuzzy comparisons.  Larger values will
   * result in floats that are relatively farther apart being treated
   * as equal.  A value of 0 essentially disables fuzzy comparisons.
   * Specifically, if the equation 'abs (1 - f1/f2) <= perc' is true,
   * then the two doubles (f1 and f2) will be treated as equal by
   * Daikon.
   */
  public static double dkconfig_fuzzy_ratio = 0.0001;


  /**
   * The program point for this invariant, includes values, number of
   * samples, VarInfos, etc.
   **/
  public PptSlice ppt;

  // Has to be public so wrappers can read it.
  /**
   * True exactly if the invariant has been falsified:  it is guaranteed
   * never to hold (and should be either in the process of being destroyed
   * or about to be destroyed.  This should never be set directly; instead,
   * call destroy().
   **/
  public boolean falsified = false;

  /**
   * True once this invariant has flowed.  Prevents invariants from
   * flowing twice.
   **/
  public boolean flowed = false;


  // Should this just be a public field?  Probably yes for performance, but
  // not now.
  /**
   * The SuppressionLink to the Invariants that suppress this.  null if
   * unsuppressed.  Clones of this have this field set to null.
   **/
  private SuppressionLink suppressor;

  /**
   * Set of SuppressionLinks this suppresses (perhaps in conjunction
   * with some other invariants).  Each link holds a suppressed
   * invariant.
   * As a space optimization, is permitted to be null (meaning empty).
   * Clones of this have this field set to empty.
   **/
  private Set/*[SuppressionLink]*/ suppressees;

  /**
   * True if we've seen all values and should ignore further add() methods.
   * This is rather a hack and should be removed later.
   * Actually, it's not used any longer, except to be checked in assertions.
   **/
  // public boolean finished = false; // [INCR] (was just in assertions, is now bogus anyway)

  // Whether an invariant is a guarding predicate, that is, creately solely
  // for the purpose of ensuring invariants with variables that can be missing
  // do not cause exceptions when tested
  public boolean isGuardingPredicate = false;

  /**
   * The probability that this could have happened by chance alone. <br>
   *   0 = could never have happened by chance; that is, we are fully confident
   *       that this invariant is a real invariant
   **/
  public final static double CONFIDENCE_JUSTIFIED = 1;

  /**
   * (0..1) = greater to lesser likelihood of coincidence <br>
   *      0 = must have happened by chance
   **/
  public final static double CONFIDENCE_UNJUSTIFIED = 0;

  /**
   * -1 = delete this invariant; we know it's not true
   **/
  public final static double CONFIDENCE_NEVER = -1;


  /**
   * The probability that this could have happened by chance alone. <br>
   *   0 = could never have happened by chance; that is, we are fully confident
   *       that this invariant is a real invariant
   **/
  public final static double PROBABILITY_JUSTIFIED = 0;

  /**
   * (0..1) = lesser to greater likelihood of coincidence <br>
   *      1 = must have happened by chance
   **/
  public final static double PROBABILITY_UNJUSTIFIED = 1;

  /**
   * 3 = delete this invariant; we know it's not true
   **/
  public final static double PROBABILITY_NEVER = 3;

  /**
   * Return Invariant.CONFIDENCE_JUSTIFIED if x>=goal.
   * Return Invariant.CONFIDENCE_UNJUSTIFIED if x<=1.
   * For intermediate inputs, the result gives confidence that grades
   * between the two extremes.
   * See the discussion of gradual vs. sudden confidence transitions.
   **/
  public static final double conf_is_ge(double x, double goal) {
    if (x>=goal)
      return 1;
    if (x<=1)
      return 0;
    double result = 1 - (goal - x)/(goal-1);
    Assert.assertTrue(0 <= result && result <= 1, "conf_is_ge: bad result = " + result + " for (x=" + x + ", goal=" + goal + ")");
    return result;
  }

  /**
   * Return Invariant.PROBABILITY_JUSTIFIED if x>=goal.
   * Return Invariant.PROBABILITY_UNJUSTIFIED if x<=1.
   * For intermediate inputs, the result gives probability that grades
   * between the two extremes.
   * See the discussion of gradual vs. sudden probability transitions.
   **/
  public static final double prob_is_ge(double x, double goal) {
    if (x>=goal)
      return 0;
    if (x<=1)
      return 1;
    double result = (goal - x)/(goal-1);
    Assert.assertTrue(0 <= result && result <= 1, "prob_is_ge: bad result = " + result + " for (x=" + x + ", goal=" + goal + ")");
    return result;
  }


  /** Return the probability that both conditions are satisfied. */
  public static final double confidence_and(double c1, double c2) {
    Assert.assertTrue(0 <= c1 && c1 <= 1, "confidence_and: bad c1 = " + c1);
    Assert.assertTrue(0 <= c2 && c2 <= 1, "confidence_and: bad c2 = " + c2);

    double result = c1*c2;

    Assert.assertTrue(0 <= result && result <= 1, "confidence_and: bad result = " + result);
    return result;
  }

  /** Return the probability that all three conditions are satisfied. */
  public static final double confidence_and(double c1, double c2, double c3) {
    Assert.assertTrue(0 <= c1 && c1 <= 1, "confidence_and: bad c1 = " + c1);
    Assert.assertTrue(0 <= c2 && c2 <= 1, "confidence_and: bad c2 = " + c1);
    Assert.assertTrue(0 <= c3 && c3 <= 1, "confidence_and: bad c3 = " + c1);

    double result =  c1*c2*c3;

    Assert.assertTrue(0 <= result && result <= 1, "confidence_and: bad result = " + result);
    return result;
  }

  /** Return the probability that either condition is satisfied. */
  public static final double confidence_or(double c1, double c2) {
    // Not "1-(1-c1)*(1-c2)" because that can produce a value too large; we
    // don't want the result to be larger than the larger argument.
    return Math.max(c1, c2);
  }

  /** Return the probability that both conditions are satisfied. */
  public static final double prob_and(double p1, double p2) {
    Assert.assertTrue(0 <= p1 && p1 <= 1, "prob_and: bad p1 = " + p1);
    Assert.assertTrue(0 <= p2 && p2 <= 1, "prob_and: bad p2 = " + p2);

    // 1 - (1-p1)*(1-p2)
    double result = p1 + p2 - p1*p2;

    Assert.assertTrue(0 <= result && result <= 1, "prob_and: bad result = " + result);
    return result;
  }

  /** Return the probability that all three conditions are satisfied. */
  public static final double prob_and(double p1, double p2, double p3) {
    Assert.assertTrue(0 <= p1 && p1 <= 1, "prob_and: bad p1 = " + p1);
    Assert.assertTrue(0 <= p2 && p2 <= 1, "prob_and: bad p2 = " + p1);
    Assert.assertTrue(0 <= p3 && p3 <= 1, "prob_and: bad p3 = " + p1);

    double result =  1 - (1 - p1) * (1 - p2) * (1 - p3);

    Assert.assertTrue(0 <= result && result <= 1, "prob_and: bad result = " + result);
    return result;
  }

  /** Return the probability that either condition is satisfied. */
  public static final double prob_or(double p1, double p2) {
    // Not "p1*p2" because that can produce a value too small; we don't
    // want the result to be smaller than the smaller argument.
    return Math.min(p1, p2);
  }


  // Subclasses should set these; Invariant never does.

  /**
   * At least this many samples are required, or else we don't report any
   * invariant at all.  (Except that OneOf invariants are treated differently.)
   **/
  public final static int min_mod_non_missing_samples = 5;

  /**
   * @return true if the invariant has enough samples to have its
   * computed constants well-formed.  Is overridden in classes like
   * LinearBinary/Ternary and Upper/LowerBound.
   **/
  public boolean enoughSamples() {
    return true;
  }


  // There are two ways to compute justification.
  //  * The probability routines (getProbability and internal helper
  //    computeProbability) use ValueTracker information.
  //  * The confidence routines (getConfidence and internal helper
  //    computeConfidence) use ModBitTracker information.
  // Configuration variable dkconfig_use_confidence controls which of the
  // two techniques is used.

  // There are three probability routines:
  //  justified() is what most clients should call
  //  getProbability() gives the actual probability.  (Likewise for
  //    getConfidence().)  It used to cache results, but it does not
  //    do so any longer.
  //  computeProbability() in an internal helper method that does the
  //    actual work, but it should not be called externally, only by
  //    getProbability.  (Likewise for computeConfidence().)  It ignores
  //    whether the invariant is falsified.

  // There are two general approaches to computing probability/confidence
  // when there is a threshold (such as needing to see 10 samples):
  //  * Make the probability typically either 0 or 1, transitioning
  //    suddenly between the two as soon as the 10th sample is observed.
  //  * Make the probability transition more gradually; for instance, each
  //    sample chnages the probability by 10%.
  // The gradual approach has advantages and disadvantages:
  //  + Users can set the probability limit to see invariants earlier; this
  //    is simpler than figuring out all the thresholds to set.
  //  + Tools such as the operational difference for test suite generation
  //    are assisted by knowing whether they are getting closer to
  //    justification.
  //  - The code is a bit more complicated.


  /** A wrapper around getConfidence() or getProbability(). **/
  public final boolean justified() {
    if (dkconfig_use_confidence) {
      boolean just = (!falsified
                      && (getConfidence() >= dkconfig_confidence_limit));
      if (logOn())
        log ("justified = " + just + ", confidence = " + getConfidence());
      return (just);
    } else {
      boolean just = !falsified && enoughSamples() &&
        (getProbability() <= dkconfig_probability_limit);
      if (logOn())
        log ("justified = " + just + ", enoughSamples = " + enoughSamples()
             + ", probability = " + getProbability() + ", repr = " + repr() +
             ", ppt.num_values() = " + ppt.num_values()
             +", num_mod_samples = "
             + ppt.num_mod_samples());
      return (just);
    }
  }

  // If probability == PROBABILITY_NEVER, then this invariant can be eliminated.
  /**
   * Given that this invariant has been true for all values seen so far,
   * this method returns the probability that that situation has occurred
   * by chance alone.  The result is a value between 0 and 1 inclusive.  0
   * means that this invariant could never have occurred by chance alone;
   * we are fully confident that its truth is no coincidence.  1 means that
   * the invariant is certainly a happenstance, so the truth of the
   * invariant is not relevant and it should not be reported.  Values
   * between 0 and 1 give differing confidences in the invariant.
   * <p>
   *
   * As an example, if the invariant is "x!=0", and only one value, 22, has
   * been seen for x, then the conclusion "x!=0" is not justified.  But if
   * there have been 1,000,000 values, and none of them were 0, then we may
   * be confident that the property "x!=0" is not a coincidence.
   * <p>
   *
   * This method need not check the value of field "falsified", as the
   * caller does that.
   * <p>
   *
   * This method is a wrapper around computeProbability(), which does the
   * actual work.
   * @see #computeProbability()
   **/
  public final double getProbability() {
    Assert.assertTrue(! falsified);
    // if (falsified)
    //   return PROBABILITY_NEVER;
    double result = computeProbability();
    if (result < PROBABILITY_JUSTIFIED || result > PROBABILITY_NEVER) {
      // Can't print this.repr_prob(), as it may compute the probability!
      System.out.println("Bad invariant probability " + result + ": ");
      System.out.println(this.getClass());
      System.out.println(repr());
      System.out.println(this.format());
    }
    // System.out.println("getProbability: " + getClass().getName() + " " + ppt.varNames());
    Assert.assertTrue((result == PROBABILITY_JUSTIFIED)
                  || (result == PROBABILITY_UNJUSTIFIED)
                  || (result == PROBABILITY_NEVER)
                  || ((0 <= result) && (result <= 1))
                  // This can be expensive, so comment out.
                  // , getClass().getName() + ": " + repr()
                  );
    return result;
  }

  /**
   * This method computes the probability that this invariant occurred by chance.
   * Users should use getProbability() instead.
   * @see     #getProbability()
   **/
  protected abstract double computeProbability();


  // If confidence == CONFIDENCE_NEVER, then this invariant can be eliminated.
  /**
   * Given that this invariant has been true for all values seen so far,
   * this method returns the confidence that that situation has occurred
   * by chance alone.  The result is a value between 0 and 1 inclusive.  0
   * means that this invariant could never have occurred by chance alone;
   * we are fully confident that its truth is no coincidence.  1 means that
   * the invariant is certainly a happenstance, so the truth of the
   * invariant is not relevant and it should not be reported.  Values
   * between 0 and 1 give differing confidences in the invariant.
   * <p>
   *
   * As an example, if the invariant is "x!=0", and only one value, 22, has
   * been seen for x, then the conclusion "x!=0" is not justified.  But if
   * there have been 1,000,000 values, and none of them were 0, then we may
   * be confident that the property "x!=0" is not a coincidence.
   * <p>
   *
   * This method need not check the value of field "falsified", as the
   * caller does that.
   * <p>
   *
   * This method is a wrapper around computeConfidence(), which does the
   * actual work.
   * @see #computeConfidence()
   **/
  public final double getConfidence() {
    Assert.assertTrue(! falsified);
    // if (falsified)
    //   return CONFIDENCE_NEVER;
    double result = computeConfidence();
    if (result < CONFIDENCE_NEVER || result > CONFIDENCE_JUSTIFIED) {
      // Can't print this.repr_prob(), as it may compute the confidence!
      System.out.println("Bad invariant confidence " + result + ": ");
      System.out.println(this.getClass());
      System.out.println(repr());
      System.out.println(this.format());
    }
    // System.out.println("getConfidence: " + getClass().getName() + " " + ppt.varNames());
    Assert.assertTrue((result == CONFIDENCE_JUSTIFIED)
                  || (result == CONFIDENCE_UNJUSTIFIED)
                  || (result == CONFIDENCE_NEVER)
                  || ((0 <= result) && (result <= 1))
                  // This can be expensive, so comment out.
                  // , getClass().getName() + ": " + repr()
                  );
    return result;
  }

  /**
   * This method computes the confidence that this invariant occurred by chance.
   * Users should use getConfidence() instead.
   * @see     #getConfidence()
   **/
  protected abstract double computeConfidence();

  /**
   * Subclasses should override.  An exact invariant indicates that given
   * all but one variable value, the last one can be computed.  (I think
   * that's correct, anyway.)  Examples are IntComparison (when only
   * equality is possible), LinearBinary, FunctionUnary.
   * OneOf is treated differently, as an interface.
   * The result of this method does not depend on whether the invariant is
   * justified, destroyed, etc.
   **/
  public boolean isExact() {
    return false;
  }

  // Implementations of this need to examine all the data values already
  // in the ppt.  Or, don't put too much work in the constructor and instead
  // have the caller do that.
  protected Invariant(PptSlice ppt) {
    this.ppt = ppt;
    suppressor = null;
    flowed = false;
    suppressees = null;
  }

  /**
   * Mark this invariant as falsified.
   * Invariants must also call flow when they are falsified
   * Has to be public because of wrappers; do not call from outside world.
   * @see #flow(Invariant)
   **/
  public void destroy() {
    falsified = true;
    if (logOn() || PptSlice.debugFlow.isLoggable(Level.FINE))
      log (PptSlice.debugFlow, "Destroyed " + format());

    // Add invariant to discard list.  Note this may not be correct
    // given the comment below.
    if (PrintInvariants.print_discarded_invariants)
      ppt.parent.falsified_invars.add(this);

    // [INCR] Commented out because removeInvariant removes this from
    // the pptslice.  In V3, this happens after invariants are
    // checked.  Plus, destroy() may be called during such removal.
    // ppt.removeInvariant(this);
  }

  /**
   * Flow argument to all lower program points.
   * Has to be public because of wrappers (?); do not call from outside world.
   * @see #destroy
   **/
  private void flow(Invariant flowed) {
    ppt.addToFlow(flowed);
  }

  /**
   * Essentially the same as flow(this).  Useful way to flow oneself
   * without much hassle (as long as internal state is still OK).
   * Nice point of control in case we later have to tweak things when
   * flowing ourselves.
   **/
  private void flowThis() {
    flow(this);
  }

  /**
   * Essentially the same as flow(this.clone()).  Useful way to flow
   * oneself without much hassle (as long as internal state is still
   * OK).  Nice point of control in case we later have to tweak things
   * when flowing ourselves.  This method and destroy() are made
   * private because both of them require a call to addToChanged() for
   * correct suppression.
   **/
  private void flowClone() {
    Invariant flowed = (Invariant) this.clone();
    flow(flowed);
  }

  /**
   * Destroy this, add this to the list of Invariants to flow, and add
   * this to list of falsified or weakened invariants.
   **/
  public void destroyAndFlow () {
    if (debugFlow.isLoggable(Level.FINE) || logOn()) {
      log (debugFlow, "added to destroyed.");
    }
    flowThis();
    destroy();
    ppt.addToChanged (this);
  }

  /**
   * Add a copy of this to the list of Invariants to flow, and add
   * this to list of falsified or weakened invariants.  Why is this
   * different from flowClone?  Because the Invariant to flow is a
   * clone of this, but the Invariant that's weakened is this.  The
   * former is needed for flow, the latter for suppression.  By
   * contract, users should call cloneAndFlow rather than flowClone.
   **/
  public void cloneAndFlow() {
    // We must still do this to check suppression
    ppt.addToChanged (this);
    if (debugFlow.isLoggable(Level.FINE) || logOn()) {
      log (debugFlow, " added to changed.");
    }

    if (!flowed) {
      if (debugFlow.isLoggable(Level.FINE) || logOn()) {
        log (debugFlow, repr() + " added to flowed.");
      }
      flowClone();
      flowed = true;
    }
  }

  /**
   * Do nothing special, except disconnect the clone from
   * this.suppressor and this.suppressees.  Overridden to remove
   * exception from declaration
   **/
  public Object clone() {
    try {
      Invariant result = (Invariant) super.clone();
      result.suppressor = null;
      result.suppressees = null;
      result.flowed = false;
      return result;
    } catch (CloneNotSupportedException e) {
      throw new Error(); // can never happen
    }
  }

  /**
   * Take an invariant and transfer it into a new PptSlice.
   * @param new_ppt must have the same arity and types
   * @param permutation gives the varinfo array index mapping in the
   * new ppt
   **/
  public Invariant transfer(PptSlice new_ppt,
                            int[] permutation
                            )
  {
    // Check some sanity conditions
    Assert.assertTrue(new_ppt.arity == ppt.arity);
    Assert.assertTrue(permutation.length == ppt.arity);
    for (int i=0; i < ppt.arity; i++) {
      VarInfo oldvi = ppt.var_infos[i];
      VarInfo newvi = new_ppt.var_infos[permutation[i]];
      // We used to check that all 3 types were equal, but we can't do
      // that anymore, because with equality, invariants may get
      // transferred between old and new VarInfos of different types.
      // They are, however, comparable
      Assert.assertTrue (oldvi.comparableNWay(newvi));
    }

    Invariant result;
    // Clone it
    result = (Invariant) this.clone();

    // Fix up the fields
    result.ppt = new_ppt;
    // Let subclasses fix what they need to
    result = result.resurrect_done(permutation);

    if (logOn())
      result.log ("Created " + result.format() + " via transfer from "
                  + format() + "using permutation "
                  + ArraysMDE.toString (permutation)
                  + " old_ppt = " + VarInfo.toString (ppt.var_infos)
                  + " new_ppt = " + VarInfo.toString (new_ppt.var_infos));
    //if (debug.isLoggable(Level.FINE))
    //    debug.fine ("Invariant.transfer to " + new_ppt.name() + " "
    //                 + result.repr());

    return result;
  }

  /**
   * Clones the invariant and then permutes it as specified.  Normally
   * used to make child invariant match the variable order of the parent
   * when merging invariants bottom up.
   */

  public Invariant clone_and_permute (int[] permutation) {

    Invariant result = (Invariant) this.clone();
    result = result.resurrect_done (permutation);

    return (result);
  }

  /**
   * Take a falsified invariant and resurrect it in a new PptSlice.
   * @param new_ppt must have the same arity and types
   * @param permutation gives the varinfo array index mapping
   **/
  public Invariant resurrect(PptSlice new_ppt,
                             int[] permutation
                             )
  {
    // Check some sanity conditions
    Assert.assertTrue(falsified);
    Assert.assertTrue(new_ppt.arity == ppt.arity);
    Assert.assertTrue(permutation.length == ppt.arity);
    for (int i=0; i < ppt.arity; i++) {
      VarInfo oldvi = ppt.var_infos[i];
      VarInfo newvi = new_ppt.var_infos[permutation[i]];
      // We used to check that all 3 types were equal, but we can't do
      // that anymore, because with equality, invariants may get
      // resurrected between old and new VarInfos of different types.
      // They are, however, comparable
      Assert.assertTrue(oldvi.comparableNWay(newvi));
    }

    Invariant result;
    // Clone it
    result = (Invariant) this.clone();

    // Fix up the fields
    result.falsified = false;
    result.ppt = new_ppt;
    // Let subclasses fix what they need to
    result = result.resurrect_done(permutation);

    return result;
  }

  /**
   * Merge the invariants in invs to form a new invariant.  This implementation
   * merely returns a clone of the first invariant in the list.  This is
   * correct for simple invariants whose equation or statistics don't depend
   * on the actual samples seen.  It should be overriden for more complex
   * invariants (eg, bound, oneof, linearbinary, etc).
   *
   * @param invs    List of invariants to merge.  The invariants must all be
   *                of the same type and should come from the children of
   *                ppt.  They should also all be permuted to match the variable
   *                order in ppt.
   * @param ppt     program point that will contain the new invariant
   *
   * @return the merged invariant or null if the invariants didn't represent
   * the same invariant.
   */
  public Invariant merge (List invs, PptSlice ppt) {


    Invariant first = (Invariant) invs.get(0);
    Invariant result = (Invariant) first.clone();
    result.ppt = ppt;
    result.log ("Merged '" + result.format() + "' from " + invs.size()
                + " child invariants");

    // Make sure that each invariant was really of the same type
    if (Assert.enabled) {
      Match m = new Match (result);
      for (int i = 1; i < invs.size(); i++ )
        Assert.assertTrue (m.equals (new Match ((Invariant) invs.get(i))));
    }

    return (result);

  }

  /**
   * Called on the new invariant just before resurrect() returns it to
   * allow subclasses to fix any information they might have cached
   * from the old Ppt and VarInfos.
   **/
  protected abstract Invariant resurrect_done(int[] permutation);

  // Regrettably, I can't declare a static abstract method.
  // // The return value is probably ignored.  The new Invariant installs
  // // itself on the PptSlice, and that's what really matters (right?).
  // public static abstract Invariant instantiate(PptSlice ppt);

  public boolean usesVar(VarInfo vi) {
    return ppt.usesVar(vi);
  }

  public boolean usesVar(String name) {
    return ppt.usesVar(name);
  }

  public boolean usesVarDerived(String name) {
    return ppt.usesVarDerived(name);
  }

  // Not used as of 1/31/2000
  // // For use by subclasses.
  // /** Put a string representation of the variable names in the StringBuffer. */
  // public void varNames(StringBuffer sb) {
  //   // sb.append(this.getClass().getName());
  //   ppt.varNames(sb);
  // }

  /** Return a string representation of the variable names. */
  final public String varNames() {
    return ppt.varNames();
  }

  final public String name() {
    return this.getClass().getName() + varNames();
  }

  // repr()'s output should not include result of getProbability, because
  // repr() may be called from computeProbability or elsewhere for
  // debugging purposes.
  /**
   * For printing invariants, there are two interfaces:
   * repr gives a low-level representation
   * (repr_prop also prints the probability), and
   * format gives a high-level representation for user output.
   **/
  public String repr() {
    // A better default would be to use reflection and print out all
    // the variable names.
    return getClass() + varNames() + ": " + format();
  }

  /**
   * For printing invariants, there are two interfaces:
   * repr gives a low-level representation
   * (repr_prop also prints the probability), and
   * format gives a high-level representation for user output.
   **/
  public String repr_prob() {
    return repr()
      + "; probability = " + getProbability()
      // + "; confidence = " + getConfidence()
      ;
  }

  /**
   * Enumeration type for output style.
   * (Should this be somewhere else?)
   **/
  public final static class OutputFormat
  {
    /* The standard, concise Daikon output format */
    public static final OutputFormat DAIKON = new OutputFormat("Daikon");
    /* ESC/Java's annotation language */
    public static final OutputFormat ESCJAVA = new OutputFormat("ESC/Java");
    /* Simplify theorem prover */
    public static final OutputFormat SIMPLIFY = new OutputFormat("Simplify");
    /* IOA language */
    public static final OutputFormat IOA = new OutputFormat("IOA");
    /* IOA language, sans invariant numbering */
    public static final OutputFormat IOATEST = new OutputFormat("IOA_test");
    /* Java boolean expression */
    public static final OutputFormat JAVA = new OutputFormat("Java");
    /* Java Modeling Language */
    public static final OutputFormat JML = new OutputFormat("JML");
    /* Design-By-Contract for Java (used by Parasoft JContract) */
    public static final OutputFormat DBCJAVA = new OutputFormat("DBC/Java");
    /* Whole names as single C/Java style indentifiers (currently just
     * for single VarInfoNames) */
    public static final OutputFormat IDENTIFIER
      = new OutputFormat("Identifier");

    private final String name;
    public final String toString() { return "OutputFormat:" + name; }

    // Nobody should ever construct these
    private OutputFormat(String name) {
      this.name = name;
    }
  }

  /**
   * For printing invariants, there are two interfaces:
   * repr gives a low-level representation
   * (repr_prop also prints the probability), and
   * format gives a high-level representation for user output.
   **/
  public String format() {
    return format_using(OutputFormat.DAIKON);
  }

  public abstract String format_using(OutputFormat format);

  /**
   * @return conjuction of mapping the same function of our
   * expresssions's VarInfos, in general.  Subclasses may override if
   * they are able to handle generally-inexpressible properties in
   * special-case ways.
   *
   * @see VarInfo#isValidEscExpression
   **/
  public boolean isValidEscExpression() {
    for (int i=0; i < ppt.var_infos.length; i++) {
      if (! ppt.var_infos[i].isValidEscExpression()) {
        return false;
      }
    }
    return true;
  }

  /**
   * @return standard "format needs to be implemented" for the given
   * requested format.  Made public so cores can call it.
   **/
  public String format_unimplemented(OutputFormat request) {
    if ((request == OutputFormat.IOA) && debugPrint.isLoggable(Level.FINE)) {
      debugPrint.fine ("Format_ioa: " + this.toString());
    }
    String classname = this.getClass().getName();
    return "warning: method " + classname + ".format(" + request + ")"
      + " needs to be implemented: " + format();
  }

  /**
   * @return standard "format is not expressible" (indicating that the
   * formalism is not powerful enough to express the logical formula) for
   * the given requested format.  Made public so cores can call it.
   **/
  public String format_inexpressible(OutputFormat request) {
    if ((request == OutputFormat.IOA) && debugPrint.isLoggable(Level.FINE)) {
      debugPrint.fine ("Format_ioa: " + this.toString());
    }
    String classname = this.getClass().getName();
    return "warning: method " + classname + ".format(" + request + ")"
      + "cannot be expressed in this format: " + format();
  }

  /**
   * @return standard "too few samples for to have interesting
   * invariant" for the requested format. For machine-readable
   * formats, this is just "true". An optional string argument, if
   * supplied, is a human-readable description of the invariant in its
   * uninformative state, which will be added to the message.
   **/
  public String format_too_few_samples(OutputFormat request, String attempt) {
    if (request == OutputFormat.SIMPLIFY) {
      return "(AND)";
    } else if (request == OutputFormat.IOA ||
               request == OutputFormat.JAVA ||
               request == OutputFormat.ESCJAVA ||
               request == OutputFormat.JML ||
               request == OutputFormat.DBCJAVA ) {
      return "true";
    }
    String classname = this.getClass().getName();
    if (attempt == null) {
      attempt = varNames();
    }
    return "warning- too few samples for " + classname
      + " invariant- " + attempt;
  }

  /**
   * Convert a floating point value into the weird Modula-3-like
   * floating point format that the Simplify tool requires.
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
   * Conver a long integer value into a format that Simplify can
   * use. If the value is too big to fit in a signed 32-bit int, we
   * have to turn it into floating point, or else Simplify will
   * be unable to parse it. Unfortunately, this can lose precision.
   **/
  public static String simplify_format_long(long l) {
    if (l >= -32000 && l <= 32000) {
      // Note that the above range is actually smaller than the
      // real range of [-2147483648..2147483647], since Simplify can
      // get in trouble close to the boundary (try
      // > (BG_PUSH (< 2147483647 n))
      // to get an internal assertion failure)
      // For that matter, try
      // > (BG_PUSH (>= x -1073741825))
      // > (BG_PUSH (<= x 1073741825))
      // > (OR)
      // Or, close to the square root of the boundary:
      // > (BG_PUSH (EQ x 56312))
      // > (BG_PUSH (EQ y (* y x)))
      return "" + l;
    } else {
      return simplify_format_double((double)l);
    }
  }

  /**
   * Convert a string value into the weird |-quoted format that the
   * Simplify tool requires. (Note that Simplify doesn't distinguish
   * between variables, symbolic constants, and strings, so we prepend
   * "_string_" to avoid collisions with variables and other symbols).
   */
  public static String simplify_format_string(String s) {
    if (s == null)
      return "null";
    StringBuffer buf = new StringBuffer("|_string_");
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
      StringBuffer summ_buf = new StringBuffer(s.substring(0, 50));
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
      if (c == '\n')
        buf.append("\\n");
      else if (c == '\r')
        buf.append("\\r");
      else if (c == '\t')
        buf.append("\\t");
      else if (c == '\f')
        buf.append("\\f");
      else if (c == '\\')
        buf.append("\\\\");
      else if (c == '|')
        buf.append("\\|");
      else if (c >= ' ' && c <= '~')
        buf.append(c);
      else {
        buf.append("\\");
        // AFAIK, Simplify doesn't glork Unicode, so lop off all but
        // the low 8 bits
        String octal = Integer.toOctalString(c & 0xff);
        // Also, Simplify only accepts octal escapes with exactly 3 digits
        while (octal.length() < 3)
          octal = "0" + octal;
        buf.append(octal);
      }
    }
    buf.append("|");
    return buf.toString();
  }

  // This should perhaps be merged with some kind of PptSlice comparator.
  /**
   * Compare based on arity, then printed representation.
   **/
  public static final class InvariantComparatorForPrinting implements Comparator {
    public int compare(Object o1, Object o2) {
      if (o1 == o2)
        return 0;
      Invariant inv1 = (Invariant)o1;
      Invariant inv2 = (Invariant)o2;

      // Guarding implications should compare as if they were without the
      // guarding predicate

      if (inv1 instanceof GuardingImplication)
        inv1 = ((GuardingImplication)inv1).right;
      if (inv2 instanceof GuardingImplication)
        inv2 = ((GuardingImplication)inv2).right;

      // Put equality invariants first
      if ((inv1 instanceof Comparison) && (! (inv2 instanceof Comparison)))
        return -1;
      if ((! (inv1 instanceof Comparison)) && (inv2 instanceof Comparison))
        return 1;

      // Assert.assertTrue(inv1.ppt.parent == inv2.ppt.parent);
      VarInfo[] vis1 = inv1.ppt.var_infos;
      VarInfo[] vis2 = inv2.ppt.var_infos;
      int arity_cmp = vis1.length - vis2.length;
      if (arity_cmp != 0)
        return arity_cmp;
      // Comparing on variable index is wrong in general:  variables of the
      // same name may have different indices at different program points.
      // However, it's safe if the invariants are from the same program
      // point.  Also, it is nice to avoid changing the order of variables
      // from that of the data trace file.

      if (inv1.ppt.parent == inv2.ppt.parent) {
        for (int i=0; i<vis1.length; i++) {
          int tmp = vis1[i].varinfo_index - vis2[i].varinfo_index;
          if (tmp != 0) {
            // This can happen when variable names have been changed by
            // VarInfo.simplify_expression().  For now, hope for the best.
            // (That is, hope it doesn't produce multiple invariants or
            // confused formatting.)
            // if (inv1.format().equals(inv2.format())) {
            //   System.out.println("ICFP says different, but same formatting:");
            //   System.out.println("  " + inv1.format() + " " + inv1.repr() + " at " + inv1.ppt.name());
            //   System.out.println(" var #" + vis1[i].varinfo_index + " = " + vis1[i].name + " = " + vis1[i]);
            //   System.out.println("  " + inv2.format() + " " + inv2.repr() + " at " + inv2.ppt.name());
            //   System.out.println(" var #" + vis2[i].varinfo_index + " = " + vis2[i].name + " = " + vis2[i]);
            // }

            // // Debugging
            // System.out.println("ICFP: compare var "
            //                    + vis1[i].name.name() + " (index " + vis1[i].varinfo_index + ")"
            //                    + " to " + vis2[i].name.name() + " (index " + vis2[i].varinfo_index + ")"
            //                    + " => " + tmp
            //                    + "\t\tfor " + inv1.format() + ", " + inv2.format());
            // System.out.println("Vars for " + inv1.format() + ": " + inv1.repr());
            // System.out.println("Vars for " + inv2.format() + ": " + inv2.repr());

            return tmp;
          }
        }
      } else {
        // // Debugging
        // System.out.println("ICFP: different parents for " + inv1.format() + ", " + inv2.format());

        for (int i=0; i<vis1.length; i++) {
          String name1 = vis1[i].name.name();
          String name2 = vis2[i].name.name();
          if (name1.equals(name2)) {
            continue;
          }
          int name1in2 = inv2.ppt.parent.indexOf(name1);
          int name2in1 = inv1.ppt.parent.indexOf(name2);
          int cmp1 = (name1in2 == -1) ? 0 : vis1[i].varinfo_index - name1in2;
          int cmp2 = (name2in1 == -1) ? 0 : vis2[i].varinfo_index - name2in1;
          int cmp = MathMDE.sign(cmp1) + MathMDE.sign(cmp2);
          if (cmp != 0)
            return cmp;
        }
      }

      // Sort OneOf invariants earlier than others
      if ((inv1 instanceof OneOf) && (! (inv2 instanceof OneOf)))
        return -1;
      if ((! (inv1 instanceof OneOf)) && (inv2 instanceof OneOf))
        return 1;

      // System.out.println("ICFP: default rule yields "
      //                    + inv1.format().compareTo(inv2.format())
      //                    + " for " + inv1.format() + ", " + inv2.format());
      return inv1.format().compareTo(inv2.format());
    }
  }

  /**
   * @return true iff the two invariants represent the same
   * mathematical formula.  Does not consider the context such as
   * variable names, confidences, sample counts, value counts, or
   * related quantities.  As a rule of thumb, if two invariants format
   * the same, this method returns true.  Furthermore, in many cases,
   * if an invariant does not involve computed constants (as "x&gt;c" and
   * "y=ax+b" do for constants a, b, and c), then this method vacuously
   * returns true.
   *
   * @exception RuntimeException if other.getClass() != this.getClass()
   **/
  public boolean isSameFormula(Invariant other) {
    return false;
  }

  /**
   * Returns whether or not it is possible to merge invariants of the same
   * class but with different formulas when combining invariants from lower
   * ppts to build invariants at upper program points.  Invariants that
   * have this characteristic (eg, bound, oneof) should override this
   * function.  Note that invariants that can do this, normally need special
   * merge code as well (to merge the different formulas into a single formula
   * at the upper point
   */
  public boolean mergeFormulasOk () {
    return (false);
  }

  public static interface IsSameInvariantNameExtractor
  {
    public VarInfoName getFromFirst(VarInfo var1);
    public VarInfoName getFromSecond(VarInfo var2);
  }

  public static class DefaultIsSameInvariantNameExtractor
    implements IsSameInvariantNameExtractor
  {
    public VarInfoName getFromFirst(VarInfo var1)  { return var1.name; }
    public VarInfoName getFromSecond(VarInfo var2) { return var2.name; }
  }
  private static final IsSameInvariantNameExtractor defaultIsSameInvariantNameExtractor = new DefaultIsSameInvariantNameExtractor();

  /**
   * @return true iff the argument is the "same" invariant as this.
   * Same, in this case, means a matching type, formula, and variable
   * names.
   **/
  public boolean isSameInvariant(Invariant inv2)
  {
    return isSameInvariant(inv2, defaultIsSameInvariantNameExtractor);
  }

  /**
   * @param name_extractor lambda to extract the variable name from the VarInfos
   * @return true iff the argument is the "same" invariant as this.
   * Same, in this case, means a matching type, formula, and variable
   * names.
   **/
  public boolean isSameInvariant(Invariant inv2,
                                 IsSameInvariantNameExtractor name_extractor)
  {
    Invariant inv1 = this;

    // Can't be the same if they aren't the same type
    if (!inv1.getClass().equals(inv2.getClass())) {
      return false;
    }

    // Can't be the same if they aren't the same formula
    if (!inv1.isSameFormula(inv2)) {
      return false;
    }

    // System.out.println("isSameInvariant(" + inv1.format() + ", " + inv2.format() + ")");

    // The variable names much match up, in order

    VarInfo[] vars1 = inv1.ppt.var_infos;
    VarInfo[] vars2 = inv2.ppt.var_infos;

//      if (PrintInvariants.debugFiltering.isLoggable(Level.FINE)) {
//        PrintInvariants.debugFiltering.fine ("\t\t----------------\n");
//      }

    Assert.assertTrue(vars1.length == vars2.length); // due to inv type match already
    for (int i=0; i < vars1.length; i++) {
      VarInfo var1 = vars1[i];
      VarInfo var2 = vars2[i];

      // Do the easy check first
      if (name_extractor.getFromFirst(var1).equals(name_extractor.getFromSecond(var2))) {
        continue;
      }

      // The names "match" iff there is an intersection of the names
      // of equal variables.
      Vector all_vars1 = new Vector(); // var1.canonicalRep().equalTo(); // [INCR]
      Vector all_vars2 = new Vector(); // var2.canonicalRep().equalTo(); // [INCR]
      all_vars1.add(var1); // all_vars1.add(var1.canonicalRep()); // [INCR]
      all_vars1.add(var2); // all_vars2.add(var2.canonicalRep()); // [INCR]
      Vector all_vars_names1 = new Vector(all_vars1.size());
      for (Iterator iter = all_vars1.iterator(); iter.hasNext(); ) {
        VarInfo elt = (VarInfo) iter.next();
        VarInfoName viname = name_extractor.getFromFirst(elt);
        all_vars_names1.add(viname);
      }
      boolean intersection = false;
      for (Iterator iter = all_vars2.iterator(); iter.hasNext(); ) {
        VarInfo elt = (VarInfo) iter.next();
        VarInfoName viname = name_extractor.getFromSecond(elt);

//      if (PrintInvariants.debugFiltering.isLoggable(Level.FINE)) {
//       PrintInvariants.debugFiltering.fine ("\t\t" + viname.toString() + " <--> " + all_vars_names1.toString() + "\n");
//      }

        intersection = all_vars_names1.contains(viname);
        if (intersection) {
          break;
        }
      }
      if (!intersection) {
        return false;
      }
    }

    // System.out.println("TRUE: isSameInvariant(" + inv1.format() + ", " + inv2.format() + ")");

    // the type, formula, and vars all matched
//      if (PrintInvariants.debugFiltering.isLoggable(Level.FINE)) {
//        PrintInvariants.debugFiltering.fine ("\tdecided " + this.format() + "\n");
//        PrintInvariants.debugFiltering.fine ("\t is the same as " + inv2.format() + "\n");
//      }
    return true;
  }


  /**
   * @return true iff the two invariants represent mutually exclusive
   * mathematical formulas -- that is, if one of them is true, then the
   * other must be false.  This method does not consider the context such
   * as variable names, confidences, sample counts, value counts, or
   * related quantities.
   **/
  public boolean isExclusiveFormula(Invariant other) {
    return false;
  }


  /**
   * Look up a previously instantiated Invariant.
   **/
  // This implementation should be made more efficient, because it's used in
  // suppression.  We should somehow index invariants by their type.
  public static Invariant find(Class invclass, PptSlice ppt) {
    for (Iterator itor = ppt.invs.iterator(); itor.hasNext(); ) {
      Invariant inv = (Invariant) itor.next();
      if (inv.getClass() == invclass)
        return inv;
    }
    return null;
  }

  /**
   * Look up a previously instantiated Invariant but only if unsuppressed.
   **/
  public static Invariant findUnsuppressed(Class invclass, PptSlice ppt) {
    for (Iterator itor = ppt.invs.iterator(); itor.hasNext(); ) {
      Invariant inv = (Invariant) itor.next();
      if (inv.getClass() == invclass && inv.getSuppressor() == null)
        return inv;
    }
    return null;
  }



  // Diff replaced by package daikon.diff

  //    String diff(Invariant other) {
  //      throw new Error("Unimplemented invariant diff for " + this.getClass() + " and " + other.getClass() + ": " + this.format() + " " + other.format());
  //    }

  //     # Possibly add an optional "args=None" argument, for formatting.
  //     def diff(self, other):
  //         """Returns None or a description of the difference."""
  //         # print "diff(invariant)"
  //         inv1 = self
  //         inv2 = other
  //         assert inv1.__class__ == inv2.__class__
  //         if inv1.is_unconstrained() and inv2.is_unconstrained():
  //             return None
  //         if inv1.is_unconstrained() ^ inv2.is_unconstrained():
  //             return "One is unconstrained but the other is not"
  //         if inv1.one_of and inv2.one_of and inv1.one_of != inv2.one_of:
  //             return "Different small number of values"
  //         if inv1.can_be_None ^ inv2.can_be_None:
  //             return "One can be None but the other cannot"
  //         # return "invariant.diff: no differences"  # debugging
  //         return None



  ///////////////////////////////////////////////////////////////////////////
  /// Tests about the invariant (for printing)
  ///

  // DO NOT OVERRIDE.  Should be declared "final", but the "final" is
  // omitted to allow for easier testing.
  public boolean isWorthPrinting() {
    return InvariantFilters.isWorthPrintingFilter().shouldKeep(this) == null;
  }

  /**
   * Like isWorthPrinting, but doesn't check whether the invariant is controlled.
   **/
  final public boolean isWorthPrinting_sansControlledCheck() {
    return InvariantFilters.isWorthPrintingFilter_sansControlledCheck()
             .shouldKeep(this) == null;
  }

  final public String isWorthPrinting_sansControlledCheck_debug() {
    return
      "iwpscc(" + format() + " @ " + ppt.name()
      + ") <=== "
      + enoughSamples()
      // + " " + (! hasNonCanonicalVariable()) [INCR]
      // + " " + (! hasOnlyConstantVariables()) [INCR]
      + " " + (! isObvious().shouldDiscard())
      + " " + justified()
      // + " " + isWorthPrinting_PostconditionPrestate() [INCR]
      ;
  }

  // This used to be final, but I want to override in EqualityInvariant.
  // (Also in IntEqual, etc. -MDE 5/6/2002)
  /** @return true if this invariant involves a non-canonical variable **/
  /* [INCR] ...
  public boolean hasNonCanonicalVariable() {
    VarInfo[] vis = ppt.var_infos;
    for (int i=0; i<vis.length; i++) {
      if (! vis[i].isCanonical()) {
        return true;
      }
    }
    return false;
  }
  */ // ... [INCR]

  /**
   * @return true if this invariant involves only constant variables
   *         and is a comparison
   **/
  /* [INCR] ...
  public boolean hasOnlyConstantVariables() {
    VarInfo[] varInfos = ppt.var_infos;
    for (int i=0; i < varInfos.length; i++) {
      if (! varInfos[i].isConstant())
        return false;
    }

    // At this point, we know all variables are constant.
    Assert.assertTrue(this instanceof OneOf ||
                      this instanceof Comparison ||
                      this instanceof Equality ||
                      this instanceof DummyInvariant
                      , "Unexpected invariant with all vars constant: "
                      + this + "  " + repr_prob() + "  " + format()
                      );
    if (this instanceof Comparison) {
      //      Assert.assertTrue(! IsEqualityComparison.it.accept(this));
      if (debugPrint.isLoggable(Level.FINE))
        debugPrint.fine ("  [over constants:  " + this.repr_prob() + " ]");
      return true;
    }
    return false;
  }
  */ // ... [INCR]

  ////////////////////////////////////////////////////////////////////////////
  // Static and dynamic checks for obviousness

  /**
   * Return true if this invariant is necessarily true from a fact
   * that can be determined statically (i.e., the decls files) (e.g.,
   * by being from a certain derivation).  Intended to be overridden
   * by subclasses.  Should only do static checking, because
   * suppression should do the dynamic checking.
   *
   * <p> This method is final because children of Invariant should be
   * extending isObviousStatically(VarInfo[]) because it is more
   * general.
   **/
  public final DiscardInfo isObviousStatically() {
    return isObviousStatically(this.ppt.var_infos);
  }

  /**
   * Return true if this invariant is necessarily true from a fact
   * that can be determined statically -- for the given varInfos
   * rather than the varInfos of this.  Conceptually, this means "is
   * this invariant statically obvious if its VarInfos were switched
   * with vis?"  Intended to be overridden by subclasses.  Should only
   * do static checking.
   * @param vis The VarInfos this invariant is obvious over.  The
   * position and data type of the variables is the *same* as that of
   * this.ppt.var_infos.
   * @pre vis.length == this.ppt.var_infos.length
   **/
  public DiscardInfo isObviousStatically(VarInfo[] vis) {
    return new DiscardInfo();
  }

  /**
   * Return true if this invariant and all equality combinations of
   * its member variables are necessarily true from a fact that can be
   * determined statically (i.e., the decls files).  For example, a ==
   * b, and f(a) is obvious, but f(b) is not.  In that case, this
   * method on f(a) would return false.  If f(b) is also obvious, then
   * this method would return true.
   **/
  // This is used because we cannot decide to non-instantiate some
  // invariants just because isObviousStatically is true, since some
  // of the member variables may be equal to non-obvious varInfos.  If
  // we were to non-instantiate, we could not copy an invariant to the
  // non-obvious VarInfos should they split off from the obvious one.
  // Of course, it's expensive to examine every possible permutation
  // of VarInfos and their equality set, so a possible conservative
  // approximation is to simply return false.
  public boolean isObviousStatically_AllInEquality() {
    if (!isObviousStatically().shouldDiscard()) return false;

    for (int i = 0; i < ppt.var_infos.length; i++) {
      if (ppt.var_infos[i].equalitySet.getVars().size() > 1) return false;
    }
    return true;
  }

  /**
   * Return true if this invariant and some equality combinations of
   * its member variables are statically obvious.  For example, a ==
   * b, and f(a) is obvious, so is f(b).  We use the someInEquality
   * (or least interesting) method during printing so we only print an
   * invariant if all its variables are interesting, since a single,
   * static, non interesting occurance means all the equality
   * combinations aren't interesting.
   * @return the VarInfo array that contains the VarInfos that showed
   * this invariant to be obvious.  The contains variables that are
   * elementwise in the same equality set as this.ppt.var_infos.  Can
   * be null if no such assignment exists.
   **/
  public DiscardInfo isObviousStatically_SomeInEquality() {
    DiscardInfo result = isObviousStatically();
    if (result.shouldDiscard()) return result;
    return isObviousStatically_SomeInEqualityHelper (this.ppt.var_infos,
                                                     new VarInfo[this.ppt.var_infos.length],
                                                     0);
  }

  /**
   * Recurse through vis and generate the cartesian product of
   **/
  private DiscardInfo isObviousStatically_SomeInEqualityHelper(VarInfo[] vis,
                                                             VarInfo[] assigned,
                                                             int position) {
    if (position == vis.length) {
      if (debugIsObvious.isLoggable(Level.FINE)) {
        StringBuffer sb = new StringBuffer();
        sb.append ("  isObviousStatically_SomeInEquality: ");
        for (int i = 0; i < vis.length; i++) {
          sb.append (assigned[i].name.name() + " ");
        }
        debugIsObvious.fine (sb.toString());
      }

      return isObviousStatically(assigned);
    } else {
      for (Iterator iSet = vis[position].equalitySet.getVars().iterator();
           iSet.hasNext(); ) {
        VarInfo vi = (VarInfo) iSet.next();
        assigned[position] = vi;
        DiscardInfo temp =
          isObviousStatically_SomeInEqualityHelper (vis, assigned, position + 1);
        if (temp.shouldDiscard()) return temp;
      }
      return new DiscardInfo();
    }
  }

  /**
   * Return true if this invariant is necessarily true from a fact
   * that can be determined statically (i.e., the decls files) or
   * dynamically (after checking data).  Intended not to be overriden,
   * because sub classes should override isObviousStatically or
   * isObviousDynamically.  Should only do static checking, because
   * suppression should do the dynamic checking.
   **/
  public final DiscardInfo isObvious() {
    // Actually actually, we'll eliminate invariants as they become obvious
    // rather than on output; the point of this is to speed up computation.
    // // Actually, we do need to check isObviousDerived after all because we
    // // add invariants that might be obvious, but might also turn out to be
    // // even stronger (and so not obvious).  We don't know how the invariant
    // // turns out until after testing it.
    // // // We don't need to check isObviousDerived because we won't add
    // // // obvious-derived invariants to lists in the first place.
    DiscardInfo staticResult = isObviousStatically_SomeInEquality();
    if (staticResult.shouldDiscard()) {
      if (debugPrint.isLoggable(Level.FINE))
        debugPrint.fine ("  [obvious:  " + repr_prob() + " ]");
      return staticResult;
    } else {
      DiscardInfo dynamicResult = isObviousDynamically_SomeInEquality();
      if (dynamicResult.shouldDiscard()) {
        if (debugPrint.isLoggable(Level.FINE))
          debugPrint.fine ("  [obvious:  " + repr_prob() + " ]");
        return dynamicResult;
      } else {
        return new DiscardInfo();
      }
    }
  }

  /**
   * Return true if this invariant is necessarily true from a fact
   * that can be determined dynamically (after checking data) -- for
   * the given varInfos rather than the varInfos of this.
   * Conceptually, this means "is this invariant dynamically obvious
   * if its VarInfos were switched with vis?"  .  Intended to be
   * overriden by subclasses so they can filter invariants after
   * checking.  Since this method is dynamic, it should only be called
   * after all processing.
   **/
  public DiscardInfo isObviousDynamically(VarInfo[] vis) {
    Assert.assertTrue (!Daikon.isInferencing);
    if (isReflexive(vis))
      return new DiscardInfo(this, DiscardCode.obvious,
                             "Two or more variables are equal");
    return new DiscardInfo();
  }

  private boolean isReflexive(VarInfo[] vis) {
    if (vis.length < 2)
      return false;
    else if (vis.length == 2)
      return vis[0] == vis[1];
    else if (vis.length == 3)
      return vis[0] == vis[1] || vis[1] == vis[2] || vis[0] == vis[2];
    else {
      Assert.assertTrue(false, "Unexpected more-than-ternary invariant");
      for (int i = 1; i < vis.length; i++) {
        for (int j = 0; j < i; j++) {
          if (vis[i] == vis[j]) {
            return true;
          }
        }
      }
      return false;
    }
  }

  /**
   * Return true if more than one of the variables in the invariant
   * are the same variable. We create such invariants for the purpose
   * of equality set processing, but they aren't intended for
   * printing; there should be invariants with the same meaning but
   * lower arity instead. For instance, we don't need "x = x + x"
   * because we have "x = 0" instead.
   *
   * Actually, this isn't strictly true: we don't have an invariant
   * "a[] is a palindrome" corresponding to "a[] is the reverse of
   * a[]", for instance.
   **/
  public boolean isReflexive() {
    return isReflexive(ppt.var_infos);
  }

  /**
   * Return true if this invariant is necessarily true from a fact
   * that can be determined dynamically (after checking data).  Since
   * this method is dynamic, it should only be called after all
   * processing.
   *
   * <p> This method is final because subclasses should extend
   * isObviousDynamically(VarInfo[]) since that method is more general.
   **/
  public final DiscardInfo isObviousDynamically() {
    Assert.assertTrue (!Daikon.isInferencing);
    return isObviousDynamically (ppt.var_infos);
  }

  /**
   * Return true if this invariant and some equality combinations of
   * its member variables are dynamically obvious.  For example, a ==
   * b, and f(a) is obvious, so is f(b).  We use the someInEquality
   * (or least interesting) method during printing so we only print an
   * invariant if all its variables are interesting, since a single,
   * dynamic, non interesting occurance means all the equality
   * combinations aren't interesting.
   * @return the VarInfo array that contains the VarInfos that showed
   * this invariant to be obvious.  The contains variables that are
   * elementwise in the same equality set as this.ppt.var_infos.  Can
   * be null if no such assignment exists.
   **/
  public DiscardInfo isObviousDynamically_SomeInEquality() {
    DiscardInfo result = isObviousDynamically();
    if (result.shouldDiscard()) return result;
    return isObviousDynamically_SomeInEqualityHelper (this.ppt.var_infos,
                                                     new VarInfo[this.ppt.var_infos.length],
                                                     0);
  }

  /**
   * Recurse through vis (an array of leaders) and generate the cartesian
   * product of their equality sets; in other words, every combination of
   * one element from each equality set.  For each such combination, test
   * isObviousDynamically; if any test is true, then return that
   * combination.  The combinations are generated via recursive calls to
   * this routine.
   **/
  private DiscardInfo isObviousDynamically_SomeInEqualityHelper(VarInfo[] vis,
                                                             VarInfo[] assigned,
                                                             int position) {
    if (position == vis.length) {
      // base case
      if (debugIsObvious.isLoggable(Level.FINE)) {
        StringBuffer sb = new StringBuffer();
        sb.append ("  isObviousDynamically_SomeInEquality: ");
        for (int i = 0; i < vis.length; i++) {
          sb.append (assigned[i].name.name() + " ");
        }
        debugIsObvious.fine (sb.toString());
      }
      return isObviousDynamically (assigned);
    } else {
      // recursive case
      for (Iterator iSet = vis[position].equalitySet.getVars().iterator();
           iSet.hasNext(); ) {
        VarInfo vi = (VarInfo) iSet.next();
        assigned[position] = vi;
        DiscardInfo temp =
          isObviousDynamically_SomeInEqualityHelper (vis, assigned, position + 1);
        if (temp.shouldDiscard()) return temp;
      }
      return new DiscardInfo();
    }
  }


  /**
   * @return true if this invariant is controlled by another invariant
   **/
  /* [INCR]
  public boolean isControlled() {
    Vector controllers = this.find_controlling_invariants();
    return (controllers.size() > 0);
  }
  */ // ... [INCR]

  /**
   * @return true if this invariant is only over prestate variables .
   */
  public boolean isAllPrestate() {
    return ppt.allPrestate();
  }

  /**
   * @return true if is is ok to suppress this invariant while samples
   * are being processed.  Should be overriden by those invariants for
   * which this is not ok (which are those that keep internal state
   * information that would be lost if they were suppressed)
   */
  public boolean inProcessSuppressOk() {
    return true;
  }

  /**
   * @return a prestate invariant that implies this as a postcondition
   * or null if no such prestate invariant exists.
   * For example, if an entry point has the invariant x+3=y, and this
   * invariant is the corresponding exit point invariant orig(x)+3=orig(y),
   * then this method would return the Invariant corresponding to x+3=y.
   **/
  /* [INCR]
  public Invariant isImpliedPostcondition() {
    PptTopLevel topLevel = ppt.parent;
    if (topLevel.entry_ppt() != null) { // if this is an exit point invariant
      Iterator entryInvariants = topLevel.entry_ppt().getInvariants().iterator(); // unstable
      while (entryInvariants.hasNext()) {
        Invariant entryInvariant = (Invariant) entryInvariants.next();
        // If entryInvariant with orig() applied to everything matches this invariant
        if (entryInvariant.isSameInvariant( this, preToPostIsSameInvariantNameExtractor)) {
          if (PrintInvariants.debugFiltering.isLoggable(Level.FINE)) {
            PrintInvariants.debugFiltering.fine ("\tImplied by precond: " + entryInvariant.format() + " (from " + entryInvariant.ppt.parent.name + ")\n");
          }
          return entryInvariant;
        }
      }
    }
    return null;
  }

  public boolean isWorthPrinting_PostconditionPrestate()
  {
    PptTopLevel pptt = ppt.parent;

    if (Daikon.suppress_implied_postcondition_over_prestate_invariants) {
      if (pptt.entry_ppt != null) {
        Iterator entry_invs = pptt.entry_ppt.invariants_iterator(); // unstable
        while (entry_invs.hasNext()) {
          Invariant entry_inv = (Invariant) entry_invs.next();
          // If entry_inv with orig() applied to everything matches this
          if (entry_inv.isSameInvariant(this, preToPostIsSameInvariantNameExtractor)) {
            if (entry_inv.isWorthPrinting_sansControlledCheck()) {
              if (debugIsWorthPrinting.isLoggable(Level.FINE)) {
                debugIsWorthPrinting.fine ("isWorthPrinting_PostconditionPrestate => false for " + format());
              }
              return false;
            }
          }
        }
      }
    }
    return true;
  }
  // ... [INCR]

  /**
   * Used in isImpliedPostcondition() and isWorthPrinting_PostconditionPrestate().
   **/
  /*  [INCR]
  private final static IsSameInvariantNameExtractor preToPostIsSameInvariantNameExtractor =
    new DefaultIsSameInvariantNameExtractor() {
        public VarInfoName getFromFirst(VarInfo var)
        { return super.getFromFirst(var).applyPrestate(); }
      };
  */ // ... [INCR]

  /**
   * Returns a Vector[Invariant] which are the sameInvariant as this,
   * drawn from the invariants of this.ppt.parent.controllers.
   **/
  /*  [INCR]
  public Vector find_controlling_invariants()
  {
    // We used to assume there was at most one of these, but that
    // turned out to be wrong.  If this ppt has more equality
    // invariants than the controller, two different invariants can
    // match.  For example, a controller might say "a > 0", "b > 0" as
    // two different invariants, but if this ppt also has "a == b"
    // then both invariants should be returned.  This especailly
    // matters if, for example, "a > 0" was obvious (and thus wouldn't
    // suppress this invariant).
    Vector results = new Vector();

    if (logOn() || debugIsWorthPrinting.isLoggable(Level.FINE)) {
      log (debugIsWorthPrinting, "find_controlling_invariants: " + format());
    }
    PptTopLevel pptt = ppt.parent;

    // Try to match inv against all controlling invariants
    Iterator controllers = pptt.controlling_ppts.pptIterator();
    while (controllers.hasNext()) {
      PptTopLevel controller = (PptTopLevel) controllers.next();
      if (logOn() || debugIsWorthPrinting.isLoggable(Level.FINE)) {
        log (debugIsWorthPrinting, "Looking for controller of " + format()
             + " in " + controller.name);
      }
      Iterator candidates = controller.invariants_iterator(); // unstable
      while (candidates.hasNext()) {
        Invariant cand_inv = (Invariant) candidates.next();
        if (isSameInvariant(cand_inv)) {
          if (logOn() || debugIsWorthPrinting.isLoggable(Level.FINE)) {
            log (debugIsWorthPrinting, "Controller found: "
                 + cand_inv.format() + " [worth printing: "
                 + cand_inv.isWorthPrinting() + "]]");
          }
          results.add(cand_inv);
        }
        if (logDetail() || debugIsWorthPrinting.isLoggable(Level.FINE)) {
          log (debugIsWorthPrinting, "Failed candidate: " + cand_inv.format());
        }
      }
    }

    return results;
  }
  */ // ... [INCR]
  /*  [INCR]
  // For reproducible results when debugging
  static Comparator invComparator = new Invariant.ClassVarnameComparator();
  public Vector find_controlling_invariants_sorted() {
    Vector unsorted = find_controlling_invariants();
    Invariant[] invs = (Invariant[]) unsorted.toArray(new Invariant[0]);
    Arrays.sort(invs, invComparator);
    Vector result = new Vector(invs.length);
    for (int i=0; i<invs.length; i++)
      result.add(invs[i]);
    return result;
  }
  */ // ... [INCR]


  // The notion of "interesting" embodied by this method is
  // unclear. You'd probably be better off using
  // hasUninterestingConstant(), or some other filter.
  // Uninteresting invariants will override this method to return
  // false
  public boolean isInteresting() {
    return true;
  }


  /** This is the test that's planned to replace the poorly specified
   * "isInteresting" check. In the future, the set of interesting
   * constants might be determined by a static analysis of the source
   * code, but for the moment, we only consider -1, 0, 1, and 2 as
   * interesting.
   *
   * Intuitively, the justification for this test is that an invariant
   * that includes an uninteresting constant (say, "size(x[]) < 237")
   * is likely to be an artifact of the way the program was tested,
   * rather than a statement that would in fact hold over all possible
   * executions. */
  public boolean hasUninterestingConstant() {
    return false;
  }

  // Orders invariants by class, then by variable names.  If the
  // invariants are both of class Implication, they are ordered by
  // comparing the predicate, then the consequent.
  public static final class ClassVarnameComparator implements Comparator {
    public int compare(Object o1, Object o2) {
      Invariant inv1 = (Invariant) o1;
      Invariant inv2 = (Invariant) o2;

      if (inv1 instanceof Implication && inv2 instanceof Implication)
        return compareImplications((Implication) inv1, (Implication) inv2);

      int compareClass = compareClass(inv1, inv2);
      if (compareClass != 0)
        return compareClass;

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
          if (cmp != 0)
            return cmp;
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
      if (vars1.length != vars2.length)
        Assert.assertTrue(vars1.length == vars2.length,
                          "Bad type match: " + inv1.format() + " vs. " +
                          inv2.format());

      for (int i=0; i < vars1.length; i++) {
        VarInfo var1 = vars1[i];
        VarInfo var2 = vars2[i];
        int compare = var1.name.compareTo(var2.name);
        if (compare != 0)
          return compare;
      }

      // All the variable names matched
      return 0;
    }

    private int compareImplications(Implication inv1, Implication inv2) {
      int comparePredicate = compare(inv1.predicate(), inv2.predicate());
      if (comparePredicate != 0)
        return comparePredicate;

      return compare(inv1.consequent(), inv2.consequent());
    }
  }

  /**
   * Orders invariants by class, then variable names, then formula.
   * If the formulas are the same, compares the printed representation
   * obtained from the format() method.
   **/
  public static final class ClassVarnameFormulaComparator
    implements Comparator {

    Comparator classVarnameComparator = new ClassVarnameComparator();

    public int compare(Object o1, Object o2) {
      int compareClassVarname = classVarnameComparator.compare(o1, o2);

      if (compareClassVarname != 0) {
        return compareClassVarname;
      }

      Invariant inv1 = (Invariant) o1;
      Invariant inv2 = (Invariant) o2;

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
      // Assert.assertTrue(result != 0
      //                   , "isSameInvariant() returned false "
      //                   + "(isSameFormula returned " + inv1.isSameFormula(inv2) + "),\n"
      //                   + "but format().compareTo() returned 0:\n"
      //                   + "  " + inv1.format() + "\n      "  + inv1.repr() + "\n"
      //                   + "    " + inv1.ppt.parent.name + "\n"
      //                   + "  " + inv2.format() + "\n      "  + inv2.repr() + "\n"
      //                   + "    " + inv1.ppt.parent.name + "\n"
      //                  );

      return result;
    }
  }

  /**
   * Class used as a key to store invariants in a MAP where their
   * equality depends on the invariant representing the same invariant
   * (ie, their class is the same) and the same internal state (when
   * multiple invariants with the same class are possible)
   *
   * Note that this is based on the Invariant type (ie, class) and the
   * internal state and not on what ppt the invariant is in or what
   * variables it is over.  Thus, invariants from different ppts are
   * the same if they represent the same type of invariant.
   */
  public static class Match {

    public Invariant inv;

    public Match (Invariant inv) {
      this.inv = inv;
    }

    public boolean equals (Object obj) {
      if (!(obj instanceof Match))
        return (false);

      Match ic = (Match) obj;
      if (ic.inv.getClass() ==  inv.getClass())
        return (inv.mergeFormulasOk() || inv.isSameFormula (ic.inv));
      else
        return (false);
    }

    public int hashCode() {
      return (inv.getClass().hashCode());
    }
  }


  // This function creates a guarding predicate for a given invariant
  public Invariant createGuardingPredicate() {
    VarInfo varInfos[] = ppt.var_infos;

    if (debugGuarding.isLoggable(Level.FINE)) {
      debugGuarding.fine ("Guarding predicate being created for: ");
      debugGuarding.fine (this.format());
    }

    // Find which VarInfos must be guarded
    List mustBeGuarded = getGuardingList(varInfos);

    if (mustBeGuarded.isEmpty()) {
      if (debugGuarding.isLoggable(Level.FINE)) {
        debugGuarding.fine ("Left predicate is empty, returning");
      }
      return null;
    }

    // Hard to decide what PptSlice to associate with
    // VarInfo temp = (VarInfo)i.next();
    // debugGuarding.fine ("First VarInfo: " + temp);
    Invariant guardingPredicate = ((VarInfo)mustBeGuarded.get(0)).createGuardingPredicate(ppt.parent);
    // debugGuarding.fine (guardingPredicate.format_using(OutputFormat.DAIKON));
    Assert.assertTrue(guardingPredicate != null);

    for (int i=1; i<mustBeGuarded.size(); i++) {
      VarInfo current = (VarInfo)mustBeGuarded.get(i);
      // debugGuarding.fine ("Another VarInfo: " + current);
      Invariant currentGuard = current.createGuardingPredicate(ppt.parent);
      // debugGuarding.fine (currentGuard.toString());

      Assert.assertTrue(currentGuard != null);

      guardingPredicate = new AndJoiner(ppt.parent, guardingPredicate, currentGuard);
    }

    // Must eliminate the dupliaction of guarding prefixes, this is liable to be slow
    // We only care if there is more than one var info, otherwise we are guarenteed that
    // there is no duplicate by VarInfo.createGuardingPredicate()
    if (mustBeGuarded.size() > 1) {
      Invariants joinerViewInvs = ppt.parent.joiner_view.invs;
      for (int i=0; i<joinerViewInvs.size(); i++) {
        Invariant currentInv = (Invariant)joinerViewInvs.get(i);
        if (currentInv.isSameInvariant(guardingPredicate)) {
          return currentInv;
        }
      }
    }
    return guardingPredicate;
  }

  // Gets a list of all the variables that must be guarded for this
  // invariant
  public static List getGuardingList(VarInfo varInfos[]) {
    List guardingList = new GuardingVariableList();

    for (int i=0; i<varInfos.length; i++) {
      // debugGuarding.fine (varInfos[i]);
      guardingList.addAll(varInfos[i].getGuardingList());
      // debugGuarding.fine (guardingSet.toString());
    }

    return guardingList;
  }


  ///////////////////////////////////////////////////////////////////////////
  /// Suppression
  ///

  /**
   * Get the SuppressionLink that is suppressing this.
   * @return can be null if there is no suppressor.
   **/
  public SuppressionLink getSuppressor () {
    return suppressor;
  }

  /**
   * Set the suppressor for this.
   **/
  public void setSuppressor (SuppressionLink sl) {
    suppressor = sl;
  }

  /** The number of suppressees of this. **/
  public int numSuppressees() {
    if (suppressees == null) {
      return 0;
    }
    return suppressees.size();
  }

  /**
   * Get the SuppressionLinks that this is suppressing.
   * Returns an unmodifiable collection.
   * @return never null.
   **/
  public Set getSuppressees () {
    if (suppressees == null) {
      return Collections.EMPTY_SET;
    }
    return Collections.unmodifiableSet (suppressees);
  }

  /**
   * Add a suppressee to this's suppressed list.
   * @param sl The link to add.  Must not already be linked to.
   **/
  public void addSuppressee (SuppressionLink sl) {
    if (suppressees == null) {
      suppressees = new LinkedHashSet();
    }
    Assert.assertTrue (!(suppressees.contains(sl)));
    suppressees.add (sl);
  }

  /**
   * Remove a suppressee from this's suppressed list.
   * @param sl The link to remove.  Must be part of this.suppresses.
   **/
  public void removeSuppressee (SuppressionLink sl) {
    Assert.assertTrue (suppressees.contains(sl));
    suppressees.remove (sl);
  }

  private static final SuppressionFactory[] defaultSuppressionFactories =
    new SuppressionFactory[] {
      SelfSuppressionFactory.getInstance()
    };

  /**
   * The typical implementation calls super.getSuppressionFactories,
   * then augments that with additional factories specific to the
   * implementing class.  This method should be cheap such as
   * returning a static variable, which is set in advance.
   **/
  public SuppressionFactory[] getSuppressionFactories() {
    return defaultSuppressionFactories;
  }

  /**
   * Check the rep invariants of this.
   **/
  public void repCheck() {
    if (suppressor != null) suppressor.repCheck();
    if (suppressees != null) {
      for (Iterator i = suppressees.iterator(); i.hasNext(); ) {
        SuppressionLink sl = (SuppressionLink) i.next();
        sl.repCheck();
      }
    }
  }

  /**
   * Returns whether or not the invariant is currently active.  This is
   * used to identify those invariants that require a certain number
   * of points before they actually do computation (eg, LinearBinary)
   *
   * This is used during suppresion.  Any invariant that is not active
   * cannot suppress another invariant
   */
  public boolean isActive() {
    return (true);
  }


  /**
   * Returns whether or not detailed logging is on.  Note that this check
   * is not performed inside the logging calls themselves, it must be
   * performed by the caller.
   *
   * @see daikon.Debug#logDetail()
   * @see daikon.Debug#logOn()
   * @see daikon.Debug#log(Logger, Class, Ppt, String)
   */

  public static boolean logDetail () {
    return (Debug.logDetail());
  }

  /**
   * Returns whether or not logging is on.
   *
   * @see daikon.Debug#logOn()
   */

  public static boolean logOn() {
    return (Debug.logOn());
  }

  /**
   * Logs a description of the invariant and the specified msg via the
   * log4j logger as described in {@link daikon.Debug#log(Logger, Class, Ppt,
   * VarInfo[], String)}
   */

  public void log (Logger debug, String msg) {

    Debug.log (debug, getClass(), ppt, msg);
  }


 /**
  * Logs a description of the invariant and the specified msg via the
  * log4j logger as described in {@link daikon.Debug#log(Logger, Class, Ppt,
  * VarInfo[], String)}
  *
  * @return whether or not it logged anything
  */

  public boolean log (String msg) {
    return (Debug.log (getClass(), ppt, msg));
  }

  public String toString() {
    return format();
  }
}


//     def format(self, args=None):
//         if self.one_of:
//             # If it can be None, print it only if it is always None and
//             # is an invariant over non-derived variable.
//             if self.can_be_None:
//                 if ((len(self.one_of) == 1)
//                     and self.var_infos):
//                     some_nonderived = false
//                     for vi in self.var_infos:
//                      some_nonderived = some_nonderived or not vi.is_derived
//                     if some_nonderived:
//                         return "%s = uninit" % (args,)
//             elif len(self.one_of) == 1:
//                 return "%s = %s" % (args, self.one_of[0])
//             ## Perhaps I should unconditionally return this value;
//             ## otherwise I end up printing ranges more often than small
//             ## numbers of values (because when few values and many samples,
//             ## the range always looks justified).
//             # If few samples, don't try to infer a function over the values;
//             # just return the list.
//             elif (len(self.one_of) <= 3) or (self.samples < 100):
//                 return "%s in %s" % (args, util.format_as_set(self.one_of))
//         return None
