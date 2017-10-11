package daikon.inv.unary.scalar;

import daikon.*;
import daikon.inv.Invariant;
import daikon.inv.InvariantStatus;
import daikon.inv.OutputFormat;

/*>>>
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.dataflow.qual.*;
import typequals.*;
*/

// This invariant is true if the variable is always positive (greater than 0).
// This invariant is provided for pedagogical reasons only.

/**
 * Represents the invariant {@code x > 0} where {@code x} is a long scalar. This exists only as an
 * example for the purposes of the manual. It isn't actually used (it is replaced by the more
 * general invariant LowerBound).
 */
public class Positive extends SingleScalar {
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20040728L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /** Boolean. True iff Positive invariants should be considered. */
  public static boolean dkconfig_enabled = Invariant.invariantEnabledDefault;

  ///
  /// Required methods
  ///

  private Positive(PptSlice ppt) {
    super(ppt);
  }

  private /*@Prototype*/ Positive() {
    super();
  }

  private static /*@Prototype*/ Positive proto = new /*@Prototype*/ Positive();

  /** Returns the prototype invariant */
  public static /*@Prototype*/ Positive get_proto() {
    return proto;
  }

  /** returns whether or not this invariant is enabled */
  @Override
  public boolean enabled() {
    return dkconfig_enabled;
  }

  /** instantiate an invariant on the specified slice */
  @Override
  public Positive instantiate_dyn(/*>>> @Prototype Positive this,*/ PptSlice slice) {
    return new Positive(slice);
  }

  // A printed representation for user output
  /*@SideEffectFree*/
  @Override
  public String format_using(/*>>>@GuardSatisfied Positive this,*/ OutputFormat format) {
    return var().name() + " > 0";
  }

  @Override
  public InvariantStatus check_modified(long v, int count) {
    if (v <= 0) {
      return InvariantStatus.FALSIFIED;
    }
    return InvariantStatus.NO_CHANGE;
  }

  @Override
  public InvariantStatus add_modified(long v, int count) {
    return check_modified(v, count);
  }

  @Override
  protected double computeConfidence() {
    // Assume that every variable has a .5 chance of being positive by
    // chance.  Then a set of n values have a have (.5)^n chance of all
    // being positive by chance.
    return 1 - Math.pow(.5, ppt.num_samples());
  }

  /*@Pure*/
  @Override
  public boolean isSameFormula(Invariant other) {
    assert other instanceof Positive;
    return true;
  }
}
