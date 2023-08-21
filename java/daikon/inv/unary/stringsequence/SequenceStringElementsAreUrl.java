package daikon.inv.unary.stringsequence;

import daikon.PptSlice;
import daikon.inv.DiscardInfo;
import daikon.inv.Invariant;
import daikon.inv.InvariantStatus;
import daikon.inv.OutputFormat;
import daikon.inv.unary.string.IsUrl;
import java.util.regex.Matcher;
import org.checkerframework.checker.interning.qual.Interned;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;
import typequals.prototype.qual.Prototype;

/**
 * Indicates that all elements of an array of strings are URLs. Prints as {@code All the elements of
 * x are URLs}
 */
public class SequenceStringElementsAreUrl extends SingleStringSequence {

  /** UID for serialization. */
  static final long serialVersionUID = 20230704L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /** Boolean. True iff SequenceStringElementsAreUrl invariants should be considered. */
  public static boolean dkconfig_enabled = true;

  /**
   * Creates a new SequenceStringElementsAreUrl.
   *
   * @param ppt the slice with the variable of interest
   */
  protected SequenceStringElementsAreUrl(PptSlice ppt) {
    super(ppt);
  }

  /** Creates a new prototype SequenceStringElementsAreUrl. */
  protected @Prototype SequenceStringElementsAreUrl() {
    super();
  }

  /** The prototype invariant. */
  private static @Prototype SequenceStringElementsAreUrl proto =
      new @Prototype SequenceStringElementsAreUrl();

  /**
   * Returns the prototype invariant.
   *
   * @return the prototype invariant
   */
  public static @Prototype SequenceStringElementsAreUrl get_proto() {
    return proto;
  }

  /** returns whether or not this invariant is enabled */
  @Override
  public boolean enabled() {
    return dkconfig_enabled;
  }

  /** instantiate an invariant on the specified slice */
  @Override
  protected SequenceStringElementsAreUrl instantiate_dyn(
      @Prototype SequenceStringElementsAreUrl this, PptSlice slice) {
    return new SequenceStringElementsAreUrl(slice);
  }

  // Don't write clone, because this.intersect is read-only
  // protected Object clone();

  @Override
  public String repr(@GuardSatisfied SequenceStringElementsAreUrl this) {
    return "SequenceStringElementsAreUrl " + varNames();
  }

  @SideEffectFree
  @Override
  public String format_using(
      @GuardSatisfied SequenceStringElementsAreUrl this, OutputFormat format) {
    return "All the elements of " + var().name() + " are URLs";
  }

  @Override
  public InvariantStatus check_modified(@Interned String @Interned [] a, int count) {
    for (int i = 0; i < a.length; i++) {
      Matcher matcher = IsUrl.PATTERN.matcher(a[i]);
      // The invariant is falsified if one of the elements of the array is NOT of type URL
      if (!matcher.matches()) {
        return InvariantStatus.FALSIFIED;
      }
    }

    return InvariantStatus.NO_CHANGE;
  }

  @Override
  public InvariantStatus add_modified(@Interned String @Interned [] a, int count) {
    return check_modified(a, count);
  }

  @Override
  protected double computeConfidence() {
    if (alwaysEmpty) {
      return Invariant.CONFIDENCE_UNJUSTIFIED;
    }

    return 1 - Math.pow(.1, ppt.num_samples());
  }

  /**
   * DiscardInfo is not used for this invariant
   *
   * @return null
   */
  @Pure
  public @Nullable DiscardInfo isObviousImplied() {
    return null;
  }

  @Pure
  @Override
  public boolean isSameFormula(Invariant other) {
    assert other instanceof SequenceStringElementsAreUrl;
    return true;
  }
}
