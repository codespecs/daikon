package daikon.inv.unary.stringsequence;

import daikon.PptSlice;
import daikon.inv.DiscardInfo;
import daikon.inv.Invariant;
import daikon.inv.InvariantStatus;
import daikon.inv.OutputFormat;
import daikon.inv.unary.string.IsEmail;
import java.util.List;
import java.util.regex.Matcher;
import org.checkerframework.checker.interning.qual.Interned;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;
import typequals.prototype.qual.Prototype;

/**
 * Indicates that all elements of an array of strings are emails. Prints as {@code All the elements
 * of x are emails}.
 */
public class SequenceStringElementsAreEmail extends SingleStringSequence {

  /** UID for serialization. */
  static final long serialVersionUID = 20230704L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /** Boolean. True iff SequenceStringElementsAreEmail invariants should be considered. */
  public static boolean dkconfig_enabled = true;

  /**
   * True if the array is always empty. Without this property, the invariant would be considered
   * true if all the arrays are empty.
   */
  private boolean alwaysEmpty = true;

  /**
   * True if all the elements of the array are null. Without this property, the invariant would be
   * reported if all the arrays contain only null elements.
   */
  private boolean allElementsAreNull = true;

  /**
   * Creates a new SequenceStringElementsAreEmail.
   *
   * @param ppt the slice with the variable of interest
   */
  protected SequenceStringElementsAreEmail(PptSlice ppt) {
    super(ppt);
  }

  /** Creates a new prototype SequenceStringElementsAreEmail. */
  protected @Prototype SequenceStringElementsAreEmail() {
    super();
  }

  /** The prototype invariant. */
  private static @Prototype SequenceStringElementsAreEmail proto =
      new @Prototype SequenceStringElementsAreEmail();

  /**
   * Returns the prototype invariant.
   *
   * @return the prototype invariant
   */
  public static @Prototype SequenceStringElementsAreEmail get_proto() {
    return proto;
  }

  /** returns whether or not this invariant is enabled */
  @Override
  public boolean enabled() {
    return dkconfig_enabled;
  }

  /** instantiate an invariant on the specified slice */
  @Override
  protected SequenceStringElementsAreEmail instantiate_dyn(
      @Prototype SequenceStringElementsAreEmail this, PptSlice slice) {
    return new SequenceStringElementsAreEmail(slice);
  }

  // Don't write clone, because this.intersect is read-only
  // protected Object clone();

  @Override
  public String repr(@GuardSatisfied SequenceStringElementsAreEmail this) {
    return "SequenceStringElementsAreEmail " + varNames();
  }

  @SideEffectFree
  @Override
  public String format_using(
      @GuardSatisfied SequenceStringElementsAreEmail this, OutputFormat format) {
    return "All the elements of " + var().name() + " are emails";
  }

  @Override
  public InvariantStatus check_modified(@Interned String @Interned [] a, int count) {
    if (a.length > 0) {
      alwaysEmpty = false;
    }

    for (int i = 0; i < a.length; i++) {
      String arrayElement = a[i];
      if (arrayElement != null) {
        allElementsAreNull = false;
        Matcher matcher = IsEmail.PATTERN.matcher(arrayElement);
        // The invariant is falsified if one of the elements of the array is NOT an email
        if (!matcher.matches()) {
          return InvariantStatus.FALSIFIED;
        }
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

    if (alwaysEmpty || allElementsAreNull) {
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
    assert other instanceof SequenceStringElementsAreEmail;
    return true;
  }

  @SideEffectFree
  @Override
  public SequenceStringElementsAreEmail clone(@GuardSatisfied SequenceStringElementsAreEmail this) {
    SequenceStringElementsAreEmail result = (SequenceStringElementsAreEmail) super.clone();
    result.alwaysEmpty = alwaysEmpty;
    result.allElementsAreNull = allElementsAreNull;
    return result;
  }

  /**
   * Merge the invariants in invs to form a new invariant. Each must be a
   * SequenceStringElementsAreEmail invariant. This code finds all of the
   * SequenceStringElementsAreEmail values from each of the invariants and returns the merged
   * invariant (if any).
   *
   * @param invs list of invariants to merge. The invariants must all be of the same type and should
   *     come from the children of parent_ppt.
   * @param parent_ppt slice that will contain the new invariant
   */
  @Override
  public @Nullable Invariant merge(List<Invariant> invs, PptSlice parent_ppt) {

    // Create the initial parent invariant from the first child
    SequenceStringElementsAreEmail first = (SequenceStringElementsAreEmail) invs.get(0);
    SequenceStringElementsAreEmail result = first.clone();
    result.ppt = parent_ppt;

    // Return result if both alwaysEmpty and allElementsAreNull are false
    if (!result.alwaysEmpty && !result.allElementsAreNull) {
      return result;
    }

    // Loop through the rest of the child invariants
    for (int i = 1; i < invs.size(); i++) {
      SequenceStringElementsAreEmail sseae = (SequenceStringElementsAreEmail) invs.get(i);
      // If sseae.alwaysEmpty is false, set the value of result.alwaysEmpty to false
      if (!sseae.alwaysEmpty) {
        result.alwaysEmpty = false;
      }
      // If sseae.allElementsAreNull is false, set the value of result.allElementsAreNull to false
      if (!sseae.allElementsAreNull) {
        result.allElementsAreNull = false;
      }
      // If both result.alwaysEmpty and result.allElementsAreNull are false, return result
      if (!result.alwaysEmpty && !result.allElementsAreNull) {
        break;
      }
    }
    return result;
  }
}
