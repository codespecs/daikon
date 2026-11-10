package daikon.inv.unary.stringsequence;

import daikon.PptSlice;
import daikon.inv.DiscardInfo;
import daikon.inv.Invariant;
import daikon.inv.InvariantStatus;
import daikon.inv.OutputFormat;
import daikon.inv.ValueSet.ValueSetStringArray;
import daikon.inv.unary.string.IsUrl;
import java.util.List;
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
   * True if all the elements of the array are null. Without this property, the invariant would be
   * reported if all the arrays contain only null elements.
   */
  private boolean allElementsAreNull = true;

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
      String arrayElement = a[i];
      if (arrayElement != null) {
        allElementsAreNull = false;
        Matcher matcher = IsUrl.PATTERN.matcher(arrayElement);
        // The invariant is falsified if one of the elements of the array is NOT of type URL
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
    // If there are no samples over our variables, its unjustified
    if (ppt.num_samples() == 0) {
      return CONFIDENCE_UNJUSTIFIED;
    }

    // If the array never has any elements, its unjustified
    ValueSetStringArray vs = (ValueSetStringArray) var().get_value_set();
    if (vs.elem_cnt() == 0) {
      return CONFIDENCE_UNJUSTIFIED;
    }

    if (allElementsAreNull) {
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

  @SideEffectFree
  @Override
  public SequenceStringElementsAreUrl clone(@GuardSatisfied SequenceStringElementsAreUrl this) {
    SequenceStringElementsAreUrl result = (SequenceStringElementsAreUrl) super.clone();
    return result;
  }

  /**
   * Merge the invariants in invs to form a new invariant. Each must be a
   * SequenceStringElementsAreUrl invariant. This code finds all of the SequenceStringElementsAreUrl
   * values from each of the invariants and returns the merged invariant (if any).
   *
   * @param invs list of invariants to merge. The invariants must all be of the same type and should
   *     come from the children of parent_ppt.
   * @param parent_ppt slice that will contain the new invariant
   */
  @Override
  public @Nullable Invariant merge(List<Invariant> invs, PptSlice parent_ppt) {

    // Create the initial parent invariant from the first child
    SequenceStringElementsAreUrl first = (SequenceStringElementsAreUrl) invs.get(0);
    SequenceStringElementsAreUrl result = first.clone();
    result.ppt = parent_ppt;

    // Return result if both alwaysEmpty and allElementsAreNull are false
    if (!result.allElementsAreNull) {
      return result;
    }

    // Loop through the rest of the child invariants
    for (int i = 1; i < invs.size(); i++) {
      SequenceStringElementsAreUrl sseau = (SequenceStringElementsAreUrl) invs.get(i);
      // If sseau.allElementsAreNull is false, set the value of result.allElementsAreNull to false
      if (!sseau.allElementsAreNull) {
        result.allElementsAreNull = false;
      }
      // If both result.alwaysEmpty and result.allElementsAreNull are false, return result
      if (!result.allElementsAreNull) {
        break;
      }
    }
    return result;
  }
}
