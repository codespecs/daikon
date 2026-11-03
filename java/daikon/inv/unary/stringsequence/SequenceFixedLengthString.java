package daikon.inv.unary.stringsequence;

import daikon.PptSlice;
import daikon.VarInfo;
import daikon.inv.*;
import daikon.inv.ValueSet.ValueSetStringArray;
import daikon.inv.unary.stringsequence.dates.SequenceStringElementsAreDateDDMMYYYY;
import daikon.inv.unary.stringsequence.dates.SequenceStringElementsAreDateMMDDYYYY;
import daikon.inv.unary.stringsequence.dates.SequenceStringElementsAreDateYYYYMMDD;
import daikon.suppress.NISuppressee;
import daikon.suppress.NISuppression;
import daikon.suppress.NISuppressionSet;
import daikon.suppress.NISuppressor;
import java.util.List;
import org.checkerframework.checker.interning.qual.Interned;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;
import org.checkerframework.framework.qual.Unused;
import typequals.prototype.qual.Prototype;

/**
 * Indicates that all the elements of an array of strings have a fixed length n. Prints as {@code
 * All the elements of x have LENGTH=n}.
 */
public class SequenceFixedLengthString extends SingleStringSequence {

  /** UID for serialization. */
  static final long serialVersionUID = 20220423L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /** Boolean. true iff SequenceFixedLengthString invariants should be considered. */
  public static boolean dkconfig_enabled = true;

  /** Numerical variable specifying the length of the array string elements */
  @Unused(when = Prototype.class)
  private @MonotonicNonNull Integer elements_length = null;

  /**
   * Creates a new SequenceFixedLengthString.
   *
   * @param ppt the slice with the variable of interest
   */
  protected SequenceFixedLengthString(PptSlice ppt) {
    super(ppt);
  }

  /** Creates a new prototype SequenceFixedLengthString. */
  protected @Prototype SequenceFixedLengthString() {
    super();
  }

  /** The prototype invariant. */
  private static @Prototype SequenceFixedLengthString proto =
      new @Prototype SequenceFixedLengthString();

  /**
   * Returns the prototype invariant.
   *
   * @return the prototype invariant
   */
  public static @Prototype SequenceFixedLengthString get_proto() {
    return proto;
  }

  /** returns whether or not this invariant is enabled */
  @Override
  public boolean enabled() {
    return dkconfig_enabled;
  }

  /** instantiate an invariant on the specified slice */
  @Override
  protected SequenceFixedLengthString instantiate_dyn(
      @Prototype SequenceFixedLengthString this, PptSlice slice) {
    return new SequenceFixedLengthString(slice);
  }

  // Don't write clone, because this.intersect is read-only
  // protected Object clone();

  @Override
  public String repr(@GuardSatisfied SequenceFixedLengthString this) {
    return "SequenceFixedLengthString " + varNames() + ": length=\"" + elements_length;
  }

  @SideEffectFree
  @Override
  public String format_using(@GuardSatisfied SequenceFixedLengthString this, OutputFormat format) {
    return "All the elements of " + var().name() + " have LENGTH=" + elements_length;
  }

  @Override
  public InvariantStatus check_modified(@Interned String @Interned [] a, int count) {

    if (a == null || a.length == 0) {
      return InvariantStatus.NO_CHANGE;
    }

    // Initialize elements_length for the first time
    if (elements_length == null) {

      // Set the length of the first array element that is not null as the value of elements_length
      int firstNonNullElementIndex = 0;
      while (firstNonNullElementIndex < a.length) {
        if (a[firstNonNullElementIndex] != null) {
          elements_length = a[firstNonNullElementIndex].length();
          break;
        }
        firstNonNullElementIndex++;
      }

      if (elements_length != null) {
        // Check that the all the remaining array elements have the same length
        // We start counting from the index of the firstNonNullElement
        for (int i = firstNonNullElementIndex; i < a.length; i++) {
          // If the array element is not null and its length is different to elements_length, the
          // invariant is falsified
          if (a[i] != null && a[i].length() != elements_length) {
            return InvariantStatus.FALSIFIED;
          }
        }
      }

    } else {
      for (int i = 0; i < a.length; i++) {
        // If the array element is not null and its length is different to elements_length, the
        // invariant is falsified
        if (a[i] != null && a[i].length() != elements_length) {
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

    if (elements_length == null) {
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
    assert other instanceof SequenceFixedLengthString;
    return true;
  }

  /** NI suppressions, initialized in get_ni_suppressions(). */
  private static @Nullable NISuppressionSet suppressions = null;

  @Pure
  @Override
  public NISuppressionSet get_ni_suppressions() {
    if (suppressions == null) {

      NISuppressee suppressee = new NISuppressee(SequenceFixedLengthString.class, 1);

      // suppressor definitions (used in suppressions below)
      NISuppressor sequenceStringElementsAreDateMMDDYYYY =
          new NISuppressor(0, SequenceStringElementsAreDateMMDDYYYY.class);
      NISuppressor sequenceStringElementsAreDateDDMMYYYY =
          new NISuppressor(0, SequenceStringElementsAreDateDDMMYYYY.class);
      NISuppressor sequenceStringElementsAreDateYYYYMMDD =
          new NISuppressor(0, SequenceStringElementsAreDateYYYYMMDD.class);

      suppressions =
          new NISuppressionSet(
              new NISuppression[] {
                new NISuppression(sequenceStringElementsAreDateMMDDYYYY, suppressee),
                new NISuppression(sequenceStringElementsAreDateDDMMYYYY, suppressee),
                new NISuppression(sequenceStringElementsAreDateYYYYMMDD, suppressee)
              });
    }
    return suppressions;
  }

  /** SequenceFixedLengthString invariant will not be reported if EltOneOfString is not falsified */
  @Pure
  @Override
  public @Nullable DiscardInfo isObviousDynamically(VarInfo[] vis) {
    DiscardInfo di = super.isObviousDynamically(vis);
    if (di != null) {
      return di;
    }

    VarInfo var1 = vis[0];

    PptSlice ppt_over1 = ppt.parent.findSlice(var1);
    if (ppt_over1 == null) {
      return null;
    }

    for (Invariant inv : ppt_over1.invs) {
      if (inv instanceof EltOneOfString) {
        return new DiscardInfo(
            this,
            DiscardCode.obvious,
            "SequenceFixedLengthString is obvious if EltOneOfString is not discarded");
      }
    }

    return null;
  }

  @SuppressWarnings("nullness:monotonic") // assigning @MNN to @MNN should be legal in clone()
  @SideEffectFree
  @Override
  public SequenceFixedLengthString clone(@GuardSatisfied SequenceFixedLengthString this) {
    SequenceFixedLengthString result = (SequenceFixedLengthString) super.clone();
    result.elements_length = elements_length;
    return result;
  }

  /**
   * Merge the invariants in invs to form a new invariant. Each must be a SequenceFixedLengthString
   * invariant. This code finds all of the SequenceFixedLengthString values from each of the
   * invariants and returns the merged invariant (if any).
   *
   * @param invs list of invariants to merge. The invariants must all be of the same type and should
   *     come from the children of parent_ppt.
   * @param parent_ppt slice that will contain the new invariant
   */
  @Override
  public @Nullable Invariant merge(List<Invariant> invs, PptSlice parent_ppt) {
    // Create the initial parent invariant from the first child
    SequenceFixedLengthString first = (SequenceFixedLengthString) invs.get(0);
    SequenceFixedLengthString result = first.clone();
    result.ppt = parent_ppt;

    // Loop through the rest of the child invariants
    for (int i = 1; i < invs.size(); i++) {
      SequenceFixedLengthString sfls = (SequenceFixedLengthString) invs.get(i);

      // If result.elements_length is null and sfls.elements_length is not null
      if (result.elements_length == null && sfls.elements_length != null) {
        // Set the elements_length to the value of sfls.elements_length
        result.elements_length = sfls.elements_length;
      } else if (result.elements_length != null
          && sfls.elements_length != null
          && !result.elements_length.equals(sfls.elements_length)) {
        // Invariant falsified
        result.log(
            "%s",
            "Child value with elements_length "
                + sfls.elements_length
                + " destroyed SequenceFixedLengthString");
        return null;
      }
    }
    return result;
  }
}
