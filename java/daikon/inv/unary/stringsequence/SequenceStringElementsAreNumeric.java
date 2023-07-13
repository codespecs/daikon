package daikon.inv.unary.stringsequence;

import daikon.PptSlice;
import daikon.inv.DiscardInfo;
import daikon.inv.Invariant;
import daikon.inv.InvariantStatus;
import daikon.inv.OutputFormat;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.checkerframework.checker.interning.qual.Interned;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;
import typequals.prototype.qual.Prototype;

/**
 * Indicates that the characters of all the elements of an array of strings are always numeric.
 * Prints as {@code All the elements of x are Numeric}.
 */
public class SequenceStringElementsAreNumeric extends SingleStringSequence {
  static final long serialVersionUID = 20230704L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  public static boolean dkconfig_enabled = false;

  // Set to true if the array is empty. If we do not use this property, the invariant would be
  // considered true if all the arrays are empty
  private boolean alwaysEmpty = true;

  private static Pattern pattern = Pattern.compile("^[+-]{0,1}(0|([1-9](\\d*|\\d{0,2}(,\\d{3})*)))?(\\.\\d*[0-9])?$");

  protected SequenceStringElementsAreNumeric(PptSlice ppt) {
    super(ppt);
  }

  protected @Prototype SequenceStringElementsAreNumeric() {
    super();
  }

  private static @Prototype SequenceStringElementsAreNumeric proto =
      new @Prototype SequenceStringElementsAreNumeric();

  /** Returns the prototype invariant for CommonStringSequence. */
  public static @Prototype SequenceStringElementsAreNumeric get_proto() {
    return proto;
  }

  /** returns whether or not this invariant is enabled */
  @Override
  public boolean enabled() {
    return dkconfig_enabled;
  }

  /** instantiate an invariant on the specified slice */
  @Override
  protected SequenceStringElementsAreNumeric instantiate_dyn(
      @Prototype SequenceStringElementsAreNumeric this, PptSlice slice) {
    return new SequenceStringElementsAreNumeric(slice);
  }

  // Don't write clone, because this.intersect is read-only
  // protected Object clone();

  @Override
  public String repr(@GuardSatisfied SequenceStringElementsAreNumeric this) {
    return "SequenceStringElementsAreNumeric " + varNames();
  }

  @SideEffectFree
  @Override
  public String format_using(
      @GuardSatisfied SequenceStringElementsAreNumeric this, OutputFormat format) {
    return "All the elements of " + var().name() + " are Numeric";
  }

  @Override
  public InvariantStatus check_modified(@Interned String @Interned [] a, int count) {
    if (a.length > 0) {
      alwaysEmpty = false;
    }

    for (int i = 0; i < a.length; i++) {
      Matcher matcher = pattern.matcher(a[i]);
      // The invariant is falsified if one of the elements of the array is NOT numeric
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

  @Pure
  public @Nullable DiscardInfo isObviousImplied() {
    return null;
  }

  @Pure
  @Override
  public boolean isSameFormula(Invariant other) {
    assert other instanceof SequenceStringElementsAreNumeric;
    return true;
  }
}
