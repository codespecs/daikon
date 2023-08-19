package daikon.inv.unary.string;

import daikon.PptSlice;
import daikon.inv.Invariant;
import daikon.inv.InvariantStatus;
import daikon.inv.OutputFormat;
import daikon.inv.unary.string.dates.IsDateDDMMYYYY;
import daikon.inv.unary.string.dates.IsDateMMDDYYYY;
import daikon.inv.unary.string.dates.IsDateYYYYMMDD;
import daikon.suppress.NISuppressee;
import daikon.suppress.NISuppression;
import daikon.suppress.NISuppressionSet;
import daikon.suppress.NISuppressor;
import org.checkerframework.checker.interning.qual.Interned;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;
import org.checkerframework.framework.qual.Unused;
import typequals.prototype.qual.Prototype;

/**
 * Indicates that the value of a string variable always has a fixed length n. Prints as {@code
 * LENGTH(x)==n}.
 */
public class FixedLengthString extends SingleString {

  /** UID for serialization. */
  static final long serialVersionUID = 20230704L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /** Boolean. True iff FixedLengthString invariants should be considered. */
  public static boolean dkconfig_enabled = false;

  /** Numerical variable specifying the string length */
  @Unused(when = Prototype.class)
  private @Nullable Integer length = null;

  ///
  /// Required methods
  ///

  /**
   * Creates a new FixedLengthString.
   *
   * @param ppt the slice with the variable of interest
   */
  private FixedLengthString(PptSlice ppt) {
    super(ppt);
  }

  /** Creates a new prototype FixedLengthString. */
  private @Prototype FixedLengthString() {
    super();
  }

  /** The prototype invariant. */
  private static @Prototype FixedLengthString proto = new @Prototype FixedLengthString();

  /**
   * Returns the prototype invariant.
   *
   * @return the prototype invariant
   */
  public static @Prototype FixedLengthString get_proto() {
    return proto;
  }

  @Override
  public boolean enabled() {
    return dkconfig_enabled;
  }

  @Override
  public FixedLengthString instantiate_dyn(@Prototype FixedLengthString this, PptSlice slice) {
    return new FixedLengthString(slice);
  }

  @SideEffectFree
  @Override
  public String format_using(@GuardSatisfied FixedLengthString this, OutputFormat format) {
    return "LENGTH(" + var().name() + ")==" + length;
  }

  @Override
  public InvariantStatus add_modified(@Interned String a, int count) {
    return check_modified(a, count);
  }

  @Override
  public InvariantStatus check_modified(@Interned String v, int count) {
    // Initialize the length the first time
    if (length == null) {
      length = v.length();
    }

    if (v.length() == length) {
      return InvariantStatus.NO_CHANGE;
    }
    return InvariantStatus.FALSIFIED;
  }

  @Override
  protected double computeConfidence() {
    return 1 - Math.pow(.1, ppt.num_samples());
  }

  @Pure
  @Override
  public boolean isSameFormula(Invariant other) {
    // Check type and length value
    assert other instanceof FixedLengthString;

    FixedLengthString o = (FixedLengthString) other;
    if (o.length != null && !o.length.equals(length)) {
      return false;
    }

    return true;
  }

  /** NI suppressions, initialized in get_ni_suppressions(). */
  private static @Nullable NISuppressionSet suppressions = null;

  @Pure
  @Override
  public NISuppressionSet get_ni_suppressions() {
    if (suppressions == null) {

      NISuppressee suppressee = new NISuppressee(FixedLengthString.class, 1);

      // suppressor definitions (used in suppressions below)
      NISuppressor isDateMMDDYYYY = new NISuppressor(0, IsDateMMDDYYYY.class);
      NISuppressor isDateDDMMYYYY = new NISuppressor(0, IsDateDDMMYYYY.class);
      NISuppressor isDateYYYYMMDD = new NISuppressor(0, IsDateYYYYMMDD.class);

      suppressions =
          new NISuppressionSet(
              new NISuppression[] {
                new NISuppression(isDateMMDDYYYY, suppressee),
                new NISuppression(isDateDDMMYYYY, suppressee),
                new NISuppression(isDateYYYYMMDD, suppressee),
              });
    }
    return suppressions;
  }
}
