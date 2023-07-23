package daikon.inv.unary.string;

import daikon.PptSlice;
import daikon.inv.Invariant;
import daikon.inv.InvariantStatus;
import daikon.inv.OutputFormat;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;
import typequals.prototype.qual.Prototype;

/**
 * Indicates that the characters of a string variable are always numeric. Prints as {@code x is
 * Numeric}.
 */
public class IsNumeric extends SingleString {
  /** UID for serialization. */
  static final long serialVersionUID = 20230704L;

  /**
   * This regular expression is designed to match positive and negative decimal numbers with
   * optional decimal plates. It has three components: 1. Optional plus or minus sign: Zero or one
   * occurrences of either '+' or '-'. 2. Whole part of the number: It can be either 0 or a sequence
   * of digits not starting with a leading 0. It supports comma separated sequences of three digits
   * (i.e., thousands). 3. Decimal part of the number: Decimal point followed by at least one digit.
   */
  public static Pattern PATTERN =
      Pattern.compile(
          // Optional plus or minus sign
          "^[+-]{0,1}"
              // Whole part of the number
              + "(0|([1-9](\\d*|\\d{0,2}(,\\d{3})*)))?"
              // Decimal part of the number
              + "(\\.\\d*[0-9])?$");

  /** true if the string is always empty If false, the invariant is unjustified */
  private boolean alwaysEmpty;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /** Boolean. True iff Positive invariants should be considered. */
  public static boolean dkconfig_enabled = false;

  ///
  /// Required methods
  ///

  /**
   * Creates a new IsNumeric.
   *
   * @param ppt the slice with the variable of interest
   */
  private IsNumeric(PptSlice ppt) {
    super(ppt);
    alwaysEmpty = true;
  }

  /** Creates a new prototype IsNumeric. */
  private @Prototype IsNumeric() {
    super();
  }

  /** The prototype invariant. */
  private static @Prototype IsNumeric proto = new @Prototype IsNumeric();

  /**
   * Returns the prototype invariant.
   *
   * @return the prototype invariant
   */
  public static @Prototype IsNumeric get_proto() {
    return proto;
  }

  @Override
  public boolean enabled() {
    return dkconfig_enabled;
  }

  @Override
  public IsNumeric instantiate_dyn(@Prototype IsNumeric this, PptSlice slice) {
    return new IsNumeric(slice);
  }

  // A printed representation for user output
  @SideEffectFree
  @Override
  public String format_using(@GuardSatisfied IsNumeric this, OutputFormat format) {
    return var().name() + " is Numeric";
  }

  @Override
  public InvariantStatus check_modified(String v, int count) {
    Matcher matcher = PATTERN.matcher(v);

    if (v.length() > 0) {
      alwaysEmpty = false;
    }

    if (matcher.matches()) {
      return InvariantStatus.NO_CHANGE;
    }
    return InvariantStatus.FALSIFIED;
  }

  @Override
  public InvariantStatus add_modified(String v, int count) {
    return check_modified(v, count);
  }

  @Override
  protected double computeConfidence() {
    if (alwaysEmpty) {
      return Invariant.CONFIDENCE_UNJUSTIFIED;
    }
    return 1 - Math.pow(.1, ppt.num_samples());
  }

  @Pure
  @Override
  public boolean isSameFormula(Invariant other) {
    assert other instanceof IsNumeric;
    return true;
  }
}
