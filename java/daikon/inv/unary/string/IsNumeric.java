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
  static final long serialVersionUID = 20230704L;

  // True if the string is always empty
  private boolean alwaysEmpty;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /** Boolean. True iff Positive invariants should be considered. */
  public static boolean dkconfig_enabled = false;

  ///
  /// Required methods
  ///
  private IsNumeric(PptSlice ppt) {
    super(ppt);
    alwaysEmpty = true;
  }

  private @Prototype IsNumeric() {
    super();
  }

  private static @Prototype IsNumeric proto = new @Prototype IsNumeric();

  // Returns the prototype invariant
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

    Pattern pattern =
        Pattern.compile("^[+-]{0,1}(0|([1-9](\\d*|\\d{0,2}(,\\d{3})*)))?(\\.\\d*[0-9])?$");

    Matcher matcher = pattern.matcher(v);

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
