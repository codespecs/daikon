package daikon.inv.unary.string;

import daikon.PptSlice;
import daikon.inv.Invariant;
import daikon.inv.InvariantStatus;
import daikon.inv.OutputFormat;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;
import typequals.prototype.qual.Prototype;

/**
 * Indicates that the string variable always represents a number. Prints as {@code x is Numeric}.
 */
public class IsNumeric extends SingleString {
  /** UID for serialization. */
  static final long serialVersionUID = 20230704L;

  /**
   * Regular expression that match positive and negative decimal numbers with optional fractional
   * part (decimal places). Permits "," every three digits. Forbids leading zero. Forbids trailing
   * "." without any fractional digits. Forbids leading "." without any whole-number digits.
   */
  public static Pattern PATTERN =
      Pattern.compile(
          "^"
              // Optional plus or minus sign
              + "[-+]?"
              // Whole part of the number
              + "(0|([1-9](\\d*|\\d{0,2}(,\\d{3})*)))?"
              // Fractional part of the number
              + "(\\.\\d+)?"
              + "$");

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /** Boolean. True iff IsNumeric invariants should be considered. */
  public static boolean dkconfig_enabled = true;

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

  @SideEffectFree
  @Override
  public String format_using(@GuardSatisfied IsNumeric this, OutputFormat format) {
    switch (format) {
      case CSHARPCONTRACT:
        throw new Error("not implemented");
      case DAIKON:
        return var().name() + " is Numeric";
      case DBCJAVA:
      case ESCJAVA:
      case JAVA:
      case JML:
        return "daikon.inv.unary.string.IsNumeric.PATTERN.matcher(" + var().name() + ").matches()";
      case SIMPLIFY:
        return "(isNumeric " + var().simplify_name() + ")";
      default:
        throw new Error("non-exhaustive switch: " + format);
    }
  }

  @Override
  public InvariantStatus check_modified(String v, int count) {
    Matcher matcher = PATTERN.matcher(v);

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
    return 1 - Math.pow(.1, ppt.num_samples());
  }

  @Pure
  @Override
  public boolean isSameFormula(Invariant other) {
    assert other instanceof IsNumeric;
    return true;
  }

  @SideEffectFree
  @Override
  public IsNumeric clone(@GuardSatisfied IsNumeric this) {
    IsNumeric result = (IsNumeric) super.clone();
    result.alwaysEmpty = alwaysEmpty;
    return result;
  }

  /**
   * Merge the invariants in invs to form a new invariant. Each must be a IsNumeric invariant. This
   * code finds all of the IsNumeric values from each of the invariants and returns the merged
   * invariant (if any).
   *
   * @param invs list of invariants to merge. The invariants must all be of the same type and should
   *     come from the children of parent_ppt.
   * @param parent_ppt slice that will contain the new invariant
   */
  @Override
  public @Nullable Invariant merge(List<Invariant> invs, PptSlice parent_ppt) {

    // Create the initial parent invariant from the first child
    IsNumeric first = (IsNumeric) invs.get(0);
    IsNumeric result = first.clone();
    result.ppt = parent_ppt;

    // Return result if the value of alwaysEmpty is false
    if (!result.alwaysEmpty) {
      return result;
    }

    // Loop through the rest of the child invariants
    for (int i = 1; i < invs.size(); i++) {
      IsNumeric in = (IsNumeric) invs.get(i);
      // If in.alwaysEmpty is false, set the value of result.isEmpty to false and return it
      if (!in.alwaysEmpty) {
        result.alwaysEmpty = false;
        break;
      }
    }

    return result;
  }
}
