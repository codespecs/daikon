package daikon.inv.unary.string.dates;

import static daikon.inv.unary.string.dates.DateRegexes.*;

import daikon.PptSlice;
import daikon.inv.Invariant;
import daikon.inv.InvariantStatus;
import daikon.inv.OutputFormat;
import daikon.inv.unary.string.SingleString;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;
import typequals.prototype.qual.Prototype;

/**
 * Indicates that the value of a string variable is always a time in 24-hour format. Prints as
 * {@code x is time of day: HH:MM 24-hour format, optional leading 0}.
 */
public class IsTimeOfDay extends SingleString {
  /** UID for serialization. */
  static final long serialVersionUID = 20230704L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /** Boolean. True iff IsTimeOfDay invariants should be considered. */
  public static boolean dkconfig_enabled = false;

  /** Matches a time of day in HH:MM 24-hour format, with optional leading 0. */
  public static final Pattern PATTERN = Pattern.compile("^" + H + ":" + MINUTES + "$");

  ///
  /// Required methods
  ///

  /**
   * Creates a new IsTimeOfDay.
   *
   * @param ppt the slice with the variable of interest
   */
  private IsTimeOfDay(PptSlice ppt) {
    super(ppt);
  }

  /** Creates a new prototype IsTimeOfDay. */
  private @Prototype IsTimeOfDay() {
    super();
  }

  /** The prototype invariant. */
  private static @Prototype IsTimeOfDay proto = new @Prototype IsTimeOfDay();

  /**
   * Returns the prototype invariant.
   *
   * @return the prototype invariant
   */
  public static @Prototype IsTimeOfDay get_proto() {
    return proto;
  }

  @Override
  public boolean enabled() {
    return dkconfig_enabled;
  }

  @Override
  public IsTimeOfDay instantiate_dyn(@Prototype IsTimeOfDay this, PptSlice slice) {
    return new IsTimeOfDay(slice);
  }

  @SideEffectFree
  @Override
  public String format_using(@GuardSatisfied IsTimeOfDay this, OutputFormat format) {
    return var().name() + " is time of day: HH:MM 24-hour format, optional leading 0";
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
    assert other instanceof IsTimeOfDay;
    return true;
  }
}
