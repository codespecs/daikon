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
 * Indicates that the value of a string type variable is always an email. Represented as {@code x is
 * Email}.
 */
public class IsEmail extends SingleString {
  /** UID for serialization. */
  static final long serialVersionUID = 20230704L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /** Boolean. True iff Positive invariants should be considered. */
  public static boolean dkconfig_enabled = false;

  /**
   * Regular expression that matches email addresses. Source: <a
   * href="https://emailregex.com/index.html">https://emailregex.com/index.html</a>.
   */
  public static Pattern PATTERN =
      Pattern.compile(
          // username
          ("^("
                  // usernames with alphanumeric characters and some special characters
                  + "?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*"
                  // or
                  + "|"
                  // usernames enclosed in double quotes

                  + "\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")")
              + "@"
              // domain
              + ("("
                  // domain names with alphanumeric characters and hyphens (separated by periods)
                  + "?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9]^[0-9]?"
                  // or
                  + "|"
                  // IP addresses enclosed in square brackets, allowing for IPv4 and IPv6 addresses
                  // with
                  // an optional port number.
                  + "[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\]"
                  + ")")
              + "$");

  ///
  /// Required methods
  ///

  /**
   * Creates a new IsEmail.
   *
   * @param ppt the slice with the variable of interest
   */
  private IsEmail(PptSlice ppt) {
    super(ppt);
  }

  /** Creates a new prototype IsEmail. */
  private @Prototype IsEmail() {
    super();
  }

  /** The prototype invariant. */
  private static @Prototype IsEmail proto = new @Prototype IsEmail();

  /**
   * Returns the prototype invariant.
   *
   * @return the prototype invariant
   */
  public static @Prototype IsEmail get_proto() {
    return proto;
  }

  @Override
  public boolean enabled() {
    return dkconfig_enabled;
  }

  @Override
  public IsEmail instantiate_dyn(@Prototype IsEmail this, PptSlice slice) {
    return new IsEmail(slice);
  }

  // A printed representation for user output
  @SideEffectFree
  @Override
  public String format_using(@GuardSatisfied IsEmail this, OutputFormat format) {
    return var().name() + " is Email";
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
    assert other instanceof IsEmail;
    return true;
  }
}
