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

/** Indicates that the value of a string variable is always a URL. Prints as {@code is Url}. */
public class IsUrl extends SingleString {

  /** UID for serialization. */
  static final long serialVersionUID = 20230704L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /** Boolean. True iff IsUrl invariants should be considered. */
  public static boolean dkconfig_enabled = false;

  /**
   * Regex that matches a URL. Source: <a
   * href="https://mathiasbynens.be/demo/url-regex">https://mathiasbynens.be/demo/url-regex</a>
   */
  public static Pattern PATTERN =
      Pattern.compile(
          "^"
              // Protocol of the URL: 'http', 'https', or 'ftp', followed by '://'
              + "(?:(?:https?|ftp)://)"
              // Optional username and password, for URLs containing "...username:password@..."
              + "(?:\\S+(?::\\S*)?@)?"

              // Reserved IP address ranges: fordid matching via negative lookahead.
              // 10.x.x.x
              + "(?:(?!10(?:\\.\\d{1,3}){3})"
              // Loopback address
              + "(?!127(?:\\.\\d{1,3}){3})"
              // Link-local address
              + "(?!169\\.254(?:\\.\\d{1,3}){2})"
              // Private network
              + "(?!192\\.168(?:\\.\\d{1,3}){2})"
              // Private network (2)
              + "(?!172\\.(?:1[6-9]|2\\d|3[0-1])(?:\\.\\d{1,3}){2})"

              // IP address OR domain name
              + ("("
                  // IP address
                  + "?:[1-9]\\d?|1\\d\\d|2[01]\\d|22[0-3])(?:\\.(?:1?\\d{1,2}|2[0-4]\\d|25[0-5])){2}(?:\\.(?:[1-9]\\d?|1\\d\\d|2[0-4]\\d|25[0-4]))"
                  + "|"
                  // Domain name
                  + "(?:(?:[\\w\\x{00a1}-\\x{ffff}0-9]+-?)*[\\w\\x{00a1}-\\x{ffff}0-9]+)(?:\\.(?:[\\w\\x{00a1}-\\x{ffff}0-9]+-)*[\\w\\x{00a1}-\\x{ffff}0-9]+)*(?:\\.(?:[a-zA-Z\\x{00a1}-\\x{ffff}]{2,}))"
                  // end of IP address OR domain name
                  + ")")
              // Optional port number
              + "(?::\\d{2,5})?"
              // Optional part of the URL
              + "(?:/[^\\s]*)?"
              + "$");

  ///
  /// Required methods
  ///

  /**
   * Creates a new IsUrl.
   *
   * @param ppt the slice with the variable of interest
   */
  private IsUrl(PptSlice ppt) {
    super(ppt);
  }

  /** Creates a new prototype IsUrl. */
  private @Prototype IsUrl() {
    super();
  }

  /** The prototype invariant. */
  private static @Prototype IsUrl proto = new @Prototype IsUrl();

  /**
   * Returns the prototype invariant.
   *
   * @return the prototype invariant
   */
  public static @Prototype IsUrl get_proto() {
    return proto;
  }

  @Override
  public boolean enabled() {
    return dkconfig_enabled;
  }

  @Override
  public IsUrl instantiate_dyn(@Prototype IsUrl this, PptSlice slice) {
    return new IsUrl(slice);
  }

  @SideEffectFree
  @Override
  public String format_using(@GuardSatisfied IsUrl this, OutputFormat format) {
    return var().name() + " is Url";
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
    assert other instanceof IsUrl;
    return true;
  }
}
