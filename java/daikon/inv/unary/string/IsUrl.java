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
  public static boolean dkconfig_enabled = Invariant.invariantEnabledDefault;

  /** Regex for 16 bits of an IPv6 address, as 1 to 4 hex digits ({@code h16} in RFC 3986). */
  private static final String H16 = "[0-9a-fA-F]{1,4}";

  /** Regex for a dotted-decimal IPv4 address ({@code IPv4address} in RFC 3986). */
  private static final String IPV4 =
      "(?:25[0-5]|2[0-4]\\d|1?\\d?\\d)(?:\\.(?:25[0-5]|2[0-4]\\d|1?\\d?\\d)){3}";

  /** Regex for the low-order 32 bits of an IPv6 address ({@code ls32} in RFC 3986). */
  private static final String LS32 = "(?:" + H16 + ":" + H16 + "|" + IPV4 + ")";

  /**
   * Returns a regex for 0 to {@code n} groups of {@code h16}, each followed by a colon, and then a
   * final {@code h16}; the whole is optional. This is {@code [ *n( h16 ":" ) h16 ]} in RFC 3986.
   *
   * @param n the maximum number of colon-terminated groups before the final group
   * @return a regex for an optional sequence of 1 to {@code n + 1} colon-separated groups
   */
  private static String h16Prefix(int n) {
    return "(?:(?:" + H16 + ":){0," + n + "}" + H16 + ")?";
  }

  /** Regex for an IPv6 address ({@code IPv6address} in RFC 3986), not including brackets. */
  private static final String IPV6 =
      "(?:"
          + ("(?:" + H16 + ":){6}" + LS32)
          + ("|::(?:" + H16 + ":){5}" + LS32)
          + ("|(?:" + H16 + ")?::(?:" + H16 + ":){4}" + LS32)
          + ("|" + h16Prefix(1) + "::(?:" + H16 + ":){3}" + LS32)
          + ("|" + h16Prefix(2) + "::(?:" + H16 + ":){2}" + LS32)
          + ("|" + h16Prefix(3) + "::" + H16 + ":" + LS32)
          + ("|" + h16Prefix(4) + "::" + LS32)
          + ("|" + h16Prefix(5) + "::" + H16)
          + ("|" + h16Prefix(6) + "::")
          + ")";

  /**
   * Regex that matches a URL. Source: <a
   * href="https://mathiasbynens.be/demo/url-regex">https://mathiasbynens.be/demo/url-regex</a>
   */
  public static Pattern PATTERN =
      Pattern.compile(
          "^"
              // Protocol of the URL: 'http', 'https', or 'ftp', followed by '://'.
              // Scheme names are case-insensitive.
              + "(?:(?i:https?|ftp)://)"
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

              // IPv6 address in brackets, OR IPv4 address, OR domain name
              + ("(?:"
                  // IPv6 address, enclosed in brackets
                  + ("\\[" + IPV6 + "\\]")
                  + "|"
                  // IPv4 address
                  + "(?:[1-9]\\d?|1\\d\\d|2[01]\\d|22[0-3])(?:\\.(?:1?\\d{1,2}|2[0-4]\\d|25[0-5])){2}(?:\\.(?:[1-9]\\d?|1\\d\\d|2[0-4]\\d|25[0-4]))"
                  + "|"
                  // Domain name.  Each label is written as runs of non-hyphen characters
                  // separated by one or more hyphens (as in "xn--"), so a label neither starts nor
                  // ends with a hyphen.  Because the runs and the separators use disjoint
                  // characters, there is no ambiguity and no exponential backtracking.
                  + "(?:[\\w\\x{00a1}-\\x{ffff}]+(?:-+[\\w\\x{00a1}-\\x{ffff}]+)*)(?:\\.[\\w\\x{00a1}-\\x{ffff}]+(?:-+[\\w\\x{00a1}-\\x{ffff}]+)*)*(?:\\.(?:[a-zA-Z\\x{00a1}-\\x{ffff}]{2,}))"
                  // end of IPv6 address OR IPv4 address OR domain name
                  + ")")
              // end of the group that starts with the negative lookaheads
              + ")"
              // Optional port number
              + "(?::\\d{1,5})?"
              // Optional path, query, or fragment
              + "(?:[/?#][^\\s]*)?"
              + "$");

  //
  // Required methods
  //

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
    if (format != OutputFormat.DAIKON) {
      return format_unimplemented(format);
    }
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
