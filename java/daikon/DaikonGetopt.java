package daikon;

import static daikon.tools.nullness.NullnessUtil.castNonNull;

import gnu.getopt.Getopt;
import gnu.getopt.LongOpt;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.plumelib.util.ArraysPlume;

/**
 * A command-line option processor that throws a {@link Daikon.UserError} that describes a bad
 * command-line option, instead of printing a message and returning {@code '?'} or {@code ':'}.
 *
 * <p>If constructed with a usage message, it also handles {@code -h} and {@code --help}: it prints
 * the usage message and throws {@link Daikon.NormalTermination}.
 */
public class DaikonGetopt extends Getopt {

  /**
   * The name passed to {@link Getopt}, which uses it only in messages that DaikonGetopt suppresses.
   */
  private static final String PROGNAME = "DaikonGetopt";

  /**
   * The usage message printed for {@code -h} and {@code --help}, or null if the caller handles
   * them.
   */
  private final @Nullable String usage;

  /**
   * Creates a command-line option processor that recognizes short and long options. Its description
   * of a bad option does not suggest running with {@code -h}, because it does not handle {@code
   * -h}.
   *
   * @param argv the command-line arguments
   * @param optstring the short options, in the format of {@link Getopt}
   * @param longopts the long options
   */
  public DaikonGetopt(String[] argv, String optstring, LongOpt[] longopts) {
    super(PROGNAME, argv, nonEmpty(optstring), longopts);
    this.usage = null;
    opterr = false;
  }

  /**
   * Creates a command-line option processor that recognizes short and long options, plus {@code -h}
   * and {@code --help}, which print the usage message and throw {@link Daikon.NormalTermination}.
   *
   * @param argv the command-line arguments
   * @param optstring the short options, in the format of {@link Getopt}; must not contain {@code h}
   * @param longopts the long options; must not contain {@code help}
   * @param usage the usage message to print for {@code -h} and {@code --help}
   */
  public DaikonGetopt(String[] argv, String optstring, LongOpt[] longopts, String usage) {
    super(PROGNAME, argv, withH(optstring), withHelp(longopts));
    this.usage = usage;
    opterr = false;
  }

  /**
   * Returns short options that are equivalent to the given ones but are not empty. Getopt replaces
   * an empty optstring by " ", which makes "- " a valid option.
   *
   * @param optstring short options, in the format of {@link Getopt}
   * @return {@code optstring}, or ":" if {@code optstring} is empty
   */
  private static String nonEmpty(String optstring) {
    // A leading ':' only makes Getopt return ':' rather than '?' for a missing argument, which
    // getopt() treats identically.
    return optstring.isEmpty() ? ":" : optstring;
  }

  /**
   * Returns the given short options plus {@code -h}.
   *
   * @param optstring short options, in the format of {@link Getopt}; must not contain {@code h}
   * @return {@code optstring} plus {@code -h}
   */
  private static String withH(String optstring) {
    if (optstring.indexOf('h') != -1) {
      throw new IllegalArgumentException("optstring already contains h: " + optstring);
    }
    return optstring + "h";
  }

  /**
   * Returns the given long options plus {@code --help}.
   *
   * @param longopts long options; must not contain {@code help}
   * @return {@code longopts} plus {@code --help}
   */
  private static LongOpt[] withHelp(LongOpt[] longopts) {
    for (LongOpt longopt : longopts) {
      if (longopt.getName().equals(Daikon.help_SWITCH)) {
        throw new IllegalArgumentException("longopts already contains " + Daikon.help_SWITCH);
      }
    }
    return ArraysPlume.append(
        longopts, new LongOpt(Daikon.help_SWITCH, LongOpt.NO_ARGUMENT, null, 'h'));
  }

  /**
   * Like {@link Getopt#getopt()}, but never returns {@code '?'} or {@code ':'}. Getopt returns
   * {@code ':'} for a missing argument if the optstring starts with {@code ':'}. If this was
   * constructed with a usage message, also never returns {@code 'h'}.
   *
   * @return the next option, as described in {@link Getopt#getopt()}
   * @throws Daikon.UserError if the next option is unrecognized, ambiguous, or malformed
   * @throws Daikon.NormalTermination after printing the usage message, if the next option is {@code
   *     -h} or {@code --help} and this was constructed with a usage message
   */
  @Override
  public int getopt() {
    int c = super.getopt();
    if (c == '?' || c == ':') {
      throw badOptionError();
    }
    if (c == 'h' && usage != null) {
      System.out.println(usage);
      throw new Daikon.NormalTermination();
    }
    return c;
  }

  /**
   * Returns an exception that describes the bad command-line option that {@link Getopt#getopt()}
   * just rejected.
   *
   * @return an exception that describes the bad command-line option
   */
  private Daikon.UserError badOptionError() {
    String message = badOptionMessage();
    if (usage != null) {
      message += "; run with -h for usage";
    }
    return new Daikon.UserError(message);
  }

  /**
   * Returns a description of the bad command-line option that {@link Getopt#getopt()} just
   * rejected.
   *
   * @return a description of the bad command-line option
   */
  private String badOptionMessage() {
    // For every bad long option, Getopt clears nextchar and advances optind past the option.  A
    // bad short option has no such guarantee, because it may be followed by other short options in
    // the same argument, as in "-xh".
    if ("".equals(nextchar) && argv[optind - 1].startsWith("--")) {
      return longOptionMessage(argv[optind - 1]);
    }
    char c = (char) optopt;
    if (c == ':' || optstring.indexOf(c) == -1) {
      return "Unrecognized command-line option -" + c;
    } else {
      return "Command-line option -" + c + " requires an argument";
    }
  }

  /**
   * Returns a description of a bad long command-line option. Uses {@code longind}, which Getopt
   * sets to the index of the long option that matched the argument, or to -1 if none matched.
   *
   * @param arg the bad command-line argument, which starts with "--"
   * @return a description of the bad command-line option
   */
  private String longOptionMessage(String arg) {
    int equalsPos = arg.indexOf('=');
    String name = (equalsPos == -1) ? arg.substring(2) : arg.substring(2, equalsPos);
    // Getopt treats an empty name, as in "--=foo", as a prefix of every long option.
    if (name.isEmpty()) {
      return "Unrecognized command-line option " + arg;
    }
    if (longind == -1) {
      return "Unrecognized command-line option --" + name;
    }
    // Every constructor passes long options to Getopt.
    LongOpt[] longopts = castNonNull(long_options);
    LongOpt match = longopts[longind];
    // For an inexact match, Getopt sets longind to the first long option that has the given
    // prefix.  The match is ambiguous if a later long option also has the prefix.
    if (!match.getName().equals(name)) {
      for (int i = longind + 1; i < longopts.length; i++) {
        if (longopts[i].getName().startsWith(name)) {
          return "Ambiguous command-line option --" + name;
        }
      }
    }
    if (equalsPos != -1 && match.getHasArg() == LongOpt.NO_ARGUMENT) {
      return "Command-line option --" + match.getName() + " does not take an argument";
    } else if (equalsPos == -1 && match.getHasArg() == LongOpt.REQUIRED_ARGUMENT) {
      return "Command-line option --" + match.getName() + " requires an argument";
    } else {
      throw new Daikon.BugInDaikon("Getopt rejected " + arg + " for no known reason");
    }
  }
}
