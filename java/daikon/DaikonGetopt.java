package daikon;

import gnu.getopt.Getopt;
import gnu.getopt.LongOpt;
import java.util.Arrays;
import org.checkerframework.checker.nullness.qual.Nullable;

/**
 * A command-line option processor that throws a {@link Daikon.UserError} that describes a bad
 * command-line option, instead of printing a message and returning {@code '?'} or {@code ':'}.
 *
 * <p>If constructed with a usage message, it also handles {@code -h} and {@code --help}: it prints
 * the usage message and throws {@link Daikon.NormalTermination}.
 */
public class DaikonGetopt extends Getopt {

  /**
   * The usage message printed for {@code -h} and {@code --help}, or null if the caller handles
   * them.
   */
  private final @Nullable String usage;

  /** Text appended to the description of a bad command-line option, or null to append nothing. */
  private @Nullable String usageHint = "run with -h for usage";

  /**
   * Creates a command-line option processor that recognizes short and long options.
   *
   * @param progname the name of the program, for use in messages
   * @param argv the command-line arguments
   * @param optstring the short options, in the format of {@link Getopt}
   * @param longopts the long options
   */
  public DaikonGetopt(String progname, String[] argv, String optstring, LongOpt[] longopts) {
    super(progname, argv, optstring, longopts);
    this.usage = null;
    opterr = false;
  }

  /**
   * Creates a command-line option processor that recognizes short and long options, plus {@code -h}
   * and {@code --help}, which print the usage message and throw {@link Daikon.NormalTermination}.
   *
   * @param progname the name of the program, for use in messages
   * @param argv the command-line arguments
   * @param optstring the short options, in the format of {@link Getopt}; must not contain {@code h}
   * @param longopts the long options; must not contain {@code help}
   * @param usage the usage message to print for {@code -h} and {@code --help}
   */
  public DaikonGetopt(
      String progname, String[] argv, String optstring, LongOpt[] longopts, String usage) {
    super(progname, argv, optstring + "h", withHelp(longopts));
    this.usage = usage;
    opterr = false;
  }

  /**
   * Returns the given long options plus {@code --help}.
   *
   * @param longopts long options
   * @return {@code longopts} plus {@code --help}
   */
  private static LongOpt[] withHelp(LongOpt[] longopts) {
    LongOpt[] result = Arrays.copyOf(longopts, longopts.length + 1);
    result[longopts.length] = new LongOpt(Daikon.help_SWITCH, LongOpt.NO_ARGUMENT, null, 'h');
    return result;
  }

  /**
   * Processes the command-line arguments of a program that takes no options other than {@code -h}
   * and {@code --help}.
   *
   * @param progname the name of the program, for use in messages
   * @param argv the command-line arguments
   * @param usage the usage message to print for {@code -h} and {@code --help}
   * @return the arguments that are not options
   * @throws Daikon.NormalTermination after printing the usage message, if an argument requests it
   * @throws Daikon.UserError if an argument is any other option
   */
  public static String[] nonOptionArgs(String progname, String[] argv, String usage) {
    DaikonGetopt g = new DaikonGetopt(progname, argv, "", new LongOpt[0], usage);
    // Every option other than -h and --help is bad, so getopt() returns only -1 or throws.
    int c = g.getopt();
    if (c != -1) {
      throw new Daikon.BugInDaikon("getopt() returned " + c);
    }
    return Arrays.copyOfRange(argv, g.getOptind(), argv.length);
  }

  /**
   * Sets the text that is appended to the description of a bad command-line option. The default
   * tells the user to run the program with -h.
   *
   * @param usageHint text appended to the description of a bad command-line option, or null to
   *     append nothing
   */
  public void setUsageHint(@Nullable String usageHint) {
    this.usageHint = usageHint;
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
    if (usageHint != null) {
      message += "; " + usageHint;
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
    LongOpt[] longopts = long_options;
    // Getopt treats an empty name, as in "--=foo", as a prefix of every long option.
    if (name.isEmpty()) {
      return "Unrecognized command-line option " + arg;
    }
    if (longopts == null || longind == -1) {
      return "Unrecognized command-line option --" + name;
    }
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
      return "Bad command-line option " + arg;
    }
  }
}
