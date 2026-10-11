package daikon;

import static daikon.tools.nullness.NullnessUtil.castNonNull;

import gnu.getopt.Getopt;
import gnu.getopt.LongOpt;
import org.plumelib.util.ArraysPlume;

/**
 * A command-line option processor that throws a {@link Daikon.UserError} that describes a bad
 * command-line option, instead of printing a message and returning {@code '?'} or {@code ':'}. It
 * also handles {@code -h} and {@code --help}: it prints the usage message and throws {@link
 * Daikon.NormalTermination}.
 */
public class DaikonGetopt extends Getopt {

  /**
   * The name passed to {@link Getopt}, which uses it only in messages that DaikonGetopt suppresses.
   */
  private static final String PROGNAME = "DaikonGetopt";

  /** The usage message printed for {@code -h} and {@code --help}. */
  private final String usage;

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
    super(
        PROGNAME,
        argv,
        optstring + "h",
        ArraysPlume.append(
            longopts, new LongOpt(Daikon.help_SWITCH, LongOpt.NO_ARGUMENT, null, 'h')));
    this.usage = usage;
    opterr = false;
  }

  /**
   * Like {@link Getopt#getopt()}, but never returns {@code '?'}, {@code ':'}, or {@code 'h'}.
   * Getopt returns {@code ':'} for a missing argument if the optstring starts with {@code ':'}.
   *
   * @return the next option, as described in {@link Getopt#getopt()}
   * @throws Daikon.UserError if the next option is unrecognized, ambiguous, or malformed
   * @throws Daikon.NormalTermination after printing the usage message, if the next option is {@code
   *     -h} or {@code --help}
   */
  @Override
  public int getopt() {
    int c = super.getopt();
    if (c == '?' || c == ':') {
      throw new Daikon.UserError(badOptionMessage() + "; run with -h for usage");
    }
    if (c == 'h') {
      System.out.println(usage);
      throw new Daikon.NormalTermination();
    }
    return c;
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
