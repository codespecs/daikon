package daikon;

import gnu.getopt.Getopt;
import gnu.getopt.LongOpt;
import org.checkerframework.checker.nullness.qual.Nullable;

/**
 * A command-line option processor that throws a {@link Daikon.UserError} that describes a bad
 * command-line option, instead of printing a message and returning {@code '?'}.
 */
public class DaikonGetopt extends Getopt {

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
    opterr = false;
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
   * Like {@link Getopt#getopt()}, but never returns {@code '?'}.
   *
   * @return the next option, as described in {@link Getopt#getopt()}
   * @throws Daikon.UserError if the next option is unrecognized, ambiguous, or malformed
   */
  @Override
  public int getopt() {
    int c = super.getopt();
    if (c == '?') {
      throw badOptionError();
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
    if ("".equals(nextchar)
        && 0 < optind
        && optind <= argv.length
        && argv[optind - 1].startsWith("--")) {
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
