package daikon;

import gnu.getopt.Getopt;
import gnu.getopt.LongOpt;

/**
 * A command-line option processor that does not print a message about a bad command-line option.
 * Instead, when {@link #getopt()} returns {@code '?'}, a client should {@code throw
 * g.badOptionError()}, which describes the problem.
 */
public class DaikonGetopt extends Getopt {

  /**
   * Creates a command-line option processor that recognizes only short options.
   *
   * @param progname the name of the program, for use in messages
   * @param argv the command-line arguments
   * @param optstring the short options, in the format of {@link Getopt}
   */
  public DaikonGetopt(String progname, String[] argv, String optstring) {
    super(progname, argv, optstring);
    opterr = false;
  }

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
   * Returns an exception to throw when {@link #getopt()} returns {@code '?'}, which indicates an
   * unrecognized, ambiguous, or malformed command-line option.
   *
   * @return an exception that describes the bad command-line option
   */
  public Daikon.UserError badOptionError() {
    return new Daikon.UserError(badOptionMessage() + "; run with -h for usage");
  }

  /**
   * Returns a description of the bad command-line option that {@link #getopt()} just rejected.
   *
   * @return a description of the bad command-line option
   */
  private String badOptionMessage() {
    // For every bad long option, Getopt clears nextchar and advances optind past the option.  A
    // bad short option has no such guarantee, because it may be followed by other short options in
    // the same argument, as in "-xh".
    if (long_options != null
        && "".equals(nextchar)
        && 0 < optind
        && optind <= argv.length
        && argv[optind - 1].startsWith("--")) {
      return longOptionMessage(argv[optind - 1], long_options);
    }
    char c = (char) optopt;
    if (c == ':' || optstring.indexOf(c) == -1) {
      return "Unrecognized command-line option -" + c;
    } else {
      return "Command-line option -" + c + " requires an argument";
    }
  }

  /**
   * Returns a description of a bad long command-line option.
   *
   * @param arg the bad command-line argument, which starts with "--"
   * @param longopts the long options that this processor recognizes
   * @return a description of the bad command-line option
   */
  private static String longOptionMessage(String arg, LongOpt[] longopts) {
    int equalsPos = arg.indexOf('=');
    String name = (equalsPos == -1) ? arg.substring(2) : arg.substring(2, equalsPos);

    // This mimics how Getopt matches a long option:  an exact match takes precedence, and
    // otherwise the name may be an unambiguous prefix of a long option.
    LongOpt match = null;
    boolean ambiguous = false;
    for (LongOpt longopt : longopts) {
      String longoptName = longopt.getName();
      if (longoptName.equals(name)) {
        match = longopt;
        ambiguous = false;
        break;
      } else if (longoptName.startsWith(name)) {
        if (match == null) {
          match = longopt;
        } else {
          ambiguous = true;
        }
      }
    }

    if (ambiguous) {
      return "Ambiguous command-line option --" + name;
    } else if (match == null) {
      return "Unrecognized command-line option --" + name;
    } else if (equalsPos != -1 && match.getHasArg() == LongOpt.NO_ARGUMENT) {
      return "Command-line option --" + match.getName() + " does not take an argument";
    } else if (equalsPos == -1 && match.getHasArg() == LongOpt.REQUIRED_ARGUMENT) {
      return "Command-line option --" + match.getName() + " requires an argument";
    } else {
      return "Bad command-line option " + arg;
    }
  }
}
