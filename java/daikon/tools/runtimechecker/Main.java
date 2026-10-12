package daikon.tools.runtimechecker;

import daikon.Daikon;
import daikon.DaikonGetopt;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.StringJoiner;

/**
 * Main entrypoint for the instrumenter. Passes control to whichever handler can handle the
 * user-specified command.
 */
public class Main extends CommandHandler {

  /**
   * Returns the usage message of this and of each handler.
   *
   * @param handlers the handlers whose usage messages to include
   * @return the usage message of this and of each handler
   */
  private String usageMessage(List<CommandHandler> handlers) {
    StringJoiner result = new StringJoiner(Daikon.lineSep);
    result.add(usageMessageString());
    for (CommandHandler h : handlers) {
      result.add(h.usageMessageString());
    }
    return result.toString();
  }

  /** The hint printed after an error message. */
  private static final String helpHint = "For more help, run the instrumenter with -h.";

  /**
   * Entry point for the instrumenter. Passes control to whichever handler can handle the
   * user-specified command.
   *
   * @param args the arguments to the program
   * @throws Daikon.DaikonTerminationException if the instrumenter should exit
   */
  public void nonStaticMain(String[] args) {

    List<CommandHandler> handlers =
        Collections.singletonList((CommandHandler) new InstrumentHandler());

    String[] commandAndArgs =
        DaikonGetopt.argsAfterLeadingOptions(args, () -> usageMessage(handlers));
    if (commandAndArgs.length < 1) {
      throw new Daikon.UserError("ERROR:  No command given." + Daikon.lineSep + helpHint);
    }
    if (commandAndArgs[0].toUpperCase(Locale.ENGLISH).equals("HELP")
        || commandAndArgs[0].equals("?")) {
      System.out.println(usageMessage(handlers));
      throw new Daikon.NormalTermination();
    }

    String command = commandAndArgs[0];

    CommandHandler h = null;
    for (CommandHandler handler : handlers) {
      if (handler.handles(command)) {
        h = handler;
        break;
      }
    }
    if (h == null) {
      throw new Daikon.UserError("Unknown command: " + command + Daikon.lineSep + helpHint);
    }

    if (!h.handle(commandAndArgs)) {
      System.err.println("The command you issued returned a failing status flag.");
      System.err.println("The instrumenter failed.");
      h.usageMessage();
      System.exit(1);
    }
  }

  public static void main(String[] args) {
    try {
      new Main().nonStaticMain(args);
    } catch (Daikon.DaikonTerminationException e) {
      Daikon.handleDaikonTerminationException(e);
    }
  }
}
