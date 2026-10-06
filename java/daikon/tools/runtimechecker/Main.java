package daikon.tools.runtimechecker;

import static java.nio.charset.StandardCharsets.UTF_8;

import daikon.Daikon;
import daikon.DaikonGetopt;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.Collections;
import java.util.List;
import java.util.Locale;

/**
 * Main entrypoint for the instrumenter. Passes control to whichever handler can handle the
 * user-specified command.
 */
public class Main extends CommandHandler {

  /**
   * Prints the usage message of each handler.
   *
   * @param handlers the handlers whose usage messages to print
   * @param out where to print the usage messages
   */
  protected void usageMessage(List<CommandHandler> handlers, PrintStream out) {
    for (CommandHandler h : handlers) {
      h.usageMessage(out);
    }
  }

  /**
   * Returns the usage message of this and of each handler.
   *
   * @param handlers the handlers whose usage messages to include
   * @return the usage message of this and of each handler
   */
  private String usageMessage(List<CommandHandler> handlers) {
    ByteArrayOutputStream bytes = new ByteArrayOutputStream();
    PrintStream out = new PrintStream(bytes, true, UTF_8);
    usageMessage(out);
    usageMessage(handlers, out);
    return bytes.toString(UTF_8).stripTrailing();
  }

  /**
   * Entry point for the instrumenter. Passes control to whichever handler can handle the
   * user-specified command.
   *
   * @param args the arguments to the program
   */
  public void nonStaticMain(String[] args) {

    List<CommandHandler> handlers =
        Collections.singletonList((CommandHandler) new InstrumentHandler());

    String[] commandAndArgs;
    try {
      commandAndArgs = DaikonGetopt.argsAfterLeadingOptions(args, () -> usageMessage(handlers));
    } catch (Daikon.DaikonTerminationException e) {
      Daikon.handleDaikonTerminationException(e);
      return;
    }
    if (commandAndArgs.length < 1) {
      System.err.println("ERROR:  No command given.");
      System.err.println(
          "For more help, invoke the instrumenter with \"help\" as its sole argument.");
      System.exit(1);
    }
    if (commandAndArgs[0].toUpperCase(Locale.ENGLISH).equals("HELP")
        || commandAndArgs[0].equals("?")) {
      System.out.println(usageMessage(handlers));
      System.exit(0);
    }

    String command = commandAndArgs[0];

    boolean success = false;

    CommandHandler h = null;
    try {

      for (CommandHandler handler : handlers) {
        if (handler.handles(command)) {
          h = handler;
          success = h.handle(commandAndArgs);
          if (!success) {
            System.err.println("The command you issued returned a failing status flag.");
          }
          break;
        }
      }

    } catch (Throwable e) {
      System.out.println("Throwable thrown while handling command:" + e);
      e.printStackTrace();
      success = false;
    } finally {
      if (!success) {
        System.err.println("The instrumenter failed.");
        if (h == null) {
          System.err.println("Unknown command: " + command);
          System.err.println(
              "For more help, invoke the instrumenter with \"help\" as its sole argument.");
        } else {
          h.usageMessage();
        }
        System.exit(1);
      } else {
      }
    }
  }

  public static void main(String[] args) {
    Main main = new Main();
    main.nonStaticMain(args);
  }
}
