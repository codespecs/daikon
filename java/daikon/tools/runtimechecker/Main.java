package daikon.tools.runtimechecker;

import java.util.Collections;
import java.util.List;
import java.util.Locale;

/**
 * Main entrypoint for the instrumenter. Passes control to whichever handler can handle the
 * user-specified command.
 */
public class Main extends CommandHandler {

  protected void usageMessage(List<CommandHandler> handlers) {
    for (CommandHandler h : handlers) {
      h.usageMessage();
    }
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

    if (args.length < 1) {
      System.err.println("ERROR:  No command given.");
      System.err.println(
          "For more help, invoke the instrumenter with \"help\" as its sole argument.");
      System.exit(1);
    }
    if (args[0].toUpperCase(Locale.ENGLISH).equals("HELP") || args[0].equals("?")) {
      usageMessage();
      usageMessage(handlers);
      System.exit(0);
    }

    String command = args[0];

    boolean success = false;

    CommandHandler h = null;
    try {

      for (CommandHandler handler : handlers) {
        if (handler.handles(command)) {
          h = handler;
          success = h.handle(args);
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
