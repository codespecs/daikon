package daikon.tools.runtimechecker;

import java.util.ArrayList;
import java.util.List;

/**
 * Main entrypoint for the instrumenter. Asks which handler can handle the command given by
 * the user, and passes control to whoever does.
 */
public class Main extends CommandHandler {

    protected void usageMessage(List<CommandHandler> handlers) {
        for (int i = 0 ; i < handlers.size() ; i++) {
            CommandHandler h = (CommandHandler)handlers.get(i);
            h.usageMessage();
        }
    }

    public void nonStaticMain(String[] args) {

        List<CommandHandler> handlers = new ArrayList<CommandHandler>();
        handlers.add(new InstrumentHandler());

        if (args.length < 1) {
            System.err.println("No command given.");
            System.err
                .println("For more help, invoke the instrumenter with \"help\" as its sole argument.");
            System.exit(1);
        }
        if (args[0].toUpperCase().equals("HELP") || args[0].equals("?")) {
            usageMessage();
            System.err.println("Commands");
            System.err.println("========");
            System.err.println();
            System.err.println();
            usageMessage(handlers);
            System.exit(0);
        }

        String command = args[0];

        boolean success = false;

        CommandHandler h = null;
        try {

            for (int i = 0 ; i < handlers.size() ; i++) {
                h = (CommandHandler)handlers.get(i);
                if (h.handles(command)) {
                    success = h.handle(args);
                    if (!success) {
                        System.err
                            .println("The command you issued returned a failing status flag.");
                    }
                    break;
                }
            }

        } catch (Exception e) {
            System.err.println("Exception thrown while handling command:" + e);
            e.printStackTrace();
            success = false;
        } finally {
            if (!success) {
                System.err.println("The instrumenter failed.");
                h.usageMessage();
                System.exit(1);
            } else {
                System.exit(0);
            }
        }
    }

    public static void main(String[] args) {
        Main main = new Main();
        main.nonStaticMain(args);
    }

}
