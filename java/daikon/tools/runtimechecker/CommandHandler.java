package daikon.tools.runtimechecker;

import static java.nio.charset.StandardCharsets.UTF_8;

import daikon.Daikon;
import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;

/**
 * A command handler handles a set of commands. A command is the first argument given to the
 * instrumenter.
 */
public class CommandHandler {

  public boolean handles(String command) {
    throw new UnsupportedOperationException();
  }

  public boolean handle(String[] args) {
    throw new UnsupportedOperationException();
  }

  /**
   * Prints the usage message to standard error.
   *
   * @throws Daikon.BugInDaikon if the documentation file is missing
   */
  public void usageMessage() {
    System.err.println(usageMessageString());
  }

  /**
   * Returns the usage message, which is the contents of this class's documentation file.
   *
   * @return the usage message
   * @throws Daikon.BugInDaikon if the documentation file is missing
   */
  public String usageMessageString() {
    String[] classnameArray = getClass().getName().split("\\.");
    String simpleClassname = classnameArray[classnameArray.length - 1];

    String docFile = simpleClassname + ".doc";
    InputStream in = getClass().getResourceAsStream(docFile);
    if (in == null) {
      throw new Daikon.BugInDaikon("Didn't find documentation " + docFile + " for " + getClass());
    }
    try (in) {
      return new String(in.readAllBytes(), UTF_8).stripTrailing();
    } catch (IOException e) {
      throw new UncheckedIOException("problem reading " + docFile, e);
    }
  }
}
