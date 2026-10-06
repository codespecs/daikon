package daikon.tools.runtimechecker;

import static java.nio.charset.StandardCharsets.UTF_8;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
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

  /** Prints the usage message to standard error. */
  public void usageMessage() {
    usageMessage(System.err);
  }

  /**
   * Returns the usage message.
   *
   * @return the usage message
   */
  public String usageMessageString() {
    ByteArrayOutputStream bytes = new ByteArrayOutputStream();
    usageMessage(new PrintStream(bytes, true, UTF_8));
    return bytes.toString(UTF_8).stripTrailing();
  }

  /**
   * Prints the usage message.
   *
   * @param out where to print the usage message
   */
  public void usageMessage(PrintStream out) {
    String[] classnameArray = getClass().getName().split("\\.");
    String simpleClassname = classnameArray[classnameArray.length - 1];

    String docFile = simpleClassname + ".doc";
    InputStream in = getClass().getResourceAsStream(docFile);
    if (in == null) {
      // This is an error message, so it goes to standard error even if `out` is standard output.
      System.err.println("Didn't find documentation " + docFile + " for " + getClass());
      return;
    }
    try (BufferedReader reader = new BufferedReader(new InputStreamReader(in, UTF_8))) {
      String line;
      while ((line = reader.readLine()) != null) {
        out.println(line);
      }
    } catch (IOException e) {
      throw new UncheckedIOException("problem reading " + docFile, e);
    }
  }
}
