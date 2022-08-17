package daikon.tools.runtimechecker;

import static java.nio.charset.StandardCharsets.UTF_8;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
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

  public void usageMessage() {
    String[] classnameArray = getClass().getName().split("\\.");
    String simpleClassname = classnameArray[classnameArray.length - 1];

    String docFile = simpleClassname + ".doc";
    InputStream in = getClass().getResourceAsStream(docFile);
    if (in == null) {
      System.err.println("Didn't find documentation for " + getClass());
      return;
    }
    BufferedReader reader = new BufferedReader(new InputStreamReader(in, UTF_8));
    try {
      String line;
      while ((line = reader.readLine()) != null) {
        System.err.println(line);
      }
    } catch (IOException e) {
      try {
        reader.close();
      } catch (IOException e2) {
        // ignore second exception
      }
      throw new Error(e);
    }
    try {
      reader.close();
    } catch (IOException e) {
      throw new UncheckedIOException("problem closing " + docFile, e);
    }
  }
}
