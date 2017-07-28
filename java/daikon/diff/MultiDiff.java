// MultiDiff.java

package daikon.diff;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.lang.ClassNotFoundException;
import java.lang.IllegalAccessException;
import java.lang.InstantiationException;
import java.lang.NoSuchMethodException;
import java.lang.reflect.InvocationTargetException;

/**
 * <B>MultiDiff</B> is an executable application that performs the same functionality as Diff with a
 * few key change. First, it always outputs the histogram even when two files are called. Second, it
 * allows the option of creating *.spinfo based on the invariants found.
 */
public class MultiDiff {
  private MultiDiff() {
    throw new Error("do not instantiate");
  }

  public static void main(String[] args)
      throws IOException, ClassNotFoundException, InstantiationException, IllegalAccessException,
          NoSuchMethodException, InvocationTargetException {
    try {
      mainHelper(args);
    } catch (daikon.Daikon.TerminationMessage e) {
      daikon.Daikon.handleTerminationMessage(e);
    }
    // Any exception other than daikon.Daikon.TerminationMessage gets propagated.
    // This simplifies debugging by showing the stack trace.
  }

  /**
   * This does the work of main, but it never calls System.exit, so it is appropriate to be called
   * progrmmatically. Termination of the program with a message to the user is indicated by throwing
   * daikon.Daikon.TerminationMessage.
   *
   * @see #main(String[])
   * @see daikon.Daikon.TerminationMessage
   */
  public static void mainHelper(final String[] args)
      throws IOException, ClassNotFoundException, InstantiationException, IllegalAccessException,
          InvocationTargetException, NoSuchMethodException {
    PrintStream out = new PrintStream(new FileOutputStream("rand_sel.spinfo"));
    /*
      try {
        if (args.length != 0) {
            FileOutputStream file = new FileOutputStream (args[0]);
            out = new PrintStream (file);
        }
    }

    catch (IOException e) {e.printStackTrace(); }
    */
    MultiDiffVisitor.setForSpinfoOut(out);
    Diff.main(args);
  }
}
