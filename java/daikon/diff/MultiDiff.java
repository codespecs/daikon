// MultiDiff.java

package daikon.diff;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
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
      throws IOException,
          ClassNotFoundException,
          InstantiationException,
          IllegalAccessException,
          NoSuchMethodException,
          InvocationTargetException {
    try {
      mainHelper(args);
    } catch (daikon.Daikon.DaikonTerminationException e) {
      daikon.Daikon.handleDaikonTerminationException(e);
    }
  }

  /**
   * This does the work of {@link #main(String[])}, but it never calls System.exit, so it is
   * appropriate to be called progrmmatically.
   */
  public static void mainHelper(final String[] args)
      throws IOException,
          ClassNotFoundException,
          InstantiationException,
          IllegalAccessException,
          InvocationTargetException,
          NoSuchMethodException {
    try (FileOutputStream fos = new FileOutputStream("rand_sel.spinfo")) {
      PrintStream out = new PrintStream(fos);
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
}
