
// MultiDiff.java

package daikon.diff;
import daikon.inv.*;
import daikon.*;
import java.io.*;
import utilMDE.*;
import java.util.*;

/** <B>MultiDiff</B> is an executable application that performs the same
 *  functionality as Diff with a few key change.  First, it always outputs
 *  the histogram even when two files are called.  Second, it allows the
 *  option of creating *.spinfo based on the invariants found
 */

public class MultiDiff {

    public static void main (String[] args)
        throws IOException, ClassNotFoundException,
               InstantiationException, IllegalAccessException {
        PrintStream out = new PrintStream
            (new FileOutputStream ("rand_sel.spinfo"));
        /*
          try {
            if (args.length != 0) {
                FileOutputStream file = new FileOutputStream (args[0]);
                out = new PrintStream (file);
            }
        }

        catch (IOException e) {e.printStackTrace(); }
        */
        MultiDiffVisitor.setForSpinfoOut (out);
        Diff.main (args);
    }

}
