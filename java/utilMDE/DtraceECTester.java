// DtraceECTester.java
package utilMDE;
import java.util.Iterator;
import java.util.List;
import java.util.ArrayList;

public class DtraceECTester {

    public static void main (String[] args) {
        DtracePartitioner dec = new DtracePartitioner (args[0]);
        // while (dec.hasNext()) {
        //   String inv = (String) dec.next();
        //   System.out.println (inv);
        //   System.out.println (dec.deriveEquivalence (inv));
        // }
        MultiRandSelector mrs = new MultiRandSelector (10, dec);
        while (dec.hasNext()) {
            mrs.accept (dec.next());
        }
        List al = new ArrayList();

        for (Iterator i = mrs.valuesIter(); i.hasNext(); ) {
            al.add (i.next());
        }
        al = dec.patchValues (al);
        for (int i = 0; i < al.size(); i++) {
            System.out.println (al.get(i));
        }
    }

}
