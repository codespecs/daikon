// DtraceEquivalenceChecker.java
package utilMDE;
import java.util.*;
import java.io.*;

/** Takes in a fileName and becomes an invocation iterator for use
 *  with MultiRandSelector
 *
 *  Invokations in the same Program Point are in the same equivalence
 *  class.  The reason is, we want to select say 10 samples for each
 *  program point, so this object is able to determine the equivalence
 *  class based on the PPT name

*/

public class DtraceEquivalenceChecker
    implements EquivalenceChecker, Iterator
{

    private BufferedReader br;
    private HashSet nonceAlert;
    private String fileName;

    public DtraceEquivalenceChecker (String fileName) {
        try {
            this.fileName = fileName;
            br = UtilMDE.BufferedFileReader (fileName);
            nonceAlert = new HashSet();
        } catch (Exception e) {e.printStackTrace(); }
    }

    public boolean hasNext() {
        try {
            return br.ready();
        } catch (Exception e) {e.printStackTrace(); return false; }
    }

    public void remove () {

    }

    public Object next() {
        try {
            String ret = grabNextInvocation ();
            if (ret.indexOf ("EXIT") != -1) {
                if (!br.ready()) return "";
                return next();
            }
            else return ret;
        } catch (Exception e) {e.printStackTrace(); }
        System.out.println ("Should never get here, all bets are off.");
        return null;
    }

    private String grabNextInvocation () throws IOException {
        StringBuffer sb = new StringBuffer();
        while (br.ready()) {
            String line = br.readLine();
            if (line.trim().equals ("")) {
                break;
            }
            sb.append(line.trim()).append ("\n");
        }
        return sb.toString();
    }


    public Object deriveEquivalence (Object invocation) {
        String s = (String) invocation;
        if (s.indexOf ('\n') == -1) return null;
        return s.substring (0, s.indexOf ('\n'));
    }

    /** Finds the exits that correspond to Enters
     *  <br>Modifies: none
     *  <br>Returns: An ArrayList containing all of the elements of 'enters'
     *    except that any ENTER ppt invocations will definitely have
     *    a corresponding EXIT ppt invocation following them.  The original
     *    order is NOT guaranteed.
     */

    public ArrayList patchValues (ArrayList enters) {
        try {
            // Build a hashmap of values to watch
            HashMap nonceMap = new HashMap();
            for (int i = 0; i < enters.size(); i++) {
                String enterStr = (String) enters.get(i);
                // it could be an OBJECT or CLASS invocation ppt, ignore those
                // by putting them in the HashMap to themselves, they'll
                // be reaped up later
                if (enterStr.indexOf ("ENTER") == -1) {
                    nonceMap.put (enterStr, enterStr);
                    continue;
                }

                int theNonce = calcNonce (enterStr);
                nonceMap.put (new Integer (theNonce), enterStr);
            }



            br = new BufferedReader (new FileReader (fileName));
            while (br.ready()) {
                String nextInvo = grabNextInvocation();
                if (nextInvo.indexOf ("EXIT") == -1) continue;
                int invoNonce = calcNonce (nextInvo);
                Integer key = new Integer (invoNonce);
                String enterInvo = (String) nonceMap.get (key);
                if (enterInvo != null) {
                    nonceMap.put (key, enterInvo + "\n" + nextInvo);
                }
            }

            ArrayList al = new ArrayList();
            for (Iterator i = nonceMap.values().iterator(); i.hasNext(); ) {
                al.add (i.next());
            }
            return al;


        } catch (IOException e) {e.printStackTrace(); }
        return enters;
    }

    private int calcNonce (String invocation) {
        StringTokenizer st = new StringTokenizer (invocation, "\n");
        while (st.hasMoreTokens()) {
            String line = st.nextToken();
            if (line.equals ("this_invocation_nonce"))
                return Integer.parseInt (st.nextToken());
        }
        throw new RuntimeException ("This invocation didn't contain a nonce: "
                                    + invocation);

    }

}
