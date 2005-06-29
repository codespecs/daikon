// MultiDiffVisitor.java

package daikon.diff;

import daikon.*;
import daikon.inv.*;
import java.io.*;
import java.util.*;

/**
 * <B>MultiDiffVisitor</B> is a state-storing NodeVisitor that works
 * across multiple files regardless of the current two-file infrastructure.
 * This allows the selection of very unique invariants that occur once over
 * an entire set of trace files
 **/

public class MultiDiffVisitor extends PrintNullDiffVisitor {

    protected PptMap currMap;
    private HashSet programPointsList;
    private HashMap freqList;
    private HashSet justifiedList;
    private int total = 0;
    private static boolean spinfoMode = false;
    private static PrintStream out = System.out;

    public MultiDiffVisitor (PptMap firstMap) {
        // I'll always want System.out, and never verbose!
        super (System.out, false);
        currMap = firstMap;
        programPointsList = new HashSet();
        freqList = new HashMap();
        justifiedList = new HashSet();
    }

    public static void setForSpinfoOut (OutputStream out_os) {
        MultiDiffVisitor.out = new PrintStream (out_os, true);
        spinfoMode = true;
    }

    public void visit (RootNode node) {

        total++;
        super.visit (node);

    }

    public void visit (InvNode node) {
        Invariant inv1 = node.getInv1();
        Invariant inv2 = node.getInv2();

        // Use the histogram map
        if (inv1 != null && shouldPrint (inv1, inv2)) {
            String tmpStr = inv1.ppt.name();
            // example:
            // tmpStr == FeedTheCat.measure(III)I:::ENTER(b, this.bCap
            String thisPptName = tmpStr.substring (0,
                                                   tmpStr.lastIndexOf ('('));

            programPointsList.add (thisPptName);
            String key = thisPptName + "$" + inv1.format_using(OutputFormat.JAVA);
            Integer val = (Integer) freqList.get (key);
            if (val == null) {
                // Use one as default, obviously
                freqList.put (key, new Integer (1));
            }
            // increment if it's already there
            else {
                freqList.put ( key,
                               new Integer (val.intValue() + 1));
            }

            // add to justified list if this was justified once
            //    if (inv1.justified()) {
                justifiedList.add (key);
                // }
        }

    }

    /** Prints everything in the goodList. */
    public void printAll () {

        if (spinfoMode) {
            printAllSpinfo();
            return;
        }

        // keeps track of supressed invariants due to appearing in
        // every sample of the MultiDiff
        int kill = 0;
        int unjustifiedKill = 0;
        ArrayList bigList = new ArrayList();

        // New historgram stuff
        System.out.println ("Histogram**************");

        // This gets all of the output in the format:
        // inv.ppt.name() + "$" + inv1.format_java() + " Count = " + freq
        for (Iterator i = freqList.keySet().iterator(); i.hasNext(); ) {
            String str = (String) i.next();
            int freq = ((Integer) freqList.get(str)).intValue();
            if (freq < total && justifiedList.contains (str)) {
                bigList.add (str + " Count =  " + freq);
                // System.out.println (str + " Count =  " + freq);

            }
            // don't print something true in EVERY set
            else if (freq == total) { kill++; }
            // don't print something that was never justified
            else unjustifiedKill++;
        }
        System.out.println ("Invariants appearing in all: " + kill);
        System.out.println ("Invariants never justified: " + unjustifiedKill);

        // Now build the final HashMap that will have the following
        // mapping:  program point names ->
        //                      ArrayList of inv.format_java() with frequency

        HashMap lastMap = new HashMap();
        // One pass to fill each mapping with an empty ArrayList
        for (Iterator i = programPointsList.iterator(); i.hasNext(); ) {
            String key = (String) i.next();
            lastMap.put (key, new ArrayList());
        }

        // Now to populate those ArrayLists
        for (int i = 0; i < bigList.size(); i++) {
            String str = (String) bigList.get(i);
            StringTokenizer st = new StringTokenizer (str, "$");
            String key = st.nextToken();
            String data = st.nextToken();
            try {
            ((ArrayList) lastMap.get(key)).add (data);
            } catch (Exception e) {System.out.println (key + " error in MultiDiffVisitor");}
        }

        // print it all
        for (Iterator i = lastMap.entrySet().iterator(); i.hasNext(); ) {
            Map.Entry entry = (Map.Entry) i.next();
            String key = (String) entry.getKey();
            ArrayList al = (ArrayList) entry.getValue();
            // don't print anything if there are no selective invariants
            if (al.size() == 0) continue;
            System.out.println ();
            System.out.println (key + "*****************");
            System.out.println ();
            for (int ii = 0; ii < al.size(); ii++) {
                System.out.println (al.get(ii));
            }
        }
        System.out.println ();
        System.out.println ();
    }

     /** Prints everything in the goodList, outputs as spinfo. */
    public void printAllSpinfo() {

        // keeps track of supressed invariants due to appearing in
        // every sample of the MultiDiff
        int kill = 0;
        ArrayList bigList = new ArrayList();


        // This gets all of the output in the format:
        // inv.ppt.name() + "$" + inv1.format_java()
        for (Iterator i = freqList.keySet().iterator(); i.hasNext(); ) {
            String str = (String) i.next();
            int freq = ((Integer) freqList.get(str)).intValue();
            if (freq < total && justifiedList.contains (str)) {
                // just want the String on its own line
                bigList.add (str);
            }

        }


        // Now build the final HashMap that will have the following
        // mapping:  program point names ->
        //                      ArrayList of inv.format_java() with frequency

        HashMap lastMap = new HashMap();
        // One pass to fill each mapping with an empty ArrayList
        for (Iterator i = programPointsList.iterator(); i.hasNext(); ) {
            String key = (String) i.next();
            lastMap.put (key, new ArrayList());
        }

        // Now to populate those ArrayLists
        for (int i = 0; i < bigList.size(); i++) {
            String str = (String) bigList.get(i);
            StringTokenizer st = new StringTokenizer (str, "$");
            String key = st.nextToken();
            String data = st.nextToken();
            try {
            ((ArrayList) lastMap.get(key)).add (data);
            } catch (Exception e) { out.println (key + " error in MultiDiffVisitor");}
        }

        // print it all
        ArrayList theKeys = new ArrayList (lastMap.keySet());
        // sort them so that multiple exits will end up being adjacent
        // to each other when they are from the same method
        Collections.sort (theKeys);
        String lastPpt = "";
        for (Iterator i = theKeys.iterator(); i.hasNext(); ) {
            String key = (String) i.next();
            ArrayList al = (ArrayList) lastMap.get(key);
            // don't print anything if there are no selective invariants

            if (al.size() == 0) continue;

            // Get rid of the extra stuff like (III)I:::ENTER
            // at the end of each of the program points

            // use the fact that we only want the stuff before the first '('
            StringTokenizer pToke = new StringTokenizer (key, "(");

            // sadly we only want EXIT values, so throw out any ENTERs
            // because the spinfo won't deal well with them anyway

            //            if (key.indexOf ("ENTER") != -1) continue;


            // Now we don't want to reprint the program point name
            // again in the spinfo file if it has been printed from
            // the previous Ppt that exited at a different point -LL

            String thisPpt = pToke.nextToken();

            if (! lastPpt.equals (thisPpt)) {
                out.println ();
                out.println ("PPT_NAME " + thisPpt);

                lastPpt = thisPpt;
            }
            for (int ii = 0; ii < al.size(); ii++) {
                out.println (al.get(ii));

            }
        }

    }

    protected boolean shouldPrint (Invariant inv1, Invariant inv2) {
        return true; // super.shouldPrint (inv1, inv2) &&
            //    inv1.format().toString().indexOf(">") == -1 &&
            // inv1.format().toString().indexOf("orig") == -1;
    }

}
