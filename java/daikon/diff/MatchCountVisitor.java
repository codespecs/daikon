package daikon.diff;

import daikon.inv.Invariant;
import java.io.*;
import daikon.*;
import daikon.inv.Invariant.OutputFormat;
import java.util.*;

/**
 * MatchCountVisitor is a visitor that almost does the opposite of
 * PrintDifferingInvariantsVisitor.  MatchCount prints invariant pairs
 * if they are the same, and only if they are a part of a conditional ppt.
 * The visitor also accumulates some state during its traversal for statistics,
 * and can report the match precision.
 *
 *
 * @author Lee Lin
 **/
public class MatchCountVisitor extends PrintAllVisitor {

    // invariants found by the splitting
    private HashSet cnt = new HashSet();
    // target set of invariants
    private HashSet targSet = new HashSet();
    // invariants found matching
    private HashSet recall = new HashSet();

  public MatchCountVisitor (PrintStream ps, boolean verbose,
                                         boolean printEmptyPpts) {
    super(ps, verbose, printEmptyPpts);
  }

    // throw out Program points that are not Conditional,
    // meaning they were NOT added from our splitters
    public void visit (PptNode node) {
        PptTopLevel ppt = node.getPpt1();
        if (! (ppt instanceof PptConditional)) return;
        else super.visit (node);
    }

  public void visit(InvNode node) {
    Invariant inv1 = node.getInv1();
    Invariant inv2 = node.getInv2();
    String key1 = "";
    String key2 = "";

    if (inv1 != null) {
        String tmpStr1 = inv1.ppt.name;
        //Contest.smallestRoom(II)I:::EXIT;condition="not(max <= num)"
        String thisPptName1 = tmpStr1.substring (0,
                                                tmpStr1.lastIndexOf (';'));
        key1 = thisPptName1 + "$" + inv1.format_using(OutputFormat.JAVA);
        // checks for justification
        if (shouldPrint (inv1, inv1))
        cnt.add (key1);
    }

    if (inv2 != null && shouldPrint (inv2, inv2)) {
        String tmpStr2 = inv2.ppt.name;
        // since inv2 is part of the "doctored" PptMap, the
        // inv2.ppt.name is actually NOT a conditional invariant at all.
        String thisPptName2 = tmpStr2.substring (0,
                                                tmpStr2.lastIndexOf ('('));
        key2 = thisPptName2 + "$" + inv2.format_using(OutputFormat.JAVA);
        targSet.add (key2);
        System.out.println ("ADDED " + key2);

    }

    if (shouldPrint(inv1, inv2)) {
        // inv1 and inv2 should be the same, so it doesn't matter
        // which one we choose when adding to recall -LL
        recall.add (key1);
        System.out.println(key1);
    }
  }

  /** Returns true if the pair of invariants should be printed **/
  protected static boolean shouldPrint(Invariant inv1, Invariant inv2) {

    int rel = DetailedStatisticsVisitor.determineRelationship(inv1, inv2);
    if (rel == DetailedStatisticsVisitor.REL_SAME_JUST1_JUST2 ) {

        // got rid of unjustified
        //   rel == DetailedStatisticsVisitor.REL_SAME_UNJUST1_UNJUST2)

        // Added to get rid of constants other than -1, 0, 1 in the
        // invariant's format_java() string... this change was made to
        // filter out targets that could never really be achived
        // example:   num >= 10378

        if (filterOut (inv1) || filterOut (inv2)) {
            return false;
        }

        // now you have a match

            return true;
    }


    return false;
  }

    /** returns true iff any token of inv.format_java() contains
     *  a number other than -1, 0, 1 or is null. */
    private static boolean filterOut (Invariant inv) {
        if (inv == null) return true;
        String str = inv.format_using(OutputFormat.JAVA);
        StringTokenizer st = new StringTokenizer (str, " ()");
        while (st.hasMoreTokens()) {
            String oneToken = st.nextToken();
            try {
                char firstChar = oneToken.charAt(0);
                // remember identifiers can not begin with [0-9\-]
                if (Character.isDigit (firstChar) || firstChar == '-') {
                    if (acceptableNumber (oneToken)) {
                        continue;
                    }
                    else return true;
                }

            }
            catch (NumberFormatException e) {
                System.out.println ("Should never get here... " +
                                     "NumberFormatException in filterOut: " +
                                    oneToken);
                continue;
            }
        }
        return false;
    }

    public double calcRecall() {
        System.out.println ("Recall: "+ recall.size() +" / "+ targSet.size());
        if (targSet.size() == 0) return -1; // avoids divide by zero
        return (double) recall.size() / targSet.size();
    }


    /** returns true iff numLiteral represents a numeric
     * literal string of integer or float that we believe
     * will be useful for a splitting condition.  Usually that
     * includes -1, 0, 1, and any other numeric literal
     * found in the source code.  */
    private static boolean acceptableNumber (String numLiteral) {

        // need to make sure that it is an integer vs. floating
        // point number

        // could be float, look for "."
        if (numLiteral.indexOf (".") > -1) {
            float fnum = Float.parseFloat (numLiteral);
            // for now, accept all floats
            return true;
        }
        // not float, must be int
        else {
            int num = Integer.parseInt (numLiteral);

            // accept -1, 0, 1
            if (num == -1 || num == 0 || num == 1)
                return true;
            else return false;
        }

    }

    public double calcPrecision() {

        System.out.println ("Prec: "+ recall.size() +" / "+ cnt.size());
        if (cnt.size() == 0) return -1; // to avoid a divide by zero -LL
        return (double) recall.size() / cnt.size();
    }

}
