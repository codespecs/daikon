package daikon.diff;

import daikon.inv.Invariant;
import java.io.*;
import daikon.*;
import daikon.inv.Invariant.OutputFormat;
  
/** MatchCountVisitor is a visitor that almost does the opposite of
 * PrintDifferingInvariantsVisitor.  MatchCount prints invariant pairs
 * if they are the same, and only if they are a part of a conditional ppt.
 * The visitor also accumulates some state during its traversal for statistics,
 * and can report the match precision.
 * 
 *
 * @author Lee Lin 2/14/2002
 **/
public class MatchCountVisitor extends PrintAllVisitor {

    private static int cnt = 0;
    private static int recall = 0;

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
    if (shouldPrint(inv1, inv2)) {
      super.visit(node);
    }
  }

  /**
   * Returns true if the pair of invariants should be printed
   **/
  protected static boolean shouldPrint(Invariant inv1, Invariant inv2) {
   
      cnt++;

    int rel = DetailedStatisticsVisitor.determineRelationship(inv1, inv2);
    if (rel == DetailedStatisticsVisitor.REL_SAME_JUST1_JUST2 )
	//   rel == DetailedStatisticsVisitor.REL_SAME_UNJUST1_UNJUST2) 
	{
	// now you have a match
	recall++;
	System.out.println (inv1.format_using(OutputFormat.JAVA));
    }


    return false;
  }

    public double calcPrecision() {
	if (cnt == 0) return -1;
	return (double) recall / cnt;
    }

    
}




