package daikon.diff;

import daikon.inv.Invariant;
import java.io.*;
import daikon.*;
import java.util.*;
import utilMDE.*;

/** <B>XorInvariantsVisitor</B> is a visitor that performs a
 * standard Diff on two PptMaps, that is, finds the set of
 * Invariants in the XOR set of two PptMaps.  However, while
 * those XOR Invariants were the end product of standard diff,
 * this visitor is useful when the XOR set is a means to an
 * end, since you get back a data structure containing the
 * XOR set. <P> Currently, this visitor actually modifies
 * the first of the two PptMaps.  This might be an undesirable
 * design call, but creating a PptMap from scratch is difficult
 * given the constraining creational pattern in place.
 *
 * @author Lee Lin 2/21/2002
 **/
public class XorInvariantsVisitor extends PrintDifferingInvariantsVisitor {

    PptTopLevel lastPpt = null;

    public XorInvariantsVisitor (PrintStream ps, boolean verbose,
                                 boolean printEmptyPpts,
                                 boolean printUninteresting) {
        super(ps, verbose, printEmptyPpts, printUninteresting);
    }

    public void visit (PptNode node) {
        lastPpt = node.getPpt1();
        super.visit(node);

    }

    public void visit (InvNode node) {
	Invariant inv1 = node.getInv1();
	Invariant inv2 = node.getInv2();
	// do nothing if they are unique

        /*
        if (shouldPrint (inv1,inv2) && inv2 != null) {
            if (inv1 == null) {
                Pair p = (Pair) node.getUserObject();
                p.a = inv2;
            }
            else {
                inv1.ppt.invs.add (inv2);
            }
        }

        else if (shouldPrint (inv1, inv2) && inv2 == null) {
            // do nothing, inv1 is already good
        }

        else if (!shouldPrint (inv1, inv2) && inv1 != null) {
            inv1.ppt.invs.remove (inv1);
        }

        else if (inv1 == null) {
            // do nothing
        }

        else {
            System.out.println ("CASE NOT COVERED, SHOULD NOT GET HERE: " + inv1);
        }
        */

        if (shouldPrint (inv1, inv2)) {
            // do nothing, keep both
        }

        else {
            if (inv1 != null) {
                inv1.ppt.invs.remove(inv1);
            }

            if (inv2 != null) {
                inv2.ppt.invs.remove(inv2);
            }

        }

        //	if (shouldKeep1(inv1, inv2)) {

            /*
	    // both can't be null
	    if (inv1 == null) {
		// adds inv2 to the PptSlice for inv1\
		PptSlice someSlice = inv2.ppt;
		VarInfo[] vars = someSlice.var_infos;
	    slice_loop:
		for (Iterator itor = lastPpt.views.iterator() ;
		     itor.hasNext() ; ) {
		    PptSlice view = (PptSlice) itor.next();
		    for (int i = 0; i < view.var_infos.length; i++) {
			if (i >= vars.length) continue slice_loop;
			Vector v = vars[i].equalTo();
			if (!v.contains(view.var_infos[i])) continue slice_loop;
			// if it got here, these must be the same
			// slices!
			view.addInvariant (inv2);
			System.out.println ("MATCH! " + inv2.format_java());
		    }
		}
	    }
	    */
        //	}
        /*
	else {
	    // otherwise gotta remove this "bad" one from both
	    if (inv2 != null)
		inv2.ppt.invs.remove (inv2);
	    if (inv1 != null)
		inv1.ppt.invs.remove (inv1);

	}
        */

     }


  /**
   * Returns true if the pair of invariants should be printed,
   * depending on their type, relationship, and printability.
   **/
  protected boolean shouldPrint(Invariant inv1, Invariant inv2) {
      int type = DetailedStatisticsVisitor.determineType(inv1, inv2);
      if (type == DetailedStatisticsVisitor.TYPE_NULLARY_UNINTERESTING ||
          type == DetailedStatisticsVisitor.TYPE_UNARY_UNINTERESTING) {
          return false;
      }

      int rel = DetailedStatisticsVisitor.determineRelationship(inv1, inv2);
      if (rel == DetailedStatisticsVisitor.REL_SAME_JUST1_JUST2 ||
          rel == DetailedStatisticsVisitor.REL_SAME_UNJUST1_UNJUST2 ||
          rel == DetailedStatisticsVisitor.REL_DIFF_UNJUST1_UNJUST2 ||
          rel == DetailedStatisticsVisitor.REL_MISS_UNJUST1 ||
          rel == DetailedStatisticsVisitor.REL_MISS_UNJUST2) {
          return false;
      }

      if ((inv1 == null || !inv1.isWorthPrinting()) &&
          (inv2 == null || !inv2.isWorthPrinting())) {
          return false;
      }

      return true;
  }
}
