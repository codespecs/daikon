package daikon.diff;

import daikon.inv.Invariant;
import java.io.*;
import daikon.*;
import java.util.*;

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
        if (shouldPrint(inv1, inv2)) {
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


        }
        else {
            // otherwise gotta remove this "bad" one from both
            if (inv2 != null)
                inv2.ppt.invs.remove (inv2);
            if (inv1 != null)
                inv1.ppt.invs.remove (inv1);
            /*
            if (inv2 != null)
                System.out.println ("CHUCKED " + inv2.format_java());
            else System.out.println ("CHUCKED " + inv1.format_java());
            */
        }
    }
}
