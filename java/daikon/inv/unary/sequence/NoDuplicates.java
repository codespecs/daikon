package daikon.inv.unary.sequence;

import daikon.*;
import daikon.inv.*;
import daikon.inv.binary.twoSequence.*;

import utilMDE.*;

import java.util.*;


public class NoDuplicates extends SingleSequence {

  final static boolean debugNoDuplicates = false;

  int elts = 0;

  protected NoDuplicates(PptSlice ppt) {
    super(ppt);
  }

  public static NoDuplicates instantiate(PptSlice ppt) {
    return new NoDuplicates(ppt);
  }

  public String repr() {
    return "NoDuplicates" + varNames() + ": "
      + "elts=\"" + elts;
  }

  public String format() {
    if (debugNoDuplicates) {
      System.out.println(repr());
    }
    return (var().name + " contains no duplicates");
  }

  public String format_esc() {
    return "format_esc " + this.getClass() + " needs to be changed: " + format();
  }


  public void add_modified(long[] a, int count) {
    for (int i=1; i<a.length; i++) {
      if (ArraysMDE.indexOf(a, a[i]) < i) {
        destroy();
        return;
      }
    }
    if (a.length > 1)
      elts += 1;
  }

  protected double computeProbability() {
    if (no_invariant) {
      return Invariant.PROBABILITY_NEVER;
    } else {
      return Math.pow(.9, elts);
    }
  }

  // Lifted from EltNonZero; should abstract some of this out.
  public boolean isObviousImplied() {
    // For every other NoDuplicates at this program point, see if there is a
    // subsequence relationship between that array and this one.

    PptTopLevel parent = (PptTopLevel)ppt.parent;
    for (Iterator itor = parent.invariants_iterator(); itor.hasNext(); ) {
      Invariant inv = (Invariant) itor.next();
      if ((inv instanceof NoDuplicates) && (inv != this) && inv.justified()) {
        VarInfo v1 = var();
        VarInfo v2 = inv.ppt.var_infos[0];
        if (SubSequence.isObviousDerived(v1, v2)) {
          // System.out.println("obvious: " + format() + "   because of " + inv.format());
          return true;
        }

        boolean this_var_first = (v1.varinfo_index < v2.varinfo_index);
        if (! this_var_first) { VarInfo temp = v1; v1 = v2; v2 = temp; }
        Assert.assert(v1.varinfo_index < v2.varinfo_index);
        PptSlice2 slice_2seq = parent.findSlice(v1, v2);
        if (slice_2seq == null) {
          // System.out.println("NoDuplicates.isObviousImplied: no slice for " + v1.name + ", " + v2.name);
        } else  {
          // slice_2seq != null
          SubSequence ss = SubSequence.find(slice_2seq);
          if (ss == null) {
            // System.out.println("NoDuplicates.isObviousImplied: no SubSequence for " + v1.name + ", " + v2.name);
          } else {
            // System.out.println("NoDuplicates.isObviousImplied: found SubSequence: " + ss.repr());
            if (this_var_first
                ? ss.var1_in_var2
                : ss.var2_in_var1) {
              return true;
            }
          }
        }
      }
    }

    return false;
  }

  public boolean isSameFormula(Invariant other)
  {
    Assert.assert(other instanceof NoDuplicates);
    return true;
  }
}
