package daikon.inv.unary.sequence;

import daikon.*;
import daikon.inv.*;
import daikon.inv.unary.sequence.*;
import daikon.inv.unary.scalar.*;
import daikon.inv.unary.*;
import daikon.inv.binary.sequenceScalar.*;
import daikon.inv.binary.twoSequence.*;
import daikon.derive.unary.*;
import utilMDE.*;

import java.util.*;

// *****
// Automatically generated from Bound-cpp.java
// *****

// One reason not to combine LowerBound and Upperbound is that they have
// separate justifications:  one may be justified when the other is not.

// What should we do if there are few values in the range?
// This can make justifying that invariant easier, because with few values
// naturally there are more instances of each value.
// This might also make justifying that invariant harder, because to get more
// than (say) twice the expected number of samples (under the assumption of
// uniform distribution) requires many samples.
// Which of these dominates?  Is the behavior what I want?

public class EltUpperBound  extends SingleSequence  {

  public UpperBoundCore  core;

  private EltUpperBound (PptSlice ppt) {
    super(ppt);
    core = new UpperBoundCore (this);
  }

  public static EltUpperBound  instantiate(PptSlice ppt) {
    return new EltUpperBound (ppt);
  }

  public String repr() {
    return "EltUpperBound"  + varNames() + ": "
      + core.repr();
  }

  public String format() {
    return var().name + " elements <= " + core.max1 ;
  }

  public String format_esc() {
    String[] esc_forall = var().esc_forall();
    return "(" + esc_forall[0]
      + "(" + esc_forall[1] + " <= " + core.max1  + "))";
  }

  public void add_modified(long[]  value, int count) {
    // System.out.println("EltUpperBound"  + varNames() + ": "
    //                    + "add(" + value + ", " + modified + ", " + count + ")");

    for (int i=0; i<value.length; i++) {
      core.add_modified(value[i], count);
      if (no_invariant)
        return;
    }

  }

  protected double computeProbability() {
    return core.computeProbability();
  }

  public boolean isExact() {
    return core.isExact();
  }

  public boolean isSameFormula(Invariant other)
  {
    return core.isSameFormula(((EltUpperBound ) other).core);
  }

  public boolean isObviousDerived() {
    VarInfo v = var();
    if (v.isDerived() && (v.derived instanceof SequenceLength)) {
      int vshift = ((SequenceLength) v.derived).shift;
      if (vshift != 0) {
        return true;

      }
    }

    // For each sequence variable, if this is an obvious member/subsequence, and
    // it has the same invariant, then this one is obvious.
    PptTopLevel pptt = (PptTopLevel) ppt.parent;
    for (int i=0; i<pptt.var_infos.length; i++) {
      VarInfo vi = pptt.var_infos[i];

      if (SubSequence.isObviousDerived(v, vi))

      {
        PptSlice1 other_slice = pptt.findSlice(vi);
        if (other_slice != null) {
          EltUpperBound  eb = EltUpperBound .find(other_slice);
          if ((eb != null)
              && eb.justified()
              && eb. core.max1  == core.max1 ) {
            return true;
          }
        }
      }
    }

    return false;
  }

  public boolean isExclusiveFormula(Invariant other) {
    if (other instanceof EltLowerBound ) {
      if (core.max1  <  ((EltLowerBound ) other). core.min1 )
        return true;
    }
    if (other instanceof OneOfScalar) {
      return other.isExclusiveFormula(this);
    }
    return false;
  }

  // Look up a previously instantiated invariant.
  public static EltUpperBound  find(PptSlice ppt) {
    Assert.assert(ppt.arity == 1);
    for (Iterator itor = ppt.invs.iterator(); itor.hasNext(); ) {
      Invariant inv = (Invariant) itor.next();
      if (inv instanceof EltUpperBound )
        return (EltUpperBound ) inv;
    }
    return null;
  }

}
