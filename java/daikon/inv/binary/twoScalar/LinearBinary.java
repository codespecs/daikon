package daikon.inv.binary.twoScalar;

import daikon.*;
import daikon.inv.Invariant;
import daikon.derive.unary.SequenceLength;
import java.util.*;
import utilMDE.*;

public class LinearBinary extends TwoScalar {

  public LinearBinaryCore core;

  protected LinearBinary(PptSlice ppt) {
    super(ppt);
    core = new LinearBinaryCore(this);
  }

  public static LinearBinary instantiate(PptSlice ppt) {
    if (ppt.debugged) {
      System.out.println("LinearBinary.instantiate(" + ppt.name + ")");
    }
    return new LinearBinary(ppt);
  }

  public String repr() {
    return "LinearBinary" + varNames() + ": "
      + "no_invariant=" + no_invariant
      + "; " + core.repr();
  }

  public String format() {
    return core.format(var1().name.name(), var2().name.name());
  }

  public String format_esc() {
    return core.format(var1().name.esc_name(), var2().name.esc_name());
  }
  
  /* IOA */
  public String format_ioa(String classname) {
    return core.format_ioa(var1().name.ioa_name(classname), var2().name.ioa_name(classname));
  }

  public String format_reversed() {
    return core.format_reversed(var1().name.name(), var2().name.name());
  }

  public String format_esc_reversed() {
    return core.format_reversed(var1().name.esc_name(), var2().name.esc_name());
  }

  public String format_simplify() {
    return core.format_simplify(var1().name, var2().name);
  }

  public void add_modified(long x, long y, int count) {
    core.add_modified(x, y, count);
  }

  public boolean enoughSamples() {
    return core.enoughSamples();
  }

  protected double computeProbability() {
    return core.computeProbability();
  }

  public boolean isExact() {
    return true;
  }

  public boolean isObviousDerived() {
    VarInfo var1 = ppt.var_infos[0];
    VarInfo var2 = ppt.var_infos[1];
    // avoid comparing "size(a)" to "size(a)-1"; yields "size(a)-1 = size(a) - 1"
    if (var1.isDerived() && (var1.derived instanceof SequenceLength)
        && var2.isDerived() && (var2.derived instanceof SequenceLength)) {
      SequenceLength sl1 = (SequenceLength) var1.derived;
      SequenceLength sl2 = (SequenceLength) var2.derived;
      if (sl1.base == sl2.base) {
        return true;
      }
    }
    // avoid comparing "size(a)-1" to anything; should compare "size(a)" instead
    if (var1.isDerived() && (var1.derived instanceof SequenceLength)
        && ((SequenceLength) var1.derived).shift != 0) {
      return true;
    }
    if (var2.isDerived() && (var2.derived instanceof SequenceLength)
        && ((SequenceLength) var2.derived).shift != 0) {
      return true;
    }

    return false;
  }


  public boolean isSameFormula(Invariant other)
  {
    return core.isSameFormula(((LinearBinary) other).core);
  }

  public boolean isExclusiveFormula(Invariant other)
  {
    if (other instanceof LinearBinary) {
      return core.isExclusiveFormula(((LinearBinary) other).core);
    }
    return false;
  }


  // Look up a previously instantiated invariant.
  public static LinearBinary find(PptSlice ppt) {
    Assert.assert(ppt.arity == 2);
    for (Iterator itor = ppt.invs.iterator(); itor.hasNext(); ) {
      Invariant inv = (Invariant) itor.next();
      if (inv instanceof LinearBinary)
        return (LinearBinary) inv;
    }
    return null;
  }

  // Returns a vector of LinearBinary objects.
  // This ought to produce an iterator instead.
  public static Vector findAll(VarInfo vi) {
    Vector result = new Vector();
    for (Iterator itor = vi.ppt.views_iterator() ; itor.hasNext() ; ) {
      PptSlice view = (PptSlice) itor.next();
      if ((view.arity == 2) && view.usesVar(vi)) {
        LinearBinary lb = LinearBinary.find(view);
        if (lb != null) {
          result.add(lb);
        }
      }
    }
    return result;
  }

}
