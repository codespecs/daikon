package daikon.derive.unary;

import daikon.*;
import daikon.derive.*;

import utilMDE.*;

public abstract class UnaryDerivation
  extends Derivation
  implements Cloneable
{

  public VarInfo base;

  public UnaryDerivation(VarInfo vi) { base = vi; }

  public Derivation switchVars(VarInfo[] old_vars, VarInfo[] new_vars) {
    try {
      UnaryDerivation result = (UnaryDerivation) this.clone();
      result.base = new_vars[ArraysMDE.indexOf(old_vars, result.base)];
      return result;
    } catch (CloneNotSupportedException e) {
      e.printStackTrace();
      throw new Error(e.toString());
    }
  }

  public abstract ValueAndModified computeValueAndModified(ValueTuple full_vt);

  public VarInfo base() {
    return base;
  }

  // This is in each class, but I can't have a private abstract method.
  protected abstract VarInfo makeVarInfo();

  protected VarInfo makeVarInfoWithPO() {
    VarInfo result = makeVarInfo();
    setup_po(result, false);
    return result;
  }

  private void setup_po(VarInfo derived, boolean lower) {
    // // Search the p.o. of our BASE
    // Actually don't do that; just one level up is enough
    /*
    for (Iterator i = base.closurePO(lower); i.hasNext(); ) {
      VarInfo lower_base = (VarInfo) i.next();
      VarInfo lower_vis = vi_lower.ppt.var_infos;
      for (int i = 0; i < lower_vis.length; i++) {
	VarInfo lower_elt = lower_vis[i];
	Derivation lower_der = elt.derived;
	if (der instanceof UnaryDerivation) {
	  UnaryDerivation un_lower_der = (UnaryDerivation) der;
	  if (un_lower_der.base == lower_base) {
	    if (un_lower_der.getClass().equals(getClass())) {
	      // Our base has lower_base as lower and we've found a
	      // matching class of derivation; therefore, derived has
	      // its varinfo as lower.
	      VarInfo lower = lower ? un_lower_der.getVarInfo() : derived;
	      VarInfo higher = lower ? derived : un_lower_der.getVarInfo();
	      lower.addHigherPO(higher, -22); // XXX
	    }
	  }
	}
      }
    }
    */
  }

  public VarInfo[] getBases() {
    return new VarInfo[] { base() };
  }

  /* [INCR]
  public boolean isDerivedFromNonCanonical() {
    return ! base.isCanonical();
  }
  */

  public int derivedDepth() {
    return 1 + base.derivedDepth();
  }

}
