package daikon.derive.unary;

import daikon.*;
import daikon.derive.*;

import utilMDE.*;

public abstract class UnaryDerivation implements Derivation, Cloneable {

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

  private VarInfo this_var_info;
  public VarInfo getVarInfo() {
    if (this_var_info == null) {
      this_var_info = makeVarInfo();
      this_var_info.derived = this;
      // base.derivees.add(this); // [INCR]
    }
    return this_var_info;
  }

  public VarInfo base() {
    return base;
  }

  // This is in each class, but I can't have a private abstract method.
  protected abstract VarInfo makeVarInfo();

  /* [INCR]
  public boolean isDerivedFromNonCanonical() {
    return ! base.isCanonical();
  }
  */

  public int derivedDepth() {
    return 1 + base.derivedDepth();
  }

}
