package daikon.derive.unary;

import daikon.*;
import daikon.derive.*;

import utilMDE.*;

public abstract class UnaryDerivation implements Derivation, Cloneable {

  public VarInfo var_info;

  public UnaryDerivation(VarInfo vi) { var_info = vi; }

  public Derivation switchVars(VarInfo[] old_vars, VarInfo[] new_vars) {
    try {
      UnaryDerivation result = (UnaryDerivation) this.clone();
      result.var_info = new_vars[ArraysMDE.indexOf(old_vars, result.var_info)];
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
      var_info.derivees.add(this);
    }
    return this_var_info;
  }

  // This is in each class, but I can't have a private abstract method.
  abstract protected VarInfo makeVarInfo();

  // public boolean isDerivedFromNonCanonical() {
  //   return ! var_info.isCanonical();
  // }

  public int derivedDepth() {
    return 1 + var_info.derivedDepth();
  }

}
