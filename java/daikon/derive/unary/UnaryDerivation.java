package daikon.derive.unary;

import daikon.*;
import daikon.derive.*;

public abstract class UnaryDerivation implements Derivation {

  public UnaryDerivation(VarInfo vi) { var_info = vi; }

  public VarInfo var_info;

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
