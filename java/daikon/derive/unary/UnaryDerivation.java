package daikon.derive.unary;

import daikon.*;
import daikon.derive.*;

import utilMDE.*;

public abstract class UnaryDerivation
  extends Derivation
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
