package daikon.derive.unary;

import daikon.*;
import daikon.derive.*;

public abstract class UnaryDerivation implements Derivation {

  public UnaryDerivation(VarInfo vi) { var_info = vi; }

  VarInfo var_info;

  public abstract ValueAndModified computeValueAndModified(ValueTuple full_vt);

  public abstract VarInfo makeVarInfo();

  public boolean isDerivedFromNonCanonical() {
    return ! var_info.isCanonical();
  }
}
