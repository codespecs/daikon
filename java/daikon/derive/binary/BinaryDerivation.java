package daikon.derive.binary;

import daikon.*;
import daikon.derive.*;

public abstract class BinaryDerivation implements Derivation {

  public BinaryDerivation(VarInfo vi1, VarInfo vi2) {
    var_info1 = vi1;
    var_info2 = vi2;
  }

  VarInfo var_info1;
  VarInfo var_info2;

  public abstract ValueAndModified computeValueAndModified(ValueTuple full_vt);

  // We don't use this function any longer.
  // This default version is likely to be overridden.
  // public abstract Object computeModified(ValueTuple full_vt) {
  //   int mod1 = var_info1.getModified(full_vt);
  //   if (mod1 == ValueTuple.MISSING)
  //     return mod1;
  //   int mod2 = var_info2.getModified(full_vt);
  //   if (mod2 == ValueTuple.MISSING)
  //     return mod2;
  //   if ((mod1 == ValueTuple.UNMODIFIED)
  //       && (mod2 == ValueTuple.UNMODIFIED))
  //     return mod1;
  //   return ValueTuple.MODIFIED;
  // }

  public abstract VarInfo makeVarInfo();

  public boolean isDerivedFromNonCanonical() {
    return !(var_info1.isCanonical() || var_info2.isCanonical());
  }

}
