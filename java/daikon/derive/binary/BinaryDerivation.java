package daikon.derive.binary;

import daikon.*;
import daikon.derive.*;

import utilMDE.*;

public abstract class BinaryDerivation implements Derivation, Cloneable {

  VarInfo base1;
  VarInfo base2;

  public BinaryDerivation(VarInfo vi1, VarInfo vi2) {
    base1 = vi1;
    base2 = vi2;
  }

  public Derivation switchVars(VarInfo[] old_vars, VarInfo[] new_vars) {
    try {
      BinaryDerivation result = (BinaryDerivation) this.clone();
      result.base1 = new_vars[ArraysMDE.indexOf(old_vars, result.base1)];
      result.base2 = new_vars[ArraysMDE.indexOf(old_vars, result.base2)];
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
      this_var_info = this.makeVarInfo();
      this_var_info.derived = this;
      base1.derivees.add(this);
      base2.derivees.add(this);
    }
    return this_var_info;
  }

  // This is in each class, but I can't have a private abstract method.
  protected abstract VarInfo makeVarInfo();

  public int derivedDepth() {
    return 1 + Math.max(base1.derivedDepth(), base2.derivedDepth());
  }

  public boolean isDerivedFromNonCanonical() {
    // We insist that both are canonical, not just one.
    return !(base1.isCanonical() && base2.isCanonical());
  }

  public static String addSubscript(String base, String subscript) {
    String suffix = "";
    if (base.endsWith("[]")) {
      base = base.substring(0, base.length()-2);
    } else if (base.startsWith("orig(") && base.endsWith("[])")) {
      // This is heuristic; I think it's probably OK.
      base = base.substring(0, base.length()-3);
      suffix = ")";
    } else if (base.startsWith("\\old(") && base.endsWith(")")) {
      // This is even more heuristic; I think it's probably also OK.
      base = base.substring(0, base.length()-1);
      suffix = ")";
      // Even more heuristic; starting to get scary.
      int subold = subscript.indexOf("\\old(");
      while (subold != -1) {
        int oldcloseparen = subscript.indexOf(")", subold);
        Assert.assert(oldcloseparen != -1);
        subscript = (subscript.substring(0, subold)
                     + subscript.substring(subold+5, oldcloseparen)
                     + subscript.substring(oldcloseparen+1));
        subold = subscript.indexOf("\\old(");
      }
    }
    return base + "[" + subscript + "]" + suffix;
  }

}
