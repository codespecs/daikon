package daikon.derive.ternary;

import daikon.*;
import daikon.derive.*;

import utilMDE.*;

/**
 * Abstract class to represent a derived variable that came from
 * three base variables.
 **/

public abstract class TernaryDerivation
  extends Derivation
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  /**
   * Original variable 1
   **/
  VarInfo base1;

  /**
   * Original variable 2
   **/
  VarInfo base2;

  /**
   * Original variable 2
   **/
  VarInfo base3;

  /**
   * Create a new TernaryDerivation from three varinfos.
   **/
  public TernaryDerivation(VarInfo vi1, VarInfo vi2, VarInfo vi3) {
    base1 = vi1;
    base2 = vi2;
    base3 = vi3;
  }

  public VarInfo[] getBases() {
    return new VarInfo[] { base1, base2, base3 };
  }

  public Derivation switchVars(VarInfo[] old_vars, VarInfo[] new_vars) {
    try {
      TernaryDerivation result = (TernaryDerivation) this.clone();
      result.base1 = new_vars[ArraysMDE.indexOf(old_vars, result.base1)];
      result.base2 = new_vars[ArraysMDE.indexOf(old_vars, result.base2)];
      result.base3 = new_vars[ArraysMDE.indexOf(old_vars, result.base3)];
      return result;
    } catch (CloneNotSupportedException e) {
      e.printStackTrace();
      throw new Error(e.toString());
    }
  }

  public abstract ValueAndModified computeValueAndModified(ValueTuple full_vt);

  /**
   * Return value for for getVarInfo().
   **/
  private VarInfo this_var_info;
  public VarInfo getVarInfo() {
    if (this_var_info == null) {
      this_var_info = this.makeVarInfo();
      this_var_info.derived = this;

      // Set whether the derivation is a param according to aux info
      boolean isParam =
        base1.aux.getFlag(VarInfoAux.IS_PARAM) ||
        base2.aux.getFlag(VarInfoAux.IS_PARAM) ||
        base3.aux.getFlag(VarInfoAux.IS_PARAM);
      if (isParam)
        this_var_info.aux = this_var_info.aux.setValue(VarInfoAux.IS_PARAM,
                                                       VarInfoAux.TRUE);
    }
    return this_var_info;
  }

  /**
   * Used by all child classes to actually create the VarInfo this
   * represents, after which it is interned for getVarInfo().
   **/
  // This is in each class, but I can't have a private abstract method.
  protected abstract VarInfo makeVarInfo();

  public int derivedDepth() {
    return 1 + Math.max(base1.derivedDepth(),
                        Math.max(base2.derivedDepth(), base3.derivedDepth()));
  }

  public boolean canBeMissing() {
    return base1.canBeMissing || base2.canBeMissing || base3.canBeMissing;
  }

  public boolean isDerivedFromNonCanonical() {
    // We insist that both are canonical, not just one.
    return !(base1.isCanonical() && base2.isCanonical()
             && base3.isCanonical());
  }

}
