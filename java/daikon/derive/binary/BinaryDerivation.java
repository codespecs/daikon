package daikon.derive.binary;

import daikon.*;
import daikon.derive.*;

import utilMDE.*;

/**
 * Abstract class to represent a derived variable that came from
 * two base variables.
 **/
public abstract class BinaryDerivation
  extends Derivation
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  /**
   * Original variable 1
   **/
  public VarInfo base1;

  /**
   * Original variable 2
   **/
  public VarInfo base2;

  /**
   * Create a new BinaryDerivation from two varinfos.
   * @param vi1, vi2 the variables this derivation will be based on.
   **/
  public BinaryDerivation(VarInfo vi1, VarInfo vi2) {
    base1 = vi1;
    base2 = vi2;
  }

  public VarInfo[] getBases() {
    return new VarInfo[] { base1, base2 };
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
        base2.aux.getFlag(VarInfoAux.IS_PARAM);
      if (isParam)
        this_var_info.aux = this_var_info.aux.setValue(VarInfoAux.IS_PARAM,
                                                       VarInfoAux.TRUE);
      // [INCR] base1.derivees.add(this);
      // [INCR] base2.derivees.add(this);
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
    return 1 + Math.max(base1.derivedDepth(), base2.derivedDepth());
  }

  /* [INCR]
  public boolean isDerivedFromNonCanonical() {
    // We insist that both are canonical, not just one.
    return !(base1.isCanonical() && base2.isCanonical());
  }
  */

}
