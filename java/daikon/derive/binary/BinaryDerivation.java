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
