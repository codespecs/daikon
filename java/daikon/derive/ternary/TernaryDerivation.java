package daikon.derive.ternary;

import daikon.*;
import daikon.derive.*;
import plume.*;

/*>>>
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

/** Abstract class to represent a derived variable that came from three base variables. */
public abstract class TernaryDerivation extends Derivation {
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  /** Original variable 1. */
  VarInfo base1;

  /** Original variable 2. */
  VarInfo base2;

  /** Original variable 3. */
  VarInfo base3;

  /** Create a new TernaryDerivation from three varinfos. */
  public TernaryDerivation(VarInfo vi1, VarInfo vi2, VarInfo vi3) {
    base1 = vi1;
    base2 = vi2;
    base3 = vi3;
  }

  /*@SideEffectFree*/
  @Override
  public TernaryDerivation clone(/*>>>@GuardSatisfied TernaryDerivation this*/) {
    try {
      return (TernaryDerivation) super.clone();
    } catch (CloneNotSupportedException e) {
      throw new Error("This can't happen", e);
    }
  }

  /*@SideEffectFree*/
  @Override
  public VarInfo[] getBases() {
    return new VarInfo[] {base1, base2, base3};
  }

  /*@Pure*/
  @Override
  public VarInfo getBase(int i) {
    switch (i) {
      case 0:
        return base1;
      case 1:
        return base2;
      case 2:
        return base3;
      default:
        throw new Error("bad base: " + i);
    }
  }

  @Override
  public Derivation switchVars(VarInfo[] old_vars, VarInfo[] new_vars) {
    TernaryDerivation result = this.clone();
    result.base1 = new_vars[ArraysMDE.indexOf(old_vars, result.base1)];
    result.base2 = new_vars[ArraysMDE.indexOf(old_vars, result.base2)];
    result.base3 = new_vars[ArraysMDE.indexOf(old_vars, result.base3)];
    return result;
  }

  @Override
  public abstract ValueAndModified computeValueAndModified(ValueTuple full_vt);

  /*@Pure*/
  @Override
  protected boolean isParam() {
    return (base1.isParam() || base2.isParam() || base3.isParam());
  }

  @Override
  public int derivedDepth() {
    return 1 + Math.max(base1.derivedDepth(), Math.max(base2.derivedDepth(), base3.derivedDepth()));
  }

  @Override
  public boolean canBeMissing() {
    return base1.canBeMissing || base2.canBeMissing || base3.canBeMissing;
  }

  /*@Pure*/
  @Override
  public boolean isDerivedFromNonCanonical() {
    // We insist that both are canonical, not just one.
    return !(base1.isCanonical() && base2.isCanonical() && base3.isCanonical());
  }
}
