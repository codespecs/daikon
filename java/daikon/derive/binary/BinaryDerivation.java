package daikon.derive.binary;

import daikon.*;
import daikon.derive.*;
import plume.*;

/*>>>
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

/** Abstract class to represent a derived variable that came from two base variables. */
public abstract class BinaryDerivation extends Derivation {
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  /** Original variable 1. */
  public VarInfo base1;

  /** Original variable 2. */
  public VarInfo base2;

  /** Create a new BinaryDerivation from two varinfos. */
  public BinaryDerivation(VarInfo vi1, VarInfo vi2) {
    base1 = vi1;
    base2 = vi2;
  }

  /*@SideEffectFree*/
  public BinaryDerivation clone(/*>>>@GuardSatisfied BinaryDerivation this*/) {
    try {
      return (BinaryDerivation) super.clone();
    } catch (CloneNotSupportedException e) {
      throw new Error("This can't happen", e);
    }
  }

  /*@SideEffectFree*/
  public VarInfo[] getBases() {
    return new VarInfo[] {base1, base2};
  }

  /*@Pure*/
  public VarInfo getBase(int i) {
    switch (i) {
      case 0:
        return base1;
      case 1:
        return base2;
      default:
        throw new Error("bad base: " + i);
    }
  }

  public Derivation switchVars(VarInfo[] old_vars, VarInfo[] new_vars) {
    BinaryDerivation result = this.clone();
    result.base1 = new_vars[ArraysMDE.indexOf(old_vars, result.base1)];
    result.base2 = new_vars[ArraysMDE.indexOf(old_vars, result.base2)];
    return result;
  }

  public ValueAndModified computeValueAndModified(ValueTuple vt) {
    int source_mod1 = base1.getModified(vt);
    int source_mod2 = base2.getModified(vt);
    // MISSING_NONSENSICAL takes precedence
    if (source_mod1 == ValueTuple.MISSING_NONSENSICAL) {
      return ValueAndModified.MISSING_NONSENSICAL;
    }
    if (source_mod2 == ValueTuple.MISSING_NONSENSICAL) {
      return ValueAndModified.MISSING_NONSENSICAL;
    }
    if (source_mod1 == ValueTuple.MISSING_FLOW) {
      return ValueAndModified.MISSING_FLOW;
    }
    if (source_mod2 == ValueTuple.MISSING_FLOW) {
      return ValueAndModified.MISSING_FLOW;
    }

    return computeValueAndModifiedImpl(vt);
  }

  /** Actual implementation once mods are handled. */
  protected abstract ValueAndModified computeValueAndModifiedImpl(ValueTuple vt);

  /*@Pure*/
  protected boolean isParam() {
    return (base1.isParam() || base2.isParam());
  }

  public int derivedDepth() {
    return 1 + Math.max(base1.derivedDepth(), base2.derivedDepth());
  }

  public boolean canBeMissing() {
    return base1.canBeMissing || base2.canBeMissing;
  }

  /*@Pure*/
  public boolean isDerivedFromNonCanonical() {
    // We insist that both are canonical, not just one.
    return !(base1.isCanonical() && base2.isCanonical());
  }

  public VarInfo var1(/*>>>@GuardSatisfied BinaryDerivation this*/) {
    return base1;
  }

  public VarInfo var2(/*>>>@GuardSatisfied BinaryDerivation this*/) {
    return base2;
  }
}
