package daikon.derive;

import daikon.*;
import java.io.Serializable;
import java.util.logging.Logger;

/**
 * Structure that represents a derivation; can generate values and
 * derived variables from base variables.  A Derivation has a set of
 * base VarInfo from which the Derivation is derived.  Use
 * getVarInfo() to get the VarInfo representation of this Derivation.
 * When we want the actual value of this derivation, we pass in a
 * ValueTuple; the Derivation picks out the values of its base
 * variables and finds the value of the derived variable.  Use
 * computeValueandModified() to get value.  Derivations are created by
 * DerivationFactory.
 **/
public abstract class Derivation
  implements Serializable, Cloneable
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  /**
   * Debug tracer.
   **/
  public static final Logger debug = Logger.getLogger("daikon.derive.Derivation");

  // This is static, so we can't mention it here.
  // It's in DerivationFactory, though. // really?
  // public boolean applicable();


  // This is essentially a clone() method that also switches the variables.
  public abstract Derivation switchVars(VarInfo[] old_vars, VarInfo[] new_vars);

  /**
   * @return array of the VarInfos this was derived from
   **/
  public abstract VarInfo[] getBases();

  /**
   * @return a pair of: the derived value and whether the variable
   * counts as modified.
   * @param full_vt The set of values in a program point that will be
   * used to derive the value.
   **/
  // I don't provide separate computeModified and computeValue
  // functions: they aren't so useful, and the same computation must
  // usually be done in both functions.
  // A value whose derivation doesn't make sense is considered
  // MISSING_NONSENSICAL, not MISSING_FLOW.
  public abstract ValueAndModified computeValueAndModified(ValueTuple full_vt);

  /**
   * Get the VarInfo that this would represent.  However,
   * the VarInfo can't be used to obtain values without further
   * modification -- use computeValueAndModified() for this.
   * @see Derivation#computeValueAndModified
   **/
  public VarInfo getVarInfo() {
    if (this_var_info == null) {
      this_var_info = makeVarInfo();
      makeVarInfo_common_setup(this_var_info);
    }
    return this_var_info;
  }
  private VarInfo this_var_info;

  /**
   * Used by all child classes to actually create the VarInfo this
   * represents, after which it is interned for getVarInfo().
   **/
  // This is in each class, but I can't have a private abstract method.
  protected abstract VarInfo makeVarInfo();

  protected void makeVarInfo_common_setup(VarInfo vi) {
    // Common tasks that are abstracted into here.
    vi.derived = this;
    vi.canBeMissing = canBeMissing();
    if (isParam()) {
      this_var_info.set_is_param();
      // VIN
      // this_var_info.aux = vi.aux.setValue(VarInfoAux.IS_PARAM,
      //                                    VarInfoAux.TRUE);
    }
  }

  // Set whether the derivation is a param according to aux info
  protected abstract boolean isParam();

  public boolean missing_array_bounds = false;
  /**
   * True if we have encountered to date any missing values in this
   * derivation due to array indices being out of bounds.  This can
   * happen with both simple subscripts and subsequences.  Note that
   * this becomes true as we are running, it cannot be set in advance
   * (which would require a first pass).
   **/
  public boolean missingOutOfBounds() {
    return (missing_array_bounds);
  }

  /* *
   * For debugging only; returns true if the variables from which this
   * one was derived are all non-canonical (which makes this derived
   * variable uninteresting).  We might not have been able to know
   * before performing the derivation that this would be the case --
   * for instance, when deriving before any values are seen.
   **/
  public abstract boolean isDerivedFromNonCanonical();

  /**
   * Returns how many levels of derivation this Derivation is based
   * on.  The depth counts this as well as the depths of its bases.
   **/
  public abstract int derivedDepth();

  /**
   * @return true iff other and this represent the same derivation
   * (modulo the variable they are applied to).  Default implentation
   * will just checks runtime type, but subclasses with state
   * (e.g. SequenceInitial index) should match that, too.
   **/
  public abstract boolean isSameFormula(Derivation other);

  public abstract boolean canBeMissing();
}
