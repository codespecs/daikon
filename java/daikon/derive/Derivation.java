package daikon.derive;

import daikon.*;

import org.apache.log4j.Category;

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
 *
 **/

public interface Derivation extends java.io.Serializable {

  public static final Category debug = Category.getInstance (Derivation.class.getName());

  // This is static, so we can't mention it here.
  // It's in DerivationFactory, though.
  // public boolean applicable();


  // This is essentially a clone() method that also switches the variables.
  public Derivation switchVars(VarInfo[] old_vars, VarInfo[] new_vars);

  /**
   * Return a pair of: the derived value and whether the variable
   * counts as modified.
   * @param vt The set of values in a program point that will be
   * used to derive the value.
   *
   **/

  // I think the separate computeModified and computeValue functions aren't
  // so useful, particularly since the same computation must usually be done
  // in both functions.

  // public abstract int computeModified(ValueTuple vt);
  // public abstract int computeModified(int[] mods);

  // public abstract Object computeValue(ValueTuple vt);
  // public abstract Object computeValue(Object[] vals);

  public abstract ValueAndModified computeValueAndModified(ValueTuple vt);

  /**
   * Get the VarInfo that this would represent.  However,
   * the VarInfo can't be used to obtain values without further
   * modification - use computeValueAndModified() for this.
   * @see Derivation#computeValueAndModified
   *
   **/

  // Same implementation in all subclasses, but I can't give it here
  // in an interface.
  public abstract VarInfo getVarInfo();

  /** 
   * For debugging only; returns true if the variables from which this
   * one was derived are all non-canonical (which makes this derived
   * variable uninteresting).  We might not have been able to know
   * before performing the derivation that this would be the case --
   * for instance, when deriving before any values are seen.  [So
   * don't do that!]  
   **/

  // public abstract boolean isDerivedFromNonCanonical(); // [INCR]

  /**
   * Returns how many levels of derivation this Derivation is based
   * on.  The depth counts this as well as the depths of its bases.
   **/

  public abstract int derivedDepth();

}



