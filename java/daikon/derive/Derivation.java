package daikon.derive;

import daikon.*;

public interface Derivation {

  // This is static, so we can't mention it here.
  // It's in DerivationFactory, though.
  // public boolean applicable();


  // I think the separate computeModified and computeValue functions aren't
  // so useful, particularly since the same computation must usually be done
  // in both functions.

  // public abstract int computeModified(ValueTuple vt);
  // public abstract int computeModified(int[] mods);

  // public abstract Object computeValue(ValueTuple vt);
  // public abstract Object computeValue(Object[] vals);

  public abstract ValueAndModified computeValueAndModified(ValueTuple vt);

  // Same implementation in all subclasses, but I can't give it here
  // in an interface.
  public abstract VarInfo getVarInfo();

  // // Returns true if the variables from which this one was derived are all
  // // non-canonical (which makes this derived variable uninteresting).  We
  // // might not have been able to know before performing the derivation that
  // // this would be the case -- for instance, when deriving before any
  // // values are seen.  [So don't do that!]
  // public abstract boolean isDerivedFromNonCanonical();

  // This depth counts this as well as the depths of its bases.
  public abstract int derivedDepth();

}
