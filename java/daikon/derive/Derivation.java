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

  public abstract VarInfo makeVarInfo();

  // // I guess this makes a new VarInfo which is just a slice.  (What is the
  // // point of that??)
  // public abstract VarInfo makeVarInfo(Ppt ppt_);
}
