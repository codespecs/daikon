package daikon;

import daikon.inv.*;

import utilMDE.*;

import java.util.*;

// This is a fake PptSlice for use with Implication invariants.

public class PptSlice0
  extends PptSlice
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  PptSlice0(PptTopLevel parent) {
     super(parent, new VarInfo[0]);
  }

  void init_po() {
    throw new Error("Shouldn't get called");
  }

  public void addInvariant(Invariant inv) {
    Assert.assertTrue(inv != null);
    // Assert.assertTrue(inv instanceof Implication);
    invs.add(inv);
  }

  // I need to figure out how to set these.
  public int num_samples() { return 2222; }
  public int num_mod_non_missing_samples() { return 2222; }
  public int num_values() { return 2222; }
  public String tuplemod_samples_summary() { return "tuplemod_samples_summary for PptSlice0 " + name; }

  void instantiate_invariants() {
    throw new Error("Shouldn't get called");
  }

  public List add(ValueTuple vt, int count) {
    throw new Error("Shouldn't get called");
  }

}
