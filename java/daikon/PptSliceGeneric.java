package daikon;

// import daikon.derive.*;
import daikon.inv.*;
import daikon.inv.scalar.*;
import daikon.inv.twoScalar.*;
import java.util.*;

// This looks a *lot* like part of PptTopLevel.  (That is fine; its purpose
// is similar and mostly subsumed by VarValues.)

// One could imagine optimized implementations for specific numbers of
// variables, and I intend to write those; but for now, just do that.

public class PptSliceGeneric extends PptSlice {

  // This is in Ppt; don't repeat it here!
  // Invariants invs;

  // local cache.  (temporarily public)
  public VarValues values_cache;

  PptSliceGeneric(Ppt parent_, VarInfo[] var_infos_) {
    super(parent_, var_infos_);
    values_cache = new VarValues();
    invs = new Invariants();

    if (arity == 1) {
      SingleScalarFactory.instantiate(this);
    } else if (arity == 2) {
      TwoScalarFactory.instantiate(this);
    } else if (arity == 3) {
      throw new Error("arity 3 not yet implemented");
    } else {
      throw new Error("bad arity");
    }
    // System.out.println("invs=" + invs + "; this.invs=" + this.invs);
    // System.out.println("Newly created PptSliceGeneric " + name + " = " + this + " has " + invs.size() + " invariants in " + invs);
  }

  PptSliceGeneric(Ppt parent_, VarInfo var_info_) {
    this(parent_, new VarInfo[] { var_info_ });
  }

  PptSliceGeneric(Ppt parent_, VarInfo var_info1_, VarInfo var_info2_) {
    this(parent_, new VarInfo[] { var_info1_, var_info2_ });
  }

  PptSliceGeneric(Ppt parent_, VarInfo var_info1_, VarInfo var_info2_, VarInfo var_info3_) {
    this(parent_, new VarInfo[] { var_info1_, var_info2_, var_info3_ });
  }

  /** Add a new derived Ppt. */
  void addView(Ppt slice) {
    throw new Error("Don't add views on a slice.");
  }

  // These accessors are for abstract methods declared in Ppt
  public int num_samples() { return values_cache.num_samples; }
  public int num_mod_non_missing_samples() { return values_cache.num_mod_non_missing_samples(); }
  public int num_values() { return values_cache.num_values; }
  // public int num_missing() { return values_cache.num_missing; }

  // Accessing data
  int num_vars() {
    return var_infos.length;
  }
  Iterator var_info_iterator() {
    return Arrays.asList(var_infos).iterator();
  }


  boolean compatible(Ppt other) {
    // This insists that the var_infos lists are identical.  The Ppt
    // copy constructor does reuse the var_infos field.
    return (var_infos == other.var_infos);
  }


  ///////////////////////////////////////////////////////////////////////////
  /// Manipulating values
  ///

  void add(ValueTuple full_vt, int count) {
    Object[] vals = new Object[arity];
    int[] mods = new int[arity];
    for (int i=0; i<arity; i++) {
      vals[i] = full_vt.getValue(i);
      mods[i] = full_vt.getModified(i);
    }
    ValueTuple vt = new ValueTuple(vals, mods);
    values_cache.increment(vt, count);

    // System.out.println("PptSliceGeneric " + name + ": add " + full_vt + " = " + vt);
    // System.out.println("PptSliceGeneric " + name + " has " + invs.size() + " invariants.");


    // Supply the new values to all the invariant objects.
    // Use full_vt and the VarInfo objects,
    // or else use vt (which is pruned) and small indices (< arity).
    int num_invs = invs.size();
    if (arity == 1) {
      VarInfo vi = var_infos[0];
      for (int i=0; i<num_invs; i++) {
	SingleScalar inv = (SingleScalar)invs.elementAt(i);
	int value = vi.getIntValue(full_vt);
	int mod = vi.getModified(full_vt);
	inv.add(value, mod, count);
      }
    } else if (arity == 2) {
      VarInfo vi1 = var_infos[0];
      VarInfo vi2 = var_infos[1];
      for (int i=0; i<num_invs; i++) {
	TwoScalar inv = (TwoScalar)invs.elementAt(i);
	int value1 = vi1.getIntValue(full_vt);
	int mod1 = vi1.getModified(full_vt);
	int value2 = vi2.getIntValue(full_vt);
	int mod2 = vi2.getModified(full_vt);
	inv.add(value1, mod1, value2, mod2, count);
      }
    } else if (arity == 3) {
      throw new Error("arity 3 not yet implemented");
      //       VarInfo vi1 = var_infos[0];
      //       VarInfo vi2 = var_infos[1];
      //       VarInfo vi3 = var_infos[2];
      //       for (int i=0; i<invs.size(); i++) {
      // 	ThreeScalar inv = (ThreeScalar)invs.elementAt(i);
      // 	int value1 = vi1.getIntValue(vt);
      // 	int mod1 = vi1.getModified(vt);
      // 	int value2 = vi2.getIntValue(vt);
      // 	int mod2 = vi2.getModified(vt);
      // 	int value3 = vi3.getIntValue(vt);
      // 	int mod3 = vi3.getModified(vt);
      // 	inv.add(value1, mod1, value2, mod2, value3, mod3, count);
      //       }
    } else {
      throw new Error("bad arity " + arity);
    }

  }

  // void process() {
  //   throw new Error("To implement");
  // }

  boolean contains(ValueTuple vt) {
    return values_cache.containsKey(vt);
  }

  int count(ValueTuple vt) {
    return values_cache.get(vt);
  }

  Iterator entrySet() {
    return values_cache.entrySet().iterator();
  }

}
