package daikon;

// import daikon.derive.*;
import daikon.inv.*;
import daikon.inv.scalar.*;
import daikon.inv.twoScalar.*;
import daikon.inv.sequence.*;
import daikon.inv.twoSequence.*;
import daikon.inv.sequenceScalar.*;
import daikon.inv.threeScalar.*;
import java.util.*;

import utilMDE.*;

// This looks a *lot* like part of PptTopLevel.  (That is fine; its purpose
// is similar and mostly subsumed by VarValues.)

// One could imagine optimized implementations for specific numbers of
// variables, and I intend to write those; but for now, just do that.

public class PptSliceGeneric extends PptSlice {

  // This is in PptSlice; don't repeat it here!
  // Invariants invs;

  // local cache.  (temporarily public)
  // This should not be confused with the values slot of a PptTopLevel.
  // In some subclasses of PptSlice, it isn't a VarValues but can be a
  // specialized representation.
  public VarValues values_cache;

  PptSliceGeneric(Ppt parent_, VarInfo[] var_infos_) {
    // super(parent_, var_infos_);
    super(parent_, var_infos_);
    values_cache = new VarValues();
    if (Global.debugPptSliceGeneric)
      System.out.println("Created PptSliceGeneric " + this.name);
    // Ensure that the VarInfo objects are in order (and not duplicated).
    for (int i=0; i<var_infos.length-1; i++)
      Assert.assert(var_infos[i].varinfo_index < var_infos[i+1].varinfo_index);

    // if (arity == 1)
    //   var_infos[0].ppt_unary = this;

    // Make the caller do this, because
    //  1. there are few callers
    //  2. don't want to instantiate all invariants all at once
    // instantiate_invariants();
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

  void instantiate_invariants(int pass) {
    // Instantiate invariants
    if (Global.debugPptSliceGeneric)
      System.out.println("instantiate_invariants (pass " + pass + ") for " + name + ": originally " + invs.size() + " invariants in " + invs);
    if (arity == 1) {
      if (var_infos[0].type.isArray()) {
        SingleSequenceFactory.instantiate(this, pass);
      } else {
        SingleScalarFactory.instantiate(this, pass);
      }
    } else if (arity == 2) {
      boolean array1 = var_infos[0].type.isArray();
      boolean array2 = var_infos[1].type.isArray();
      if (array1 && array2) {
        TwoSequenceFactory.instantiate(this, pass);
      } else if (array1 || array2) {
        SequenceScalarFactory.instantiate(this, pass);
      } else {
        TwoScalarFactory.instantiate(this, pass);
      }
    } else if (arity == 3) {
      boolean array1 = var_infos[0].type.isArray();
      boolean array2 = var_infos[1].type.isArray();
      boolean array3 = var_infos[2].type.isArray();
      if (! (array1 || array2 || array3)) {
        ThreeScalarFactory.instantiate(this, pass);
      }
    } else {
      throw new Error("bad arity");
    }
    if (Global.debugPptSliceGeneric) {
      System.out.println("after instantiate_invariants (pass " + pass + "), PptSliceGeneric " + name + " = " + this + " has " + invs.size() + " invariants in " + invs);
    }
  }


  // A reason to defer removal is because we're currently iterating over
  // the invariants by index rather than using an Iterator.
  private Vector invs_to_remove_deferred = null;
  // This to avoid constructing a new Vector every time through add().
  // One can just use this one (and be sure to clear it out afterward).
  private Vector itrd_cache = new Vector(1);
  public void removeInvariant(Invariant inv) {
    if (Global.debugPptSliceGeneric)
      System.out.println("PptSliceGeneric.removeInvariant(" + inv.name() + ")" +
                         ((invs_to_remove_deferred != null)
                          ? " will be deferred"
                          : ""));
    Assert.assert(invs.contains(inv));
    if (invs_to_remove_deferred != null) {
      Assert.assert(! invs_to_remove_deferred.contains(inv));
      invs_to_remove_deferred.add(inv);
    } else {
      super.removeInvariant(inv);
    }
  }

  void addView(PptSlice slice) {
    throw new Error("Don't add views on a slice.");
  }

  void addViews(Vector slices) {
    throw new Error("Don't add views on a slice.");
  }

  void removeView(Ppt slice) {
    throw new Error("Don't remove view from a slice.");
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
    Assert.assert(invs.size() > 0);

    // Avoid constructing a new Vector every time through this function.
    invs_to_remove_deferred = itrd_cache;

    Object[] vals = new Object[arity];
    int[] mods = new int[arity];
    for (int i=0; i<arity; i++) {
      // The values in parens were incorrectly "i" instead of value_index.
      // How could anything have possibly worked before?
      int value_index = var_infos[i].value_index;
      vals[i] = full_vt.getValue(value_index);
      mods[i] = full_vt.getModified(value_index);
    }
    ValueTuple vt = new ValueTuple(vals, mods);
    values_cache.increment(vt, count);
    // System.out.println(name + " values_cache.increment("
    //                    + ArraysMDE.toString(vals) + ", "
    //                    + ArraysMDE.toString(mods) + ")");

    // System.out.println("PptSliceGeneric " + name + ": add " + full_vt + " = " + vt);
    // System.out.println("PptSliceGeneric " + name + " has " + invs.size() + " invariants.");

    // Supply the new values to all the invariant objects.
    // Use full_vt and the VarInfo objects,
    // or else use vt (which is pruned) and small indices (< arity).
    int num_invs = invs.size();
    if (arity == 1) {
      VarInfo vi = var_infos[0];
      if (vi.rep_type.isArray()) {
        for (int i=0; i<num_invs; i++) {
          SingleSequence inv = (SingleSequence)invs.elementAt(i);
          int mod = vi.getModified(full_vt);
          if (mod == ValueTuple.MISSING)
            continue;
          int[] value = (int[])vi.getValue(full_vt);
          inv.add(value, mod, count);
        }
      } else {
        for (int i=0; i<num_invs; i++) {
          SingleScalar inv = (SingleScalar)invs.elementAt(i);
          int mod = vi.getModified(full_vt);
          if (mod == ValueTuple.MISSING)
            continue;
          int value = vi.getIntValue(full_vt);
          inv.add(value, mod, count);
        }
      }
    } else if (arity == 2) {
      VarInfo vi1 = var_infos[0];
      VarInfo vi2 = var_infos[1];
      int num_arrays = 0;
      if (vi1.rep_type.isArray()) num_arrays++;
      if (vi2.rep_type.isArray()) num_arrays++;
      if (num_arrays == 0) {
        for (int i=0; i<num_invs; i++) {
          TwoScalar inv = (TwoScalar)invs.elementAt(i);
          int mod1 = vi1.getModified(full_vt);
          if (mod1 == ValueTuple.MISSING)
            continue;
          int mod2 = vi2.getModified(full_vt);
          if (mod2 == ValueTuple.MISSING)
            continue;
          int value1 = vi1.getIntValue(full_vt);
          int value2 = vi2.getIntValue(full_vt);
          inv.add(value1, mod1, value2, mod2, count);
        }
      } else if (num_arrays == 1) {
        if (num_invs > 0) {
          // This is going to be the same every time; no use in
          // testing each time through loop
          VarInfo seqvi;
          VarInfo sclvi;
          SequenceScalar inv1 = (SequenceScalar)invs.elementAt(0);
          // testing each time through loop
          if (inv1.seq_first) {
            seqvi = vi1;
            sclvi = vi2;
          } else {
            seqvi = vi2;
            sclvi = vi1;
          }
          for (int i=0; i<num_invs; i++) {
            SequenceScalar inv = (SequenceScalar)invs.elementAt(i);
            int mod1 = seqvi.getModified(full_vt);
            if (mod1 == ValueTuple.MISSING)
              continue;
            int mod2 = sclvi.getModified(full_vt);
            if (mod2 == ValueTuple.MISSING)
              continue;
            int[] value1 = (int[])seqvi.getValue(full_vt);
            int value2 = sclvi.getIntValue(full_vt);
            inv.add(value1, mod1, value2, mod2, count);
          }
        }
      } else if (num_arrays == 2) {
        for (int i=0; i<num_invs; i++) {
          TwoSequence inv = (TwoSequence)invs.elementAt(i);
          int mod1 = vi1.getModified(full_vt);
          if (mod1 == ValueTuple.MISSING)
            continue;
          int mod2 = vi2.getModified(full_vt);
          if (mod2 == ValueTuple.MISSING)
            continue;
          int[] value1 = (int[])vi1.getValue(full_vt);
          int[] value2 = (int[])vi2.getValue(full_vt);
          inv.add(value1, mod1, value2, mod2, count);
        }
      } else {
        throw new Error("impossible");
      }
    } else if (arity == 3) {
      VarInfo vi1 = var_infos[0];
      VarInfo vi2 = var_infos[1];
      VarInfo vi3 = var_infos[2];
      int num_arrays = 0;
      if (vi1.rep_type.isArray()) num_arrays++;
      if (vi2.rep_type.isArray()) num_arrays++;
      if (vi3.rep_type.isArray()) num_arrays++;
      if (num_arrays == 0) {
        for (int i=0; i<invs.size(); i++) {
          ThreeScalar inv = (ThreeScalar) invs.elementAt(i);
          int value1 = vi1.getIntValue(full_vt);
          int mod1 = vi1.getModified(full_vt);
          int value2 = vi2.getIntValue(full_vt);
          int mod2 = vi2.getModified(full_vt);
          int value3 = vi3.getIntValue(full_vt);
          int mod3 = vi3.getModified(full_vt);
          inv.add(value1, mod1, value2, mod2, value3, mod3, count);
        }
      } else {
        // temporarily do nothing:  efficiency hack, and there are currently
        // no ternary invariants over non-scalars
      }
    } else {
      throw new Error("bad arity " + arity);
    }

    // I need to have invs_to_remove_deferred null, or
    // else removeInvariants just adds to that list!!
    // The old value is still available in itrd_cache.
    invs_to_remove_deferred = null;
    if (itrd_cache.size() > 0) {
      removeInvariants(itrd_cache);
      itrd_cache.clear();
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
