package daikon;

// import daikon.derive.*;
import daikon.inv.*;
import daikon.inv.scalar.*;
import daikon.inv.twoScalar.*;
import daikon.inv.string.*;
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
  VarValues values_cache;
  // These are used only when the values_cache has been set to null.
  int num_samples_post_cache = -2222;
  int num_mod_non_missing_samples_post_cache = -2222;
  int num_values_post_cache = -2222;
  String tuplemod_samples_summary_post_cache = "UNINITIALIZED";

  // true if we've seen all values and should not add the result of further
  // add() methods to values_cache.
  // This is rather a hack and should be removed later.
  public boolean already_seen_all = false;

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
    Assert.assert(!no_invariants);

    // Instantiate invariants
    if (Global.debugPptSliceGeneric)
      System.out.println("instantiate_invariants (pass " + pass + ") for " + name + ": originally " + invs.size() + " invariants in " + invs);
    if (arity == 1) {
      ProglangType rep_type = var_infos[0].rep_type;
      if (rep_type.equals(ProglangType.INT)) {
        SingleScalarFactory.instantiate(this, pass);
      } else if (rep_type.equals(ProglangType.INT_ARRAY)) {
        SingleSequenceFactory.instantiate(this, pass);
      } else if (rep_type.equals(ProglangType.STRING)) {
        SingleStringFactory.instantiate(this, pass);
      } else if (rep_type.equals(ProglangType.STRING_ARRAY)) {
        // SingleStringSequenceFactory.instantiate(this, pass);
      } else {
        // Do nothing; don't even complain
      }
    } else if (arity == 2) {
      ProglangType rep1 = var_infos[0].rep_type;
      ProglangType rep2 = var_infos[1].rep_type;
      if (rep1.equals(ProglangType.INT)
          && rep2.equals(ProglangType.INT)) {
        TwoScalarFactory.instantiate(this, pass);
      } else if (rep1.equals(ProglangType.INT)
          && rep2.equals(ProglangType.INT_ARRAY)) {
        SequenceScalarFactory.instantiate(this, pass);
      } else if (rep1.equals(ProglangType.INT_ARRAY)
          && rep2.equals(ProglangType.INT)) {
        SequenceScalarFactory.instantiate(this, pass);
      } else if (rep1.equals(ProglangType.INT_ARRAY)
          && rep2.equals(ProglangType.INT_ARRAY)) {
        TwoSequenceFactory.instantiate(this, pass);
      } else {
        // Do nothing; don't even complain
      }
    } else if (arity == 3) {
      ProglangType rep1 = var_infos[0].rep_type;
      ProglangType rep2 = var_infos[1].rep_type;
      ProglangType rep3 = var_infos[2].rep_type;
      if (rep1.equals(ProglangType.INT)
          && rep2.equals(ProglangType.INT)
          && rep3.equals(ProglangType.INT)) {
        ThreeScalarFactory.instantiate(this, pass);
      } else {
        // Do nothing; don't even complain
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
    Assert.assert(! no_invariants);
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
  public int num_samples() {
    Assert.assert(! no_invariants);
    if (values_cache == null) {
      return num_samples_post_cache;
    } else {
      return values_cache.num_samples;
    }
  }
  public int num_mod_non_missing_samples() {
    Assert.assert(! no_invariants);
    if (values_cache == null) {
      return num_mod_non_missing_samples_post_cache;
    } else {
      int result = values_cache.num_mod_non_missing_samples();
      // mod bits get wonky for conditional program points
      // (in particular, all the modified versions might be
      // filtered out).
      if (parent instanceof PptConditional) {
        return Math.max(result, num_values());
      }
      return result;
    }
  }
  // WARNING!  This is the number of distinct ValueTuple objects,
  // which can be as much as 2^arity times as many as the number of
  // distinct tuples of values.
  public int num_values() {
    Assert.assert(! no_invariants);
    if (values_cache == null) {
      return num_values_post_cache;
    } else {
      return values_cache.num_values;
    }
  }
  public String tuplemod_samples_summary() {
    Assert.assert(! no_invariants);
    if (values_cache == null) {
      return tuplemod_samples_summary_post_cache;
    } else {
      return values_cache.tuplemod_samples_summary();
    }
  }

  boolean check_modbits () {
    Assert.assert(! no_invariants);
    // The value "0" can be had for missing samples.
    if ((num_mod_non_missing_samples() << arity) < num_values()) {
      if (values_cache == null) {
        System.out.println("Values cache has been cleared");
      } else {
        values_cache.dump();
      }
      throw new Error("Bad mod bits in dtrace file:\n"
                      + "num_mod_non_missing_samples()=" + num_mod_non_missing_samples()
                      + ", num_samples()=" + num_samples()
                      + ", num_values()=" + num_values() + "\n"
                      + "for " + name + "\n"
                      + tuplemod_samples_summary() + "\n"
                      + "Consider running modbit-munge.pl");
    }
    return true;
  }

  public void clear_cache() {
    // Don't do check_modbits()!  We might have only partially filled up
    // the cache.  Do this at call sites where appropriate.
    // Assert.assert(check_modbits());

    // Commented out for debugging[
    // if (values_cache != null) {
    //   num_samples_post_cache = num_samples();
    //   num_mod_non_missing_samples_post_cache = num_mod_non_missing_samples();
    //   num_values_post_cache = num_values();
    //   tuplemod_samples_summary_post_cache = tuplemod_samples_summary();
    //   values_cache = null;
    // }
  }

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
    Assert.assert(! no_invariants);
    Assert.assert(invs.size() > 0);

    // System.out.println("PptSliceGeneric.add(" + full_vt + ", " + count + ")"
    //                    + " for " + name);

    // Don't bother putting values into a slice if not modified, because we
    // won't be doing anything with it!

    Object[] vals = new Object[arity];
    int[] mods = new int[arity];
    for (int i=0; i<arity; i++) {
      // The values in parens were incorrectly "i" instead of value_index.
      // How could anything have possibly worked before?
      int value_index = var_infos[i].value_index;
      vals[i] = full_vt.getValue(value_index);
      mods[i] = full_vt.getModified(value_index);
      if (mods[i] == ValueTuple.MISSING) {
        // System.out.println("Bailing out of add(" + full_vt + ") for " + name);
        return;
      }
    }
    // I won't reuse the array below because I would have to do casting anyway.
    if (! already_seen_all) {
      ValueTuple vt = new ValueTuple(vals, mods);
      values_cache.increment(vt, count);
      // System.out.println(name + " values_cache.increment("
      //                    + ArraysMDE.toString(vals) + ", "
      //                    + ArraysMDE.toString(mods) + ")");
    }

    // System.out.println("PptSliceGeneric " + name + ": add " + full_vt + " = " + vt);
    // System.out.println("PptSliceGeneric " + name + " has " + invs.size() + " invariants.");

    // Avoid constructing a new Vector every time through this function.
    invs_to_remove_deferred = itrd_cache;
    Assert.assert(invs_to_remove_deferred.size() == 0);

    // Supply the new values to all the invariant objects.
    // Use full_vt and the VarInfo objects,
    // or else use vt (which is pruned) and small indices (< arity).
    int num_invs = invs.size();
    if (arity == 1) {
      VarInfo vi = var_infos[0];
      // int mod = vi.getModified(full_vt);
      int mod = mods[0];
      Assert.assert(mod == vi.getModified(full_vt));
      Assert.assert(mod != ValueTuple.MISSING);
      ProglangType rep = vi.rep_type;
      if (rep.equals(ProglangType.INT)) {
        // int value = vi.getIntValue(full_vt);
        int value = ((Integer) vals[0]).intValue();
        for (int i=0; i<num_invs; i++) {
          SingleScalar inv = (SingleScalar)invs.elementAt(i);
          inv.add(value, mod, count);
        }
      } else if (rep.equals(ProglangType.STRING)) {
        // String value = vi.getStringValue(full_vt);
        String value = (String) vals[0];
        for (int i=0; i<num_invs; i++) {
          // System.out.println("Trying " + invs.elementAt(i));
          SingleString inv = (SingleString) invs.elementAt(i);
          inv.add(value, mod, count);
        }
      } else if (rep.equals(ProglangType.INT_ARRAY)) {
        // int[] value = vi.getIntArrayValue(full_vt);
        int[] value = (int[]) vals[0];
        for (int i=0; i<num_invs; i++) {
          SingleSequence inv = (SingleSequence)invs.elementAt(i);
          inv.add(value, mod, count);
        }
      }
    } else if (arity == 2) {
      VarInfo vi1 = var_infos[0];
      VarInfo vi2 = var_infos[1];
      // int mod1 = vi1.getModified(full_vt);
      // int mod2 = vi2.getModified(full_vt);
      int mod1 = mods[0];
      int mod2 = mods[1];
      Assert.assert((mod1 != ValueTuple.MISSING)
                    && (mod2 != ValueTuple.MISSING));
      int num_arrays = 0;
      if (vi1.rep_type.isArray()) num_arrays++;
      if (vi2.rep_type.isArray()) num_arrays++;
      if (num_arrays == 0) {
        // int value1 = vi1.getIntValue(full_vt);
        // int value2 = vi2.getIntValue(full_vt);
        int value1 = ((Integer) vals[0]).intValue();
        int value2 = ((Integer) vals[1]).intValue();
        for (int i=0; i<num_invs; i++) {
          TwoScalar inv = (TwoScalar)invs.elementAt(i);
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
          int[] value1 = (int[])seqvi.getValue(full_vt);
          int value2 = sclvi.getIntValue(full_vt);
          for (int i=0; i<num_invs; i++) {
            SequenceScalar inv = (SequenceScalar)invs.elementAt(i);
            // Can this reordering be right?  SequenceScalar
            // is supposed to take care of it itself.
            inv.add(value1, mod1, value2, mod2, count);
          }
        }
      } else if (num_arrays == 2) {
        // int[] value1 = vi1.getIntArrayValue(full_vt);
        // int[] value2 = vi2.getIntArrayValue(full_vt);
        int[] value1 = (int[]) vals[0];
        int[] value2 = (int[]) vals[1];
        for (int i=0; i<num_invs; i++) {
          TwoSequence inv = (TwoSequence)invs.elementAt(i);
          inv.add(value1, mod1, value2, mod2, count);
        }
      } else {
        throw new Error("impossible");
      }
    } else if (arity == 3) {
      VarInfo vi1 = var_infos[0];
      VarInfo vi2 = var_infos[1];
      VarInfo vi3 = var_infos[2];
      // int mod1 = vi1.getModified(full_vt);
      // int mod2 = vi2.getModified(full_vt);
      // int mod3 = vi3.getModified(full_vt);
      int mod1 = mods[0];
      int mod2 = mods[1];
      int mod3 = mods[2];
      Assert.assert((mod1 != ValueTuple.MISSING)
                    && (mod2 != ValueTuple.MISSING)
                    && (mod3 != ValueTuple.MISSING));
      int num_arrays = 0;
      if (vi1.rep_type.isArray()) num_arrays++;
      if (vi2.rep_type.isArray()) num_arrays++;
      if (vi3.rep_type.isArray()) num_arrays++;
      if (num_arrays == 0) {
        // int value1 = vi1.getIntValue(full_vt);
        // int value2 = vi2.getIntValue(full_vt);
        // int value3 = vi3.getIntValue(full_vt);
        int value1 = ((Integer) vals[0]).intValue();
        int value2 = ((Integer) vals[1]).intValue();
        int value3 = ((Integer) vals[2]).intValue();
        for (int i=0; i<invs.size(); i++) {
          ThreeScalar inv = (ThreeScalar) invs.elementAt(i);
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
