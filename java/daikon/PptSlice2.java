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

public class PptSlice2 extends PptSlice {

  // This is in PptSlice; don't repeat it here!
  // Invariants invs;

  // values_cache maps (interned) values to 4-element arrays of
  // [num_unmod_unmod, num_unmod_mod, num_mod_unmod, num_mod_mod].
  int[] tm_total = new int[4];


  PptSlice2(Ppt parent_, VarInfo[] var_infos_) {
    super(parent_, var_infos_);
    values_cache = new HashMap();
    if (Global.debugPptSlice)
      System.out.println("Created PptSlice2 " + this.name);

    // Perhaps add them here; then must remove if appropriate.
    //   var_infos[0].views.add(this);

    // Make the caller do this, because
    //  1. there are few callers
    //  2. don't want to instantiate all invariants all at once
    // instantiate_invariants();
  }

  PptSlice2(Ppt parent_, VarInfo var_info1_, VarInfo var_info2_) {
    this(parent_, new VarInfo[] { var_info1_, var_info2_ });
  }

  void instantiate_invariants(int pass) {
    Assert.assert(!no_invariants);

    // Instantiate invariants
    if (Global.debugPptSlice)
      System.out.println("instantiate_invariants (pass " + pass + ") for " + name + ": originally " + invs.size() + " invariants in " + invs);

    Vector new_invs = null;
    ProglangType rep1 = var_infos[0].rep_type;
    ProglangType rep2 = var_infos[1].rep_type;
    if ((rep1 == ProglangType.INT)
        && (rep2 == ProglangType.INT)) {
      new_invs = TwoScalarFactory.instantiate(this, pass);
    } else if ((rep1 == ProglangType.INT)
               && (rep2 == ProglangType.INT_ARRAY)) {
      new_invs = SequenceScalarFactory.instantiate(this, pass);
    } else if ((rep1 == ProglangType.INT_ARRAY)
               && (rep2 == ProglangType.INT)) {
      new_invs = SequenceScalarFactory.instantiate(this, pass);
    } else if ((rep1 == ProglangType.INT_ARRAY)
               && (rep2 == ProglangType.INT_ARRAY)) {
      new_invs = TwoSequenceFactory.instantiate(this, pass);
    } else {
      // Do nothing; don't even complain
    }
    if (new_invs != null) {
      for (int i=0; i<new_invs.size(); i++) {
        Invariant inv = (Invariant) new_invs.elementAt(i);
        if (inv == null)
          continue;
        addInvariant(inv);
      }
    }


    if (Global.debugPptSlice) {
      System.out.println("after instantiate_invariants (pass " + pass + "), PptSlice2 " + name + " = " + this + " has " + invs.size() + " invariants in " + invs);
    }
  }


  // These accessors are for abstract methods declared in Ppt
  public int num_samples() {
    return tm_total[0] + tm_total[1] + tm_total[2] + tm_total[3];
  }
  public int num_mod_non_missing_samples() {
    return tm_total[1] + tm_total[2] + tm_total[3];
  }

  public int num_values() {
    Assert.assert(! no_invariants);
    if (values_cache == null) {
      return num_values_post_cache;
    } else {
      return values_cache.size();
    }
  }
  public String tuplemod_samples_summary() {
    Assert.assert(! no_invariants);
    return "UU=" + tm_total[0]
      + ", UM=" + tm_total[1]
      + ", MU=" + tm_total[2]
      + ", MM=" + tm_total[3];
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

    // System.out.println("PptSlice2.add(" + full_vt + ", " + count + ")"
    //                    + " for " + name);

    // Don't bother putting values into a slice if not modified, because we
    // won't be doing anything with it!

    int value_index_1 = var_infos[0].value_index;
    int mod1 = full_vt.getModified(value_index_1);
    if (mod1 == ValueTuple.MISSING) {
      // System.out.println("Bailing out of add(" + full_vt + ") for " + name);
      return;
    }
    int value_index_2 = var_infos[1].value_index;
    int mod2 = full_vt.getModified(value_index_2);
    if (mod2 == ValueTuple.MISSING) {
      // System.out.println("Bailing out of add(" + full_vt + ") for " + name);
      return;
    }
    Object val1 = full_vt.getValue(value_index_1);
    Object val2 = full_vt.getValue(value_index_2);


    if (! already_seen_all) {
      Object[] vals = Intern.intern(new Object[] { val1, val2 });
      int[] tm_arr = (int[]) values_cache.get(vals);
      if (tm_arr == null) {
        tm_arr = new int[4];
        values_cache.put(vals, tm_arr);
      }
      int mod_index = mod1 * 2 + mod2;
      tm_arr[mod_index] += count;
      tm_total[mod_index] += count;
    }

    // System.out.println("PptSlice2 " + name + ": add " + full_vt + " = " + vt);
    // System.out.println("PptSlice2 " + name + " has " + invs.size() + " invariants.");

    defer_invariant_removal();

    // Supply the new values to all the invariant objects.
    // Use full_vt and the VarInfo objects,
    // or else use vt (which is pruned) and small indices (< arity).
    int num_invs = invs.size();

    VarInfo vi1 = var_infos[0];
    VarInfo vi2 = var_infos[1];
    Assert.assert((mod1 != ValueTuple.MISSING)
                  && (mod2 != ValueTuple.MISSING));
    int mod_index = mod1 * 2 + mod2;
    boolean array1 = vi1.rep_type.isArray();
    boolean array2 = vi2.rep_type.isArray();
    if ((!array1) && (!array2)) {
      // int value1 = vi1.getIntValue(full_vt);
      // int value2 = vi2.getIntValue(full_vt);
      int value1 = ((Integer) val1).intValue();
      int value2 = ((Integer) val2).intValue();
      for (int i=0; i<num_invs; i++) {
        TwoScalar inv = (TwoScalar)invs.elementAt(i);
        inv.add(value1, value2, mod_index, count);
      }
    } else if (array1 && (!array2)) {
      int[] seqval = (int[]) val1;
      int sclval = ((Integer) val2).intValue();
      for (int i=0; i<num_invs; i++) {
        SequenceScalar inv = (SequenceScalar)invs.elementAt(i);
        inv.add(seqval, sclval, mod_index, count);
      }
    } else if ((!array1) && (array2)) {
      int[] seqval = (int[]) val2;
      int sclval = ((Integer) val1).intValue();
      for (int i=0; i<num_invs; i++) {
        SequenceScalar inv = (SequenceScalar)invs.elementAt(i);
        inv.add(seqval, sclval, mod_index, count);
      }
    } else if (array1 && array2) {
      int[] value1 = (int[]) val1;
      int[] value2 = (int[]) val2;
      for (int i=0; i<num_invs; i++) {
        TwoSequence inv = (TwoSequence)invs.elementAt(i);
        inv.add(value1, value2, mod_index, count);
      }
    } else {
      throw new Error("impossible");
    }

    undefer_invariant_removal();
  }

  // void process() {
  //   throw new Error("To implement");
  // }

  boolean contains(ValueTuple vt) {
    return values_cache.containsKey(vt);
  }

  Iterator entrySet() {
    return values_cache.entrySet().iterator();
  }

  // Perhaps it will be more efficient to do addInvariants, one day.
  public void addInvariant(Invariant invariant) {
    Assert.assert(invariant != null);
    invs.add(invariant);

    VarInfo vi1 = var_infos[0];
    VarInfo vi2 = var_infos[1];
    boolean array1 = vi1.rep_type.isArray();
    boolean array2 = vi2.rep_type.isArray();

    if (already_seen_all) {
      if ((!array1) && (!array2)) {
        TwoScalar inv = (TwoScalar) invariant;
        // Make this invariant up to date by supplying it with all the values.
        for (Iterator itor = values_cache.entrySet().iterator() ; itor.hasNext() ; ) {
          Map.Entry entry = (Map.Entry) itor.next();
          Object[] vals = (Object[]) entry.getKey();
          int value1 = ((Integer) vals[0]).intValue();
          int value2 = ((Integer) vals[1]).intValue();
          int[] tm_array = (int[]) entry.getValue();
          for (int mi=0; mi<tm_array.length; mi++) {
            if (tm_array[mi] > 0) {
              inv.add(value1, value2, mi, tm_array[mi]);
              if (inv.no_invariant)
                break;
            }
          }
          if (inv.no_invariant)
            break;
        }
      } else if (array1 && (!array2)) {
        SequenceScalar inv = (SequenceScalar) invariant;
        // Make this invariant up to date by supplying it with all the values.
        for (Iterator itor = values_cache.entrySet().iterator() ; itor.hasNext() ; ) {
          Map.Entry entry = (Map.Entry) itor.next();
          Object[] vals = (Object[]) entry.getKey();
          int[] seqval = (int[]) vals[0];
          int sclval = ((Integer) vals[1]).intValue();
          int[] tm_array = (int[]) entry.getValue();
          for (int mi=0; mi<tm_array.length; mi++) {
            if (tm_array[mi] > 0) {
              inv.add(seqval, sclval, mi, tm_array[mi]);
              if (inv.no_invariant)
                break;
            }
          }
          if (inv.no_invariant)
            break;
        }
      } else if ((!array1) && array2) {
        SequenceScalar inv = (SequenceScalar) invariant;
        // Make this invariant up to date by supplying it with all the values.
        for (Iterator itor = values_cache.entrySet().iterator() ; itor.hasNext() ; ) {
          Map.Entry entry = (Map.Entry) itor.next();
          Object[] vals = (Object[]) entry.getKey();
          int[] seqval = (int[]) vals[1];
          int sclval = ((Integer) vals[0]).intValue();
          int[] tm_array = (int[]) entry.getValue();
          for (int mi=0; mi<tm_array.length; mi++) {
            if (tm_array[mi] > 0) {
              inv.add(seqval, sclval, mi, tm_array[mi]);
              if (inv.no_invariant)
                break;
            }
          }
          if (inv.no_invariant)
            break;
        }
      } else if (array1 && array2) {
        TwoSequence inv = (TwoSequence) invariant;
        // Make this invariant up to date by supplying it with all the values.
        for (Iterator itor = values_cache.entrySet().iterator() ; itor.hasNext() ; ) {
          Map.Entry entry = (Map.Entry) itor.next();
          Object[] vals = (Object[]) entry.getKey();
          int[] val1 = (int[]) vals[0];
          int[] val2 = (int[]) vals[1];
          int[] tm_array = (int[]) entry.getValue();
          for (int mi=0; mi<tm_array.length; mi++) {
            if (tm_array[mi] > 0) {
              inv.add(val1, val2, mi, tm_array[mi]);
              if (inv.no_invariant)
                break;
            }
          }
          if (inv.no_invariant)
            break;
        }
      }
    }
  }

}
