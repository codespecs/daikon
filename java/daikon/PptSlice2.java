package daikon;

import daikon.inv.*;

import daikon.inv.binary.twoScalar.*;
import daikon.inv.binary.twoSequence.*;
import daikon.inv.binary.twoString.*;
import daikon.inv.binary.sequenceScalar.*;

import java.util.*;

import utilMDE.*;

// *****
// Automatically generated from PptSlice.java.cpp
// *****

// This looks a *lot* like part of PptTopLevel.  (That is fine; its purpose
// is similar and mostly subsumed by VarValues.)

public final class PptSlice2  extends PptSlice {

  // This is in PptSlice; do not repeat it here!
  // Invariants invs;

  // values_cache maps (interned) values to 4-element arrays of
  // [num_unmod_unmod, num_unmod_mod, num_mod_unmod, num_mod_mod].

  int[] tm_total = new int[4 ];  // "tm" stands for "tuplemod"

  PptSlice2 (Ppt parent, VarInfo[] var_infos) {
    super(parent, var_infos);
    Assert.assert(var_infos.length == 2 );

    values_cache = new HashMap();
    if (this.debugged || Global.debugPptSlice)
      System.out.println("Created PptSlice2 " + this.name);

    // Make the caller do this, because
    //  1. there are few callers
    //  2. do not want to instantiate all invariants all at once
    // instantiate_invariants();
  }

  PptSlice2(Ppt parent, VarInfo var_info1, VarInfo var_info2) {
    this(parent, new VarInfo[] { var_info1, var_info2 });
  }

  void instantiate_invariants(int pass) {
    Assert.assert(!no_invariants);

    // Instantiate invariants
    if (this.debugged || Global.debugPptSlice)
      System.out.println("instantiate_invariants (pass " + pass + ") for " + name + ": originally " + invs.size() + " invariants in " + invs);

    Vector new_invs = null;

    ProglangType rep1 = var_infos[0].rep_type;
    ProglangType rep2 = var_infos[1].rep_type;
    if ((rep1 == ProglangType.INT)
        && (rep2 == ProglangType.INT)) {
      new_invs = TwoScalarFactory.instantiate(this, pass);
    } else if ((rep1 == ProglangType.STRING)
        && (rep2 == ProglangType.STRING)) {
      new_invs = TwoStringFactory.instantiate(this, pass);
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
      // Do nothing; do not even complain
    }

    if (new_invs != null) {
      for (int i=0; i<new_invs.size(); i++) {
        Invariant inv = (Invariant) new_invs.elementAt(i);
        if (inv == null)
          continue;
        addInvariant(inv);
      }
    }

    if (this.debugged || Global.debugPptSlice) {
      System.out.println("after instantiate_invariants (pass " + pass + "), PptSlice2 " + name + " = " + this + " has " + invs.size() + " invariants in " + invs);
    }
    if (this.debugged && (invs.size() > 0)) {
      System.out.println("the invariants are:");
      for (int i=0; i<invs.size(); i++) {
        Invariant inv = (Invariant) invs.elementAt(i);
        System.out.println("  " + inv.format());
        System.out.println("    " + inv.repr());
      }
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
    Assert.assert(! already_seen_all);
    for (int i=0; i<invs.size(); i++) {
      Assert.assert(invs.elementAt(i) != null);
    }

    // System.out.println("PptSlice2.add(" + full_vt + ", " + count + ") for " + name);

    // Do not bother putting values into a slice if missing.

    VarInfo vi1 = var_infos[0];
    VarInfo vi2 = var_infos[1];

    int mod1 = full_vt.getModified(vi1);
    if (mod1 == ValueTuple.MISSING) {
      // System.out.println("Bailing out of add(" + full_vt + ") for " + name);
      return;
    }
    if (mod1 == ValueTuple.STATIC_CONSTANT) {
      Assert.assert(vi1.is_static_constant);
      mod1 = ((num_mod_non_missing_samples() == 0)
              ? ValueTuple.MODIFIED : ValueTuple.UNMODIFIED);
    }

    int mod2 = full_vt.getModified(vi2);
    if (mod2 == ValueTuple.MISSING) {
      // System.out.println("Bailing out of add(" + full_vt + ") for " + name);
      return;
    }
    if (mod2 == ValueTuple.STATIC_CONSTANT) {
      Assert.assert(vi2.is_static_constant);
      mod2 = ((num_mod_non_missing_samples() == 0)
              ? ValueTuple.MODIFIED : ValueTuple.UNMODIFIED);
    }

    Object val1 = full_vt.getValue(vi1);

    Object val2 = full_vt.getValue(vi2);

    if (! already_seen_all) {

      Object[] vals = Intern.intern(new Object[] { val1, val2 });

      int[] tm_arr = (int[]) values_cache.get(vals);
      if (tm_arr == null) {
        tm_arr = new int[4 ];
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
    int num_invs = invs.size();

    Assert.assert((mod1 == vi1.getModified(full_vt))
                  || ((vi1.getModified(full_vt) == ValueTuple.STATIC_CONSTANT)
                      && ((mod1 == ValueTuple.UNMODIFIED)
                          || (mod1 == ValueTuple.MODIFIED))));

    Assert.assert((mod1 != ValueTuple.MISSING)
                  && (mod2 != ValueTuple.MISSING));
    int mod_index = mod1 * 2 + mod2;
    boolean string1 = vi1.rep_type == ProglangType.STRING;
    boolean string2 = vi2.rep_type == ProglangType.STRING;
    boolean array1 = vi1.rep_type.isArray();
    boolean array2 = vi2.rep_type.isArray();
    if (string1 && string2) {
      String value1 = (String) val1;
      String value2 = (String) val2;
      for (int i=0; i<num_invs; i++) {
        TwoString inv = (TwoString)invs.elementAt(i);
        inv.add(value1, value2, mod_index, count);
      }
    } else if (string1 || string2) {
      throw new Error("impossible");
    } else if ((!array1) && (!array2)) {
      // long value1 = vi1.getIntValue(full_vt);
      // long value2 = vi2.getIntValue(full_vt);
      long value1 = ((Long) val1).longValue();
      long value2 = ((Long) val2).longValue();
      for (int i=0; i<num_invs; i++) {
        TwoScalar inv = (TwoScalar)invs.elementAt(i);
        inv.add(value1, value2, mod_index, count);
      }
    } else if (array1 && (!array2)) {
      long[] seqval = (long[]) val1;
      long sclval = ((Long) val2).longValue();
      for (int i=0; i<num_invs; i++) {
        SequenceScalar inv = (SequenceScalar)invs.elementAt(i);
        inv.add(seqval, sclval, mod_index, count);
      }
    } else if ((!array1) && (array2)) {
      long[] seqval = (long[]) val2;
      long sclval = ((Long) val1).longValue();
      for (int i=0; i<num_invs; i++) {
        SequenceScalar inv = (SequenceScalar)invs.elementAt(i);
        inv.add(seqval, sclval, mod_index, count);
      }
    } else if (array1 && array2) {
      long[] value1 = (long[]) val1;
      long[] value2 = (long[]) val2;
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

  // boolean contains(ValueTuple vt) {
  //   return values_cache.containsKey(vt);
  // }

  // Iterator entrySet() {
  //   return values_cache.entrySet().iterator();
  // }

  // Perhaps it will be more efficient to do addInvariants, one day.
  public void addInvariant(Invariant invariant) {
    Assert.assert(invariant != null);
    invs.add(invariant);
    Global.instantiated_invariants++;
    if (Global.debugStatistics || this.debugged)
      System.out.println("instantiated_invariant: " + invariant
                         + "; already_seen_all=" + already_seen_all);

    if (already_seen_all) {
      // Make this invariant up to date by supplying it with all the values
      // which have already been seen.
      // (Do not do
      //   Assert.assert(values_cache.entrySet().size() > 0);
      // because all the values might have been missing.  We used to ignore
      // variables that could have some missing values, but no longer.)

      VarInfo vi1 = var_infos[0];
      VarInfo vi2 = var_infos[1];
      boolean string1 = vi1.rep_type == ProglangType.STRING;
      boolean string2 = vi2.rep_type == ProglangType.STRING;
      boolean array1 = vi1.rep_type.isArray();
      boolean array2 = vi2.rep_type.isArray();

      if (string1 && string2) {
        TwoString inv = (TwoString) invariant;
        // Make this invariant up to date by supplying it with all the values.
        for (Iterator itor = values_cache.entrySet().iterator() ; itor.hasNext() ; ) {
          Map.Entry entry = (Map.Entry) itor.next();
          Object[] vals = (Object[]) entry.getKey();
          String value1 = (String) vals[0];
          String value2 = (String) vals[1];
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
      } else if (string1 || string2) {
        throw new Error("impossible");
      } else if ((!array1) && (!array2)) {
        TwoScalar inv = (TwoScalar) invariant;
        // Make this invariant up to date by supplying it with all the values.
        for (Iterator itor = values_cache.entrySet().iterator() ; itor.hasNext() ; ) {
          Map.Entry entry = (Map.Entry) itor.next();
          Object[] vals = (Object[]) entry.getKey();
          long value1 = ((Long) vals[0]).longValue();
          long value2 = ((Long) vals[1]).longValue();
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
          long[] seqval = (long[]) vals[0];
          long sclval = ((Long) vals[1]).longValue();
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
          long[] seqval = (long[]) vals[1];
          long sclval = ((Long) vals[0]).longValue();
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
          long[] val1 = (long[]) vals[0];
          long[] val2 = (long[]) vals[1];
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

