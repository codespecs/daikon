package daikon;

import daikon.inv.*;

import daikon.inv.unary.scalar.*;
import daikon.inv.unary.string.*;
import daikon.inv.unary.sequence.*;
import daikon.inv.unary.stringsequence.*;

import java.util.*;

import utilMDE.*;

// *****
// Automatically generated from PptSlice.java.cpp
// *****

// This looks a *lot* like part of PptTopLevel.  (That is fine; its purpose
// is similar and mostly subsumed by VarValues.)

public final class PptSlice1  extends PptSlice {

  // This is in PptSlice; do not repeat it here!
  // Invariants invs;

  // Used to have default access; why?
  public VarInfo var_info;

  // values_cache maps (interned) values to 2-element arrays of
  // [num_unmodified, num_modified].

  int[] tm_total = new int[2 ];  // "tm" stands for "tuplemod"

  PptSlice1 (Ppt parent, VarInfo[] var_infos) {
    super(parent, var_infos);
    Assert.assert(var_infos.length == 1 );

    var_info = var_infos[0];

    values_cache = new HashMap();
    if (this.debugged || Global.debugPptSlice)
      System.out.println("Created PptSlice1 " + this.name);

    // Make the caller do this, because
    //  1. there are few callers
    //  2. do not want to instantiate all invariants all at once
    // instantiate_invariants();
  }

  PptSlice1(Ppt parent, VarInfo var_info) {
    this(parent, new VarInfo[] { var_info });
  }

  void instantiate_invariants(int pass) {
    Assert.assert(!no_invariants);

    // Instantiate invariants
    if (this.debugged || Global.debugPptSlice)
      System.out.println("instantiate_invariants (pass " + pass + ") for " + name + ": originally " + invs.size() + " invariants in " + invs);

    Vector new_invs = null;

    ProglangType rep_type = var_info.rep_type;
    if (rep_type == ProglangType.INT) {
      new_invs = SingleScalarFactory.instantiate(this, pass);
    } else if (rep_type == ProglangType.INT_ARRAY) {
      new_invs = SingleSequenceFactory.instantiate(this, pass);
    } else if (rep_type == ProglangType.DOUBLE) {
      new_invs = SingleFloatFactory.instantiate(this, pass);
    } else if (rep_type == ProglangType.DOUBLE_ARRAY) {
      new_invs = SingleFloatSequenceFactory.instantiate(this, pass);
    } else if (rep_type == ProglangType.STRING) {
      new_invs = SingleStringFactory.instantiate(this, pass);
    } else if (rep_type == ProglangType.STRING_ARRAY) {
      new_invs = SingleStringSequenceFactory.instantiate(this, pass);
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
      System.out.println("after instantiate_invariants (pass " + pass + "), PptSlice1 " + name + " = " + this + " has " + invs.size() + " invariants in " + invs);
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
    return tm_total[0] + tm_total[1];
  }
  public int num_mod_non_missing_samples() {
    return tm_total[1];
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
    return "U=" + tm_total[0]
      + ", M=" + tm_total[1];
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

    // System.out.println("PptSlice1.add(" + full_vt + ", " + count + ") for " + name);

    // Do not bother putting values into a slice if missing.

    VarInfo vi1 = var_info;

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

    Object val1 = full_vt.getValue(vi1);

    if (! already_seen_all) {

      Object vals = val1;

      int[] tm_arr = (int[]) values_cache.get(vals);
      if (tm_arr == null) {
        tm_arr = new int[2 ];
        values_cache.put(vals, tm_arr);
      }

      int mod_index = mod1;

      tm_arr[mod_index] += count;
      tm_total[mod_index] += count;
    }

    // System.out.println("PptSlice1 " + name + ": add " + full_vt + " = " + vt);
    // System.out.println("PptSlice1 " + name + " has " + invs.size() + " invariants.");

    defer_invariant_removal();

    // Supply the new values to all the invariant objects.
    int num_invs = invs.size();

    Assert.assert((mod1 == vi1.getModified(full_vt))
                  || ((vi1.getModified(full_vt) == ValueTuple.STATIC_CONSTANT)
                      && ((mod1 == ValueTuple.UNMODIFIED)
                          || (mod1 == ValueTuple.MODIFIED))));

    Assert.assert(mod1 != ValueTuple.MISSING);
    ProglangType rep = vi1.rep_type;
    if (rep == ProglangType.INT) {
      // long value = vi1.getIntValue(full_vt);
      long value = ((Long) val1).longValue();
      for (int i=0; i<num_invs; i++) {
        SingleScalar inv = (SingleScalar)invs.elementAt(i);
        inv.add(value, mod1, count);
      }
    } else if (rep == ProglangType.DOUBLE) {
      // int value = vi1.getDoubleValue(full_vt);
      double value = ((Double) val1).doubleValue();
      for (int i=0; i<num_invs; i++) {
        SingleFloat inv = (SingleFloat)invs.elementAt(i);
        inv.add(value, mod1, count);
      }
    } else if (rep == ProglangType.STRING) {
      // String value = vi1.getStringValue(full_vt);
      String value = (String) val1;
      for (int i=0; i<num_invs; i++) {
        // System.out.println("Trying " + invs.elementAt(i));
        SingleString inv = (SingleString) invs.elementAt(i);
        inv.add(value, mod1, count);
      }
    } else if (rep == ProglangType.DOUBLE_ARRAY) {
      // double[] value = vi1.getDoubleArrayValue(full_vt);
      double[] value = (double[]) val1;
      for (int i=0; i<num_invs; i++) {
        SingleFloatSequence inv = (SingleFloatSequence)invs.elementAt(i);
        inv.add(value, mod1, count);
      }
    } else if (rep == ProglangType.INT_ARRAY) {
      // long[] value = vi1.getIntArrayValue(full_vt);
      long[] value = (long[]) val1;
      for (int i=0; i<num_invs; i++) {
        SingleSequence inv = (SingleSequence)invs.elementAt(i);
        inv.add(value, mod1, count);
      }
    } else if (rep == ProglangType.STRING_ARRAY) {
      String[] value = (String[]) val1;
      for (int i=0; i<num_invs; i++) {
        SingleStringSequence inv = (SingleStringSequence)invs.elementAt(i);
        inv.add(value, mod1, count);
      }
    } else {
      throw new Error("unrecognized representation " + rep.format());
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

      ProglangType rep = var_info.rep_type;

      if (rep == ProglangType.INT) {
        SingleScalar inv = (SingleScalar) invariant;
        for (Iterator itor = values_cache.entrySet().iterator() ; itor.hasNext() ; ) {
          Map.Entry entry = (Map.Entry) itor.next();
          long val = ((Long) entry.getKey()).longValue();
          int[] tm_array = (int[]) entry.getValue();
          inv.add(val, 0, tm_array[0]);
          inv.add(val, 1, tm_array[1]);
          if (inv.no_invariant)
            break;
        }
      } else if (rep == ProglangType.DOUBLE) {
        SingleFloat inv = (SingleFloat) invariant;
        for (Iterator itor = values_cache.entrySet().iterator() ; itor.hasNext() ; ) {
          Map.Entry entry = (Map.Entry) itor.next();
          double val = ((Double) entry.getKey()).doubleValue();
          int[] tm_array = (int[]) entry.getValue();
          inv.add(val, 0, tm_array[0]);
          inv.add(val, 1, tm_array[1]);
          if (inv.no_invariant)
            break;
        }
      } else if (rep == ProglangType.STRING) {
        SingleString inv = (SingleString) invariant;
        for (Iterator itor = values_cache.entrySet().iterator() ; itor.hasNext() ; ) {
          Map.Entry entry = (Map.Entry) itor.next();
          String val = (String) entry.getKey();
          int[] tm_array = (int[]) entry.getValue();
          inv.add(val, 0, tm_array[0]);
          inv.add(val, 1, tm_array[1]);
          if (inv.no_invariant)
            break;
        }
      } else if (rep == ProglangType.INT_ARRAY) {
        SingleSequence inv = (SingleSequence) invariant;
        for (Iterator itor = values_cache.entrySet().iterator() ; itor.hasNext() ; ) {
          Map.Entry entry = (Map.Entry) itor.next();
          long[] val = (long[]) entry.getKey();
          int[] tm_array = (int[]) entry.getValue();
          inv.add(val, 0, tm_array[0]);
          inv.add(val, 1, tm_array[1]);
          if (inv.no_invariant)
            break;
        }
      } else if (rep == ProglangType.DOUBLE_ARRAY) {
        SingleFloatSequence inv = (SingleFloatSequence) invariant;
        for (Iterator itor = values_cache.entrySet().iterator() ; itor.hasNext() ; ) {
          Map.Entry entry = (Map.Entry) itor.next();
          double[] val = (double[]) entry.getKey();
          int[] tm_array = (int[]) entry.getValue();
          inv.add(val, 0, tm_array[0]);
          inv.add(val, 1, tm_array[1]);
          if (inv.no_invariant)
            break;
        }
      } else if (rep == ProglangType.STRING_ARRAY) {
        SingleStringSequence inv = (SingleStringSequence) invariant;
        for (Iterator itor = values_cache.entrySet().iterator() ; itor.hasNext() ; ) {
          Map.Entry entry = (Map.Entry) itor.next();
          String[] val = (String[]) entry.getKey();
          int[] tm_array = (int[]) entry.getValue();
          inv.add(val, 0, tm_array[0]);
          inv.add(val, 1, tm_array[1]);
          if (inv.no_invariant)
            break;
        }
      } else {
        throw new Error("unrecognized representation " + rep.format());
      }

    }
  }

}

