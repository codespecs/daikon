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

public class PptSlice1 extends PptSlice {

  // This is in PptSlice; don't repeat it here!
  // Invariants invs;

  VarInfo var_info;

  // values_cache maps (interned) values to 2-element arrays of
  // [num_unmodified, num_modified].
  int[] tm_total = new int[2];


  PptSlice1(Ppt parent_, VarInfo[] var_infos_) {
    super(parent_, var_infos_);
    Assert.assert(var_infos.length == 1);
    var_info = var_infos[0];
    values_cache = new HashMap();
    if (Global.debugPptSlice)
      System.out.println("Created PptSlice1 " + this.name);

    // Reinstate this??  I'll need to remove it as appropriate, too...
    // if (arity == 1)
    //   var_infos[0].ppt_unary = this;

    // Make the caller do this, because
    //  1. there are few callers
    //  2. don't want to instantiate all invariants all at once
    // instantiate_invariants();
  }

  PptSlice1(Ppt parent_, VarInfo var_info_) {
    this(parent_, new VarInfo[] { var_info_ });
  }

  void instantiate_invariants(int pass) {
    Assert.assert(!no_invariants);

    // Instantiate invariants
    if (Global.debugPptSlice)
      System.out.println("instantiate_invariants (pass " + pass + ") for " + name + ": originally " + invs.size() + " invariants in " + invs);

    ProglangType rep_type = var_info.rep_type;
    Vector new_invs = null;
    if (rep_type == ProglangType.INT) {
      new_invs = SingleScalarFactory.instantiate(this, pass);
    } else if (rep_type == ProglangType.INT_ARRAY) {
      new_invs = SingleSequenceFactory.instantiate(this, pass);
    } else if (rep_type == ProglangType.STRING) {
      new_invs = SingleStringFactory.instantiate(this, pass);
    } else if (rep_type == ProglangType.STRING_ARRAY) {
      // new_invs = SingleStringSequenceFactory.instantiate(this, pass);
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
      System.out.println("after instantiate_invariants (pass " + pass + "), PptSlice1 " + name + " = " + this + " has " + invs.size() + " invariants in " + invs);
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

    // System.out.println("PptSlice1.add(" + full_vt + ", " + count + ")" + " for " + name);

    // Don't bother putting values into a slice if not modified, because we
    // won't be doing anything with it!

    int mod = full_vt.getModified(var_info.value_index);
    if (mod == ValueTuple.MISSING) {
      // System.out.println("Bailing out of add(" + full_vt + ") for " + name);
      return;
    }
    Object val = full_vt.getValue(var_info.value_index);

    if (! already_seen_all) {
      int[] tm_arr = (int[]) values_cache.get(val);
      if (tm_arr == null) {
        tm_arr = new int[2];
        values_cache.put(val, tm_arr);
      }
      int mod_index = mod;
      tm_arr[mod] += count;
      tm_total[mod] += count;
    }

    // System.out.println("PptSlice1 " + name + ": add " + full_vt + " = " + vt);
    // System.out.println("PptSlice1 " + name + " has " + invs.size() + " invariants.");

    defer_invariant_removal();

    // Supply the new values to all the invariant objects.
    // Use full_vt and the VarInfo objects,
    // or else use vt (which is pruned) and small indices (< arity).
    int num_invs = invs.size();

    VarInfo vi = var_info;
    Assert.assert(mod == vi.getModified(full_vt));
    Assert.assert(mod != ValueTuple.MISSING);
    ProglangType rep = vi.rep_type;
    if (rep == ProglangType.INT) {
      // int value = vi.getIntValue(full_vt);
      int value = ((Integer) val).intValue();
      for (int i=0; i<num_invs; i++) {
        SingleScalar inv = (SingleScalar)invs.elementAt(i);
        inv.add(value, mod, count);
      }
    } else if (rep == ProglangType.STRING) {
      // String value = vi.getStringValue(full_vt);
      String value = (String) val;
      for (int i=0; i<num_invs; i++) {
        // System.out.println("Trying " + invs.elementAt(i));
        SingleString inv = (SingleString) invs.elementAt(i);
        inv.add(value, mod, count);
      }
    } else if (rep == ProglangType.INT_ARRAY) {
      // int[] value = vi.getIntArrayValue(full_vt);
      int[] value = (int[]) val;
      for (int i=0; i<num_invs; i++) {
        SingleSequence inv = (SingleSequence)invs.elementAt(i);
        inv.add(value, mod, count);
      }
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

    ProglangType rep = var_info.rep_type;

    if (already_seen_all) {
      if (rep == ProglangType.INT) {
        SingleScalar inv = (SingleScalar) invariant;
        // Make this invariant up to date by supplying it with all the values.
        for (Iterator itor = values_cache.entrySet().iterator() ; itor.hasNext() ; ) {
          Map.Entry entry = (Map.Entry) itor.next();
          int val = ((Integer) entry.getKey()).intValue();
          int[] tm_array = (int[]) entry.getValue();
          inv.add(val, 0, tm_array[0]);
          inv.add(val, 1, tm_array[1]);
          if (inv.no_invariant)
            break;
        }
      } else if (rep == ProglangType.STRING) {
        SingleString inv = (SingleString) invariant;
        // Make this invariant up to date by supplying it with all the values.
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
        // Make this invariant up to date by supplying it with all the values.
        for (Iterator itor = values_cache.entrySet().iterator() ; itor.hasNext() ; ) {
          Map.Entry entry = (Map.Entry) itor.next();
          int[] val = (int[]) entry.getKey();
          int[] tm_array = (int[]) entry.getValue();
          inv.add(val, 0, tm_array[0]);
          inv.add(val, 1, tm_array[1]);
          if (inv.no_invariant)
            break;
        }
      }
    }
  }

}
