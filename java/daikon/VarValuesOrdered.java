package daikon;

import daikon.derive.*;

import java.util.*;

import utilMDE.*;

// This is essentially just a count of how many times each ValueTuple
// appears in the data.

// Perhaps have a way for the invariants to indicate when they no longer
// care about the cached values, so caching can be turned off here.



// I can't say " implements Map" because this takes ValueTuples and ints, not
// Objects and Objects, so it doesn't obey the Map interface.  I could add
// methods that also obey the Map signatures, but I don't see the point.

public final class VarValuesOrdered {

  // More efficient to have three vectors than a vector of triplets.
  private Vector values;
  private Vector modbits;
  private Vector counts;        // for run-length encoding
  private int num_samples;      // total number of samples; sum of count Vector.
                                //   also computable from tuplemod_*.

  private HashSet values_set;   // for count of values.

                                // Could map to 8-element array of
                                // tupleMod() counts.  (We only care about
                                // that for debugging output.  Should I bother?)

  // num_samples and num_values can be computed from tuplemod_samples and
  // tuplemod_values, so there is no need to maintain them separately.
  // (Would they be used frequently enough to justify maintaining them?)

  private int[] tuplemod_samples;       // multiplicities of all tuples with each tuplemod


  VarValuesOrdered() {
    values = new Vector();
    modbits = new Vector();
    counts = new Vector();
    num_samples = 0;
    values_set = new HashSet();
    tuplemod_samples = new int[ValueTuple.TUPLEMOD_VALUES];
  }

  public int num_samples() { return num_samples; }
  public int num_values() { return values_set.size(); }
  public int num_mod_non_missing_samples() {
    int[] tm_indices = ValueTuple.tuplemod_modified_not_missing;
    int result = 0;
    for (int i=0; i<tm_indices.length; i++)
      result += tuplemod_samples[tm_indices[i]];
    return result;
  }

  public String tuplemod_samples_summary() {
    StringBuffer sb = new StringBuffer();
    for (int tm=0; tm<ValueTuple.TUPLEMOD_VALUES; tm++) {
      if (tm != 0)
        sb.append("; ");
      sb.append(ValueTuple.tuplemodToStringBrief(tm));
      sb.append("=");
      sb.append(tuplemod_samples[tm]);
    }
    return sb.toString();
  }


  Set valueSet() {
    return values_set;
  }

  // Add new samples
  void add(ValueTuple vt, int count) {
    Assert.assertTrue(Intern.isInterned(vt.vals));
    Assert.assertTrue(Intern.isInterned(vt.mods));
    add(vt.vals, vt.mods, count);
  }

  void add(Object[] vals, int[] mods, int count) {
    Assert.assertTrue(Intern.isInterned(vals));
    Assert.assertTrue(Intern.isInterned(mods));
    // System.out.println("VarValuesOrdered.add(" + vals + ", " + mods + ", " + count + ")");
    int last_index = values.size()-1;
    if ((last_index > 0)
        && (vals == values.elementAt(last_index))
        && (mods == modbits.elementAt(last_index))) {
      int new_count = count + ((Integer) counts.elementAt(last_index)).intValue();
      counts.set(last_index, new Integer(new_count));
    } else {
      values.add(vals);
      modbits.add(mods);
      counts.add(new Integer(count));
    }

    int tuplemod = ValueTuple.tupleMod(mods);
    tuplemod_samples[tuplemod] += count;
    num_samples += count;

    Assert.assertTrue(Intern.isInterned(vals));
    values_set.add(vals);
  }


  // Add new variables.
  void extend(Derivation[] derivs) {
    // The VarInfos have already been extended; but we don't care about
    // that here.
    // I could do something cleverer about remembering old computed
    // values; but skip that for now.
    int old_num_vars = ((Object[]) values.elementAt(0)).length;
    int new_num_vars = old_num_vars + derivs.length;
    for (int i=0; i<values.size(); i++) {
      Object[] vals = (Object[]) values.elementAt(i);
      int[] mods = (int[]) modbits.elementAt(i);
      Assert.assertTrue(vals.length == old_num_vars);
      Assert.assertTrue(mods.length == old_num_vars);
      ValueTuple vt = ValueTuple.makeFromInterned(vals, mods);
      vt.extend(derivs);
      values.set(i, vt.vals);
      modbits.set(i, vt.mods);
    }
  }

  /** A pair of a ValueTuple and an Integer count. **/
  public final class ValueTupleCount {
    public final ValueTuple value_tuple;
    public final int count;
    ValueTupleCount(ValueTuple vt, int c) {
      value_tuple = vt;
      count = c;
    }
  }

  // The iterator returns ValueTupleCount objects
  private final class SampleIterator implements Iterator {
    int i;
    SampleIterator() { i=0; }
    public boolean hasNext() { return i<values.size(); }
    public Object next() {
      Object[] vals = (Object[]) values.elementAt(i);
      int[] mods = (int[]) modbits.elementAt(i);
      int count = ((Integer) counts.elementAt(i)).intValue();
      i++;
      return new ValueTupleCount(ValueTuple.makeFromInterned(vals, mods),
                                 count);
    }
    public void remove() { throw new UnsupportedOperationException(); }
  }

  Iterator sampleIterator() {
    return new SampleIterator();
  }



  // ///
  // /// HashMap style operations
  // ///
  // /** Removes all mappings from this map. */
  // public void clear() { map.clear(); }
  // /** Returns a shallow copy of this HashMap instance: the keys and values themselves are not cloned. */
  // // public Object clone() { throw new Error("Unimplemented"); }
  // /** Returns true if this map contains a mapping for the specified key. */
  // public boolean containsKey(ValueTuple key) { return map.containsKey(key); }
  // /** Returns true if this map maps one or more keys to the specified value. */
  // public boolean containsValue(int value) {
  //   return map.containsValue(new Integer(value));
  // }
  // /** Returns a collection view of the mappings contained in this map. */
  // public Set entrySet() { return map.entrySet(); }
  // /** Returns the value to which this map maps the specified key. */
  // public int get(ValueTuple vt) {
  //   Object raw = map.get(vt);
  //   if (raw == null)
  //     return 0;
  //   else
  //     return ((Integer)raw).intValue();
  // }
  // /** Returns true if this map contains no key-value mappings. */
  // public boolean isEmpty() { return map.isEmpty(); }
  // /** Returns a set view of the keys contained in this map. */
  // public Set keySet() { return map.keySet(); }
  // /** Associates the specified value with the specified key in this map. */
  // public int put(ValueTuple key, int value) {
  //   Assert.assertTrue(value >= 0);
  //   Object prev_object = map.put(key, new Integer(value));
  //   int tuplemod = key.tupleMod();
  //   int prev = (prev_object == null) ? 0 : ((Integer)prev_object).intValue();
  //   int samples_difference = value - prev;
  //   tuplemod_samples[tuplemod] += samples_difference;
  //   num_samples += samples_difference;
  //   int values_difference = MathMDE.sign(value) - MathMDE.sign(prev);
  //   if (values_difference != 0) {
  //     tuplemod_values[tuplemod] += values_difference;
  //     num_values += values_difference;
  //   }
  //   return prev;
  // }
  // /** Copies all of the mappings from the specified map to this one. */
  // public void putAll(Map t) { throw new Error("Unimplemented"); }
  // /** Removes the mapping for this key from this map if present. */
  // int remove(ValueTuple key) {
  //   // Consider inlining some of this work.
  //   int result = put(key, 0);
  //   map.remove(key);
  //   return result;
  // }
  // /** Returns the number of key-value mappings in this map. */
  // public int size() { return map.size(); }
  // /** Returns a collection view of the values contained in this map. */
  // Collection values() { return map.values(); }


  // ///
  // /// New operations
  // ///
  // public int increment(ValueTuple vt, int count) {
  //   // This repeates a bit of work.
  //   // Consider special-casing it by inlining the definition of put.
  //   return put(vt, get(vt) + count);
  // }


  // ///
  // /// Map.Entry style operations.
  // ///
  // public static int getValue(Map.Entry entry) {
  //   return ((Integer)entry.getValue()).intValue();
  // }
  // public static int setValue(Map.Entry entry, int newValue) {
  //   Object prev = entry.setValue(new Integer(newValue));
  //   return ((Integer)prev).intValue();
  // }

  // ///
  // /// Debugging
  // ///

  public void dump() {
    int tuple_len = -1;
    Assert.assertTrue(values.size() == modbits.size());
    Assert.assertTrue(values.size() == counts.size());
    for (int i=0; i<values.size(); i++) {
      Object[] vals = (Object[]) values.elementAt(i);
      int[] mods = (int[]) modbits.elementAt(i);
      int count = ((Integer) counts.elementAt(i)).intValue();
      if (i==0) { tuple_len = vals.length; }
      Assert.assertTrue(vals.length == tuple_len);
      Assert.assertTrue(mods.length == tuple_len);
      System.out.println(ValueTuple.valsToString(vals) + " "
                         + ArraysMDE.toString(mods) + ": " + count);
      Assert.assertTrue(Intern.isInterned(vals));
      Assert.assertTrue(Intern.isInterned(mods));
    }
  }

}
