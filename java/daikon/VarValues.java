package daikon;

import java.util.*;

import utilMDE.*;

// This is essentially just a count of how many times each ValueTuple
// appears in the data.

// Perhaps have a way for the invariants to indicate when they no longer
// care about the cached values, so caching can be turned off here.



// I can't say " implements Map" because this takes KeyTuples and ints, not
// Objects and Objects, so it doesn't obey the Map interface.  I could add
// methods that also obey the Map signatures, but I don't see the point.

public class VarValues {

  // Maybe "values" should map to a MyIntWrapper that permits the content to
  // be set; the advantage of that would be that I wouldn't have to keep
  // allocating new Integer objects all the time, particularly since I expect
  // it to be frequently updated.
  private HashMap map;		// map from ValueTuple to Integer (a count)

  // A view may not be able to supply exact values for these; I need a way
  // of indicating that.

  // num_samples and num_values can be computed from tuplemod_samples and
  // tuplemod_values, so there is no need to maintain them separately.
  // (Would they be used frequently enough to justify maintaining them?)

  public int num_samples;	// total number of samples
  public int[] tuplemod_samples;	// multiplicities of all tuples with each tuplemod
  public int num_values;	// number of distinct values.  (Differences in
				//   mod bits make tuples distinct.)
				//   This is equal to values.size() only if
				//   we haven't discarded any information
				//   from the values hashtable.
  public int[] tuplemod_values;	// count of distinct tuples with each tuplemod

  public boolean data_discarded; // indicates whether values slot contains all
				//   the data seen so far.
				//   Probably need a way to warn views this is
				//   about to get set.

  VarValues() {
    map = new HashMap();
    num_samples = 0;
    tuplemod_samples = new int[ValueTuple.TUPLEMOD_VALUES];
    num_values = 0;
    tuplemod_values = new int[ValueTuple.TUPLEMOD_VALUES];
    data_discarded = false;
  }

  public int samples_non_missing = 22222;



  public int num_samples() { return num_samples; }
  public int num_mod_non_missing_samples() {
    int[] tm_indices = ValueTuple.tuplemod_modified_not_missing;
    int result = 0;
    for (int i=0; i<tm_indices.length; i++)
      result += tuplemod_samples[tm_indices[i]];
    return result; }
  // public int num_values() { return map.size(); }
  // public int num_missing() { return num_missing; }


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


  ///
  /// HashMap style operations
  ///
  /** Removes all mappings from this map. */
  public void clear() { map.clear(); }
  /** Returns a shallow copy of this HashMap instance: the keys and values themselves are not cloned. */
  // public Object clone() { throw new Error("Unimplemented"); }
  /** Returns true if this map contains a mapping for the specified key. */
  public boolean containsKey(ValueTuple key) { return map.containsKey(key); }
  /** Returns true if this map maps one or more keys to the specified value. */
  public boolean containsValue(int value) {
    return map.containsValue(new Integer(value));
  }
  /** Returns a collection view of the mappings contained in this map. */
  public Set entrySet() { return map.entrySet(); }
  /** Returns the value to which this map maps the specified key. */
  public int get(ValueTuple vt) {
    Object raw = map.get(vt);
    if (raw == null)
      return 0;
    else
      return ((Integer)raw).intValue();
  }
  /** Returns true if this map contains no key-value mappings. */
  public boolean isEmpty() { return map.isEmpty(); }
  /** Returns a set view of the keys contained in this map. */
  public Set keySet() { return map.keySet(); }
  /** Associates the specified value with the specified key in this map. */
  public int put(ValueTuple key, int value) {
    Assert.assert(value >= 0);
    Object prev_object = map.put(key, new Integer(value));
    int tuplemod = key.tupleMod();
    int prev = (prev_object == null) ? 0 : ((Integer)prev_object).intValue();
    int samples_difference = value - prev;
    tuplemod_samples[tuplemod] += samples_difference;
    num_samples += samples_difference;
    int values_difference = MathMDE.sign(value) - MathMDE.sign(prev);
    if (values_difference != 0) {
      tuplemod_values[tuplemod] += values_difference;
      num_values += values_difference;
    }
    return prev;
  }
  /** Copies all of the mappings from the specified map to this one. */
  public void putAll(Map t) { throw new Error("Unimplemented"); }
  /** Removes the mapping for this key from this map if present. */
  int remove(ValueTuple key) {
    // Consider inlining some of this work.
    int result = put(key, 0);
    map.remove(key);
    return result;
  }
  /** Returns the number of key-value mappings in this map. */
  public int size() { return map.size(); }
  /** Returns a collection view of the values contained in this map. */
  Collection values() { return map.values(); }


  ///
  /// New operations
  ///
  public int increment(ValueTuple vt, int count) {
    // This repeates a bit of work.
    // Consider special-casing it by inlining the definition of put.
    return put(vt, get(vt) + count);
  }


  ///
  /// Map.Entry style operations.
  ///
  public static int getValue(Map.Entry entry) {
    return ((Integer)entry.getValue()).intValue();
  }
  public static int setValue(Map.Entry entry, int newValue) {
    Object prev = entry.setValue(new Integer(newValue));
    return ((Integer)prev).intValue();
  }


}
