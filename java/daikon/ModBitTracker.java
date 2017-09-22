package daikon;

import java.io.Serializable;
import java.util.*;
import plume.*;

/*>>>
import org.checkerframework.checker.index.qual.*;
import org.checkerframework.checker.initialization.qual.*;
import org.checkerframework.checker.nullness.qual.*;
*/

// "ModBitTracker" is a poor name for this class, since it tracks
// whether a value is missing, not whether it is modified.
/**
 * ModBitTracker maintains a BitSet for each variable at a program point. The BitSet indicates, for
 * each sample seen in order, whether that variable was present or not.
 */
public class ModBitTracker implements Serializable, Cloneable {
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20031014L;

  // Should make this a configuration option.
  private static boolean debug = false;

  /** The maximum number of BitSets; the size of modbits_arrays. */
  private /*@LengthOf("modbits_arrays")*/ int num_vars;
  /** The size of each BitSet in modbits_arrays. */
  private /*@NonNegative*/ int num_samples;

  /** The BitSets themselves. */
  // In the future, I could imagine trying to optimize this with (say)
  // run-length encoding; but it's probably not worth it.
  // All elements of modbits_arrays at or past num_sets are null.
  private /*@Nullable*/ BitSet[] modbits_arrays;

  /**
   * Conceptually, there is a BitSet per variable. In actuality, when two different variables have
   * the same modbits, they can share a single BitSet; we say the variables are in an equivalence
   * set. "index" indicates, for each variable, which BitSet it should use; it is the identifier of
   * the variable's equivalence set.
   */
  private /*@IndexFor({"this_bits", "this_bits_valid"})*/ int[] index;

  /**
   * The number of BitSets (equivalence sets) in use. All elements of modbits_arrays at or past this
   * index are null.
   */
  private /*@IndexFor("modbits_arrays")*/ int num_sets;

  // Member variables to avoid re-allocating every time "add" is entered.
  /** The bits for this ValueTuple (indexed by equivalence set. */
  private boolean[] this_bits;
  /** True if the corresponding element of this_bits has a valid value. */
  private boolean[] this_bits_valid;
  /**
   * The equivalence set for when an equivalence set is split: if a variable has a conflicting bit,
   * then it goes to the specified index instead.
   */
  private /*@IndexFor({"this_bits", "this_bits_valid"})*/ int[] this_bits_exception_index;

  public ModBitTracker(/*@NonNegative*/ int num_vars) {
    assert num_vars >= 0;
    this.num_vars = num_vars;
    modbits_arrays = new /*@Nullable*/ BitSet[num_vars];
    if (num_vars > 0) {
      modbits_arrays[0] = new BitSet();
    }
    num_samples = 0;
    index = new int[num_vars];
    num_sets = 1;
    this_bits = new boolean[num_vars];
    this_bits_valid = new boolean[num_vars];
    this_bits_exception_index = new int[num_vars];
    if (debug) checkRep();
  }

  public /*@NonNegative*/ int num_vars() {
    return num_vars;
  }

  public /*@NonNegative*/ int num_samples() {
    return num_samples;
  }

  /** Accessor for testing only. */
  public int num_sets() {
    return num_sets;
  }

  /** Check the representation invariant. */
  public void checkRep(
      /*>>>@UnknownInitialization(ModBitTracker.class) @Raw(ModBitTracker.class) ModBitTracker this*/) {
    assert index.length == num_vars;
    assert modbits_arrays.length == num_vars;
    for (int i = 0; i < num_vars; i++) {
      int this_index = index[i];
      assert this_index >= 0;
      assert this_index < num_sets;
      assert modbits_arrays[this_index] != null;
    }
    for (int i = 0; i < num_vars; i++) {
      if (i < num_sets) {
        assert modbits_arrays[i] != null;
        // Can't make this assertion, as there is no method that tells
        // the highest index that has been used in the BitSet.  (size()
        // gives physical size.)
        // assert modbits_arrays[i].size() == num_samples
        //                   : "modbits_arrays.[" + i + "].size() == "
        //                   + modbits_arrays[i].size()
        //                   + ", num_samples == " + num_samples;
      } else {
        assert modbits_arrays[i] == null;
      }
    }
  }

  /**
   * Returns a BitSet of modbit values for the given variable. The caller must not modify the
   * returned value!
   */
  @SuppressWarnings(
      "nullness") // application invariant: index[varindex] is an index for a non-null BitSet in modbits_arrays
  public BitSet get(int varindex) {
    return modbits_arrays[index[varindex]];
  }

  /** Returns the modbit for the given variable and sample number. */
  public boolean get(int varindex, /*@NonNegative*/ int sampleno) {
    return get(varindex).get(sampleno);
  }

  /** Split the specified equivalence set into two pieces. Returns the index of the copy. */
  private int split(/*@IndexFor("modbits_arrays")*/ int split_index) {
    @SuppressWarnings("nullness") // application invariant: split_index is in range
    /*@NonNull*/ BitSet bs = (BitSet) modbits_arrays[split_index].clone();
    modbits_arrays[num_sets] = bs;
    num_sets++;
    return num_sets - 1;
  }

  /** Add to this the modbits for the given ValueTuple. */
  public void add(ValueTuple vt, /*@NonNegative*/ int count) {
    if (debug) checkRep();
    assert vt.size() == num_vars : "vt.size()=" + vt.size() + ", num_vars = " + num_vars;
    if (num_vars == 0) {
      num_samples += count;
      return;
    }
    Arrays.fill(this_bits_valid, false);
    Arrays.fill(this_bits_exception_index, -1);
    for (int i = 0; i < num_vars; i++) {
      int this_index = index[i];
      // Should this use the whole modbit, not just a boolean?
      boolean modbit = !vt.isMissing(i);
      if (!this_bits_valid[this_index]) {
        // This is the first variable belonging to this equivalence set
        // that we have seen so far.
        this_bits[this_index] = modbit;
        this_bits_valid[this_index] = true;
        assert this_bits_exception_index[this_index] == -1;
      } else {
        // We have seen some other variable belonging to this equivalence set.
        if (this_bits[this_index] == modbit) {
          // This bit has the same value as we saw previously for its
          // equivalence set.
        } else {
          // This bit has a different value than we have previously seen
          // for its equivalence set.
          if (this_bits_exception_index[this_index] == -1) {
            // We have't previously seen an exception.
            this_bits_exception_index[this_index] = split(this_index);
          }
          index[i] = this_bits_exception_index[this_index];
          this_index = index[i];
          this_bits[this_index] = modbit;
          this_bits_valid[this_index] = true;
        }
      }
    }
    for (int i = 0; i < num_sets; i++) {
      @SuppressWarnings("nullness") // application invariant: non-null up to index=num_sets
      /*@NonNull*/ BitSet bs = modbits_arrays[i];
      bs.set(num_samples, num_samples + count, this_bits[i]);
    }
    num_samples += count;

    if (debug) checkRep();
  }
}
