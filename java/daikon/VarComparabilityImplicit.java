package daikon;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.EnsuresNonNullIf;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;

/**
 * A VarComparabilityImplicit is an arbitrary integer, and comparisons succeed exactly if the two
 * integers are equal, except that negative integers compare equal to everything. Alternately, for
 * an array variable, a VarComparabilityImplicit may separately indicate comparabilities for the
 * elements and indices.
 *
 * <pre>
 * VarComparabilityImplicit ::= int
 *                            | VarComparabilityImplicit "[" int "]"
 * </pre>
 *
 * <p>This is called "implicit" because the comparability objects do not refer to one another or
 * refer directly to variables; whether two variables are comparable depends on their comparability
 * objects. Implicit comparability has the flavor of types in programming languages.
 *
 * <p>Soon, this will probably be modified to permit the group identifiers to be arbitrary strings
 * (not containing square brackets) instead of arbitrary integers.
 */
public final class VarComparabilityImplicit extends VarComparability implements Serializable {
  static final long serialVersionUID = 20020122L;

  /** The number that indicates which comparable set the VarInfo belongs to. */
  int base;

  /** indexTypes[0] is comparability of the first index of this array. */
  // null only for the "unknown" type??
  VarComparabilityImplicit @Nullable [] indexTypes;

  /** Indicates how many of the indices are in use; there may be more indices than this. */
  int dimensions;

  private @MonotonicNonNull VarComparabilityImplicit cached_element_type;

  public static final VarComparabilityImplicit unknown = new VarComparabilityImplicit(-3, null, 0);

  private VarComparabilityImplicit(
      int base, VarComparabilityImplicit @Nullable [] indexTypes, int dimensions) {
    this.base = base;
    this.indexTypes = indexTypes;
    this.dimensions = dimensions;
  }

  @Pure
  @Override
  public int hashCode(@GuardSatisfied VarComparabilityImplicit this) {
    if (base < 0) {
      // This is equals() to everything
      return -1;
    }
    if (dimensions > 0) {
      return (indexType(dimensions - 1).hashCode() << 4) ^ elementType().hashCode();
    }
    return base;
  }

  @EnsuresNonNullIf(result = true, expression = "#1")
  @Pure
  @Override
  public boolean equals(
      @GuardSatisfied VarComparabilityImplicit this, @GuardSatisfied @Nullable Object o) {
    if (!(o instanceof VarComparabilityImplicit)) {
      return false;
    }
    return equalsVarComparabilityImplicit((VarComparabilityImplicit) o);
  }

  @EnsuresNonNullIf(result = true, expression = "#1")
  @Pure
  public boolean equalsVarComparabilityImplicit(
      @GuardSatisfied VarComparabilityImplicit this, @GuardSatisfied VarComparabilityImplicit o) {
    return equality_set_ok(o);
  }

  public boolean baseAlwayscomparable() {
    return (base < 0);
  }

  @Pure
  @Override
  public boolean alwaysComparable(@GuardSatisfied VarComparabilityImplicit this) {
    return (dimensions == 0) && (base < 0);
  }

  static VarComparabilityImplicit parse(String rep, @Nullable ProglangType vartype) {
    // String rep_ = rep;          // for debugging

    List<String> dim_reps = new ArrayList<>();
    // handle array types
    while (rep.endsWith("]")) {
      int openpos = rep.lastIndexOf('[');
      dim_reps.add(0, rep.substring(openpos + 1, rep.length() - 1));
      rep = rep.substring(0, openpos);
    }
    int dims = dim_reps.size();
    VarComparabilityImplicit[] index_types = new VarComparabilityImplicit[dims];
    for (int i = 0; i < dims; i++) {
      index_types[i] = parse(dim_reps.get(i), null);
    }
    try {
      int base = Integer.parseInt(rep);
      return new VarComparabilityImplicit(base, index_types, dims);
    } catch (NumberFormatException e) {
      throw new IllegalArgumentException(e);
    }
  }

  @Override
  public VarComparability makeAlias() {
    return this;
  }

  @Override
  public VarComparability elementType(@GuardSatisfied VarComparabilityImplicit this) {
    if (cached_element_type == null) {
      // When Ajax is modified to output non-atomic info for arrays, this
      // check will no longer be necessary.
      if (dimensions > 0) {
        cached_element_type = new VarComparabilityImplicit(base, indexTypes, dimensions - 1);
      } else {
        // COMPARABILITY TEST
        // System.out.println("Warning: taking element type of non-array comparability.");
        cached_element_type = unknown;
      }
    }
    return cached_element_type;
  }

  /**
   * Determines the comparability of the length of this string. Currently always returns unknown,
   * but it would be best if string lengths were only comparable with other string lengths (or
   * perhaps nothing).
   */
  @Override
  public VarComparability string_length_type() {
    return unknown;
  }

  @Pure
  @Override
  public VarComparability indexType(@GuardSatisfied VarComparabilityImplicit this, int dim) {
    // When Ajax is modified to output non-atomic info for arrays, this
    // check will no longer be necessary.
    if (dim < dimensions) {
      assert indexTypes != null : "@AssumeAssertion(nullness): dependent: not the unknown type";
      return indexTypes[dim];
    } else {
      return unknown;
    }
  }

  @SuppressWarnings("all:purity") // Override the purity checker
  @Pure
  static boolean comparable(
      @GuardSatisfied VarComparabilityImplicit type1,
      @GuardSatisfied VarComparabilityImplicit type2) {
    if (type1.alwaysComparable()) {
      return true;
    }
    if (type2.alwaysComparable()) {
      return true;
    }
    if ((type1.dimensions > 0) && (type2.dimensions > 0)) {
      // Both are arrays
      return (comparable(
              type1.indexType(type1.dimensions - 1), type2.indexType(type2.dimensions - 1))
          && comparable(type1.elementType(), type2.elementType()));
    } else if ((type1.dimensions == 0) && (type2.dimensions == 0)) {
      // Neither is an array.
      return type1.base == type2.base;
    } else {
      // One array, one non-array, and the non-array isn't universally comparable.
      assert type1.dimensions == 0 || type2.dimensions == 0;
      return false;
    }
  }

  /**
   * Records in {@code groups} that the two comparabilities must be comparable, by merging their
   * comparable sets. Merging is needed only for sets that are not already comparable to everything.
   *
   * @param type1 a comparability
   * @param type2 a comparability
   * @param groups a union-find structure over comparable sets, as used by {@link #findGroup}
   */
  static void unify(
      VarComparabilityImplicit type1,
      VarComparabilityImplicit type2,
      Map<Integer, Integer> groups) {
    if ((type1.dimensions > 0) && (type2.dimensions > 0)) {
      unify(
          (VarComparabilityImplicit) type1.indexType(type1.dimensions - 1),
          (VarComparabilityImplicit) type2.indexType(type2.dimensions - 1),
          groups);
      unify(
          (VarComparabilityImplicit) type1.elementType(),
          (VarComparabilityImplicit) type2.elementType(),
          groups);
    } else if ((type1.dimensions == 0) && (type2.dimensions == 0)) {
      if (type1.base >= 0 && type2.base >= 0) {
        int group1 = findGroup(type1.base, groups);
        int group2 = findGroup(type2.base, groups);
        // Use the smaller value as the representative, for deterministic results.
        if (group1 < group2) {
          groups.put(group2, group1);
        } else if (group2 < group1) {
          groups.put(group1, group2);
        }
      }
    }
    // Otherwise, one is an array and the other is not.  Such variables are never equal, so there
    // is nothing to do.
  }

  /**
   * Returns the representative of the comparable set that contains {@code base}. Compresses the
   * path from {@code base} to its representative.
   *
   * @param base a comparable set
   * @param groups a union-find structure over comparable sets: maps a comparable set to another
   *     comparable set in the same group; a representative has no entry
   * @return the representative of the comparable set that contains {@code base}
   */
  private static int findGroup(int base, Map<Integer, Integer> groups) {
    int root = base;
    Integer next;
    while ((next = groups.get(root)) != null) {
      root = next;
    }
    while (base != root) {
      next = groups.put(base, root);
      assert next != null : "@AssumeAssertion(nullness): base is not a representative";
      base = next;
    }
    return root;
  }

  /**
   * Returns a comparability like this one, but with each comparable set replaced by its
   * representative in {@code groups}.
   *
   * @param groups a union-find structure over comparable sets, as built by {@link #unify}
   * @return a comparability like this one, with each comparable set replaced by its representative
   */
  VarComparabilityImplicit remap(Map<Integer, Integer> groups) {
    int newBase = (base < 0) ? base : findGroup(base, groups);
    VarComparabilityImplicit @Nullable [] oldIndexTypes = indexTypes;
    VarComparabilityImplicit @Nullable [] newIndexTypes = oldIndexTypes;
    if (oldIndexTypes != null) {
      for (int i = 0; i < oldIndexTypes.length; i++) {
        VarComparabilityImplicit newIndexType = oldIndexTypes[i].remap(groups);
        if (newIndexType != oldIndexTypes[i]) {
          if (newIndexTypes == oldIndexTypes) {
            newIndexTypes = oldIndexTypes.clone();
          }
          assert newIndexTypes != null : "@AssumeAssertion(nullness): copy of oldIndexTypes";
          newIndexTypes[i] = newIndexType;
        }
      }
    }
    if (newBase == base && newIndexTypes == indexTypes) {
      return this;
    }
    return new VarComparabilityImplicit(newBase, newIndexTypes, dimensions);
  }

  /**
   * Returns the comparability of a variable that is always equal to two variables with the given
   * comparabilities, which have been made comparable by {@link #unify} and {@link #remap}. Wherever
   * either is comparable to everything, so is the result.
   *
   * @param type1 a comparability
   * @param type2 a comparability that is comparable to {@code type1}
   * @return a comparability that is comparable to everything that either argument is comparable to
   */
  static VarComparabilityImplicit join(
      VarComparabilityImplicit type1, VarComparabilityImplicit type2) {
    if (type1.dimensions != type2.dimensions) {
      // Such variables are never equal.
      return type1;
    }
    VarComparabilityImplicit @Nullable [] newIndexTypes = type1.indexTypes;
    for (int i = 0; i < type1.dimensions; i++) {
      VarComparabilityImplicit indexType1 = (VarComparabilityImplicit) type1.indexType(i);
      VarComparabilityImplicit newIndexType =
          join(indexType1, (VarComparabilityImplicit) type2.indexType(i));
      if (newIndexType != indexType1) {
        assert newIndexTypes != null : "@AssumeAssertion(nullness): dependent: dimensions > 0";
        if (newIndexTypes == type1.indexTypes) {
          newIndexTypes = newIndexTypes.clone();
        }
        newIndexTypes[i] = newIndexType;
      }
    }
    // A negative base, which is comparable to everything, takes precedence.  If both are
    // negative, the choice is arbitrary.  If both are non-negative, they are equal.
    int newBase = Math.min(type1.base, type2.base);
    if (newBase == type1.base && newIndexTypes == type1.indexTypes) {
      return type1;
    }
    return new VarComparabilityImplicit(newBase, newIndexTypes, type1.dimensions);
  }

  /**
   * Same as comparable, except that variables that are comparable to everything (negative
   * comparability value) can't be included in the same equality set as those with positive values.
   */
  @Override
  public boolean equality_set_ok(
      @GuardSatisfied VarComparabilityImplicit this, @GuardSatisfied VarComparability other) {

    VarComparabilityImplicit type1 = this;
    VarComparabilityImplicit type2 = (VarComparabilityImplicit) other;

    if ((type1.dimensions > 0) && (type2.dimensions > 0)) {
      return (type1
              .indexType(type1.dimensions - 1)
              .equality_set_ok(type2.indexType(type2.dimensions - 1))
          && type1.elementType().equality_set_ok(type2.elementType()));
    }

    if ((type1.dimensions == 0) && (type2.dimensions == 0)) {
      return type1.base == type2.base;
    }

    // One array, one non-array
    assert type1.dimensions == 0 || type2.dimensions == 0;
    return false;
  }

  // for debugging
  @SideEffectFree
  @Override
  public String toString(@GuardSatisfied VarComparabilityImplicit this) {
    String result = Integer.toString(base);
    for (int i = 0; i < dimensions; i++) {
      result += "[" + indexType(i) + "]";
    }
    return result;
  }
}
