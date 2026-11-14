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

  /**
   * Returns the implicit comparability that the given textual representation denotes.
   *
   * @param rep a textual representation of an implicit comparability, such as "3" or "3[4][5]"
   * @param vartype the type of the variable whose comparability {@code rep} is; unused
   * @return the implicit comparability that {@code rep} denotes
   * @throws IllegalArgumentException if {@code rep} is malformed
   */
  static VarComparabilityImplicit parse(String rep, @Nullable ProglangType vartype) {
    int[] components = parseComponents(rep);
    int dims = components.length - 1;
    VarComparabilityImplicit[] index_types = new VarComparabilityImplicit[dims];
    for (int i = 0; i < dims; i++) {
      index_types[i] =
          new VarComparabilityImplicit(components[i + 1], new VarComparabilityImplicit[0], 0);
    }
    return new VarComparabilityImplicit(components[0], index_types, dims);
  }

  /**
   * Parses the textual representation of an implicit comparability, such as "3" or "3[4][5]".
   *
   * @param rep the textual representation of an implicit comparability
   * @return the base comparability followed by the comparability of each index, from first to last
   * @throws IllegalArgumentException if {@code rep} is malformed
   */
  public static int[] parseComponents(String rep) {
    List<String> dim_reps = new ArrayList<>();
    // handle array types
    while (rep.endsWith("]")) {
      int openpos = rep.lastIndexOf('[');
      if (openpos == -1) {
        throw new IllegalArgumentException("Unmatched \"]\" in comparability \"" + rep + "\"");
      }
      dim_reps.add(0, rep.substring(openpos + 1, rep.length() - 1));
      rep = rep.substring(0, openpos);
    }
    dim_reps.add(0, rep);
    int[] result = new int[dim_reps.size()];
    for (int i = 0; i < result.length; i++) {
      // Integer.parseInt throws NumberFormatException, a subclass of IllegalArgumentException.
      result[i] = Integer.parseInt(dim_reps.get(i));
    }
    return result;
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
   * If both comparabilities are implicit, records in {@code groups} the comparable sets that must
   * be merged at a parent program point. Otherwise, does nothing. See {@link
   * #mergeSets(VarComparabilityImplicit, VarComparabilityImplicit, Map, Map)}.
   *
   * @param parent the comparability of a variable at a parent program point
   * @param child the comparability of the corresponding variable at a child program point
   * @param childToParentSet maps each comparable set at the child to a comparable set at the parent
   *     whose variables correspond to child variables in that set; updated by this method
   * @param groups a union-find structure over the parent's comparable sets, as used by {@link
   *     #findGroup}; updated by this method
   */
  static void mergeSets(
      VarComparability parent,
      VarComparability child,
      Map<Integer, Integer> childToParentSet,
      Map<Integer, Integer> groups) {
    if (parent instanceof VarComparabilityImplicit && child instanceof VarComparabilityImplicit) {
      mergeSets(
          (VarComparabilityImplicit) parent,
          (VarComparabilityImplicit) child,
          childToParentSet,
          groups);
    }
  }

  /**
   * Records in {@code groups} the comparable sets that must be merged at a parent program point so
   * that the parent variable is comparable to every parent variable whose child variable is
   * comparable to {@code child}. Wherever {@code child} is comparable to everything (its set is
   * negative), the parent's comparable set is merged with the child's negative set, so that every
   * variable in it becomes comparable to everything. A negative set means the same thing at every
   * program point, so the parent variable can share an equality set with the parent variables whose
   * child variables are in that negative set.
   *
   * <p>The caller applies the merges by calling {@link #remap} after every variable has been
   * processed.
   *
   * @param parent the comparability of a variable at a parent program point
   * @param child the comparability of the corresponding variable at a child program point
   * @param childToParentSet maps each comparable set at the child to a comparable set at the parent
   *     whose variables correspond to child variables in that set; updated by this method
   * @param groups a union-find structure over the parent's comparable sets, as used by {@link
   *     #findGroup}; updated by this method
   */
  static void mergeSets(
      VarComparabilityImplicit parent,
      VarComparabilityImplicit child,
      Map<Integer, Integer> childToParentSet,
      Map<Integer, Integer> groups) {
    if (parent.dimensions != child.dimensions) {
      // The variables have different types, so their comparabilities cannot be related.
      return;
    }
    for (int i = 0; i < parent.dimensions; i++) {
      mergeSets(
          (VarComparabilityImplicit) parent.indexType(i),
          (VarComparabilityImplicit) child.indexType(i),
          childToParentSet,
          groups);
    }
    if (parent.base < 0) {
      return;
    }
    if (child.base < 0) {
      union(parent.base, child.base, groups);
    } else {
      Integer parentSet = childToParentSet.putIfAbsent(child.base, parent.base);
      if (parentSet != null) {
        union(parentSet, parent.base, groups);
      }
    }
  }

  /**
   * Merges two comparable sets in {@code groups}.
   *
   * @param set1 the integer id, such as 3, of a comparable set
   * @param set2 the integer id, such as 5, of another comparable set
   * @param groups a union-find structure over comparable sets, as used by {@link #findGroup}
   */
  private static void union(int set1, int set2, Map<Integer, Integer> groups) {
    int group1 = findGroup(set1, groups);
    int group2 = findGroup(set2, groups);
    // Use the smaller value as the representative, for deterministic results and so that a group
    // that contains a negative set, whose variables are comparable to everything, is negative.
    if (group1 < group2) {
      groups.put(group2, group1);
    } else if (group2 < group1) {
      groups.put(group1, group2);
    }
  }

  /**
   * Returns the id of the representative of the group that contains comparable set {@code base}.
   * Compresses the path from {@code base} to its representative.
   *
   * @param base the integer id, such as 3, of a comparable set
   * @param groups a union-find structure over comparable sets: maps the id of a comparable set to
   *     the id of another comparable set in the same group; a representative has no entry
   * @return the id of the representative of the group that contains comparable set {@code base}
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
   * @param groups a union-find structure over comparable sets, as built by {@link #mergeSets}
   * @return a comparability like this one, with each comparable set replaced by its representative
   */
  @SuppressWarnings("interning:not.interned") // test whether remap() created a new object
  VarComparabilityImplicit remap(Map<Integer, Integer> groups) {
    // Also remap this program point's own negative sets:  groups may have merged a child's negative
    // set, which has the same meaning at every program point, with another negative set.
    int newBase = findGroup(base, groups);
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
