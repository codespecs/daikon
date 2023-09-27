package daikon;

import java.io.Serializable;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.EnsuresNonNullIf;
import org.checkerframework.checker.nullness.qual.KeyFor;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;
import org.plumelib.util.CollectionsPlume;

/**
 * Maps from a program point name (a String) to a PptTopLevel.
 *
 * <p>This is the major data structure of Daikon. All the invariants can be found in it, and an
 * {@code .inv} file contains (only) the serialized form of this object.
 */
// Why doesn't this implement Map<String,PptTopLevel> or extend
// LinkedHashMap<String,PptTopLevel>?
public class PptMap implements Serializable {
  /** If you add or remove fields, change this number to the current date. */
  static final long serialVersionUID = 20040921L;

  /** The map that represents this PptMap. */
  @SuppressWarnings("serial")
  private final Map<String, PptTopLevel> nameToPpt = new LinkedHashMap<>();

  public void add(PptTopLevel ppt) {
    nameToPpt.put(ppt.name(), ppt);
  }

  public void addAll(List<PptTopLevel> ppts) {
    for (PptTopLevel ppt : ppts) {
      add(ppt);
    }
  }

  /**
   * Get the pptname named 'name' from the map. Note that conditional program points are not stored
   * in the map by name. They are only available through their parent.
   */
  @Pure
  public @Nullable PptTopLevel get(String name) {
    return nameToPpt.get(name);
  }

  /**
   * Get the pptname 'name' from the map. Note that conditional program points are not stored in the
   * map by name. They are only available through their parent.
   */
  @Pure
  public @Nullable PptTopLevel get(PptName name) {
    return get(name.toString());
  }

  /**
   * Returns whether or not 'name' is the name of a Ppt in the map. Note that conditional program
   * points are not stored in the map by name. They are only available through their parent.
   */
  @Pure
  @SuppressWarnings("nullness") // postcondition: linked maps
  @EnsuresNonNullIf(result = true, expression = "get(#1)")
  // get(#1) == nameToPpt.get(#1)
  public boolean containsName(String name) {
    return nameToPpt.containsKey(name);
  }

  /** Returns all of the program points in the map. */
  public Collection<PptTopLevel> all_ppts() {
    return nameToPpt.values();
  }

  /**
   * Returns unstably-ordered collection of PptTopLevels.
   *
   * @return unstably-ordered collection of PptTopLevels
   * @see #pptIterator()
   */
  public Collection<PptTopLevel> asCollection() {
    return Collections.unmodifiableCollection(nameToPpt.values());
  }

  /**
   * Returns an unmodifiable version of the keySet.
   *
   * @return an unmodifiable version of the keySet
   */
  public Collection<@KeyFor("nameToPpt") String> nameStringSet() {
    return Collections.unmodifiableSet(nameToPpt.keySet());
  }

  /**
   * Returns an iterator over the PptTopLevels in this, sorted by Ppt.NameComparator on their names.
   * The sorting makes the iterator deterministic.
   *
   * <p>If you wish to merely iterate over the result in a foreach loop, use {@link #pptIterable()}
   * instead.
   *
   * @return an iterator over the PptTopLevels in this, sorted by Ppt.NameComparator on their names
   * @see #pptIterable()
   */
  // See https://bugs.openjdk.java.net/browse/JDK-8195645 and
  // https://bugs.openjdk.java.net/browse/JDK-8195646
  @SuppressWarnings("lock") // JLS bug: can't write receiver annotation on method of anonymous class
  public Iterator<PptTopLevel> pptIterator() {
    TreeSet<PptTopLevel> sorted = new TreeSet<>(new Ppt.NameComparator());
    sorted.addAll(nameToPpt.values());
    // Use a (live) view iterator to get concurrent modification
    // exceptions, and an iterator over sorted to get consistency.
    final Iterator<PptTopLevel> iter_view = nameToPpt.values().iterator();
    final Iterator<PptTopLevel> iter_sort = sorted.iterator();
    return new Iterator<PptTopLevel>() {
      @Override
      public boolean hasNext(/*! >>>@GuardSatisfied Iterator<PptTopLevel> this*/ ) {
        boolean result = iter_view.hasNext();
        assert result == iter_sort.hasNext();
        return result;
      }

      @Override
      public PptTopLevel next(/*! >>>@GuardSatisfied Iterator<PptTopLevel> this*/ ) {
        iter_view.next(); // to check for concurrent modifications
        return iter_sort.next();
      }

      @Override
      public void remove(/*! >>>@GuardSatisfied Iterator<PptTopLevel> this*/ ) {
        throw new UnsupportedOperationException();
      }
    };
  }

  /**
   * Returns an iterable over the PptTopLevels in this, sorted by Ppt.NameComparator on their names.
   * The sorting makes the iterator deterministic.
   *
   * <p>It is a wrapper around {@link #pptIterator()} that can be used in a foreach loop.
   *
   * @return an iterable over the PptTopLevels in this, sorted by Ppt.NameComparator on their names
   * @see #pptIterator()
   */
  public Iterable<PptTopLevel> pptIterable() {
    return CollectionsPlume.iteratorToIterable(pptIterator());
  }

  /**
   * Returns an iterator over the PptTopLevels in this, sorted by Ppt.NameComparator on their names.
   * This differs from pptIterator() in that it includes all ppts (including conditional ppts).
   *
   * <p>If you wish to merely iterate over the result in a Java new-style for loop ("foreach loop"),
   * use {@link #ppt_all_iterable()} instead.
   *
   * @return an iterator over the PptTopLevels in this, sorted by Ppt.NameComparator on their names
   * @see #ppt_all_iterable()
   */
  // See https://bugs.openjdk.java.net/browse/JDK-8195645 and
  // https://bugs.openjdk.java.net/browse/JDK-8195646
  @SuppressWarnings("lock") // JLS bug: can't write receiver annotation on method of anonymous class
  public Iterator<PptTopLevel> ppt_all_iterator() {
    TreeSet<PptTopLevel> sorted = new TreeSet<>(new Ppt.NameComparator());
    sorted.addAll(nameToPpt.values());
    // Use a (live) view iterator to get concurrent modification
    // exceptions, and an iterator over sorted to get consistency.
    final Iterator<PptTopLevel> iter_view = nameToPpt.values().iterator();
    final Iterator<PptTopLevel> iter_sort = sorted.iterator();
    return new Iterator<PptTopLevel>() {
      @Nullable Iterator<PptConditional> cond_iterator = null;

      @Override
      public boolean hasNext(/*! >>>@GuardSatisfied Iterator<PptConditional> this*/ ) {
        if ((cond_iterator != null) && cond_iterator.hasNext()) {
          return true;
        }
        boolean result = iter_view.hasNext();
        assert result == iter_sort.hasNext();
        return result;
      }

      @Override
      public PptTopLevel next(/*! >>>@GuardSatisfied Iterator<PptTopLevel> this*/ ) {
        if ((cond_iterator != null) && cond_iterator.hasNext()) {
          return cond_iterator.next();
        }
        iter_view.next(); // to check for concurrent modifications
        PptTopLevel ppt = iter_sort.next();
        if ((ppt != null) && ppt.has_splitters()) {
          cond_iterator = ppt.cond_iterator();
        }
        return ppt;
      }

      @Override
      public void remove(/*! >>>@GuardSatisfied Iterator<PptTopLevel> this*/ ) {
        throw new UnsupportedOperationException();
      }
    };
  }

  /**
   * Returns an iterable over the PptTopLevels in this, sorted by Ppt.NameComparator on their names.
   * This differs from pptIterable() in that it includes all ppts (including conditional ppts).
   *
   * <p>It is a wrapper around {@link #ppt_all_iterator()} that can be used in a Java new-style for
   * loop ("foreach loop").
   *
   * @return an iterable over the PptTopLevels in this, sorted by Ppt.NameComparator on their names
   * @see #ppt_all_iterator()
   */
  public Iterable<PptTopLevel> ppt_all_iterable() {
    return CollectionsPlume.iteratorToIterable(ppt_all_iterator());
  }

  /** Iterate over the PptTopLevels and trim them. */
  public void trimToSize() {
    for (PptTopLevel ppt : nameToPpt.values()) {
      ppt.trimToSize();
    }
  }

  /** Check the rep invariant of this. Throws an Error if incorrect. */
  public void repCheck() {
    for (PptTopLevel ppt : this.pptIterable()) {
      ppt.repCheck();
    }
  }

  /** Return the number of active PptSlices. */
  @Pure
  public int countSlices() {
    int result = 0;
    for (PptTopLevel ppt : this.pptIterable()) {
      result += ppt.numViews();
    }
    return result;
  }

  @Pure
  public int size() {
    return nameToPpt.size();
  }

  @SideEffectFree
  @Override
  public String toString(@GuardSatisfied PptMap this) {
    return "PptMap: " + nameToPpt.toString();
  }

  /** Blow away any PptTopLevels that never saw any samples (to reclaim space). */
  public void removeUnsampled() {
    Iterator<PptTopLevel> iter = nameToPpt.values().iterator();
    while (iter.hasNext()) {
      PptTopLevel ppt = iter.next();
      if ((ppt.num_samples() == 0) && !FileIO.has_unmatched_procedure_entry(ppt)) {
        iter.remove();
      }
    }
  }
}
