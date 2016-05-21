package daikon;

import java.io.*;
import java.util.*;
import plume.*;

/*>>>
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

/**
 * Maps from a program point name (a String) to a PptTopLevel.<p>
 *
 * This is the major data structure of Daikon.  All the invariants can be
 * found in it, and an .inv file contains (only) the serialized form of
 * this object.
 */
// Why doesn't this implement Map<String,PptTopLevel> or extend
// LinkedHashMap<String,PptTopLevel>?
public class PptMap implements Serializable {
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20040921L;

  private final Map<String, PptTopLevel> nameToPpt = new LinkedHashMap<String, PptTopLevel>();

  public void add(PptTopLevel ppt) {
    nameToPpt.put(ppt.name(), ppt);
  }

  public void addAll(List<PptTopLevel> ppts) {
    for (PptTopLevel ppt : ppts) {
      add(ppt);
    }
  }

  /**
   * Get the pptname named 'name' from the map.  Note that conditional
   * program points are not stored in the map by name.  They are only
   * available through their parent.
   */
  /*@Pure*/
  public /*@Nullable*/ PptTopLevel get(String name) {
    return nameToPpt.get(name);
  }

  /**
   * Get the pptname 'name' from the map.  Note that conditional
   * program points are not stored in the map by name.  They are only
   * available through their parent.
   */
  /*@Pure*/
  public /*@Nullable*/ PptTopLevel get(PptName name) {
    return get(name.toString());
  }

  /**
   * Returns whether or not 'name' is the name of a Ppt in the map.  Note
   * that conditional program points are not stored in the map by name.
   * They are only available through their parent.
   */
  /*@Pure*/
  @SuppressWarnings("nullness") // postcondition: linked maps
  /*@EnsuresNonNullIf(result=true, expression="get(#1)")*/
  // get(#1) == nameToPpt.get(#1)
  public boolean containsName(String name) {
    return nameToPpt.containsKey(name);
  }

  /** Returns all of the program points in the map **/
  public Collection<PptTopLevel> all_ppts() {
    return (nameToPpt.values());
  }

  /**
   * @return unstably-ordered collection of PptTopLevels
   * @see #pptIterator()
   **/
  public Collection<PptTopLevel> asCollection() {
    return Collections.unmodifiableCollection(nameToPpt.values());
  }

  /**
   * @return an unmodifiable version of the keySet
   */
  // daikon/tools/compare/LogicalCompare.java:745 does not typecheck, no
  // matter whether the annotation argument is "this.nameToPpt" or
  // "nameToPpt".  This method only typechecks if the annotation argument
  // is "nameToPpt", not "this.nameToPpt".  (Yes, nameToPpt is a private
  // variable, but I'd like the annotation to work anyway, at least for the
  // moment.)
  public Collection</*@KeyFor("nameToPpt")*/ String> nameStringSet() {
    // return Collections.unmodifiableSet(nameToPpt.keySet());
    Set</*@KeyFor("nameToPpt")*/ String> s = nameToPpt.keySet();
    return Collections.unmodifiableSet(s);
  }

  /**
   * @return an iterator over the PptTopLevels in this, sorted by
   * Ppt.NameComparator on their names.  This is good for consistency.
   * <p>
   * If you wish to merely iterate over the result in a Java new-style for
   * loop ("foreach loop"), use {@link #pptIterable()} instead.
   * @see #pptIterable()
   **/
  public Iterator<PptTopLevel> pptIterator() {
    TreeSet<PptTopLevel> sorted = new TreeSet<PptTopLevel>(new Ppt.NameComparator());
    sorted.addAll(nameToPpt.values());
    // Use a (live) view iterator to get concurrent modification
    // exceptions, and an iterator over sorted to get consistency.
    final Iterator<PptTopLevel> iter_view = nameToPpt.values().iterator();
    final Iterator<PptTopLevel> iter_sort = sorted.iterator();
    return new Iterator<PptTopLevel>() {
      public boolean hasNext() {
        boolean result = iter_view.hasNext();
        assert result == iter_sort.hasNext();
        return result;
      }

      public PptTopLevel next() {
        iter_view.next(); // to check for concurrent modifications
        return iter_sort.next();
      }

      public void remove() {
        throw new UnsupportedOperationException();
      }
    };
  }

  /**
   * @return an iterable over the PptTopLevels in this, sorted by
   * Ppt.NameComparator on their names.  This is good for consistency.
   * <p>
   * It is a wrapper around {@link #pptIterator()} that can be used in a
   * Java new-style for loop ("foreach loop").
   * @see #pptIterator()
   */
  public Iterable<PptTopLevel> pptIterable() {
    return new IterableIterator<PptTopLevel>(pptIterator());
  }

  /**
   * @return an iterator over the PptTopLevels in this, sorted by
   * Ppt.NameComparator on their names.  This differs from pptIterator()
   * in that it includes all ppts (including conditional ppts).
   * <p>
   * If you wish to merely iterate over the result in a Java new-style for
   * loop ("foreach loop"), use {@link #ppt_all_iterable()} instead.
   * @see #ppt_all_iterable()
   **/
  public Iterator<PptTopLevel> ppt_all_iterator() {
    TreeSet<PptTopLevel> sorted = new TreeSet<PptTopLevel>(new Ppt.NameComparator());
    sorted.addAll(nameToPpt.values());
    // Use a (live) view iterator to get concurrent modification
    // exceptions, and an iterator over sorted to get consistency.
    final Iterator<PptTopLevel> iter_view = nameToPpt.values().iterator();
    final Iterator<PptTopLevel> iter_sort = sorted.iterator();
    return new Iterator<PptTopLevel>() {
      /*@Nullable*/ Iterator<PptConditional> cond_iterator = null;

      public boolean hasNext() {
        if ((cond_iterator != null) && cond_iterator.hasNext()) {
          return true;
        }
        boolean result = iter_view.hasNext();
        assert result == iter_sort.hasNext();
        return result;
      }

      public PptTopLevel next() {
        if ((cond_iterator != null) && cond_iterator.hasNext()) {
          return (cond_iterator.next());
        }
        iter_view.next(); // to check for concurrent modifications
        PptTopLevel ppt = iter_sort.next();
        if ((ppt != null) && ppt.has_splitters()) cond_iterator = ppt.cond_iterator();
        return ppt;
      }

      public void remove() {
        throw new UnsupportedOperationException();
      }
    };
  }

  /**
   * @return an iterable over the PptTopLevels in this, sorted by
   * Ppt.NameComparator on their names.  This differs from pptIterable()
   * in that it includes all ppts (including conditional ppts).
   * <p>
   * It is a wrapper around {@link #ppt_all_iterator()} that can be used in a
   * Java new-style for loop ("foreach loop").
   * @see #ppt_all_iterator()
   */
  public Iterable<PptTopLevel> ppt_all_iterable() {
    return new IterableIterator<PptTopLevel>(ppt_all_iterator());
  }

  /** Iterate over the PptTopLevels and trim them. */
  public void trimToSize() {
    for (PptTopLevel ppt : nameToPpt.values()) {
      ppt.trimToSize();
    }
  }

  /**
   * Check the rep invariant of this.  Throws an Error if incorrect.
   **/
  public void repCheck() {
    for (PptTopLevel ppt : this.pptIterable()) {
      ppt.repCheck();
    }
  }

  /**
   * Return the number of active PptSlices.
   **/
  /*@Pure*/
  public int countSlices() {
    int result = 0;
    for (PptTopLevel ppt : this.pptIterable()) {
      result += ppt.numViews();
    }
    return result;
  }

  /*@Pure*/
  public int size() {
    return nameToPpt.size();
  }

  /*@SideEffectFree*/
  public String toString() {
    return "PptMap: " + nameToPpt.toString();
  }

  /**
   * Blow away any PptTopLevels that never saw any samples (to reclaim space).
   **/
  public void removeUnsampled() {
    Iterator<PptTopLevel> iter = nameToPpt.values().iterator();
    while (iter.hasNext()) {
      PptTopLevel ppt = iter.next();
      if ((ppt.num_samples() == 0) && !FileIO.has_unmatched_procedure_entry(ppt)) iter.remove();
    }
  }
}
