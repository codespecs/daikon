package utilMDE;

import java.util.*;

/**
 * Given two sequences/iterators/whatever, thsi class returns a new
 * sequence/iterator/whatever which pairs the matching elements of the
 * inputs.
 *
 * Requires that the elements in the input iterators be sorted in the
 * same order.
 *
 * It's not clear that this is always the right thing to do; you might
 * find it more convenient to use use set intersection/difference.  */
public class OrderedPairIterator implements java.util.Iterator {

  Iterator itor1, itor2;
  Object next1, next2;
  Comparator comparator;

  public OrderedPairIterator(Iterator itor1, Iterator itor2) {
    this(itor1, itor2, null);
  }
  public OrderedPairIterator(Iterator itor1, Iterator itor2, Comparator comparator) {
    this.itor1 = itor1;
    this.itor2 = itor2;
    setnext1();
    setnext2();
    this.comparator = comparator;
  }
  private void setnext1() { next1 = itor1.hasNext() ? itor1.next() : null; }
  private void setnext2() { next2 = itor2.hasNext() ? itor2.next() : null; }
  // Have the caller do this directly, probably.
  // public OrderedPairIterator(Set s1, Set s2) {
  //   this((new TreeSet(s1)).iterator(), (new TreeSet(s2)).iterator());
  // }
  public boolean hasNext() { return ((next1 != null) || (next2 != null)); }
  /** Return an element of the first iterator, paired with null. */
  private Pair return1() {
    Pair result = new Pair(next1, null);
    setnext1();
    return result;
  }
  /** Return a pair of null and an element of the second iterator. */
  private Pair return2() {
    Pair result = new Pair(null, next2);
    setnext2();
    return result;
  }
  /** Return a pair containing an element from each iterator. */
  private Pair returnboth() {
    Pair result = new Pair(next1, next2);
    setnext1();
    setnext2();
    return result;
  }

  public Object next() {
    if (next1 == null) {
      if (next2 == null) {
        throw new NoSuchElementException();
      } else {
        return return2();
      }
    } else {
      if (next2 == null) {
        return return1();
      } else {
        int comparison;
        if (comparator == null) {
          comparison = ((Comparable)next1).compareTo(next2);
        } else {
          comparison = comparator.compare(next1, next2);
        }
        if (comparison < 0)
          return return1();
        else if (comparison > 0)
          return return2();
        else
          return returnboth();
      }
    }
  }
  public void remove() { throw new UnsupportedOperationException(); }
}

