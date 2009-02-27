package utilMDE;

import java.util.*;

/**
 * Given two sequences/iterators/whatever, this class returns a new
 * sequence/iterator/whatever which pairs the matching elements of the
 * inputs, according to their respective sort orders.
 * The result is as long as the longer of the inputs and uses nulls to pad out the shorter input.
 *
 * <p>
 * In some cases this is just the right abstraction.  But in some cases
 * it's appropriate to use set intersection/difference instead.
 */
// T need not extend Comparable<T>, because a comparator can be passed in.
public class OrderedPairIterator<T extends /*@Nullable*/ Object> implements java.util.Iterator<Pair<T,T>> {

  Iterator<T> itor1, itor2;
  T next1, next2;
  /*@Nullable*/ Comparator<? super T> comparator;

  // For this constructor, the arg type is actually Iterator<T extends
  // Comparable<T>>, but T is already bound above and can't be changed.
  public OrderedPairIterator(Iterator<T> itor1, Iterator<T> itor2) {
    this(itor1, itor2, null);
  }
  public OrderedPairIterator(Iterator<T> itor1, Iterator<T> itor2, /*@Nullable*/ Comparator<T> comparator) {
    this.itor1 = itor1;
    this.itor2 = itor2;
    setnext1();
    setnext2();
    this.comparator = comparator;
  }
  private void setnext1() /*@Raw*/ {
    assert itor1 != null;
    next1 = itor1.hasNext() ? itor1.next() : null;
  }
  private void setnext2() /*@Raw*/ {
    assert itor2 != null;
    next2 = itor2.hasNext() ? itor2.next() : null;
  }
  // Have the caller do this directly, probably.
  // public OrderedPairIterator(Set s1, Set s2) {
  //   this((new TreeSet(s1)).iterator(), (new TreeSet(s2)).iterator());
  // }
  public boolean hasNext() { return ((next1 != null) || (next2 != null)); }
  /** Return an element of the first iterator, paired with null. */
  private Pair<T,T> return1() {
    Pair<T,T> result = new Pair<T,T>(next1, (T)null);
    setnext1();
    return result;
  }
  /** Return a pair of null and an element of the second iterator. */
  private Pair<T,T> return2() {
    Pair<T,T> result = new Pair<T,T>((T)null, next2);
    setnext2();
    return result;
  }
  /** Return a pair containing an element from each iterator. */
  private Pair<T,T> returnboth() {
    Pair<T,T> result = new Pair<T,T>(next1, next2);
    setnext1();
    setnext2();
    return result;
  }

  public Pair<T,T> next() {
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
        // Either T extends Comparable<T>, or else a comparator was passed in.
        if (comparator == null) {
          @SuppressWarnings("unchecked")
          Comparable<T> cble1 = (Comparable<T>)next1;
          comparison = cble1.compareTo(next2);
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
