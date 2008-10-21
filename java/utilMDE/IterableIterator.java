// See http://www.rittau.org/blog/20061122-00

package utilMDE;

import java.util.*;

/**
 * In Java, Iterators are not Iterable, so they cannot be used in new-style
 * for loops.  This wrapper works around that by making an Iterator that is
 * also Iterable -- that is, it implements the iterator() method.
 * <p>
 *
 * <b>Note:</b> Some clients might expect that calling Iterable.iterator()
 * twice on an Iterable results in two objects that can both iterate over
 * the whole sequence, and that won't interfere with one another.  That is
 * not the case for this Iterable.
 * <p>
 *
 * It's often beter to use a real Iterable (e.g., a collections class)
 * rather than an Iterator.  But in some cases the overhead is undesirable,
 * or there are multiple ways to iterate so it doesn't make sense to
 * reserve the iterator() method for just one of them.  This class can be
 * appropriate in such circumstances.
 */

class IterableIterator<T> implements Iterable<T> {
    private Iterator<T> iter;

    public IterableIterator(Iterator<T> iter) {
        this.iter = iter;
    }

    public Iterator<T> iterator() {
        return iter;
    }
}
