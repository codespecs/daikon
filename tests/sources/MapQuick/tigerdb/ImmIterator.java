package MapQuick.tigerdb;

import java.util.Iterator;

/**
 * ImmIterator.java
 *
 *
 * Created: Thu Aug 17 17:12:54 2000
 *
 * @author Felix S. Klock
 */

public abstract class ImmIterator implements Iterator {
  public static Iterator wrap(final Iterator i) {
    return new ImmIterator() {
	public boolean hasNext() { return i.hasNext(); }
	public Object next() { return i.next(); }
      };
  }
  public void remove() {
    throw new UnsupportedOperationException();
  }
    
} // ImmIterator
