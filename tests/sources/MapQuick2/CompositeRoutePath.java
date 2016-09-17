package MapQuick2;

import MapQuick.*;
import java.util.Iterator;

public class CompositeRoutePath implements Path {

  private CompositeRoute compRoute;
  private GeoSegment[] gsArray;
  private double length;
  private Cons geoList;
  private int size;
  

  public CompositeRoutePath(GeoSegment gs) {
    compRoute = new CompositeRoute(gs);
    geoList = new Cons(gs, null);
    gsArray = null;
    size = 1;
    length = compRoute.length();
  }

  private CompositeRoutePath(CompositeRoutePath crp, GeoSegment gs) {
    compRoute = crp.compRoute.addSegment(gs);
    geoList = new Cons(gs, crp.geoList);
    gsArray = null;
    size = crp.size + 1;
    length = compRoute.length();
  }
  

  // Inherited from the Path interface.
  
  // Producers
  
  /**
   * Creates an extended path by adding a new node to its end.
   * @requires n != null && n is a valid node type for this particular
   *           path implementation
   * @return a new Path p such that p.elements = this.elements + [ n ]
   *         && p.cost >= this.cost
   */
  public Path extend(Object n) {
    if (n == null)
      throw new NullPointerException("Cannot extend with a null node.");
    if (!(n instanceof GeoSegment))
      throw new RuntimeException("CompositeRoutePath must be extended "+
                                 "with a GeoSegment.");
    return new CompositeRoutePath(this, (GeoSegment)n);
  }
  
  // Observers
  
  /**
   * @return an Iterator that produces the contents of this.elements,
   *            in order
   */
  public Iterator elements() {

    if (gsArray == null) {
      gsArray = new GeoSegment[size];
      Cons gsInd = geoList;
      for (int i=size-1; i>=0; i--) {
        gsArray[i] = gsInd.head;
        gsInd = gsInd.tail;
      }
    }
    return new UnmodifyableArrayIterator(gsArray);
  }
  
  /** @return this.cost */
  public double cost() {
    return length;
  }

  public String directions() {
    return compRoute.directions(0.0);
  }
  
  
  
  private class UnmodifyableArrayIterator implements Iterator {
    private Object[] ar;
    private int index;
    
    public UnmodifyableArrayIterator(Object[] a) {
      ar = a;
      index = 0;
    }

    public boolean hasNext() {
      return (index < ar.length);
    }

    public Object next() {
      return ar[index++];
    }

    public void remove() {
      throw new RuntimeException("Cannot remove item from unmodifyable Iterator.");
    }
  }

  private class Cons {
    public GeoSegment head;
    public Cons tail;

    Cons(GeoSegment head, Cons tail) {
      this.head = head;
      this.tail = tail;
    }
  }
  
}
