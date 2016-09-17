package MapQuick2;

import MapQuick.*;
import java.util.*;

public class RouteIterator implements java.util.Iterator{
  private Enumeration e;
  
  public RouteIterator(Vector v){
    e = v.elements();
  }

  /**  Tests if this enumeration contains more elements.
   * @return:
   *     true if and only if this enumeration object contains at least one more
   *     element to provide; false otherwise.
   */
  public boolean hasNext(){
    return e.hasMoreElements();
  }

  /** Returns the next element of this enumeration if this enumeration object
   * has at least one more element to provide.
   * @return:
   *     the next element of this enumeration.
   * @throws:
   *     NoSuchElementException - if no more elements exist.
   */
  public Object next(){
    try{
      return e.nextElement();
    } catch(NoSuchElementException e) {
      // throw an exception?
      return null;
    }
  }

  /** immutable Iterator, cannot remove an object
   *  modifies: nothing
   *  returns: nothing
   *  effects: nothing
   */
  public final void remove(){
    //nothing
  }
}
