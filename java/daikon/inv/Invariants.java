package daikon.inv;

import daikon.*;
import java.util.*;

// This is essentially a collection of Invariant objects, but with a few
// convenience methods.
public class Invariants {

  // Should this specify a Ppt or PptSlice with which it is associated?
  // Probably.

  // This was public.  Why?
  private Vector invs;

  public Invariants() {
    invs = new Vector();
  }

  // return a list of all the invariants that involve the specified VarInfo
  public Vector lookup(VarInfo vi) {
    Vector result = new Vector();
    for (Enumeration e = invs.elements(); e.hasMoreElements() ; ) {
      Invariant inv = (Invariant) e.nextElement();
      if (inv.usesVar(vi))
	result.add(inv);
    }
    return result;
  }

  // return a list of all the invariants that involve the specified VarInfos
  public Vector lookup(VarInfo vi1, VarInfo vi2) {
    Vector result = new Vector();
    for (Enumeration e = invs.elements(); e.hasMoreElements() ; ) {
      Invariant inv = (Invariant) e.nextElement();
      if (inv.usesVar(vi1) && inv.usesVar(vi2))
	result.add(inv);
    }
    return result;
  }

  // return a list of all the invariants that involve the specified VarInfos
  public Vector lookup(VarInfo vi1, VarInfo vi2, VarInfo vi3) {
    Vector result = new Vector();
    for (Enumeration e = invs.elements(); e.hasMoreElements() ; ) {
      Invariant inv = (Invariant) e.nextElement();
      if (inv.usesVar(vi1) && inv.usesVar(vi2) && inv.usesVar(vi3))
	result.add(inv);
    }
    return result;
  }

  ///
  /// Vector style operations
  ///

  //      public void
  //          add(int index, Invariant element)
  //                    Inserts the specified element at the specified position in this Vector.
  /** Appends the specified element to the end of this collection. */
  public boolean add(Invariant o) { return invs.add(o); }
  //    public boolean
  //          addAll(Collection c)
  //                    Appends all of the elements in the specified Collection to the end of this Vector, in the order that they are returned by the
  //          specified Collection's Iterator.
  //    public boolean
  //          addAll(int index, Collection c)
  //                    Inserts all of the elements in in the specified Collection into this Vector at the specified position.
  //      public void
  //          addElement(Invariant obj)
  //                    Adds the specified component to the end of this vector, increasing its size by one.
  //       public int
  //          capacity()
  //                    Returns the current capacity of this vector.
  //      public void
  //          clear()
  //                    Removes all of the elements from this Vector.
  //     public Invariant
  //          clone()
  //                    Returns a clone of this vector.
  //    public boolean
  //          contains(Invariant elem)
  //                    Tests if the specified Invariant is a component in this vector.
  //    public boolean
  //          containsAll(Collection c)
  //                    Returns true if this Vector contains all of the elements in the specified Collection.
  //      public void
  //          copyInto(Invariant[] anArray)
  //                    Copies the components of this vector into the specified array.
  /** Returns the component at the specified index. */
  public Invariant elementAt(int index) {
    return (Invariant)invs.elementAt(index);
  }
  // public Enumeration
  //          elements()
  //                    Returns an enumeration of the components of this vector.
  //      public void
  //          ensureCapacity(int minCapacity)
  //                    Increases the capacity of this vector, if necessary, to ensure that it can hold at least the number of components specified
  //          by the minimum capacity argument.
  //    public boolean
  //          equals(Invariant o)
  //                    Compares the specified Invariant with this Vector for equality.
  //     public Invariant
  //          firstElement()
  //                    Returns the first component (the item at index 0) of this vector.
  //     public Invariant
  //          get(int index)
  //                    Returns the element at the specified position in this Vector.
  //       public int
  //          hashCode()
  //                    Returns the hash code value for this Vector.
  //       public int
  //          indexOf(Invariant elem)
  //                    Searches for the first occurence of the given argument, testing for equality using the equals method.
  //       public int
  //          indexOf(Invariant elem, int index)
  //                    Searches for the first occurence of the given argument, beginning the search at index, and testing for equality using the
  //          equals method.
  //      public void
  //          insertElementAt(Invariant obj, int index)
  //                    Inserts the specified Invariant as a component in this vector at the specified index.
  //    public boolean
  //          isEmpty()
  //                    Tests if this vector has no components.
  //     public Invariant
  //          lastElement()
  //                    Returns the last component of the vector.
  //       public int
  //          lastIndexOf(Invariant elem)
  //                    Returns the index of the last occurrence of the specified Invariant in this vector.
  //       public int
  //          lastIndexOf(Invariant elem, int index)
  //                    Searches backwards for the specified Invariant, starting from the specified index, and returns an index to it.
  //     public Invariant
  //          remove(int index)
  //                    Removes the element at the specified position in this Vector.
  //    public boolean
  //          remove(Invariant o)
  //                    Removes the first occurrence of the specified element in this Vector If the Vector does not contain the element, it is
  //          unchanged.
  //    public boolean
  //          removeAll(Collection c)
  //                    Removes from this Vector all of its elements that are contained in the specified Collection.
  //      public void
  //          removeAllElements()
  //                    Removes all components from this vector and sets its size to zero.
  //    public boolean
  //          removeElement(Invariant obj)
  //                    Removes the first (lowest-indexed) occurrence of the argument from this vector.
  //      public void
  //          removeElementAt(int index)
  //                    Deletes the component at the specified index.
  //  protected void
  //          removeRange(int fromIndex, int toIndex)
  //                    Removes from this List all of the elements whose index is between fromIndex, inclusive and toIndex, exclusive.
  //    public boolean
  //          retainAll(Collection c)
  //                    Retains only the elements in this Vector that are contained in the specified Collection.
  //     public Invariant
  //          set(int index, Invariant element)
  //                    Replaces the element at the specified position in this Vector with the specified element.
  //      public void
  //          setElementAt(Invariant obj, int index)
  //                    Sets the component at the specified index of this vector to be the specified Invariant.
  //      public void
  //          setSize(int newSize)
  //                    Sets the size of this vector.
  /** Returns the number of components in this collection. */
  public int size() { return invs.size(); }
  //      public List
  //          subList(int fromIndex, int toIndex)
  //                    Returns a view of the portion of this List between fromIndex, inclusive, and toIndex, exclusive.
  //   public Invariant[]
  //          toArray()
  //                    Returns an array containing all of the elements in this Vector in the correct order.
  //   public Invariant[]
  //          toArray(Invariant[] a)
  //                    Returns an array containing all of the elements in this Vector in the correct order.
  //     public String
  //          toString()
  //                    Returns a string representation of this Vector, containing the String representation of each element.
  //      public void
  //          trimToSize()
  //                    Trims the capacity of this vector to be the vector's current size.



}
