package daikon;

import java.util.*;

public class GuardingVariableList
  implements List
{
  private List listPart;
  private HashSet setPart;

  public GuardingVariableList() {
    listPart = new ArrayList();
    setPart = new HashSet();
  }

  public int size() {
    return listPart.size();
  }

  public boolean isEmpty() {
    return listPart.isEmpty();
  }

  public void add(int index, Object element) throws UnsupportedOperationException
  {
    throw new UnsupportedOperationException();
  }

  public boolean add(Object o) {
    boolean canAdd = setPart.add(o);

    if (canAdd)
      listPart.add(o);

    return canAdd;
  }

  public boolean addAll(Collection c) {
    Iterator i = c.iterator();

    boolean retval = false;
    boolean currentRep;

    while (i.hasNext()) {
      currentRep = add(i.next());
      retval = retval || currentRep;
    }

    return retval;
  }

  public boolean addAll(int index, Collection c) throws UnsupportedOperationException {
    throw new UnsupportedOperationException();
  }

  public void clear() throws UnsupportedOperationException {
    listPart.clear();
    setPart.clear();
  }

  public boolean contains(Object o) {
    return setPart.contains(o);
  }

  public Iterator iterator() {
    return listPart.iterator();
  }

  public Object[] toArray() {
    return listPart.toArray();
  }

  public Object[] toArray(Object[] a) throws ArrayStoreException {
    return listPart.toArray(a);
  }

  public boolean containsAll(Collection c) {
    return setPart.containsAll(c);
  }

  public boolean remove(Object o) {
    listPart.remove(o);
    return setPart.remove(o);
  }

  public Object remove(int index) throws IndexOutOfBoundsException {
    Object removed = listPart.remove(index);
    setPart.remove(removed);

    return removed;
  }

  public boolean removeAll(Collection c) {
    listPart.removeAll(c);
    return setPart.removeAll(c);
  }

  public boolean retainAll(Collection c) throws UnsupportedOperationException {
    listPart.retainAll(c);
    return setPart.retainAll(c);
  }

  public boolean equals(Object o) {
    if (!(o instanceof List))
      return false;

    List oAsList = (List)o;

    return oAsList.equals(listPart);
  }

  public int hashCode() {
    return listPart.hashCode();
  }

  public Object get(int index) {
    return listPart.get(index);
  }

  public Object set(int index, Object o) throws UnsupportedOperationException {
    throw new UnsupportedOperationException();
  }

  public int indexOf(Object o) {
    return listPart.indexOf(o);
  }

  public int lastIndexOf(Object o) {
    return listPart.lastIndexOf(o);
  }

  public ListIterator listIterator() {
    return listPart.listIterator();
  }

  public ListIterator listIterator(int index) throws IndexOutOfBoundsException {
    return listPart.listIterator(index);
  }

  public List subList(int from, int to) throws IndexOutOfBoundsException {
    return listPart.subList(from, to);
  }
}
