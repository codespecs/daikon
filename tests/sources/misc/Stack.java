// Authored by Jeremy Nimmer as an example for his thesis.

package misc;

public class Stack {

  private Object[] elts;
  private int      size;

  public Stack(int capacity) {
    elts = new Object[capacity];
    size = 0;
  }

  public boolean isEmpty() {
    return size == 0;
  }

  public boolean isFull() {
    return size == elts.length;
  }

  public void push(Object x) {
    elts[size++] = x;
  }

  public Object pop() {
    Object item = elts[--size];
    elts[size] = null;
    return item;
  }

}
