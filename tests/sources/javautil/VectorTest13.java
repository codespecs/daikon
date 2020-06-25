package javautil;

import junit.framework.*;
import org.junit.Test;

public class VectorTest13 extends TestCase {

  public static final int NUM = 5;

  Vector13 v;
  Vector13[] vectors = new Vector13[NUM];

  Object[] o = new Object[NUM];

  public VectorTest13(String name) {
    super(name);
    o[0] = new Object();
    o[1] = o[0];
    o[2] = null;
    o[3] = new Integer(0);
    o[4] = new Integer(1);

    for (int i = 0; i < NUM; i++) {
      vectors[i] = new Vector13(NUM);
      for (int j = 0; j < i; j++) {
        vectors[i].addElement(o[j]);
      }
    }
  }

  public static void main(String[] args) {
    junit.textui.TestRunner.run(new TestSuite(VectorTest13.class));
  }

  @Test
  public void testConstructorNoArg() {
    for (int i = 0; i < NUM; i++) {
      v = new Vector13();
    }
  }

  @Test
  public void testConstructorOneArg() {
    for (int j = 0; j < 50; j++) {
      for (int i = 0; i < NUM; i++) {
        v = new Vector13(i);
      }
    }
  }

  @Test
  public void testConstructorTwoArg() {
    for (int i = 0; i < NUM; i++) {
      for (int j = 0; j < NUM; j++) {
        v = new Vector13(i, j);
      }
    }
  }

  @Test
  public void testCopyInto() {
    VectorCommand c =
        new VectorCommand() {
          public void run(Vector13 v, int cap, int inc, int elems) {
            Object[] a;
            a = new Object[elems];
            v.copyInto(a);
            a = new Object[2 * elems];
            v.copyInto(a);
            a = new Object[elems + 1];
            a[0] = o;
            v.copyInto(a);
          }
        };
    runVectorCommand(c);
  }

  @Test
  public void testTrimToSize() {
    VectorCommand c =
        new VectorCommand() {
          public void run(Vector13 v, int cap, int inc, int elems) {
            v.trimToSize();
          }
        };
    runVectorCommand(c);
  }

  @Test
  public void testEnsureCapacity() {
    VectorCommand c =
        new VectorCommand() {
          public void run(Vector13 v, int cap, int inc, int elems) {
            for (int i = 0; i < NUM + 1; i++) {
              v.ensureCapacity(i);
            }
          }
        };
    runVectorCommand(c);
  }

  // private method in Vector
  @Test
  public void testEnsureCapacityHelper() {}

  @Test
  public void testSetSize() {
    VectorCommand c =
        new VectorCommand() {
          public void run(Vector13 v, int cap, int inc, int elems) {
            v.setSize((cap + inc) % (NUM + 1));
          }
        };
    runVectorCommand(c);
  }

  @Test
  public void testObservers() {
    VectorCommand c =
        new VectorCommand() {
          public void run(Vector13 v, int cap, int inc, int elems) {
            v.size();
            v.isEmpty();
            v.elements();
            v.contains(o[0]);
            v.contains(o[3]);
            for (int i = 0; i < NUM; i++) {
              Object obj = (o[i] != null ? o[i] : new Object());
              v.indexOf(obj);
              for (int j = 0; j < NUM + 1; j++) {
                v.indexOf(obj, j);
              }
            }
            for (int i = 0; i < NUM; i++) {
              Object obj = (o[i] != null ? o[i] : new Object());
              v.lastIndexOf(obj);
              for (int j = 0; j < NUM + 1; j++) {
                // lastIndexOf throws an ArrayIndexOutOfBounds exception
                // if j >= elems
                if (j < elems) {
                  v.lastIndexOf(obj, j);
                }
              }
            }
            for (int i = 0; i < elems; i++) {
              v.elementAt(i);
            }
            if (elems > 0) {
              v.firstElement();
              v.lastElement();
            }
            v.clone();
            v.toString();
          }
        };

    runVectorCommand(c);
  }

  @Test
  public void testSetElementAt() {
    VectorCommand c =
        new VectorCommand() {
          public void run(Vector13 v, int cap, int inc, int elems) {
            for (int i = 0; i < elems; i++) {
              v.setElementAt(o[i], i);
            }
          }
        };
    runVectorCommand(c);
  }

  @Test
  public void testRemoveElementAt() {
    VectorCommand c =
        new VectorCommand() {
          public void run(Vector13 v, int cap, int inc, int elems) {
            if (elems > 0) {
              v.removeElementAt((cap + inc) % elems);
            }
          }
        };
    runVectorCommand(c);
  }

  @Test
  public void testInsertElementAt() {
    VectorCommand c =
        new VectorCommand() {
          public void run(Vector13 v, int cap, int inc, int elems) {
            if (elems == 0) {
              v.insertElementAt(o[(cap + inc) % NUM], 0);
            } else {
              v.insertElementAt(o[(cap + inc) % NUM], (cap + inc) % (elems + 1));
            }
          }
        };
    runVectorCommand(c);
  }

  // Runs command c on many vectors of different size, capacity, and contents
  public void runVectorCommand(VectorCommand command) {
    for (int cap = 0; cap < NUM; cap++) {
      for (int inc = 0; inc < NUM; inc++) {
        for (int elems = 0; elems < NUM + 1; elems++) {
          v = new Vector13(cap, inc);
          for (int elem = 0; elem < elems; elem++) {
            // Add the elements in different order sometimes
            v.addElement(o[(elem + cap) % elems]);
          }
          command.run(v, cap, inc, elems);
        }
      }
    }
  }

  @Test
  public void testRemoveElement() {
    VectorCommand c =
        new VectorCommand() {
          public void run(Vector13 v, int cap, int inc, int elems) {
            Object obj = o[(cap + inc) % NUM];
            obj = (obj != null) ? obj : new Object();
            v.removeElement(obj);
          }
        };
    runVectorCommand(c);
  }

  @Test
  public void testRemoveAllElements() {
    VectorCommand c =
        new VectorCommand() {
          public void run(Vector13 v, int cap, int inc, int elems) {
            v.removeAllElements();
          }
        };
    runVectorCommand(c);
  }

  private interface VectorCommand {
    public abstract void run(Vector13 v, int cap, int inc, int elems);
  }
}
