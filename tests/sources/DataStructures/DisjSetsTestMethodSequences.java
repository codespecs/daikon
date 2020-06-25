package DataStructures;

/**
 * DisjSetsTestMethodSequences.java
 *
 * <p>Class for testing DisjSets by invoking all possible methods sequences (up to a given length)
 * on DisjSets objects.
 *
 * <p>Created: Sun Aug 5 06:01:34 2001
 *
 * @author Darko Marinov (marinov@lcs.mit.edu)
 * @version
 */
import java.lang.reflect.*;
import java.util.*;
import org.junit.Test;

public class DisjSetsTestMethodSequences {

  // This is the main method for testing.

  public static void main(String[] args) {
    // Get the length of method sequences.
    int length = Integer.parseInt(args[0]);
    // Get the maximum values for method parameters.
    int maxSize = Integer.parseInt(args[1]);
    for (int size = 1; size <= maxSize; size++) {
      Class claz = getClassForName("DataStructures.DisjSets");
      // Create all possible constructors.
      Constructor constr = getFirstConstructor(claz);
      Set[] params = new Set[] {Collections.singleton(new Integer(size))};
      Set constructors = makeCartesianProduct(constr, params);
      // Create all possible methods.
      Method union = getFirstMethod(claz, "unionCareful");
      Set range = makeIntegerSet(0, size - 1);
      Set[] paramsUnion = new Set[] {range, range};
      Set methods = makeCartesianProduct(union, paramsUnion);
      Method find = getFirstMethod(claz, "find");
      Set[] paramsFind = new Set[] {range};
      methods.addAll(makeCartesianProduct(find, paramsFind));
      // Test all possible method sequences.
      testMethodSequences(constructors, methods, length);
    }
  }

  // The rest is some generic (i.e., not specific to DisjSets) code
  // for invoking all possible method sequences.  This code should be
  // refactored in several classes if the approach of testing method
  // sequences looks promising, which is not the case right now.

  public static boolean printExceptions = false;

  // Test all possible sequences of constructors and methods.
  static @Test public void testMethodSequences(Set constructors, Set methods, int length) {
    Set[] sets = new Set[length];
    sets[0] = constructors;
    for (int i = 1; i < length; i++) sets[i] = methods;
    CartesianProduct c = new CartesianProduct(sets);
    for (Iterator t = c.iterator(); t.hasNext(); ) {
      Object[] sequence = (Object[]) t.next();
      // Create an object with one of the constructors.
      Object o = createObject((TestConstructor) sequence[0]);
      // Invoke methods.
      for (int i = 1; i < length; i++) invokeMethod(o, (TestMethod) sequence[i]);
    }
  }

  // Create an object using the given constructor (parameters given).
  public static Object createObject(TestConstructor testConstr) {
    try {
      return testConstr.constr.newInstance(testConstr.params);
    } catch (InstantiationException e) {
      if (printExceptions) System.out.println(e);
    } catch (IllegalAccessException e) {
      if (printExceptions) System.out.println(e);
    } catch (IllegalArgumentException e) {
      if (printExceptions) System.out.println(e);
    } catch (InvocationTargetException e) {
      if (printExceptions) System.out.println(e);
      if (printExceptions) System.out.println(e.getTargetException());
    }
    return null;
  }

  // Invoke the given method (parameters given) on the given object.
  public static Object invokeMethod(Object o, TestMethod testMethod) {
    try {
      return testMethod.method.invoke(o, testMethod.params);
    } catch (IllegalAccessException e) {
      if (printExceptions) System.out.println(e);
    } catch (InvocationTargetException e) {
      if (printExceptions) System.out.println(e);
      if (printExceptions) System.out.println(e.getTargetException());
    }
    return null;
  }

  // Get the class object for the given name.
  public static Class getClassForName(String name) {
    try {
      return Class.forName(name);
    } catch (ClassNotFoundException e) {
      if (printExceptions) System.out.println(e);
    }
    return null;
  }

  // Get the first constructor of the given class.
  public static Constructor getFirstConstructor(Class claz) {
    Constructor[] constrs = claz.getConstructors();
    return constrs[0];
  }

  // Get the first method with the given name from the given class.
  public static Method getFirstMethod(Class claz, String name) {
    Method[] m = claz.getDeclaredMethods();
    for (int i = 0; i < m.length; i++) if (m[i].getName().equals(name)) return m[i];
    return null;
  }

  // Pair of a constructor and its parameters.
  public static class TestConstructor {
    Constructor constr;
    Object[] params;

    public TestConstructor(Constructor c, Object[] p) {
      constr = c;
      params = p;
    }

    public String toString() {
      return "<" + constr.toString() + "," + Arrays.asList(params).toString() + ">";
    }
  }

  // Pair of a method and its parameters.
  public static class TestMethod {
    Method method;
    Object[] params;

    public TestMethod(Method m, Object[] p) {
      method = m;
      params = p;
    }

    public String toString() {
      return "<" + method.toString() + "," + Arrays.asList(params).toString() + ">";
    }
  }

  // Class for generating Cartesian products of a number of sets.
  public static class CartesianProduct {
    private Object[][] objects;

    public CartesianProduct(Set[] sets) {
      objects = new Object[sets.length][];
      for (int i = 0; i < objects.length; i++) objects[i] = sets[i].toArray();
    }

    Iterator iterator() {
      return new CartesianProductIterator(this);
    }

    private class CartesianProductIterator implements Iterator {
      int index[];
      boolean more;

      public CartesianProductIterator(CartesianProduct cp) {
        index = new int[objects.length];
        more = true;
      }

      public boolean hasNext() {
        return more;
      }

      public Object next() {
        if (!more) throw new NoSuchElementException();
        Object[] r = new Object[index.length];
        for (int i = 0; i < r.length; i++) r[i] = objects[i][index[i]] /*.clone()*/;
        int i = index.length - 1;
        while (i >= 0) {
          index[i]++;
          if (index[i] == objects[i].length) {
            index[i] = 0;
            i--;
          } else break;
        }
        more = i > -1;
        return (Object) r;
      }

      public void remove() {
        throw new UnsupportedOperationException();
      }
    }
  }

  // Make Cartesian product of all construtors and all parameters.
  public static Set makeCartesianProduct(Constructor constr, Set[] params) {
    Set r = new HashSet();
    CartesianProduct c = new CartesianProduct(params);
    for (Iterator i = c.iterator(); i.hasNext(); )
      r.add(new TestConstructor(constr, (Object[]) i.next()));
    return r;
  }

  // Make Cartesian product of all construtors and all parameters.
  public static Set makeCartesianProduct(Method method, Set[] params) {
    Set r = new HashSet();
    CartesianProduct c = new CartesianProduct(params);
    for (Iterator i = c.iterator(); i.hasNext(); )
      r.add(new TestMethod(method, (Object[]) i.next()));
    return r;
  }

  // Make Cartesian product of all constructors/methods and all parameters.
  public static Set makeCartesianProduct(Set constrOrMethod, Set[] params) {
    Set r = new HashSet();
    for (Iterator i = constrOrMethod.iterator(); i.hasNext(); ) {
      Object o = i.next();
      if (o instanceof Constructor) {
        r.addAll(makeCartesianProduct((Constructor) o, params));
      } else if (o instanceof Method) {
        r.addAll(makeCartesianProduct((Method) o, params));
      } // else throw new ...
    }
    return r;
  }

  // Create a set of integers in the given range.
  public static Set makeIntegerSet(int low, int high) {
    Set r = new HashSet();
    while (low <= high) {
      r.add(new Integer(low));
      low++;
    }
    return r;
  }
} // DisjSetsTestMethodSequences
