package utilMDE;

import java.lang.ref.WeakReference;
import java.util.*;

/**
 * Utilities for interning objects.
 */
public class Intern {

  ///////////////////////////////////////////////////////////////////////////
  /// Strings
  ///

  /**
   * Replace each element of the array by its interned version.
   * @see String#intern
   */
  public static void internStrings(String[] a) {
    for (int i=0; i<a.length; i++)
      a[i] = a[i].intern();
  }


  ///////////////////////////////////////////////////////////////////////////
  /// Testing interning
  ///

  public static boolean isInterned(Object value) {
    if (value == null) {
      // nothing to do
      return true;
    } else if (value instanceof String) {
      return (value == ((String) value).intern());
    } else if (value instanceof Integer) {
      return (value == intern((Integer) value));
    } else if (value instanceof int[]) {
      return (value == intern((int[]) value));
    } else if (value instanceof Object[]) {
      return (value == intern((Object[]) value));
    } else {
      // Nothing to do, because we don't intern other types.
      // System.out.println("What type? " + value.getClass().getName());
      return true;
    }
  }


  ///////////////////////////////////////////////////////////////////////////
  /// Interning objects
  ///

  /**
   * Hasher object which hashes and compares Integers.
   * This is the obvious implementation that uses intValue() for the hashCode.
   * @see Hasher
   */
  private static class IntegerHasher implements Hasher {
    public boolean equals(Object a1, Object a2) {
      return a1.equals(a2);
    }
    public int hashCode(Object o) {
      Integer i = (Integer) o;
      return i.intValue();
    }
  }

  /**
   * Hasher object which hashes and compares int[] objects according
   * to their contents.
   * @see Hasher, java.util.Arrays.equals
   */
  private static class IntArrayHasher implements Hasher {
    public boolean equals(Object a1, Object a2) {
      return java.util.Arrays.equals((int[])a1, (int[])a2);
    }
    final static int FACTOR = 23;
    public int hashCode(Object o) {
      int[] a = (int[])o;
      int result = 0;
      for (int i=0; i<a.length; i++) {
        result = result * FACTOR + a[i];
      }
      return result;
    }
  }

  /**
   * Hasher object which hashes and compares Object[] objects according
   * to their contents.
   * @see Hasher, java.util.Arrays.equals
   */
  private static class ObjectArrayHasher implements Hasher {
    public boolean equals(Object a1, Object a2) {
      return java.util.Arrays.equals((Object[])a1, (Object[])a2);
    }
    final static int FACTOR = 23;
    public int hashCode(Object o) {
      Object[] a = (Object[])o;
      int result = 0;
      for (int i=0; i<a.length; i++) {
        int a_hashcode = (a[i] == null) ? 0 : a[i].hashCode();
        result = result * FACTOR + a_hashcode;
      }
      return result;
    }
  }

  // Map from an ArrayWrapper to the array (I don't need to map to a
  // WeakReference because the array isn't the key of the WeakHashMap).

  private static WeakHasherMap internedIntegers;
  private static WeakHasherMap internedIntArrays;
  private static WeakHasherMap internedObjectArrays;

  static {
    internedIntegers = new WeakHasherMap(new IntegerHasher());
    internedIntArrays = new WeakHasherMap(new IntArrayHasher());
    internedObjectArrays = new WeakHasherMap(new ObjectArrayHasher());
  }

  // For testing only
  public static int numIntegers() { return internedIntegers.size(); }
  public static int numIntArrays() { return internedIntArrays.size(); }
  public static int numObjectArrays() { return internedObjectArrays.size(); }
  public static Iterator integers() { return internedIntegers.keySet().iterator(); }
  public static Iterator intArrays() { return internedIntArrays.keySet().iterator(); }
  public static Iterator objectArrays() { return internedObjectArrays.keySet().iterator(); }

  /**
   * Intern (canonicalize) an Integer.
   * Returns a canonical representation for the Integer.
   */
  public static Integer intern(Integer a) {
    Object lookup = internedIntegers.get(a);
    if (lookup != null) {
      WeakReference ref = (WeakReference)lookup;
      return (Integer)ref.get();
    } else {
      internedIntegers.put(a, new WeakReference(a));
      return a;
    }
  }

  // Not sure whether this convenience method is really worth it.
  /** Returns an interned Integer with value i. */
  public static Integer internedInteger(int i) {
    return intern(new Integer(i));
  }

  // Not sure whether this convenience method is really worth it.
  /** Returns an interned Integer with value parsed from the string. */
  public static Integer internedInteger(String s) {
    return internedInteger(Integer.parseInt(s));
  }


  // I might prefer to have the intern methods first check using a straight
  // eq hashing, which would be more efficient if the array is already
  // interned.  (How frequent do I expect that to be, and how much would
  // that really improve performance even in that case?)

  /**
   * Intern (canonicalize) an int[].
   * Returns a canonical representation for the int[] array.
   * Arrays are compared according to their elements.
   */
  public static int[] intern(int[] a) {
    Object lookup = internedIntArrays.get(a);
    if (lookup != null) {
      WeakReference ref = (WeakReference)lookup;
      return (int[])ref.get();
    } else {
      internedIntArrays.put(a, new WeakReference(a));
      return a;
    }
  }

  /**
   * Intern (canonicalize) an Object[].
   * Returns a canonical representation for the Object[] array.
   * Arrays are compared according to their elements.
   * The elements should themselves already be interned;
   * they are compared using their equals() methods.
   */
  public static Object[] intern(Object[] a) {
    Object lookup = internedObjectArrays.get(a);
    if (lookup != null) {
      WeakReference ref = (WeakReference)lookup;
      return (Object[])ref.get();
    } else {
      internedObjectArrays.put(a, new WeakReference(a));
      return a;
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  /// Interning arrays:  old implementation #1
  ///

  /// Interning arrays:  old implmentation.
  /// The problem with this is that it doesn't release keys.
  // // I can also use java.util.Arrays.equals() to compare two arrays of base
  // // or Object type; but that doesn't do ordering.  (It does properly deal
  // // with the possibility that the argument is null, which this doesn't
  // // right now.  I may want to err in this implementation if the arguments
  // // are null or the lengths are not equal -- if I never mix arrays of
  // // different lengths.)

  // // Note: this comparator imposes orderings that are inconsistent with equals.
  // // That is, it may return 0 if the arrays are not equal (but do contain
  // // identical numbers).
  // static class IntArrayComparator implements Comparator {
  //   public int compare(Object o1, Object o2) {
  //     if (o1 == o2)
  //       return 0;
  //     int[] a1 = (int[])o1;
  //     int[] a2 = (int[])o2;
  //     int tmp;
  //     tmp = a1.length - a2.length;
  //     if (tmp != 0)
  //       return tmp;
  //     for (int i=0; i<a1.length; i++) {
  //       tmp = a1[i] - a2[i];
  //       if (tmp != 0)
  // 	return tmp;
  //     }
  //     return 0;
  //   }
  // }

  // // Note: this comparator imposes orderings that are inconsistent with equals.
  // // That is, it may return 0 if the arrays are not equal (but do contain
  // // identical objects).
  // static class ObjectArrayComparator implements Comparator {
  //   public int compare(Object o1, Object o2) {
  //     if (o1 == o2)
  //       return 0;
  //     Object[] a1 = (Object[])o1;
  //     Object[] a2 = (Object[])o2;
  //     int tmp;
  //     tmp = a1.length - a2.length;
  //     if (tmp != 0)
  //       return tmp;
  //     for (int i=0; i<a1.length; i++) {
  //       tmp = a1[i].hashCode() - a2[i].hashCode();
  //       if (tmp != 0)
  // 	return tmp;
  //       // I'm counting on the fact that hashCode returns a different
  //       // number for each Object in the system.  This checks that assumption.
  //       Assert.assert(a1[i].equals(a2[i]));
  //     }
  //     return 0;
  //   }
  // }

  // private static TreeSet internedIntArrays;
  // private static TreeSet internedObjectArrays;

  // static {
  //   internedIntArrays = new TreeSet(new IntArrayComparator());
  //   internedObjectArrays = new TreeSet(new ObjectArrayComparator());
  // }

  // public static int[] internIntArray(int[] a) {
  //   boolean added = internedIntArrays.add(a);
  //   if (added)
  //     return a;
  //   else
  //     return (int[])internedIntArrays.tailSet(a).first();
  // }

  // // All the elements should already themselves be interned
  // public static Object[] internObjectArray(Object[] a) {
  //   boolean added = internedObjectArrays.add(a);
  //   if (added)
  //     return a;
  //   else
  //     return (Object[])internedObjectArrays.tailSet(a).first();
  // }



  ///////////////////////////////////////////////////////////////////////////
  /// Interning arrays:  old implementation #2
  ///

  /// This doesn't work because there are no references to the Wrappers,
  /// so all of the WeakHashMap elements are immediately removed.

  // // Create an ArrayWrapper which redefines equal (and hash) to act the
  // // way I want them to.

  // static class IntArrayWrapper {
  //   private int[] a;
  //   IntArrayWrapper(int[] a_) {
  //     a = a_;
  //   }
  //   boolean equals(IntArrayWrapper other) {
  //     return java.util.Arrays.equals(a, other.a);
  //   }
  //   static final int FACTOR = 23;
  //   public int hashCode() {
  //     int result = 0;
  //     for (int i=0; i<a.length; i++) {
  //       result = result * FACTOR + a[i];
  //     }
  //     return result;
  //   }
  // }

  // static class ObjectArrayWrapper {
  //   private Object[] a;
  //   ObjectArrayWrapper(Object[] a_) {
  //     a = a_;
  //   }
  //   boolean equals(ObjectArrayWrapper other) {
  //     return java.util.Arrays.equals(a, other.a);
  //   }
  //   static final int FACTOR = 23;
  //   // Alternately, just xor all the element hash codes.
  //   public int hashCode() {
  //     int result = 0;
  //     for (int i=0; i<a.length; i++) {
  //       result = result * FACTOR + a[i].hashCode();
  //     }
  //     return result;
  //   }
  // }

  // // Map from an ArrayWrapper to the array (I don't need to map to a
  // // WeakReference because the array isn't the key of the WeakHashMap).

  // // non-private for debugging only
  // static WeakHashMap internedIntArrays;
  // static WeakHashMap internedObjectArrays;
  // // private static WeakHashMap internedIntArrays;
  // // private static WeakHashMap internedObjectArrays;

  // static {
  //   internedIntArrays = new WeakHashMap();
  //   internedObjectArrays = new WeakHashMap();
  // }

  // public static int[] internIntArray(int[] a) {
  //   IntArrayWrapper w = new IntArrayWrapper(a);
  //   Object result = internedIntArrays.get(w);
  //   if (result != null)
  //     return (int[])result;
  //   else {
  //     internedIntArrays.put(w, a);
  //     return a;
  //   }
  // }

  // // All the elements should already themselves be interned
  // public static Object[] internObjectArray(Object[] a) {
  //   ObjectArrayWrapper w = new ObjectArrayWrapper(a);
  //   Object result = internedObjectArrays.get(w);
  //   if (result != null)
  //     return (Object[])result;
  //   else {
  //     internedObjectArrays.put(w, a);
  //     return a;
  //   }
  // }

}
