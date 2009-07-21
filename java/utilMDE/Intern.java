package utilMDE;

import java.lang.ref.WeakReference;
import java.util.*;

/**
 * Utilities for interning objects.
 **/
public final class Intern {
  private Intern() { throw new Error("do not instantiate"); }

  ///////////////////////////////////////////////////////////////////////////
  /// Strings
  ///

  /**
   * Replace each element of the array by its interned version.
   * Side-effects the array, but also returns it.
   * @see String#intern
   **/
  @SuppressWarnings("interning")
  public static /*@Interned*/ String[] internStrings(String[] a) {
    for (int i=0; i<a.length; i++)
      if (a[i] != null)
        a[i] = a[i].intern();
    return a;
  }


  ///////////////////////////////////////////////////////////////////////////
  /// Testing interning
  ///

  /**
   * Return true if the argument is interned (is canonical among all
   * objects equal to itself).
   **/
  @SuppressWarnings("interning")
  public static boolean isInterned(/*@Nullable*/ Object value) {
    if (value == null) {
      // nothing to do
      return true;
    } else if (value instanceof String) {
      return (value == ((String) value).intern());
    } else if (value instanceof String[]) {
      return (value == intern((String[]) value));
    } else if (value instanceof Integer) {
      return (value == intern((Integer) value));
    } else if (value instanceof Long) {
      return (value == intern((Long) value));
    } else if (value instanceof int[]) {
      return (value == intern((int[]) value));
    } else if (value instanceof long[]) {
      return (value == intern((long[]) value));
    } else if (value instanceof Double) {
      return (value == intern((Double) value));
    } else if (value instanceof double[]) {
      return (value == intern((double[]) value));
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
   **/
  private static final class IntegerHasher implements Hasher {
    public boolean equals(Object a1, Object a2) {
      return a1.equals(a2);
    }
    public int hashCode(Object o) {
      Integer i = (Integer) o;
      return i.intValue();
    }
  }

  /**
   * Hasher object which hashes and compares Longs.
   * This is the obvious implementation that uses intValue() for the hashCode.
   * @see Hasher
   **/
  private static final class LongHasher implements Hasher {
    public boolean equals(Object a1, Object a2) {
      return a1.equals(a2);
    }
    public int hashCode(Object o) {
      Long i = (Long) o;
      return i.intValue();
    }
  }

  /**
   * Hasher object which hashes and compares int[] objects according
   * to their contents.
   * @see Hasher
   * @see java.util.Arrays#equals(int[], int[])
   **/
  private static final class IntArrayHasher implements Hasher {
    public boolean equals(Object a1, Object a2) {
      return java.util.Arrays.equals((int[])a1, (int[])a2);
    }
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
   * Hasher object which hashes and compares long[] objects according
   * to their contents.
   * @see Hasher
   * @see java.util.Arrays#equals (long[], long[])
   **/
  private static final class LongArrayHasher implements Hasher {
    public boolean equals(Object a1, Object a2) {
      return java.util.Arrays.equals((long[])a1, (long[])a2);
    }
    public int hashCode(Object o) {
      long[] a = (long[])o;
      long result = 0;
      for (int i=0; i<a.length; i++) {
        result = result * FACTOR + a[i];
      }
      return (int) (result % Integer.MAX_VALUE);
    }
  }

  private static final int FACTOR = 23;
  // private static final double DOUBLE_FACTOR = 65537;
  private static final double DOUBLE_FACTOR = 263;

  /**
   * Hasher object which hashes and compares Doubles.
   * @see Hasher
   **/
  private static final class DoubleHasher implements Hasher {
    public boolean equals(Object a1, Object a2) {
      return a1.equals(a2);
    }
    public int hashCode(Object o) {
      Double d = (Double) o;
      // Could add "... % Integer.MAX_VALUE" here; is that good to do?
      long result = Math.round(d.doubleValue() * DOUBLE_FACTOR);
      return (int) (result % Integer.MAX_VALUE);
    }
  }

  /**
   * Hasher object which hashes and compares double[] objects according
   * to their contents.
   * @see Hasher
   * @see java.util.Arrays#equals(Object[],Object[])
   **/
  private static final class DoubleArrayHasher implements Hasher {
    public boolean equals(Object a1, Object a2) {
      // "java.util.Arrays.equals" considers +0.0 != -0.0.
      // Also, it gives inconsistent results (on different JVMs/classpaths?).
      // return java.util.Arrays.equals((double[])a1, (double[])a2);
      double[] da1 = (double[])a1;
      double[] da2 = (double[])a2;
      if (da1.length != da2.length)
        return false;
      for (int i=0; i<da1.length; i++) {
        if (! ((da1[i] == da2[i])
               || (Double.isNaN(da1[i]) && Double.isNaN(da2[i])))) {
          return false;
        }
      }
      return true;
    }
    public int hashCode(Object o) {
      double[] a = (double[])o;
      double running = 0;
      for (int i=0; i<a.length; i++) {
        double elt = (Double.isNaN(a[i]) ? 0.0 : a[i]);
        running = running * FACTOR + elt * DOUBLE_FACTOR;
      }
      // Could add "... % Integer.MAX_VALUE" here; is that good to do?
      long result = Math.round(running);
      return (int) (result % Integer.MAX_VALUE);
    }
  }

  /**
   * Hasher object which hashes and compares String[] objects according
   * to their contents.
   * @see Hasher
   * java.util.Arrays.equals
   **/
  private static final class StringArrayHasher implements Hasher {
    public boolean equals(Object a1, Object a2) {
      return java.util.Arrays.equals((String[])a1, (String[])a2);
    }
    public int hashCode(Object o) {
      String[] a = (String[])o;
      int result = 0;
      for (int i=0; i<a.length; i++) {
        int a_hashcode = (a[i] == null) ? 0 : a[i].hashCode();
        result = result * FACTOR + a_hashcode;
      }
      return result;
    }
  }

  /**
   * Hasher object which hashes and compares Object[] objects according
   * to their contents.
   * @see Hasher
   * @see java.util.Arrays#equals(Object[], Object[])
   **/
  private static final class ObjectArrayHasher implements Hasher {
    public boolean equals(Object a1, Object a2) {
      return java.util.Arrays.equals((/*@Nullable*/ Object[])a1, (/*@Nullable*/ Object[])a2);
    }
    public int hashCode(Object o) {
      /*@Nullable*/ Object[] a = (/*@Nullable*/ Object[])o;
      int result = 0;
      for (int i=0; i<a.length; i++) {
        Object elt = a[i];
        int elt_hashcode = (elt == null) ? 0 : elt.hashCode();
        result = result * FACTOR + elt_hashcode;
      }
      return result;
    }
  }

  // Each of these maps has:
  //   key = an interned object
  //   value = a WeakReference for the object itself.
  // They can be looked up using a non-interned value; equality tests know
  // nothing of the interning types.

  private static WeakHasherMap</*@Interned*/ Integer,WeakReference</*@Interned*/ Integer>> internedIntegers;
  private static WeakHasherMap</*@Interned*/ Long,WeakReference</*@Interned*/ Long>> internedLongs;
  private static WeakHasherMap<int /*@Interned*/ [],WeakReference<int /*@Interned*/ []>> internedIntArrays;
  private static WeakHasherMap<long /*@Interned*/ [],WeakReference<long /*@Interned*/ []>> internedLongArrays;
  private static WeakHasherMap</*@Interned*/ Double,WeakReference</*@Interned*/ Double>> internedDoubles;
  private static /*@Interned*/ Double internedDoubleNaN;
  private static /*@Interned*/ Double internedDoubleZero;
  private static WeakHasherMap<double /*@Interned*/ [],WeakReference<double /*@Interned*/ []>> internedDoubleArrays;
  private static WeakHasherMap</*@Nullable*/ /*@Interned*/ String /*@Interned*/ [],WeakReference</*@Nullable*/ /*@Interned*/ String /*@Interned*/ []>> internedStringArrays;
  private static WeakHasherMap</*@Nullable*/ /*@Interned*/ Object /*@Interned*/ [],WeakReference</*@Nullable*/ /*@Interned*/ Object /*@Interned*/ []>> internedObjectArrays;
  private static WeakHasherMap<SequenceAndIndices<int /*@Interned*/ []>,WeakReference<int /*@Interned*/ []>> internedIntSequenceAndIndices;
  private static WeakHasherMap<SequenceAndIndices<long /*@Interned*/ []>,WeakReference<long /*@Interned*/ []>> internedLongSequenceAndIndices;
  private static WeakHasherMap<SequenceAndIndices<double /*@Interned*/ []>,WeakReference<double /*@Interned*/ []>> internedDoubleSequenceAndIndices;
  private static WeakHasherMap<SequenceAndIndices</*@Nullable*/ /*@Interned*/ Object /*@Interned*/ []>,WeakReference</*@Nullable*/ /*@Interned*/ Object /*@Interned*/ []>> internedObjectSequenceAndIndices;
  private static WeakHasherMap<SequenceAndIndices</*@Nullable*/ /*@Interned*/ String /*@Interned*/ []>,WeakReference</*@Nullable*/ /*@Interned*/ String /*@Interned*/ []>> internedStringSequenceAndIndices;

  static {
    internedIntegers = new WeakHasherMap</*@Interned*/ Integer,WeakReference</*@Interned*/ Integer>>(new IntegerHasher());
    internedLongs = new WeakHasherMap</*@Interned*/ Long,WeakReference</*@Interned*/ Long>>(new LongHasher());
    internedIntArrays = new WeakHasherMap<int /*@Interned*/ [],WeakReference<int /*@Interned*/ []>>(new IntArrayHasher());
    internedLongArrays = new WeakHasherMap<long /*@Interned*/ [],WeakReference<long /*@Interned*/ []>>(new LongArrayHasher());
    internedDoubles = new WeakHasherMap</*@Interned*/ Double,WeakReference</*@Interned*/ Double>>(new DoubleHasher());
    internedDoubleNaN = new /*@Interned*/ Double(Double.NaN);
    internedDoubleZero = new /*@Interned*/ Double(0);
    internedDoubleArrays = new WeakHasherMap<double /*@Interned*/ [],WeakReference<double /*@Interned*/ []>>(new DoubleArrayHasher());
    internedStringArrays = new WeakHasherMap</*@Nullable*/ /*@Interned*/ String /*@Interned*/ [],WeakReference</*@Nullable*/ /*@Interned*/ String /*@Interned*/ []>>(new StringArrayHasher());
    internedObjectArrays = new WeakHasherMap</*@Nullable*/ /*@Interned*/ Object /*@Interned*/ [],WeakReference</*@Nullable*/ /*@Interned*/ Object /*@Interned*/ []>>(new ObjectArrayHasher());
    internedIntSequenceAndIndices = new WeakHasherMap<SequenceAndIndices<int /*@Interned*/ []>,WeakReference<int /*@Interned*/ []>>(new SequenceAndIndicesHasher<int /*@Interned*/ []>());
    internedLongSequenceAndIndices = new WeakHasherMap<SequenceAndIndices<long /*@Interned*/ []>,WeakReference<long /*@Interned*/ []>>(new SequenceAndIndicesHasher<long /*@Interned*/ []>());
    internedDoubleSequenceAndIndices = new WeakHasherMap<SequenceAndIndices<double /*@Interned*/ []>,WeakReference<double /*@Interned*/ []>>(new SequenceAndIndicesHasher<double /*@Interned*/ []>());
    internedObjectSequenceAndIndices = new WeakHasherMap<SequenceAndIndices</*@Nullable*/ /*@Interned*/ Object /*@Interned*/ []>,WeakReference</*@Nullable*/ /*@Interned*/ Object /*@Interned*/ []>>(new SequenceAndIndicesHasher</*@Nullable*/ /*@Interned*/ Object /*@Interned*/ []>());
    internedStringSequenceAndIndices = new WeakHasherMap<SequenceAndIndices</*@Nullable*/ /*@Interned*/ String /*@Interned*/ []>,WeakReference</*@Nullable*/ /*@Interned*/ String /*@Interned*/ []>>(new SequenceAndIndicesHasher</*@Nullable*/ /*@Interned*/ String /*@Interned*/ []>());
  }

  // For testing only
  public static int numIntegers() { return internedIntegers.size(); }
  public static int numLongs() { return internedLongs.size(); }
  public static int numIntArrays() { return internedIntArrays.size(); }
  public static int numLongArrays() { return internedLongArrays.size(); }
  public static int numDoubles() { return internedDoubles.size(); }
  public static int numDoubleArrays() { return internedDoubleArrays.size(); }
  public static int numStringArrays() { return internedStringArrays.size(); }
  public static int numObjectArrays() { return internedObjectArrays.size(); }
  public static Iterator</*@Interned*/ Integer> integers() { return internedIntegers.keySet().iterator(); }
  public static Iterator</*@Interned*/ Long> longs() { return internedLongs.keySet().iterator(); }
  public static Iterator<int /*@Interned*/ []> intArrays() { return internedIntArrays.keySet().iterator(); }
  public static Iterator<long /*@Interned*/ []> longArrays() { return internedLongArrays.keySet().iterator(); }
  public static Iterator</*@Interned*/ Double> doubles() { return internedDoubles.keySet().iterator(); }
  public static Iterator<double /*@Interned*/ []> doubleArrays() { return internedDoubleArrays.keySet().iterator(); }
  public static Iterator</*@Nullable*/ /*@Interned*/ String /*@Interned*/ []> stringArrays() { return internedStringArrays.keySet().iterator(); }
  public static Iterator</*@Nullable*/ /*@Interned*/ Object /*@Interned*/ []> objectArrays() { return internedObjectArrays.keySet().iterator(); }

  // Interns a String.
  // Delegates to the builtin String.intern() method.  Provided for
  // completeness, so we can intern() any type used in OneOf.java.jpp.
  public static /*@Interned*/ /*@PolyNull*/ String intern(/*@PolyNull*/ String a) {
    return (a == null) ? null : a.intern();
  }

  // Interns a long.
  // A no-op.  Provided for completeness, so we can intern() any type
  // used in OneOf.java.jpp.
  public static long intern(long l) {
    return l;
  }

  // Interns a long.
  // A no-op.  Provided for completeness, so we can intern() any type
  // used in OneOf.java.jpp.
  public static double intern(double l) {
    return l;
  }

  /**
   * Intern (canonicalize) an Integer.
   * Returns a canonical representation for the Integer.
   **/
  // TODO: JLS 5.1.7 requires that the boxing conversion interns integer
  // values between -128 and 127 (and Intern.valueOf is intended to promise
  // the same).  This does not currently take advantage of that.
  @SuppressWarnings("interning")
  public static /*@Interned*/ Integer intern(Integer a) {
    WeakReference</*@Interned*/ Integer> lookup = internedIntegers.get(a);
    if (lookup != null) {
      return lookup.get();
    } else {
      @SuppressWarnings("cast") // cast is redundant (except in JSR 308)
      /*@Interned*/ Integer result = (/*@Interned*/ Integer) a;
      internedIntegers.put(result, new WeakReference</*@Interned*/ Integer>(result));
      return result;
    }
  }

  // Not sure whether this convenience method is really worth it.
  /** Returns an interned Integer with value i. */
  public static /*@Interned*/ Integer internedInteger(int i) {
    return intern(Integer.valueOf(i));
  }

  // Not sure whether this convenience method is really worth it.
  /** Returns an interned Integer with value parsed from the string. */
  public static /*@Interned*/ Integer internedInteger(String s) {
    return intern(Integer.decode(s));
  }


  /**
   * Intern (canonicalize) a Long.
   * Returns a canonical representation for the Long.
   **/
  // TODO: JLS 5.1.7 requires that the boxing conversion interns integer
  // values between -128 and 127 (and Long.valueOf is intended to promise
  // the same).  This could take advantage of that.
  @SuppressWarnings("interning")
  public static /*@Interned*/ Long intern(Long a) {
    WeakReference</*@Interned*/ Long> lookup =  internedLongs.get(a);
    if (lookup != null) {
      return lookup.get();
    } else {
      @SuppressWarnings("cast") // cast is redundant (except in JSR 308)
      /*@Interned*/ Long result = (/*@Interned*/ Long) a;
      internedLongs.put(result, new WeakReference</*@Interned*/ Long>(result));
      return result;
    }
  }

  // Not sure whether this convenience method is really worth it.
  /** Returns an interned Long with value i. */
  public static /*@Interned*/ Long internedLong(long i) {
    return intern(Long.valueOf(i));
  }

  // Not sure whether this convenience method is really worth it.
  /** Returns an interned Long with value parsed from the string. */
  public static /*@Interned*/ Long internedLong(String s) {
    return intern(Long.decode(s));
  }


  // I might prefer to have the intern methods first check using a straight
  // eq hashing, which would be more efficient if the array is already
  // interned.  (How frequent do I expect that to be, and how much would
  // that really improve performance even in that case?)

  /**
   * Intern (canonicalize) an int[].
   * Returns a canonical representation for the int[] array.
   * Arrays are compared according to their elements.
   **/
  @SuppressWarnings("interning")
  public static int /*@Interned*/ [] intern(int[] a) {
    // Throwable stack = new Throwable("debug traceback");
    // stack.fillInStackTrace();
    // stack.printStackTrace();

    WeakReference<int /*@Interned*/ []> lookup = internedIntArrays.get(a);
    if (lookup != null) {
      return lookup.get();
    } else {
      @SuppressWarnings("cast") // cast is redundant (except in JSR 308)
      /*@Interned*/ int[] result = (int /*@Interned*/ []) a;
      internedIntArrays.put(result, new WeakReference<int /*@Interned*/ []>(result));
      return result;
    }
  }

  /**
   * Intern (canonicalize) a long[].
   * Returns a canonical representation for the long[] array.
   * Arrays are compared according to their elements.
   **/
  @SuppressWarnings("interning")
  public static long /*@Interned*/ [] intern(long[] a) {
    //System.out.printf ("intern %s %s long[] %s\n", a.getClass(),
    //                   a, Arrays.toString (a));
    WeakReference<long /*@Interned*/ []> lookup = internedLongArrays.get(a);
    if (lookup != null) {
      return lookup.get();
    } else {
      @SuppressWarnings("cast") // cast is redundant (except in JSR 308)
      /*@Interned*/ long[] result = (long /*@Interned*/ []) a;
      internedLongArrays.put(result, new WeakReference<long /*@Interned*/ []>(result));
      return result;
    }
  }

  /**
   * Intern (canonicalize) a Double.
   * Returns a canonical representation for the Double.
   **/
  // TODO: JLS 5.1.7 requires that the boxing conversion interns integer
  // values between -128 and 127 (and Double.valueOf is intended to promise
  // the same).  This could take advantage of that.
  @SuppressWarnings("interning")
  public static /*@Interned*/ Double intern(Double a) {
    // Double.NaN == Double.Nan  always evaluates to false.
    if (a.isNaN())
      return internedDoubleNaN;
    // Double.+0 == Double.-0,  but they compare true via equals()
    if (a.doubleValue() == 0)   // catches both positive and negative zero
      return internedDoubleZero;
    WeakReference</*@Interned*/ Double> lookup = internedDoubles.get(a);
    if (lookup != null) {
      return lookup.get();
    } else {
      @SuppressWarnings("cast") // cast is redundant (except in JSR 308)
      /*@Interned*/ Double result = (/*@Interned*/ Double) a;
      internedDoubles.put(result, new WeakReference</*@Interned*/ Double>(result));
      return result;
    }
  }

  // Not sure whether this convenience method is really worth it.
  /** Returns an interned Double with value i. */
  public static /*@Interned*/ Double internedDouble(double d) {
    return intern(Double.valueOf(d));
  }

  // Not sure whether this convenience method is really worth it.
  /** Returns an interned Double with value parsed from the string. */
  public static /*@Interned*/ Double internedDouble(String s) {
    return internedDouble(Double.parseDouble(s));
  }


  // I might prefer to have the intern methods first check using a straight
  // eq hashing, which would be more efficient if the array is already
  // interned.  (How frequent do I expect that to be, and how much would
  // that really improve performance even in that case?)

  /**
   * Intern (canonicalize) a double[].
   * Returns a canonical representation for the double[] array.
   * Arrays are compared according to their elements.
   **/
  @SuppressWarnings("interning")
  public static double /*@Interned*/ [] intern(double[] a) {
    WeakReference<double /*@Interned*/ []> lookup = internedDoubleArrays.get(a);
    if (lookup != null) {
      return lookup.get();
    } else {
      @SuppressWarnings("cast") // cast is redundant (except in JSR 308)
      /*@Interned*/ double[] result = (double /*@Interned*/ []) a;
      internedDoubleArrays.put(result, new WeakReference<double /*@Interned*/ []>(result));
      return result;
    }
  }

  /**
   * Intern (canonicalize) an String[].
   * Returns a canonical representation for the String[] array.
   * Arrays are compared according to their elements.
   * The elements should themselves already be interned;
   * they are compared using their equals() methods.
   **/
  @SuppressWarnings("interning")
  public static /*@PolyNull*/ /*@Interned*/ String /*@Interned*/ [] intern(/*@PolyNull*/ /*@Interned*/ String[] a) {

    // Make sure each element is already interned
    for (int k = 0; k < a.length; k++)
      assert a[k] == Intern.intern (a[k]);

    // PolyNull because the value = parameter a, so the type is the same as
    // for parameter a
    @SuppressWarnings("nullness")
    WeakReference</*@PolyNull*/ /*@Interned*/ String /*@Interned*/ []> lookup = internedStringArrays.get(a);
    if (lookup != null) {
      return lookup.get();
    } else {
      @SuppressWarnings("cast") // cast is redundant (except in JSR 308)
      /*@PolyNull*/ /*@Interned*/ String /*@Interned*/ [] result = (/*@PolyNull*/ /*@Interned*/ String /*@Interned*/ []) a;
      internedStringArrays.put(result, new WeakReference</*@Nullable*/ /*@Interned*/ String /*@Interned*/ []>(result));
      return result;
    }
  }

  /**
   * Intern (canonicalize) an Object[].
   * Returns a canonical representation for the Object[] array.
   * Arrays are compared according to their elements.
   * The elements should themselves already be interned;
   * they are compared using their equals() methods.
   **/
  @SuppressWarnings({"interning"})
  public static /*@PolyNull*/ /*@Interned*/ Object /*@Interned*/ [] intern(/*@PolyNull*/ /*@Interned*/ Object[] a) {
    // PolyNull because the value = parameter a, so the type is the same as
    // for parameter a
    @SuppressWarnings("nullness")
    WeakReference</*@PolyNull*/ /*@Interned*/ Object /*@Interned*/ []> lookup = internedObjectArrays.get(a);
    if (lookup != null) {
      return lookup.get();
    } else {
      @SuppressWarnings("cast") // cast is redundant (except in JSR 308)
      /*@PolyNull*/ /*@Interned*/ Object /*@Interned*/ [] result = (/*@PolyNull*/ /*@Interned*/ Object /*@Interned*/ []) a;
      internedObjectArrays.put(result, new WeakReference</*@Nullable*/ /*@Interned*/ Object /*@Interned*/ []>(result));
      return result;
    }
  }


  /**
   * Convenince method to intern an Object when we don't know its
   * runtime type.  Its runtime type must be one of the types for
   * which we have an intern() method, else an exception is thrown.
   * If the argument is an array, its elements should themselves be
   * interned.
   **/
  public static /*@Interned*/ /*@PolyNull*/ Object intern(/*@PolyNull*/ Object a) {
    if (a == null) {
      return null;
    } else if (a instanceof String) {
      return intern((String) a);
    } else if (a instanceof String[]) {
      @SuppressWarnings("interning")
      /*@Interned*/ String[] asArray = (/*@Interned*/ String[]) a;
      return intern(asArray);
    } else if (a instanceof Integer) {
      return intern((Integer) a);
    } else if (a instanceof Long) {
      return intern((Long) a);
    } else if (a instanceof int[]) {
      return intern((int[]) a);
    } else if (a instanceof long[]) {
      return intern((long[]) a);
    } else if (a instanceof Double) {
      return intern((Double) a);
    } else if (a instanceof double[]) {
      return intern((double[]) a);
    } else if (a instanceof Object[]) {
      @SuppressWarnings("interning")
      /*@Interned*/ Object[] asArray = (/*@Interned*/ Object[]) a;
      return intern(asArray);
    } else {
      throw new IllegalArgumentException
        ("Arguments of type " + a.getClass() + " cannot be interned");
    }
  }

  /**
   * Return the subsequence of seq from start (inclusive) to end
   * (exclusive) that is interned.  What's different about this method
   * from manually finding the subsequence and interning the
   * subsequence is that if the subsequence is already interned, we
   * can avoid having to compute the sequence.  Since derived
   * variables in Daikon compute the subsequence many times, this
   * shortcut saves quite a bit of computation.  It saves even more
   * when there may be many derived variables that are non-canonical,
   * since they are guaranteed to be ==.
   * <p>
   * Requires that seq is already interned.
   * @return a subsequence of seq from start to end that is interned.
   **/
  public static int /*@Interned*/ [] internSubsequence (int /*@Interned*/ [] seq, int start, int end) {
    assert Intern.isInterned(seq);
    SequenceAndIndices<int /*@Interned*/ []> sai = new SequenceAndIndices<int /*@Interned*/ []> (seq, start, end);
    WeakReference<int /*@Interned*/ []> lookup = internedIntSequenceAndIndices.get(sai);
    if (lookup != null) {
      return lookup.get();
    } else {
      int[] subseqUninterned = ArraysMDE.subarray(seq, start, end - start);
      int /*@Interned*/ [] subseq = Intern.intern (subseqUninterned);
      internedIntSequenceAndIndices.put (sai, new WeakReference<int /*@Interned*/ []>(subseq));
      return subseq;
    }
  }

  /**
   * @see #internSubsequence(int[], int, int)
   **/
  public static long /*@Interned*/ [] internSubsequence (long /*@Interned*/ [] seq, int start, int end) {
    assert Intern.isInterned(seq);
    SequenceAndIndices<long /*@Interned*/ []> sai = new SequenceAndIndices<long /*@Interned*/ []> (seq, start, end);
    WeakReference<long /*@Interned*/ []> lookup = internedLongSequenceAndIndices.get(sai);
    if (lookup != null) {
      return lookup.get();
    } else {
      long[] subseq_uninterned = ArraysMDE.subarray(seq, start, end - start);
      long /*@Interned*/ [] subseq = Intern.intern (subseq_uninterned);
      internedLongSequenceAndIndices.put (sai, new WeakReference<long /*@Interned*/ []>(subseq));
      return subseq;
    }
  }

  /**
   * @see #internSubsequence(int[], int, int)
   **/
  public static double /*@Interned*/ [] internSubsequence (double /*@Interned*/ [] seq, int start, int end) {
    assert Intern.isInterned(seq);
    SequenceAndIndices<double /*@Interned*/ []> sai = new SequenceAndIndices<double /*@Interned*/ []> (seq, start, end);
    WeakReference<double /*@Interned*/ []> lookup = internedDoubleSequenceAndIndices.get(sai);
    if (lookup != null) {
      return lookup.get();
    } else {
      double[] subseq_uninterned = ArraysMDE.subarray(seq, start, end - start);
      double /*@Interned*/ [] subseq = Intern.intern (subseq_uninterned);
      internedDoubleSequenceAndIndices.put (sai, new WeakReference<double /*@Interned*/ []>(subseq));
      return subseq;
    }
  }

  /**
   * @see #internSubsequence(int[], int, int)
   **/
  public static /*@PolyNull*/ /*@Interned*/ Object /*@Interned*/ [] internSubsequence (/*@PolyNull*/ /*@Interned*/ Object /*@Interned*/ [] seq, int start, int end) {
    assert Intern.isInterned(seq);
    SequenceAndIndices</*@PolyNull*/ /*@Interned*/ Object /*@Interned*/ []> sai = new SequenceAndIndices</*@PolyNull*/ /*@Interned*/ Object /*@Interned*/ []> (seq, start, end);
    @SuppressWarnings("nullness")                   // same nullness as key
    WeakReference</*@PolyNull*/ /*@Interned*/ Object /*@Interned*/ []> lookup = internedObjectSequenceAndIndices.get(sai);
    if (lookup != null) {
      return lookup.get();
    } else {
      /*@PolyNull*/ /*@Interned*/ Object[] subseq_uninterned = ArraysMDE.subarray(seq, start, end - start);
      /*@PolyNull*/ /*@Interned*/ Object /*@Interned*/ [] subseq = Intern.intern (subseq_uninterned);
      @SuppressWarnings("nullness") Object ignore = // safe because map does no side effects
      internedObjectSequenceAndIndices.put (sai, new WeakReference</*@PolyNull*/ /*@Interned*/ Object /*@Interned*/ []>(subseq));
      return subseq;
    }
  }

  /**
   * @see #internSubsequence(int[], int, int)
   **/
  public static /*@PolyNull*/ /*@Interned*/ String /*@Interned*/ [] internSubsequence (/*@PolyNull*/ /*@Interned*/ String /*@Interned*/ [] seq, int start, int end) {
    assert Intern.isInterned(seq);
    SequenceAndIndices</*@PolyNull*/ /*@Interned*/ String /*@Interned*/ []> sai = new SequenceAndIndices</*@PolyNull*/ /*@Interned*/ String /*@Interned*/ []> (seq, start, end);
    @SuppressWarnings("nullness")                   // same nullness as key
    WeakReference</*@PolyNull*/ /*@Interned*/ String /*@Interned*/ []> lookup = internedStringSequenceAndIndices.get(sai);
    if (lookup != null) {
      return lookup.get();
    } else {
      /*@PolyNull*/ /*@Interned*/ String[] subseq_uninterned = ArraysMDE.subarray(seq, start, end - start);
      /*@PolyNull*/ /*@Interned*/ String /*@Interned*/ [] subseq = Intern.intern (subseq_uninterned);
      @SuppressWarnings("nullness") Object ignore = // safe because map does no side effects
      internedStringSequenceAndIndices.put (sai, new WeakReference</*@PolyNull*/ /*@Interned*/ String /*@Interned*/ []>(subseq));
      return subseq;
    }
  }

  /**
   * Data structure for storing triples of a sequence and start and
   * end indices, to represent a subsequence.  Requires that the
   * sequence be interned.  Used for interning the repeated finding
   * of subsequences on the same sequence.
   **/
  private static final class SequenceAndIndices<T extends /*@Interned*/ Object> {
    public T seq;
    public int start;
    public int end;

    /**
     * @param seq An interned array
     **/
    public SequenceAndIndices (T seq, int start, int end) {
      this.seq = seq;
      this.start = start;
      this.end = end;
      assert isInterned(seq);
    }

    @SuppressWarnings("unchecked")
    public boolean equals (/*@Nullable*/ Object other) {
      if (other instanceof SequenceAndIndices<?>) {
        @SuppressWarnings("unchecked")
        SequenceAndIndices<T> other_sai = (SequenceAndIndices<T>) other;
        return equals(other_sai);
      } else {
        return false;
      }
    }

    public boolean equals (SequenceAndIndices<T> other) {
      return ((this.seq == other.seq)
              && this.start == other.start
              && this.end == other.end);
    }

    public int hashCode() {
      return seq.hashCode() + start * 30 - end * 2;
    }

    // For debugging
    public String toString() {
      return "SAI(" + start + "," + end + ") from: " + ArraysMDE.toString(seq);
    }

  }

  /**
   * Hasher object which hashes and compares String[] objects according
   * to their contents.
   * @see Hasher
   **/
  private static final class SequenceAndIndicesHasher<T extends /*@Interned*/ Object> implements Hasher {
    public boolean equals(Object a1, Object a2) {
      @SuppressWarnings("unchecked")
      SequenceAndIndices<T> sai1 = (SequenceAndIndices<T>) a1;
      @SuppressWarnings("unchecked")
      SequenceAndIndices<T> sai2 = (SequenceAndIndices<T>) a2;
      // The SAI objects are *not* interned, but the arrays inside them are.
      return sai1.equals(sai2);
    }

    public int hashCode(Object o) {
      return o.hashCode();
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
  // static final class IntArrayComparator implements Comparator {
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
  //         return tmp;
  //     }
  //     return 0;
  //   }
  // }

  // // Note: this comparator imposes orderings that are inconsistent with equals.
  // // That is, it may return 0 if the arrays are not equal (but do contain
  // // identical objects).
  // static final class ObjectArrayComparator implements Comparator {
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
  //         return tmp;
  //       // I'm counting on the fact that hashCode returns a different
  //       // number for each Object in the system.  This checks that assumption.
  //       assert a1[i].equals(a2[i]);
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

  // static final class IntArrayWrapper {
  //   private int[] a;
  //   IntArrayWrapper(int[] a) {
  //     this.a = a;
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

  // static final class ObjectArrayWrapper {
  //   private Object[] a;
  //   ObjectArrayWrapper(Object[] a) {
  //     this.a = a;
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
