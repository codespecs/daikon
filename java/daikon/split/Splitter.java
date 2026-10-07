package daikon.split;

import daikon.Ppt;
import daikon.PptTopLevel;
import daikon.ValueTuple;
import daikon.inv.DummyInvariant;
import java.io.Serializable;
import org.checkerframework.checker.initialization.qual.UnknownInitialization;
import org.checkerframework.checker.interning.qual.UsesObjectEquals;
import org.checkerframework.checker.nullness.qual.Nullable;

/**
 * A Splitter represents a test that can be used to separate all samples into two parts. For
 * instance, a Splitter might represent the condition "x &gt; 0". The Splitter is used to divide a
 * collection of variable values into sub-sets. Invariant detection can then occur for the two
 * subsets independently.
 *
 * <p>This class Splitter is the superclass for all the classes we dynamically compile; there will
 * be one subclass for each condition that's checked. Other information about the splitting
 * condition is kept in a SplitterObject object, which keeps reference to the corresponding
 * Splitter. One instance of each Splitter subclass is then created for each program point at which
 * the splitting condition is applicable.
 */

// Should not be "implements Serializable":  the classes are created on
// demand, so the class doesn't exist when a serialized object is being
// re-read.
@UsesObjectEquals
public abstract class Splitter implements Serializable {
  static final long serialVersionUID = 20020122L;

  /**
   * Creates a splitter "factory" that should only be used for creating new copies via {@link
   * #instantiateSplitter(Ppt)}. (That is, the result of "new Splitter()" should not do any
   * splitting itself.) There is no need for subclasses to override this (but most will have to,
   * since they will add their own constructors as well).
   */
  protected Splitter() {}

  /**
   * Creates a valid splitter than can be used for testing the condition via test(ValueTuple). The
   * implementation should always set the "instantiated" protected field to true, if that field is
   * present in the Splitter class.
   */
  public abstract Splitter instantiateSplitter(@UnknownInitialization(Ppt.class) Ppt ppt);

  /** True for an instantiated (non-"factory") splitter. */
  protected boolean instantiated = false;

  /**
   * Returns true for an instantiated (non-"factory") splitter. Clients also need to check valid().
   */
  public boolean instantiated() {
    return instantiated;
  }

  /**
   * Returns true or false according to whether this was instantiated correctly and test(ValueTuple)
   * can be called without error. An alternate design would have {@link #instantiateSplitter(Ppt)}
   * check this, but it's a bit easier on implementers of subclasses of Splitter for the work to be
   * done (in just one place) by the caller.
   */
  public abstract boolean valid();

  /**
   * Returns true or false according to whether the values in the specified ValueTuple satisfy the
   * condition represented by this Splitter. Requires that valid() returns true.
   */
  public abstract boolean test(ValueTuple vt);

  // This method could be static; but don't bother making it so.
  /** Returns the condition being tested, as a String. */
  public abstract String condition();

  /**
   * Set up the static ('factory') DummyInvariant for this kind of splitter. This only modifies
   * static data, but it can't be static because subclasses must override it.
   */
  public void makeDummyInvariantFactory(DummyInvariant inv) {}

  /**
   * Make an instance DummyInvariant for this instance of the splitter, if possible on an
   * appropriate slice from ppt.
   */
  public void instantiateDummy(PptTopLevel ppt) {}

  /** On an instantiated Splitter, give back an appropriate instantiated DummyInvariant. */
  public abstract @Nullable DummyInvariant getDummyInvariant();

  // The methods below replace the daikon.Quant methods that access arrays, when those methods are
  // called in a splitting condition (see QuantFixer).  The daikon.Quant methods cannot be called
  // directly because a splitter represents an array differently than the program does: an
  // integral or boolean array as a long[] (or an index array as an int[]), a float or double array
  // as a double[], and a char array as a String.  Each method has the same semantics as the
  // daikon.Quant method of the same name, including its default value for a null array.  There is
  // a method only for each element type that the representation can hold, so a splitting
  // condition that applies a method to an array of a different type does not compile.

  /**
   * Like {@link daikon.Quant#size(Object)}.
   *
   * @param a an array, or null
   * @return the length of a, or Integer.MAX_VALUE if a is null
   */
  public static int size(long @Nullable [] a) {
    return (a == null) ? Integer.MAX_VALUE : a.length;
  }

  /**
   * Like {@link daikon.Quant#size(Object)}.
   *
   * @param a an array, or null
   * @return the length of a, or Integer.MAX_VALUE if a is null
   */
  public static int size(int @Nullable [] a) {
    return (a == null) ? Integer.MAX_VALUE : a.length;
  }

  /**
   * Like {@link daikon.Quant#size(Object)}.
   *
   * @param a an array, or null
   * @return the length of a, or Integer.MAX_VALUE if a is null
   */
  public static int size(double @Nullable [] a) {
    return (a == null) ? Integer.MAX_VALUE : a.length;
  }

  /**
   * Like {@link daikon.Quant#size(Object)}.
   *
   * @param a an array, or null
   * @return the length of a, or Integer.MAX_VALUE if a is null
   */
  public static int size(@Nullable String @Nullable [] a) {
    return (a == null) ? Integer.MAX_VALUE : a.length;
  }

  /**
   * Like {@link daikon.Quant#size(Object)}, for a char array represented as a String.
   *
   * @param a a char array represented as a String, or null
   * @return the length of a, or Integer.MAX_VALUE if a is null
   */
  public static int size(@Nullable String a) {
    return (a == null) ? Integer.MAX_VALUE : a.length();
  }

  /**
   * Like {@link daikon.Quant#getElement_boolean(boolean[], long)}.
   *
   * @param a a boolean array represented as a long[], or null
   * @param i an index into a
   * @return the ith element of a, or false if a is null
   */
  public static boolean getElement_boolean(long @Nullable [] a, long i) {
    return (a == null) ? false : a[(int) i] > 0;
  }

  /**
   * Like {@link daikon.Quant#getElement_byte(byte[], long)}.
   *
   * @param a a byte array represented as a long[], or null
   * @param i an index into a
   * @return the ith element of a, or Byte.MAX_VALUE if a is null
   */
  public static byte getElement_byte(long @Nullable [] a, long i) {
    return (a == null) ? Byte.MAX_VALUE : (byte) a[(int) i];
  }

  /**
   * Like {@link daikon.Quant#getElement_short(short[], long)}.
   *
   * @param a a short array represented as a long[], or null
   * @param i an index into a
   * @return the ith element of a, or Short.MAX_VALUE if a is null
   */
  public static short getElement_short(long @Nullable [] a, long i) {
    return (a == null) ? Short.MAX_VALUE : (short) a[(int) i];
  }

  /**
   * Like {@link daikon.Quant#getElement_int(int[], long)}.
   *
   * @param a an int array represented as a long[], or null
   * @param i an index into a
   * @return the ith element of a, or Integer.MAX_VALUE if a is null
   */
  public static int getElement_int(long @Nullable [] a, long i) {
    return (a == null) ? Integer.MAX_VALUE : (int) a[(int) i];
  }

  /**
   * Like {@link daikon.Quant#getElement_int(int[], long)}.
   *
   * @param a an index array, or null
   * @param i an index into a
   * @return the ith element of a, or Integer.MAX_VALUE if a is null
   */
  public static int getElement_int(int @Nullable [] a, long i) {
    return (a == null) ? Integer.MAX_VALUE : a[(int) i];
  }

  /**
   * Like {@link daikon.Quant#getElement_long(long[], long)}.
   *
   * @param a a long array, or null
   * @param i an index into a
   * @return the ith element of a, or Long.MAX_VALUE if a is null
   */
  public static long getElement_long(long @Nullable [] a, long i) {
    return (a == null) ? Long.MAX_VALUE : a[(int) i];
  }

  /**
   * Like {@link daikon.Quant#getElement_float(float[], long)}.
   *
   * @param a a float array represented as a double[], or null
   * @param i an index into a
   * @return the ith element of a, or Float.NaN if a is null
   */
  public static float getElement_float(double @Nullable [] a, long i) {
    return (a == null) ? Float.NaN : (float) a[(int) i];
  }

  /**
   * Like {@link daikon.Quant#getElement_double(double[], long)}.
   *
   * @param a a double array, or null
   * @param i an index into a
   * @return the ith element of a, or Double.NaN if a is null
   */
  public static double getElement_double(double @Nullable [] a, long i) {
    return (a == null) ? Double.NaN : a[(int) i];
  }

  /**
   * Like {@link daikon.Quant#getElement_char(char[], long)}.
   *
   * @param a a char array represented as a String, or null
   * @param i an index into a
   * @return the ith element of a, or Character.MAX_VALUE if a is null
   */
  public static char getElement_char(@Nullable String a, long i) {
    return (a == null) ? Character.MAX_VALUE : a.charAt((int) i);
  }

  /**
   * Like {@link daikon.Quant#getElement_String(String[], long)}.
   *
   * @param a a String array, or null
   * @param i an index into a
   * @return the ith element of a, or null if a is null
   */
  public static @Nullable String getElement_String(@Nullable String @Nullable [] a, long i) {
    return (a == null) ? null : a[(int) i];
  }

  /**
   * Like {@link daikon.Quant#getElement_Object(Object, long)}, for a String array. Daikon's Java
   * output format uses getElement_Object for an array whose elements are not primitives.
   *
   * @param a a String array, or null
   * @param i an index into a
   * @return the ith element of a, or null if a is null
   */
  public static @Nullable Object getElement_Object(@Nullable String @Nullable [] a, long i) {
    return getElement_String(a, i);
  }
}
