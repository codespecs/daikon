package daikon.test.dcomp;

/**
 * Test data for {@link daikon.dcomp.DCInstrumentTest24}. Its {@link #localVariableAccesses} method
 * loads and stores a local variable of every type. The test rewrites those instructions into their
 * wide (two-byte operand) forms, which javac never emits for so few local variables.
 */
public class WideOpcodeSample24 {

  /** Creates a WideOpcodeSample24. */
  public WideOpcodeSample24() {}

  /**
   * Copies each parameter into a local variable, then reads that local variable back. Thus, this
   * method contains a load and a store of a local variable of each of the five types that the load
   * and store instructions distinguish.
   *
   * @param i an int
   * @param l a long
   * @param f a float
   * @param d a double
   * @param o an Object
   * @return an arbitrary function of the arguments
   */
  public static int localVariableAccesses(int i, long l, float f, double d, Object o) {
    int i2 = i;
    long l2 = l;
    float f2 = f;
    double d2 = d;
    Object o2 = o;
    return i2 + (int) l2 + (int) f2 + (int) d2 + (o2 == null ? 0 : 1);
  }
}
