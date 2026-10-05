package misc;

/**
 * Regression test for GitHub issue 554.
 *
 * <p>DynComp computes comparability separately for each program point. Because the constructor
 * stores {@code ZERO_MAG} in {@code mag}, at the constructor's exit the index of {@code ZERO_MAG}
 * is comparable to {@code bitLength}, so Daikon derives {@code ZERO_MAG[this.bitLength]} there. At
 * the OBJECT program point they are not comparable, so that derived variable does not exist in
 * the parent. If Daikon required every derived variable to have a counterpart in the parent, it
 * would crash with "Can't find parent variable".
 */
public class Issue554 {

  public static final int[] ZERO_MAG = new int[0];

  public static final Issue554 ZERO = new Issue554(ZERO_MAG);

  public int[] mag;

  public int bitLength = -1;

  public Issue554(int[] mag) {
    this.mag = mag;
  }

  public int bitLength() {
    if (bitLength == -1) {
      bitLength = calcBitLength(0, mag);
    }
    return bitLength;
  }

  static int calcBitLength(int start, int[] mag) {
    int i = start;
    while (i < mag.length && mag[i] == 0) {
      i++;
    }
    return i;
  }

  public static void main(String[] args) {
    new Issue554(new int[] {1, 2}).bitLength();
    new Issue554(new int[] {3}).bitLength();
    ZERO.bitLength();
  }
}
