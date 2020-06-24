package daikon.chicory;

import static java.lang.System.out;

import org.checkerframework.checker.nullness.qual.Nullable;

class ChicoryTest {

  int t1 = 55;

  public static void main(String[] args) {

    ChicoryTest t = new ChicoryTest();
    t.t1 = 5;
    int i = t.sample(0);
    out.format("sample return [35]   = %d%n", i);

    t = t.sample1();
    out.format("sample return [32]   = %d%n", t.t1);

    double d = t.sample2();
    out.println("sample return [62.4] = " + d);

    t.test_d(1.0, 5.0);
  }

  public ChicoryTest @Nullable [] test_array() {
    return null;
  }

  public double test_d(double d1, double d2) {
    return (d1 * d2);
  }

  public int sample(int myint) {

    double my_d = t1;

    if (t1 == 6) {
      return (int) my_d + 7;
    } else {
      return (7 * (int) my_d);
    }
  }

  public ChicoryTest sample1() {

    ChicoryTest test1 = new ChicoryTest();
    test1.t1 = 32;
    return test1;
  }

  public double sample2() {

    if (t1 == 6) {
      return 5.43;
    } else {
      return 62.4;
    }
  }
}
