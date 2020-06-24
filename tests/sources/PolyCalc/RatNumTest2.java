package PolyCalc;

import java.util.*;

public class RatNumTest2 extends RatNumTest {

  public RatNumTest2(String name) {
    super(name);
  }

  private ArrayList rs = new ArrayList();

  private static Random rnd = new Random(1978);

  private static int rnd() {
    return rnd.nextInt() % 19781978;
  }

  @Test
  public void testLots() {
    rs.add(new RatNum(1));
    int n = 30;
    for (int i = 1; i < n; i++) {
      for (int j = 1; j < n; j++) {
        RatNum r = new RatNum(rnd(), rnd());
        observe(r);
        rs.add(r);
        RatNum z = new RatNum(rnd(), 0);
        observe(r);
        rs.add(z);
      }
    }
  }

  private void observe(RatNum r) {
    RatNum other = (RatNum) rs.get(Math.abs(rnd()) % rs.size());

    r.isNaN();
    r.isNegative();
    r.isPositive();
    r.compareTo(other);
    r.approx();
    r.unparse();
    r.negate();
    r.add(other);
    r.sub(other);
    r.mul(other);
    r.div(other);
  }
}
