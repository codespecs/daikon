package PolyCalc;

import junit.framework.*;
import org.junit.Test;

public class RatPolyTest extends TestCase {

  private RatNum[] num =
      new RatNum[] {
        new RatNum(0), new RatNum(1), new RatNum(2), new RatNum(3),
        new RatNum(4), new RatNum(5), new RatNum(6), new RatNum(7),
        new RatNum(8), new RatNum(9), new RatNum(10)
      };

  private RatNum nanNum = (new RatNum(1)).div(new RatNum(0));

  private RatPoly rp(int coef, int expt) {
    return new RatPoly(coef, expt);
  }

  private RatPoly parse(String s) {
    return RatPoly.parse(s);
  }

  // 0
  private RatPoly zero;
  // x^2 + 2*x
  private RatPoly _XSqPlus2X;
  // 2*x^2 + x
  private RatPoly _2XSqPlusX;
  private boolean isSetup = false;

  private void checkInitRatPolyFields() {
    if (!isSetup) {
      zero = new RatPoly();
      _XSqPlus2X = rp(1, 2).add(rp(1, 1)).add(rp(1, 1));
      _2XSqPlusX = rp(1, 2).add(rp(1, 2)).add(rp(1, 1));
      isSetup = true;
    }
  }

  public RatPolyTest(String name) {
    super(name);
    // System.err.println(name);
  }

  // only unparse is tested here
  private void eq(RatPoly p, String target) {
    String t = p.unparse();
    assertEquals(target, t);
  }

  private void eq(RatPoly p, String target, String message) {
    String t = p.unparse();
    assertEquals(message, target, t);
  }

  @Test
  public void testNoArgCtor() {
    eq(new RatPoly(), "0");
  }

  @Test
  public void testTwoArgCtor() {
    eq(rp(0, 0), "0");
    eq(rp(0, 1), "0");
    eq(rp(1, 0), "1");
    eq(rp(-1, 0), "-1");
    eq(rp(1, 1), "x");

    eq(rp(1, 2), "x^2");
    eq(rp(2, 2), "2*x^2");
    eq(rp(2, 3), "2*x^3");

    eq(rp(-2, 3), "-2*x^3");

    eq(rp(-1, 1), "-x");
    eq(rp(-1, 3), "-x^3");
  }

  @Test
  public void testDegree() {
    assertTrue("x^0 degree 0", rp(1, 0).degree() == 0);
    assertTrue("x^1 degree 1", rp(1, 1).degree() == 1);
    assertTrue("x^100 degree 100", rp(1, 100).degree() == 100);

    assertTrue("0*x^100 degree 0", rp(0, 100).degree() == 0);

    assertTrue("0*x^0 degree 0", rp(0, 0).degree() == 0);
  }

  @Test
  public void testAdd() {
    checkInitRatPolyFields();
    eq(rp(1, 0).add(rp(1, 0)), "2");
    eq(rp(1, 0).add(rp(5, 0)), "6");

    eq(rp(1, 1).add(rp(1, 1)), "2*x");
    eq(rp(1, 2).add(rp(1, 2)), "2*x^2");
    eq(rp(1, 2).add(rp(1, 1)), "x^2+x");

    eq(_XSqPlus2X, "x^2+2*x");
    eq(_2XSqPlusX, "2*x^2+x");

    eq(rp(1, 3).add(rp(1, 1)), "x^3+x");
  }

  @Test
  public void testSub() {
    eq(rp(1, 1).sub(rp(1, 0)), "x-1");

    eq(rp(1, 1).add(rp(1, 0)), "x+1");
  }

  @Test
  public void testMul() {
    eq(rp(0, 0).mul(rp(0, 0)), "0");
    eq(rp(1, 0).mul(rp(1, 0)), "1");
    eq(rp(1, 0).mul(rp(2, 0)), "2");
    eq(rp(2, 0).mul(rp(2, 0)), "4");
    eq(rp(1, 0).mul(rp(1, 1)), "x");

    eq(rp(1, 1).mul(rp(1, 1)), "x^2");

    eq(rp(1, 1).sub(rp(1, 0)).mul(rp(1, 1).add(rp(1, 0))), "x^2-1");
  }

  @Test
  public void testOpsWithNaN(RatPoly p) {
    RatPoly nan = RatPoly.parse("NaN");
    eq(p.add(nan), "NaN");
    eq(nan.add(p), "NaN");

    eq(p.sub(nan), "NaN");
    eq(nan.sub(p), "NaN");

    eq(p.mul(nan), "NaN");
    eq(nan.mul(p), "NaN");

    eq(p.div(nan), "NaN");
    eq(nan.div(p), "NaN");
  }

  @Test
  public void testOpsWithNaN() {
    testOpsWithNaN(rp(0, 0));
    testOpsWithNaN(rp(0, 1));
    testOpsWithNaN(rp(1, 0));
    testOpsWithNaN(rp(1, 1));

    testOpsWithNaN(rp(2, 0));
    testOpsWithNaN(rp(2, 1));
    testOpsWithNaN(rp(0, 2));
    testOpsWithNaN(rp(1, 2));
  }

  @Test
  public void testImmutabilityOfOperations() {
    // not the most thorough test possible, but hopefully will
    // catch the easy cases early on...
    RatPoly one = rp(1, 0);
    RatPoly two = rp(2, 0);

    one.degree();
    two.degree();
    eq(one, "1", "Degree mutates receiver!");
    eq(two, "2", "Degree mutates receiver!");

    one.coeff(0);
    two.coeff(0);
    eq(one, "1", "Coeff mutates receiver!");
    eq(two, "2", "Coeff mutates receiver!");

    one.isNaN();
    two.isNaN();
    eq(one, "1", "isNaN mutates receiver!");
    eq(two, "2", "isNaN mutates receiver!");

    one.eval(0.0);
    two.eval(0.0);
    eq(one, "1", "eval mutates receiver!");
    eq(two, "2", "eval mutates receiver!");

    one.negate();
    two.negate();
    eq(one, "1", "Negate mutates receiver!");
    eq(two, "2", "Negate mutates receiver!");

    one.add(two);
    eq(one, "1", "Add mutates receiver!");
    eq(two, "2", "Add mutates argument!");

    one.sub(two);
    eq(one, "1", "Sub mutates receiver!");
    eq(two, "2", "Sub mutates argument!");

    one.mul(two);
    eq(one, "1", "Mul mutates receiver!");
    eq(two, "2", "Mul mutates argument!");

    one.div(two);
    eq(one, "1", "Div mutates receiver!");
    eq(two, "2", "Div mutates argument!");
  }

  @Test
  public void testEval() {
    RatPoly zero = new RatPoly();
    RatPoly one = new RatPoly(1, 0);
    RatPoly _X = new RatPoly(1, 1);
    RatPoly _2X = new RatPoly(2, 1);
    RatPoly _XSq = new RatPoly(1, 2);

    assertEquals(" 0 at 0 ", 0.0, zero.eval(0.0), 0.0001);
    assertEquals(" 0 at 1 ", 0.0, zero.eval(1.0), 0.0001);
    assertEquals(" 0 at 2 ", 0.0, zero.eval(2.0), 0.0001);
    assertEquals(" 1 at 0 ", 1.0, one.eval(0.0), 0.0001);
    assertEquals(" 1 at 1 ", 1.0, one.eval(1.0), 0.0001);
    assertEquals(" 1 at 1 ", 1.0, one.eval(2.0), 0.0001);

    assertEquals(" x at 0 ", 0.0, _X.eval(0.0), 0.0001);
    assertEquals(" x at 1 ", 1.0, _X.eval(1.0), 0.0001);
    assertEquals(" x at 2 ", 2.0, _X.eval(2.0), 0.0001);

    assertEquals(" 2*x at 0 ", 0.0, _2X.eval(0.0), 0.0001);
    assertEquals(" 2*x at 1 ", 2.0, _2X.eval(1.0), 0.0001);
    assertEquals(" 2*x at 2 ", 4.0, _2X.eval(2.0), 0.0001);

    assertEquals(" x^2 at 0 ", 0.0, _XSq.eval(0.0), 0.0001);
    assertEquals(" x^2 at 1 ", 1.0, _XSq.eval(1.0), 0.0001);
    assertEquals(" x^2 at 2 ", 4.0, _XSq.eval(2.0), 0.0001);

    RatPoly _XSq_minus_2X = _XSq.sub(_2X);

    assertEquals(" x^2-2*x at 0 ", 0.0, _XSq_minus_2X.eval(0.0), 0.0001);
    assertEquals(" x^2-2*x at 1 ", -1.0, _XSq_minus_2X.eval(1.0), 0.0001);
    assertEquals(" x^2-2*x at 2 ", 0.0, _XSq_minus_2X.eval(2.0), 0.0001);
    assertEquals(" x^2-2*x at 3 ", 3.0, _XSq_minus_2X.eval(3.0), 0.0001);
  }

  // parses s into p, and then checks that it is as anticipATED
  // forall i, parse(s).coeff(expts[i]) = anticipCoeffForExpts(i)
  private void eqP(String s, int anticipDegree, int[] expts, RatNum[] anticipCoeffForExpts) {
    RatPoly p = parse(s);
    eqP(p, s, anticipDegree, expts, anticipCoeffForExpts);
  }
  // checks that 'p' is as anticipATED (unparse, degree, and coeff are tested)
  // forall i, parse(s).coeff(expts[i]) = anticipCoeffForExpts(i)
  private void eqP(
      RatPoly p, String s, int anticipDegree, int[] expts, RatNum[] anticipCoeffForExpts) {
    eq(p, s);
    assertTrue(p.degree() == anticipDegree);
    for (int i = 0; i < expts.length; i++) {
      assertTrue(
          "wrong coeff; \n"
              + "anticipated: "
              + anticipCoeffForExpts[i]
              + "; received: "
              + p.coeff(expts[i])
              + "\n"
              + " received: "
              + p
              + " anticipated:"
              + s,
          p.coeff(expts[i]).equals(anticipCoeffForExpts[i]));
    }
  }

  @Test
  public void testParseSimple() {
    eqP("0", 0, new int[] {0}, new RatNum[] {num[0]});
    eqP("x", 1, new int[] {1, 0}, new RatNum[] {num[1], num[0]});

    eqP("x^2", 2, new int[] {2, 1, 0}, new RatNum[] {num[1], num[0], num[0]});
  }

  @Test
  public void testParseMultTerms() {
    eqP("x^3+x^2", 3, new int[] {3, 2}, new RatNum[] {num[1], num[1]});
    eqP("x^3-x^2", 3, new int[] {3, 2}, new RatNum[] {num[1], num[1].negate()});
    eqP("x^100+x^2", 100, new int[] {100, 2}, new RatNum[] {num[1], num[1]});
  }

  @Test
  public void testParseLeadingNeg() {
    eqP("-x^2", 2, new int[] {2, 1, 0}, new RatNum[] {num[1].negate(), num[0], num[0]});
    eqP("-x^2+1", 2, new int[] {2, 1, 0}, new RatNum[] {num[1].negate(), num[0], num[1]});
    eqP("-x^2+x", 2, new int[] {2, 1, 0}, new RatNum[] {num[1].negate(), num[1], num[0]});
  }

  @Test
  public void testParseLeadingConstants() {
    eqP("10*x", 1, new int[] {2, 1, 0}, new RatNum[] {num[0], num[10], num[0]});

    eqP("10*x^100+x^2", 100, new int[] {100, 2, 0}, new RatNum[] {num[10], num[1], num[0]});

    eqP(
        "10*x^100+100*x^2",
        100,
        new int[] {100, 2, 0},
        new RatNum[] {num[10], new RatNum(100), num[0]});

    eqP(
        "-10*x^100+100*x^2",
        100,
        new int[] {100, 2, 0},
        new RatNum[] {num[10].negate(), new RatNum(100), num[0]});
  }

  @Test
  public void testParseRationals() {
    eqP("1/2", 0, new int[] {0}, new RatNum[] {num[1].div(num[2])});
    eqP("1/2*x", 1, new int[] {1}, new RatNum[] {num[1].div(num[2])});

    eqP("x+1/3", 1, new int[] {1, 0}, new RatNum[] {num[1], num[1].div(num[3])});

    eqP("1/2*x+1/3", 1, new int[] {1, 0}, new RatNum[] {num[1].div(num[2]), num[1].div(num[3])});
    eqP("1/2*x+3/2", 1, new int[] {1, 0}, new RatNum[] {num[1].div(num[2]), num[3].div(num[2])});
    eqP(
        "1/2*x^10+3/2",
        10,
        new int[] {10, 0},
        new RatNum[] {num[1].div(num[2]), num[3].div(num[2])});
    eqP(
        "1/2*x^10+3/2*x^2+1",
        10,
        new int[] {10, 2, 0},
        new RatNum[] {num[1].div(num[2]), num[3].div(num[2]), num[1]});
  }

  @Test
  public void testParseNaN() {
    eq(parse("NaN"), "NaN");
  }

  @Test
  public void testCoeff() {
    checkInitRatPolyFields();
    // coeff already gets some grunt testing in eqP; checking an interesting
    // input here...

    assertTrue(_XSqPlus2X.coeff(-1).equals(num[0]));
    assertTrue(_XSqPlus2X.coeff(-10).equals(num[0]));
    assertTrue(_2XSqPlusX.coeff(-1).equals(num[0]));
    assertTrue(_2XSqPlusX.coeff(-10).equals(num[0]));
    assertTrue(zero.coeff(-10).equals(num[0]));
    assertTrue(zero.coeff(-1).equals(num[0]));
  }

  @Test
  public void testDiv() {
    // 0/x = 0
    eq(rp(0, 1).div(rp(1, 1)), "0");

    // x/x = 1
    eq(rp(1, 1).div(rp(1, 1)), "1");

    // -x/x = -1
    eq(rp(-1, 1).div(rp(1, 1)), "-1");

    // x/-x = -1
    eq(rp(1, 1).div(rp(-1, 1)), "-1");

    // -x/-x = 1
    eq(rp(-1, 1).div(rp(-1, 1)), "1");

    // -x^2/x = -x
    eq(rp(-1, 2).div(rp(1, 1)), "-x");

    // x^100/x^1000 = 0
    eq(rp(1, 100).div(rp(1, 1000)), "0");

    // x^100/x = x^99
    eq(rp(1, 100).div(rp(1, 1)), "x^99");

    // x^99/x^98 = x
    eq(rp(1, 99).div(rp(1, 98)), "x");

    // x^10 / x = x^9 (r: 0)
    eq(rp(1, 10).div(rp(1, 1)), "x^9");

    // x^10 / x^3+x^2 = x^7-x^6+x^5-x^4+x^3-x^2+x-1  (r: -x^2)
    eq(rp(1, 10).div(rp(1, 3).add(rp(1, 2))), "x^7-x^6+x^5-x^4+x^3-x^2+x-1");

    // x^10 / x^3+x^2+x = x^7-x^6+x^4-x^3+x-1 (r: -x)
    eq(rp(1, 10).div(rp(1, 3).add(rp(1, 2).add(rp(1, 1)))), "x^7-x^6+x^4-x^3+x-1");

    // x^10+x^5 / x = x^9+x^4 (r: 0)
    eq(rp(1, 10).add(rp(1, 5)).div(rp(1, 1)), "x^9+x^4");

    // x^10+x^5 / x^3 = x^7+x^2 (r: 0)
    eq(rp(1, 10).add(rp(1, 5)).div(rp(1, 3)), "x^7+x^2");

    // x^10+x^5 / x^3+x+3 = x^7-x^5-3*x^4+x^3+7*x^2+8*x-10 (r: 29*x^2+14*x-30)
    eq(
        rp(1, 10).add(rp(1, 5)).div(rp(1, 3).add(rp(1, 1)).add(rp(3, 0))),
        "x^7-x^5-3*x^4+x^3+7*x^2+8*x-10");
  }

  @Test
  public void testDivComplexI() {
    // (x+1)*(x+1) = x^2+2*x+1
    eq(rp(1, 2).add(rp(2, 1)).add(rp(1, 0)).div(rp(1, 1).add(rp(1, 0))), "x+1");

    // (x-1)*(x+1) = x^2-1
    eq(rp(1, 2).add(rp(-1, 0)).div(rp(1, 1).add(rp(1, 0))), "x-1");
  }

  @Test
  public void testDivComplexII() {
    // x^8+2*x^6+8*x^5+2*x^4+17*x^3+11*x^2+8*x+3 =
    // (x^3+2*x+1) * (x^5+7*x^2+2*x+3)
    RatPoly large =
        rp(1, 8)
            .add(rp(2, 6))
            .add(rp(8, 5))
            .add(rp(2, 4))
            .add(rp(17, 3))
            .add(rp(11, 2))
            .add(rp(8, 1))
            .add(rp(3, 0));

    // x^3+2*x+1
    RatPoly sub1 = rp(1, 3).add(rp(2, 1)).add(rp(1, 0));
    // x^5+7*x^2+2*x+3
    RatPoly sub2 = rp(1, 5).add(rp(7, 2)).add(rp(2, 1)).add(rp(3, 0));

    // just a last minute typo check...
    eq(sub1.mul(sub2), large.unparse());
    eq(sub2.mul(sub1), large.unparse());

    eq(large.div(sub2), "x^3+2*x+1");
    eq(large.div(sub1), "x^5+7*x^2+2*x+3");
  }

  @Test
  public void testDivExamplesFromSpec() {
    // seperated this test case out because it has a dependency on
    // both "parse" and "div" functioning properly

    // example 1 from spec
    eq(parse("x^3-2*x+3").div(parse("3*x^2")), "1/3*x");
    // example 2 from spec
    eq(parse("x^2+2*x+15").div(parse("2*x^3")), "0");
  }

  @Test
  public void testDivExampleFromPset() {
    eq(
        parse("x^8+x^6+10*x^4+10*x^3+8*x^2+2*x+8").div(parse("3*x^6+5*x^4+9*x^2+4*x+8")),
        "1/3*x^2-2/9");
  }

  private void assertIsNaNanswer(RatPoly nanAnswer) {
    eq(nanAnswer, "NaN");
  }

  @Test
  public void testDivByZero() {
    checkInitRatPolyFields();

    RatPoly nanAnswer;
    nanAnswer = rp(1, 0).div(zero);
    assertIsNaNanswer(nanAnswer);

    nanAnswer = rp(1, 1).div(zero);
    assertIsNaNanswer(nanAnswer);
  }

  @Test
  public void testDivByPolyWithNaN() {
    checkInitRatPolyFields();

    RatPoly nan_x2 = rp(1, 2).mul(rp(1, 1).div(zero));
    RatPoly one_x1 = new RatPoly(1, 1);

    assertIsNaNanswer(nan_x2.div(one_x1));
    assertIsNaNanswer(one_x1.div(nan_x2));
    assertIsNaNanswer(nan_x2.div(zero));
    assertIsNaNanswer(zero.div(nan_x2));
    assertIsNaNanswer(nan_x2.div(nan_x2));
  }

  @Test
  public void testIsNaN() {
    assertTrue(RatPoly.parse("NaN").isNaN());

    assertTrue(!RatPoly.parse("1").isNaN());
    assertTrue(!RatPoly.parse("1/2").isNaN());
    assertTrue(!RatPoly.parse("x+1").isNaN());
    assertTrue(!RatPoly.parse("x^2+x+1").isNaN());
  }

  // Tell JUnit what order to run the tests in
  public static Test suite() {
    TestSuite suite = new TestSuite();
    suite.addTest(new RatPolyTest("testNoArgCtor"));
    suite.addTest(new RatPolyTest("testTwoArgCtor"));
    suite.addTest(new RatPolyTest("testDegree"));
    suite.addTest(new RatPolyTest("testAdd"));
    suite.addTest(new RatPolyTest("testSub"));
    suite.addTest(new RatPolyTest("testMul"));
    suite.addTest(new RatPolyTest("testOpsWithNaN"));
    suite.addTest(new RatPolyTest("testParseSimple"));
    suite.addTest(new RatPolyTest("testParseMultTerms"));
    suite.addTest(new RatPolyTest("testParseLeadingNeg"));
    suite.addTest(new RatPolyTest("testParseLeadingConstants"));
    suite.addTest(new RatPolyTest("testParseRationals"));
    suite.addTest(new RatPolyTest("testParseNaN"));
    suite.addTest(new RatPolyTest("testCoeff"));
    suite.addTest(new RatPolyTest("testDiv"));
    suite.addTest(new RatPolyTest("testDivComplexI"));
    suite.addTest(new RatPolyTest("testDivComplexII"));
    suite.addTest(new RatPolyTest("testDivExamplesFromSpec"));
    suite.addTest(new RatPolyTest("testDivExampleFromPset"));
    suite.addTest(new RatPolyTest("testDivByZero"));
    suite.addTest(new RatPolyTest("testDivByPolyWithNaN"));
    suite.addTest(new RatPolyTest("testIsNaN"));
    suite.addTest(new RatPolyTest("testImmutabilityOfOperations"));
    suite.addTest(new RatPolyTest("testEval"));
    return suite;
  }
}
