package PolyCalc;

import junit.framework.*;

public class PublicTest extends TestSuite
{
  // This seems not to have the same effect as desired.
  public static void main(String[] args) {
    run_directly();
  }

  // Toggles behavior between original version and PAG-augmented version
  private static final boolean use_original = false;

  public static void run_directly() {
    // addTestSuite(RatNumTest.class);
    RatNumTest rn;
    rn = new RatNumTest("testOneArgCtor"); rn.testOneArgCtor();
    rn = new RatNumTest("testTwoArgCtor"); rn.testTwoArgCtor();
    rn = new RatNumTest("testTwoArgCtorOnNaN"); rn.testTwoArgCtorOnNaN();
    rn = new RatNumTest("testReduction"); rn.testReduction();
    rn = new RatNumTest("testApprox"); rn.testApprox();
    rn = new RatNumTest("testAddSimple"); rn.testAddSimple();
    rn = new RatNumTest("testAddComplex"); rn.testAddComplex();
    rn = new RatNumTest("testAddImproper"); rn.testAddImproper();
    rn = new RatNumTest("testAddOnNaN"); rn.testAddOnNaN();
    rn = new RatNumTest("testAddTransitively"); rn.testAddTransitively();
    rn = new RatNumTest("testSubSimple"); rn.testSubSimple();
    rn = new RatNumTest("testSubComplex"); rn.testSubComplex();
    rn = new RatNumTest("testSubImproper"); rn.testSubImproper();
    rn = new RatNumTest("testSubOnNaN"); rn.testSubOnNaN();
    rn = new RatNumTest("testSubTransitively"); rn.testSubTransitively();
    rn = new RatNumTest("testMulProperties"); rn.testMulProperties();
    rn = new RatNumTest("testMulSimple"); rn.testMulSimple();
    rn = new RatNumTest("testMulComplex"); rn.testMulComplex();
    rn = new RatNumTest("testMulImproper"); rn.testMulImproper();
    rn = new RatNumTest("testMulOnNaN"); rn.testMulOnNaN();
    rn = new RatNumTest("testMulTransitively"); rn.testMulTransitively();
    rn = new RatNumTest("testDivSimple"); rn.testDivSimple();
    rn = new RatNumTest("testDivComplex"); rn.testDivComplex();
    rn = new RatNumTest("testDivImproper"); rn.testDivImproper();
    rn = new RatNumTest("testDivOnNaN"); rn.testDivOnNaN();
    rn = new RatNumTest("testDivTransitively"); rn.testDivTransitively();
    rn = new RatNumTest("testNegate"); rn.testNegate();
    rn = new RatNumTest("testParse"); rn.testParse();
    rn = new RatNumTest("testEqualsReflexive"); rn.testEqualsReflexive();
    rn = new RatNumTest("testEquals"); rn.testEquals();
    rn = new RatNumTest("testCompareToReflexive"); rn.testCompareToReflexive();
    rn = new RatNumTest("testCompareToNonFract"); rn.testCompareToNonFract();
    rn = new RatNumTest("testCompareToFract"); rn.testCompareToFract();
    rn = new RatNumTest("testCompareToNaNs"); rn.testCompareToNaNs();
    rn = new RatNumTest("testIsPosAndIsNeg"); rn.testIsPosAndIsNeg();
    rn = new RatNumTest("testIsNaN"); rn.testIsNaN();

    if (! use_original) {
      // addTestSuite(RatNumTest2.class);
      RatNumTest2 rn2;
      rn2 = new RatNumTest2("testLots"); rn2.testLots();
    }

    // addTestSuite(RatPolyTest.class);
    RatPolyTest rp;
    rp = new RatPolyTest("testNoArgCtor"); rp.testNoArgCtor();
    rp = new RatPolyTest("testTwoArgCtor"); rp.testTwoArgCtor();
    rp = new RatPolyTest("testDegree"); rp.testDegree();
    rp = new RatPolyTest("testAdd"); rp.testAdd();
    rp = new RatPolyTest("testSub"); rp.testSub();
    rp = new RatPolyTest("testMul"); rp.testMul();
    rp = new RatPolyTest("testOpsWithNaN"); rp.testOpsWithNaN();
    rp = new RatPolyTest("testImmutabilityOfOperations"); rp.testImmutabilityOfOperations();
    rp = new RatPolyTest("testEval"); rp.testEval();
    rp = new RatPolyTest("testParseSimple"); rp.testParseSimple();
    rp = new RatPolyTest("testParseMultTerms"); rp.testParseMultTerms();
    rp = new RatPolyTest("testParseLeadingNeg"); rp.testParseLeadingNeg();
    rp = new RatPolyTest("testParseLeadingConstants"); rp.testParseLeadingConstants();
    rp = new RatPolyTest("testParseRationals"); rp.testParseRationals();
    rp = new RatPolyTest("testParseNaN"); rp.testParseNaN();
    rp = new RatPolyTest("testCoeff"); rp.testCoeff();
    rp = new RatPolyTest("testDiv"); rp.testDiv();
    rp = new RatPolyTest("testDivComplexI"); rp.testDivComplexI();
    rp = new RatPolyTest("testDivComplexII"); rp.testDivComplexII();
    rp = new RatPolyTest("testDivExamplesFromSpec"); rp.testDivExamplesFromSpec();
    rp = new RatPolyTest("testDivExampleFromPset"); rp.testDivExampleFromPset();
    rp = new RatPolyTest("testDivByZero"); rp.testDivByZero();
    rp = new RatPolyTest("testDivByPolyWithNaN"); rp.testDivByPolyWithNaN();
    rp = new RatPolyTest("testIsNaN"); rp.testIsNaN();

    // addTestSuite(RatPolyStackTest.class);
    RatPolyStackTest rps;
    rps = new RatPolyStackTest("testCtor"); rps.testCtor();
    rps = new RatPolyStackTest("testPush"); rps.testPush();
    rps = new RatPolyStackTest("testPushCheckForSharingTwixtStacks"); rps.testPushCheckForSharingTwixtStacks();
    rps = new RatPolyStackTest("testPop"); rps.testPop();
    rps = new RatPolyStackTest("testDup"); rps.testDup();
    rps = new RatPolyStackTest("testSwap"); rps.testSwap();
    rps = new RatPolyStackTest("testClear"); rps.testClear();
    rps = new RatPolyStackTest("testAdd"); rps.testAdd();
    rps = new RatPolyStackTest("testSub"); rps.testSub();
    rps = new RatPolyStackTest("testMul"); rps.testMul();
    rps = new RatPolyStackTest("testDiv"); rps.testDiv();

    if (! use_original) {
      // extra stuff
      PublicTest2.run();
    }
  }

  public static Test suite() { return new PublicTest(); }
  public PublicTest() { this("Problem Set 1 Public Test"); }
  public PublicTest(String s)
  {
    super(s);
    addTestSuite(RatNumTest.class);
    if (! use_original) {
      addTestSuite(RatNumTest2.class);
    }
    addTestSuite(RatPolyTest.class);
    addTestSuite(RatPolyStackTest.class);
  }

}
