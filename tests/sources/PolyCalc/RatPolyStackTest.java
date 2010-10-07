package PolyCalc;

import junit.framework.*;

public class RatPolyStackTest extends TestCase {
    private RatPoly zero = new RatPoly(0,0);
    private RatPoly one = new RatPoly(1,0);
    private RatPoly two = new RatPoly(2,0);
    private RatPoly three = new RatPoly(3,0);
    private RatPoly four = new RatPoly(4,0);

    private RatPoly five = new RatPoly(5,0);
    private RatPoly six = new RatPoly(6,0);
    private RatPoly seven = new RatPoly(7,0);
    private RatPoly eight = new RatPoly(8,0);
    private RatPoly nine = new RatPoly(9,0);


    private RatPoly poly;

    // scratch stacks; note that stk1 is a default argument; if you
    // see an operation but no stack, its getting applied to stk1.
    private RatPolyStack stk1, stk2;

    //* @return a new RatPolyStack instance
    private RatPolyStack stack() { return new RatPolyStack(); }

    private RatPolyStack stack3() {
	RatPolyStack s = new RatPolyStack();
	s.push(three);
	return s;
    }
    private RatPolyStack stack23() {
	RatPolyStack s = stack3();
	s.push(two);
	return s;
    }
    private RatPolyStack stack123() {
	RatPolyStack s = stack23();
	s.push(one);
	return s;
    }
    private RatPolyStack stack112() {
	RatPolyStack s = new RatPolyStack();
	s.push(two);
	s.push(one);
	s.push(one);
	return s;
    }
    private RatPolyStack stack57() {
	RatPolyStack s = new RatPolyStack();
	s.push(seven);
	s.push(five);
	return s;
    }
    private RatPolyStack stack5723() {
	RatPolyStack s = new RatPolyStack();
	s.push(three);
	s.push(two);
	s.push(seven);
	s.push(five);
	return s;
    }


    // RatPoly equality check
    // (getting around non-definition of RatPoly.equals)
    private boolean eqv( RatPoly p1, RatPoly p2 ) {
	return p1.unparse().equals(p2.unparse());
    }

    // helper function (assumes compare to stk1)
    private void assertStkIs( String desc ) {assertStkIs(stk1,desc);}

    // compares 's' to a string describing its values
    // thus stack123 = "123".  desc MUST be a sequence of
    // decimal number chars
    private void assertStkIs( RatPolyStack s, String desc ) {
	assertTrue(s.size() == desc.length());
    polys:
	for(int i=0; i<desc.length(); i++) {
	    RatPoly p = s.get(i);
	    char c = desc.charAt(i);
	    String asstr =
		"Elem("+i+"): "+p.unparse()+
		", Expected "+c+
		", (Expected Stack:"+desc+") Compared against poly: ";

	    switch(c) {
	    case '0':
		assertTrue( asstr+zero.unparse(), eqv( p, zero ));
		continue polys;
	    case '1':
		assertTrue( asstr+one.unparse(), eqv( p, one ));
		continue polys;
	    case '2':
		assertTrue( asstr+two.unparse(), eqv( p, two ));
		continue polys;
	    case '3':
		assertTrue( asstr+three.unparse(), eqv( p, three ));
		continue polys;
	    case '4':
		assertTrue( asstr+four.unparse(), eqv( p, four ));
		continue polys;
	    case '5':
		assertTrue( asstr+five.unparse(), eqv( p, five ));
		continue polys;
	    case '6':
		assertTrue( asstr+six.unparse(), eqv( p, six ));
		continue polys;
	    case '7':
		assertTrue( asstr+seven.unparse(), eqv( p, seven ));
		continue polys;
	    case '8':
		assertTrue( asstr+eight.unparse(), eqv( p, eight ));
		continue polys;
	    case '9':
		assertTrue( asstr+nine.unparse(), eqv( p, nine ));
		continue polys;
	    default:
		fail("internal test inconsistency");
	    }
	}
    }

    public RatPolyStackTest( String name ) {
      super(name);
      // System.err.println(name);
    }

    public void testCtor() {
	stk1 = stack();
	assertTrue( stk1.size() == 0 );
    }

    public void testPush() {
	stk1 = stack();
	stk1.push(zero);

	assertStkIs( "0" );

	stk1.push(zero);
	assertStkIs( "00" );

	stk1.push(one);
	assertStkIs( "100");

	stk1 = stack3();
	assertStkIs( "3" );

	stk1 = stack23();
	assertStkIs( "23" );

	stk1 = stack123();
	assertStkIs( "123" );
    }

    public void testPushCheckForSharingTwixtStacks() {
	stk1 = stack();
	stk2 = stack123();
	assertStkIs( stk1, "" );
	assertStkIs( stk2, "123" );

	stk1.push(zero);
	assertStkIs( stk1, "0" );
	assertStkIs( stk2, "123" );

	stk1.push(zero);
	assertStkIs( stk1, "00" );
	assertStkIs( stk2, "123" );

	stk1.push(one);
	assertStkIs( stk1, "100" );
	assertStkIs( stk2, "123" );

	stk2.push(eight);
	assertStkIs( stk1, "100" );
	assertStkIs( stk2, "8123" );
    }

    public void testPop() {
	stk1 = stack123();

	poly = stk1.pop();
	assertTrue( eqv( poly, one ));
	assertStkIs( "23" );

	poly = stk1.pop();
	assertTrue( eqv( poly, two ));
	assertStkIs( "3" );

	poly = stk1.pop();
	assertStkIs( "" );
    }

    public void testDup() {
	stk1 = stack3();
	stk1.dup();
	assertStkIs( "33" );

	stk1 = stack23();
	stk1.dup();
	assertStkIs( "223" );
	assertTrue( stk1.size() == 3 );
	assertTrue( eqv( stk1.get(0), two ));
	assertTrue( eqv( stk1.get(1), two ));
	assertTrue( eqv( stk1.get(2), three ));

	stk1 = stack123();
	stk1.dup();
	assertStkIs( "1123" );

    }

    public void testSwap() {
	stk1 = stack23();
	stk1.swap();
	assertStkIs( "32" );

	stk1 = stack123();
	stk1.swap();
	assertStkIs( "213" );

	stk1 = stack112();
	stk1.swap();
	assertStkIs( "112" );
    }

    public void testClear() {
	stk1 = stack123();
	stk1.clear();
	assertStkIs( "" );
	stk2 = stack112();
	stk2.clear();
	assertStkIs( "" );
    }

    public void testAdd() {
	stk1 = stack123();
	stk1.add();
	assertStkIs( "33" );
	stk1.add();
	assertStkIs( "6" );

	stk1 = stack112();
	stk1.add();
	assertStkIs( "22" );
	stk1.add();
	assertStkIs( "4" );
	stk1.push( five );
	assertStkIs( "54" );
	stk1.add();
	assertStkIs( "9" );

    }

    public void testSub() {
	stk1 = stack123();
	stk1.sub();
	assertStkIs( "13" );
	stk1.sub();
	assertStkIs( "2" );

	stk1 = stack5723();
	stk1.sub();
	assertStkIs( "223" );
	stk1.sub();
	assertStkIs( "03" );
	stk1.sub();
	assertStkIs( "3" );
    }

    public void testMul() {
	stk1 = stack123();
	stk1.mul();
	assertStkIs( "23" );
	stk1.mul();
	assertStkIs( "6" );

	stk1 = stack112();
	stk1.mul();
	assertStkIs( "12" );
	stk1.mul();
	assertStkIs( "2" );
	stk1.push( four );
	assertStkIs( "42" );
	stk1.mul();
	assertStkIs( "8" );
    }

    public void testDiv() {
	stk1 = stack123();
	stk1.div();
	assertStkIs( "23" );

	stk1.push( six );
	stk1.push( three );
	assertStkIs( "3623" );
	stk1.div();
	assertStkIs( "223" );
	stk1.div();
	assertStkIs( "13" );
	stk1.div();
	assertStkIs( "3" );

    }

  // Tell JUnit what order to run the tests in
  public static Test suite()
  {
    TestSuite suite = new TestSuite();
    suite.addTest(new RatPolyStackTest("testCtor"));
    suite.addTest(new RatPolyStackTest("testPush"));
    suite.addTest(new RatPolyStackTest("testPushCheckForSharingTwixtStacks"));
    suite.addTest(new RatPolyStackTest("testPop"));
    suite.addTest(new RatPolyStackTest("testDup"));
    suite.addTest(new RatPolyStackTest("testSwap"));
    suite.addTest(new RatPolyStackTest("testClear"));
    suite.addTest(new RatPolyStackTest("testAdd"));
    suite.addTest(new RatPolyStackTest("testSub"));
    suite.addTest(new RatPolyStackTest("testMul"));
    suite.addTest(new RatPolyStackTest("testDiv"));
    return suite;
  }

}
