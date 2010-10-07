package PolyCalc;

public class PublicTest2
{
  public static void run()
  {
    // System.out.println("PublicTest2");

    RatNum n;
    RatPoly x;
    RatTerm t;
    RatTermVec v;

    //  ========== RatPoly
    //  coeff
    //  kill result denom 1,2,3... then >= 1
    x = RatPoly.parse("1/9");
    x.coeff(0);
    for (int i=0; i<10; i++) {
      x = x.negate().add(RatPoly.parse("2")).mul(RatPoly.parse("2/3"));
    }
    x = RatPoly.parse("NaN");
    x.coeff(0);
    x.negate();

    //  hintedGet
    //  kill /*# ensures \result.coeff.denom == 1 || \result.coeff.denom == 3 */
    //  replaceExpt
    //  kill /*# requires term.coeff.denom == 1 || term.coeff.denom == 3 || term.coeff.denom == 9 */
    RatPoly.parse("7*x^8+1/34*x^6+17*x^4+1/29*x^3+11*x^2+3*x+5").div(RatPoly.parse("3*x^6+1/5*x^4+9*x^2+4*x+8"));

    //  ========= RatTermVec
    //  insert
    //  /*# requires t.coeff.denom == 1 */
    //  /*# requires index == 1 || index == 2 || index == 3 */
    //  //@ requires index >= 0
    //  //@ requires t != null
    RatPoly.parse("7*x^8+34*x^6+17*x^4+29*x^3+11*x^2+3*x+5").add(RatPoly.parse("3*x^6+5*x^4+9*x^2+4*x+8"));
    //  /*# requires t.expt >= index */
    v = new RatTermVec();
    t = new RatTerm(RatNum.parse("0"), 0);
    v.insert(t, 0);
    v.insert(t, 1);
    //  get
    //    result coeff != 0
    v.get(0);

    //  size
    //  //@ \result >= 0
    v = new RatTermVec();
    for (int i=0; i<300; i++) {
      v.size();
    }
    v = new RatTermVec();
    //  toString (debugPrint) // != null
    for (int i=0; i<20; i++) {
      v.addElement(new RatTerm(RatNum.parse((((i & 1) == 0) ? "-" : "")+(i+2)+"/"+(i*3)), (i*7 % 19)));
      v.toString();
    }

    v = new RatTermVec();
    //  insert
    //  denom == 1
    //  index 1,2,3   // also toString coverage
    v.insert(new RatTerm(RatNum.parse("1/9"), 0), 0); v.toString();
    v.insert(new RatTerm(RatNum.parse("1/8"), 1), 1); v.toString();
    v.insert(new RatTerm(RatNum.parse("1/7"), 2), 2); v.toString();
    v.insert(new RatTerm(RatNum.parse("1/6"), 3), 3); v.toString();
    v.insert(new RatTerm(RatNum.parse("5"), 4), 4); v.toString();
    v.insert(new RatTerm(RatNum.parse("0"), 5), 5); v.toString();
    //  index >= 0 desired
    v = new RatTermVec();
    t = new RatTerm(RatNum.parse("0"), 0);
    for (int i=0; i<100; i++) {
      v.insert(t, 0);
    }

    //  set
    //  /*# requires t.coeff.denom == 1 || t.coeff.denom == 3 || t.coeff.denom == 9 */

    //  ========== RatNum
    // toString 1 2 or 3
    n = RatNum.parse("1/9");
    n.toString();
    n = RatNum.parse("NaN");
    n.toString();
    // parse numer >= -1
    n = RatNum.parse("-111/911");
  }
}
