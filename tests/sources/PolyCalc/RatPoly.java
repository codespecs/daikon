package PolyCalc;

import java.util.StringTokenizer;

import junit.framework.Assert;

/** RatPoly represents an immutable single-variate polynomial
    expression.  RatPolys have rational coefficients and integer
    exponents.  Examples of RatPolys include "x^3-1/2*x^2+5/3*x+3",
    "x-10", and "0".  RatPolys may also have NaN as a coefficient.
*/
public class RatPoly {

    // holds terms of this
    private RatTermVec terms;

    // Definitions:
    // For a RatPoly p, let C(p,i) be "p.terms.get(i).coeff" and
    //                      E(p,i) be "p.terms.get(i).expt"
    //                      length(p) be "p.terms.size()"

    // (( These are helper functions that will make it easier for us
    // to write the remainder of the specifications.  They are not
    // executable code; they just represent complex expressions in a
    // concise manner, so that we can stress the important parts of
    // other expressions in the spec rather than get bogged down in
    // the details of how we extract the coefficient for the 2nd term
    // or the exponent for the 5th term.  So when you see C(p,i),
    // think "coefficient for the ith term in p" ))

    // Abstraction Function:
    // A RatPoly p is the Sum, from i=0 to length(p), of C(p,i)*x^E(p,i)

    // (( This explains what the state of the fields in a RatPoly
    // represents: it is the sum of a series of terms, forming an
    // expression like "C_0 + C_1*x^1 + C_2*x^2 + ..." If there are no
    // terms, then the RatPoly represents the zero polynomial ))

    // Rep. Invariant for every RatPoly p:
    // terms != null &&
    // forall i such that (0 <= i < length(p), C(p,i) != 0 &&
    // forall i such that (0 <= i < length(p), E(p,i) >= 0 &&
    // forall i such that (0 <= i < length(p) - 1), E(p,i) > E(p, i+1)

    // (( This tells us three important facts about every RatPoly: the
    // terms field always points to some usable object, no term in a
    // RatPoly has a negative exponent or a zero coefficient, and the
    // terms in a RatPoly are sorted according to descending exponent
    // order.

    /** @effects: Constructs a new Poly, "0".
     */
    public RatPoly() {
        // IMPL IS STAFF ONLY
        terms = new RatTermVec();
        checkRep();
    }

    /** @requires: e >= 0
        @effects: Constructs a new Poly, "c * x^e".
     */
    public RatPoly(int c, int e) {
        // IMPL IS STAFF ONLY
        terms = new RatTermVec();
        if (c != 0)
            terms.addElement( new RatTerm(new RatNum(c), e) );
        checkRep();
    }

    /** @requires: 'rt' satisfies clauses given in rep. invariant
        @effects: Constructs a new Poly using 'rt' (does not make a copy)
    */
    private RatPoly(RatTermVec rt) {
        // IMPL IS STAFF ONLY
        terms = rt;
        checkRep();
    }

    /** @return true if and only if this has some coefficient = "NaN".
     */
    public boolean isNaN() {
        checkRep();
        for(int i=0; i<terms.size(); i++) {
            if (terms.get(i).coeff.isNaN()) {
                checkRep();
                return true;
            }
        }
        checkRep();
        return false;
    }

    /** Returns the degree of this.
        @return the largest exponent with a non-zero coefficient, or 0
                if this = "0".
     */
    public int degree() {
        // IMPL IS STAFF ONLY
        checkRep();
        if (terms.size() > 0) {
            checkRep();
            return terms.get(0).expt;
        } else {
            checkRep();
            return 0;
        }
    }

    /** @return the coefficient associated with term of degree 'deg'.
        If there is no term of degree 'deg' in this poly, then returns
        zero.
    */
    public RatNum coeff(int deg) {
        // IMPL IS STAFF ONLY
        checkRep();
        for(int i=0; i<terms.size(); i++) {
            RatTerm t = terms.get(i);
            if (t.expt == deg) {
                checkRep();
                return t.coeff;
            }
        }
        checkRep();
        return new RatNum(0);
    }

    /** @return a String representation of the expression represented
        by this, with the terms sorted in order of degree from highest
        to lowest.

        Terms with zero coefficients do not appear in the returned
        string, unless the polynomial is itself zero, in which case
        the returned string will just be "0."

        The string is in the form "(-)T(+|-)T(+|-)...",where for each
        term, T takes the form "C*x^E" or "C*x", UNLESS: (1) the
        exponent E is zero, in which case T takes the form "C", or (2)
        the coefficient C is one, in which case T takes the form "x^E"
        or "x"

        Note that this format allows only the first term to be output
        as a negative expression.

        Valid example outputs include "x^17-3/2*x^2+1", "-x+1", "-1/2",
        and "0".

        (FSK: formal BNF grammar below, not intended for students, but
        rather for the enterprising TA who wants to make the above
        description clearer)

        P ::= 0 | S | - S
        S ::= T + S | T - S | T
        T ::= K | L * x E | x E
        K ::= 1 | L
        L ::= K | K / N
        N ::= 2 | 3 | 4 | ...
        E ::= ^L | <empty>

    */
    public String unparse() {
        // IMPL IS STAFF ONLY
        checkRep();
        if (terms.size() == 0) {
            checkRep();
            return "0";
        }

        StringBuffer sb = new StringBuffer();

        // first term, special case to omit initial '+'
        RatTerm term = terms.get(0);
        if (term.coeff.isNegative()) {
            sb.append("-");
            term = new RatTerm( term.coeff.negate(), term.expt );
        }
        appendTerm(sb, term);

        for(int i=1; i<terms.size(); i++) {
            RatTerm rt = terms.get(i);
            RatNum num = rt.coeff;
            if (num.equals(new RatNum(0))) {
                continue; // do nothing;
            } else if (num.isNegative()) {
                sb.append("-");
                num = num.negate();
            } else {
                sb.append("+");
            }
            appendTerm(sb, new RatTerm( num, rt.expt ));
        }

        if (sb.length() == 0)
            sb.append('0');

        checkRep();
        return sb.toString();
    }

    // METHOD IS STAFF ONLY
    // appendTerm is a helper method, not given in spec
    // Let c = rt.coeff and e = rt.expt
    // If c is zero then does nothing.
    // Else, does the following in series:
    //    1) if c != 1 or e = 0 then appends c to sb else does nothing
    //    2) if c != 1 and e != 0 then appends "*" to sb
    //    3) if e > 1 then appends ("x^"+e) to sb.
    //    4) if e == 1 then appends ("x") to sb.
    private static void appendTerm(StringBuffer sb, RatTerm rt) {
        RatNum c = rt.coeff;
        int e = rt.expt;
        if (c.equals( new RatNum(0) ) ) {
            return;
        }

        if (e == 0 || !c.equals( new RatNum(1) ) ) {
            sb.append(c.unparse());
        }

        if (e != 0) {
            if (!c.equals( new RatNum(1) )) {
                sb.append('*');
            }
            sb.append('x');
            if (e != 1) {
                sb.append('^');
                sb.append(e);
            }
        }
    }

    // METHOD IS STAFF ONLY
    private RatPoly scaleBy(RatTerm scalar) {
        checkRep();
        RatTermVec vec = this.terms.copy();
        scaleCoeff(vec, scalar.coeff);
        incremExpt(vec, scalar.expt );
        checkRep();
        return new RatPoly(vec);
    }

    /** Scales coefficients within 'vec' by 'scalar' (helper procedure).
        @requires: vec, scalar != null
        @modifies: vec
        @effects: forall i s.t. 0 <= i < vec.size(),
                     if (C . E) = vec.get(i)
                     then vec_post.get(i) = (C*scalar . E)
    */
    private static void scaleCoeff(RatTermVec vec, RatNum scalar) {
        // IMPL IS STAFF ONLY
        for(int i=0; i<vec.size(); i++) {
            RatTerm rt = vec.get(i);
            RatTerm replace = new RatTerm( rt.coeff.mul(scalar), rt.expt );
            vec.set( replace, i );
        }
    }

    /** Increments exponents within 'vec' by 'degree' (helper procedure).
        @requires: vec != null
        @modifies: vec
        @effects: forall i s.t. 0 <= i < vec.size(),
                     if (C . E) = vec.get(i)
                     then vec_post.get(i) = (C . E*degree)
     */
    private static void incremExpt(RatTermVec vec, int degree) {
        // IMPL IS STAFF ONLY
        for(int i=0; i<vec.size(); i++) {
            RatTerm rt = vec.get(i);
            RatTerm replace = new RatTerm( rt.coeff, rt.expt+degree );
            vec.set( replace, i );
        }
    }

    /** Merges a term into a sequence of terms, preserving the
        sorted nature of the sequence (helper procedure).

        Definitions:
        Let a "Sorted RatTermVec" be a RatTermVec V such that
        [1] V is sorted in descending exponent order &&
        [2] there are no two RatTerms with the same exponent in V &&
        [3] there is no RatTerm in V with a coefficient equal to zero

        For a Sorted(RatTermVec) V and integer e, let cofind(V, e)
        be either the coefficient for a RatTerm rt in V whose
        exponent is e, or zero if there does not exist any such
        RatTerm in V.

        @requires: sorted(vec)
        @modifies: vec
        @effects: sorted(vec_post) &&
                  cofind(vec_post,e) = cofind(vec,e) + newTerm.coeff
    */
    private static void sortedAdd(RatTermVec vec, RatTerm newTerm) {
        // IMPL IS STAFF ONLY
        assertSorted(vec);

        // FSK: impl could be faster [use binary instead of linear
        // search, not to mention get rid of the expensive "sorted"
        // assertions C:) ] but this works...

        for(int i=0; i<vec.size(); i++) {
            RatTerm rt = vec.get(i);
            if (rt.expt == newTerm.expt) {
                // MATCH!
                RatNum newCoeffValue = rt.coeff.add(newTerm.coeff);
                if (newCoeffValue.equals( new RatNum(0) )) {
                    vec.remove(i);
                    assertSorted(vec);
                    return;
                } else {
                    vec.set(new RatTerm( newCoeffValue , rt.expt ), i);
                    assertSorted(vec);
                    return;
                }
            } else if (rt.expt < newTerm.expt) {
                // we've hit a lower exponent w/o seeing a match; insert term here
                if (!newTerm.coeff.equals(new RatNum(0)))
                    vec.insert(newTerm, i);
                assertSorted(vec);
                return;
            }
        }

        // if we get here, we didn't hit a lower exponent OR find a
        // match; append onto the end of vec
        if (!newTerm.coeff.equals(new RatNum(0)))
            vec.addElement(newTerm);
        assertSorted(vec);
    }

    // METHOD IS STAFF ONLY
    private static void assertSorted(RatTermVec v) {
        // ( verifies Sorted property defined above in spec for sortedAdd )
        int lastExp = Integer.MAX_VALUE;
        for(int i=0; i<v.size(); i++) {
            RatTerm rt = v.get(i);

            Assert.assertTrue(!(rt.expt >= lastExp)); // "unsorted vec generated! "+v.printDebug());
            Assert.assertTrue(!rt.coeff.equals( new RatNum(0) )); // "vec w/ zero coeff generated! "+v.printDebug());

            lastExp = rt.expt;
        }
    }

    /** @return a new RatPoly, r, such that r = "this + p". */
    public RatPoly add(RatPoly p) {
        // IMPL IS STAFF ONLY
        checkRep();
        if (this.isNaN()) {
            checkRep();
            return this;
        }
        if (p.isNaN()) {
            checkRep();
            return p;
        }

        RatTermVec r = this.terms.copy();
        for(int i=0; i<p.terms.size(); i++) {
            sortedAdd( r, p.terms.get(i) );
        }

        checkRep();
        return new RatPoly(r);
    }

    /** @return a new RatPoly, r, such that r = "this - p". */
    public RatPoly sub(RatPoly p) {
        // IMPL IS STAFF ONLY
        checkRep();
        if (this.isNaN()) {
            checkRep();
            return this;
        }
        if (p.isNaN()) {
            checkRep();
            return p;
        }
        checkRep();
        return this.add(p.negate());
    }

    /** @return a new RatPoly, r, such that r = "this * p";
                if this.isNaN() or p.isNaN(), returns "NaN * x^0".
     */
    public RatPoly mul(RatPoly p) {
        // IMPL IS STAFF ONLY
        checkRep();
        if (this.isNaN()) {
            checkRep();
            return this;
        }
        if (p.isNaN()) {
            checkRep();
            return p;
        }

        RatPoly accum = new RatPoly();
        for(int i=0; i<this.terms.size(); i++) {
            RatTerm rt = this.terms.get(i);
            RatPoly augend = p.scaleBy( rt );
            accum = accum.add(augend);
        }
        checkRep();
        return accum;
    }

    /** Division operation (truncating).
        @return a new RatPoly, q, such that q = "this / p";
                if p = 0 or
                   p.isNaN() or
                   this.isNaN(), returns "NaN * x^0".

        Division of polynomials is defined as follows:
        Given two polynomials u and v, with v != "0", we can divide u by
        v to obtain a quotient polynomial q and a remainder polynomial
        r satisfying the condition u = "q * v + r" where the degree of
        r is strictly less than the degree of v.

        For the purposes of this class, the operation "u / v" returns
        q as defined above.

        Thus, "x^3-2*x+3" / "3*x^2" = "1/3*x" (with the corresponding
        r = "2*x+3"), and "x^2+2*x+15 / 2*x^3" = "0" (with the
        corresponding r = "x^2+2*x+15").

        Note that this truncating behavior is similar to the behavior
        of integer division on computers.
    */
    public RatPoly div(RatPoly p) {
        // IMPL IS STAFF ONLY
        checkRep();
        if (this.isNaN() || p.isNaN() ||
            p.unparse().equals((new RatPoly()).unparse())) {
            checkRep();
            return RatPoly.parse("NaN");
        } else {
            checkRep();
            return divAndRem(p)[0];
        }
    }

    // METHOD IS STAFF ONLY
    /** @returns a RatPoly[] ps = { "this/p", "this mod p" }
     */
    private RatPoly[] divAndRem(RatPoly p) {
        // Taken from Knuth, "The Art of Computer Programming"
        // sec. 4.6.1, Algorithm D
        checkRep();
        RatPoly u = this, v = p;
        final int m = u.degree();
        final int n = v.degree();
        RatTermVec q_Terms = new RatTermVec();
        RatTermVec r_Terms = u.terms.copy();

        // D1: do step D2 for k = m-n, m-n-1, ..., 0; then terminate
        // with (r[n-1], ..., r[0]) = (u[n-1], ..., u[0])
        for(int k=m - n; k >= 0; k--) {
            // D2: Set q[k] := u[n+k]/v[n] ...
            RatTerm u_nk = hintedGet(r_Terms, findTermIndex(r_Terms, n+k), n+k);
            RatNum q_k = u_nk.coeff.div(v.coeff(n));
            if (!q_k.equals(new RatNum(0))) {
                q_Terms.addElement(new RatTerm( q_k, k ));
            }

            //     Then set u[j] := u[j] - q[k]*v[j-k]
            //                      for j=n+k-l, n+k-2, ... k
            for(int j=n+k-1; j>=k; j--) {
                int u_j_ind = findTermIndex(r_Terms, j);
                RatTerm u_j = hintedGet(r_Terms, u_j_ind, j);
                RatNum v_jk = v.coeff(j-k);

                RatNum new_u_j = u_j.coeff.sub( q_k.mul( v_jk ));

                u_j = new RatTerm(new_u_j, j);
                replaceExpt(u_j, r_Terms, u_j_ind);
            }
        }

        // Repeat:
        // with (r[n-1], ..., r[0]) = (u[n-1], ..., u[0])
        // (need to fixup r_Terms so that it has no terms from n and up
        while(r_Terms.size() > 0) { // not the only condition; see below
            RatTerm t = r_Terms.get(0);
            if (t.expt >= n) {
                r_Terms.remove(0);
            } else {
                break;
            }
        }

        RatPoly quotient = new RatPoly(q_Terms);
        RatPoly remainder = new RatPoly(r_Terms);

        if (!quotient.mul(p).add(remainder).unparse().equals(this.unparse())) {

            // check for NaN before we throw a spurious exception...
            Assert.assertTrue(!(containsNaN(q_Terms) || containsNaN(r_Terms)));
        }
        Assert.assertTrue(!(remainder.degree() >= p.degree() && p.degree() > 0)); // "invalid remainder "+remainder.unparse()
        Assert.assertTrue(!quotient.isNaN()); // "has nan...dunno how..."

        checkRep();
        return new RatPoly[] { quotient, remainder };
    }
    // METHOD IS STAFF ONLY
    private static boolean containsNaN(RatTermVec vec) {
        for(int i=0; i<vec.size(); i++) {
            RatTerm term = vec.get(i);
            if (term.coeff.equals(RatNum.parse("NaN"))) {
                return true;
            }
        }
        return false;
    }

    // METHOD IS STAFF ONLY
    private static int findTermIndex(RatTermVec ts, int expt) {
        for(int i=0; i<ts.size(); i++) {
            RatTerm t = ts.get(i);
            if (t.expt == expt) {
                return i;
            } else if (t.expt < expt) {
                return i;
            }
        }
        return ts.size();
    }

    // METHOD IS STAFF ONLY
    private static RatTerm hintedGet(RatTermVec ts, int i, int expt) {
        if (i < ts.size()) {
            RatTerm ith = ts.get(i);
            if (ith.expt == expt) {
                return ith;
            }
        }
        return new RatTerm(new RatNum(0), expt);
    }

    // METHOD IS STAFF ONLY
    private static void replaceExpt(RatTerm term, RatTermVec ts, int index) {
        if (index < ts.size()) {
            RatTerm t = ts.get(index);
            if (t.expt == term.expt) {
                if (!term.coeff.equals(new RatNum(0))) {
                    ts.set(term, index);
                } else {
                    ts.remove(index);
                }
            } else {
                if (!term.coeff.equals(new RatNum(0)))
                    ts.insert(term, index);
            }
        } else {
            if (!term.coeff.equals(new RatNum(0)))
                ts.addElement(term);
        }
    }

    /** @return a new Poly equal to "0 - this". */
    public RatPoly negate() {
        // IMPL IS STAFF ONLY
        checkRep();
        RatTermVec r = this.terms.copy();
        scaleCoeff( r, new RatNum(-1) );
        checkRep();
        return new RatPoly(r);
    }

    /** @return value of this expression when evaluated at 'd'.
     */
    public double eval(double d) {
        // IMPL IS STAFF ONLY
        checkRep();
        double tot = 0;
        for(int i=0; i<terms.size(); i++) {
            RatTerm rt = terms.get(i);
            tot += rt.coeff.approx() * Math.pow(d, (double) rt.expt);
        }
        checkRep();
        return tot;
    }


    /** @requires: 'polyStr' is an instance of a string with no spaces
                   that expresses a poly in the form defined in the
                   unparse() method.
        @return a RatPoly p such that p.unparse() = polyStr
    */
    public static RatPoly parse(String polyStr) {
        RatPoly result = new RatPoly();

        // First we decompose the polyStr into its component terms;
        // third arg orders "+" and "-" to be returned as tokens.
        StringTokenizer termStrings =
            new StringTokenizer(polyStr, "+-", true);

        boolean nextTermIsNegative = false;
        while (termStrings.hasMoreTokens()) {
            String termToken = termStrings.nextToken();

            if (termToken.equals("-")) {
                nextTermIsNegative = true;
            } else if (termToken.equals("+")) {
                nextTermIsNegative = false;
            } else {
                // Not "+" or "-"; must be a term

                // Term is: "R" or "R*x" or "R*x^N" or "x^N" or "x",
                // where R is a rational num and N is a natural num.

                // Decompose the term into its component parts.
                // third arg orders '*' and '^' to act purely as delimiters.
                StringTokenizer numberStrings =
                    new StringTokenizer(termToken, "*^", false);

                RatNum coeff;
                int expt;

                String c1 = numberStrings.nextToken();
                if (c1.equals("x")) {
                    // ==> "x" or "x^N"
                    coeff = new RatNum(1);

                    if (numberStrings.hasMoreTokens()) {
                        // ==> "x^N"
                        String N = numberStrings.nextToken();
                        expt = Integer.parseInt(N);

                    } else {
                        // ==> "x"
                        expt = 1;
                    }
                } else {
                    // ==> "R" or "R*x" or "R*x^N"
                    String R = c1;
                    coeff = RatNum.parse(R);

                    if (numberStrings.hasMoreTokens()) {
                        // ==> "R*x" or "R*x^N"
                        String x = numberStrings.nextToken();

                        if (numberStrings.hasMoreTokens()) {
                            // ==> "R*x^N"
                            String N = numberStrings.nextToken();
                            expt = Integer.parseInt(N);
                        } else {
                            // ==> "R*x"
                            expt = 1;
                        }

                    } else {
                        // ==> "R"
                        expt = 0;
                    }
                }

                // at this point, coeff and expt are initialized.
                // Need to fix coeff if it was preceeded by a '-'
                if (nextTermIsNegative) {
                    coeff = coeff.negate();
                }


                // accumulate terms of polynomial in 'result'
                if (!coeff.equals(new RatNum(0))) {
                    RatPoly termPoly = new RatPoly();
                    termPoly.terms.addElement(new RatTerm(coeff, expt));
                    result = result.add(termPoly);
                }
            }
        }
        return result;
    }

    public String toString() {
        checkRep();
        return this.unparse();
    }

    // METHOD IS STAFF ONLY
    private void checkRep() {
      Assert.assertTrue(terms != null); // "terms == null!"
        for (int i=0; i < terms.size(); i++) {
            Assert.assertTrue(!terms.get(i).coeff.equals(new RatNum(0))); // "zero coefficient!"
            Assert.assertTrue(!(terms.get(i).expt < 0)); // "negative exponent!"
            Assert.assertTrue(!((i < terms.size() - 1) && (terms.get(i+1).expt >= terms.get(i).expt))); // "terms out of order!"
        }
    }
}
