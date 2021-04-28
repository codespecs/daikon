package PolyCalc;

import java.util.ArrayList;

/** RatTermVec is a mutable, growable array of RatTerms.  Such arrays
    are commonly referred to as <i>vectors</i>.  One can access
    RatTerms in a RatTermVec using an integer index (starting at
    zero).  The size of a RatTermVec grows (or shrinks) as needed to
    accommodate the addition and removal of RatTerms.
    <p>
    The current state of a RatTermVec can be notated as a sequence:
    [Ta, Tb, ...].  Examples of RatTermVecs include [] (an empty
    vector), [Ta], [Ta, Tb], and [Ta, Tb, Ta], where Ta and Tb are
    RatTerm objects.
*/
public class RatTermVec {
    // Definitions:
    // For a RatTermVec r, let T(r,i) be "r.wrapped.get(i)"

    // AF(r) = forall i=0 to r.wrapped.size()-1, [T(r,0), T(r,1), ..., T(r,i), ...]

    // RI(r) = r.wrapped != null && forall i=0 to r.wrapped.size()-1, T(r,i) is-a Term

    private ArrayList<RatTerm> wrapped;

    /** @effects: constructs a new empty RatTermVec, []. */
    public RatTermVec() { wrapped = new ArrayList<RatTerm>(); }

    /** @return the size of this RatTermVec. */
    public int size() { return wrapped.size(); }

    /** Indexing operation.
	@requires: 0 <= index < this.size()
	@return the RatTerm at the specified index.
	<br>
	e.g. Given a RatTermVec v = [t2, t3, t4], the expression
	"v.get(1)" will return the RatTerm t3.
     */
    public RatTerm get(int index) { return wrapped.get(index); }

    /** Appending operation.
	@requires: t != null
	@modifies: this
	@effects: Adds the specified RatTerm, 't', to the end of this
	vector, increasing the vector's size by one.
	<br>
	e.g. Given a RatTermVec v = [t2, t3, t4], the statement
	"v.addElement(t3);" will make v_post = [t2, t3, t4, t3].
    */
    public void addElement(RatTerm t) { wrapped.add(t); }

    /** Insertion operation.
	@requires: t != null && 0 <= index <= this.size()
	@modifies: this
	@effects: Inserts 't' as a component in this RatTermVec at the
	specified index. Each component in this vector with an index
	greater or equal to the specified index is shifted upward to
	have an index one greater than the value it had previously.
	The size of this vector is increased by 1.
	<br>
	e.g. Given a RatTermVec v = [t2, t3, t4], the statement
	"v.insert(t5, 1);" will make v_post = [t2, t5, t3, t4].
    */
    public void insert(RatTerm t, int index) { wrapped.add(index, t); }

    /** Deletion operation.
	@requires: 0 <= index < this.size()
	@modifies: this
	@effects: Deletes the RatTerm at the specified index. Each
	RatTerm in this vector with an index greater or equal to the
	specified index is shifted downward to have an index one
	smaller than the value it had previously. The size of this
	vector is decreased by 1.
	<br>
	e.g. Given a RatTermVec v = [t2, t3, t4], the statement
	"v.remove(1);" will make v_post = [t2, t4].
    */
    public void remove(int index) { wrapped.remove(index); }

    /** Replacement operation.
	@requires: t != null && 0 < index < this.size()
	@modifies: this
	@effects: Sets the RatTerm at the 'index' of this vector to be
	          't'. The previous RatTerm at 'index' is discarded.
	<br>
	e.g. Given a RatTermVec v = [t2, t3, t4], the statement
	"v.set(t5, 1);" will make v_post = [t2, t5, t4].
    */
    public void set(RatTerm t, int index) { wrapped.set(index, t); }

    /** Copy operation.
	@return a new RatTermVec whose initial state matches that of
	this RatTermVec.  Changes made to the state of the returned vector
	will NOT be reflected in this vector, and vice versa.  (Also recall
	that RatTerm objects are immutable.)
    */
    @SuppressWarnings("unchecked")
    public RatTermVec copy() {
	RatTermVec tv = new RatTermVec();
	tv.wrapped = (ArrayList<RatTerm>) this.wrapped.clone();
	return tv;
    }

    /** @return implementation specific debugging string. */
    public String printDebug() {
	return "RatTermVec<wrapped:"+this.wrapped+">";
    }

    public String toString() { return printDebug(); }
}
