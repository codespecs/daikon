// MultiRandSelector.java 
package utilMDE;
import java.util.*;

/**
 *
 * <b>MultiRandSelector</b> delegates to a set of RandomSelector Objects,
 * one for each of several unique equivalence classes.  The constructor
 * takes in an Object implementing an interface EquivalenceChecker
 * @author Lee Lin 4/3/2002 */

public class MultiRandSelector {

    private int num_elts;
    private Random seed;
    private EquivalenceChecker eq;

    private HashMap map;

    public MultiRandSelector (int num_elts, EquivalenceChecker eq) {
	this (num_elts, new Random(), eq);
    }

    public MultiRandSelector (int num_elts, Random r, 
			      EquivalenceChecker eq) {
	this.num_elts = num_elts;
	seed = r;
	this.eq = eq;
	map = new HashMap();
    }

    public void acceptIter (Iterator iter) {
	while (iter.hasNext()) {
	    accept (iter.next());
	}
    }

    public void accept (Object next) {
	Object equivClass = eq.deriveEquivalence (next);
	if (equivClass == null) return;
	RandomSelector delegation = (RandomSelector) map.get (equivClass);
	if (delegation == null) {
	    delegation = new RandomSelector (num_elts, seed);
	    map.put (equivClass, delegation);
	}
	delegation.accept (next);
    }

    /** NOT safe from concurrent modification */
    public HashMap values () {
	return map;
    }

    /** Returns an iterator of all objects selected */
    public Iterator valuesIter() {
	ArrayList ret = new ArrayList();
	for (Iterator i = map.keySet().iterator(); i.hasNext();) {
	    RandomSelector rs = (RandomSelector) map.get (i.next());
	    ret.addAll (rs.getValues());
	}
	return ret.iterator();
    }

}


