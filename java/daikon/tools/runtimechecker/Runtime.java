package daikon.tools.runtimechecker;

import java.util.ArrayList;
import java.util.List;

/**
 * If a class has been instrumented with the instrumenter (
 * invariant violations are added to the <code>violations</code> list.
 *
 */
public class Runtime {

    /**
     * A list of throwables seen when attempting to evaluate properties.
     */
    public static List<Throwable> internalInvariantEvaluationErrors = new ArrayList<Throwable>();

    private static List<Violation> violations = new ArrayList<Violation>();

    /**
     * Returns the list of violations.
     */
    public static synchronized List<Violation> getViolations() {
	List<Violation> retval = new ArrayList<Violation>();
	for (int i = 0 ; i < violations.size() ; i++) {
	    retval.add((Violation)violations.get(i));
	}
	return retval;
    }

    /**
     * Empty the violations list.
     */
    public static synchronized void resetViolations() {
	violations = new ArrayList();
    }

    /**
     * True if the violations list is empty.
     */
    public static synchronized boolean violationsEmpty() {
	return violations.isEmpty();
    }

    /**
     * Add a violation to the violations list.
     */
    public static synchronized void violationsAdd(Violation v) {
	violations.add(v);
    }

}
