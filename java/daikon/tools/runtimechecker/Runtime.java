package daikon.tools.runtimechecker;

import java.util.ArrayList;
import java.util.List;

/**
 * If a class has been instrumented with the instrumenter (
 * invariant violations are added to the <code>violations</code> list.
 *
 */
public class Runtime {

    private static List/* Violation */violations = new ArrayList/* Violation */();

    public static synchronized List/*<Violation>*/ getViolations() {
	List/*<Violation>*/ retval = new ArrayList/*<Violation>*/();
	for (int i = 0 ; i < violations.size() ; i++) {
	    retval.add((Violation)violations.get(i));
	}
	return retval;
    }

    public static synchronized void resetViolations() {
	violations = new ArrayList();
    }

    public static synchronized boolean violationsEmpty() {
	return violations.isEmpty();
    }

    public static synchronized void violationsAdd(Violation v) {
	violations.add(v);
    }

    private static List errors = new ArrayList();

    public static synchronized void resetErrors() {
	errors = new ArrayList();
    }

    public static synchronized boolean errorsEmpty() {
	return errors.isEmpty();
    }

    public static List/* Throwable */internalInvariantEvaluationErrors = new ArrayList();

}
