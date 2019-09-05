package daikon.tools.runtimechecker;

import java.util.ArrayList;
import java.util.List;
import org.checkerframework.checker.lock.qual.GuardedBy;

/**
 * If a class has been instrumented with the instrumenter, invariant violations are added to the
 * {@code violations} list.
 */
@SuppressWarnings("JavaLangClash")
public class Runtime {

  /** A list of throwables seen when attempting to evaluate properties. */
  public static List<Throwable> internalInvariantEvaluationErrors = new ArrayList<>();

  private static @GuardedBy("Runtime.class") List<Violation> violations =
      new ArrayList<Violation>();

  // The number of times that an invariant was checked (whether the
  // check succeeded or failed).
  public static long numEvaluations = 0;

  // The number of entry program points traversed.
  public static long numPptEntries = 0;

  // The number of normal-exit program points traversed.
  public static long numNormalPptExits = 0;

  // The number of exceptional-exit program points traversed.
  public static long numExceptionalPptExits = 0;

  /** Returns the list of violations. */
  public static synchronized List<Violation> getViolations() {
    List<Violation> retval = new ArrayList<>();
    for (Violation v : violations) {
      retval.add(v);
    }
    return retval;
  }

  /** Empty the violations list. */
  public static synchronized void resetViolations() {
    violations = new ArrayList<Violation>();
  }

  /** True if the violations list is empty. */
  public static synchronized boolean violationsEmpty() {
    return violations.isEmpty();
  }

  /** Add a violation to the violations list. */
  public static synchronized void violationsAdd(Violation v) {
    violations.add(v);
  }

  // Works for non-negative values
  public static final boolean isPowerOfTwo(int x) {
    if (x == 0) {
      return true;
    }
    // If x is a power of two, then x - 1 has no bits in common with x
    // OTOH, if x is not a power of two, then x and x - 1 have the same
    // most-significant bit set, so they have at least one bit in common.
    return (x & (x - 1)) == 0;
  }

  private static int largestNonPointerValue = 100000;

  private static int smallestNonPointerValue = -100000;

  public static final boolean isWithinPointerRange(int value) {
    if (value == 0) {
      return true;
    }
    return (value >= largestNonPointerValue) || (value <= smallestNonPointerValue);
  }
}
