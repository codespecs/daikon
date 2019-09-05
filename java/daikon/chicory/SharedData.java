package daikon.chicory;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;
import org.checkerframework.checker.lock.qual.GuardedBy;

/**
 * Data that is shared across Chicory. The primary users are Instrument.java and Runtime.java. As
 * those classes may be executing on different threads, these items must be accessed via
 * synchronized statements.
 */
@SuppressWarnings(
    "initialization.fields.uninitialized") // library initialized in code added by run-time
// instrumentation
public class SharedData {
  /**
   * List of classes recently transformed. This list is examined in each enter/exit and the decl
   * information for any new classes are printed out and the class is then removed from the list.
   */
  // The order of this list depends on the order of loading by the JVM.
  // Declared as Deque instead of List to permit use of removeFirst().
  public static final @GuardedBy("<self>") Deque<ClassInfo> new_classes =
      new ArrayDeque<ClassInfo>();

  /** List of all instrumented classes. */
  public static final @GuardedBy("<self>") List<ClassInfo> all_classes = new ArrayList<>();

  /** List of all instrumented methods. */
  public static final @GuardedBy("<self>") List<MethodInfo> methods = new ArrayList<>();
}
