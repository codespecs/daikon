package daikon.inv.unary;

import daikon.*;

/*>>>
import org.checkerframework.checker.initialization.qual.*;
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.checker.nullness.qual.*;
*/

// An interface satisfied by OneOfScalar, OneOfString, OneOfSequence, and
// OneOfStringSequence.
// The variable takes on exactly one value.
public interface OneOf {
  /** The number of elements in the OneOf invariant. */
  public int num_elts();

  /**
   * The single value represented by the OneOf invariant. Throws an error if not exactly one value
   * is represented by this.
   */
  public Object elt();

  public VarInfo var(
      /*>>>@GuardSatisfied @UnknownInitialization(OneOf.class) @Raw(OneOf.class) OneOf this*/);
}
