package daikon.inv;

/*>>>
import org.checkerframework.checker.interning.qual.*;
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

/**
 * This class is an enumerated type representing the possible results of adding an sample to an
 * invariant.
 */
public final /*@Interned*/ class InvariantStatus {

  private final String status;

  private InvariantStatus(String status) {
    this.status = status;
  }

  /*@SideEffectFree*/
  @Override
  public String toString(/*>>>@GuardSatisfied InvariantStatus this*/) {
    return status;
  }

  /** The InvariantStatus that represents no change being made to the invariant's validity. */
  public static final InvariantStatus NO_CHANGE = new InvariantStatus("no_change");

  /** The InvariantStatus that represents an invariant being falsified. */
  public static final InvariantStatus FALSIFIED = new InvariantStatus("falsified");

  /**
   * The InvariantStatus that represents an invariant's condition being weakened. For example
   * OneOf{1,3} going to OneOf{1,3,10}.
   */
  public static final InvariantStatus WEAKENED = new InvariantStatus("weakened");
}
