package daikon.inv;

import java.util.Locale;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.dataflow.qual.SideEffectFree;

/**
 * This class is an enumerated type representing the possible results of adding a sample to an
 * invariant.
 */
public enum InvariantStatus {

  /** No change was made to the invariant's validity. */
  NO_CHANGE,

  /** The invariant was falsified. */
  FALSIFIED,

  /** The invariant's condition being weakened. For example OneOf{1,3} became OneOf{1,3,10}. */
  WEAKENED;

  @SideEffectFree
  @Override
  public String toString(@GuardSatisfied InvariantStatus this) {
    return name().toLowerCase(Locale.ENGLISH);
  }
}
