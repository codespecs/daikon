package daikon.simplify;

import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.lock.qual.GuardedBy;
import org.checkerframework.dataflow.qual.SideEffectFree;

public interface Cmd {
  /**
   * Runs the command in the given session.
   *
   * @param s a session in which to run this command
   */
  public void apply(final @GuardedBy("<self>") Session s);

  /**
   * Returns a string for debugging only.
   *
   * @return a string for debugging only
   */
  @SideEffectFree
  @Override
  public String toString(@GuardSatisfied Cmd this);
}
