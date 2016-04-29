package daikon.simplify;

/*>>>
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

public interface Cmd
{
  /**
   * Runs the command in the given session.
   **/
  public void apply(final /*@GuardedBy("itself")*/ Session s);

  /**
   * @return a string for debugging only.
   **/
  /*@SideEffectFree*/ public String toString(/*>>>@GuardSatisfied Cmd this*/);
}
