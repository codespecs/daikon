package daikon.simplify;

/*>>>
import org.checkerframework.dataflow.qual.*;
*/

public interface Cmd {
  /**
   * Runs the command in the given session.
   **/
  public void apply(Session s);

  /**
   * @return a string for debugging only.
   **/
  /*@SideEffectFree*/ public String toString();
}
