package daikon.simplify;

/*>>>
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

/**
 * An UndoAssume command removes an assumption from the assumption stack of the given session. The
 * command will not block.
 */
public class CmdUndoAssume implements Cmd {
  public static CmdUndoAssume single = new CmdUndoAssume();

  /** For documentation, read the class overview. */
  @Override
  public void apply(final /*@GuardedBy("<self>")*/ Session s) {

    synchronized (s) {
      // send out the (BG_POP)
      s.sendLine("(BG_POP)");
      if (Session.dkconfig_verbose_progress > 1) {
        System.out.print(">");
        System.out.flush();
      }

      // there is no output from Simplify
    }
  }

  /*@SideEffectFree*/
  @Override
  public String toString(/*>>>@GuardSatisfied CmdUndoAssume this*/) {
    return "CmdUndoAssume";
  }
}
