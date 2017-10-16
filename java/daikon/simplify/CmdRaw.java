package daikon.simplify;

/*>>>
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

/**
 * A Raw command provides no additional structure, allowing arbitrary commands (as long as they have
 * no output) to be sent to the prover. It will not block.
 */
public class CmdRaw implements Cmd {
  public final String cmd;

  public CmdRaw(String cmd) {
    this.cmd = cmd.trim();
    SimpUtil.assert_well_formed(this.cmd);
  }

  /** For documentation, read the class overview. */
  @Override
  public void apply(final /*@GuardedBy("<self>")*/ Session s) {

    synchronized (s) {
      // send out the command
      s.sendLine(cmd);
      // there is no output from Simplify
    }
  }

  /*@SideEffectFree*/
  @Override
  public String toString(/*>>>@GuardSatisfied CmdRaw this*/) {
    return "CmdRaw: " + cmd;
  }
}
