package daikon.simplify;

import java.io.IOException;

/**
 * An UndoAssume command removes an assumption from the assumption
 * stack of the given session.  The command will not block.
 **/
public class CmdUndoAssume
  implements Cmd
{
  public static CmdUndoAssume single = new CmdUndoAssume();

  /** Read the class overview */
  public void apply(Session s) {

    synchronized(s) {
      // send out the (BG_POP)
      s.input.println("(BG_POP)");
      s.input.flush();
      
      // there is no output from Simplify
    }
	
  }

  public String toString() {
    return "CmdUndoAssume";
  }

}
