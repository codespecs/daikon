package daikon.asm;

import java.util.Set;

/*>>>
import org.checkerframework.dataflow.qual.*;
*/

/*
 * Represents an assembly instruction.
 */
public interface IInstruction {

  public abstract Set<String> getBinaryVarNames();

  /*@SideEffectFree*/ public abstract String toString();

  public abstract boolean kills(String var);

  public abstract String getAddress();
}
