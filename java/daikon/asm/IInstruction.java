package daikon.asm;

import java.util.Set;

/*
 * Represents an assembly instruction.
 */
public interface IInstruction {

  public abstract Set<String> getBinaryVarNames();

  public abstract String toString();

  public abstract boolean kills(String var);

  public abstract String getAddress();

}
