package daikon.asm;

import java.util.Set;

/*
 * Represents an assembly instruction.
 */
public interface IInstruction {

  public abstract Set<String> getBinaryVarNames();

  @SideEffectFree
  @Override
  public abstract String toString(@GuardSatisfied IInstruction this);

  public abstract boolean kills(String var);

  public abstract String getAddress();
}
