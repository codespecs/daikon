package daikon.asm;

import java.util.Collection;
import java.util.Collections;
import java.util.Set;

/*>>>
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

/**
 * Represents a set of instructions that may or may not be executed, and whose only effect is that
 * they may kill one or more variables.
 *
 * <p>A killer instruction is the abstraction we use to think of all the possible paths between a
 * basic block and one of its dominators, and the effect of these paths on the state of the program.
 */
public class KillerInstruction implements IInstruction {

  private Collection<X86Instruction> instructions;

  public Collection<X86Instruction> getInstructions() {
    return instructions;
  }

  public KillerInstruction(Collection<X86Instruction> instructions) {
    if (instructions == null) throw new IllegalArgumentException("instructions cannot be null.");
    this.instructions = instructions;
  }

  @Override
  public String getAddress() {
    throw new UnsupportedOperationException();
  }

  @Override
  public Set<String> getBinaryVarNames() {
    return Collections.emptySet();
  }

  @Override
  public boolean kills(String var) {
    for (X86Instruction i : instructions) {
      if (i.kills(var)) {
        return true;
      }
    }
    return false;
  }

  /*@SideEffectFree*/
  @Override
  public String toString(/*>>>@GuardSatisfied KillerInstruction this*/) {
    StringBuilder b = new StringBuilder();
    for (X86Instruction i : instructions) {
      b.append("(potential)" + i + "\n");
    }
    return b.toString();
  }
}
