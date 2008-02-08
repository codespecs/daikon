package asm;

import java.util.Collection;
import java.util.Collections;
import java.util.Set;


public class KillerInstruction implements IInstruction {

  private Collection<IInstruction> instructions;

  public KillerInstruction(Collection<IInstruction> instructions) {
    if (instructions == null) throw new IllegalArgumentException("instructions cannot be null.");
    this.instructions = instructions;
  }

  public String getAddress() {
    throw new UnsupportedOperationException();
  }

  public Set<String> getLHSVars() {
    return Collections.emptySet();
  }

  public boolean kills(String var) {
    for (IInstruction i : instructions)
      if (i.kills(var))
        return true;
    return false;
  }

  public String toString() {
    StringBuilder b = new StringBuilder();
    for (IInstruction i : instructions) {
      b.append("(killer)" + i + "\n");
    }
    return b.toString();
  }
}
