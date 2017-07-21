package daikon.asm;

import java.util.*;

/*>>>
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

/** Represents an x86 instruction. */
public class X86Instruction implements IInstruction {

  private String dllName;

  private String address;

  private String opName;

  List<String> args;

  List<String> killedVars;

  protected boolean isFirstInBlock = false;

  // For debugging printing purposes.
  /** The name of the basic block that contains this instruction. */
  // (Maybe this should be @MonotonicNonNull?)
  public /*@Nullable*/ String owner = null;

  // See method parseInstruction. It sets all fields appropriately.
  private X86Instruction(
      String dllName, String address, String opName, List<String> args, List<String> killedVars) {
    this.dllName = dllName;
    this.address = address;
    this.opName = opName;
    this.args = args;
    this.killedVars = killedVars;
  }

  public String getOpName() {
    return opName;
  }

  public List<String> getArgs() {
    return args;
  }

  @Override
  public Set<String> getBinaryVarNames() {
    Set<String> retval = new LinkedHashSet<String>();

    for (String s : args) {

      if (Operand.isConstant(s)) continue;
      if (Operand.isDerefdNumber(s)) continue;
      if (Operand.is8BitReg(s)) continue;
      if (Operand.is16BitReg(s)) continue;
      if (Operand.isFPUReg(s)) continue;

      // If the string looks like this: [0+eax+(edx*4)] return the
      // string without brackets, and return eax.  There's some
      // ugliness about lea, which is a special case.
      if (Operand.isDeref(s)) {
        List<String> regs = Operand.getExtendedRegisters(s);
        assert regs.size() == 1 || regs.size() == 2 : s;

        if (regs.size() == 1) {
          if (opName.equals("lea")) {
            retval.add(s.substring(1, s.length() - 1));
            // More lea ugliness. I'm just trying to match the
            // variables from the dtrace file.
            // If instruction is something like
            //  lea [-2++(eax*4)] -> eax
            // then we do return "eax" as well.
            if (s.contains("++")) {
              retval.add(regs.get(0));
            }
            continue;
          }
        } else { // regs.size() == 2
          retval.add(regs.get(1));
          retval.add(s.substring(1, s.length() - 1));
          if (!opName.equals("lea")) retval.add(s);
          continue;
        }
      }

      if (s.equals("esp")) {
        if (isFirstInBlock) {
          retval.add(s);
        } else if (opName.equals("call") || opName.equals("call_ind")) {
          retval.add(s);
        } else {
          continue;
        }
      }

      retval.add(s);
    }
    return retval;
  }

  /*
   * (non-Javadoc)
   *
   * @see daikon.IInstruction#toString()
   */
  /*@SideEffectFree*/
  @Override
  public String toString(/*>>>@GuardSatisfied X86Instruction this*/) {
    StringBuilder b = new StringBuilder();
    // b.append(owner != null ? owner + ":" : "");
    // b.append(dllName);
    // b.append(":");
    b.append(address);
    b.append(" ");
    b.append(opName);
    for (String a : args) {
      b.append(" ");
      b.append(a);
    }
    if (killedVars.isEmpty()) {
      return b.toString();
    }

    b.append(" -> ");
    for (String a : killedVars) {
      b.append(" ");
      b.append(a);
    }
    return b.toString();
  }

  /** dllname:address opname arg ... arg &rarr; resultvar */
  public static X86Instruction parseInstruction(String s) {
    if (s == null) throw new IllegalArgumentException("String cannot be null.");
    String[] tokens = s.trim().split("\\s+");
    if (tokens.length < 2) throw new IllegalArgumentException("Invalid instruction string: " + s);

    // Set dllName and address fields.
    String[] dllAddr = tokens[0].split(":");
    if (dllAddr.length != 2) throw new IllegalArgumentException("Invalid instruction string: " + s);
    if (!(dllAddr[0].endsWith(".dll") || dllAddr[0].endsWith(".exe"))) {
      throw new IllegalArgumentException("Invalid instruction string: " + s);
    }
    String inst_dllName = dllAddr[0];
    if (!dllAddr[1].startsWith("0x")) {
      throw new IllegalArgumentException("Invalid instruction string: " + s);
    }
    try {
      Long.parseLong(dllAddr[1].substring(2), 16);
    } catch (NumberFormatException e) {
      throw new IllegalArgumentException("Invalid instruction string: " + s);
    }
    String inst_address = dllAddr[1];

    // Set opName field.
    if (!isValidOp(tokens[1])) {
      throw new IllegalArgumentException("Invalid instruction string: " + s);
    }
    String inst_opName = tokens[1];

    // Set args
    List<String> inst_args = new ArrayList<String>();
    int i = 2;
    for (; i < tokens.length; i++) {
      if (tokens[i].equals("->")) {
        if (tokens.length < i + 2) // There should be at least one resultVar.
        throw new IllegalArgumentException("Invalid instruction string: " + s);
        i++; // Move i to first result var.
        break;
      }
      if (!isValidLHSOp(tokens[i])) {
        throw new IllegalArgumentException("Invalid instruction string: " + s);
      }
      inst_args.add(tokens[i]);
    }

    // Set resultVars.
    List<String> inst_killedVars = new ArrayList<String>();
    for (; i < tokens.length; i++) {
      if (!isValidRHSVar(tokens[i])) {
        throw new IllegalArgumentException("Invalid instruction string: " + s);
      }
      inst_killedVars.add(tokens[i]);
    }

    return new X86Instruction(inst_dllName, inst_address, inst_opName, inst_args, inst_killedVars);
  }

  // TODO fill in if necessary.
  private static boolean isValidOp(String string) {
    return true;
  }

  private static boolean isValidRHSVar(String s) {
    if (Operand.isRegister(s)) return true;
    if (Operand.isDeref(s)) return true;
    return false;
  }

  // Refine if needed.
  private static boolean isValidLHSOp(String s) {
    if (Operand.isConstant(s)) return true;
    if (Operand.isRegister(s)) return true;
    if (Operand.isDeref(s)) return true;
    return false;
  }

  private static Set<String> varsUnmodifiedByCallOp;

  static {
    varsUnmodifiedByCallOp = new LinkedHashSet<String>();
    varsUnmodifiedByCallOp.add("ebx");
    varsUnmodifiedByCallOp.add("esi");
    varsUnmodifiedByCallOp.add("edi");
    varsUnmodifiedByCallOp.add("ebp");
  }

  // Only works for extended registers. This should
  // be fine because we only compute variables over
  // extended registers.
  @Override
  public boolean kills(String var) {

    assert Operand.isRegister(var) ? Operand.isExtendedReg(var) : true;

    if (opName.startsWith("call")) {
      return !varsUnmodifiedByCallOp.contains(var);
    }

    if (var.equals("eax")) {
      if (killedVars.contains(var)
          || killedVars.contains("ax")
          || killedVars.contains("ah")
          || killedVars.contains("al")) {
        return true;
      } else {
        return false;
      }
    }

    if (var.equals("ebx")) {
      if (killedVars.contains(var)
          || killedVars.contains("bx")
          || killedVars.contains("bh")
          || killedVars.contains("bl")) {
        return true;
      } else {
        return false;
      }
    }

    if (var.equals("ecx")) {
      if (killedVars.contains(var)
          || killedVars.contains("cx")
          || killedVars.contains("ch")
          || killedVars.contains("cl")) {
        return true;
      } else {
        return false;
      }
    }

    if (var.equals("edx")) {
      if (killedVars.contains(var)
          || killedVars.contains("dx")
          || killedVars.contains("dh")
          || killedVars.contains("dl")) {
        return true;
      } else {
        return false;
      }
    }

    if (var.equals("edi")) {
      return (killedVars.contains(var) || killedVars.contains("di"));
    }

    if (var.equals("esi")) {
      return (killedVars.contains(var) || killedVars.contains("si"));
    }

    if (var.equals("esp")) {
      return (killedVars.contains(var) || killedVars.contains("sp"));
    }

    if (var.equals("ebp")) {
      return (killedVars.contains(var) || killedVars.contains("bp"));
    }

    if (Operand.isRegister(var)) {
      return killedVars.contains(var);
    }

    if (Operand.isDeref(var)) {
      if (killedVars.contains(var)) return true;
      for (String reg : Operand.getExtendedRegisters(var)) {
        for (String killedVar : killedVars) {
          // [...eax...] killed by [...eax...]
          // [...eax...] killed by eax
          // Note that this assumes that non-extended registers
          // are never inside a dereference expression (which
          // appears to be true for all asm files I've seen).
          if (killedVar.contains(reg)) {
            return true;
          }
        }
      }
      return false;
    }

    //     if (Operand.isDeref(var)) {
    //       if (killedVars.contains(var))
    //         return true;
    //       for (String killedVar : killedVars) {
    //         if (Operand.isDeref(killedVar))
    //           return true;
    //       }
    //       for (String reg : Operand.getExtendedRegisters(var)) {
    //         if (killedVars.contains(reg))
    //           return true;
    //       }
    //       return false;
    //     }

    // It may be something like "16+ebx". Do a quick sanity check.
    if (var.indexOf('+') != -1) {
      for (String reg : Operand.getExtendedRegisters(var)) {
        if (killedVars.contains(reg)) {
          return true;
        }
      }
    }

    return false;
  }

  /*
   * (non-Javadoc)
   *
   * @see daikon.IInstruction#getAddress()
   */
  @Override
  public String getAddress() {
    return address;
  }
}
