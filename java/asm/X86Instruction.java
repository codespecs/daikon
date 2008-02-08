package asm;

import java.util.*;


/**
 * Represents an x86 instruction.
 */
public class X86Instruction implements IInstruction  {

    private String dllName;
    private String address;
    private String opName;
    private List<String> args;
    private List<String> killedVars;


    // For debugging printing purposes.
    // The name of the basic block that contains this instruction.
    public String owner = null;

    // See method parseInstruction. It sets all fields appropriately.
    private X86Instruction() { }

    /* (non-Javadoc)
     * @see daikon.IInstruction#getArgs()
     */
    public Set<String> getLHSVars() {
      Set<String> retval = new LinkedHashSet<String>();
      for (String s : args) {
        if (!s.startsWith("$"))
          retval.add(s);
      }
    	return retval;
    }

    /* (non-Javadoc)
     * @see daikon.IInstruction#toString()
     */
    public String toString() {
        StringBuilder b = new StringBuilder();
        b.append(owner != null ? owner + ":" : "");
        b.append(dllName);
        b.append(":");
        b.append(address);
        b.append(" ");
        b.append(opName);
        for (String a : args) {
            b.append(" ");
            b.append(a);
        }
        if (killedVars.isEmpty())
            return b.toString();

        b.append(" -> ");
        for (String a : killedVars) {
            b.append(" ");
            b.append(a);
        }
        return b.toString();
    }

    /**
     * <dll name>:<address> <op name> <arg> ... <arg> -> <result var>
     */
    public static X86Instruction parseInstruction(String s) {
        if (s == null) throw new IllegalArgumentException("String cannot be null.");
        String[] tokens = s.trim().split("\\s");
        if (tokens.length < 2)
            throw new IllegalArgumentException("Invalid instruction string: " + s);

        X86Instruction inst = new X86Instruction();

        // Set dllName and address fields.
        String[] dllAddr = tokens[0].split(":");
        if (dllAddr.length != 2)
            throw new IllegalArgumentException("Invalid instruction string: " + s);
        if (!dllAddr[0].endsWith(".dll"))
            throw new IllegalArgumentException("Invalid instruction string: " + s);
        inst.dllName = dllAddr[0];
        if (!dllAddr[1].startsWith("0x"))
            throw new IllegalArgumentException("Invalid instruction string: " + s);
        try { Long.parseLong(dllAddr[1].substring(2), 16);
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid instruction string: " + s);
        }
        inst.address = dllAddr[1];

        // Set opName field.
        if (!isValidOp(tokens[1]))
            throw new IllegalArgumentException("Invalid instruction string: " + s);
        inst.opName = tokens[1];

        // Set args
        inst.args = new ArrayList<String>();
        int i = 2;
        for (; i < tokens.length ; i++) {
            if (tokens[i].equals("->")){
                if (tokens.length < i+2) // There should be at least one resultVar.
                    throw new IllegalArgumentException("Invalid instruction string: " + s);
                i++; // Move i to first result var.
                break;
            }
            if (!isValidVar(tokens[i]))
                throw new IllegalArgumentException("Invalid instruction string: " + s);
            inst.args.add(tokens[i]);
        }

        // Set resultVars.
        inst.killedVars = new ArrayList<String>();
        for (; i < tokens.length ; i++) {
            if (!isValidVar(tokens[i]))
                throw new IllegalArgumentException("Invalid instruction string: " + s);
            inst.killedVars.add(tokens[i]);
        }

        return inst;
    }

    // Refine if needed.
    private static boolean isValidOp(String opString) {
        return true;
    }

    // Refine if needed.
    private static boolean isValidVar(String varString) {
        return true;
    }

  private static Set<String> varsUnmodifiedByCallOp;

  static {
    varsUnmodifiedByCallOp = new LinkedHashSet<String>();
    varsUnmodifiedByCallOp.add("ebx");
    varsUnmodifiedByCallOp.add("esi");
    varsUnmodifiedByCallOp.add("edi");
    varsUnmodifiedByCallOp.add("ebp");
  }

	/* (non-Javadoc)
   * @see daikon.IInstruction#kills(java.lang.String)
   */
    public boolean kills(String var) {
        if (var == null) throw new IllegalArgumentException("var cannot be null.");
        if (opName.equals("call")) {
            return !varsUnmodifiedByCallOp.contains(var);
        }
        return killedVars.contains(var);
    }

	/* (non-Javadoc)
   * @see daikon.IInstruction#getAddress()
   */
	public String getAddress() {
		return address;
	}

}
