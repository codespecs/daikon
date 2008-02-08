package asm;

import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Utility methods that operate on collections of instructions.
 *
 */
public class InstructionUtils {

  /**
   * Computes a set of binary variables that are guaranteed to be redundant.
   * It does this by inspecting the instructions along the path and finding
   * binary variables associated with a register or dereference expression
   * that is not killed by any instruction executed between the variable
   * addresses.
   * 
   */
  public static Map<String, Set<String>> computeRedundantVars(List<IInstruction> path) {

    // For the purposes of this code, we will say that an instruction occurs
    // at a particular `time' where time is just the instruction's index in the path.
    //
    // Given a binary variable <address>:<name>, we refer to <name> as its
    // `base name', and to <address>:<name> as its `full name'.
    
    // Maps base names to the last time the bv was killed. 
    Map<String, Integer> timeKilled = new LinkedHashMap<String, Integer>();
    
    // Maps base names to the last time a bv with the given base name
    // was declared a leader.
    Map<String, Integer> leaders = new LinkedHashMap<String, Integer>();
    
    // Maps full names to the set of other full names that represent
    // variables that are redundant to the key. We call the key the `leader'.
    Map<String, Set<String>> redundantVars = new LinkedHashMap<String, Set<String>>();

    // For debugging only: the set of all addresses seen so far.
    Set<String> addresses = new LinkedHashSet<String>();

    // We detect redundant variables as follows.
    // For each bv var that appears in the LHS of instruction i (appearing
    // at time t):
    //
    // if this is the first time we have seen var:
    //     make t:var a leader
    //     set its killed time to -1 (it's never been killed)
    // else (we've seen it before)
    //     if var was last killed after it was declared a leader
    //       make it a leader
    //     else
    //       it's redundant. Add it to the appropriate redundant set.
    for (int time = 0; time < path.size(); time++) {

      IInstruction instr = path.get(time);

//       System.out.println("INSTRUCTION>>>" + instr.getClass().getName() + instr.toString());
//       System.out.println("TIMEKILLEDP>>" + timeKilled);
//       System.out.println("LEADERS>>>" + leaders);
//       System.out.println("REDUNDANTVARS>>>" + redundantVars);

      if (!(instr instanceof KillerInstruction)) {

        // Sanity check: we're processing a new instruction.
        String instrAddress = instr.getAddress();
        assert !addresses.contains(instrAddress);
        addresses.add(instrAddress);

        for (String var : instr.getLHSVars()) {

          // If we've never seen this variable, make it a leader.
          if (!leaders.keySet().contains(var)) {
            leaders.put(var, time);
            timeKilled.put(var, -1);
            redundantVars.put(instrAddress + ":" + var,
                new LinkedHashSet<String>());

          } else { // If we've seen it, it may be a leader or redundant.
            assert timeKilled.get(var) != null;
            assert leaders.get(var) != null;
            if (timeKilled.get(var) >= leaders.get(var)) {
              // It was killed by a ghost instruction.
              // Make it a leader.
              leaders.put(var, time);
              redundantVars.put(instrAddress + ":" + var,
                  new LinkedHashSet<String>());

            } else {
              // It's redundant. Add it to redundant list.
              // Find the leader
              String leaderName = path.get(leaders.get(var)).getAddress() + ":"
                  + var;
              String varName = instr.getAddress() + ":" + var;
              redundantVars.get(leaderName).add(varName);
            }
          }
        }
      }
      // Update kill list.
      for (String var : leaders.keySet()) {
        if (instr.kills(var))
          timeKilled.put(var, time);
      }
    }
    // Keep only entries with redundant variables.
    Map<String, Set<String>> redundantVarsFinal = new LinkedHashMap<String, Set<String>>();
    for (Map.Entry<String, Set<String>> e : redundantVars.entrySet()) {
      if (!e.getValue().isEmpty())
        redundantVarsFinal.put(e.getKey(), e.getValue());
    }
    return redundantVarsFinal;
  }
}
