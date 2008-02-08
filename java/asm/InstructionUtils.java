package asm;

import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;


public class InstructionUtils {

  public static Map<String, Set<String>> computeRedundantVars(List<IInstruction> path) {

    Map<String, Integer> timeKilled = new LinkedHashMap<String, Integer>();
    Map<String, Integer> leaders = new LinkedHashMap<String, Integer>();
    Map<String, Set<String>> redundantVars = new LinkedHashMap<String, Set<String>>();

    Set<String> addresses = new LinkedHashSet<String>();

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
            // System.out.println("NEW " + var + " ENTRYSET" +
            // leaders.entrySet());
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
