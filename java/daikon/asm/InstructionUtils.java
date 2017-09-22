package daikon.asm;

import daikon.util.*;
import java.util.*;

/*>>>
import org.checkerframework.checker.nullness.qual.*;
*/

/** Utility methods that operate on collections of instructions. */
public class InstructionUtils {

  /**
   * Computes a partition over the variables in the given path. Two variables are in the same
   * partition iff they are comparable. Two variables are comparable if they appear in the same
   * instruction.
   */
  public static Set<Set<String>> computeComparableSets(List<IInstruction> path) {

    // The elements of the partition.
    Set<String> vars = getAllVarNames(path);

    // Stores the sets that make up the partition. Initially, the
    // partition consists of singleton sets, one per variable.
    DSForest partition = new DSForest();
    for (String v : vars) {
      partition.add(v);
    }

    for (IInstruction instr : path) {

      if (instr instanceof X86Instruction) {
        processComparables((X86Instruction) instr, vars, partition);
      } else {
        assert instr instanceof KillerInstruction;
        for (X86Instruction ki : ((KillerInstruction) instr).getInstructions()) {
          processComparables(ki, vars, partition);
        }
      }
    }

    return partition.getSets();
  }

  private static void processComparables(
      X86Instruction instr, Set<String> vars, DSForest partition) {
    List<String> comparables = new ArrayList<String>();

    Set<String> bvs = instr.getBinaryVarNames();
    for (String v : vars) {
      if (bvs.contains(v)) {
        comparables.add(v);
        continue;
      }
      if (instr.kills(v)) {
        comparables.add(v);
        continue;
      }
    }
    if (comparables.size() <= 1) return; // Nothing to union.
    String e1 = comparables.get(0);
    for (int i = 1; i < comparables.size(); i++) {
      partition.union(e1, comparables.get(i));
    }
  }

  public static Set<String> getAllVarNames(List<IInstruction> path) {
    Set<String> ret = new LinkedHashSet<String>();
    for (IInstruction i : path) {
      ret.addAll(i.getBinaryVarNames());
    }
    return ret;
  }

  // Dead code. We used this to compute an upper bound on the number
  // of possible redundant variables, assuming that every reuse of
  // a variable was redundant.
  //
  // To compute the upper limit, replace the invocations of
  // computeRedundantVars() with this call instead.
  public static Map<String, Set<String>> computeRedundantVarsFake(List<IInstruction> path) {
    Map<String, Set<String>> redundants = new LinkedHashMap<String, Set<String>>();
    Map<String, String> leaders = new LinkedHashMap<String, String>();
    Set</*@KeyFor("leaders")*/ String> varsUsedPreviously =
        new LinkedHashSet</*@KeyFor("leaders")*/ String>();
    for (IInstruction instr : path) {
      for (String varName : instr.getBinaryVarNames()) {
        String varFullName = "bv:" + instr.getAddress() + ":" + varName;
        if (!varsUsedPreviously.contains(varName)) {
          // Make it a leader.
          redundants.put(varFullName, new LinkedHashSet<String>());
          leaders.put(varName, varFullName);
          varsUsedPreviously.add(varName);
        } else {
          // Add it to redundants.
          @SuppressWarnings(
              "nullness") // map: varName in varsUsedPreviously => all map keys OK, inserted on previous iteration
          boolean dummy = // to afford a place for the @SuppressWarnings annotation
              redundants.get(leaders.get(varName)).add(varFullName);
        }
      }
    }
    Map<String, Set<String>> redundantsFinal = new LinkedHashMap<String, Set<String>>();
    for (Map.Entry</*@KeyFor("redundants")*/ String, Set<String>> e : redundants.entrySet()) {
      if (!e.getValue().isEmpty()) {
        redundantsFinal.put(e.getKey(), e.getValue());
      }
    }
    return redundantsFinal;
  }

  /**
   * A second pass on rvars analysis. If a memory location is loaded onto a register, and the
   * register is used, the register's value will be equal to the memory location's.
   */
  public static void computeRVarsLoad(List<IInstruction> path, Map<String, String> rvars) {

    Set<String> allVarBases = new LinkedHashSet<String>();

    // Maps base names to the last time the bv was killed.
    Map<String, Integer> timeKilled = new LinkedHashMap<String, Integer>();

    for (int time = 0; time < path.size(); time++) {
      IInstruction instr = path.get(time);
      if (!(instr instanceof KillerInstruction)) {
        for (String varBase : instr.getBinaryVarNames()) {
          allVarBases.add(varBase);
          String var = "bv:" + instr.getAddress() + ":" + varBase;
          // If variable is already redundant, skip it.
          if (rvars.containsKey(var)) {
            continue;
          }
          // If last time variable var was killed was by a memory load
          // instruction like "mov_ld [memref] -> var", then make var
          // redundant and make [memref] its leader.
          if (!Operand.isRegister(varBase)) {
            continue;
          }
          Integer lastTimeKilled = timeKilled.get(varBase);
          if (lastTimeKilled == null) {
            continue;
          }
          IInstruction ii = path.get(lastTimeKilled);
          if (!(ii instanceof X86Instruction)) {
            continue;
          }
          X86Instruction mov_ld = (X86Instruction) ii;
          if (!mov_ld.getOpName().equals("mov_ld")) {
            continue;
          }
          if (!mov_ld.killedVars.contains(varBase)) {
            continue;
          }
          // bvs for mov_ld could be zero: for example, mov_ld [48178228+].
          if (mov_ld.getBinaryVarNames().size() == 0) {
            continue;
          }
          assert mov_ld.args.size() == 1;
          assert mov_ld.getBinaryVarNames().contains(mov_ld.args.get(0));
          String leaderBase = mov_ld.args.get(0);
          String leader = "bv:" + mov_ld.getAddress() + ":" + leaderBase;
          //           System.out.println("MOV_LD(" + var + ", " + leader + ")");
          //           System.out.println("  mov_ld instruction: " + mov_ld);
          //           System.out.println("  use    instruction: " + instr);
          rvars.put(var, leader);
        }
      }
      // Update kill list.
      for (String var : allVarBases) {
        if (instr.kills(var)) timeKilled.put(var, time);
      }
    }
  }

  /**
   * Computes a set of binary variables that are guaranteed to be redundant.
   *
   * <p>The redundant variables are returned as a map. Each entry &lt;rvar, leader&gt; represents a
   * redundant variable rvar and its leader. If a variable is not in the map, it is not redundant.
   */
  public static Map<String, String> computeRedundantVars(List<IInstruction> path) {

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

      if (!(instr instanceof KillerInstruction)) {

        // Sanity check: we're processing a new instruction.
        String instrAddress = instr.getAddress();
        assert !addresses.contains(instrAddress);
        addresses.add(instrAddress);

        for (String var : instr.getBinaryVarNames()) {

          // If we've never seen this variable, make it a leader.
          if (!leaders.containsKey(var)) {
            leaders.put(var, time);
            timeKilled.put(var, -1);
            redundantVars.put("bv:" + instrAddress + ":" + var, new LinkedHashSet<String>());

          } else { // If we've seen it, it may be a leader or redundant.
            assert leaders.containsKey(var);
            assert timeKilled.containsKey(var)
                : "@AssumeAssertion(keyfor): keys of leaders and timeKilled are the same";
            if (timeKilled.get(var) >= leaders.get(var)) {
              // It was killed by a killer instruction.
              // Make it a leader.
              leaders.put(var, time);
              redundantVars.put("bv:" + instrAddress + ":" + var, new LinkedHashSet<String>());

            } else {
              // It's redundant. Add it to redundant list.
              // Find the leader
              String leaderName = "bv:" + path.get(leaders.get(var)).getAddress() + ":" + var;
              String varName = "bv:" + instr.getAddress() + ":" + var;
              @SuppressWarnings("nullness") // map: was set on previous iteration
              /*@NonNull*/ Set<String> rset = redundantVars.get(leaderName);
              rset.add(varName);
            }
          }
        }
      }
      // Update kill list.
      for (String var : leaders.keySet()) {
        if (instr.kills(var)) timeKilled.put(var, time);
      }
    }

    // Keep only entries with redundant variables.
    // Also, compute statistics for redundant variables obtained via the analysis.
    int totalRedVars = 0;
    int totalVars = 0;
    Map<String, String> result = new LinkedHashMap<String, String>();
    //Map<String, Set<String>> redundantVarsFinal = new LinkedHashMap<String, Set<String>>();
    for (Map.Entry</*@KeyFor("redundantVars")*/ String, Set<String>> e : redundantVars.entrySet()) {
      totalVars++;
      if (!e.getValue().isEmpty()) {
        for (String rvar : e.getValue()) {
          result.put(rvar, e.getKey());
        }
        //redundantVarsFinal.put(e.getKey(), e.getValue());
        cum_redsperleader += e.getValue().size();
        sam_redsperleader++;
        totalRedVars += e.getValue().size();
      }
    }
    cum_redratio += (totalRedVars / (double) totalVars);
    sam_redratio++;

    computeRVarsLoad(path, result);

    //return redundantVarsFinal;
    return result;
  }

  private static double cum_redratio = 0;
  private static int sam_redratio = 0;
  private static double cum_redsperleader = 0;
  private static int sam_redsperleader = 0;

  public static void printStats() {
    System.out.println(
        "average redundant variable ratio per ppt: "
            + Double.toString(cum_redratio / sam_redratio));
    System.out.println(
        "average redundant variables per leader: "
            + Double.toString(cum_redsperleader / sam_redsperleader));
  }
}
