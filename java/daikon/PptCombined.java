package daikon;

import daikon.derive.Derivation;
import daikon.derive.ValueAndModified;
import daikon.util.Pair;
import daikon.util.UtilMDE;
import daikon.util.GraphMDE;
import daikon.FileIO.ParentRelation;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintStream;
import java.util.*;

import daikon.asm.AsmFile;
import daikon.asm.IInstruction;
import daikon.asm.InstructionUtils;
import daikon.asm.KillerInstruction;
import daikon.asm.X86Instruction;

/**
 * A program point which consists of a number of program points.  Invariants
 * are looked for over all combinations of variables from all of the program
 * points that make up the combined ppt.
 */
@SuppressWarnings("nullness")   // defer annotating this class to later
public class PptCombined extends PptTopLevel {

  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20071129L;

  /** List of ppts that make up this combined ppt **/
  public List<PptTopLevel> ppts;

  private boolean debug = false;

  static int maxVarInfoSize = 10000;

  private static /*@LazyNonNull*/ AsmFile assemblies = null;

  /**
   * Map from the name of a redundant variable to its leader (the earliest
   * executed variable with guaranteed statically to have the same value).
   */
  public Map<String, String> rvars = null;

  /**
   * If non-null, we will compute redundant binary variables
   * when creating a CombinedProgramPoint, using
   * the assembly information in the file specified.
   */
  public static String dkconfig_asm_path_name = null;

  /**
   * If redundant variables are being computed, the results
   * of the redundancy analysis are printed to this stream.
   * See dkconfig_asm_path_name above.
   */
  public static String dkconfig_rvars_file = null;
  private static PrintStream rvars_stream = null;

  public PptCombined (List<PptTopLevel> ppts, CombinedVisResults vis) {

    // old name: ppts.get(0).name()+".."+ppts.get(ppts.size()-1).ppt_name.name()
    super (ppts.get(0).name() + ".." + short_component_str (ppts),
           PptType.COMBINED_BASIC_BLOCK,
           new ArrayList<ParentRelation>(), EnumSet.noneOf (PptFlags.class),
           null, ppts.get(0).function_id, -1, vis.var_infos);
    this.ppts = new ArrayList<PptTopLevel>(ppts);
    this.rvars = vis.rvarMap; // May be null.
    init();
    System.out.printf ("Combined ppt %s has %d variables%n", name(),
                       var_infos.length);

    if (debug) {
      for (VarInfo vi : var_infos) {
        System.out.printf ("  %s [%d/%d]%n", vi.name(), vi.varinfo_index,
                           vi.value_index);
      }
    }
  }

  private static void loadAssemblies(String assembliesFile) {
    if (assemblies == null)
      assemblies = AsmFile.getAsmFile(assembliesFile);
  }

  private static void openRvarsStream() {
    if (rvars_stream == null && dkconfig_rvars_file != null) {
      try {
        rvars_stream = new PrintStream(new FileOutputStream(dkconfig_rvars_file), true);
      } catch (FileNotFoundException e) {
        throw new RuntimeException(e);
      }
    }
  }

  // Preconditions: assemblies != null.
  private static Map<String, String> computeRedundantVariables(List<PptTopLevel> ppts) {
    assert assemblies != null;
    //System.out.println("Computing redundant variables in combined ppt...");

    // Create a list of instructions representing this ppt's execution flow.
    List<IInstruction> path = null;
    path = createPath(ppts);


    // We currently do nothing with the redundant variables information.
    Map<String,String> result = InstructionUtils.computeRedundantVars(path);

    // Debugging: print path.
    if (false) {
      System.out.println("\n\nPATH:");
      System.out.println(" NOTE: Instructions in blocks between dominators are preceded by \"*\"");
      System.out.println("       and may or may not have been executed in the path.");
      for (IInstruction instr : path) {
        if (instr instanceof X86Instruction) {
          debugPrint(instr, false, result);
        } else {
          for (X86Instruction potentialI : ((KillerInstruction)instr).getInstructions()) {
            debugPrint(potentialI, true, result);
          }
        }
      }
    }

    return result;
  }

  private static List<IInstruction> createPath(List<PptTopLevel> ppts) {
    List<IInstruction> path;
    path = new ArrayList<IInstruction>();
    for (int i = ppts.size() - 1; i >= 0; i--) {
      PptTopLevel ppt = ppts.get(i);
      List<X86Instruction> instructionsForPpt = assemblies.getInstructions(ppt.name());
      path.addAll (0, instructionsForPpt);
      if (i > 0) {
        // Find intermediate basic blocks: blocks that are on some path from
        // the current ppt and its immediate dominator (the dominator right
        // above the current basic block).
        List<PptTopLevel> interBlocks = findIntermediateBlocks(ppt, ppts.get(i - 1));

        if (interBlocks.size() > 0) {
          // Find the set of instructions over all intermediate blocks.
          Set<X86Instruction> intermediateBlocksInstrs = new LinkedHashSet<X86Instruction>();
          for (PptTopLevel interBlock : interBlocks) {
            List<X86Instruction> iis = assemblies.getInstructions(interBlock.name());
            intermediateBlocksInstrs.addAll(iis);
          }

          // Create a killer instruction representing the set of intermediate
          // block instructions.
          path.add(0, new KillerInstruction(intermediateBlocksInstrs));
        }
      }
    }
    return path;
  }

  private static void debugPrint(IInstruction instr, boolean isPotential,
                                 Map<String, String> redundantVariables) {
    System.out.print(UtilMDE.rpad((isPotential ? "*" : " ") + instr.toString(), 60));
    System.out.print("VARS: ");
    for (String s :  instr.getBinaryVarNames()) System.out.print(" " + s);
    System.out.println("  RVARS:" + printRedudant(instr, redundantVariables));
  }

  private static String printRedudant(IInstruction instr, Map<String, String> redundantVariables) {
    StringBuilder ret = new StringBuilder();
    for (String var : instr.getBinaryVarNames()) {
      String fullName = "bv:" + instr.getAddress() + ":" + var;
      String leader = redundantVariables.get(fullName);
      if (leader == null)
        continue;
      String leaderVarName = leader.split(":")[1];
      ret.append(var + " (" + leaderVarName + ")");
    }
    return ret.toString();
  }

  // Checks that variables in var_infos are the same are the variables
  // obtained from the asm file for this set of basic blocks.
  // Happens to return number of variables (TODO remove this ugliness).
  private static int checkVarsOk(List<PptTopLevel> ppts, List<VarInfo> list) {

    // Create the set of variables in var_infos.
    Set<String> varsFromPpts = new LinkedHashSet<String>();
    for (VarInfo vi : list) {
      varsFromPpts.add(vi.name());
    }

    // Create the set of variables in asm file.
    Set<String> varsFromAsm = new LinkedHashSet<String>();
    for (PptTopLevel p : ppts) {
      boolean firstInst = true;

      List<X86Instruction> instructions = assemblies.getInstructions(p.name());

      for (IInstruction i : instructions) {
        if (firstInst) {
          // The dynamic technique always creates a variable
          // for esp at the first address in the block.
          // We add it here for comparison purposes.
          varsFromAsm.add("bv:" + i.getAddress() + ":" + "esp");
          firstInst = false;
        }

        for (String var : i.getBinaryVarNames()) {
          if (var.startsWith("[")) {
            // For a variable of the form [x] (dereference),
            // the dynamic technique always creates variable x.
            // We add it here for comparison purposes.
            //
            // FIX this is a misleading variable list!
            varsFromAsm.add("bv:" + i.getAddress() + ":" + var.substring(1, var.length() - 1));

          }
          varsFromAsm.add("bv:" + i.getAddress() + ":" + var);
        }
      }
    }

    Set<String> all = new LinkedHashSet<String>();
    all.addAll(varsFromPpts);
    all.addAll(varsFromAsm);
    List<String> allSorted = new ArrayList<String>(all);
    Collections.sort(allSorted);

    if (!varsFromPpts.equals(varsFromAsm)) {
      System.out.println("ERROR: mismatched variables in combined ppt vs. asm file:");
      System.out.println(UtilMDE.rpad("from var_infos:", 30) + UtilMDE.rpad("from asm file:", 30));
      for (String s : allSorted) {
        if (!iff(varsFromPpts.contains(s), varsFromAsm.contains(s))) {
          System.out.print(UtilMDE.rpad(varsFromPpts.contains(s) ? s : "", 30));
          System.out.print(UtilMDE.rpad(varsFromAsm.contains(s) ? s : "", 30));
          System.out.println();
        }
      }
      //throw new RuntimeException();
    }
    return varsFromPpts.size();
  }

  private static boolean iff(boolean b, boolean c) {
    if (b) {
      if (c) return true; // b true, c true
      else return false; // b true, c false
    } else {
      if (!c) return true; // b false, c false
      else return false; // b false, c true
    }
  }


  public static List<PptTopLevel> findIntermediateBlocks(PptTopLevel dest, PptTopLevel source) {

    Set<PptTopLevel> visited = new LinkedHashSet<PptTopLevel>();
    Queue<PptTopLevel> toProcess = new LinkedList<PptTopLevel>();
    toProcess.addAll(dest.predecessors);
    if (!dest.predecessors.contains(dest)) {
      // Check to see if this basic block really does not have a
      // self-loop. If it does, output warning message (but in galar)
      // and add the loop ourselves.
      if (has_self_cycle(dest)) {
        System.out.println("ERROR: Missing self-loop in successor list of: " + dest.toString());
        toProcess.add(dest);
      }
    }
    while (!toProcess.isEmpty()) {
      PptTopLevel p = toProcess.poll();
      if (p == source)
        continue;
      visited.add(p);
      for (PptTopLevel parent: p.predecessors) {
        if (!visited.contains(parent))
          toProcess.add(parent);
      }
    }
    return new ArrayList<PptTopLevel>(visited);
  }

  // WORKAROUND FOR BUG IN GALAR, which does not output self-cycles.
  private static boolean has_self_cycle(PptTopLevel ppt) {
    assert assemblies != null;

    // Find out of last instruction is a jump instruction.
    List<X86Instruction> instructionsForPpt = assemblies.getInstructions(ppt.name());
    if (instructionsForPpt.size() == 0)
      return false;
    X86Instruction lastInstr = instructionsForPpt.get(instructionsForPpt.size() -1);
    if (lastInstr.getOpName().charAt(0) != 'j') // It's not a jump instruction
      return false;

    // Last instruction is a jump. Find the last 4 digits of the jump address.
    assert lastInstr.getArgs().size() == 1 : lastInstr.toString();
    String operand = lastInstr.getArgs().get(0);
    if (!operand.startsWith("$0x"))
      return false; // Not of the form "<jump instr> <constant>"
    String operand_last_4_digits = last_4_digits(operand.substring(1) /* without the $ */);

    // Now, find the last 4 digits of this block's address.
    assert ppt.name().indexOf(":0x") != -1;
    int start = ppt.name().indexOf(":0x") + 1;
    assert ppt.name().indexOf(":::") != -1;
    int end = ppt.name().indexOf(":::");
    String block_last_4_digits = last_4_digits(ppt.name().substring(start, end));

    // If the last 4 digits of the block's address and the jump
    // address match, the block has a self-cycle.
    if (operand_last_4_digits.equals(block_last_4_digits))
      return true;

    return false;

  }

  private static String last_4_digits(String address) {
    assert address.startsWith("0x") : address;
    return address.substring(address.length() - 4, address.length());
  }

/** Returns a name basic on its constituent ppts **/
/*
  public String name() {
    String name = ppts.get(0).name();
    name += ".." + ppts.get(ppts.size()-1).ppt_name.name();
    return name;
  }
*/
  /**
   * Initialize the ppt.  This is similar to init_ppt in Daikon.java
   * except that orig variables are never created (they don't make sense
   * in this context).  Splitters are also not created.  Equality ses are
   * always setup (since this is always a leaf in the hierarchy)
   **/
  private void init() {
    if (!Derivation.dkconfig_disable_derived_variables) {
      create_derived_variables();
    }

    if (!Daikon.using_DaikonSimple && Daikon.use_equality_optimization) {
      equality_view = new PptSliceEquality(this);
      equality_view.instantiate_invariants();
    }
  }

  /**
   * Add the current sample.  The last samples for each of the program
   * points that make up the combined program point must have been added
   * to their last_values field.
   */
  public void add_combined() {

    // Number of values in the combined ValueTuple
    int vals_array_size = var_infos.length - num_static_constant_vars;

    // Allocate arrays for the combined values and mod information
    Object[] vals = new Object[vals_array_size];
    int[] mods = new int[vals_array_size];
    ValueTuple partial_vt = ValueTuple.makeUninterned (vals, mods);

    // Copy the values from each constituent program point
    int index = 0;
    for (PptTopLevel ppt : ppts) {
      int filled_slots = ppt.num_orig_vars + ppt.num_tracevars;
      for (int i = 0; i < filled_slots; i++) {

        // If var was statically found to be redundant, don't add.
        if (rvars != null) {
          VarInfo vi = ppt.var_infos[i];
          if (rvars.containsKey(vi.name())) {
            assert !vi.vardef.declared_type.is_function_pointer();
            continue;
          }
        }

        if (ppt.last_values == null) {
          //System.out.printf ("no last valfor %s in %s%n", ppt.name(), name());
          vals[index] = null;
          mods[index] = ValueTuple.MISSING_NONSENSICAL;
        } else {
          vals[index] = ppt.last_values.vals[i];
          // we now have some nonsensical values in our input
          // assert (vals[index] != null);
          mods[index] = ppt.last_values.mods[i];
        }
        assert (!ppt.var_infos[i].isDerived());
        index++;
      }
    }
    assert (index == (num_orig_vars + num_tracevars));
    assert (index == vals_array_size);

    // add the derived variables
    while (index < vals_array_size) {
      assert (var_infos[index].isDerived());
      ValueAndModified vm =
        var_infos[index].derived.computeValueAndModified(partial_vt);
      vals[index] = vm.value;
      mods[index]= vm.modified;
      index++;
    }

    // Create an interned ValueTuple
    ValueTuple vt = new ValueTuple (vals, mods);

    // Add the sample
    add_bottom_up (vt, 1);

  }

  private static class CombinedVisResults {
    public final VarInfo[] var_infos;
    public final Map<String,String> rvarMap; // May be null.
    public CombinedVisResults(VarInfo[] vis, Map<String,String> rvarMap) {
      this.var_infos = vis;
      this.rvarMap = rvarMap;
    }
  }

  /**
   * Build a combined VarInfo array for all of the ppts.
   * If dkconfig_asm_path_name was given as a configuration option,
   * compute redundant variables from the asm file, and do not
   * include them in the varInfo array.
   */
  private static CombinedVisResults combined_vis (List<PptTopLevel> ppts) {

    assert (ppts.size() > 0) : "No ppts in list";

    // Create a list of candidate varinfos that may become part
    // of the final list.
    List<VarInfo> candidateList = new ArrayList<VarInfo>();
    for (PptTopLevel ppt : ppts) {
      for (VarInfo vi : ppt.var_infos) {
        if (vi.isDerived())
          continue;
        candidateList.add(new VarInfo(vi.vardef));
      }
    }


    Map<String, String> redundantVariables = null;

    // Compute redudant binary variables.
    if (dkconfig_asm_path_name != null) {

      // Load the asm file. If already loaded, does nothing.
      loadAssemblies(dkconfig_asm_path_name);

      // Open the rvars output stream. If already open, does nothing.
      openRvarsStream();

      // Sanity check: same variables obtained statically and dynamically.
      if (true) checkVarsOk(ppts, candidateList);

      // Compute redundant variables for the variables in the given ppts
      // using the information in the asm file.
      redundantVariables = computeRedundantVariables(ppts);

      System.out.println("Redundant variable (static) analysis found " + redundantVariables.size()
                         + "/" + candidateList.size() + " rvars.");

      // Print out rvars info.
      String cppt_name = ppts.get(0).name() + ".." + ppts.get(ppts.size() - 1).ppt_name.name();
      if (false) {
        System.out.println("Redundant vars for ppt " + cppt_name);
        for (Map.Entry<String, String> e : redundantVariables.entrySet()) {
          System.out.println("   " + e.getKey() + "(" + e.getValue() + ")");
        }
        System.out.println("End redundant vars");
      }
      if (dkconfig_rvars_file != null) {
        assert rvars_stream != null;
        rvars_stream.println("===========================================================================");
        rvars_stream.println(cppt_name);
        for (Map.Entry<String, String> e : redundantVariables.entrySet()) {
          rvars_stream.println(e.getKey() + "(" + e.getValue() + ")");
        }
      }
    }

    // Create a new VarInfo for each VarInfo in the ppt list.
    // If redundantVariables != null, don't add redundant variables.
    List<VarInfo> finalList = new ArrayList<VarInfo>();
    Map<String, String> finalRvars = new LinkedHashMap<String, String>();
    for (VarInfo vi : candidateList) {

      boolean is_function_ptr = vi.vardef.declared_type.is_function_pointer();

      if (is_function_ptr) {
        // Variable's type is is function pointers. Always add it to
        // the var_info list, and never to the rvars map.
        finalList.add(vi);

      } else if (redundantVariables != null
                 && redundantVariables.containsKey(vi.name())) {

        // Variable is considered redundant. Don't add it to var_info
        // list, and add it to final rvars map.
        finalRvars.put(vi.name(), redundantVariables.get(vi.name()));
      } else {

        // Variable is not redundant. Add it to var_info list, but
        // don't add it to rvars map.
        finalList.add(vi);
      }
    }

    return new CombinedVisResults(finalList.toArray(new VarInfo[0]), finalRvars);
  }


  /**
   * Creates combined program points that cover multiple basic blocks.
   * Given a list of basic block ppts, each one is made into a combined
   * program point along with any basic blocks that pre-dominate it (always
   * execute previously to it).
   *
   * The input is a list of the basic block ppts that make up the
   * function.  The first element in the list is the function entry.
   * In each bb ppt, the field ppt_successors contains a list of the names
   * of all of the basic blocks that directly succeed it.  That list
   * is used to calculate the dominators.
   *
   * The resulting combined ppt has samples added to it when its
   * 'trigger' ppt is executed.  The trigger is always the last ppt in
   * the combined program point.  The trigger is thus dominated by all
   * of the other basic blocks in the combined ppt.  That guarantees
   * that samples for those basic blocks were received before the
   * trigger.  Those samples are just saved away when they are
   * received.  When the trigger ppt is executed, its samples are
   * combined with the samples from the other (previously executed)
   * basic blocks and the combined sample is processed by the combined
   * program point.
   *
   * It is not necessary to create a unique combined program point for
   * each basic block.  Consider two basic blocks (A and B).  If A is
   * a pre-dominator of B, A will be included in B's combined program
   * point.  If A is post-dominated by B, it can share B's combined
   * program point (because the combined program point for B will have
   * seen all of the samples for A).  We say that A is 'subsumed by'
   * B.
   *
   * Each program point (referred to as P) in the function is
   * modified as follows: <ul>
   *   <li> P's combined_ppts_init flag is set to true.
   *   <li> P's combined_ppt field is set to point to the (newly created)
   *    combined ppt that will contain its invariants.  The trigger for that
   *    combined ppt is P's last postdominator that P predominates (which
   *    may be P itself).  This
   *    combined ppt must see all of the samples for P.  That implies that
   *    the trigger for the combined ppt must post-dominate P.  This is
   *    obviously true when P is the trigger.
   *   <li> If P is not the trigger, its combined_subsumed boolean field is
   *    set to true.
   * </ul>
   * Invariants over P:
   *      P.combined_ppt != null
   *      P.combined_subsumed==true implies
   *        P.combined_ppt.trigger post-dominates P
   *      P.combine_subsumed==false implies
   *        P.combined_ppt.trigger == P
   *
   * Invariants over the combined ppt C:
   *    C.ppts[i] pre-dominates C.ppts[i+1]
   *    trigger = C.ppts[sizeof(C.ppts)-1]
   *    trigger.combine_subsumed==false;
   *    trigger.combined_ppt == C
   *    for (P : C.ppts[0..sizeof(C.ppts)-2])
   *      P.combined_subsumed==false implies P.combined_ppt != C
   *      P.combined_ppt == C implies Trigger post-dominates P
   *
   *  Note that trigger is not an actual field of PptCombined (though it
   *  could be).  But it should always be the last ppt in the list of
   *  ppts in the combined ppt:
   *
   *    trigger = PptCombined.ppts.get(PptCombined.ppts.size()-1)
   */
  public static void combine_func_ppts (PptMap all_ppts,
          List<PptTopLevel> func_ppts) {

    // Compute convenience maps: graph successors and predecessors
    Map<PptTopLevel,List<PptTopLevel>> succ = new LinkedHashMap<PptTopLevel,List<PptTopLevel>>(func_ppts.size());
    Map<PptTopLevel,List<PptTopLevel>> pred = new LinkedHashMap<PptTopLevel,List<PptTopLevel>>(func_ppts.size());
    for (PptTopLevel ppt : func_ppts) {
      succ.put(ppt, new ArrayList<PptTopLevel>());
      pred.put(ppt, new ArrayList<PptTopLevel>());
    }
    for (PptTopLevel ppt : func_ppts) {
      if (ppt.ppt_successors != null) {
        for (String succName : ppt.ppt_successors) {
          PptTopLevel succPpt = all_ppts.get(succName);
          succ.get(ppt).add(succPpt);
          pred.get(succPpt).add(ppt);
        }
      }
    }

    // Compute pre- and post-dominators
    Map<PptTopLevel,List<PptTopLevel>> predoms = GraphMDE.dominators(pred);
    Map<PptTopLevel,List<PptTopLevel>> postdoms = GraphMDE.dominators(succ);

    /// Debugging
    // for (PptTopLevel ppt : func_ppts) {
    //   assert predoms.get(ppt).contains(ppt);
    //   assert postdoms.get(ppt).contains(ppt);
    // }
    //
    // if (false) {
    //   System.out.println("predominators:");
    //   GraphMDE.print(predoms, System.out, 2);
    //   System.out.println("postdominators:");
    //   GraphMDE.print(postdoms, System.out, 2);
    // }

    // Find all post-dominators of each program point that are
    // pre-dominated by it.
    // "pp" = "predominated_postdominators".
    Map<PptTopLevel,List<PptTopLevel>> pp = new LinkedHashMap<PptTopLevel,List<PptTopLevel>>();
    for (PptTopLevel ppt : func_ppts) {
      List<PptTopLevel> this_pp = new ArrayList<PptTopLevel>();
      pp.put(ppt, this_pp);
      for (PptTopLevel candidate : postdoms.get(ppt)) {
        if (predoms.get(candidate).contains(ppt)) {
          this_pp.add(candidate);
        }
      }
    }

    // // Sanity checks
    // assert pp.keySet().size() == func_ppts.size();
    // for (PptTopLevel ppt : func_ppts) {
    //   List<PptTopLevel> this_pp = pp.get(ppt);
    //   if (! this_pp.contains(ppt)) {
    //     System.out.printf("ppt: %s, this_pp.size %s%n", ppt, this_pp.size());
    //     for (PptTopLevel pp_elt : this_pp) {
    //       System.out.printf("pp_elt: %s%n", pp_elt);
    //     }
    //   }
    //   assert this_pp.contains(ppt);
    // }
    // for (PptTopLevel ppt1 : func_ppts) {
    //   List<PptTopLevel> this_pp = pp.get(ppt1);
    //   for (PptTopLevel ppt2 : this_pp) {
    //     assert this_pp.containsAll(pp.get(ppt2));
    //   }
    // }


    // For convenience & debuggability, sort each element of this_pp into order:
    // each element in the list pre-dominates everything that follows it.
    // This implementation is a bit inefficient, but so be it.
    // It should be sufficient to sort according to the number of elements
    // in each one's pp entry.
    PpSizeComparator ppComparator = new PpSizeComparator(pp);
    for (PptTopLevel ppt : func_ppts) {
      Collections.sort(pp.get(ppt), ppComparator);
    }
    // Now, the trigger is the last element of each list.


    // Make the combined program points.
    Map<PptTopLevel,PptCombined> trigger_comb = new LinkedHashMap<PptTopLevel, PptCombined>();
    for (PptTopLevel ppt : func_ppts) {
      List<PptTopLevel> this_pp = pp.get(ppt);
      if (this_pp.get(this_pp.size()-1) == ppt) {
        List<PptTopLevel> ppts_to_combine = new ArrayList<PptTopLevel>(predoms.get(ppt));
        PpSizeComparator predomComparator = new PpSizeComparator(predoms);
        Collections.sort(ppts_to_combine, predomComparator);
        PptCombined pptc = new PptCombined(ppts_to_combine, combined_vis(ppts_to_combine));
        trigger_comb.put(ppt, pptc);
      }
    }

    // Actually do the work of side-effecting the ppts.
    for (PptTopLevel ppt : func_ppts) {
      List<PptTopLevel> this_pp = pp.get(ppt);
      PptTopLevel this_trigger = this_pp.get(this_pp.size()-1);

      ppt.combined_ppts_init = true;
      ppt.combined_ppt = trigger_comb.get(this_trigger);
      assert ppt.combined_ppt != null;
      assert ppt.combined_ppt.ppts.contains(ppt);
      ppt.combined_subsumed = (this_trigger != ppt);
    }

  }

  /** Given a map, sort elements of the map according to the size of the mapped-to list. **/
  public static final class PpSizeComparator implements Comparator<PptTopLevel> {
    Map<PptTopLevel,List<PptTopLevel>> pp;
    public PpSizeComparator(Map<PptTopLevel,List<PptTopLevel>> pp) {
      this.pp = pp;
    }
    public int compare(PptTopLevel ppt1, PptTopLevel ppt2) {
      if (ppt1 == ppt2)
        return 0;
      int len1 = pp.get(ppt1).size();
      int len2 = pp.get(ppt2).size();
      return len1 - len2;
    }
  }



  /**
   * Checks the combined program point for correctness.  Returns true
   * if all is well.  Prints messages preceeded with 'ERROR' to stdout
   * and returns false on errors.
   */
  public boolean check() {

    System.out.printf ("Checking combined ppt %s\n", name());

    if (ppts.size() <= 0) {
      System.out.printf ("ERROR: Size of %s ppts is %d\n", name(), ppts.size());
      return false;
    }

    // Make sure that every ppt has a combined_ppt
    for (PptTopLevel ppt : ppts) {
      if (ppt.combined_ppt == null) {
        System.out.printf ("ERROR: ppt %s has no combined ppt\n", ppt);
        return false;
      }
    }

    PptTopLevel trigger;
    boolean ppts_in_dominator_order = true;

    if (ppts_in_dominator_order) {

      // Make sure that each block is dominated by the previous one
      for (int i = ppts.size()-1; i > 0; i--) {
        PptTopLevel ppt = ppts.get (i);
        PptTopLevel prev = ppts.get(i-1);
        // System.out.printf ("Checking %s dominated by %s\n", bb_short_name(ppt),
        //                   bb_short_name (prev));
        if (!ppt.all_predecessors_goto (prev)) {
          System.out.printf ("ERROR: ppt %s not dominated by ppt %s\n",
                             ppt.name(), prev.name());
          return false;
        }
      }

      // Make sure that the last ppt is the only trigger for the combined ppt
      trigger = ppts.get(ppts.size()-1);
      if (trigger.combined_ppt != this) {
        System.out.printf ("ERROR: trigger ppt %s combined ppt is %s not %s\n",
                           trigger, trigger.combined_ppt, combined_ppt);
        return false;
      }
      if (trigger.combined_subsumed) {
        System.out.printf ("ERROR: trigger ppt %s is combined_subsumed in %s\n",
                           trigger, this);
        return false;
      }
      for (int i = 0; i < ppts.size()-1; i++) {
        PptTopLevel ppt = ppts.get(i);
        if (!ppt.combined_subsumed && (ppt.combined_ppt == this)) {
          System.out.printf ("ERROR: multiple triggers (%s) to %s\n", ppt,
                             this);
          return false;
        }
      }
    } else { // ppts are NOT in dominator order

      //Find the trigger (and make sure there is only one)
      trigger = null;
      for (PptTopLevel ppt : ppts) {
        if (!ppt.combined_subsumed && (ppt.combined_ppt == this)) {
          if (trigger == null)
            trigger = ppt;
          else {
            System.out.printf ("ERROR: multiple triggers (%s,%s) for %s\n",
                               trigger, ppt, this);
            return false;
          }
        }
      }

      // Make sure every other block pre-dominates the trigger
      for (PptTopLevel ppt : ppts) {
        if (ppt == trigger)
          continue;
        if (!trigger.all_predecessors_goto (ppt)) {
          System.out.printf ("ERROR: %s does not pre-dominate trigger %s\n",
                             ppt, trigger);
          return false;
        }
      }
    }

    // Make sure that every ppt that is combined_subsumed and has this
    // combined ppt always flows to the trigger.  This guarantees that
    // all ppts whose invariants are calculated in this combined ppt
    // see all of their samples
    for (PptTopLevel ppt : ppts) {
      if (ppt == trigger)
        continue;
      if (ppt.combined_subsumed && (ppt.combined_ppt == this)) {
        if (!ppt.all_successors_goto (trigger)) {
          System.out.printf ("ERROR ppt %s does not flow to trigger ppt %s\n",
                             ppt, trigger);
          return false;
        }
      }
    }

    System.out.printf ("Finished checking combined ppt %s\n", name());
    return true;

  }

  /**
   * Checks all of the ppts in a function for validity after combined
   * program points are created.  Only performs checks that can't be
   * done in check.  Returns true if all is well.  Prints messages
   * preceeded with 'ERROR' to stdout and returns false on errors.
   */
  public static boolean check_func_ppts (List<PptTopLevel> ppts) {

    // Make sure every ppt was placed in a combined ppt
    for (PptTopLevel ppt : ppts) {
      if (ppt.combined_ppt == null) {
        System.out.printf ("ERROR: Ppt %s doesn't refer to a combined ppt\n",
                           ppt);
        return (false);
      }
      if (!ppt.combined_ppt.ppts.contains (ppt)) {
        System.out.printf ("ERROR: ppt %s in combined ppt %s, "
                           + "is not in its ppt list %s\n",
                           ppt, ppt.combined_ppt,
                           ppt.combined_ppt.ppts);
        return false;
      }
    }

    return true;
  }

  /** Dumps out the basic blocks that make up this combined ppt **/
  public void dump() {
    System.out.printf ("    Combined PPT %s\n", name());
    dump (ppts);
  }

  /** Dumps out the basic blocks in the list  **/
  public static void dump(List<PptTopLevel> ppts) {

    for (PptTopLevel ppt : ppts) {
      String succs = "";
      if (ppt.ppt_successors != null) {
        for (String succ : ppt.ppt_successors) {
          PptTopLevel ppt_succ = Daikon.all_ppts.get (succ);
          if (succs == "")      // "interned"
            succs = bb_short_name (ppt_succ);
          else
            succs += " " + bb_short_name (ppt_succ);
        }
      }
      String preds = "";
      if (ppt.predecessors != null) {
        for (PptTopLevel pred : ppt.predecessors) {
          if (preds == "")      // "interned"
            preds = bb_short_name (pred);
          else
            preds += " " + bb_short_name (pred);
        }
      }
      System.out.printf ("      %s: [%s] {%s} %s "
                         + "combined_ppt: %s\n",
                         bb_short_name (ppt), succs, preds,
                         ppt.combined_subsumed ? "subsumed" : "trigger",
                         ((ppt.combined_ppt == null)
                          ? "null" : ppt.combined_ppt.short_name()));
    }
  }


  public String short_name() {
    String start = bb_short_name(ppts.get(0));
    String end = bb_short_name(ppts.get(ppts.size()-1));
    return (start + ".." + end);
  }

  /** Returns a list of the component ppts that make up this combined ppt **/
  public String short_component_str() {
    return short_component_str (ppts);
  }

  /** Returns a list of the component ppts that make up this combined ppt **/
  public static String short_component_str(List<PptTopLevel> ppts) {
    StringBuilder sb = new StringBuilder();
    for (PptTopLevel ppt : ppts) {
      sb.append (String.format ("%s-", bb_short_name (ppt)));
    }
    return sb.deleteCharAt (sb.length()-1).toString();
  }


  public static String bb_short_name (PptTopLevel ppt) {
    if (ppt == null)
      return "null";
    return String.format ("%04X", ppt.bb_offset() & 0xFFFF);
  }

  public static void main(String[] args) throws IOException {

    // Load the asm file.
    loadAssemblies(args[1]);

    // Read in the invariants
    System.out.println("Reading invariants...");
    String filename = args[0];
    PptMap ppts = FileIO.read_serialized_pptmap(new File(filename),
                                                true // use saved config
                                                );

    PptTopLevel.succ_map = new LinkedHashSet<String>();
    PptTopLevel.pred_map = new LinkedHashSet<String>();
    Daikon.all_ppts = ppts;

    dkconfig_asm_path_name = args[1];

    for (PptTopLevel p : ppts.all_ppts()) {
      if (p instanceof PptCombined) {
        PptCombined cp = (PptCombined)p;
        CombinedVisResults res = combined_vis(cp.ppts); // This calls the redundancy computing code.
        cp.rvars = res.rvarMap;
      }
    }

    System.out.println("Testing redundant variables...");
    int exitCode = redundantVarsTest(ppts);

    System.exit(exitCode);
  }

  // Checks that if two variables are said to be redundant by Carlos's
  // analysis, they are deemed equal by Daikon's dynamic analysis.
  //
  // Returns 0 if all tests pass, 1 if something goes wrong.
  public static int redundantVarsTest(PptMap all_ppts) {
    int retval = 0;
    for (PptTopLevel ppt : all_ppts.all_ppts()) {
      if (ppt instanceof PptCombined) {
        PptCombined cp = (PptCombined) ppt;
        int numRedVars = 0;

        for (Map.Entry<String, String> e : cp.rvars.entrySet()) {
          String rvar = e.getKey();
          String leader = e.getValue();
          //System.out.println("Testing " + rvar);
          numRedVars++;

          if (cp.num_samples() == 0)
            continue;

          VarInfo leaderVI = cp.var_infos[cp.indexOf(leader)];

          // The rvar will should not be in cp.var_infos because
          // it was deemed redundant. However, it should still be part of
          // the var_infos for a child program point.
          VarInfo rvarVI = cp.var_infos[cp.indexOf(rvar)];

          if (!cp.is_equal(leaderVI.canonicalRep(), rvarVI.canonicalRep())) {
            //printNumSamples(cp);
            if (!cp.check()) // This signals an error in dominator computation.
              continue;

            retval = 1;

            String msg = "\n\nBEGIN ERROR: Two variables found redundant via static analysis\n"
              + "are not equal dynamically. The two variables are:"
              + "\nleader: " + leaderVI.toString()
              + "\nrvar:" + rvarVI.toString()
              + "\nMore information about the variabes and their ppts:"
              + "\n   PPT name: " + ppt.name()
              + "\n   num_samples=" + cp.num_samples()
              + "\n   rvar samples=" + cp.num_samples(rvarVI)
              + "\n   rvar is_missing=" + cp.is_missing(rvarVI)
              + "\n   leader samples=" + cp.num_samples(leaderVI)
              + "\n   leader is_missing=" + cp.is_missing(leaderVI)
              + "\n   rvar+leader samples=" + cp.num_samples(rvarVI,leaderVI)
              + "\n   leader value set:" + cp.value_sets[leaderVI.value_index].repr_short()
              + "\n   rvar value set:" + cp.value_sets[rvarVI.value_index].repr_short();
            System.out.println(msg);
            System.out.println("   Combined ppt:");
            System.out.println("      name=" + cp.name());
            System.out.println("      id=" + System.identityHashCode(cp));
            System.out.println("      samples " + cp.num_samples());
            System.out.println("      children ppts:");
            for (PptTopLevel p : cp.ppts) {
              System.out.println("         " + p.name());
            }

            // Create a list of instructions representing this ppt's execution flow.
            System.out.println("   Instruction path, including potential blocks between dominators:");
            List<IInstruction> path = null;
            path = createPath(cp.ppts);
            System.out.println(" NOTE: Instructions in blocks between dominators are preceded by \"*\"");
            System.out.println("       and may or may not have been executed in the path.");
            for (IInstruction instr : path) {
              if (instr instanceof X86Instruction) {
                debugPrint(instr, false, cp.rvars);
              } else {
                for (X86Instruction potentialI : ((KillerInstruction)instr).getInstructions()) {
                  debugPrint(potentialI, true, cp.rvars);
                }
              }
            }

            System.out.println("END ERROR");
            //throw new RuntimeException(msg);
          } else {
            //System.out.print("!");
          }
        }
      }
    }
    return retval;
  }

}
