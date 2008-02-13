package daikon;

import daikon.derive.Derivation;
import daikon.derive.ValueAndModified;
import daikon.util.CollectionsExt;
import daikon.util.UtilMDE;
import daikon.FileIO.ParentRelation;

import java.util.*;

import asm.AsmFile;
import asm.IInstruction;
import asm.InstructionUtils;
import asm.KillerInstruction;
import asm.X86Instruction;

/**
 * A program point which consists of a number of program points.  Invariants
 * are looked for over all combinations of variables from all of the program
 * points that make up the combined ppt.
 */
public class PptCombined extends PptTopLevel {

  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20071129L;

  /** List of ppts that make up this combined ppt **/
  public List<PptTopLevel> ppts;

  private boolean debug = false;

  static int maxVarInfoSize = 10000;

  private static AsmFile assemblies = null;

  /**
   * If non-null, we will compute redundant binary variables
   * when creating a CombinedProgramPoint, using
   * the assembly information in the file specified.
   */
  public static String dkconfig_asm_path_name = null;

  public PptCombined (List<PptTopLevel> ppts) {

    super (ppts.get(0).name() + ".." + ppts.get(ppts.size()-1).ppt_name.name(),
           PptType.COMBINED_BASIC_BLOCK,
           new ArrayList<ParentRelation>(), EnumSet.noneOf (PptFlags.class),
           null, ppts.get(0).function_id, -1, combined_vis (ppts));
    this.ppts = new ArrayList<PptTopLevel>(ppts);
    init();
    System.out.printf ("Combined ppt %s has %d variables%n", name(),
                       var_infos.length);

    // Compute redudant binary variables.
    if (dkconfig_asm_path_name != null) {
    	loadAssemblies(dkconfig_asm_path_name);
    	computeRedundantVariables();
    }

    if (debug) {
      for (VarInfo vi : var_infos) {
        System.out.printf ("  %s [%d/%d]%n", vi.name(), vi.varinfo_index,
                           vi.value_index);
      }
    }
  }

  // Preconditions: dkconfig_asm_path_name != null
  private void loadAssemblies(String assembliesFile) {
    assert dkconfig_asm_path_name != null;

    if (assemblies == null)
      assemblies = AsmFile.getAsmFile(assembliesFile);
  }

  // Preconditions: assemblies != null.
  private void computeRedundantVariables() {
    assert assemblies != null;
    System.out.println("Computing redundant variables in combined ppt...");

    // Create a list of instructions representing this ppt's execution flow.
    List<IInstruction> path = new ArrayList<IInstruction>();
    for (int i = ppts.size() - 1; i >= 0; i--) {
      PptTopLevel ppt = ppts.get(i);
      List<X86Instruction> instructionsForPpt = assemblies.getInstructions(ppt.name());
      CollectionsExt.prepend(instructionsForPpt, path);
      if (i > 0) {
        // Find intermediate basic blocks: blocks that are on some path from
        // the current ppt and its immediate dominator (the dominator right
        // above the current basic block).
        List<PptTopLevel> interBlocks = findIntermediateBlocks(ppt, ppts.get(i - 1));

        if (interBlocks.size() > 0) {
          // Find the set of instructions over all intermediate blocks.
          Set<IInstruction> intermediateBlocksInstrs = new LinkedHashSet<IInstruction>();
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

    // Debugging: print path.
    if (false) {
      System.out.println("PATH:");
      for (IInstruction instr : path)
        System.out.println(instr);
    }

    // Sanity check: same variables obtained statically and dynamically.
    //checkVarsOk();

    // We currently do nothing with the redundant variables information.
    Map<String, Set<String>> reds = InstructionUtils.computeRedundantVars(path);
    printKillers(reds);
  }

  // Checks that variables in var_infos are the same are the variables
  // obtained from the asm file for this set of basic blocks.
  private void checkVarsOk() {

    // Create the set of variables in var_infos.
    Set<String> varsFromPpts = new LinkedHashSet<String>();
    for (VarInfo vi : var_infos)
      varsFromPpts.add(vi.name());

    // Create the set of variables in asm file.
    Set<String> varsFromAsm = new LinkedHashSet<String>();
    for (PptTopLevel p : ppts) {
      boolean firstInst = true;
      for (IInstruction i : assemblies.getInstructions(p.name())) {
        if (firstInst) {
          // The dynamic technique always creates a variable
          // for esp at the first address in the block.
          // We add it here for comparison purposes.
          varsFromAsm.add("bv:" + i.getAddress() + ":" + "esp");
          firstInst = false;
        }

        for (String var : i.getLHSVars()) {
          if (var.startsWith("[")) {
            // For a variable of the form [x] (dereference),
            // the dynamic technique always creates variable x.
            // We add it here for comparison purposes.
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
        System.out.print(UtilMDE.rpad(varsFromPpts.contains(s) ? s : "", 30));
        System.out.print(UtilMDE.rpad(varsFromAsm.contains(s) ? s : "", 30));
        System.out.println();
      }
    }
    throw new RuntimeException();
  }

  private void printKillers(Map<String, Set<String>> reds) {

    System.out.println("REDUNDANT VARIABLES (leader followed by redundant variables):");
    for (Map.Entry<String, Set<String>> e : reds.entrySet()) {
      System.out.println(e.getKey() + " : " + e.getValue());
    }
  }

  public static List<PptTopLevel> findIntermediateBlocks(PptTopLevel dest, PptTopLevel source) {

    Set<PptTopLevel> ghostBBSet = new LinkedHashSet<PptTopLevel>();
    Queue<PptTopLevel> toProcess = new LinkedList<PptTopLevel>();
    toProcess.addAll(dest.predecessors);
    while (!toProcess.isEmpty()) {
      PptTopLevel p = toProcess.poll();
      if (p == source)
        continue;
      ghostBBSet.add(p);
      for (PptTopLevel parent: p.predecessors) {
        if (!ghostBBSet.contains(parent))
          toProcess.add(parent);
      }
    }
    return new ArrayList<PptTopLevel>(ghostBBSet);
  }

/** Returns a name basic on its constituent ppts **/
  public String name() {
    String name = ppts.get(0).name();
    name += ".." + ppts.get(ppts.size()-1).ppt_name.name();
    return name;
  }

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
   * to their last_values field
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

  /**
   * Build a combined VarInfo array for all of the ppts
   */
  private static VarInfo[] combined_vis (List<PptTopLevel> ppts) {

    assert (ppts.size() > 0) : "No ppts in list";

    // Allocate an array of the combined size
    int len = 0;
    for (PptTopLevel ppt : ppts) {
      len += ppt.var_infos.length;
    }
    VarInfo[] vis = new VarInfo[len];

    // Create a new VarInfo for each VarInfo in the ppt list
    int index = 0;
    for (PptTopLevel ppt : ppts) {
      for (VarInfo vi : ppt.var_infos) {
        if (vi.isDerived())
          continue;
        vis[index++] = new VarInfo (vi.vardef);
      }
    }

    return (vis);
  }


  /**
   * Creates combined program points that cover multiple basic
   * blocks.  Each basic block ppt is combined with any basic blocks
   * that dominate it (always occur before it).
   *
   * The input is a list of the basic block ppts that make up the
   * function.  Each bb ppt contains a list of the names of all of the
   * basic blocks that directly succeed it.  That list is used to
   * calculate the dominators.
   *
   * Each program point in the function is modified as follows: <ul>
   *   <li> Its combined_ppts_init flag is set to true
   *   <li> Its combined_ppt field is set to point to the combined
   *    program point that should be processed when this bb ppt is
   *    executed.   This field may be null if this bb ppt is completely
   *    subsumed by other combined ppts
   *    <li> Its combined_subsumed boolean field is set to true if this
   *    ppt is subsumed by a combined program point, false otherwise.
   *
   */
  public static void combine_func_ppts (PptMap all_ppts,
          List<PptTopLevel> func_ppts) {

        List<List<PptTopLevel>> successorsGraph = new ArrayList<List<PptTopLevel>>();

        // Initialize the graph structure with each PPT in the function
        for (PptTopLevel ppt : func_ppts) {

            List<PptTopLevel> successorRow = new ArrayList<PptTopLevel>();
            successorRow.add(ppt);

            // Get the successors ppt
            List<String> successors = ppt.ppt_successors;

            if (successors != null)
                for (String successorName : successors) {
                    PptTopLevel successorPPT = all_ppts.get(successorName);
                    successorRow.add(successorPPT);
                }

            successorsGraph.add(successorRow);
        }

        BasicBlockMerger<PptTopLevel> pptMerger = new BasicBlockMerger<PptTopLevel>(successorsGraph);
        List<List<PptTopLevel>> combinedPPTs = pptMerger.mergeBasicBlocks();
        List<PptTopLevel> pptIndex = pptMerger.getIndexes();
        List<PptTopLevel> subsummedList = pptMerger.getSubsummedList();

        //initialize the PPT_Combined structures
        for (PptTopLevel ppt : func_ppts) {
            // Mark this ppt as initialized
            ppt.combined_ppts_init = true;

            List<PptTopLevel> computedCombinedPPTs = combinedPPTs.get(pptIndex.indexOf(ppt));
            //eliminate first element since it the current ppt itself
            List<PptTopLevel> combined_ppts = computedCombinedPPTs.subList(1, computedCombinedPPTs.size());

            if (subsummedList.get(pptIndex.indexOf(ppt)) == null) {
                // the ppt is not subsummed by other ppts
                ppt.combined_subsumed = false;
                if (combined_ppts.isEmpty()) {
                    // this is a zombie PPT (i.e., no parents, no children)
                    ppt.combined_ppt = null;
                    continue;
                }

                // associate a PptCombined to this ppt.
                // If the var_infos for all combined blocks is
                // smaller than the threshold, associate one single PptCombined,
                // otherwise split this PptCombined into smaller chunks
                List<List<PptTopLevel>> partitions = splitCombinedPpts(combined_ppts);
                for (List<PptTopLevel> partition : partitions) {
                    PptTopLevel splitPpt = partition.get(partition.size() - 1);

                    // do not override a previously written PptCombined
                    if (splitPpt.combined_ppt == null)
                        splitPpt.combined_ppt = new PptCombined(partition);
                }

            } else {
                // ppt is subsummed by other PPTs

                //check whether there was an artificial split
                //at this ppt due to maxVarInfos
                if (ppt.combined_ppt == null) {
                  ppt.combined_subsumed = true;
                  ppt.combined_subsumed_by
                    = subsummedList.get(pptIndex.indexOf(ppt));
                }
            }
        }

        // last pass to set the PPT_Combined value even
        // for subsumed ppts
        for (PptTopLevel ppt : func_ppts) {
            if (ppt.combined_ppt == null) {
                boolean subsumed = ppt.combined_subsumed;
                PptTopLevel subsumedByPpt = ppt;
                while (subsumed) {
                    subsumedByPpt = subsumedByPpt.combined_subsumed_by;
                    subsumed = subsumedByPpt.combined_subsumed;
                }
                ppt.combined_ppt = subsumedByPpt.combined_ppt;
            }
        }

    }

  static List<List<PptTopLevel>> splitCombinedPpts(List<PptTopLevel> list) {
        List<List<PptTopLevel>> result = new ArrayList<List<PptTopLevel>>();
        List<PptTopLevel> partition = new ArrayList<PptTopLevel>();

        int varInfosSize = 0;
        for (PptTopLevel ppt : list) {
            if (varInfosSize + ppt.var_infos.length <= maxVarInfoSize) {
                varInfosSize += ppt.var_infos.length;
                partition.add(ppt);
            }
            else { //create a new partition

                // force at least one element per partition
                // even when that element is larger than the threshold
                if (partition.isEmpty()) {
                    partition.add(ppt);

                    result.add(partition);

                    partition = new ArrayList<PptTopLevel>();
                    varInfosSize = 0;
                }
                else {
                    result.add(partition);

                    partition = new ArrayList<PptTopLevel>();
                    partition.add(ppt);
                    varInfosSize = ppt.var_infos.length;
                }
            }
        }
        result.add(partition);
        return result;
    }


  /**
   * Checks the combined program point for correctness
   */
  public boolean check() {

    if (ppts.size() <= 0) {
      System.out.printf ("ERROR: Size of %s ppts is %d\n", name(), ppts.size());
      return false;
    }

    // Make sure that every ppt has a combined_ppt
    for (PptTopLevel ppt : ppts) {
      if (ppt.combined_ppt == null) {
        System.out.printf ("ERROR: ppt %s has no combined ppt", ppt);
        return false;
      }
    }

    // Make sure that each block is dominated by the previous one
    for (int i = ppts.size()-1; i > 0; i--) {
      PptTopLevel ppt = ppts.get (i);
      PptTopLevel prev = ppts.get(i-1);
      // System.out.printf ("Checking %s dominated by %s\n", bb_short_name(ppt),
      //                   bb_short_name (prev));
      if (!ppt.all_predecessors_goto (prev)) {
        System.out.printf ("ERROR: ppt %s not dominated by ppt %s", ppt.name(),
                           prev.name());
        return false;
      }
    }

    return true;

  }

  /** Dumps out the basic blocks that make up this combined ppt **/
  void dump() {
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
          if (succs == "")
            succs = bb_short_name (ppt_succ);
          else
            succs += " " + bb_short_name (ppt_succ);
        }
      }
      String preds = "";
      if (ppt.predecessors != null) {
        for (PptTopLevel pred : ppt.predecessors) {
          if (preds == "")
            preds = bb_short_name (pred);
          else
            preds += " " + bb_short_name (pred);
        }
      }
      System.out.printf ("      %s: [%s] {%s} combined_subsumed:%b "
                         + "combined_ppt: %s\n",
                         bb_short_name (ppt), succs, preds,
                         ppt.combined_subsumed,
                         ((ppt.combined_ppt == null)
                          ? "null" : ppt.combined_ppt.short_component_str()));
    }
  }


  /** Returns a list of the component ppts that make up this combined ppt **/
  public String short_component_str() {
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

}
