package daikon;

import static daikon.FileIO.ParentRelation;

import daikon.inv.Equality;
import daikon.inv.Invariant;
import daikon.split.PptSplitter;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.StringJoiner;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.checkerframework.checker.initialization.qual.UnderInitialization;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;

/**
 * Class that builds and describes relations in the ppt hierarchy. Building the relationship is
 * specific to each type of parent/child relationship (eg, method to object, exit to combined exit,
 * etc). The use of the relationship is general.
 *
 * <p>The basic function of the class is to translate from a variable in the parent to the
 * equivalent variable in the child and vice-versa. For example, in the ENTER &rarr; EXIT
 * relationship, the parent is the ENTER ppt and the child is the EXIT ppt. Each variable in the
 * ENTER ppt is connected to the corresponding orig variable in the EXIT ppt.
 */
public class PptRelation implements Serializable {

  static final long serialVersionUID = 20030819L;

  /**
   * The different ppt/variable hierarchy relationships. Parent and User relations are specified in
   * the declaration record of the ppt. ENTER_EXIT, EXIT_EXITNN, and PPT_COND are automatically
   * constructed. MERGE_CHILD is not used by Daikon.
   */
  public enum PptRelationType {
    /** Acyclic relationship to a parent, eg, method to its object. */
    PARENT,
    /** Possibly cyclic relationship, eg, nested object instances. */
    USER,
    /** Entrance of method to exit of method. */
    ENTER_EXIT,
    /** Combined exit to numbered exit of a method. */
    EXIT_EXITNN,
    /** Relation between the same ppt in two different PptMaps. */
    MERGE_CHILD,
    /** Relation from a program point to its conditional ppts. */
    PPT_PPTCOND
  }

  /** The logger for daikon.PptRelation. */
  private static final Logger debug = Logger.getLogger("daikon.PptRelation");

  /** Description of type of parent-child relationship (debug output only). */
  PptRelationType relationship;

  /** Parent of relation. */
  public PptTopLevel parent;

  /** Child of relation. */
  public PptTopLevel child;

  /** Map from parent vars to matching child vars. */
  @SuppressWarnings("serial")
  public Map<VarInfo, VarInfo> parent_to_child_map;

  /** Map from child vars to matching parent vars. */
  @SuppressWarnings("serial")
  public Map<VarInfo, VarInfo> child_to_parent_map;

  /** Boolean. Controls whether the object-user relation is created in the variable hierarchy. */
  public static boolean dkconfig_enable_object_user = false;

  /**
   * Create a relation between the specified parent and child. The actual variable relations are
   * filled in by the caller. Note that this creates the connection between this relation and the
   * parent/child. As a side effect, the constructed PptRelation is stored in both the parent and
   * the child.
   */
  private PptRelation(PptTopLevel parent, PptTopLevel child, PptRelationType rel_type) {

    this.parent = parent;
    this.child = child;
    parent_to_child_map = new LinkedHashMap<>();
    child_to_parent_map = new LinkedHashMap<>();
    // rel_type is one of the above relationship types because this is a
    // private constructor, called only within this file.
    relationship = rel_type;
    connect();
  }

  /** Adds this relation to its child's parent list and its parent's children list. */
  @SuppressWarnings({
    "nullness:argument" // won't be used until initialization is finished
  })
  private void connect(@UnderInitialization(PptRelation.class) PptRelation this) {
    assert !child.parents.contains(this);
    assert !parent.children.contains(this);
    child.parents.add(this);
    parent.children.add(this);
  }

  /** Returns the number of parent to child variable relations. */
  @Pure
  public int size() {
    return parent_to_child_map.size();
  }

  /**
   * Returns true if this PptRelation is empty.
   *
   * @return true if this PptRelation is empty
   */
  @Pure
  public boolean isEmpty() {
    return parent_to_child_map.isEmpty();
  }

  @SideEffectFree
  @Override
  public String toString(@GuardSatisfied PptRelation this) {
    return (parent.ppt_name + "->" + child.ppt_name + "(" + relationship + ")");
  }

  /** Returns a string containing all of the parent&rarr;child var relations. */
  public String parent_to_child_var_string() {

    StringJoiner var_str = new StringJoiner(", ");
    for (Map.Entry<VarInfo, VarInfo> entry : parent_to_child_map.entrySet()) {
      var_str.add(entry.getKey().name() + "->" + entry.getValue().name());
    }

    return var_str.toString();
  }

  /** Prints a ppt hierarchy of all of the ppts of this child and below. */
  public void debug_print_tree(Logger l, int indent) {

    // Print the child tree including vars and class name
    child.debug_print_tree(l, indent, this);
  }

  /**
   * Returns true if this relation is a primary relation. This used to simplify debug prints of the
   * PPt tree (so that extra relations don't result in duplicative information).
   *
   * <p>Somewhat arbitrarily, Object&rarr;User and Enter&rarr;Exit are not considered primary while
   * all others are. The remaining relations (class&rarr;object, object&rarr;method,and
   * exit&rarr;exitNN) form a simple tree without duplication.
   */
  @Pure
  public boolean is_primary() {
    return (relationship != PptRelationType.USER) && (relationship != PptRelationType.ENTER_EXIT);
  }

  /** Returns a string describing the parent-child relationship. */
  public PptRelationType getRelationType() {
    return relationship;
  }

  /**
   * Returns the parent variable that corresponds to childVar. Returns null if there is no
   * corresponding variable.
   */
  public @Nullable VarInfo parentVar(VarInfo childVar) {
    return child_to_parent_map.get(childVar);
  }

  /**
   * Like parentVar(VarInfo), but if no parent is found, tries every variable in the equality set
   * and returns null only if none of them has a parent.
   */
  public @Nullable VarInfo parentVarAnyInEquality(VarInfo childVar) {
    VarInfo result = parentVar(childVar);
    if (result != null) {
      return result;
    }
    if (childVar.equalitySet == null) {
      return null;
    }
    for (VarInfo v : childVar.equalitySet.getVars()) {
      result = parentVar(v);
      if (result != null) {
        return result;
      }
    }
    return null;
  }

  /**
   * Returns the child variable that corresponds to parentVar. Returns null if there is no
   * corresponding variable.
   */
  public @Nullable VarInfo childVar(VarInfo parentVar) {
    return parent_to_child_map.get(parentVar);
  }

  /** Returns true if this relation's child has children of its own. */
  public boolean hasChildren() {
    return !child.children.isEmpty();
  }

  /**
   * Returns a map of VarInfo.Pair with an entry for each pair of equal variables in all of the
   * equality sets of the child. The variables are the corresponding parent variables and not the
   * child variables themselves. The map is from the pair to itself, which allows the pair to be
   * looked up (which is not possible with a set).
   */
  public Map<VarInfo.Pair, VarInfo.Pair> get_child_equalities_as_parent() {

    debug.fine(
        "get_child_equalities for "
            + child.name()
            + " for parent "
            + parent.name()
            + " "
            + relationship);
    Map<VarInfo.Pair, VarInfo.Pair> emap = new LinkedHashMap<>();

    if (child.equality_view == null) {
      throw new Error(
          "child.equality_view == null for child ppt: "
              + child.name()
              + " samples = "
              + child.num_samples());
    }
    if (child.equality_view.invs == null) {
      throw new Error(
          "child.equality_view.invs == null for child ppt: "
              + child.name()
              + " samples = "
              + child.num_samples()
              + "children = "
              + child.children);
    }

    // Loop through each equality set in the child
    for (Invariant inv : child.equality_view.invs) {
      Equality e = (Equality) inv;
      debug.fine("-- processing equality set " + e);
      Set<VarInfo> eqset = e.getVars();
      VarInfo[] varr = eqset.toArray(new VarInfo[0]);

      // Build each combination of variables in the equality set and produce
      // a pair for each.  Skip any variables that do not have corresponding
      // variables in the parent.
      for (int j = 0; j < varr.length; j++) {
        VarInfo v1 = parentVar(varr[j]);
        if (v1 == null) {
          debug.fine("-- -- " + varr[j].name() + " not in parent (skip)");
          continue;
        }
        for (int k = j + 1; k < varr.length; k++) {
          VarInfo v2 = parentVar(varr[k]);
          if (v2 == null) {
            debug.fine("-- -- " + varr[k].name() + " not in parent (skip)");
            continue;
          }
          VarInfo.Pair parent_pair = new VarInfo.Pair(v1, v2, e.numSamples());
          emap.put(parent_pair, parent_pair);
          if (debug.isLoggable(Level.FINE)) {
            debug.fine(
                "-- -- "
                    + varr[j].name()
                    + ", "
                    + varr[k].name()
                    + " in child yield "
                    + parent_pair
                    + " in parent");
          }
        }
      }
    }
    return emap;
  }

  /**
   * Relates parent_var to a variable in child that matches name.
   *
   * @param parent_var the parent variable being matched
   * @param viname the name to look for in child variables
   * @return true if there was a matching variable, false otherwise
   */
  private boolean relate(VarInfo parent_var, String viname) {

    for (VarInfo vc : child.var_infos) {
      if (viname.equals(vc.name())) {
        child_to_parent_map.put(vc, parent_var);
        parent_to_child_map.put(parent_var, vc);
        return true;
      }
    }
    return false;
  }

  /**
   * Creates a USER or PARENT relation from child to parent. The variable relationships are
   * specified in the declaration record and stored in the VarInfo for each variable. A derived
   * child variable with no counterpart in the parent is omitted from the relation, because whether
   * a variable is derived depends on per-ppt information such as comparability. RuntimeException
   * will be thrown if the parent variable of any non-derived child variable cannot be found.
   */
  public static PptRelation newParentRelation(
      ParentRelation pr, PptTopLevel parent, PptTopLevel child) {

    assert pr != null && parent != null && child != null;
    // System.out.printf("Parent Relation %s[%d] to %s%n", pr.parent_ppt_name,
    //                   pr.id, child.name());

    PptRelation rel = new PptRelation(parent, child, pr.rel_type);
    for (VarInfo vc : child.var_infos) {
      for (VarParent pi : vc.parents) {
        // System.out.printf("--child variable %s, ppt %s[%d], parent_var %s%n",
        //                    vc.name(), pi.parent_ppt, pi.parent_relation_id,
        //                    pi.parent_variable);
        if (pi.parent_relation_id != pr.id) {
          continue;
        }

        // Get the name of the parent variable.  Its the same as this one if
        // not specified.  For now, remove the array placeholder (..) since
        // VarInfoName doesn't support it.
        String parent_name = pi.parent_variable;
        if (parent_name == null) {
          parent_name = vc.name();
        }
        // parent_name = parent_name.replace ("[..]", "[]");

        // System.out.printf("---parent name %s%n", parent_name);
        VarInfo vp = parent.find_var_by_name(parent_name);
        if (vp == null && vc.derived != null) {
          // Whether a variable is derived depends on per-ppt information such as comparability,
          // so a variable that is derived in the child need not be derived in the parent.
          if (debug.isLoggable(Level.FINE)) {
            debug.fine(
                String.format(
                    "No parent variable '%s' in ppt '%s' for derived var '%s' in ppt '%s'",
                    parent_name, pi.parent_ppt, vc.name(), child.name()));
          }
          continue;
        }
        if (vp == null) {
          throw new RuntimeException(
              String.format(
                  "Can't find parent variable '%s' in ppt '%s', "
                      + "with vars %s specified by var '%s' in ppt '%s'",
                  parent_name, pi.parent_ppt, parent.var_names(), vc.name(), child.name()));
        }
        rel.child_to_parent_map.put(vc, vp);
        rel.parent_to_child_map.put(vp, vc);
      }
    }
    return rel;
  }

  /**
   * Returns a relation in the ppt hierarchy from enter points to exit points over orig variables.
   */
  public static PptRelation newEnterExitRel(PptTopLevel parent, PptTopLevel child) {

    assert (parent != null) && (child != null);

    PptRelation rel = new PptRelation(parent, child, PptRelationType.ENTER_EXIT);

    // Look for orig versions of each non-derived parent variable in the child
    // Note that static constants don't have orig versions (since they are
    // known to be the same), so we connect to the post version instead.
    for (VarInfo vp : parent.var_infos) {
      if (vp.derived != null) {
        continue;
      }
      if (vp.isStaticConstant()) {
        @SuppressWarnings("UnusedVariable") // see comment below
        boolean found = rel.relate(vp, vp.name());
        // Static constants are not always placed at each level in hierarchy
        // (due to mutually recursive constants that contain one another as
        // fields).
        // assert found;
      } else {
        // VarInfoName orig_name = vp.name.applyPrestate().intern();
        boolean found = rel.relate(vp, vp.prestate_name());
        assert found
            : String.format(
                "vp %s orig_name %s parent %s child %s with vars %s",
                vp, vp.prestate_name(), parent.name(), child.name(), child.var_names());
      }
    }

    // Look for orig versions of derived variables in the child.  This is
    // done by finding the base of each derived variable and looking for
    // a child variable with the same bases and the same equation.  This
    // is necessary because derivations are done AFTER orig variables so
    // applying the prestate name (as done above) won't work (the resulting
    // variable is really the same but the name is constructed differently).

    // Loop through each derived parent (ENTER) variable
    for (VarInfo vp : parent.var_infos) {
      if (vp.derived == null) {
        continue;
      }

      // Get a child version of each of the bases of the derivation
      VarInfo[] vp_bases = vp.derived.getBases();
      // TODO: Is this "@Nullable" annotation correct?  (That is, can the
      // element value actually be null?)
      @Nullable VarInfo[] child_vp_bases = new VarInfo[vp_bases.length];
      for (int j = 0; j < vp_bases.length; j++) {
        child_vp_bases[j] = rel.childVar(vp_bases[j]);
      }

      // Loop through the child (exit) looking for a matching derived variable
      for (VarInfo vc : child.var_infos) {
        if (vc.derived == null) {
          continue;
        }
        if (vc.derived.isSameFormula(vp.derived)) {
          assert vc.derived != null;
          VarInfo[] vc_bases = vc.derived.getBases();
          if (Arrays.equals(child_vp_bases, vc_bases)) {
            rel.child_to_parent_map.put(vc, vp);
            rel.parent_to_child_map.put(vp, vc);
            break;
          }
        }
      }
    }

    // Make sure every non-static ENTER variable was found in the EXIT point
    boolean all_found = true;
    for (VarInfo vp : parent.var_infos) {
      if (vp.isStaticConstant()) {
        continue;
      }
      if (!rel.parent_to_child_map.containsKey(vp)) {
        if (all_found) {
          System.out.printf(
              "missing variables in newEnterExitRel:%n"
                  + "  parent = %s%n"
                  + "  child = %s%n"
                  + "  parent.var_infos = %s%n"
                  + "parent varinfos missing from parent_to_child_map:%n",
              parent.name(), child.name(), parent.var_infos);
          all_found = false;
        }
        System.out.printf("   %s%n", vp.name());
      }
    }
    if (!all_found) {
      System.out.printf("rel.parent_to_child_map:%n");
      for (Map.Entry<VarInfo, VarInfo> entry : rel.parent_to_child_map.entrySet()) {
        System.out.printf("    %s => %s%n", entry.getKey(), entry.getValue());
      }
      System.out.printf("child.var_infos:%n");
      for (VarInfo vc : child.var_infos) {
        System.out.println("    " + vc.name());
      }
      System.out.printf(
          "End of diagnostics for newEnterExitRel(%s, %s)%n", parent.name(), child.name());
      // throw new Error("Missing orig variable in EXIT");
    }
    return rel;
  }

  /**
   * Returns a relation in the ppt hierarchy from combined exit points (parent) to an individual
   * exit point (child). Individual exit points are often referred to as exitNN where NN is the line
   * number of the exit point).
   */
  public static PptRelation newCombinedExitExitNNRel(PptTopLevel parent, PptTopLevel child) {

    assert (parent != null) && (child != null);

    PptRelation rel = new PptRelation(parent, child, PptRelationType.EXIT_EXITNN);

    // Create the parent-child variable map.  This one is easy as the
    // variables should match exactly
    assert parent.var_infos.length == child.var_infos.length;
    for (int i = 0; i < parent.var_infos.length; i++) {
      VarInfo vc = child.var_infos[i];
      VarInfo vp = parent.var_infos[i];
      assert vc.name().equals(vp.name());
      rel.child_to_parent_map.put(vc, vp);
      rel.parent_to_child_map.put(vp, vc);
    }
    return rel;
  }

  /** Returns a relation in the ppt hierarchy from a ppt to a PptConditional for that point. */
  public static PptRelation newPptPptConditional(PptTopLevel parent, PptTopLevel child) {

    assert (parent != null) && (child != null);

    PptRelation rel = new PptRelation(parent, child, PptRelationType.PPT_PPTCOND);

    // Create the parent-child variable map.  This one is easy as the
    // variables should match exactly
    assert parent.var_infos.length == child.var_infos.length;
    for (int i = 0; i < parent.var_infos.length; i++) {
      VarInfo vc = child.var_infos[i];
      VarInfo vp = parent.var_infos[i];
      assert vc.name().equals(vp.name());
      rel.child_to_parent_map.put(vc, vp);
      rel.parent_to_child_map.put(vp, vc);
    }
    return rel;
  }

  /**
   * Returns an artificial relation in the Program point hierarchy between the same ppt in two
   * different PptMaps. Used to merge invariants between different data sets. The parent and the
   * child should have exactly the same variables.
   */
  public static PptRelation newMergeChildRel(PptTopLevel parent, PptTopLevel child) {

    assert (parent != null) && (child != null);

    PptRelation rel = new PptRelation(parent, child, PptRelationType.MERGE_CHILD);

    // assert that parent vars match child vars
    if (parent.var_infos.length != child.var_infos.length) {
      System.out.println("newMergeChildRel: in ppt " + parent.name() + " vars don't match");
      System.out.println("parent vars= " + Arrays.toString(parent.var_infos));
      System.out.println("child vars=  " + Arrays.toString(child.var_infos));
      assert parent.var_infos.length == child.var_infos.length;
    }

    // Create the parent-child variable map.  This one is easy as the
    // variables should match exactly
    for (int i = 0; i < parent.var_infos.length; i++) {
      VarInfo vc = child.var_infos[i];
      VarInfo vp = parent.var_infos[i];
      if (!vc.name().equals(vp.name())) {
        System.out.println(
            "newMergeChildRel: in ppt " + parent.name() + " var " + vc.name() + " doesn't match");
        System.out.println("par vars  = " + Arrays.toString(parent.var_infos));
        System.out.println("child vars= " + Arrays.toString(child.var_infos));
        assert vc.name().equals(vp.name());
      }
      rel.child_to_parent_map.put(vc, vp);
      rel.parent_to_child_map.put(vp, vc);
    }
    return rel;
  }

  /**
   * Copies the relation from its current ppts to the specified ppts. The new ppts must have the
   * same variables in the same order as do the original ones.
   */
  public PptRelation copy(PptTopLevel new_parent, PptTopLevel new_child) {

    PptRelation rel = new PptRelation(new_parent, new_child, relationship);
    for (VarInfo vc : child_to_parent_map.keySet()) {
      VarInfo vp = child_to_parent_map.get(vc);
      VarInfo new_vc = new_child.var_infos[vc.varinfo_index];
      VarInfo new_vp = new_parent.var_infos[vp.varinfo_index];
      assert new_vc.name().equals(vc.name());
      assert new_vp.name().equals(vp.name());
      rel.child_to_parent_map.put(new_vc, new_vp);
      rel.parent_to_child_map.put(new_vp, new_vc);
    }
    return rel;
  }

  /**
   * Creates an equality view and invariants for each non-leaf ppt that has no children and does not
   * already have an equality view. This happens for non-leaf ppts such as OBJECT, CLASS, or GLOBAL
   * that do not end up with any children (due to the program source or because of ppt filtering).
   * It also happens for a combined exit point that a decls file declares when no corresponding
   * numbered exit point is declared or included. Leaves get their equality views from {@link
   * Daikon#setupEquality}.
   *
   * <p>The equality view is created even if {@link Daikon#use_equality_optimization} is false,
   * because {@link PptTopLevel#mergeInvs} requires every childless ppt to have one.
   *
   * @param all_ppts the program points
   */
  private static void setup_childless_nonleaves(PptMap all_ppts) {
    for (PptTopLevel ppt : all_ppts.pptIterable()) {
      if (ppt.children.isEmpty() && (ppt.equality_view == null) && !ppt.is_dataflow_leaf()) {
        ppt.create_equality_view();
      }
    }
  }

  // used by init_hierarchy below
  private static class SplitChild {
    PptRelation rel;
    PptSplitter ppt_split;

    SplitChild(PptRelation rel, PptSplitter ppt_split) {
      this.rel = rel;
      this.ppt_split = ppt_split;
    }
  }

  /**
   * Connects a ppt to the conditional ppts of its first splitter. Only connect to the first
   * splitter, since each splitter should yield the same results at the parent (since each splitter
   * sees the same points). This should only happen at the leaves (such as numbered exit points)
   * since all other points should be built from their other children. But since we need the
   * relation from the child's point of view when printing, we create it in all cases and then
   * remove it from the children list of non-leaves. This doesn't seem like the best solution.
   *
   * @param ppt the ppt whose conditional ppts to connect
   * @return the relations that were created
   */
  @SuppressWarnings("MixedMutabilityReturnType")
  private static List<PptRelation> connect_conditionals(PptTopLevel ppt) {
    if (!ppt.has_splitters()) {
      return Collections.emptyList();
    }
    assert ppt.splitters != null; // guaranteed by call to has_splitters
    List<PptRelation> result = new ArrayList<>();
    PptSplitter ppt_split = ppt.splitters.get(0);
    for (int ii = 0; ii < ppt_split.ppts.length; ii++) {
      PptRelation rel = newPptPptConditional(ppt, ppt_split.ppts[ii]);
      result.add(rel);
      if (!ppt.is_dataflow_leaf()) {
        ppt.children.remove(rel);
      }
    }
    return result;
  }

  /**
   * Completes the hierarchy, after the relations between the (unconditional) ppts have been
   * created: creates the relations between conditional ppts, and sets up childless non-leaves.
   *
   * @param all_ppts the program points
   */
  private static void finish_hierarchy(PptMap all_ppts) {
    // Create relations between conditional ppts and their children.
    // The relationship between conditional ppts matches exactly
    // the relationship between each their parents.  For example,
    // presume ppt A has a child ppt B.  A has two conditional
    // ppts (AC1, AC2) and B has two conditional ppts (BC1, BC2)
    // Then AC1 is the parent of BC1 and AC2 is the parent of BC2

    // Loop over each ppt and process each non-leaf with splitters
    for (PptTopLevel ppt : all_ppts.pptIterable()) {
      if (ppt.is_dataflow_leaf()) {
        continue;
      }
      if (!ppt.has_splitters()) {
        continue;
      }

      // System.out.printf("processing splitter %s%n", ppt.name());

      // Loop over each splitter
      // splitter_loop:
      for (Iterator<PptSplitter> ii = ppt.splitters.iterator(); ii.hasNext(); ) {
        PptSplitter ppt_split = ii.next();

        // list of children that match this splitter
        List<SplitChild> split_children = new ArrayList<>();

        // Create a list of children for this splitter
        child_loop:
        for (PptRelation rel : ppt.children) {
          if (!rel.child.has_splitters()) {
            break;
          }
          for (PptSplitter csplit : rel.child.splitters) {
            if (ppt_split.splitter == csplit.splitter) {
              split_children.add(new SplitChild(rel, csplit));
              continue child_loop;
            }
          }
          break;
        }

        // If there are no children, or we didn't find a matching splitter
        // at each child, can't merge this point.  Just remove it from the
        // list of splitters.
        if (ppt.children.isEmpty() || split_children.size() != ppt.children.size()) {
          ii.remove();
          continue;
        }

        // Build the PptRelations for each child.  The PptRelation from
        // the conditional point is of the same type as the original
        // relation from parent to child
        for (SplitChild sc : split_children) {
          ppt_split.add_relation(sc.rel, sc.ppt_split);
        }
      }
    }

    setup_childless_nonleaves(all_ppts);

    // Debug print the hierarchy in a more readable manner
    if (debug.isLoggable(Level.FINE)) {
      debug.fine("PPT Hierarchy");
      for (PptTopLevel ppt : all_ppts.pptIterable()) {
        if (ppt.parents.isEmpty()) {
          ppt.debug_print_tree(debug, 0, null);
        }
      }
    }

    // Debug print the equality sets for each ppt
    if (debug.isLoggable(Level.FINE)) {
      for (PptTopLevel ppt : all_ppts.pptIterable()) {
        debug.fine(ppt.name() + " equality sets: " + ppt.equality_sets_txt());
      }
    }
  }

  /**
   * Initialize the hierarchical relationship between ppts. Specifically process each ppt, find its
   * parent(s) in the partial order, and fill this point into the children field in the parent. Note
   * that children contains only the immediate descendants of the ppt.
   */
  public static void init_hierarchy(PptMap all_ppts) {

    for (PptTopLevel ppt : all_ppts.pptIterable()) {
      PptName pname = ppt.ppt_name;
      // rels is solely for debugging; each relation is stored in the
      // parent and child ppts
      List<PptRelation> rels = new ArrayList<>();
      Daikon.debugProgress.finer("Processing ppt " + pname);
      debug.fine("Processing ppt " + pname);

      assert ppt.parent_relations != null : "missing parent_relations in ppt " + ppt.name();

      // Process the front-end specified relations
      for (ParentRelation pr : ppt.parent_relations) {
        // Skip all relations in subexits.  These relations will be handled
        // in the combined exit point.
        if (ppt.is_subexit()) {
          continue;
        }

        PptTopLevel parent = all_ppts.get(pr.parent_ppt_name);
        if (parent == null) {
          throw new RuntimeException(
              "parent ppt " + pr.parent_ppt_name + " not found for ppt " + ppt.name());
        }
        // A leaf obtains its invariants from its samples (and its conditional ppts), never from
        // its children.
        if (parent.is_dataflow_leaf()) {
          throw new Daikon.UserError(
              String.format(
                  "ppt %s is declared as a parent of ppt %s, but its type (%s) makes it a leaf of"
                      + " the dataflow hierarchy",
                  parent.name(),
                  ppt.name(),
                  parent.type.name().toLowerCase(Locale.ENGLISH)));
        }
        if ((pr.rel_type == PptRelationType.USER) && !dkconfig_enable_object_user) {
          continue;
        }
        // System.out.printf("processing hierarchy rel from '%s' to '%s'%n",
        //                    ppt.name(), pr.parent_ppt_name);
        rels.add(newParentRelation(pr, parent, ppt));
      }

      // if an exitNN point, parent is combined exit point
      if (ppt.is_subexit()) {
        PptTopLevel parent = all_ppts.get(pname.makeExit());
        if (parent != null) {
          rels.add(newCombinedExitExitNNRel(parent, ppt));
        }

        // Connect combined exit points to enter points over orig variables
      } else if (ppt.is_combined_exit()) {
        PptTopLevel enter = all_ppts.get(pname.makeEnter());
        if (enter != null) {
          rels.add(PptRelation.newEnterExitRel(enter, ppt));
        }
      }

      rels.addAll(connect_conditionals(ppt));
      // Debug print the created relations
      for (PptRelation rel : rels) {
        debug.fine(
            "-- ppt parent is "
                + rel.parent.name()
                + " with connections ["
                + rel.parent_to_child_var_string()
                + "]");
      }
    }

    finish_hierarchy(all_ppts);
  }
}
