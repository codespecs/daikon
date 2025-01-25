package daikon;

import static daikon.FileIO.ParentRelation;

import daikon.inv.Equality;
import daikon.inv.Invariant;
import daikon.split.PptSplitter;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
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
    /** Possibly cyclic relationship, eg. nested object instances */
    USER,
    /** Entrance of method to exit of method. */
    ENTER_EXIT,
    /** Combined exit to numbered exit of a method. */
    EXIT_EXITNN,
    /** Relation between the same ppt in two different PptMaps. */
    MERGE_CHILD,
    /** Relation from a program point to its conditional ppts. */
    PPT_PPTCOND
  };

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

  @SideEffectFree
  @Override
  public String toString(@GuardSatisfied PptRelation this) {
    return (parent.ppt_name + "->" + child.ppt_name + "(" + relationship + ")");
  }

  /** Return a string containing all of the parent&rarr;child var relations. */
  public String parent_to_child_var_string() {

    StringJoiner var_str = new StringJoiner(", ");
    for (Map.Entry<VarInfo, VarInfo> entry : parent_to_child_map.entrySet()) {
      var_str.add(entry.getKey().name() + "->" + entry.getValue().name());
    }

    return var_str.toString();
  }

  /**
   * Relates all of the variables with the same name in parent and child. Returns true if each
   * non-static parent variable was related to a child variable.
   */
  public boolean relate_same_name() {

    boolean relate_all = true;
    for (VarInfo vp : parent.var_infos) {
      boolean relate_var = relate(vp, vp.name());
      if (!relate_var && !vp.isStaticConstant()) {
        // System.out.printf("no relation for '%s' from %s-%s with vars %s%n",
        //                   vp.name(), parent.name(), child.name(),
        //                   child.varNames());
        relate_all = false;
      }
    }

    return relate_all;
  }

  /** Prints a ppt hierarchy of all of the ppts of this child and below. */
  public void debug_print_tree(Logger l, int indent) {

    // Print the child tree including vars and class name
    child.debug_print_tree(l, indent, this);
  }

  /**
   * Returns whether or not this relation is a primary relation. This used to simplify debug prints
   * of the PPt tree (so that extra relations don't result in duplicative information).
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

  /** Returns whether or not this relation's child has children of its own. */
  public boolean hasChildren() {
    return (child.children.size() > 0);
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
   * Returns a relation in the ppt hierarchy from an object (parent) to a method (child) on that
   * object.
   */
  public static PptRelation newObjectMethodRel(PptTopLevel parent, PptTopLevel child) {

    assert (parent != null) && (child != null);

    PptRelation rel = new PptRelation(parent, child, PptRelationType.PARENT);

    debug.fine(parent.name() + " parent vars = " + Arrays.toString(parent.var_infos));
    debug.fine(child.name() + " child vars = " + Arrays.toString(child.var_infos));

    // Connect each 'this' variable between parent and child.
    // Note that these should be the only variables whose names match and
    // that each parent variable should match one in the child.
    boolean relate_all = rel.relate_same_name();
    assert relate_all;
    return rel;
  }

  /**
   * Returns a relation in the ppt hierarchy from a class (parent) to an object (child) containing
   * static members of that class.
   */
  public static PptRelation newClassObjectRel(PptTopLevel parent, PptTopLevel child) {

    assert (parent != null) && (child != null);

    PptRelation rel = new PptRelation(parent, child, PptRelationType.PARENT);

    // Connect each static variable between parent and child
    // Note that these should be the only variables whose names match
    rel.relate_same_name();
    return rel;
  }

  /**
   * Creates a USER or PARENT relation from child to parent. The variable relationships are
   * specified in the declaration record and stored in the VarInfo for each variable.
   * RuntimeException will be thrown if any of the parent variables cannot be found.
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
   * Returns a relation in the ppt hierarchy from an object (parent) to a user (child) of that
   * objects (eg, from the object B to the method A.foo (B arg)).
   *
   * @param parent ppt of the object definition
   * @param child ppt of a user of parent's object
   * @param arg variable of type object found in child
   */
  public static PptRelation newObjectUserRel(PptTopLevel parent, PptTopLevel child, VarInfo arg) {

    assert (parent != null) && (child != null);

    PptRelation rel = new PptRelation(parent, child, PptRelationType.USER);

    // Connect each each field in arg between parent and child.  Do this
    // by substituting args name for this in the parent and then looking
    // for a name match in the child
    for (VarInfo vp : parent.var_infos) {
      rel.relate(vp, vp.replace_this(arg));
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
   * Returns a an artificial relation in the Program point hierarchy between the same ppt in two
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

  // used by init_hierarchy below
  private static class SplitChild {
    public PptRelation rel;
    public PptSplitter ppt_split;

    public SplitChild(PptRelation rel, PptSplitter ppt_split) {
      this.rel = rel;
      this.ppt_split = ppt_split;
    }
  }

  /**
   * Initialize the hierarchical relationship between ppts. Specifically process each ppt, find its
   * parent(s) in the partial order, and fill this point into the children field in the parent. Note
   * that children contains only the immediate descendants of the ppt.
   *
   * <p>This version should be used with the old version of declaration records. Use
   * init_hierarchy_new() with new declaration records.
   */
  public static void init_hierarchy(PptMap all_ppts) {

    for (PptTopLevel ppt : all_ppts.pptIterable()) {
      PptName pname = ppt.ppt_name;
      PptRelation rel = null;
      Daikon.debugProgress.fine("Processing ppt " + pname);
      debug.fine("Processing ppt " + pname);

      // If this is an object ppt, parent is the class point
      if (pname.isObjectInstanceSynthetic()) {
        PptTopLevel parent = all_ppts.get(pname.makeClassStatic());
        if (parent != null) {
          rel = newClassObjectRel(parent, ppt);
        }

        // Else if it's a method and not a constructor, parent is
        // object or class static methods will relate to the class,
        // while non-static methods will relate to the object.
        // Whether or not a method is static is not in the decls file.
        // We infer this by looking to see if the variables match with
        // the object ppt or the class ppt.
      } else if ((pname.isEnterPoint() && !pname.isConstructor()) || pname.isCombinedExitPoint()) {

        PptTopLevel parent = all_ppts.get(pname.makeObject());

        if (parent != null) {
          if (ppt.find_var_by_name(parent.var_infos[0].name()) != null) {
            rel = newObjectMethodRel(parent, ppt);
          } else {
            parent = all_ppts.get(parent.ppt_name.makeClassStatic());
            if (parent != null) {
              rel = newObjectMethodRel(parent, ppt);
            }
          }
        }

        // Else if an exitNN point, parent is combined exit point
      } else if (pname.isExitPoint()) {
        PptTopLevel parent = all_ppts.get(pname.makeExit());
        // System.out.printf("Parent of %s is %s%n", pname.name(),
        //                   parent.name());
        if (parent != null) {
          rel = newCombinedExitExitNNRel(parent, ppt);
        }
      }

      // If a relation was created, connect it into its ppts
      if (rel != null) {
        debug.fine(
            "-- ppt parent is "
                + rel.parent.name()
                + " with connections ["
                + rel.parent_to_child_var_string()
                + "]");
      } else {
        debug.fine(" -- no ppt parent");
      }

      // Connect combined exit points to enter points over orig variables
      if (pname.isCombinedExitPoint()) {
        PptTopLevel enter = all_ppts.get(pname.makeEnter());
        if (enter != null) {
          rel = PptRelation.newEnterExitRel(enter, ppt);
          debug.fine(
              " -- exit to enter "
                  + enter.name
                  + " with connections ["
                  + rel.parent_to_child_var_string()
                  + "]");
        } else {
          debug.fine("-- No matching enter for exit");
        }
      }

      // For all points, look for vars of a declared type for which we have
      // a corresponding OBJECT ppt.  Essentially these are all of the
      // users of the object.  Don't match if the variable already has
      // a parent (since the parent will provide the link back to the
      // object) For each variable of this type that we find, setup a
      // parent-child relationship with its corresponding OBJECT
      // variables.
      //
      // For example, consider class A with fields x and y and method
      // B.foo (A arg1, A arg2).  We will setup two relations to this
      // ppt -- one from A to b.foo.arg1 and one from A to b.foo.arg2.
      // in each we will equate A.x with arg.x and A.y with arg.y.
      //
      // We skip variables named exactly 'this' so that we don't setup a
      // recursive relationship from the object to itself.

      // DaikonSimple can not see these relations, so don't create them
      // if we'll be comparing to DaikonSimple.
      if (dkconfig_enable_object_user) {

        debug.fine("-- Looking for variables with an OBJECT ppt");
        for (VarInfo vc : ppt.var_infos) {
          String dstr = "-- -- var '" + vc.name() + "' - ";
          if (ppt.has_parent(vc)) {
            debug.fine(dstr + " Skipping, already has a parent");
            continue;
          }
          if (vc.isThis()) {
            debug.fine(dstr + " skipping, name is 'this'");
            continue;
          }
          PptTopLevel object_ppt = vc.find_object_ppt(all_ppts);
          if (object_ppt != null) {
            if (object_ppt == ppt) {
              debug.fine(dstr + " skipping, OBJECT (" + object_ppt + ") is the same as this");
              continue;
            }
            rel = PptRelation.newObjectUserRel(object_ppt, ppt, vc);
            debug.fine(
                dstr
                    + " Connected to Object ppt "
                    + object_ppt.name()
                    + " with connections ["
                    + rel.parent_to_child_var_string()
                    + "]");
          } else {
            debug.fine(dstr + " No object ppt");
          }
        }
      }
      // Connect any conditional ppt variables.  Only connect to the
      // first splitter, since each splitter should yield the same
      // results at the parent (since each splitter sees the same
      // points)  This should only happen at the leaves (numbered
      // exit points) since all other points should be built from
      // their other children.  But since we need the relation
      // from the child's point of view when printing, we create
      // under all cases and then remove it from non-leaves children
      // list.  This doesn't seem like the best solution.
      if (ppt.has_splitters()) {
        assert ppt.splitters != null; // guaranteed by call to has_splitters
        PptSplitter ppt_split = ppt.splitters.get(0);
        for (int ii = 0; ii < ppt_split.ppts.length; ii++) {
          rel = newPptPptConditional(ppt, ppt_split.ppts[ii]);
          debug.fine(
              " -- Connected down to ppt conditional "
                  + ppt_split.ppts[ii].name()
                  + " with connections ["
                  + rel.parent_to_child_var_string()
                  + "]");
          if (!ppt.ppt_name.isNumberedExitPoint()) {
            ppt.children.remove(rel);
          }
        }
      }
    }

    // Create relations between conditional ppts and their children.
    // The relationship between conditional ppts matches exactly
    // the relationship between each their parents.  For example,
    // presume ppt A has a child ppt B.  A has two conditional
    // ppts (AC1, AC2) and B has two conditional ppts (BC1, BC2)
    // Then AC1 is the parent of BC1 and AC2 is the parent of BC2

    // Loop over each ppt and process each non-leaf with splitters
    for (PptTopLevel ppt : all_ppts.pptIterable()) {
      if (ppt.ppt_name.isNumberedExitPoint()) {
        continue;
      }
      if (!ppt.has_splitters()) {
        continue;
      }

      // System.out.printf("processing splitter '%s' [%s] %b%n", ppt.name(),
      //                    ppt.ppt_name.getPoint(),
      //                    ppt.ppt_name.isNumberedExitPoint());

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

        // If we didn't find a matching splitter at each child, can't merge
        // this point.  Just remove it from the list of splitters
        if (split_children.size() != ppt.children.size()) {
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

    // Debug print the hierarchy in a more readable manner
    if (debug.isLoggable(Level.FINE)) {
      debug.fine("PPT Hierarchy");
      for (PptTopLevel ppt : all_ppts.pptIterable()) {
        if (ppt.parents.size() == 0) {
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
  public static void init_hierarchy_new(PptMap all_ppts) {

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

      // Connect any conditional ppt variables.  Only connect to the
      // first splitter, since each splitter should yield the same
      // results at the parent (since each splitter sees the same
      // points)  This should only happen at the leaves (numbered
      // exit points) since all other points should be built from
      // their other children.  But since we need the relation
      // from the child's point of view when printing, we create
      // under all cases and then remove it from non-leaves children
      // list.  This doesn't seem like the best solution.
      if (ppt.has_splitters()) {
        assert ppt.splitters != null; // guaranteed by call to has_splitters
        PptSplitter ppt_split = ppt.splitters.get(0);
        for (int ii = 0; ii < ppt_split.ppts.length; ii++) {
          PptRelation rel = newPptPptConditional(ppt, ppt_split.ppts[ii]);
          rels.add(rel);
          if (!ppt.is_subexit()) {
            ppt.children.remove(rel);
          }
        }
      }
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

    // Create relations between conditional ppts and their children.
    // The relationship between conditional ppts matches exactly
    // the relationship between each their parents.  For example,
    // presume ppt A has a child ppt B.  A has two conditional
    // ppts (AC1, AC2) and B has two conditional ppts (BC1, BC2)
    // Then AC1 is the parent of BC1 and AC2 is the parent of BC2

    // Loop over each ppt and process each non-leaf with splitters
    for (PptTopLevel ppt : all_ppts.pptIterable()) {
      if (ppt.is_subexit()) {
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

        // If we didn't find a matching splitter at each child, can't merge
        // this point.  Just remove it from the list of splitters
        if (split_children.size() != ppt.children.size()) {
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

    // Loop over each ppt and create an equality view and invariants for
    // any ppt without children that doesn't already have them.  This can
    // happen when there are ppts such as OBJECT or CLASS that don't end up
    // with any children (due to the program source or because of ppt filtering).
    for (PptTopLevel ppt : all_ppts.pptIterable()) {
      if ((ppt.children.size() == 0) && (ppt.equality_view == null)) {
        assert ppt.is_object() || ppt.is_class() || ppt.is_enter() : ppt;
        ppt.equality_view = new PptSliceEquality(ppt);
        ppt.equality_view.instantiate_invariants();
      }
    }

    // Debug print the hierarchy in a more readable manner
    if (debug.isLoggable(Level.FINE)) {
      debug.fine("PPT Hierarchy");
      for (PptTopLevel ppt : all_ppts.pptIterable()) {
        if (ppt.parents.size() == 0) {
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
}
