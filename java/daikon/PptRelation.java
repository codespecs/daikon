package daikon;

import daikon.inv.*;
import java.io.*;
import java.util.*;
import utilMDE.*;
import java.util.logging.Logger;
import java.util.logging.Level;

/**
 * Class that builds and describes relations in the ppt hierachy.
 * Building the relationship is specific to each type of parent/child
 * relationship (eg, method to object, exit to combined exit, etc).
 * The use of the relationship is general.
 *
 * The basic function of the class is to translate from a variable in
 * the parent to the equivalent variable in the child and vice-versa.
 * For example, in the ENTER -> EXIT relationship, the parent is the
 * ENTER ppt and the child is the EXIT ppt.  Each variable in the ENTER
 * ppt is connected to the corresponding orig variable in the EXIT ppt.
 */

public class PptRelation implements Serializable {

  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20030819L;

  private static final Logger debug = Logger.getLogger("daikon.PptRelation");

  /** description of type of parent-child relationship (debug output only) **/
  String relationship;

  /** parent of relation **/
  public PptTopLevel parent;

  /** child of relation **/
  public PptTopLevel child;

  /** map from parent vars to matching child vars */
  Map parent_to_child_map;

  /** map from child vars to matching parent vars */
  Map child_to_parent_map;

  /**
   * Create a relation between the specified parent and child.  The actual
   * variable relations are filled in by the caller.
   */
  private PptRelation (PptTopLevel parent, PptTopLevel child) {

    this.parent = parent;
    this.child = child;
    parent_to_child_map = new LinkedHashMap();
    child_to_parent_map = new LinkedHashMap();
    relationship = "";
  }

  /**
   * Adds this relation to its child's parent list and its parent's
   * children list
   */
  public void connect() {
    child.parents.add (this);
    parent.children.add (this);
  }

  /**
   * Returns the number of parent to child variable relations
   */
  public int size() {
    return (parent_to_child_map.size());
  }

  /**
   * Return a string containing all of the parent->child var relations
   */
  public String parent_to_child_var_string() {

    String var_str = "";
    for (Iterator i = parent_to_child_map.keySet().iterator(); i.hasNext(); ) {
      VarInfo pv = (VarInfo) i.next();
      VarInfo cv = (VarInfo) parent_to_child_map.get (pv);
      if (var_str != "")
        var_str += ", ";
      var_str += pv.name.name() + "->" + cv.name.name();
    }

    return var_str;
  }

  /**
   * Relates all of the variables with the same name in parent and child.
   */
  public void relate_same_name() {

    for (int i = 0; i < parent.var_infos.length; i++) {
      VarInfo vp = parent.var_infos[i];
      for (int j = 0; j < child.var_infos.length; j++) {
        VarInfo vc = child.var_infos[j];
        if (vp.name.name().equals (vc.name.name())) {
          child_to_parent_map.put (vc, vp);
          parent_to_child_map.put (vp, vc);
          break;
        }
      }
    }
  }

  /**
   * Prints a ppt hierarchy of all of the ppts of this child and below
   */
  public void debug_print_tree (Logger l, int indent) {

    // Print the child tree including vars and class name
    child.debug_print_tree (l, indent, this);
  }

  /**
   * Returns whether or not this relation is a primary relation.  This
   * used to simplify debug prints of the PPt tree (so that extra relations
   * don't result in duplicative information).
   *
   * Somewhat arbitrarily, Object->User and Enter->Exit are not considered
   * primary while all others are.  The remaining relations (class->object,
   * object->method,and exit->exitNN) form a simple tree without duplication
   */

  public boolean is_primary() {
    return ((relationship != "object -> user")
            && (relationship != "enter -> exit"));
  }

  /** Returns a string describing the parent-child relationship **/
  public String getRelationType() {
    return (relationship);
  }

  /**
   * Returns the parent variable that correspondes to childVar.  Returns
   * null if there is no corresponding variable
   */

  public VarInfo parentVar (VarInfo childVar) {
    return (VarInfo) child_to_parent_map.get (childVar);
  }

  /**
   * Returns the child variable that correspondes to parentVar.  Returns
   * null if there is no corresponding variable
   */

  public VarInfo childVar (VarInfo parentVar) {
    return (VarInfo) parent_to_child_map.get (parentVar);
  }

  /**
   * Returns whether or not this relations child has children of its own
   */
  public boolean hasChildren() {
    return (child.children.size() > 0);
  }

  /**
   * Returns a set of VarInfo.Pair with an entry for each pair of
   * equal variables in all of the equality sets of the child.  The
   * variables are the corresponding parent variables and not the
   * child variables themselves.
   */

  public Set get_child_equalities_as_parent(){

    debug.fine ("get_child_equalities for " + child.ppt_name);
    Set eset = new LinkedHashSet();

    // Loop through each equality set in the child
    for (int i = 0; i < child.equality_view.invs.size(); i++) {
      Equality e = (Equality) child.equality_view.invs.get (i);
      debug.fine ("-- processing equality set " + e);
      Set eqset = e.getVars();
      VarInfo[] varr = new VarInfo[eqset.size()];
      varr = (VarInfo[]) eqset.toArray (varr);

      // Build each combination of variables in the equality set and produce
      // a pair for each.  Skip any variables that do not have corresponding
      // variables in the parent.
      for (int j = 0; j < varr.length; j++) {
        VarInfo v1 = parentVar (varr[j]);
        if (v1 == null) {
          debug.fine ("-- -- " + varr[j].name.name() + " not in parent (skip)");
          continue;
        }
        for (int k = j+1; k < varr.length; k++) {
          VarInfo v2 = parentVar (varr[k]);
          if (v2 == null) {
            debug.fine ("-- -- " + varr[k].name.name()+" not in parent (skip)");
            continue;
          }
          VarInfo.Pair parent_pair = new VarInfo.Pair (v1, v2, e.numSamples());
          eset.add (parent_pair);
          debug.fine ("-- -- " + varr[j].name.name() + ", "
                      + varr[k].name.name() + " in child yield "
                      + parent_pair + " in parent");
        }
      }
    }
    return (eset);
  }

  /**
   * Returns a relation in the ppt hierarchy from an object (parent) to a
   * method (child) on that object
   */
  public static PptRelation newObjectMethodRel (PptTopLevel parent,
                                                PptTopLevel child) {

    if ((parent == null) || (child == null))
      return (null);

    PptRelation rel = new PptRelation (parent, child);
    rel.relationship = "object -> method";

    // Connect each 'this' variable between parent and child
    // Note that these should be the only variables whose names match
    rel.relate_same_name();
    return (rel);
  }


  /**
   * Returns a relation in the ppt hierarchy from a class (parent)
   * to an object (child) containing static members of that class.
   */

  public static PptRelation newClassObjectRel (PptTopLevel parent,
                                               PptTopLevel child) {

    if ((parent == null) || (child == null))
      return (null);

    PptRelation rel = new PptRelation (parent, child);
    rel.relationship = "class -> object";

    // Connect each static variable between parent and child
    // Note that these should be the only variables whose names match
    rel.relate_same_name();
    return (rel);
  }

  /**
   * Returns a relation in the ppt hierarchy from an object (parent)
   * to a user (child) of that objects (eg, from the object B to the method
   * A.foo (B arg))
   *
   * Note that only the fields of the object (eg, this.x, this.y)
   * and not the object itself (eg, this) are substituted in this
   * fashion.
   *
   * While it could be argued that a pointer to an object of type
   * T and the 'this' pointer in an object of type T are analogous,
   * they are really not the same.  The pointer is a reference to
   * the object while 'this' is really the object itself.  The
   * relationship is also not intuitive when looking at the
   * invariants.  For example, assume that every reference to T at
   * all ppts was not null.  This invariant would print as 'this
   * != null.'  The invariant is both confusing (since in a normal
   * context 'this' can never be null) and it is not obvious that
   * it implies that all references to the object are not NULL.
   *
   * @param parent Ppt of the object definition
   * @param child Ppt of a user of parent's object
   * @param arg Variable of type object found in child
   */
  static public PptRelation newObjectUserRel (PptTopLevel parent,
                                              PptTopLevel child,
                                              VarInfo arg) {

    if ((parent == null) || (child == null))
      return (null);

    PptRelation rel = new PptRelation (parent, child);
    rel.relationship = "object -> user";

    // Connect each each field in arg between parent and child.  Do this
    // by substituting args name for this in the parent and then looking
    // for a name match in the child
    for (int i = 0; i < parent.var_infos.length; i++) {
      VarInfo vp = parent.var_infos[i];
      if (vp.name.equals (VarInfoName.THIS))
        continue;
      VarInfoName parent_name = vp.name.replaceAll
                                  (VarInfoName.THIS, arg.name);
      for (int j = 0; j < child.var_infos.length; j++) {
        VarInfo vc = child.var_infos[j];
        if (parent_name == vc.name) {
          rel.child_to_parent_map.put (vc, vp);
          rel.parent_to_child_map.put (vp, vc);
          break;
        }
      }
    }
    return (rel);
  }

  /**
   * Returns a relation in the ppt hierarchy from enter points to exit
   * points over orig variables.
   */
  static public PptRelation newEnterExitRel (PptTopLevel parent,
                                             PptTopLevel child) {

    if ((parent == null) || (child == null))
      return (null);

    PptRelation rel = new PptRelation (parent, child);
    rel.relationship = "enter -> exit";

    // Look for orig versions of each non-derived parent variable in the child
    for (int i = 0; i < parent.var_infos.length; i++) {
      VarInfo vp = parent.var_infos[i];
      if (vp.derived != null)
        continue;
      VarInfoName orig_name = vp.name.applyPrestate().intern();
      for (int j = 0; j < child.var_infos.length; j++) {
        VarInfo vc = child.var_infos[j];
        if (orig_name == vc.name) {
          rel.child_to_parent_map.put (vc, vp);
          rel.parent_to_child_map.put (vp, vc);
          break;
        }
      }
    }

    // Look for orig versions of derived variables in the child.  This is
    // done by finding the base of each derived variable and looking for
    // a child variable with the same bases and the same equation.  This
    // is necessary because derivations are done AFTER orig variables so
    // applying the prestate name (as done above) won't work (the resulting
    // variable is really the same but the name is constructed differently)

    // Loop through each derived parent (ENTER) variable
    for (int i = 0; i < parent.var_infos.length; i++) {
      VarInfo vp = parent.var_infos[i];
      if (vp.derived == null)
        continue;

      // Get a child version of each of the bases of the derivation
      VarInfo[] vp_bases = vp.derived.getBases();
      VarInfo[] child_vp_bases = new VarInfo[vp_bases.length];
      for (int j = 0; j < vp_bases.length; j++)
        child_vp_bases[j] = rel.childVar (vp_bases[j]);

      // Loop through the child (exit) looking for a matching derived variable
      for (int j = 0; j < child.var_infos.length; j++) {
        VarInfo vc = child.var_infos[j];
        if (vc.derived == null)
          continue;
        if (vc.derived.isSameFormula (vp.derived)) {
          VarInfo[] vc_bases = vc.derived.getBases();
          if (Arrays.equals (child_vp_bases, vc_bases)) {
            rel.child_to_parent_map.put (vc, vp);
            rel.parent_to_child_map.put (vp, vc);
            break;
          }
        }
      }
    }

    // Make sure every ENTER variable was found in the EXIT point
    for (int i = 0; i < parent.var_infos.length; i++) {
      VarInfo vp = parent.var_infos[i];
      if (!rel.parent_to_child_map.containsKey (vp)) {
        System.out.println ("No match for " + vp.name.name() + " from parent "
                            + parent.ppt_name + " in child " + child.ppt_name);
        for (int j = 0; j < child.var_infos.length; j++)
          System.out.println ("    " + child.var_infos[j].name.name());
        //Assert.assertTrue (false, "Missing orig variable in EXIT");
      }
    }
    return (rel);
  }

  /**
   * Returns a relation in the ppt hierarchy from combined
   * exit points (parent) to an individual exit point (child).  Individual
   * exit points are often referred to as exitNN where NN is the line
   * number of the exit point).
   */

  static public PptRelation newCombinedExitExitNNRel (PptTopLevel parent,
                                                      PptTopLevel child) {

    if ((parent == null) || (child == null))
      return (null);

    PptRelation rel = new PptRelation (parent, child);
    rel.relationship = "exit -> exitNN";

    // Create the parent-child variable map.  This one is easy as the
    // variables should match exactly
    Assert.assertTrue (parent.var_infos.length == child.var_infos.length);
    for (int i = 0; i < parent.var_infos.length; i++) {
      VarInfo vc = child.var_infos[i];
      VarInfo vp = parent.var_infos[i];
      Assert.assertTrue (vc.name.name().equals (vp.name.name()));
      rel.child_to_parent_map.put (vc, vp);
      rel.parent_to_child_map.put (vp, vc);
    }
    return (rel);
  }

  public String toString () {
    return (parent.ppt_name + "->" + child.ppt_name + "(" + relationship + ")");
  }
}
