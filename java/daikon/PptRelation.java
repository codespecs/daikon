package daikon;

import daikon.inv.*;
import java.io.*;
import java.util.*;
import utilMDE.*;
import java.util.logging.Logger;
import java.util.logging.Level;

/**
 * Base class for relations in the ppt hierachy.  Extenders are specific
 * for each type of parent/child relationship (eg, method to object,
 * exit to combined exit, etc).
 */

public class PptRelation {

  /** parent of relation **/
  public PptTopLevel parent;

  /** child of relation **/
  public PptTopLevel child;

  /** map from parent vars to matching child vars */
  Map parent_to_child_map;

  /** map from child vars to matching parent vars */
  Map child_to_parent_map;

  /**
   * Map between child and parent variables.  Looking up a parent
   * variable gets the corresponding variable in the child and vice-versa.
   * When a variable exists in either the child or the parent, but not
   * the other, the variable will not be added to the map (ie, get will
   * return null)
   */
  LinkedHashMap varmap;

  public PptRelation() {

    parent = null;
    child = null;
    parent_to_child_map = null;
    child_to_parent_map = null;
  }

  public PptRelation (PptTopLevel parent, PptTopLevel child) {

    this.parent = parent;
    this.child = child;
    parent_to_child_map = new LinkedHashMap();
    child_to_parent_map = new LinkedHashMap();
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
   * Ignore variables named exactly 'this' since a this point in an object
   * is guaranteed to be non-null and a pointer to the object is not.
   */
  public void relate_same_name() {

    for (int i = 0; i < parent.var_infos.length; i++) {
      VarInfo vp = parent.var_infos[i];
      if (vp.name.equals (VarInfoName.THIS))
        continue;
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

  public List get_child_equalities_as_parent(){

    List elist = new ArrayList();

    for (int i = 0; i < child.equality_view.invs.size(); i++) {
      Equality e = (Equality) child.equality_view.invs.get (i);
      VarInfo[] varr = (VarInfo[]) e.getVars().toArray();
      for (int j = 0; j < varr.length; j++) {
        VarInfo v1 = parentVar (varr[j]);
        if (v1 == null)
          continue;
        for (int k = j+1; k < varr.length; k++) {
          VarInfo v2 = parentVar (varr[j]);
          if (v2 == null)
            continue;
          elist.add (new VarInfo.Pair (v1, v2));
        }
      }
    }
    return (elist);
  }

  // abstract public void

  // both of the below need to figure out the best way to return these
  // so they are in the context of the caller (ie, deal with the different
  // vars between the parent and the child (eg, this.x in the parent
  // and arg.x in the child))

  // return equality sets from the child

  // return invariants from the child

}
