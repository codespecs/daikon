package daikon;

import daikon.inv.*;
import daikon.inv.unary.scalar.*;
import daikon.inv.unary.string.*;
import daikon.inv.unary.sequence.*;
import daikon.inv.unary.stringsequence.*;

import java.io.*;
import java.util.*;
import java.text.*;

import java.util.logging.Logger;
import java.util.logging.Level;

import utilMDE.*;


  /**
   * Class that implements dynamic constants optimization.  This
   * optimization doesn't instantiate invariants over constant
   * variables (ie, that that have only seen one value).  When the
   * variable receives a second value, invariants are instantiated and
   * are given the sample representing the previous constant value.
   **/
public class DynamicConstants implements Serializable {

  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20030913L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /**
   * Boolean.  If true, don't create post processed invariants
   * over dynamic constants.  For experimental purposes only
   **/
  public static boolean dkconfig_no_post_process = false;

  /**
   * Boolean. If true only create OneOf invariants for variables that
   * are constant for the entire run.  If false, all possible invariants
   * are created between constants.  Note that setting this to true only
   * fails to create invariants between constants.  Invariants between
   * constants and non-constants are created regardless.
   *
   * A problem occurs with merging when this is turned on.  If a var_info
   * is constant at one child slice, but not constant at the other child
   * slice, interesting invariants may not be merged because they won't
   * exist on the slice with the constant.  This is thus currently
   * defaulted to false.
   */
  public static boolean dkconfig_OneOf_only = false;

  /** Debug Tracer **/
  public static final Logger debug
                          = Logger.getLogger ("daikon.DynamicConstants");

  /** list of dynamic constants **/
  List /*Constant*/ con_list = new ArrayList();

  /** list of variables that have always been missing */
  List /*Constant*/ missing_list = new ArrayList();

  /** list of all variables **/
  List /*Constant*/ all_list = new ArrayList();

  /** program point of these constants **/
  PptTopLevel ppt = null;

  /** number of sample received **/
  int sample_cnt = 0;

  /**
   * Class used to store the value and count for each constant
   * Note that two objects of this class are equal if they refer
   * to the same variable.  This allows these to be stored in
   * sets
   **/
  public static class Constant implements Serializable {

    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20030913L;

    public Object val;
    public int count;
    public VarInfo vi;

    public Constant (VarInfo vi) {
      this.vi = vi;
      this.val = null;
      this.count = 0;
    }

    public boolean equals (Object obj) {
      if (!(obj instanceof Constant))
        return (false);
      Constant c = (Constant) obj;
      return (c.vi == vi);
    }

    public int hashCode() {
      return (vi.hashCode());
    }

    public String toString() {

      StringBuffer out = new StringBuffer();
      out.append (vi.name.name());
      if (val == null)
        out.append (" (val missing)");
      else
        out.append (" (val=" + val + ")");
      if (vi.isCanonical())
        out.append (" (leader) ");
      return (out.toString());
    }
  }

  /** compares two constants based on the vi_index of their variable **/
  public static final class ConIndexComparator
    implements Comparator, Serializable {

    private ConIndexComparator() {
    }

    public int compare(Object o1, Object o2) {
      Constant con1 = (Constant) o1;
      Constant con2 = (Constant) o2;
      return (con1.vi.varinfo_index - con2.vi.varinfo_index);
    }

    public static ConIndexComparator getInstance() {
      return theInstance;
    }
    static final ConIndexComparator theInstance = new ConIndexComparator();
  }

  /**
   * Create an initial list of constants and missing variables for the
   * specified ppt.
   */
  public DynamicConstants (PptTopLevel ppt) {

    this.ppt = ppt;

    // Start everything off as missing (since we haven't seen any values yet)
    for (int i = 0; i < ppt.var_infos.length; i++) {
      VarInfo vi = ppt.var_infos[i];
      missing_list.add (new Constant (vi));
      all_list.add (new Constant (vi));
    }

  }

  /**
   * Checks each current constant to see if it is still a constant.
   * Constants must have the same value and cannot be missing.  In the
   * long run a better job of dealing with missing might be helpful.
   * Returns a list of any constants that are no longer constant and
   * by side-effect all of the variables that are no longer always missing.
   */
  public List /*Constant*/ add (ValueTuple vt, int count,
                                List /*Constant*/ non_missing) {

    List /*Constant*/ non_con = new ArrayList();
    non_missing.clear();

    // Check each constant, destroy any that are missing or different
    for (Iterator i = con_list.iterator(); i.hasNext(); ) {
      Constant con = (Constant) i.next();
      Object val = con.vi.getValue (vt);
      if (Debug.logDetail())
        Debug.log (getClass(), ppt, Debug.vis(con.vi), "Adding "
                   + Debug.toString(val) +
                   " to constant " + con.val +" : missing = "
                   + missing (con.vi, vt)
                  +": samples = " + con.count + "/" + count);
      if ((con.val != val) || missing (con.vi, vt)) {
        i.remove();
        non_con.add (con);
      } else {
        con.count += count;
      }
    }

    // Move any non-missing variables to the constant list and init their val
    // If a variable is missing out of bounds, leave it on this list
    // forever (guranteeing that invariants will never be instantiated over
    // it)
    for (Iterator i = missing_list.iterator(); i.hasNext(); ) {
      Constant con = (Constant) i.next();
      if (con.vi.missingOutOfBounds())
        continue;
      Object val = con.vi.getValue (vt);
      if (!missing (con.vi, vt)) {
        i.remove();
        if (Debug.logDetail())
          Debug.log (getClass(), ppt, Debug.vis(con.vi), "Adding "
                     + Debug.toString(val) +
                     " to missing : missing = "
                     + missing (con.vi, vt)
                    +": samples = " + con.count + "/" + count);
        if (sample_cnt == 0) {
          con.val = val;
          con.count = count;
          con_list.add (con);
        } else {
          non_missing.add (con);
        }
      }
    }

    sample_cnt += count;
    return (non_con);
  }

  /** returns whether the specified variable is missing in this ValueTuple **/
  private boolean missing (VarInfo vi, ValueTuple vt) {

    int mod = vt.getModified (vi);
    return ((mod == ValueTuple.MISSING_FLOW)
            || (mod == ValueTuple.MISSING_NONSENSICAL));
  }

  /** returns whether the specified variable is currently a constant **/
  public boolean is_constant (VarInfo vi) {

    for (int i = 0; i < con_list.size(); i++) {
      Constant con = (Constant) con_list.get(i);
      if (con.vi == vi)
        return (true);
    }
    return (false);
  }

  /** returns whether the specified variable missing for all values so far **/
  public boolean is_missing (VarInfo vi) {

    for (int i = 0; i < missing_list.size(); i++) {
      Constant con = (Constant) missing_list.get(i);
      if (con.vi == vi)
        return (true);
    }
    return (false);
  }

  /** returns the number of constants that are leaders **/
  public int constant_leader_cnt() {

    int con_cnt = 0;
    for (int i = 0; i < con_list.size(); i++) {
      Constant con = (Constant) con_list.get(i);
      if (con.vi.isCanonical())
        con_cnt++;
    }

    return (con_cnt);
  }

  /**
   * Creates all new views required for the newly non constants (noncons)
   * and the newly non-missing (non_missing)
   */
  public void instantiate_new_views (List /*Constant*/ noncons,
                                     List /*Constant*/ non_missing) {

    if (Debug.logOn()) {
      for (int i = 0; i < noncons.size(); i++) {
        Constant con = (Constant) noncons.get (i);
        Debug.log (getClass(), ppt, Debug.vis(con.vi), "is non constant"
                    + " with val = " + Debug.toString(con.val)
                    + " with count = " + con.count);
      }
      for (int i = 0; i < non_missing.size(); i++) {
        Constant con = (Constant) non_missing.get (i);
        Debug.log (getClass(), ppt, Debug.vis(con.vi), "is non missing");
      }
    }


    // Create all of the views over noncons and noncons+con_list.
    // Since everything starts out as a constant, it is only necessary
    // to combine the newly non-constants with a combination of
    // the remaining constants and the newly-non constants.  Any slices
    // between the non-constants and other variables will have already
    // been created when those other variables became non-constants.
    if (noncons.size() > 0) {
      List cons = new ArrayList();
      cons.addAll (con_list);
      cons.addAll (noncons);
      debug.fine ("Instantiating non constants in ppt: " + ppt.name());
      instantiate_views (noncons, cons);
    }

    // Create all views over the newly non-missing.  Since missing
    // vars were not included in any previous views, we must match them
    // against all variables.
    if (non_missing.size() > 0) {
      debug.fine ("Instantiating non missing in ppt: " + ppt.name());
      instantiate_views (non_missing, all_list);
    }
  }


  /**
   * Instantiate views and invariants across each combination of
   * vars from list1 and list2.  If each item in a new slice
   * was a constant, the constant values are applied.
   *
   * The following slices will be created:
   *    unary:   list1-vars
   *    binary:  list1-vars X list2-vars
   *    ternary: list1-vars X list2-vars X list2-vars
   */
  private void instantiate_views (List /*Constant*/ list1,
                                  List /*Constant*/ list2) {

    // Get list1 leaders
    Set leaders1 = new LinkedHashSet();
    for (int i = 0; i < list1.size(); i++) {
      Constant con = (Constant) list1.get(i);
      if (con.vi.isCanonical())
        leaders1.add (con);
    }

    // Get list2 leaders
    Set leaders2 = new LinkedHashSet();
    for (int i = 0; i < list2.size(); i++) {
      Constant con = (Constant) list2.get(i);
      if (con.vi.isCanonical())
        leaders2.add (con);
    }

    if (debug.isLoggable (Level.FINE)) {
      debug.fine ("instantiating over " + leaders1.size()
                  + " leaders1: " + leaders1);
      debug.fine ("instantiating over " + leaders2.size()
                  + " leaders2: " + leaders2);
    }

    // any new views created
    Vector new_views = new Vector();

    int mod = ValueTuple.MODIFIED;

    // Unary slices/invariants
    for (Iterator i = leaders1.iterator(); i.hasNext(); ) {
      Constant con = (Constant) i.next();
      if (!ppt.is_slice_ok (con.vi))
         continue;
      PptSlice1 slice1 = new PptSlice1 (ppt, con.vi);
      slice1.instantiate_invariants();
      if (ppt.is_slice_global (con.vi))
        slice1.remove_global_invs();
      if (con.count > 0) {
        if (Daikon.dkconfig_df_bottom_up)
          slice1.add_val_bu (con.val, mod, con.count);
        else
          slice1.add_val(con.val, mod, con.count);
      }
      new_views.add (slice1);
    }

    // Binary slices/invariants.  Note that if a variable is in both
    // leader lists, it is only added when it is in order (to prevent
    // creating the slice twice)
    for (Iterator i = leaders1.iterator(); i.hasNext(); ) {
      Constant con1 = (Constant) i.next();
      for (Iterator j = leaders2.iterator(); j.hasNext(); ) {
        Constant con2 = (Constant) j.next();
        Constant c1 = con1;
        Constant c2 = con2;
        Debug.log (getClass(), ppt, Debug.vis(c1.vi, c2.vi),
                   "Considering slice");
        if (con2.vi.varinfo_index < con1.vi.varinfo_index) {
          if (leaders1.contains (con2))
            continue;
          c1 = con2;
          c2 = con1;
        }
        if (!ppt.is_slice_ok (c1.vi, c2.vi)) {
          if (Debug.logOn())
            Debug.log (debug, getClass(), ppt, Debug.vis(c1.vi, c2.vi),
                       "Not instantiating slice " + c1.vi.equalitySet.size());
          continue;
        }
        PptSlice2 slice2 = new PptSlice2 (ppt, c1.vi, c2.vi);
        slice2.instantiate_invariants();
        Debug.log (getClass(), ppt, Debug.vis(c1.vi, c2.vi), "slice_global= "
                   + ppt.is_slice_global (c1.vi, c2.vi));
        if (ppt.is_slice_global (c1.vi, c2.vi))
          slice2.remove_global_invs();
        if (c1.count > 0 && c2.count > 0) {
          if (Daikon.dkconfig_df_bottom_up)
            slice2.add_val_bu (c1.val, c2.val, mod, mod, con1.count);
          else
            slice2.add_val (c1.val, c2.val, mod, mod, con1.count);
        }
        new_views.add (slice2);
      }
    }

    // Ternary slices/invariants.  Note that if a variable is in both
    // leader lists, it is only added when it is in order (to prevent
    // creating the slice twice)
    for (Iterator i = leaders1.iterator(); i.hasNext(); ) {
      Constant con1 = (Constant) i.next();
      for (Iterator j = leaders2.iterator(); j.hasNext(); ) {
        Constant con2 = (Constant) j.next();
        if ((con2.vi.varinfo_index < con1.vi.varinfo_index)
            && leaders1.contains (con2))
          continue;
        for (Iterator k = leaders2.iterator(); k.hasNext(); ) {
          Constant con3 = (Constant) k.next();
          if ((con3.vi.varinfo_index < con2.vi.varinfo_index) ||
          	  ((con3.vi.varinfo_index < con1.vi.varinfo_index)
          	  && leaders1.contains (con3)))
            continue;
          Constant con_arr[] = {con1, con2, con3};
          Arrays.sort (con_arr, ConIndexComparator.getInstance());
          Assert.assertTrue ((con_arr[0].vi.varinfo_index
                              <= con_arr[1].vi.varinfo_index) &&
                             (con_arr[1].vi.varinfo_index
                              <= con_arr[2].vi.varinfo_index));
          if (!ppt.is_slice_ok (con_arr[0].vi, con_arr[1].vi, con_arr[2].vi))
            continue;

          PptSlice3 slice3 = new PptSlice3 (ppt, con_arr[0].vi, con_arr[1].vi,
                                            con_arr[2].vi);
          slice3.instantiate_invariants();
          if (ppt.is_slice_global (con_arr[0].vi, con_arr[1].vi,con_arr[2].vi))
            slice3.remove_global_invs();
          if ((con_arr[0].count > 0) && (con_arr[1].count > 0)
              && (con_arr[2].count > 0)) {
            if (Daikon.dkconfig_df_bottom_up)
              slice3.add_val_bu (con_arr[0].val, con_arr[1].val,
                              con_arr[2].val, mod, mod, mod, con_arr[0].count);
            else
              slice3.add_val (con_arr[0].val, con_arr[1].val,
                              con_arr[2].val, mod, mod, mod, con_arr[0].count);
          }
          new_views.add (slice3);
        }
      }
    }

    // Debug print the created slies
    if (Debug.logOn() || debug.isLoggable (Level.FINE)) {
      int[] slice_cnt = {0, 0, 0, 0};
      int[] non_gslice_cnt = {0, 0, 0, 0};
      int[] non_ginv_cnt = {0, 0, 0, 0};
      int[] inv_cnt = {0, 0, 0, 0};
      int[] true_inv_cnt = {0, 0, 0, 0};
      for (int i = 0; i < new_views.size(); i++) {
        PptSlice slice = (PptSlice) new_views.get (i);
        for (int j = 0; j < slice.invs.size(); j++) {
          Invariant inv = (Invariant) slice.invs.get (j);
          inv.log ("created, falsified = " + inv.is_false());
          if (!inv.is_false())
            true_inv_cnt[slice.arity()]++;
        }
        if (!ppt.is_slice_global (slice.var_infos) && slice.invs.size() > 0) {
          non_gslice_cnt[slice.arity()]++;
          non_ginv_cnt[slice.arity()] += slice.invs.size();
        }
        if (slice.invs.size() > 0)
          slice_cnt[slice.arity()]++;
        inv_cnt[slice.arity()] += slice.invs.size();
        if (Debug.logDetail()) {
          StringBuffer sb = new StringBuffer();
          for (int j = 0; j < slice.arity(); j++) {
            VarInfo v = slice.var_infos[j];
            sb.append (v.name.name() + " [" + v.file_rep_type +"] ["
                        + v.comparability + "] ");
          }
          Debug.log (debug, getClass(), ppt, slice.var_infos,
                      "Adding slice over " + sb + ": with " + slice.invs.size()
                      + " invariants" );
        }
      }
      for (int i = 1; i <= 3; i++)
        debug.fine ("Added " + slice_cnt[i] + " slice" + i + "s with "
                    + true_inv_cnt[i] + " invariants (" + inv_cnt[i]
                    + " total): with " + non_gslice_cnt[i]
                    + " non global slices, with " + non_ginv_cnt[i]
                    + " invariants");

      String leader1_str = "";
      int leader1_cnt = 0;
      for (Iterator i = leaders1.iterator(); i.hasNext(); ) {
        Constant con1 = (Constant) i.next();
        if (con1.vi.file_rep_type == ProglangType.INT) {
          leader1_str += con1.vi.name.name() + " ";
          leader1_cnt++;
        }
      }

      String leader2_str = "";
      int leader2_cnt = 0;
      for (Iterator i = leaders2.iterator(); i.hasNext(); ) {
        Constant con1 = (Constant) i.next();
        if (con1.vi.file_rep_type == ProglangType.INT) {
          leader2_str += con1.vi.name.name() + " ";
          leader2_cnt++;
        }
      }
      debug.fine (leader1_cnt + " leader1 ints (" + leader1_str + "): "
                  + leader2_cnt + " leader2 ints (" + leader2_str);
    }


    // Remove any false invariants
    {
      List/*Invariant*/ toRemove;
      for (int i = 0; i < new_views.size(); i++) {
        PptSlice slice = (PptSlice) new_views.get (i);
        toRemove = new ArrayList();
        for (Iterator j = slice.invs.iterator(); j.hasNext(); ) {
          Invariant inv = (Invariant) j.next();
          if (inv.is_false())
            toRemove.add(inv);
        }
        slice.removeInvariants(toRemove);
      }
    }

    // Add the new slices to the top level ppt
    ppt.addViews (new_views);

    // Attempt to suppress any new invariants
    for (int i = 0; i < new_views.size(); i++) {
      PptSlice slice = (PptSlice) new_views.get (i);
      for (Iterator j = slice.invs.iterator(); j.hasNext(); ) {
        Invariant inv = (Invariant) j.next();
        ppt.attemptSuppression (inv, true);
      }
    }

  }

  /**
   * Create invariants for any remaining constants.  Right now, this looks
   * for invariants between all of the constants.  Its not clear that
   * between constants are interesting, but to match previous behavior, this
   * is what we will do for now.
   */
  public void post_process () {

    // if requested, don't create any post-processed invariants
    if (dkconfig_no_post_process) {
      int con_count = 0;
      for (int i = 0; i < con_list.size(); i++) {
        Constant con = (Constant) con_list.get(i);
        if (!con.vi.isCanonical())
          continue;
        System.out.println ("  Not creating invariants over leader "
                            + con.vi.name.name() + " = " + con.val);
        con_count++;
      }
      System.out.println (con_count + " constants at ppt " + ppt);
      return;
    }

    // If specified, create only OneOf invariants.  Also create a reflexive
    // equality invariant, since that is assumed to exist in many places
    if (dkconfig_OneOf_only) {
      for (int i = 0; i < con_list.size(); i++) {
        Constant con = (Constant) con_list.get(i);
        if (!con.vi.isCanonical())
          continue;
        instantiate_oneof (con);
        ppt.create_equality_inv (con.vi, con.vi, con.count);
      }
      return;
    }

    // Get a list of all remaining constants and clear the existing list
    // (if the existing list is not cleared, constant slices will not
    // be created)
    List noncons = con_list;
    con_list = new ArrayList();

    // Don't do anything with variables that have always been missing.  They
    // should have no invariants over them.
    List non_missing = new ArrayList();

    instantiate_new_views (noncons, non_missing);

  /* Code to just create just unary slices for constants
    for (int i = 0; i < con_list.size(); i++) {
      Constant con = (Constant) con_list.get(i);
      if (!con.vi.isCanonical())
        continue;
      PptSlice1 slice1 = new PptSlice1 (ppt, con.vi);
      slice1.instantiate_invariants();
      if (con.val != null)
        slice1.add_val (con.val, ValueTuple.MODIFIED, con.count);
      new_views.add (slice1);
    }
    ppt.addViews (new_views);
  */
  }

  public void print_missing (PrintWriter out) {

    for (int i = 0; i < missing_list.size(); i++) {
      Constant con = (Constant) missing_list.get(i);
      out.println (con.vi.name.name() + " is always missing");
    }
  }

  /**
   * Merge dynamic constants from the children of this ppt.  Only missing
   * is merged since constants are not used after we are done processing
   * samples.
   */

  public void merge () {

    // clear the constant and missing lists
    missing_list.clear();
    con_list.clear();

    // Process each variable at this ppt.  If the variable is missing at
    // each of the children, it is also missing here.  Ignore children that
    // have no mapping for this variable
    for (int i = 0; i < ppt.var_infos.length; i++) {
      VarInfo pvar = ppt.var_infos[i];
      boolean missing = true;
      for (int j = 0; j < ppt.children.size(); j++) {
        PptRelation rel = (PptRelation) ppt.children.get(j);
        VarInfo cvar = rel.childVar (pvar);
        if ((cvar != null) && (rel.child.constants != null)
            && !rel.child.constants.is_missing (cvar)) {
          missing = false;
          break;
        }
      }
      if (missing)
        missing_list.add (new Constant (pvar));
    }
  }

  public void instantiate_oneof (Constant con) {

    Invariant inv = null;
    PptSlice1 slice1 = (PptSlice1) ppt.get_or_instantiate_slice (con.vi);

    // Create the correct OneOf invariant
    ProglangType rep_type = con.vi.rep_type;
    boolean is_scalar = rep_type.isScalar();
    if (is_scalar) {
      inv = OneOfScalar.instantiate (slice1);
    } else if (rep_type == ProglangType.INT_ARRAY) {
      inv = OneOfSequence.instantiate (slice1);
    } else if (Daikon.dkconfig_enable_floats
               && rep_type == ProglangType.DOUBLE) {
      inv = OneOfFloat.instantiate (slice1);
    } else if (Daikon.dkconfig_enable_floats
               && rep_type == ProglangType.DOUBLE_ARRAY) {
      inv = OneOfFloatSequence.instantiate (slice1);
    } else if (rep_type == ProglangType.STRING) {
      inv = OneOfString.instantiate (slice1);
    } else if (rep_type == ProglangType.STRING_ARRAY) {
      inv = OneOfStringSequence.instantiate (slice1);
    } else {
      // Do nothing; do not even complain
    }
    slice1.addInvariant (inv);

    // Add the value to it
    slice1.add_val_bu (con.val, ValueTuple.MODIFIED, con.count);
  }

  /**
   * Instantiate views and invariants over the specified list of
   * non-constants.  Views are created over each variable in the
   * non-constant list crossed with itself and each constant variable.
   *
   * NOT USED, KEPT FOR REFERENCE ONLY!!
   */
  private void old_instantiate_views (List /*Constant*/ noncons) {

    if (debug.isLoggable (Level.FINE)) {
      debug.fine ("instantiating over " + noncons.size() + " non-constants: "
                  + noncons);
      debug.fine (con_list.size() + " remaining constants: " + con_list);
    }

    // Remove any new non-constants that are not leaders
    for (Iterator i = noncons.iterator(); i.hasNext(); ) {
      Constant con = (Constant) i.next();
      if (!con.vi.isCanonical())
        i.remove();
    }

    if (debug.isLoggable (Level.FINE))
      debug.fine ("instantiating over " + noncons.size()
                  + " non-constants leaders: " + noncons);

    // Create a list of constant leaders + the new noncon leaders
    List cons = new ArrayList();
    for (int i = 0; i < con_list.size(); i++) {
      Constant con = (Constant) con_list.get(i);
      if (con.vi.isCanonical())
        cons.add (con);
    }
    for (int i = 0; i < noncons.size(); i++ )
      cons.add (noncons.get(i));

    // any new views created
    Vector new_views = new Vector();

    int mod = ValueTuple.MODIFIED;

    // Unary slices/invariants
    for (int i = 0; i < noncons.size(); i++) {
      Constant con = (Constant) noncons.get(i);
      if (!ppt.is_slice_ok (con.vi))
        continue;
      PptSlice1 slice1 = new PptSlice1 (ppt, con.vi);
      slice1.instantiate_invariants();
      if (con.val != null)
        slice1.add_val (con.val, mod, con.count);
      new_views.add (slice1);
    }

    // Binary slices/invariants
    for (int i = 0; i < noncons.size(); i++) {
      Constant con1 = (Constant) noncons.get(i);
      for (int j = 0; j < cons.size(); j++) {
        Constant con2 = (Constant) cons.get(j);
        Constant c1 = con1;
        Constant c2 = con2;
        if (con2.vi.varinfo_index < con1.vi.varinfo_index) {
          c1 = con2;
          c2 = con1;
        }
        if (!ppt.is_slice_ok (c1.vi, c2.vi))
          continue;
        PptSlice2 slice2 = new PptSlice2 (ppt, c1.vi, c2.vi);
        slice2.instantiate_invariants();
        if (c1.val != null && c2.val != null)
          slice2.add_val (c1.val, c2.val, mod, mod, con1.count);
        new_views.add (slice2);
      }
    }

    // Ternary slices/invariants
    for (int i = 0; i < noncons.size(); i++) {
      Constant con1 = (Constant) noncons.get(i);
      for (int j = 0; j < cons.size(); j++) {
        Constant con2 = (Constant) cons.get(j);
        for (int k = 0; k < cons.size(); k++) {
          Constant con3 = (Constant) cons.get (k);
          Constant con_arr[] = {con1, con2, con3};
          Arrays.sort (con_arr, ConIndexComparator.getInstance());
          Assert.assertTrue ((con_arr[0].vi.varinfo_index
                              <= con_arr[1].vi.varinfo_index) &&
                             (con_arr[1].vi.varinfo_index
                              <= con_arr[2].vi.varinfo_index));
          if (!ppt.is_slice_ok (con_arr[0].vi, con_arr[1].vi, con_arr[2].vi))
            continue;

          PptSlice3 slice3 = new PptSlice3 (ppt, con_arr[0].vi, con_arr[1].vi,
                                            con_arr[2].vi);
          slice3.instantiate_invariants();
          if ((con_arr[0].val != null) && (con_arr[1].val != null)
              && (con_arr[2].val != null))
            slice3.add_val (con_arr[0].val, con_arr[1].val, con_arr[2].val,
                            mod, mod, mod, con_arr[0].count);
          new_views.add (slice3);
        }
      }
    }

    // Make sure that all of these slices are new
    for (int i = 0; i < new_views.size(); i++) {
      PptSlice slice = (PptSlice) new_views.get (i);
      PptSlice current = ppt.findSlice (slice.var_infos);
      Assert.assertTrue (current == null, "Slice " + current
                                          + " already exists");
    }

    // Debug print the created slies
    if (debug.isLoggable (Level.FINE)) {
      int[] slice_cnt = {0, 0, 0, 0};
      int[] inv_cnt = {0, 0, 0, 0};
      for (int i = 0; i < new_views.size(); i++) {
        PptSlice slice = (PptSlice) new_views.get (i);
        if (slice.invs.size() > 0)
          slice_cnt[slice.arity()]++;
        inv_cnt[slice.arity()] += slice.invs.size();
        if (Debug.logDetail()) {
          StringBuffer sb = new StringBuffer();
          for (int j = 0; j < slice.arity(); j++) {
            VarInfo v = slice.var_infos[j];
            sb.append (v.name.name() + " [" + v.file_rep_type +"] ["
                        + v.comparability + "] ");
          }
          debug.fine ("Adding slice over " + sb + ": with " + slice.invs.size()
                      + " invariants");
        }
      }
      for (int i = 1; i <= 3; i++)
        debug.fine ("Added " + slice_cnt[i] + " slice" + i + "s with "
                    + inv_cnt[i] + " invariants");
    }

    // Remove any false invariants
    for (int i = 0; i < new_views.size(); i++) {
      PptSlice slice = (PptSlice) new_views.get (i);
      for (Iterator j = slice.invs.iterator(); j.hasNext(); ) {
        Invariant inv = (Invariant) j.next();
        if (inv.is_false())
          j.remove();
      }
    }

    // Add the new slices to the top level ppt
    ppt.addViews (new_views);

    // Attempt to suppress any new invariants
    for (int i = 0; i < new_views.size(); i++) {
      PptSlice slice = (PptSlice) new_views.get (i);
      for (Iterator j = slice.invs.iterator(); j.hasNext(); ) {
        Invariant inv = (Invariant) j.next();
        ppt.attemptSuppression (inv, true);
      }
    }

  }


}
