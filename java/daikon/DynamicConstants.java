package daikon;

import daikon.inv.*;
import daikon.inv.binary.*;
import daikon.inv.binary.twoScalar.*;
import daikon.inv.ternary.*;
import daikon.inv.ternary.threeScalar.*;
import daikon.inv.unary.*;
import daikon.inv.unary.scalar.*;
import daikon.inv.unary.sequence.*;
import daikon.inv.unary.string.*;
import daikon.inv.unary.stringsequence.*;
import daikon.suppress.*;
import java.io.*;
import java.util.*;
import java.util.logging.Level;
import java.util.logging.Logger;
import plume.*;

/*>>>
import org.checkerframework.checker.interning.qual.*;
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

/**
 * Class that implements dynamic constants optimization. This optimization doesn't instantiate
 * invariants over constant variables (i.e., that that have only seen one value). When the variable
 * receives a second value, invariants are instantiated and are given the sample representing the
 * previous constant value. Each DynamicConstants object is associated with a single program point,
 * ppt.
 */
public class DynamicConstants implements Serializable {

  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20040401L;

  // If true don't create any invariants (including OneOfs) over dynamic
  // constants during post processing.  Normally, the configuration
  // variable OneOf_only is more appropriate.
  static final boolean no_post_process = false;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.

  /**
   * Whether to use the dynamic constants optimization. This optimization doesn't instantiate
   * invariants over constant variables (i.e., that that have only seen one value). When the
   * variable receives a second value, invariants are instantiated and are given the sample
   * representing the previous constant value.
   */
  public static boolean dkconfig_use_dynamic_constant_optimization = true;

  /**
   * Boolean. Controls which invariants are created for variables that are constant for the entire
   * run. If true, create only OneOf invariants. If false, create all possible invariants.
   *
   * <p>Note that setting this to true only fails to create invariants between constants. Invariants
   * between constants and non-constants are created regardless.
   *
   * <p>A problem occurs with merging when this is turned on. If a var_info is constant at one child
   * slice, but not constant at the other child slice, interesting invariants may not be merged
   * because they won't exist on the slice with the constant. This is thus currently defaulted to
   * false.
   */
  public static boolean dkconfig_OneOf_only = false;

  /** Debug tracer. */
  public static final Logger debug = Logger.getLogger("daikon.DynamicConstants");

  /**
   * List of dynamic constants.
   *
   * <p>Each element, c, has c.constant = true, c.count &gt; 0, elt.val != null.
   */
  List<Constant> con_list = new ArrayList<Constant>();

  /**
   * List of variables that have always been missing.
   *
   * <p>For each element c, c.always_missing = true or con.vi.missingOutOfBounds().
   */
  List<Constant> missing_list = new ArrayList<Constant>();

  // Same contents in both.  Why two data structures?
  /** List of all variables. Some may be non-constant. Same length and indexing as ppt.var_infos. */
  Constant[] all_vars;

  /** List of all variables. Some may be non-constant. Same length and indexing as ppt.var_infos. */
  List<Constant> all_list = new ArrayList<Constant>();

  /** Program point of these constants. */
  PptTopLevel ppt;

  /** Number of samples received. */
  int sample_cnt = 0;

  /**
   * Class used to indicate, for each variable, whether it is constant (see boolean field
   * "constant"). If it is, then the class also stores its constant value and its sample count.
   *
   * <p>Note that two objects of this class are equal if they refer to the same variable. This
   * allows these to be stored in sets.
   */
  public static /*@Interned*/ class Constant implements Serializable {

    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20030913L;

    // XXX Question: what if the constant value is itself null, as for a
    // String or pointer?  Does the code distinguish that case from val not
    // being set?
    /**
     * The value of the constant, or the previous constant value if constant==false and
     * previous_constant==true. Null iff count=0.
     */
    public /*@MonotonicNonNull*/ /*@Interned*/ Object val = null;

    /** The sample count of the constant. */
    public int count = 0;

    /** The variable that has this value. */
    public VarInfo vi;

    /** Whether or not this has been missing for every sample to date. */
    boolean always_missing = true;

    /** Whether or not this is constant. */
    boolean constant = false;

    /**
     * Whether or not this was constant at the beginning of this sample. At the beginning of the
     * add() method, all newly non constant variables are marked (constant=false). It is sometimes
     * useful within the remainder of processing that sample to know that a variable was constant at
     * the beginning. The field previous_constant is set to true when constant is set to false, and
     * then is itself set to false at the end of the add() method.
     */
    boolean previous_constant = false;

    /**
     * Whether or not this was always missing at the beginning of this sample. At the beginning of
     * the add() method, all newly non missing variables are marked (always_missing=false). It is
     * sometimes useful within the remainder of processing that sample to know that a variable was
     * missing at the beginning. The field previous_missing set to true when missing is set to
     * false, and then is itself set to false at the end of the add() method.
     */
    boolean previous_missing = false;

    /** Check representation invariant. */
    public void checkRep() {
      // This assertion is not valid.  If first sample is missing, then
      // always_missing=true and previous_missing=false.
      // assert (always_missing ? previous_missing : true) : toString();

      assert !(constant && previous_constant) : toString();

      // Whereas values can be null, null is never the value for a dynamic
      // constant.
      assert ((constant || previous_constant)
              ? (val != null && count > 0)
              : (val == null && count == 0))
          : toString();
    }

    public Constant(VarInfo vi) {
      this.vi = vi;
    }

    /**
     * Returns whether the specified variable is currently a constant OR was a constant at the
     * beginning of constants processing.
     */
    /*@Pure*/
    public boolean is_prev_constant() {
      return constant || previous_constant;
    }

    /*@EnsuresNonNullIf(result=true, expression="#1")*/
    /*@Pure*/
    public boolean equals(
        /*>>>@GuardSatisfied Constant this,*/
        /*@GuardSatisfied*/ /*@Nullable*/ Object obj) {
      if (!(obj instanceof Constant)) {
        return false;
      }
      Constant c = (Constant) obj;
      return (c.vi == vi);
    }

    /*@Pure*/
    public int hashCode(/*>>>@GuardSatisfied Constant this*/) {
      return (vi.hashCode());
    }

    @SuppressWarnings("purity") // side effects to local state (string creation)
    /*@SideEffectFree*/
    public String toString(/*>>>@GuardSatisfied Constant this*/) {

      StringBuffer out = new StringBuffer();
      out.append(vi.name());
      if (val == null) {
        out.append(" (val missing)");
      } else {
        out.append(" (val=" + val + ")");
      }
      if (vi.isCanonical()) out.append(" (leader) ");
      out.append(
          " [always_missing="
              + always_missing
              + ", constant="
              + constant
              + ", previous_constant="
              + previous_constant
              + ", previous_missing="
              + previous_missing
              + "]");
      return (out.toString());
    }
  }

  /** Compares two constants based on the vi_index of their variable. */
  public static final class ConIndexComparator implements Comparator<Constant>, Serializable {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20050923L;

    private ConIndexComparator() {}

    /*@Pure*/
    public int compare(Constant con1, Constant con2) {
      return (con1.vi.varinfo_index - con2.vi.varinfo_index);
    }

    public static ConIndexComparator getInstance() {
      return theInstance;
    }

    static final ConIndexComparator theInstance = new ConIndexComparator();
  }

  /** Create an initial list of constants and missing variables for the specified ppt. */
  public DynamicConstants(PptTopLevel ppt) {

    this.ppt = ppt;

    // Start everything off as missing (since we haven't seen any values yet)
    for (VarInfo vi : ppt.var_infos) {
      Constant c = new Constant(vi);
      all_list.add(c);
      missing_list.add(c);
    }
    all_vars = all_list.toArray(new Constant[all_list.size()]);
  }

  /**
   * Checks each current constant to see if it is still a constant. Constants must have the same
   * value and cannot be missing. In the long run a better job of dealing with missing might be
   * helpful. Also checks each variable that has always been missing to date to insure that it is
   * still missing.
   *
   * <p>Creates all new views required for the newly non constants (noncons) and the newly
   * non-missing (non_missing).
   */
  public void add(ValueTuple vt, int count) {

    // System.out.println("DynamicConstants.add : " + vt.toString(ppt.var_infos));

    List<Constant> non_missing = new ArrayList<Constant>();
    List<Constant> non_con = new ArrayList<Constant>();

    // Check each constant, destroy any that are missing or different
    for (Iterator<Constant> i = con_list.iterator(); i.hasNext(); ) {
      Constant con = i.next();
      assert con.constant;
      con.checkRep();

      if (Debug.logDetail()) {
        Debug.log(
            getClass(),
            ppt,
            Debug.vis(con.vi),
            "Adding "
                + Debug.toString(con.vi.getValueOrNull(vt))
                + " to constant "
                + con.val
                + " : missing = "
                + missing(con.vi, vt)
                + ": samples = "
                + con.count
                + "/"
                + count);
      }
      if (missing(con.vi, vt) || (con.val != con.vi.getValue(vt))) {
        i.remove();
        con.constant = false;
        con.previous_constant = true;
        assert all_vars[con.vi.varinfo_index].constant == false;
        non_con.add(con);
      } else {
        con.count += count;
      }
      con.checkRep();
    }

    // Move any non-missing variables to the constant list and init their val.
    // If a variable is missing out of bounds, leave it on this list
    // forever (guranteeing that invariants will never be instantiated over
    // it).
    for (Iterator<Constant> i = missing_list.iterator(); i.hasNext(); ) {
      Constant con = i.next();
      con.checkRep();
      if (con.vi.missingOutOfBounds()) continue;

      if (missing(con.vi, vt)) {
        // value is still missingg, nothing to do (we incremented its count above)
        continue;
      }

      /*@Interned*/ Object val = con.vi.getValue(vt);
      // the variable is not missing, so it is non-null
      assert val != null;

      i.remove();
      con.always_missing = false;
      if (Debug.logDetail()) {
        Debug.log(
            getClass(),
            ppt,
            Debug.vis(con.vi),
            "Adding "
                + Debug.toString(val)
                + " to missing : missing = "
                + missing(con.vi, vt)
                + ": samples = "
                + con.count
                + "/"
                + count
                + "/"
                + sample_cnt);
      }
      // Is the Constant missing because it was initialized that way, or
      // has the program point seen values in the past?
      if (sample_cnt == 0) {
        // First sample for this program point (& this variable)
        con.val = val;
        con.count = count;
        con.constant = true;
        con_list.add(con);
      } else {
        // This variable truly is missing; has seen a missing value in the past.
        non_missing.add(con);
        con.previous_missing = true;
      }
    }

    sample_cnt += count;

    // Create slices over newly non-constant and non-missing variables
    instantiate_new_views(non_con, non_missing);

    // Turn off previous_constant on all newly non-constants
    for (Constant con : non_con) {
      con.previous_constant = false;
      @SuppressWarnings("nullness") // reinitialization
      /*@NonNull*/ Object nullValue = null;
      con.val = nullValue;
      con.count = 0;
      con.checkRep();
    }

    // Turn off previous_missing on all newly non-missing
    for (Constant con : non_missing) {
      con.previous_missing = false;
      con.checkRep();
    }
  }

  /** Returns whether the specified variable is missing in this ValueTuple. */
  private boolean missing(VarInfo vi, ValueTuple vt) {

    int mod = vt.getModified(vi);
    return ((mod == ValueTuple.MISSING_FLOW) || (mod == ValueTuple.MISSING_NONSENSICAL));
  }

  /** Returns the Constant for the specified variable. */
  /*@Pure*/
  public Constant getConstant(VarInfo vi) {

    Constant result = all_vars[vi.varinfo_index];
    result.checkRep();
    return result;
  }

  /** Returns whether the specified variable is currently a constant. */
  /*@Pure*/
  public boolean is_constant(VarInfo vi) {

    return getConstant(vi).constant;
  }

  /**
   * Returns whether the specified variable is currently a constant OR was a constant at the
   * beginning of constants processing.
   */
  /*@Pure*/
  public boolean is_prev_constant(VarInfo vi) {

    return getConstant(vi).is_prev_constant();
  }

  /**
   * Returns the constant value of the specified variable, or null if the variable is not constant
   * or prev_constant. But, it is apparently only called on constants with a value.
   */
  public /*@Interned*/ Object constant_value(VarInfo vi) {

    @SuppressWarnings("nullness") // non-missing value, so non-null val field
    /*@NonNull*/ Object result = getConstant(vi).val;
    return result;
  }

  /** Returns whether the specified variable missing for all values so far. */
  /*@Pure*/
  public boolean is_missing(VarInfo vi) {

    return (getConstant(vi).always_missing);
  }

  /**
   * Returns whether the specified variable is currently missing OR was missing at the beginning of
   * constants processing.
   */
  /*@Pure*/
  public boolean is_prev_missing(VarInfo vi) {

    Constant c = all_vars[vi.varinfo_index];
    return (c.always_missing || c.previous_missing);
  }

  /** Returns the number of constants that are leaders. */
  public int constant_leader_cnt() {

    int con_cnt = 0;
    for (Constant con : con_list) {
      if (con.vi.isCanonical()) con_cnt++;
    }

    return con_cnt;
  }

  /**
   * Creates all new views required for the newly non constants (noncons) and the newly non-missing
   * (non_missing).
   */
  public void instantiate_new_views(List<Constant> noncons, List<Constant> non_missing) {

    if (Debug.logOn()) {
      for (Constant con : noncons) {
        Debug.log(
            getClass(),
            ppt,
            Debug.vis(con.vi),
            "is non constant"
                + " with val = "
                + Debug.toString(con.val)
                + " with count = "
                + con.count);
      }
      for (Constant con : non_missing) {
        Debug.log(getClass(), ppt, Debug.vis(con.vi), "is non missing");
      }
    }

    for (Constant con : noncons) {
      con.checkRep();
    }
    for (Constant con : non_missing) {
      con.checkRep();
    }

    // Create all of the views over noncons and noncons+con_list.
    // Since everything starts out as a constant, it is only necessary
    // to combine the newly non-constants with a combination of
    // the remaining constants and the newly-non constants.  Any slices
    // between the non-constants and other variables will have already
    // been created when those other variables became non-constants.
    if (noncons.size() > 0) {
      List<Constant> cons = new ArrayList<Constant>();
      cons.addAll(con_list);
      cons.addAll(noncons);
      debug.fine("Instantiating non constants in ppt: " + ppt.name());
      instantiate_views(noncons, cons);
    }

    // Create all views over the newly non-missing.  Since missing
    // vars were not included in any previous views, we must match them
    // against all variables.
    if (non_missing.size() > 0) {
      debug.fine("Instantiating non missing in ppt: " + ppt.name());
      instantiate_views(non_missing, all_list);
    }

    // Create any ternary invariants that are suppressed when one
    // of the variables is a constant.  Currently, only LinearTernary
    // falls into this list (It is suppressed by (x = C) && (Ay + Bz = D))
    if (NIS.dkconfig_enabled) {
      instantiate_constant_suppressions(noncons, all_list);
    }
  }

  /**
   * Instantiate views and invariants across each combination of vars from list1 and list2. If each
   * item in a new slice was a constant, the constant values are applied.
   *
   * <p>The following slices will be created:
   *
   * <pre>
   *    unary:   list1-vars
   *    binary:  list1-vars X list2-vars
   *    ternary: list1-vars X list2-vars X list2-vars
   * </pre>
   */
  private void instantiate_views(List<Constant> list1, List<Constant> list2) {

    // Get list1 leaders
    Set<Constant> leaders1 = new LinkedHashSet<Constant>();
    for (Constant con : list1) {
      if (con.vi.isCanonical()) leaders1.add(con);
    }

    // Get list2 leaders
    Set<Constant> leaders2 = new LinkedHashSet<Constant>();
    for (Constant con : list2) {
      if (con.vi.isCanonical()) leaders2.add(con);
    }

    if (debug.isLoggable(Level.FINE)) {
      debug.fine("instantiating over " + leaders1.size() + " leaders1: " + leaders1);
      debug.fine("instantiating over " + leaders2.size() + " leaders2: " + leaders2);
    }

    // any new views created
    List<PptSlice> new_views = new ArrayList<PptSlice>();

    int mod = ValueTuple.MODIFIED;

    // Unary slices/invariants
    for (Constant con : leaders1) {
      if (Debug.logOn()) Debug.log(getClass(), ppt, Debug.vis(con.vi), "Considering slice");
      if (!ppt.is_slice_ok(con.vi)) continue;
      PptSlice1 slice1 = new PptSlice1(ppt, con.vi);
      slice1.instantiate_invariants();
      if (Debug.logOn()) Debug.log(getClass(), ppt, Debug.vis(con.vi), "Instantiated invs");
      if (con.count > 0) {
        assert con.val != null : "@AssumeAssertion(nullness): dependent: val != null when count>0";
        slice1.add_val_bu(con.val, mod, con.count);
      }
      new_views.add(slice1);
    }

    // Binary slices/invariants.
    for (Constant con1 : leaders1) {
      for (Constant con2 : leaders2) {
        Constant c1 = con1;
        Constant c2 = con2;
        Debug.log(getClass(), ppt, Debug.vis(c1.vi, c2.vi), "Considering slice");
        if (con2.vi.varinfo_index < con1.vi.varinfo_index) {
          if (leaders1.contains(con2)) {
            // The variable is in both leader lists.
            // Don't add it on this iteration; add it when the variables
            // are given in order (to prevent creating the slice twice).
            continue;
          }
          c1 = con2;
          c2 = con1;
        }
        if (!ppt.is_slice_ok(c1.vi, c2.vi)) {
          if (Debug.logOn()) {
            Debug.log(
                debug,
                getClass(),
                ppt,
                Debug.vis(c1.vi, c2.vi),
                "Not instantiating slice " + c1.vi.equalitySet.size());
          }
          continue;
        }
        PptSlice2 slice2 = new PptSlice2(ppt, c1.vi, c2.vi);
        Debug.log(
            getClass(),
            ppt,
            Debug.vis(c1.vi, c2.vi),
            String.format("instantiating slice %s [%s %s]%n", slice2, c1, c2));
        slice2.instantiate_invariants();
        if (c1.count > 0 && c2.count > 0) {
          assert c1.val != null : "@AssumeAssertion(nullness): dependent: val != null when count>0";
          assert c2.val != null : "@AssumeAssertion(nullness): dependent: val != null when count>0";
          slice2.add_val_bu(c1.val, c2.val, mod, mod, con1.count);
        }
        new_views.add(slice2);
      }
    }

    // Ternary slices/invariants.  Note that if a variable is in both
    // leader lists, it is only added when it is in order (to prevent
    // creating the slice twice).
    for (Constant con1 : leaders1) {
      for (Constant con2 : leaders2) {
        if ((con2.vi.varinfo_index < con1.vi.varinfo_index) && leaders1.contains(con2)) continue;
        for (Constant con3 : leaders2) {
          if ((con3.vi.varinfo_index < con2.vi.varinfo_index)
              || ((con3.vi.varinfo_index < con1.vi.varinfo_index) && leaders1.contains(con3)))
            continue;
          Constant[] con_arr = {con1, con2, con3};
          Arrays.sort(con_arr, ConIndexComparator.getInstance());
          assert (con_arr[0].vi.varinfo_index <= con_arr[1].vi.varinfo_index)
              && (con_arr[1].vi.varinfo_index <= con_arr[2].vi.varinfo_index);
          if (!ppt.is_slice_ok(con_arr[0].vi, con_arr[1].vi, con_arr[2].vi)) continue;

          PptSlice3 slice3 = new PptSlice3(ppt, con_arr[0].vi, con_arr[1].vi, con_arr[2].vi);
          slice3.instantiate_invariants();
          if ((con_arr[0].count > 0) && (con_arr[1].count > 0) && (con_arr[2].count > 0)) {
            assert con_arr[0].val != null
                : "@AssumeAssertion(nullness): dependent: val != null when count>0";
            assert con_arr[1].val != null
                : "@AssumeAssertion(nullness): dependent: val != null when count>0";
            assert con_arr[2].val != null
                : "@AssumeAssertion(nullness): dependent: val != null when count>0";
            slice3.add_val_bu(
                con_arr[0].val, con_arr[1].val, con_arr[2].val, mod, mod, mod, con_arr[0].count);
          }
          new_views.add(slice3);
        }
      }
    }

    // Debug print the created slies
    if (Debug.logOn() || debug.isLoggable(Level.FINE)) {
      int[] slice_cnt = {0, 0, 0, 0};
      int[] inv_cnt = {0, 0, 0, 0};
      int[] true_inv_cnt = {0, 0, 0, 0};
      for (PptSlice slice : new_views) {
        for (Invariant inv : slice.invs) {
          inv.log("created, falsified = %b", inv.is_false());
          if (!inv.is_false()) {
            true_inv_cnt[slice.arity()]++;
          } else {
            String vals = "";
            for (VarInfo vi : slice.var_infos) {
              vals += vi.name() + "=" + Debug.toString(constant_value(vi)) + " ";
            }
            inv.log("Invariant %s destroyed by constant values %s", inv.format(), vals);
          }
        }
        if (slice.invs.size() > 0) {
          slice_cnt[slice.arity()]++;
        }
        inv_cnt[slice.arity()] += slice.invs.size();
        if (Debug.logDetail()) {
          StringBuffer sb = new StringBuffer();
          for (int j = 0; j < slice.arity(); j++) {
            VarInfo v = slice.var_infos[j];
            sb.append(v.name() + " [" + v.file_rep_type + "] [" + v.comparability + "] ");
          }
          Debug.log(
              debug,
              getClass(),
              ppt,
              slice.var_infos,
              "Adding slice over " + sb + ": with " + slice.invs.size() + " invariants");
        }
      }
      for (int i = 1; i <= 3; i++) {
        debug.fine(
            "Added "
                + slice_cnt[i]
                + " slice"
                + i
                + "s with "
                + true_inv_cnt[i]
                + " invariants ("
                + inv_cnt[i]
                + " total)");
      }

      String leader1_str = "";
      int leader1_cnt = 0;
      for (Constant con1 : leaders1) {
        if (con1.vi.file_rep_type == ProglangType.INT) {
          leader1_str += con1.vi.name() + " ";
          leader1_cnt++;
        }
      }

      String leader2_str = "";
      int leader2_cnt = 0;
      for (Constant con1 : leaders2) {
        if (con1.vi.file_rep_type == ProglangType.INT) {
          leader2_str += con1.vi.name() + " ";
          leader2_cnt++;
        }
      }
      debug.fine(
          leader1_cnt
              + " leader1 ints ("
              + leader1_str
              + "): "
              + leader2_cnt
              + " leader2 ints ("
              + leader2_str);
    }

    // Remove any falsified invariants from the new views.  Don't
    // call remove_falsified() since that has side-effects (such as
    // NIS processing on the falsified invariants) that we don't want.
    for (PptSlice slice : new_views) {
      List<Invariant> to_remove = new ArrayList<Invariant>();
      for (Invariant inv : slice.invs) {
        if (inv.is_false()) {
          to_remove.add(inv);
        }
      }
      slice.removeInvariants(to_remove);
    }

    // Add the new slices to the top level ppt.  This will discard any
    // slices that ended up with zero invariants
    ppt.addViews(new_views);
  }

  public void instantiate_constant_suppressions(List<Constant> new_noncons, List<Constant> all) {

    // Find all of the variable (non-constant) non-missing
    // integral/float leaders
    List<Constant> vars = new ArrayList<Constant>();
    for (Constant con : all) {
      if (con.always_missing || con.previous_missing) continue;
      if (con.constant || con.previous_constant) continue;
      if (!con.vi.isCanonical()) continue;
      if (!con.vi.file_rep_type.isIntegral() && !con.vi.file_rep_type.isFloat()) continue;
      if (con.vi.rep_type.isArray()) continue;
      vars.add(con);
    }

    // Find all of the new non-constant integer/float leaders
    List<Constant> new_leaders = new ArrayList<Constant>();
    for (Constant con : new_noncons) {
      if (!con.vi.isCanonical()) continue;
      if (!con.vi.file_rep_type.isIntegral() && !con.vi.file_rep_type.isFloat()) continue;
      if (con.vi.rep_type.isArray()) continue;
      new_leaders.add(con);
    }

    if (debug.isLoggable(Level.FINE)) {
      debug.fine("new non-con leaders = " + new_leaders);
      debug.fine("variable leaders = " + vars);
    }

    // Consider all of the ternary slices with one new non-constant
    for (int i = 0; i < new_leaders.size(); i++) {
      Constant con1 = new_leaders.get(i);
      assert con1.val != null : "@AssumeAssertion(nullness)";
      for (int j = 0; j < vars.size(); j++) {
        Constant con2 = vars.get(j);
        assert con1 != con2;
        for (int k = j; k < vars.size(); k++) {
          Constant con3 = vars.get(k);
          assert con1 != con3;
          if (!ppt.is_slice_ok(con1.vi, con2.vi, con3.vi)) continue;

          if (debug.isLoggable(Level.FINE)) {
            debug.fine(String.format("considering slice %s %s %s", con1, con2, con3));
          }

          // Look for a linearbinary over two variables.  If it doesn't
          // exist we don't create a LinearTernary
          Invariant lb = find_linear_binary(ppt.findSlice(con2.vi, con3.vi));
          if (lb == null) continue;

          // Find the ternary slice and create it if it is not there
          PptSlice slice = ppt.get_or_instantiate_slice(con1.vi, con2.vi, con3.vi);

          // Create the LinearTernary invariant from the LinearBinary
          // invariant and the constant value
          Invariant lt = null;
          if (con1.vi.file_rep_type.isIntegral()) {
            lt = LinearTernary.get_proto().instantiate(slice);
            if (lt != null) {
              ((LinearTernary) lt).setup((LinearBinary) lb, con1.vi, ((Long) con1.val).longValue());
            }
          } else /* must be float */ {
            lt = LinearTernaryFloat.get_proto().instantiate(slice);
            if (lt != null) {
              ((LinearTernaryFloat) lt)
                  .setup((LinearBinaryFloat) lb, con1.vi, ((Double) con1.val).doubleValue());
            }
          }
          if (lt != null) {
            if (Debug.dkconfig_internal_check) {
              assert slice.find_inv_by_class(lt.getClass()) == null
                  : "inv = " + lt.format() + " slice = " + slice;
            }
            slice.addInvariant(lt);
            if (debug.isLoggable(Level.FINE)) {
              debug.fine("Adding invariant " + lt.format() + " to slice " + slice);
            }
          }
        }
      }
    }

    // Consider all of the ternary slices with two new non-constants
    for (int i = 0; i < new_leaders.size(); i++) {
      Constant con1 = new_leaders.get(i);
      assert con1.val != null : "@AssumeAssertion(nullness)";
      for (int j = i; j < new_leaders.size(); j++) {
        Constant con2 = new_leaders.get(j);
        for (int k = 0; k < vars.size(); k++) {
          Constant con3 = vars.get(k);
          assert con2 != con3;
          assert con1 != con3;
          if (!ppt.is_slice_ok(con1.vi, con2.vi, con3.vi)) continue;

          if (debug.isLoggable(Level.FINE)) {
            debug.fine(String.format("considering slice %s %s %s", con1, con2, con3));
          }

          // Create the ternary slice

          // Create the LinearTernary invariant from the OneOf invariant
          // (if any) and the constant values.  If no OneOf exists,
          // there can be no interesting plane of the points
          Invariant lt = null;
          PptSlice slice = null;
          InvariantStatus sts = InvariantStatus.NO_CHANGE;
          if (con1.vi.file_rep_type.isIntegral()) {
            OneOfScalar oo =
                (OneOfScalar) ppt.find_inv_by_class(new VarInfo[] {con3.vi}, OneOfScalar.class);
            if (oo == null) continue;
            slice = ppt.get_or_instantiate_slice(con1.vi, con2.vi, con3.vi);

            lt = LinearTernary.get_proto().instantiate(slice);
            if (lt != null) {
              assert con2.val != null : "@AssumeAssertion(nullness)";
              sts =
                  ((LinearTernary) lt)
                      .setup(
                          oo,
                          con1.vi,
                          ((Long) con1.val).longValue(),
                          con2.vi,
                          ((Long) con2.val).longValue());
            }
          } else /* must be float */ {
            OneOfFloat oo =
                (OneOfFloat) ppt.find_inv_by_class(new VarInfo[] {con3.vi}, OneOfFloat.class);
            if (oo == null) continue;
            slice = ppt.get_or_instantiate_slice(con1.vi, con2.vi, con3.vi);
            lt = LinearTernaryFloat.get_proto().instantiate(slice);
            if (lt != null) {
              assert con2.val != null : "@AssumeAssertion(nullness)";
              sts =
                  ((LinearTernaryFloat) lt)
                      .setup(
                          oo,
                          con1.vi,
                          ((Double) con1.val).doubleValue(),
                          con2.vi,
                          ((Double) con2.val).doubleValue());
            }
          }
          if ((lt != null) && (sts == InvariantStatus.NO_CHANGE)) {
            if (Debug.dkconfig_internal_check) {
              assert slice.find_inv_by_class(lt.getClass()) == null
                  : "inv = " + lt.format() + " slice = " + slice;
            }
            slice.addInvariant(lt);
            debug.fine("Adding invariant " + lt.format() + " to slice " + slice);
          }
        }
      }
    }
  }

  /**
   * Looks for a LinearBinary invariant in the specified slice. Will match either float or integer
   * versions.
   */
  private /*@Nullable*/ Invariant find_linear_binary(/*@Nullable*/ PptSlice slice) {

    // if (debug.isLoggable (Level.FINE))
    //  debug.fine ("considering slice " + slice);

    if (slice == null) {
      return null;
    }

    for (Invariant inv : slice.invs) {
      // debug.fine ("inv = " + inv.getClass());
      if ((inv.getClass() == LinearBinary.class) || (inv.getClass() == LinearBinaryFloat.class)) {
        return inv;
      }
    }

    return null;
  }

  /**
   * Create invariants for any remaining constants. Right now, this looks for invariants between all
   * of the constants. It's not clear that invariants between constants are interesting, but to
   * match previous behavior, this is what we will do for now.
   */
  public void post_process() {

    // if requested, don't create any post-processed invariants
    if (no_post_process) {
      int con_count = 0;
      for (Constant con : con_list) {
        if (!con.vi.isCanonical()) continue;
        System.out.println(
            "  Not creating invariants over leader " + con.vi.name() + " = " + con.val);
        con_count++;
      }
      System.out.println(con_count + " constants at ppt " + ppt);
      return;
    }

    // If specified, create only OneOf invariants.  Also create a reflexive
    // equality invariant, since that is assumed to exist in many places.
    if (dkconfig_OneOf_only) {
      for (Constant con : con_list) {
        if (!con.vi.isCanonical()) continue;
        instantiate_oneof(con);
        ppt.create_equality_inv(con.vi, con.vi, con.count);
      }
      return;
    }

    // Get a list of all remaining constants and clear the existing list
    // (if the existing list is not cleared, constant slices will not
    // be created).
    List<Constant> noncons = con_list;
    for (Constant con : con_list) {
      con.constant = false;
      con.previous_constant = true;
    }
    con_list = new ArrayList<Constant>();

    // Don't do anything with variables that have always been missing.  They
    // should have no invariants over them.
    List<Constant> non_missing = new ArrayList<Constant>();

    instantiate_new_views(noncons, non_missing);

    /* Code to just create just unary slices for constants
      for (Constant con : con_list) {
        if (!con.vi.isCanonical()) {
          continue;
          }
        PptSlice1 slice1 = new PptSlice1 (ppt, con.vi);
        slice1.instantiate_invariants();
        if (con.val != null) {
          slice1.add_val (con.val, ValueTuple.MODIFIED, con.count);
          }
        new_views.add (slice1);
      }
      ppt.addViews (new_views);
    */
  }

  /**
   * Create unary and binary constant invariants. The slices and invariants are created and
   * returned, but not added to the ppt. Note that when NIS.dkconfig_suppressor_list is turned on
   * (default is on), only unary and binary invariants that can be suppressors in NIS suppressions
   * are created.
   */
  /*@RequiresNonNull("NIS.suppressor_proto_invs")*/
  public List<PptSlice> create_constant_invs() {

    // Turn off track logging so that we don't get voluminous messages
    // each time this is called
    boolean debug_on = Logger.getLogger("daikon.Debug").isLoggable(Level.FINE);
    if (debug_on) LogHelper.setLevel("daikon.Debug", Level.OFF);

    // Get constant leaders
    List<Constant> leaders = new ArrayList<Constant>(100);
    for (Constant con : con_list) {
      if (!con.vi.isCanonical()) continue;

      // hashcode types are not involved in suppressions
      if (NIS.dkconfig_skip_hashcode_type) {
        if (con.vi.file_rep_type.isHashcode()) {
          continue;
        }
      }

      leaders.add(con);
    }

    List<PptSlice> new_views = new ArrayList<PptSlice>(100);
    int mod = ValueTuple.MODIFIED;

    // Unary slices/invariants
    for (Constant con : leaders) {

      PptSlice1 slice1 = new PptSlice1(ppt, con.vi);

      if (NIS.dkconfig_suppressor_list) {
        slice1.instantiate_invariants(NIS.suppressor_proto_invs);
      } else {
        slice1.instantiate_invariants();
      }

      if (con.count > 0) {
        assert con.val != null : "@AssumeAssertion(nullness): dependent: val when count>0";
        slice1.add_val_bu(con.val, mod, con.count);
      }
      if (slice1.invs.size() > 0) new_views.add(slice1);
    }

    // Binary slices/invariants
    for (int i = 0; i < leaders.size(); i++) {
      Constant con1 = leaders.get(i);
      for (int j = i; j < leaders.size(); j++) {
        Constant con2 = leaders.get(j);
        if (!con1.vi.compatible(con2.vi)) continue;

        PptSlice2 slice2 = new PptSlice2(ppt, con1.vi, con2.vi);
        if (NIS.dkconfig_suppressor_list) {
          slice2.instantiate_invariants(NIS.suppressor_proto_invs);
        } else {
          slice2.instantiate_invariants();
        }

        if (con1.count > 0 && con2.count > 0) {
          assert con1.val != null
              : "@AssumeAssertion(nullness): dependent: val != null when count>0";
          assert con2.val != null
              : "@AssumeAssertion(nullness): dependent: val != null when count>0";
          slice2.add_val_bu(con1.val, con2.val, mod, mod, con1.count);
        }
        if (slice2.invs.size() > 0) new_views.add(slice2);
      }
    }

    // Remove any falsified invariants from the new views.
    for (PptSlice slice : new_views) {
      for (Iterator<Invariant> j = slice.invs.iterator(); j.hasNext(); ) {
        Invariant inv = j.next();
        if (inv.is_false()) {
          j.remove();
        }
      }
    }

    if (debug_on) LogHelper.setLevel("daikon.Debug", Level.FINE);

    return new_views;
  }

  public void print_missing(PrintWriter out) {

    for (Constant con : missing_list) {
      out.println(con.vi.name() + " is always missing");
    }
  }

  /**
   * Merge dynamic constants from the children of this ppt. Only missing is merged since constants
   * are not used after we are done processing samples.
   */
  public void merge() {

    // clear the constant and missing lists
    missing_list.clear();
    con_list.clear();

    // Process each variable at this ppt.  If the variable is missing at
    // each of the children, it is also missing here.  Ignore children that
    // have no mapping for this variable
    for (VarInfo pvar : ppt.var_infos) {
      boolean missing = true;
      for (PptRelation rel : ppt.children) {
        VarInfo cvar = rel.childVar(pvar);
        if ((cvar != null)
            && (rel.child.constants != null)
            && !rel.child.constants.is_missing(cvar)) {
          missing = false;
          break;
        }
      }
      Constant c = all_vars[pvar.varinfo_index];
      c.checkRep();
      c.always_missing = missing;
      c.checkRep();
      if (missing) missing_list.add(c);
    }
  }

  /** Creates OneOf invariants for each constant. */
  public void instantiate_oneof(Constant con) {
    assert con.val != null : "@AssumeAssertion(nullness)";

    // @NonNull, but not marked that way to ease warning suppression.
    Invariant inv;
    PptSlice1 slice1 = (PptSlice1) ppt.get_or_instantiate_slice(con.vi);

    // Create the correct OneOf invariant
    ProglangType rep_type = con.vi.rep_type;
    boolean is_scalar = rep_type.isScalar();
    if (is_scalar) {
      if (!OneOfScalar.dkconfig_enabled) {
        return;
      }
      inv = OneOfScalar.get_proto().instantiate(slice1);
    } else if (rep_type == ProglangType.INT_ARRAY) {
      if (!OneOfSequence.dkconfig_enabled) {
        return;
      }
      inv = OneOfSequence.get_proto().instantiate(slice1);
    } else if (rep_type == ProglangType.DOUBLE) {
      if (!Daikon.dkconfig_enable_floats || !OneOfFloat.dkconfig_enabled) {
        return;
      }
      inv = OneOfFloat.get_proto().instantiate(slice1);
    } else if (rep_type == ProglangType.DOUBLE_ARRAY) {
      if (!Daikon.dkconfig_enable_floats || !OneOfFloatSequence.dkconfig_enabled) {
        return;
      }
      inv = OneOfFloatSequence.get_proto().instantiate(slice1);
    } else if (rep_type == ProglangType.STRING) {
      if (!OneOfString.dkconfig_enabled) {
        return;
      }
      inv = OneOfString.get_proto().instantiate(slice1);
    } else if (rep_type == ProglangType.STRING_ARRAY) {
      if (!OneOfStringSequence.dkconfig_enabled) {
        return;
      }
      inv = OneOfStringSequence.get_proto().instantiate(slice1);
    } else {
      throw new Error("Unrecognized rep_type in instantiate_oneof");
    }
    assert inv != null
        : "@AssumeAssertion(nullness): instantiation of the given invariants always succeeds";
    slice1.addInvariant(inv);

    // Add the value to it
    slice1.add_val_bu(con.val, ValueTuple.MODIFIED, con.count);
  }
}
