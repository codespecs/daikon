package daikon.suppress;

import daikon.*;
import daikon.inv.*;
import daikon.inv.binary.*;
import daikon.inv.ternary.threeScalar.*;
import utilMDE.*;

import java.lang.reflect.*;
import java.util.logging.*;
import java.util.*;

// Outstanding NIS todo list
//
//  - Merging is slow when there are multiple children.
//
//  - Move the missingOutOfBounds check to is_slice_ok()
//
// Work to be done when all NI suppressions are not unary/binary -> ternary
//
//  - The apply_samples() routines will have to
//    be applied iteratively when chains of NIS are supported.
//

/**
 * Main class for non-instantiating suppression.  Handles setup and other
 * overall functions.
 */
public class NIS {

  /** Debug tracer. **/
  public static final Logger debug = Logger.getLogger ("daikon.suppress.NIS");

  /** Debug Tracer for antecedent method **/
  public static final Logger debugAnt = Logger.getLogger
                                                ("daikon.suppress.NIS.Ant");

  /**
   * Boolean.  If true, enabled non-instantiating supressions
   */
  public static boolean dkconfig_enabled = true;

  /** Boolean. If true, use antecedent method for creating unsuppressed invs*/
  public static boolean dkconfig_antecedent_method = false;

  /** Possible states for suppressors and suppressions. **/
  static final String NONE = "none";
  static final String MATCH = "match";
  static final String VALID = "valid";
  static final String INVALID = "invalid";
  static final String MISSING = "missing";

  /**
   * Map from invariant class to a list of all of the suppression sets
   * that contain a suppressor of that class.
   */
  static Map/*NISuppressor.class -> List<NISuppressionSet>*/
    suppressor_map = new LinkedHashMap(256);

  /** List of all suppressions */
  static List/*NISuppressionSet*/ all_suppressions = new ArrayList();

  /**
   * List of invariants that are newly created.  This list is cleared
   * by apply_samples()
   */
  public static List new_invs = new ArrayList();

  static boolean keep_stats = false;
  static int false_invs = 0;
  static int suppressions_processed = 0;
  static int new_invs_cnt = 0;
  static int false_invs_cnt = 0;
  static int created_invs_cnt = 0;
  static int still_suppressed_cnt = 0;
  static int possibly_unsuppressed_cnt = 0;
  static Stopwatch watch = new Stopwatch (false);

  static ValueTuple vt;

  /**
   * Sets up non-instantiation suppression.  Primarily this includes setting
   * up the map from suppressor classes to all of the suppression sets
   * associated with that suppressor invariant
   */
  public static void init_ni_suppression() {

    if (!dkconfig_enabled)
      return;

    // Get all of the ternary non-instantiating suppressions.  Other's
    // will be added later
    all_suppressions = FunctionBinary.get_all_ni_suppressions();
    all_suppressions.addAll (FunctionBinaryFloat.get_all_ni_suppressions());

    // map suppressor classes to suppression sets
    for (Iterator i = all_suppressions.iterator(); i.hasNext(); ) {
      NISuppressionSet suppression_set = (NISuppressionSet) i.next();
      suppression_set.add_to_suppressor_map (suppressor_map);
    }

    if (Debug.logDetail() && debug.isLoggable (Level.FINE))
      dump (debug);
  }

  /**
   * Instantiates any invariants that are no longer suppressed because
   * inv has been falsified.
   */
  public static void falsified (Invariant inv) {

    if (!dkconfig_enabled || dkconfig_antecedent_method)
      return;

    // if (debug.isLoggable (Level.FINE))
    //  debug.fine ("inv " + inv.format() + " falsified");

    // Get the suppressor sets (if any) associated with this invariant
    List ss_list = (List) suppressor_map.get(inv.getClass());
    if (ss_list == null) {
      return;
    }

    // Keep track of falsified invariants that are antecedents
    if (keep_stats) {
      watch.start();
      false_invs++;
    }

    // Process each suppression set
    for (Iterator i = ss_list.iterator(); i.hasNext(); ) {
      NISuppressionSet ss = (NISuppressionSet) i.next();
      ss.clear_state();
      if (debug.isLoggable (Level.FINE))
        debug.fine ("processing suppression set " + ss + " over falsified inv "
                    + inv.format());
      ss.falsified (inv, new_invs);
      suppressions_processed += ss.suppression_set.length;
    }

    if (keep_stats)
      watch.stop();
  }

  /**
   * Applies sample values to all of the newly created
   * invariants.  If the invariant is not falsified, it is added
   * to the slice.
   *
   * Clears the new_invs list after processing.  Currently this
   * routine checks to insure that the newly falsified invariant
   * is not itself a possible NI suppressor.
   */
  public static void apply_samples (ValueTuple vt, int count) {

    if (NIS.debug.isLoggable (Level.FINE))
      NIS.debug.fine ("Applying samples to " + new_invs.size()
                       + " new invariants");
    // new_invs_cnt = new_invs.size();

    // Loop through each invariant
    inv_loop: for (Iterator i = new_invs.iterator(); i.hasNext(); ) {
      Invariant inv = (Invariant) i.next();
      if (inv.is_false())
        Assert.assertTrue (!inv.is_false(), Fmt.spf ("inv %s in ppt %s is "
            + " false before sample is applied ", inv.format(), inv.ppt));

      // Looks to see if any variables are missing.  This can happen
      // when a variable not involved in the suppressor is missing on
      // this sample.
      boolean missing = false;
      for (int j = 0; j < inv.ppt.var_infos.length; j++) {
        if (inv.ppt.var_infos[j].isMissing(vt)) {
          missing = true;
          break;
        }
      }

      InvariantStatus result = InvariantStatus.NO_CHANGE;

      // Apply the sample
      if (!missing) {
        result = inv.add_sample (vt, count);
        Assert.assertTrue (result != InvariantStatus.FALSIFIED);
        if (Debug.logOn())
          inv.log ("after applying sample to " + inv.format() +
                   " result = " + result);
      }

      // Add the invariant to its slice if it was not falsified by this sample
      if (result == InvariantStatus.FALSIFIED) {
        if (Daikon.dkconfig_internal_check) {
          List ss_list = (List) suppressor_map.get(inv.getClass());
          Assert.assertTrue (ss_list == null);
        }
        // false_invs_cnt++;
      } else {
        if (Daikon.dkconfig_internal_check)
          Assert.assertTrue (inv.ppt.parent.findSlice(inv.ppt.var_infos)
                              == inv.ppt);
        inv.ppt.addInvariant (inv);
        if (Debug.logOn())
          inv.log (inv.format() + " added to slice");
        created_invs_cnt++;
      }
    }

    NIS.vt = null;
    new_invs.clear();
  }

  /**
   * Clears the current NIS statistics and enables the keeping of statistics
   */
  public static void clear_stats() {

    keep_stats = true;
    watch.clear();
    false_invs = 0;
    suppressions_processed = 0;
    new_invs_cnt = 0;
    false_invs_cnt = 0;
    created_invs_cnt = 0;
    still_suppressed_cnt = 0;
    possibly_unsuppressed_cnt = 0;
  }

  /**
   * dump statistics on NIS to the specified logger
   */
  public static void dump_stats (Logger log, PptTopLevel ppt) {

    if (false_invs > 0) {
      log.fine (false_invs + " : "
                  + suppressions_processed + " : "
                  + new_invs_cnt + " : "
                  + false_invs_cnt + " : "
                  + created_invs_cnt + " : "
                  + still_suppressed_cnt + " : "
                  + possibly_unsuppressed_cnt + " : "
                  + watch.elapsedMillis() + " msecs : "
                  + ppt.name);
    }
  }

  /**
   * Creates any invariants that were previously suppressed, but are no
   * longer suppressed based after falsified suppressors are removed.
   */
  public static void process_falsified_invs (PptTopLevel ppt) {

    if (!dkconfig_enabled || !dkconfig_antecedent_method)
      return;

    // If there are no falsified invariants, there is nothing to do
    int false_cnt = 0;
    for (Iterator i = ppt.views_iterator(); i.hasNext(); ) {
      PptSlice slice = (PptSlice) i.next();
      for (Iterator j = slice.invs.iterator(); j.hasNext(); ) {
        Invariant inv = (Invariant) j.next();
        if (inv.is_false())
          false_cnt++;
      }
    }
    if (false_cnt == 0)
      return;

    watch.start();

    // find all of the possible antecedent invariants for each class.
    // This needs to include both existing invariants and any invariants
    // over constants.
    Map /*Invariant.class->List<Invariant> */ antecedent_map
      = new LinkedHashMap();
    find_antecedents (ppt.views_iterator(), antecedent_map);
    if (debugAnt.isLoggable (Level.FINE))
      debugAnt.fine ("Antecedent Map at " + ppt.name + ", false cnt = "
                  + false_cnt + " : " + toString (antecedent_map));
    //PptTopLevel.debugNISStats.fine ("active invariants: "
    //                                + map_size_string (antecedent_map));
    false_invs += false_cnt;
    find_antecedents (ppt.constants.create_constant_invs().iterator(),
                     antecedent_map);
    if (debugAnt.isLoggable (Level.FINE))
      debugAnt.fine ("Antecedent Map at " + ppt.name + " : "
                   + toString (antecedent_map));
    //PptTopLevel.debugNISStats.fine ("constant invariants: "
    //                                 + map_size_string (antecedent_map));


    // Sets to contain invariants that are still suppressed and those that
    // are no longer suppressed
    Set suppressed_invs = new LinkedHashSet();
    Set unsuppressed_invs = new LinkedHashSet();

    // Loop through each suppression creating each invariant that
    // is suppressed by that suppression.
    for (Iterator i = all_suppressions.iterator(); i.hasNext(); ) {
      NISuppressionSet ss = (NISuppressionSet) i.next();
      for (Iterator j = ss.iterator(); j.hasNext(); ) {
        NISuppression sup = (NISuppression) j.next();
        suppressions_processed++;
        sup.find_suppressed_invs (suppressed_invs, unsuppressed_invs,
                                    antecedent_map);
      }
    }

    still_suppressed_cnt = suppressed_invs.size();
    possibly_unsuppressed_cnt = unsuppressed_invs.size();

    // Create each new unsuppressed invariant that is not still suppressed
    // by a different suppression.  Skip any that will be falsified by
    // the sample.
    unsuppressed_invs.removeAll (suppressed_invs);
    for (Iterator i = unsuppressed_invs.iterator(); i.hasNext(); ) {
      SupInv supinv = (SupInv) i.next();
      new_invs_cnt++;
      if (supinv.check (NIS.vt) == InvariantStatus.FALSIFIED) {
        false_invs_cnt++;
        continue;
      }
      Invariant inv = supinv.instantiate (ppt);
      if (Daikon.dkconfig_internal_check) {
        if (inv.ppt.find_inv_exact (inv) != null)
          Assert.assertTrue (false, "inv " + inv.format()
                             + " already exists in ppt " + ppt.name);
      }
      // inv.ppt.addInvariant (inv);
      new_invs.add (inv);
    }

    watch.stop();
  }

  /**
   * Creates all suppressed invariants for the specified ppt and
   * places them in their associated slices.  @return a list of
   * created invariants.
   */
  public static List/*Invariant*/ create_suppressed_invs (PptTopLevel ppt) {

    // List of created invariants
    List suppressed_invs = new ArrayList();

    // First find all of the possible antecedent invariants and group them
    // by class
    Map /*Invariant.class->List<Invariant> */ antecedent_map
      = new LinkedHashMap();
    find_antecedents (ppt.views_iterator(), antecedent_map);

    // Loop through each suppression creating each invariant that
    // is suppressed by that suppression.
    for (Iterator i = all_suppressions.iterator(); i.hasNext(); ) {
      NISuppressionSet ss = (NISuppressionSet) i.next();
      for (Iterator j = ss.iterator(); j.hasNext(); ) {
        NISuppression sup = (NISuppression) j.next();
        suppressed_invs.addAll (sup.create_suppressed_invs (antecedent_map));
      }
    }

    // Add each invariant to its slice if it is not already there.  Since
    // an invariant can be suppressed by multiple suppressions, it may be
    // created multiple times.  Remove any duplicates from suppressed_invs
    for (Iterator i = suppressed_invs.iterator(); i.hasNext(); ) {
      Invariant inv = (Invariant) i.next();
      if (inv.ppt.find_inv_exact (inv) == null)
        inv.ppt.addInvariant (inv);
      else /* its a duplicate, remove it */
        i.remove();
    }

    return (suppressed_invs);
  }

  /**
   * Processes each slice in slice_iterator and fills the specified
   * map with a list of all of the antecedent invariants for each
   * class. @return the number of false antecedents found.
   */
  static int find_antecedents (Iterator slice_iterator,
                    Map /*Invariant.class->List<Invariant> */ antecedent_map) {

    int false_cnt = 0;

    while (slice_iterator.hasNext()) {
      PptSlice slice = (PptSlice) slice_iterator.next();
      for (Iterator j = slice.invs.iterator(); j.hasNext(); ) {
        Invariant inv = (Invariant) j.next();
        if (!is_suppressor (inv.getClass()))
          continue;
        if (inv.is_false())
          false_cnt++;
        List antecedents = (List) antecedent_map.get (inv.getClass());
        if (antecedents == null) {
          antecedents = new ArrayList();
          antecedent_map.put (inv.getClass(), antecedents);
        }
        antecedents.add (inv);
      }
    }

    return (false_cnt);
  }

  /**
   * Removes any invariants in the specified ppt that are suppressed
   */
  public static void remove_suppressed_invs (PptTopLevel ppt) {

    // Fmt.pf ("Removing suppressed invariants for " + ppt.name);
    for (Iterator i = ppt.views_iterator(); i.hasNext(); ) {
      PptSlice slice = (PptSlice) i.next();
      for (Iterator j = slice.invs.iterator(); j.hasNext(); ) {
        Invariant inv = (Invariant) j.next();
        inv.log ("Considering removal for " + inv.format());
        if (inv.is_ni_suppressed())
          j.remove();
      }
    }
  }

  /**
   * Returns true if the specified class is an antecedent in any NI suppression
   */
  public static boolean is_suppressor (Class cls) {
    return (suppressor_map.containsKey (cls));
  }

  public static void set_vt (ValueTuple vt) {
    NIS.vt = vt;
  }

  /**
   * Dump out the suppressor map.
   */
  public static void dump (Logger log) {

    if (!log.isLoggable(Level.FINE))
      return;

    for (Iterator i = suppressor_map.keySet().iterator(); i.hasNext(); ) {
      Class sclass = (Class) i.next();
      List suppression_set_list = (List) suppressor_map.get (sclass);
      for (ListIterator j = suppression_set_list.listIterator(); j.hasNext();) {
        NISuppressionSet ss = (NISuppressionSet) j.next();
        if (j.previousIndex() > 0)
          log.fine (Fmt.spf ("        : %s", ss));
        else
          log.fine (Fmt.spf ("%s: %s", sclass, ss));
      }
    }
  }

  public static String toString
                (Map /*Invariant.class->List<Invariant>*/ antecedent_map) {

    String out = "";

    for (Iterator i = antecedent_map.keySet().iterator(); i.hasNext(); ) {
      Class iclass = (Class) i.next();
      out += UtilMDE.unqualified_name (iclass) + " : ";
      List /*Invariant*/ ilist = (List) antecedent_map.get (iclass);
      for (Iterator j = ilist.iterator(); j.hasNext(); ) {
        Invariant inv = (Invariant) j.next();
        if (inv.is_false())
          out += inv.format() + "[FALSE] ";
        else
          out += inv.format() + " ";
      }
      out += " : ";
    }

    return (out);
  }

  public static String map_size_string (Map antecedent_map) {

    String out = "";

    for (Iterator i = antecedent_map.keySet().iterator(); i.hasNext(); ) {
      Class iclass = (Class) i.next();
      List /*Invariant*/ ilist = (List) antecedent_map.get (iclass);
      out += UtilMDE.unqualified_name (iclass) + " " + ilist.size() + " : ";
    }

    return (out);
  }

  /**
   * Class used to describe invariants without instantiating the invariant.
   * The invariant is defined by its class and variables (its ppt is
   * presumed to be known externally).  Since only the class is specified,
   * this is only adequate for invariants determined completely by their
   * class (all ternary invariants fall into this category)
   */
  static class SupInv {
    NISuppressee suppressee;
    VarInfo[] vis = new VarInfo[3];

    /** Invariant is defined by its suppressee and the variables it is over**/
    public SupInv (NISuppressee suppressee, VarInfo[] vis) {
      this.suppressee = suppressee;
      this.vis = vis;
    }

    /** equal iff classes and variables match exactly **/
    public boolean equals (Object obj) {
      if (!(obj instanceof SupInv))
        return (false);

      SupInv sinv = (SupInv) obj;
      return ((sinv.suppressee.sup_class == suppressee.sup_class)
              && (sinv.vis[0] == vis[0]) && (sinv.vis[1] == vis[1])
              && (sinv.vis[2] == vis[2]));
    }

    /** hash on class and variables **/
    public int hashCode() {
      return (suppressee.sup_class.hashCode() + vis[0].hashCode()
              + vis[1].hashCode() + vis[2].hashCode());
    }

    /** Check this invariant against the sample and return the result */
    public InvariantStatus check (ValueTuple vt) {
      return suppressee.check (vt, vis);
    }

    /** Instantiate this invariant on the specified ppt */
    public Invariant instantiate (PptTopLevel ppt) {
      return suppressee.instantiate (vis, ppt);
    }
  }



}
