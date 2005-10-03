package daikon.suppress;

import daikon.*;
import daikon.inv.*;
import daikon.inv.binary.*;
import daikon.inv.binary.twoScalar.*;
import daikon.inv.binary.twoString.*;
import daikon.inv.ternary.*;
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

  /** Boolean.  If true, enable non-instantiating suppressions. **/
  public static boolean dkconfig_enabled = true;

  /** Boolean. If true, use antecedent method for creating unsuppressed
   *  invariants.
   */
  public static boolean dkconfig_antecedent_method = true;

  // Possible states for suppressors and suppressions.  When a suppression
  // is checked, it sets one of these states on each suppressor
  /** initial state -- suppressor has not been checked yet **/
  static final String NONE = "none";
  /** suppressor matches the falsified invariant **/
  static final String MATCH = "match";
  /** suppressor is true **/
  static final String VALID = "valid";
  /** suppressor is not true **/
  static final String INVALID = "invalid";
  /** suppressor contains a variable that has always been missing **/
  static final String MISSING = "missing";

  /**
   * Map from invariant class to a list of all of the suppression sets
   * that contain a suppressor of that class.
   */
  static Map<Class,List<NISuppressionSet>>
    suppressor_map = new LinkedHashMap<Class,List<NISuppressionSet>>(256);

  /** List of all suppressions */
  static List<NISuppressionSet> all_suppressions = new ArrayList<NISuppressionSet>();

  /**
   * List of invariants that are unsuppressed by the current sample.
   * The falsified() and process_falsified_invs() methods add created
   * invariants to this list.  This list is cleared by apply_samples()
   */
  public static List<Invariant> new_invs = new ArrayList<Invariant>();

  // Statistics that are kept during processing.  Some of these are kept
  // and/or make sense for some approches and not for others

  /** Whether or not to keep statistics **/
  static boolean keep_stats = false;
  /** Number of falsified invariants processed **/
  static int false_invs = 0;
  /** Number of suppressions processed **/
  static int suppressions_processed = 0;
  /** Number of invariants that are no longer suppressed  by a suppression **/
  static int new_invs_cnt = 0;
  /** Number of new_invs_cnt that are falsified by the sample **/
  static int false_invs_cnt = 0;
  /** Number of invariants actually created **/
  static int created_invs_cnt = 0;
  /** Number of invariants that are still suppressed **/
  static int still_suppressed_cnt = 0;
  /** Total time spent in NIS processing for this sample **/
  static Stopwatch watch = new Stopwatch (false);
  /** First execution of dump_stats().  Used to dump a header **/
  static boolean first_time = true;

  /**
   * Sets up non-instantiation suppression.  Primarily this includes setting
   * up the map from suppressor classes to all of the suppression sets
   * associated with that suppressor invariant
   */
  public static void init_ni_suppression() {

    if (!dkconfig_enabled)
      return;

    // Get all defined suppressions.
    for (Iterator<Invariant> i = Daikon.proto_invs.iterator(); i.hasNext(); ) {
      Invariant inv = i.next();
      NISuppressionSet ss = inv.get_ni_suppressions();
      if (ss != null) {
        for (int j = 0; j < ss.suppression_set.length; j++) {
          NISuppression sup = ss.suppression_set[j];
          if (true) {
          assert inv.getClass() == sup.suppressee.sup_class : "class "
            + inv.getClass() + " doesn't match " + sup + "/"
            + sup.suppressee.sup_class;
          Assert.assertTrue (inv.getClass()
                            == ss.suppression_set[j].suppressee.sup_class,
                           "class " + inv.getClass() + " doesn't match "
                            + ss.suppression_set[j]);
          }
        }
        all_suppressions.add (ss);
      }
    }

    // map suppressor classes to suppression sets
    for (Iterator<NISuppressionSet> i = all_suppressions.iterator(); i.hasNext(); ) {
      NISuppressionSet suppression_set = i.next();
      suppression_set.add_to_suppressor_map (suppressor_map);
    }

    // If any suppressor is itself suppressed, augment the suppressions
    // where the suppressor is used with the suppressor's suppressions.
    for (Iterator<List<NISuppressionSet>> i = suppressor_map.values().iterator(); i.hasNext(); ) {
      List<NISuppressionSet> ss_list = i.next();
      for (Iterator<NISuppressionSet> j = ss_list.iterator(); j.hasNext(); ) {
        NISuppressionSet ss = j.next();
        NISuppressee suppressee = ss.get_suppressee();
        List<NISuppressionSet> suppressor_ss_list
          = suppressor_map.get (suppressee.sup_class);
        if (suppressor_ss_list == null)
          continue;
        for (Iterator<NISuppressionSet> k = suppressor_ss_list.iterator(); k.hasNext(); ) {
          NISuppressionSet suppressor_ss = k.next();
          suppressor_ss.recurse_definitions (ss);
          // Fmt.pf ("New recursed suppressions: " + suppressor_ss);
        }
      }
    }

    if (Debug.logDetail() && debug.isLoggable (Level.FINE))
      dump (debug);
  }

  /**
   * Instantiates any invariants that are no longer suppressed because
   * inv has been falsified.
   *
   * Note: this method should NOT be used with the antecedent approach.
   * See NIS.process_falsified_invs()
   */
  public static void falsified (Invariant inv) {

    if (!dkconfig_enabled || dkconfig_antecedent_method)
      return;

    // Get the suppression sets (if any) associated with this invariant
    List<NISuppressionSet> ss_list = suppressor_map.get(inv.getClass());
    if (ss_list == null) {
      return;
    }

    // Count the number of falsified invariants that are antecedents
    if (keep_stats) {
      watch.start();
      false_invs++;
    }

    // Process each suppression set
    for (Iterator<NISuppressionSet> i = ss_list.iterator(); i.hasNext(); ) {
      NISuppressionSet ss = i.next();
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
   * Applies sample values to all of the newly created invariants
   * (kept in new_invs).  The sample should never falsify the
   * invariant (since we don't create an invariant if the sample would
   * invalidate it).  The sample still needs to be applied, however, for
   * sample-dependent invariants.
   *
   * Clears the new_invs list after processing.  Currently this
   * routine checks to insure that the newly falsified invariant
   * is not itself a possible NI suppressor.
   */
  public static void apply_samples (ValueTuple vt, int count) {

    if (NIS.debug.isLoggable (Level.FINE))
      NIS.debug.fine ("Applying samples to " + new_invs.size()
                       + " new invariants");

    // Loop through each invariant
    for (Iterator<Invariant> i = new_invs.iterator(); i.hasNext(); ) {
      Invariant inv = i.next();
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

      // If no variables are missing, apply the sample
      if (!missing) {
        InvariantStatus result = inv.add_sample (vt, count);
        if (result == InvariantStatus.FALSIFIED)
          Assert.assertTrue (false, "inv " + inv.format()
                             + " falsified by sample "
                             + Debug.toString (inv.ppt.var_infos, vt)
                             + " at ppt " + inv.ppt);
      }

      // Add the invariant to its slice
      if (Daikon.dkconfig_internal_check)
        Assert.assertTrue (inv.ppt.parent.findSlice(inv.ppt.var_infos)
                            == inv.ppt);
      inv.ppt.addInvariant (inv);
      if (Debug.logOn())
        inv.log (inv.format() + " added to slice");
      created_invs_cnt++;
    }

    // Make a second pass through the new invariants and make sure that
    // they are still not suppressed.  They can become suppressed when
    // there are recursive suppressions and the new suppressor wasn't
    // yet created above when the invariant was first checked to see
    // if it was suppressed.
    for (Iterator<Invariant> i = new_invs.iterator(); i.hasNext(); ) {
      Invariant inv = i.next();
      // inv.log ("Considering whether still suppressed in second pass");
      if (inv.is_ni_suppressed()) {
        still_suppressed_cnt++;
        inv.log ("removed, still suppressed in second pass");
        inv.ppt.invs.remove (inv);
        i.remove();
        // Fmt.pf ("Invariant %s suppressed in second pass", inv.format());
      }
    }

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
  }

  public static void stats_header (Logger log) {

    log.fine ("false invs  : "
             + "suppressions processed  : "
             + "new invs cnt  : "
             + "false invs cnt  : "
             + "created invs cnt  : "
             + "still suppressed cnt  : "
             + "elapsed time msecs : "
             + "ppt name");
  }

  /**
   * dump statistics on NIS to the specified logger
   */
  public static void dump_stats (Logger log, PptTopLevel ppt) {

    if (first_time) {
      stats_header (log);
      first_time = false;
    }

    if (false_invs > 0) {
      log.fine (false_invs + " : "
                  + suppressions_processed + " : "
                  + new_invs_cnt + " : "
                  + false_invs_cnt + " : "
                  + created_invs_cnt + " : "
                  + still_suppressed_cnt + " : "
                  + watch.elapsedMillis() + " msecs "
                // + build_ants_msecs + " " + process_ants_msecs + " : "
                  + ppt.name);
    }
  }

  /**
   * Creates any invariants that were previously suppressed, but are no
   * longer suppressed.  Must be called after the sample has been processed
   * and any invariants falsified by the sample are marked as such, but before
   * they have been removed.
   */
  public static void process_falsified_invs (PptTopLevel ppt, ValueTuple vt) {

    if (!dkconfig_enabled || !dkconfig_antecedent_method)
      return;

    if (false) {
      Fmt.pf ("Variables for ppt " + ppt.name());
      for (int i = 0; i < ppt.var_infos.length; i++) {
        VarInfo v = ppt.var_infos[i];
        ValueSet vs = v.get_value_set();
        Fmt.pf ("  %s %s %s %s %s", v.comparability, v.name.name(),
                v.file_rep_type, "" + ppt.is_constant(v), vs.repr_short());
      }
    }

    // If there are no falsified invariants, there is nothing to do
    int false_cnt = 0;
    int inv_cnt = 0;
    for (Iterator<Invariant> i = ppt.invariants_iterator(); i.hasNext(); ) {
      Invariant inv = i.next();
      if (inv.is_false())
        false_cnt++;
      inv_cnt++;
    }
    // System.out.printf ("Invariants for ppt %s: %d\n", ppt, inv_cnt);
    if (false_cnt == 0)
      return;
    if (debugAnt.isLoggable (Level.FINE))
      debugAnt.fine ("at ppt " + ppt.name + " false_cnt = " + false_cnt);
    false_invs = false_cnt;

    watch.start();

    if (debugAnt.isLoggable (Level.FINE))
      ppt.debug_invs (debugAnt);

    // Find all antecedents and organize them by their variables comparability
    Map<VarComparability,Antecedents> comp_ants = new LinkedHashMap<VarComparability,Antecedents>();
    store_antecedents_by_comparability (ppt.views_iterator(), comp_ants);
    if (ppt.constants != null)
      store_antecedents_by_comparability
        (ppt.constants.create_constant_invs().iterator(), comp_ants);
    if (debugAnt.isLoggable (Level.FINE)) {
      for (Iterator<Antecedents> i = comp_ants.values().iterator(); i.hasNext(); ) {
        Antecedents ants = i.next();
        debugAnt.fine (ants.toString());
      }
    }

    // Add always-comparable antecedents to each of the other maps.
    merge_always_comparable (comp_ants);

    if (false) {
      for (Iterator<Antecedents> i = comp_ants.values().iterator(); i.hasNext(); ) {
        Antecedents ants = i.next();
        List<Invariant> eq_invs = ants.get (IntEqual.class);
        if ((eq_invs != null) && (eq_invs.size() > 1000)) {
          Map<VarInfo,Count> var_map = new LinkedHashMap<VarInfo,Count>();
          Fmt.pf ("ppt %s, comparability %s has %s equality invariants",
                  ppt.name, ants.comparability, "" + eq_invs.size());
          for (Iterator<Invariant> j = eq_invs.iterator(); j.hasNext(); ) {
            IntEqual inv = (IntEqual) j.next();
            VarInfo v1 = inv.ppt.var_infos[0];
            VarInfo v2 = inv.ppt.var_infos[1];
            if (ppt.is_constant(v1) && ppt.is_constant(v2))
              Fmt.pf ("inv %s has two constant variables", inv.format());
            if (!v1.compatible (v2))
              Fmt.pf ("inv %s has incompatible variables", inv.format());
            Count cnt = (Count) var_map.get (v1);
            if (cnt == null) {
              cnt = new Count (0);
              var_map.put (v1, cnt);
            }
            cnt.val++;
            cnt = (Count) var_map.get (v2);
            if (cnt == null) {
              cnt = new Count (0);
              var_map.put (v2, cnt);
            }
            cnt.val++;
          }
          Fmt.pf ("%s distinct variables", "" + var_map.size());
          for (VarInfo key : var_map.keySet()) {
            Count cnt = (Count) var_map.get (key);
            Fmt.pf (" %s %s %s ", key.comparability, key.name.name(),
                    "" + cnt.val);
          }
        }
      }
    }

    // Remove any Antecedents without any falsified invariants.  They can't
    // possibly create any newly unsuppressed invariants
    for (Iterator<Antecedents> i = comp_ants.values().iterator(); i.hasNext(); ) {
      Antecedents ants = i.next();
      // Fmt.pf ("ants = " + ants);
      if (ants.false_cnt == 0)
        i.remove();
    }
    if (debugAnt.isLoggable (Level.FINE)) {
      for (Iterator<Antecedents> i = comp_ants.values().iterator(); i.hasNext(); ) {
        Antecedents ants = i.next();
        debugAnt.fine (ants.toString());
      }
    }

    // Loop through each suppression creating each invariant that
    // is suppressed by that suppression.  Each set of comparable antecedents
    // is processed separately
    Set<SupInv> unsuppressed_invs = new LinkedHashSet<SupInv>();
    for (Iterator<NISuppressionSet> i = all_suppressions.iterator(); i.hasNext(); ) {
      NISuppressionSet ss = i.next();
      for (Iterator<NISuppression> j = ss.iterator(); j.hasNext(); ) {
        NISuppression sup = j.next();
        suppressions_processed++;
        for (Iterator<Antecedents> k = comp_ants.values().iterator(); k.hasNext(); ) {
          Antecedents ants = k.next();
          sup.find_unsuppressed_invs (unsuppressed_invs, ants);
        }
      }
    }
    if (debugAnt.isLoggable (Level.FINE))
      debugAnt.fine ("Found " + unsuppressed_invs.size() +
                     " unsuppressed invariants: " + unsuppressed_invs);

    // Create each new unsuppressed invariant that is not still suppressed
    // by a different suppression.  Skip any that will be falsified by
    // the sample.  Checking the sample is faster than checking suppression
    // and removes the invariant more often, so it is checked first
    for (Iterator<SupInv> i = unsuppressed_invs.iterator(); i.hasNext(); ) {
      SupInv supinv = i.next();
      new_invs_cnt++;
      if (supinv.check (vt) == InvariantStatus.FALSIFIED) {
        supinv.log ("unsuppressed inv falsified by sample");
        false_invs_cnt++;
        continue;
      }
      if (supinv.is_ni_suppressed()) {
        supinv.log ("unsuppresed inv still suppressed");
        still_suppressed_cnt++;
        continue;
      }
      Invariant inv = supinv.instantiate (ppt);
      if (inv != null) {
        if (Daikon.dkconfig_internal_check) {
          if (inv.ppt.find_inv_exact (inv) != null)
            Assert.assertTrue (false, "inv " + inv.format()
                               + " already exists in ppt " + ppt.name);
        }
        new_invs.add (inv);
      }
    }


    watch.stop();
  }

  /**
   * Merges the always-comparable antecedents (if any) into each of the
   * other sets of antecedents.  Also removes the always-comparable
   * set of antecedents as a separate set (since it is now merged into
   * each of the other sets).  Updates comp_ants accordingly.
   *
   * In general, in implicit comparability, the variables at a program
   * point are partioned into disjoint sets of comparable variables.
   * However, implicit comparability also allows some variables to be
   * comparable to all others (always-comparable).  An invariant is
   * always-comparable if all of its variables are always-comparable.
   * Since always-comparable invariants can form suppressions with all
   * other invariants, they must be added to each of set of comparable
   * antecedents.
   */
  static void merge_always_comparable
                        (Map<VarComparability,Antecedents> comp_ants) {

    // Find the antecedents that are always comparable (if any)
    Antecedents compare_all = null;
    for (VarComparability vc : comp_ants.keySet()) {
      if (vc.alwaysComparable()) {
        compare_all = (Antecedents) comp_ants.get (vc);
        break;
      }
    }

    // Add always comparable antecedents to each of the other maps.
    if ((compare_all != null) && (comp_ants.size() > 1)) {
      for (Iterator<Antecedents> i = comp_ants.values().iterator(); i.hasNext(); ) {
        Antecedents ants = i.next();
        if (ants.alwaysComparable())
          continue;
        ants.add (compare_all);
      }
      comp_ants.remove (compare_all.comparability);
    }
  }

  /**
   * Creates all suppressed invariants for the specified ppt and
   * places them in their associated slices.
   * @return a list of created invariants.
   */
  public static List<Invariant> create_suppressed_invs (PptTopLevel ppt) {

    // Find all antecedents and organize them by their variables comparability
    Map<VarComparability,Antecedents> comp_ants = new LinkedHashMap<VarComparability,Antecedents>();
    store_antecedents_by_comparability (ppt.views_iterator(), comp_ants);

    // Add always-comparable antecedents to each of the other maps.
    merge_always_comparable (comp_ants);

    // Loop through each suppression creating each invariant that
    // is suppressed by that suppression.  Each set of comparable antecedents
    // is processed separately.
    Set<SupInv> suppressed_invs = new LinkedHashSet<SupInv>();
    for (Iterator<NISuppressionSet> i = all_suppressions.iterator(); i.hasNext(); ) {
      NISuppressionSet ss = i.next();
      for (Iterator<NISuppression> j = ss.iterator(); j.hasNext(); ) {
        NISuppression sup = j.next();
        for (Iterator<Antecedents> k = comp_ants.values().iterator(); k.hasNext(); ) {
          Antecedents ants = k.next();
          sup.find_suppressed_invs (suppressed_invs, ants);
        }
      }
    }

    // Create each invariant and add it to its slice.
    List<Invariant> created_invs = new ArrayList<Invariant> (suppressed_invs.size());
    for (Iterator<SupInv> i = suppressed_invs.iterator(); i.hasNext(); ) {
      SupInv supinv = i.next();
      Invariant inv = supinv.instantiate (ppt);
      if (inv != null) {
        if (Daikon.dkconfig_internal_check)
          Assert.assertTrue (inv.ppt.find_inv_exact (inv) == null);
        inv.ppt.addInvariant (inv);
        created_invs.add (inv);
      }
    }

    return (created_invs);
  }

  /**
   * Adds each antecedent invariant in the specified slices to the Antecedents
   * object in comp_ants with the corresponding VarComparability.
   */
  static void store_antecedents_by_comparability
                           (Iterator<PptSlice> slice_iterator,
                            Map<VarComparability,Antecedents> comp_ants) {

    for (Iterator<PptSlice> i = slice_iterator; i.hasNext(); ) {
      PptSlice slice = i.next();
      for (Iterator<Invariant> j = slice.invs.iterator(); j.hasNext(); ) {
        Invariant inv = j.next();
        if (!is_suppressor (inv.getClass()))
          continue;
        if (inv.is_false())
          false_invs++;
        VarComparability vc = inv.get_comparability();
        Antecedents ants = (Antecedents) comp_ants.get (vc);
        if (ants == null) {
          ants = new Antecedents (vc);
          comp_ants.put (vc, ants);
        }
        ants.add (inv);
        //if (Debug.logOn())
        //  inv.log ("Added to antecedent map " + inv.format() + " compare = "
        //           + vc);
      }
    }
  }

  /**
   * Processes each slice in slice_iterator and fills the specified
   * map with a list of all of the antecedent invariants for each
   * class. @return the number of false antecedents found.
   */
  static int find_antecedents (Iterator<PptSlice> slice_iterator,
                    Map<Class,List<Invariant>> antecedent_map) {

    int false_cnt = 0;

    while (slice_iterator.hasNext()) {
      PptSlice slice = slice_iterator.next();
      for (Iterator<Invariant> j = slice.invs.iterator(); j.hasNext(); ) {
        Invariant inv = j.next();
        if (!is_suppressor (inv.getClass()))
          continue;
        if (inv.is_false())
          false_cnt++;
        List<Invariant> antecedents = antecedent_map.get (inv.getClass());
        if (antecedents == null) {
          antecedents = new ArrayList<Invariant>();
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
    for (Iterator<PptSlice> i = ppt.views_iterator(); i.hasNext(); ) {
      PptSlice slice = i.next();
      for (Iterator<Invariant> j = slice.invs.iterator(); j.hasNext(); ) {
        Invariant inv = j.next();
        if (inv.is_ni_suppressed()) {
          inv.log ("Removed because suppressed " + inv.format());
          j.remove();
        }
      }
    }
  }

  /**
   * Returns true if the specified class is an antecedent in any NI suppression
   */
  public static boolean is_suppressor (Class cls) {
    return (suppressor_map.containsKey (cls));
  }

  /**
   * Dump out the suppressor map.
   */
  public static void dump (Logger log) {

    if (!log.isLoggable(Level.FINE))
      return;

    for (Class sclass : suppressor_map.keySet()) {
      List<NISuppressionSet> suppression_set_list = suppressor_map.get (sclass);
      for (ListIterator<NISuppressionSet> j = suppression_set_list.listIterator(); j.hasNext();) {
        NISuppressionSet ss = j.next();
        if (j.previousIndex() > 0)
          log.fine (Fmt.spf ("        : %s", ss));
        else
          log.fine (Fmt.spf ("%s: %s", sclass, ss));
      }
    }
  }

  /**
   * Class used to describe invariants without instantiating the
   * invariant.  The invariant is defined by its NISuppressee and variables
   * (Its ppt is also stored, but not used in comparisions, its
   * presumed that only SupInvs from the same ppt will every be
   * compared)
   */
  static class SupInv {
    NISuppressee suppressee;
    VarInfo[] vis;
    PptTopLevel ppt;

    /** Create an invariant definition for a suppressed invariant */
    public SupInv (NISuppressee suppressee, VarInfo[] vis, PptTopLevel ppt) {
      this.suppressee = suppressee;
      this.vis = vis;
      this.ppt = ppt;
      if (Debug.logOn())
        log ("Created " + suppressee);
    }

    /** Track Log the specified message **/
    public void log (String message) {
      if (Debug.logOn())
        Debug.log (suppressee.sup_class, ppt, vis, message);
    }

    /** Equal iff classes / swap variable / and variables match exactly **/
    public boolean equals (Object obj) {
      if (!(obj instanceof SupInv))
        return (false);

      // Class and variables must match
      SupInv sinv = (SupInv) obj;
      if (sinv.suppressee.sup_class != suppressee.sup_class)
        return (false);
      if (vis.length != sinv.vis.length)
        return (false);
      for (int i = 0; i < vis.length; i++)
        if (vis[i] != sinv.vis[i])
          return (false);

      // Binary invariants must match swap var as well
      if (suppressee.var_count == 2) {
        if (sinv.suppressee.get_swap() != suppressee.get_swap())
          return (false);
      }

      return (true);
    }

    /** Hash on class and variables **/
    public int hashCode() {
      int code = suppressee.sup_class.hashCode();
      for (int i = 0; i < vis.length; i++)
        code += vis[i].hashCode();
      return (code);
    }

    /** Check this invariant against the sample and return the result */
    public InvariantStatus check (ValueTuple vt) {
      return suppressee.check (vt, vis);
    }

    /** Returns true if the invariant is still suppressed **/
    public boolean is_ni_suppressed() {

      NISuppressionSet ss = suppressee.sample_inv.get_ni_suppressions();
      return (ss.suppressed (ppt, vis));
    }

    /** Instantiate this invariant on the specified ppt */
    public Invariant instantiate (PptTopLevel ppt) {
      return suppressee.instantiate (vis, ppt);
    }

    /**
     * Checks to see if the invariant already exists.  Unary and
     * and ternary invariant must match by class (there are no
     * permutations for unary invariants and ternary invariants handle
     * permutations as different classes).  Binary invariants must
     * match the class and if there is an internal swap variable for
     * variable order, that must match as well.
     */
    public Invariant already_exists () {
      Invariant cinv = ppt.find_inv_by_class (vis, suppressee.sup_class);
      if (cinv == null)
        return (null);
      if (suppressee.var_count != 2)
        return (cinv);
      BinaryInvariant binv = (BinaryInvariant) cinv;
      if (binv.is_symmetric())
        return (cinv);
      if (binv.get_swap() != suppressee.get_swap())
        return (null);
      return (cinv);
    }

    /** Return string representation of the suppressed invariant **/
    public String toString () {
      String out = "";
      for (int i = 0; i < vis.length; i++) {
        if (out != "")          // interned
          out += ", ";
        out += vis[i].name.name();
      }
      out = suppressee + "[" + out + "]";
      return (out);
    }
  }

  /**
   * Class that organizes all of the antecedent invariants with
   * the same comparability by class.
   */
  static class Antecedents {

    /**
     * Comparability of the variables in the antecedents.  Only
     * variables that are comparable should be stored here.
     **/
    VarComparability comparability;

    /**
     * Map from the antecedent invariants class to a list of the
     * antecedent invariants of that class.  Allows fast access to
     * invariants by type
     */
    Map<Class,List<Invariant>> antecedent_map;

    /** Number of antecedents that are false **/
    int false_cnt = 0;

    /** Create with specified comparability */
    public Antecedents (VarComparability comparability) {

      antecedent_map = new LinkedHashMap<Class,List<Invariant>>();
      this.comparability = comparability;
    }

    /**
     * Returns true if this contains antecedents that are always comparable
     */
    public boolean alwaysComparable() {
      return comparability.alwaysComparable();
    }

    /**
     * Adds the specified invariant to the list for its class.  Falsified
     * invariants are added to the beginning of the list, non-falsified
     * ones to the end.
     */
    public void add (Invariant inv) {

      // Only possible antecedents need to be added
      if (!is_suppressor (inv.getClass()))
        return;

      // Only antecedents comparable to this one should be added
      Assert.assertTrue (comparability.comparable (inv.get_comparability(),
                                                   comparability));

      // Ignore antecedents that are missing out of bounds.  They can't
      // create any valid invariants (since the suppressee is always over
      // the same variables
      for (int i = 0; i < inv.ppt.var_infos.length; i++) {
        VarInfo v = inv.ppt.var_infos[i];
        if (v.missingOutOfBounds())
          return;
      }

      if (inv.is_false())
        false_cnt++;

      // Add the invariant to the map for its class
      List<Invariant> antecedents = get (inv.getClass());
      if (antecedents == null) {
        antecedents = new ArrayList<Invariant>();
        antecedent_map.put (inv.getClass(), antecedents);
      }
      if (inv.is_false())
        antecedents.add (0, inv);
      else
        antecedents.add (inv);
    }

    /**
     * Adds all of the antecedents specified to the lists for their class
     */
    public void add (Antecedents ants) {

      for (Iterator<List<Invariant>> i = ants.antecedent_map.values().iterator(); i.hasNext();){
        List<Invariant> invs = i.next();
        for (Iterator<Invariant> j = invs.iterator(); j.hasNext(); ) {
          Invariant inv = j.next();
          add (inv);
        }
      }
    }

    /**
     * Returns a list of all of the antecedent invariants of the specified
     * class.  Returns NULL if there are none of that class
     */
    public List<Invariant> get (Class cls) {

      return antecedent_map.get (cls);
    }

    /**
     * Returns a string representation of all of the antecedents by class
     */
    public String toString() {

      String out = "Comparability " + comparability + " : ";

      for (Class iclass : antecedent_map.keySet()) {
        out += UtilMDE.unqualified_name (iclass) + " : ";
        List<Invariant> ilist = antecedent_map.get (iclass);
        for (Iterator<Invariant> j = ilist.iterator(); j.hasNext(); ) {
          Invariant inv = j.next();
          if (inv.is_false())
            out += inv.format() + "[FALSE] ";
          else
            out += inv.format() + " ";
        }
        out += " : ";
      }

      return (out);
    }
  }

  static class Count {
    public int val;
    Count (int val) {
      this.val = val;
    }
  }
}
