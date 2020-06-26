package daikon.suppress;

import daikon.Daikon;
import daikon.Debug;
import daikon.PptSlice;
import daikon.PptTopLevel;
import daikon.ValueTuple;
import daikon.VarComparability;
import daikon.VarInfo;
import daikon.inv.Invariant;
import daikon.inv.InvariantStatus;
import daikon.inv.ValueSet;
import daikon.inv.binary.BinaryInvariant;
import daikon.inv.binary.twoScalar.IntEqual;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.checkerframework.checker.initialization.qual.UnknownInitialization;
import org.checkerframework.checker.interning.qual.Interned;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.EnsuresNonNull;
import org.checkerframework.checker.nullness.qual.EnsuresNonNullIf;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.nullness.qual.RequiresNonNull;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;
import typequals.prototype.qual.Prototype;

// Outstanding NIS todo list
//
//  - Merging is slow when there are multiple children.
//
//  - Move the missingOutOfBounds check to is_slice_ok()
//

/** Main class for non-instantiating suppression. Handles setup and other overall functions. */
public class NIS {

  @SuppressWarnings("initialization.fields.uninitialized") // never instantiated
  public NIS() {
    throw new Error("Do not instantiate");
  }

  /** Debug tracer. */
  public static final Logger debug = Logger.getLogger("daikon.suppress.NIS");

  /** Debug Tracer for antecedent method. */
  public static final Logger debugAnt = Logger.getLogger("daikon.suppress.NIS.Ant");

  /** Boolean. If true, enable non-instantiating suppressions. */
  public static boolean dkconfig_enabled = true;

  /** Signifies which algorithm is used by NIS to process suppressions. */
  public enum SuppressionProcessor {
    HYBRID,
    ANTECEDENT,
    FALSIFIED
  }

  /**
   * Specifies the algorithm that NIS uses to process suppressions. Possible selections are
   * 'HYBRID', 'ANTECEDENT', and 'FALSIFIED'. The default is the hybrid algorithm which uses the
   * falsified algorithm when only a small number of suppressions need to be processed and the
   * antecedent algorithm when a large number of suppressions are processed.
   */
  public static SuppressionProcessor dkconfig_suppression_processor = SuppressionProcessor.HYBRID;

  /**
   * Boolean. If true, use antecedent method for NIS processing. If false, use falsified method for
   * processing falsified invariants for NISuppressions. Note this flag is for internal use only and
   * is controlled by NIS.dkconfig_suppression_processor.
   */
  public static boolean antecedent_method = true;

  /**
   * Boolean. If true, use a combination of the falsified method for a small number of suppressions
   * to be processed and the antecedent method for a large number. Number is determined by
   * NIS.dkconfig_hybrid_threshhold. Note this flag is for internal use only and is controlled by
   * NIS.dkconfig_suppression_processor.
   */
  public static boolean hybrid_method = true;

  /**
   * Int. Less and equal to this number means use the falsified method in the hybrid method of
   * processing falsified invariants, while greater than this number means use the antecedent
   * method. Empirical data shows that number should not be more than 10000.
   */
  public static int dkconfig_hybrid_threshhold = 2500;

  /**
   * Boolean. If true, use the specific list of suppressor related invariant prototypes when
   * creating constant invariants in the antecedent method.
   */
  public static boolean dkconfig_suppressor_list = true;

  /**
   * Boolean. If true, skip variables of file rep type {@code hashcode} when creating invariants
   * over constants in the antecedent method.
   */
  public static boolean dkconfig_skip_hashcode_type = true;

  /**
   * Possible states for suppressors and suppressions. When a suppression is checked, it sets one of
   * these states on each suppressor.
   */
  public enum SuppressState {
    /** initial state -- suppressor has not been checked yet */
    NONE,
    /** suppressor matches the falsified invariant */
    MATCH,
    /** suppressor is true */
    VALID,
    /** suppressor is not true */
    INVALID,
    /** suppressor contains a variable that has always been nonsensical */
    NONSENSICAL
  }

  // This should be an enum!!
  /** initial state -- suppressor has not been checked yet */
  static final @Interned String NONE = "none";

  /**
   * Map from invariant class to a list of all of the suppression sets that contain a suppressor of
   * that class.
   */
  public static @MonotonicNonNull Map<Class<? extends Invariant>, List<NISuppressionSet>>
      suppressor_map;

  /**
   * Map from invariant class to the number of suppressions that contain a suppressor of that class.
   */
  public static @MonotonicNonNull Map<Class<? extends Invariant>, Integer>
      suppressor_map_suppression_count;

  /** List of all suppressions. */
  static @MonotonicNonNull List<NISuppressionSet> all_suppressions;

  /** List of suppressor invariant prototypes. */
  public static @MonotonicNonNull List<@Prototype Invariant> suppressor_proto_invs;

  /**
   * List of invariants that are unsuppressed by the current sample. The {@link #falsified} and
   * {@link #process_falsified_invs} methods add created invariants to this list. This list is
   * cleared by {@link #apply_samples}.
   */
  public static List<Invariant> new_invs = new ArrayList<>();

  /**
   * List of invariants that are unsuppressed and then falsified by the current sample. This list is
   * cleared at the beginning of apply_samples() and falsified invariants are added as the current
   * sample is applied to invariants in new_invs. The list is only used when the falsified method is
   * used for processing suppressions.
   */
  public static List<Invariant> newly_falsified = new ArrayList<>();

  // Statistics that are kept during processing.  Some of these are kept
  // and/or make sense for some approaches and not for others

  /** Whether or not to keep statistics. */
  public static boolean keep_stats = false;
  /** Number of falsified invariants in the program point. */
  public static int false_cnts = 0;
  /** Number of falsified invariants in the program point that are potential suppressors. */
  public static int false_invs = 0;
  /** Number of suppressions processed. */
  public static int suppressions_processed = 0;
  /** Number of suppressions processed by the falsified method. */
  public static int suppressions_processed_falsified = 0;
  /** Number of invariants that are no longer suppressed by a suppression. */
  static int new_invs_cnt = 0;
  /** Number of new_invs_cnt that are falsified by the sample. */
  public static int false_invs_cnt = 0;
  /** Number of invariants actually created. */
  public static int created_invs_cnt = 0;
  /** Number of invariants that are still suppressed. */
  static int still_suppressed_cnt = 0;

  /** Total time spent in NIS processing. */
  public static long duration = 0;

  /** First execution of dump_stats(). Used to dump a header. */
  static boolean first_time = true;

  /**
   * Sets up non-instantiation suppression. Primarily this includes setting up the map from
   * suppressor classes to all of the suppression sets associated with that suppressor invariant.
   */
  @EnsuresNonNull({
    "suppressor_map",
    "suppressor_map_suppression_count",
    "all_suppressions",
    "suppressor_proto_invs"
  })
  public static void init_ni_suppression() {

    // Creating these here, rather than where they are declared, allows
    // this method to be called multiple times without a problem.
    suppressor_map = new LinkedHashMap<>(256);
    suppressor_map_suppression_count = new LinkedHashMap<>(256);
    all_suppressions = new ArrayList<NISuppressionSet>();
    suppressor_proto_invs = new ArrayList<@Prototype Invariant>();

    // This should be the first statement in the method, but put it after the
    // field initalizations so that the Initialization Checker doesn't complain.
    if (!dkconfig_enabled) {
      return;
    }

    // Get all defined suppressions.
    for (Invariant inv : Daikon.proto_invs) {
      NISuppressionSet ss = inv.get_ni_suppressions();
      if (ss != null) {
        for (int j = 0; j < ss.suppression_set.length; j++) {
          NISuppression sup = ss.suppression_set[j];
          if (true) {
            assert inv.getClass() == sup.suppressee.sup_class
                : "class "
                    + inv.getClass()
                    + " doesn't match "
                    + sup
                    + "/"
                    + sup.suppressee.sup_class;
            assert inv.getClass() == ss.suppression_set[j].suppressee.sup_class
                : "class " + inv.getClass() + " doesn't match " + ss.suppression_set[j];
          }
        }
        all_suppressions.add(ss);
      }
    }

    // map suppressor classes to suppression sets
    for (NISuppressionSet suppression_set : all_suppressions) {
      suppression_set.add_to_suppressor_map(suppressor_map);
    }

    // If any suppressor is itself suppressed, augment the suppressions
    // where the suppressor is used with the suppressor's suppressions.
    for (List<NISuppressionSet> ss_list : suppressor_map.values()) {
      for (NISuppressionSet ss : ss_list) {
        NISuppressee suppressee = ss.get_suppressee();
        List<NISuppressionSet> suppressor_ss_list = suppressor_map.get(suppressee.sup_class);
        if (suppressor_ss_list == null) {
          continue;
        }
        for (NISuppressionSet suppressor_ss : suppressor_ss_list) {
          suppressor_ss.recurse_definitions(ss);
        }
      }
    }

    // map suppressor classes to suppression sets
    // (now that recursive definitions are in place)
    suppressor_map.clear();
    for (NISuppressionSet suppression_set : all_suppressions) {
      suppression_set.add_to_suppressor_map(suppressor_map);
    }

    if (NIS.dkconfig_suppressor_list) {
      // Set up the list of suppressor invariant prototypes
      for (@Prototype Invariant i : Daikon.proto_invs) {
        if (suppressor_map.containsKey(i.getClass())) {
          suppressor_proto_invs.add(i);
        }
      }
    }

    // count the number of suppressions associated with each
    // suppressor
    //  if (NIS.hybrid_method) {
    for (Class<? extends Invariant> a : suppressor_map.keySet()) {

      int x = 0;
      List<NISuppressionSet> ss_list = suppressor_map.get(a);
      for (NISuppressionSet ss : ss_list) {
        x += ss.suppression_set.length;
      }

      suppressor_map_suppression_count.put(a, x);
    }
    // }

    if (Debug.logDetail() && debug.isLoggable(Level.FINE)) dump(debug);
  }

  /**
   * Instantiates any invariants that are no longer suppressed because inv has been falsified.
   *
   * <p>Note: this method is should NOT be used with the antecedent approach.
   *
   * @see #process_falsified_invs
   */
  @RequiresNonNull("suppressor_map")
  public static void falsified(Invariant inv) {

    if (!dkconfig_enabled || antecedent_method) {
      return;
    }

    if (NIS.dkconfig_skip_hashcode_type) {

      boolean hashFound = false;

      for (VarInfo vi : inv.ppt.var_infos) {
        if (vi.file_rep_type.isScalar()
            && !vi.file_rep_type.isIntegral()
            && !vi.file_rep_type.isArray()) {
          hashFound = true;
        }
      }

      if (hashFound) {
        return;
      }
    }

    // Get the suppression sets (if any) associated with this invariant
    List<NISuppressionSet> ss_list = suppressor_map.get(inv.getClass());
    if (ss_list == null) {
      return;
    }

    long startTime = 0;
    // Count the number of falsified invariants that are antecedents
    if (keep_stats) {
      startTime = System.nanoTime();
      if (PptTopLevel.first_pass_with_sample && suppressor_map.containsKey(inv.getClass())) {
        false_invs++;
      }
    }

    // Process each suppression set
    for (NISuppressionSet ss : ss_list) {
      ss.clear_state();
      if (debug.isLoggable(Level.FINE)) {
        debug.fine("processing suppression set " + ss + " over falsified inv " + inv.format());
      }
      ss.falsified(inv, new_invs);
      suppressions_processed += ss.suppression_set.length;
    }

    if (keep_stats) {
      duration += (System.nanoTime() - startTime);
    }
  }

  /**
   * Applies sample values to all of the newly created invariants (kept in new_invs). The sample
   * should never falsify the invariant (since we don't create an invariant if the sample would
   * invalidate it). The sample still needs to be applied, however, for sample-dependent invariants.
   *
   * <p>Clears the new_invs list after processing. Currently this routine checks to insure that the
   * newly falsified invariant is not itself a possible NI suppressor.
   */
  public static void apply_samples(ValueTuple vt, int count) {
    newly_falsified.clear();

    if (NIS.debug.isLoggable(Level.FINE)) {
      NIS.debug.fine("Applying samples to " + new_invs.size() + " new invariants");
    }

    // Loop through each invariant
    for (Invariant inv : new_invs) {
      if (inv.is_false()) {
        assert !inv.is_false()
            : String.format(
                "inv %s in ppt %s is false before sample is applied ", inv.format(), inv.ppt);
      }

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
        InvariantStatus result = inv.add_sample(vt, count);
        if (result == InvariantStatus.FALSIFIED) {
          if (NIS.antecedent_method) {
            throw new Error(
                "inv "
                    + inv.format()
                    + " falsified by sample "
                    + Debug.toString(inv.ppt.var_infos, vt)
                    + " at ppt "
                    + inv.ppt);
          } else {
            inv.falsify();
            newly_falsified.add(inv);
          }
        }
      }

      // Add the invariant to its slice
      if (Debug.dkconfig_internal_check) {
        assert inv.ppt.parent.findSlice(inv.ppt.var_infos) == inv.ppt;
      }
      inv.ppt.addInvariant(inv);
      if (Debug.logOn()) {
        inv.log("%s added to slice", inv.format());
      }

      if (NIS.antecedent_method) created_invs_cnt++;
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
        inv.log("removed, still suppressed in second pass");
        inv.ppt.invs.remove(inv);
        i.remove();
      }
    }

    new_invs.clear();
  }

  /** Clears the current NIS statistics and enables the keeping of statistics. */
  public static void clear_stats() {

    keep_stats = true;
    duration = 0;
    false_invs = 0;
    false_cnts = 0;
    suppressions_processed = 0;
    suppressions_processed_falsified = 0;
    new_invs_cnt = 0;
    false_invs_cnt = 0;
    created_invs_cnt = 0;
    still_suppressed_cnt = 0;
  }

  public static void clear_sample_stats() {
    keep_stats = true;
    false_invs = 0;
    false_cnts = 0;
    suppressions_processed = 0;
    suppressions_processed_falsified = 0;
    new_invs_cnt = 0;
    false_invs_cnt = 0;
    created_invs_cnt = 0;
    still_suppressed_cnt = 0;
  }

  public static void stats_header(Logger log) {

    log.fine(
        "false invs  : "
            + "suppressions processed  : "
            + "new invs cnt  : "
            + "false invs cnt  : "
            + "created invs cnt  : "
            + "still suppressed cnt  : "
            + "elapsed time msecs : "
            + "ppt name");
  }

  /** dump statistics on NIS to the specified logger. */
  public static void dump_stats(Logger log, PptTopLevel ppt) {

    if (first_time) {
      stats_header(log);
      first_time = false;
    }

    if (false_invs > 0) {
      log.fine(
          false_invs
              + " : "
              + suppressions_processed
              + " : "
              + new_invs_cnt
              + " : "
              + false_invs_cnt
              + " : "
              + created_invs_cnt
              + " : "
              + still_suppressed_cnt
              + " : "
              + TimeUnit.NANOSECONDS.toMillis(duration)
              + " msecs "
              // + build_ants_msecs + " " + process_ants_msecs + " : "
              + ppt.name);
    }
  }

  /**
   * Creates any invariants that were previously suppressed, but are no longer suppressed. Must be
   * called after the sample has been processed and any invariants falsified by the sample are
   * marked as such, but before they have been removed.
   */
  @RequiresNonNull({
    "suppressor_map",
    "suppressor_map_suppression_count",
    "all_suppressions",
    "NIS.suppressor_proto_invs"
  })
  public static void process_falsified_invs(PptTopLevel ppt, ValueTuple vt) {

    // if using the hybrid method, need to know the number of falsified suppressor
    // invariants before deciding which method to use
    if (NIS.hybrid_method) {
      int count = 0;
      for (Iterator<Invariant> i = ppt.invariants_iterator(); i.hasNext(); ) {
        Invariant inv = i.next();

        if (NIS.dkconfig_skip_hashcode_type) {

          boolean hashFound = false;

          for (VarInfo vi : inv.ppt.var_infos) {
            if (vi.file_rep_type.isHashcode()) {
              hashFound = true;
            }
          }

          if (hashFound) {
            continue;
          }
        }

        if (inv.is_false()) {
          false_cnts++;

          if (suppressor_map.containsKey(inv.getClass())) {

            // use the following count update when splitting the hybrid method by the
            // number of total suppressions associated with the falsified invariants
            @SuppressWarnings("nullness") // map:  same keys in suppressor_map and
            // suppressor_map_suppression_count
            int map_count = suppressor_map_suppression_count.get(inv.getClass());
            count += map_count;
            suppressions_processed_falsified += map_count;
          }
        }
      }

      if (count > NIS.dkconfig_hybrid_threshhold) {
        antecedent_method = true;
      } else {
        antecedent_method = false;
      }
    }

    if (!dkconfig_enabled || !antecedent_method) {
      return;
    }

    if (false) {
      System.out.println("Variables for ppt " + ppt.name());
      for (int i = 0; i < ppt.var_infos.length; i++) {
        VarInfo v = ppt.var_infos[i];
        ValueSet vs = v.get_value_set();
        System.out.printf(
            "  %s %s %s %b %s%n",
            v.comparability, v.name(), v.file_rep_type, ppt.is_constant(v), vs.repr_short());
      }
    }

    // If there are no falsified invariants that are suppressors, there is nothing to do
    int false_cnt = 0;
    for (Iterator<Invariant> i = ppt.invariants_iterator(); i.hasNext(); ) {
      Invariant inv = i.next();
      if (inv.is_false() && suppressor_map.containsKey(inv.getClass())) {
        false_cnt++;
      }
    }

    // System.out.printf("Invariants for ppt %s: %d%n", ppt, inv_cnt);
    if (false_cnt == 0) {
      return;
    }

    if (debugAnt.isLoggable(Level.FINE)) {
      debugAnt.fine("at ppt " + ppt.name + " false_cnt = " + false_cnt);
    }
    // false_invs = false_cnt;

    if (debugAnt.isLoggable(Level.FINE)) {
      ppt.debug_invs(debugAnt);
    }

    // Find all antecedents and organize them by their variables comparability
    Map<VarComparability, Antecedents> comp_ants = new LinkedHashMap<>();
    store_antecedents_by_comparability(ppt.views_iterator(), comp_ants);

    if (ppt.constants != null) {
      store_antecedents_by_comparability(
          ppt.constants.create_constant_invs().iterator(), comp_ants);
    }

    if (debugAnt.isLoggable(Level.FINE)) {
      for (Antecedents ants : comp_ants.values()) {
        debugAnt.fine(ants.toString());
      }
    }

    // Add always-comparable antecedents to each of the other maps.
    merge_always_comparable(comp_ants);

    if (false) {
      for (Antecedents ants : comp_ants.values()) {
        List<Invariant> eq_invs = ants.get(IntEqual.class);
        if ((eq_invs != null) && (eq_invs.size() > 1000)) {
          Map<VarInfo, Count> var_map = new LinkedHashMap<>();
          System.out.printf(
              "ppt %s, comparability %s has %s equality invs%n",
              ppt.name, ants.comparability, eq_invs.size());
          for (Invariant inv : eq_invs) {
            IntEqual ie = (IntEqual) inv;
            VarInfo v1 = ie.ppt.var_infos[0];
            VarInfo v2 = ie.ppt.var_infos[1];
            if (ppt.is_constant(v1) && ppt.is_constant(v2)) {
              System.out.printf("inv %s has two constant variables%n", ie.format());
            }
            if (!v1.compatible(v2)) {
              System.out.printf("inv %s has incompatible variables%n", ie.format());
            }
            Count cnt = var_map.get(v1);
            if (cnt == null) {
              cnt = new Count(0);
              var_map.put(v1, cnt);
            }
            cnt.val++;
            cnt = var_map.get(v2);
            if (cnt == null) {
              cnt = new Count(0);
              var_map.put(v2, cnt);
            }
            cnt.val++;
          }
          System.out.printf("%d distinct variables%n", var_map.size());
          for (VarInfo key : var_map.keySet()) {
            Count cnt = var_map.get(key);
            System.out.printf(" %s %s %d %n", key.comparability, key.name(), cnt.val);
          }
        }
      }
    }

    // Remove any Antecedents without any falsified invariants.  They can't
    // possibly create any newly unsuppressed invariants
    for (Iterator<Antecedents> i = comp_ants.values().iterator(); i.hasNext(); ) {
      Antecedents ants = i.next();
      if (ants.false_cnt == 0) i.remove();
    }
    if (debugAnt.isLoggable(Level.FINE)) {
      for (Antecedents ants : comp_ants.values()) {
        debugAnt.fine(ants.toString());
      }
    }

    // Loop through each suppression creating each invariant that
    // is suppressed by that suppression.  Each set of comparable antecedents
    // is processed separately
    Set<SupInv> unsuppressed_invs = new LinkedHashSet<>();
    for (NISuppressionSet ss : all_suppressions) {
      for (NISuppression sup : ss) {
        suppressions_processed++;
        for (Antecedents ants : comp_ants.values()) {
          sup.find_unsuppressed_invs(unsuppressed_invs, ants);
        }
      }
    }

    if (debugAnt.isLoggable(Level.FINE)) {
      debugAnt.fine(
          "Found " + unsuppressed_invs.size() + " unsuppressed invariants: " + unsuppressed_invs);
    }

    // Create each new unsuppressed invariant that is not still suppressed
    // by a different suppression.  Skip any that will be falsified by
    // the sample.  Checking the sample is faster than checking suppression
    // and removes the invariant more often, so it is checked first
    for (SupInv supinv : unsuppressed_invs) {
      new_invs_cnt++;
      if (supinv.check(vt) == InvariantStatus.FALSIFIED) {
        supinv.log("unsuppressed inv falsified by sample");
        false_invs_cnt++;
        continue;
      }
      if (supinv.is_ni_suppressed()) {
        supinv.log("unsuppresed inv still suppressed");
        still_suppressed_cnt++;
        continue;
      }
      Invariant inv = supinv.instantiate(ppt);
      if (inv != null) {
        if (Debug.dkconfig_internal_check) {
          assert !inv.is_ni_suppressed() : "Still suppressed: " + inv.format();
          if (inv.ppt.find_inv_exact(inv) != null) {
            throw new Error("inv " + inv.format() + " already exists in ppt " + ppt.name);
          }
        }
        new_invs.add(inv);
      }
    }
  }

  /**
   * Merges the always-comparable antecedents (if any) into each of the other sets of antecedents.
   * Also removes the always-comparable set of antecedents as a separate set (since it is now merged
   * into each of the other sets). Updates comp_ants accordingly.
   *
   * <p>In general, in implicit comparability, the variables at a program point are partioned into
   * disjoint sets of comparable variables. However, implicit comparability also allows some
   * variables to be comparable to all others (always-comparable). An invariant is always-comparable
   * if all of its variables are always-comparable. Since always-comparable invariants can form
   * suppressions with all other invariants, they must be added to each of set of comparable
   * antecedents.
   */
  @RequiresNonNull("NIS.suppressor_map")
  static void merge_always_comparable(Map<VarComparability, Antecedents> comp_ants) {

    // Find the antecedents that are always comparable (if any)
    Antecedents compare_all = null;
    for (VarComparability vc : comp_ants.keySet()) {
      if (vc.alwaysComparable()) {
        compare_all = comp_ants.get(vc);
        break;
      }
    }

    // Add always comparable antecedents to each of the other maps.
    if ((compare_all != null) && (comp_ants.size() > 1)) {
      for (Antecedents ants : comp_ants.values()) {
        if (ants.alwaysComparable()) {
          continue;
        }
        ants.add(compare_all);
      }
      comp_ants.remove(compare_all.comparability);
    }
  }

  /**
   * Creates all suppressed invariants for the specified ppt and places them in their associated
   * slices.
   *
   * @return a list of created invariants
   */
  @RequiresNonNull({"all_suppressions", "suppressor_map"})
  public static List<Invariant> create_suppressed_invs(PptTopLevel ppt) {

    // Find all antecedents and organize them by their variables comparability
    Map<VarComparability, Antecedents> comp_ants = new LinkedHashMap<>();
    store_antecedents_by_comparability(ppt.views_iterator(), comp_ants);

    // Add always-comparable antecedents to each of the other maps.
    merge_always_comparable(comp_ants);

    // Loop through each suppression creating each invariant that
    // is suppressed by that suppression.  Each set of comparable antecedents
    // is processed separately.
    Set<SupInv> suppressed_invs = new LinkedHashSet<>();
    for (NISuppressionSet ss : all_suppressions) {
      for (NISuppression sup : ss) {
        for (Antecedents ants : comp_ants.values()) {
          sup.find_suppressed_invs(suppressed_invs, ants);
        }
      }
    }

    // Create each invariant and add it to its slice.
    List<Invariant> created_invs = new ArrayList<>(suppressed_invs.size());
    for (SupInv supinv : suppressed_invs) {
      Invariant inv = supinv.instantiate(ppt);
      if (inv != null) {
        if (Debug.dkconfig_internal_check) assert inv.ppt.find_inv_exact(inv) == null;
        inv.ppt.addInvariant(inv);
        created_invs.add(inv);
      }
    }

    return created_invs;
  }

  /**
   * Adds each antecedent invariant in the specified slices to the Antecedents object in comp_ants
   * with the corresponding VarComparability.
   */
  @RequiresNonNull("suppressor_map")
  static void store_antecedents_by_comparability(
      Iterator<PptSlice> slice_iterator, Map<VarComparability, Antecedents> comp_ants) {

    for (Iterator<PptSlice> i = slice_iterator; i.hasNext(); ) {
      PptSlice slice = i.next();

      if (NIS.dkconfig_skip_hashcode_type) {

        boolean hashFound = false;

        for (VarInfo vi : slice.var_infos) {
          if (vi.file_rep_type.isHashcode()) {
            hashFound = true;
          }
        }

        if (hashFound) {
          continue;
        }
      }

      for (Invariant inv : slice.invs) {
        if (!is_suppressor(inv.getClass())) {
          continue;
        }

        if (inv.is_false()) {
          false_invs++;
        }

        VarComparability vc = inv.get_comparability();
        Antecedents ants = comp_ants.get(vc);
        if (ants == null) {
          ants = new Antecedents(vc);
          comp_ants.put(vc, ants);
        }
        ants.add(inv);
        // if (Debug.logOn())
        //  inv.log ("Added to antecedent map " + inv.format() + " compare = "
        //           + vc);
      }
    }
  }

  /**
   * Processes each slice in slice_iterator and fills the specified map with a list of all of the
   * antecedent invariants for each class. @return the number of false antecedents found
   */
  @RequiresNonNull("suppressor_map")
  static int find_antecedents(
      Iterator<PptSlice> slice_iterator,
      Map<Class<? extends Invariant>, List<Invariant>> antecedent_map) {

    int false_cnt = 0;

    while (slice_iterator.hasNext()) {
      PptSlice slice = slice_iterator.next();
      for (Invariant inv : slice.invs) {
        if (!is_suppressor(inv.getClass())) {
          continue;
        }
        if (inv.is_false()) false_cnt++;
        List<Invariant> antecedents = antecedent_map.get(inv.getClass());
        if (antecedents == null) {
          antecedents = new ArrayList<Invariant>();
          antecedent_map.put(inv.getClass(), antecedents);
        }
        antecedents.add(inv);
      }
    }

    return false_cnt;
  }

  /** Removes any invariants in the specified ppt that are suppressed. */
  public static void remove_suppressed_invs(PptTopLevel ppt) {

    for (PptSlice slice : ppt.views_iterable()) {
      // Old-style for loop with Iterator because it will be side-effected
      for (Iterator<Invariant> j = slice.invs.iterator(); j.hasNext(); ) {
        Invariant inv = j.next();
        if (inv.is_ni_suppressed()) {
          inv.log("Removed because suppressed %s", inv.format());
          j.remove();
        }
      }
    }
  }

  /** Returns true if the specified class is an antecedent in any NI suppression. */
  @RequiresNonNull("NIS.suppressor_map")
  @Pure
  public static boolean is_suppressor(Class<? extends Invariant> cls) {
    return (suppressor_map.containsKey(cls));
  }

  /** Dump out the suppressor map. */
  @RequiresNonNull("suppressor_map")
  public static void dump(Logger log) {

    if (!log.isLoggable(Level.FINE)) {
      return;
    }

    for (Class<? extends Invariant> sclass : suppressor_map.keySet()) {
      List<NISuppressionSet> suppression_set_list = suppressor_map.get(sclass);
      for (ListIterator<NISuppressionSet> j = suppression_set_list.listIterator(); j.hasNext(); ) {
        NISuppressionSet ss = j.next();
        if (j.previousIndex() > 0) {
          log.fine(String.format("        : %s", ss));
        } else {
          log.fine(String.format("%s: %s", sclass, ss));
        }
      }
    }
  }

  /**
   * Class used to describe invariants without instantiating the invariant. The invariant is defined
   * by its NISuppressee and variables (Its ppt is also stored, but not used in comparisions, its
   * presumed that only SupInvs from the same ppt will every be compared.)
   */
  static class SupInv {
    NISuppressee suppressee;
    VarInfo[] vis;
    PptTopLevel ppt;

    /** Create an invariant definition for a suppressed invariant. */
    public SupInv(NISuppressee suppressee, VarInfo[] vis, PptTopLevel ppt) {
      this.suppressee = suppressee;
      this.vis = vis;
      this.ppt = ppt;
      if (Debug.logOn()) log("Created " + suppressee);
    }

    /** Track Log the specified message. */
    public void log(@UnknownInitialization(SupInv.class) SupInv this, String message) {
      if (Debug.logOn()) Debug.log(suppressee.sup_class, ppt, vis, message);
    }

    /** Equal iff classes / swap variable / and variables match exactly. */
    @EnsuresNonNullIf(result = true, expression = "#1")
    @Pure
    @Override
    public boolean equals(@GuardSatisfied SupInv this, @GuardSatisfied @Nullable Object obj) {
      if (!(obj instanceof SupInv)) {
        return false;
      }

      // Class and variables must match
      SupInv sinv = (SupInv) obj;
      if (sinv.suppressee.sup_class != suppressee.sup_class) {
        return false;
      }
      if (vis.length != sinv.vis.length) {
        return false;
      }
      for (int i = 0; i < vis.length; i++) {
        if (vis[i] != sinv.vis[i]) {
          return false;
        }
      }

      // Binary invariants must match swap var as well
      if (suppressee.var_count == 2) {
        if (sinv.suppressee.get_swap() != suppressee.get_swap()) {
          return false;
        }
      }

      return true;
    }

    /** Hash on class and variables. */
    @Pure
    @Override
    public int hashCode(@GuardSatisfied SupInv this) {
      int code = suppressee.sup_class.hashCode();
      for (int i = 0; i < vis.length; i++) {
        code += vis[i].hashCode();
      }
      return code;
    }

    /** Check this invariant against the sample and return the result. */
    public InvariantStatus check(ValueTuple vt) {
      return suppressee.check(vt, vis);
    }

    /** Returns true if the invariant is still suppressed. */
    @SuppressWarnings("all:purity") // new object is not returned
    @Pure
    public boolean is_ni_suppressed() {

      NISuppressionSet ss = suppressee.sample_inv.get_ni_suppressions();
      assert ss != null
          : "@AssumeAssertion(nullness):  dependent:  this invariant's class can be suppressed, so ss != null";
      return (ss.suppressed(ppt, vis));
    }

    /** Instantiate this invariant on the specified ppt. */
    public @Nullable Invariant instantiate(PptTopLevel ppt) {
      return suppressee.instantiate(vis, ppt);
    }

    /**
     * Returns the invariant if it already exists, or null otherwise. Unary and and ternary
     * invariant must match by class (there are no permutations for unary invariants and ternary
     * invariants handle permutations as different classes). Binary invariants must match the class
     * and if there is an internal swap variable for variable order, that must match as well.
     */
    public @Nullable Invariant already_exists() {
      Invariant cinv = ppt.find_inv_by_class(vis, suppressee.sup_class);
      if (cinv == null) {
        return null;
      }
      if (suppressee.var_count != 2) {
        return cinv;
      }
      BinaryInvariant binv = (BinaryInvariant) cinv;
      if (binv.is_symmetric()) {
        return cinv;
      }
      if (binv.get_swap() != suppressee.get_swap()) {
        return null;
      }
      return cinv;
    }

    /** Return string representation of the suppressed invariant. */
    @SideEffectFree
    @Override
    public String toString(@GuardSatisfied SupInv this) {
      String[] names = new String[vis.length];
      for (int i = 0; i < vis.length; i++) {
        names[i] = vis[i].name();
      }
      return suppressee + "[" + String.join(", ", names) + "]";
    }
  }

  /** Class that organizes all of the antecedent invariants with the same comparability by class. */
  static class Antecedents {

    /**
     * Comparability of the variables in the antecedents. Only variables that are comparable should
     * be stored here.
     */
    VarComparability comparability;

    /**
     * Map from the antecedent invariants class to a list of the antecedent invariants of that
     * class. Allows fast access to invariants by type.
     */
    Map<Class<? extends Invariant>, List<Invariant>> antecedent_map;

    /** Number of antecedents that are false. */
    int false_cnt = 0;

    /** Create with specified comparability. */
    public Antecedents(VarComparability comparability) {

      antecedent_map = new LinkedHashMap<>();
      this.comparability = comparability;
    }

    /** Returns true if this contains antecedents that are always comparable. */
    public boolean alwaysComparable() {
      return comparability.alwaysComparable();
    }

    /**
     * Adds the specified invariant to the list for its class. Falsified invariants are added to the
     * beginning of the list, non-falsified ones to the end.
     */
    @RequiresNonNull("NIS.suppressor_map")
    public void add(Invariant inv) {

      // Only possible antecedents need to be added
      if (!is_suppressor(inv.getClass())) {
        return;
      }

      // Only antecedents comparable to this one should be added
      assert VarComparability.comparable(inv.get_comparability(), comparability);

      // Ignore antecedents that are missing out of bounds.  They can't
      // create any valid invariants (since the suppressee is always over
      // the same variables
      for (int i = 0; i < inv.ppt.var_infos.length; i++) {
        VarInfo v = inv.ppt.var_infos[i];
        if (v.missingOutOfBounds()) {
          return;
        }
      }

      if (inv.is_false()) {
        false_cnt++;
      }

      // Add the invariant to the map for its class
      List<Invariant> antecedents = get(inv.getClass());
      if (antecedents == null) {
        antecedents = new ArrayList<Invariant>();
        antecedent_map.put(inv.getClass(), antecedents);
      }
      if (inv.is_false()) {
        antecedents.add(0, inv);
      } else {
        antecedents.add(inv);
      }
    }

    /** Adds all of the antecedents specified to the lists for their class. */
    @RequiresNonNull("NIS.suppressor_map")
    public void add(Antecedents ants) {

      for (List<Invariant> invs : ants.antecedent_map.values()) {
        for (Invariant inv : invs) {
          add(inv);
        }
      }
    }

    /**
     * Returns a list of all of the antecedent invariants of the specified class. Returns null if
     * there are none of that class.
     */
    public @Nullable List<Invariant> get(Class<? extends Invariant> cls) {

      return antecedent_map.get(cls);
    }

    /** Returns a string representation of all of the antecedents by class. */
    @SideEffectFree
    @Override
    public String toString(@GuardSatisfied Antecedents this) {

      String out = "Comparability " + comparability + " : ";

      for (Class<? extends Invariant> iclass : antecedent_map.keySet()) {
        out += iclass.getSimpleName() + " : ";
        List<Invariant> ilist = antecedent_map.get(iclass);
        for (Invariant inv : ilist) {
          if (inv.is_false()) {
            out += inv.format() + "[FALSE] ";
          } else {
            out += inv.format() + " ";
          }
        }
        out += " : ";
      }

      return out;
    }
  }

  static class Count {
    public int val;

    Count(int val) {
      this.val = val;
    }
  }
}
