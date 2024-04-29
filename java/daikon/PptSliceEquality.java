package daikon;

import static daikon.tools.nullness.NullnessUtil.castNonNullDeep;

import daikon.inv.Equality;
import daikon.inv.Invariant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.checkerframework.checker.initialization.qual.UnknownInitialization;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.EnsuresNonNullIf;
import org.checkerframework.checker.nullness.qual.KeyFor;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;

/** Holds Equality invariants. */
public class PptSliceEquality extends PptSlice {
  static final long serialVersionUID = 20021231L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.

  /**
   * If true, create one equality set for each variable. This has the effect of turning the equality
   * optimization off, without actually removing the sets themselves (which are presumed to exist in
   * many parts of the code).
   */
  public static boolean dkconfig_set_per_var = false;

  public static final Logger debug = Logger.getLogger("daikon.PptSliceEquality");

  public static final Logger debugGlobal = Logger.getLogger("daikon.PptSliceEquality.Global");

  PptSliceEquality(PptTopLevel parent) {
    super(parent, parent.var_infos);
  }

  @Override
  public final int arity(@UnknownInitialization(PptSlice.class) PptSliceEquality this) {
    throw new Error("Don't call arity on PptSliceEquality");
  }

  void init_po() {
    throw new Error("Shouldn't get called");
  }

  @Override
  public void addInvariant(Invariant inv) {
    assert inv instanceof Equality;
    invs.add(inv);
  }

  // Not valid for this type of slice.  Always pretend there are enough.
  @Override
  public int num_samples(@UnknownInitialization @GuardSatisfied PptSliceEquality this) {
    if (true) {
      throw new Error();
    }
    return Integer.MAX_VALUE;
  }

  public int num_mod_samples() {
    if (true) {
      throw new Error();
    }
    return Integer.MAX_VALUE;
  }

  @Override
  public int num_values() {
    if (true) {
      throw new Error();
    }
    return Integer.MAX_VALUE;
  }

  /**
   * Encapsulates a VarInfo and its Comparability so that the two can be used to create sets of
   * VarInfos that are initially equal. Two VarInfoAndComparability's are true iff they are
   * VarComparability.comparable() to each other.
   */
  private static class VarInfoAndComparability {
    public VarInfo vi;

    @Pure
    @Override
    public int hashCode(@GuardSatisfied VarInfoAndComparability this) {
      // This is very coarse but is about as good as we can do it.  Can't do hashcode of
      // the comparability because two comparabilities may be
      // comparable and yet be not the same.
      // (e.g. VarComparabilityExplicit).
      return vi.file_rep_type.hashCode();
    }

    @EnsuresNonNullIf(result = true, expression = "#1")
    @Pure
    @Override
    public boolean equals(
        @GuardSatisfied VarInfoAndComparability this, @GuardSatisfied @Nullable Object o) {
      if (!(o instanceof VarInfoAndComparability)) {
        return false;
      }
      return equalsVarInfoAndComparability((VarInfoAndComparability) o);
    }

    /**
     * Whether two VarInfos can be set to be equal to each other is whether they are comparableNWay.
     * Since we do not yet handle inheritance, we require that the comparability go both ways.
     */
    @EnsuresNonNullIf(result = true, expression = "#1")
    @Pure
    public boolean equalsVarInfoAndComparability(
        @GuardSatisfied VarInfoAndComparability this, @GuardSatisfied VarInfoAndComparability o) {

      return (vi.comparableNWay(o.vi)
          && vi.comparability.equality_set_ok(o.vi.comparability)
          && vi.aux.equals_for_instantiation(o.vi.aux));
    }

    public VarInfoAndComparability(VarInfo vi) {
      this.vi = vi;
    }
  }

  /** Actually instantiate the equality sets. */
  @Override
  void instantiate_invariants() {

    // If each variable gets its own set, create those sets and return
    if (dkconfig_set_per_var) {
      // Debug.debugTrack.fine ("Vars for " + parent.name());
      for (VarInfo vi : var_infos) {
        List<VarInfo> vi_list = Collections.singletonList(vi);
        Equality eq = new Equality(vi_list, this);
        invs.add(eq);
        // System.out.println ("  eq set = " + eq.shortString());
      }
      return;
    }

    // Start with everything comparable being equal.
    if (debug.isLoggable(Level.FINE)) {
      debug.fine("PptSliceEquality.instantiate_invariants: " + parent.name() + " vars:");
    }
    LinkedHashMap<VarInfoAndComparability, List<VarInfo>> multiMap = new LinkedHashMap<>();
    for (VarInfo vi : var_infos) {
      VarInfoAndComparability viac = new VarInfoAndComparability(vi);
      addToBindingList(multiMap, viac, vi);
      if (debug.isLoggable(Level.FINE)) {
        debug.fine("  " + vi.name() + ": " + vi.comparability);
      }
    }
    if (debug.isLoggable(Level.FINE)) {
      debug.fine(
          "PptSliceEquality.instantiate_invariants "
              + parent.name()
              + ": "
              + Integer.toString(multiMap.keySet().size())
              + " VarInfoAndComparability keys");
    }
    Equality[] newInvs = new Equality[multiMap.keySet().size()];
    int varCount = 0;
    int invCount = 0;
    for (List<VarInfo> list : multiMap.values()) {
      varCount += list.size();

      Equality eq = new Equality(list, this);
      newInvs[invCount] = eq;
      if (debug.isLoggable(Level.FINE)) {
        debug.fine(" Created: " + eq);
        for (VarInfo vi : list) {
          debug.fine("   vi: " + vi + " aux : " + vi.aux);
        }
      }
      if (Debug.logOn()) {
        Debug.log(getClass(), parent, Debug.vis(eq.leader()), "Created");
      }
      invCount++;
    }
    // Ensure determinism
    Arrays.sort(newInvs, EqualityComparator.theInstance);
    invs.addAll(Arrays.<Invariant>asList(newInvs));
    assert varCount == var_infos.length; // Check that we get all vis
  }

  /**
   * Instantiate the full equality sets from a set of variable pairs where each member of a pair is
   * equal to the other.
   */
  public void instantiate_from_pairs(Set<VarInfo.Pair> eset) {

    // Build a map from each variable to all those that are equal to it
    Map<VarInfo, List<VarInfo>> varmap = new LinkedHashMap<>();
    Map<VarInfo, Integer> sample_cnt_map = new LinkedHashMap<>();
    for (VarInfo.Pair cp : eset) {
      List<VarInfo> vlist = varmap.get(cp.v1);
      if (vlist == null) {
        vlist = new ArrayList<VarInfo>();
        vlist.add(cp.v1);
        varmap.put(cp.v1, vlist);
        sample_cnt_map.put(cp.v1, cp.samples);
      }
      vlist.add(cp.v2);
      vlist = varmap.get(cp.v2);
      if (vlist == null) {
        vlist = new ArrayList<VarInfo>();
        vlist.add(cp.v2);
        varmap.put(cp.v2, vlist);
        sample_cnt_map.put(cp.v2, cp.samples);
      }
      vlist.add(cp.v1);
    }

    // Loop through each variable, building the appropriate equality set
    // for each.  Note that variables that are distinct still have an
    // equality set (albeit with only the one variable)
    ArrayList<Invariant> newInvs = new ArrayList<>();
    for (VarInfo v : var_infos) {
      if (v.equalitySet != null) {
        continue;
      }
      List<VarInfo> vlist = varmap.computeIfAbsent(v, Collections::singletonList);
      Equality eq = new Equality(vlist, this);
      Integer sample_cnt = sample_cnt_map.get(v);
      if (sample_cnt != null) {
        eq.setSamples(sample_cnt.intValue());
      }
      v.equalitySet = eq;
      newInvs.add(eq);
    }
    invs.addAll(newInvs);
  }

  /**
   * Returns a List of Invariants that have been weakened/destroyed. However, this handles the
   * creation of new Equality invariants and the instantiation of other invariants.
   *
   * @return a List of invariants that have been weakened
   */
  // The basic approach is as follows:
  //    - Loop through each equality set
  //        - look for any variables that are no longer equal
  //        - Create new equality sets (call createEqualityInvs)
  //        - Get the new leaders
  //        - Create new slices and invariants (call CopyInvsFromLeader)
  //
  @Override
  public List<Invariant> add(ValueTuple vt, int count) {

    ArrayList<Equality> allNewInvs = new ArrayList<>();
    ArrayList<Invariant> weakenedInvs = new ArrayList<>();

    // Loop through each existing equality invariant
    for (Invariant invar : invs) {
      Equality inv = (Equality) invar;

      // Add this sample to the invariant and track any vars that fall
      // out of the set.
      List<VarInfo> nonEqualVis = inv.add(vt, count);

      // If some vars fell out
      if (nonEqualVis.size() > 0) {

        // Create new equality sets for all of the non-equal vars
        List<Equality> newInvs = createEqualityInvs(nonEqualVis, vt, inv, count);

        // Get a list of all of the new non-missing leaders
        List<VarInfo> newInvsLeaders = new ArrayList<>(newInvs.size());
        for (Equality eq : newInvs) {
          if ((parent.constants == null) || !parent.constants.is_missing(eq.leader())) {
            newInvsLeaders.add(eq.leader());
          }
        }

        // Debug print the new leaders
        if (Debug.logOn()) {
          for (VarInfo nileader : newInvsLeaders) {
            Debug.log(
                getClass(),
                parent,
                Debug.vis(nileader),
                "Split off from previous leader "
                    + inv.leader().name()
                    + ": new set = "
                    + nileader.equalitySet
                    + ": old set = "
                    + inv);
          }
        }

        // Create new slices and invariants for each new leader
        weakenedInvs.addAll(copyInvsFromLeader(inv.leader(), newInvsLeaders));

        // Keep track of all of the new invariants created.
        allNewInvs.addAll(newInvs);
      }
    }

    // Add all of the new equality sets to our list
    invs.addAll(allNewInvs);

    return weakenedInvs;
  }

  /**
   * Dummy value that's incomparable to everything else to indicate missings in createEqualityInvs.
   */
  private static final Object dummyMissing = new Object();

  /**
   * Create a List of Equality invariants based on the values given by vt for the VarInfos in vis.
   * Any variables that are out of bounds are forced into a separate equality set (since they no
   * longer make sense and certainly shouldn't be equal to anything else)
   *
   * <p>Requires: vis.size() &gt; 0
   *
   * <p>Ensures: result.size() &gt; 0
   *
   * @param vis the VarInfos that were different from leader
   * @param vt the ValueTuple associated with the VarInfos now
   * @param leader the original leader of VarInfos
   * @param count the number of samples seen (needed to set the number of samples for the new
   *     Equality invariants)
   * @return a List of Equality invariants bundling together same values from vis, and if needed,
   *     another representing all the missing values
   */
  private List<Equality> createEqualityInvs(
      List<VarInfo> vis, ValueTuple vt, Equality leader, int count) {
    assert vis.size() > 0;
    HashMap<Object, List<VarInfo>> multiMap = new HashMap<>(); /* key is a value */
    List<VarInfo> out_of_bounds = new ArrayList<>();
    for (VarInfo vi : vis) {
      if (vi.missingOutOfBounds()) {
        out_of_bounds.add(vi);
      } else if (vt.isMissing(vi)) {
        addToBindingList(multiMap, dummyMissing, vi);
      } else {
        if (vi.getValue(vt) == null) {
          System.out.printf(
              "null value for variable %s, mod=%d at ppt %s%n",
              vi.name(), vt.getModified(vi), parent.name());
          VarInfo rv = parent.find_var_by_name("return");
          assert rv != null : "@AssumeAssertion(nullness)";
          System.out.println("return value = " + Debug.toString(rv.getValue(vt)));
          System.out.println("At line number " + FileIO.get_linenum());
        }
        addToBindingList(multiMap, vi.getValue(vt), vi);
      }
    }
    // Why use an array?  Because we'll be sorting shortly
    /*NNC:@MonotonicNonNull*/ Equality[] resultArray =
        new Equality[multiMap.values().size() + out_of_bounds.size()];
    int resultCount = 0;
    for (Map.Entry<@KeyFor("multiMap") Object, List<VarInfo>> entry : multiMap.entrySet()) {
      Object key = entry.getKey();
      List<VarInfo> list = entry.getValue();
      assert list.size() > 0;
      Equality eq = new Equality(list, this);
      @SuppressWarnings("interning") // special value
      boolean isMissing = (key == dummyMissing);
      if (isMissing) {
        eq.setSamples(leader.numSamples() - count);
      } else {
        eq.setSamples(leader.numSamples());
      }
      if (debug.isLoggable(Level.FINE)) {
        debug.fine("  created new inv: " + eq + " samples: " + eq.numSamples());
      }
      resultArray[resultCount] = eq;
      resultCount++;
    }
    for (VarInfo oob : out_of_bounds) {
      List<VarInfo> list = Collections.singletonList(oob);
      resultArray[resultCount] = new Equality(list, this);
      resultCount++;
    }
    resultArray = castNonNullDeep(resultArray); // https://tinyurl.com/cfissue/986

    // Sort for determinism
    Arrays.sort(resultArray, EqualityComparator.theInstance);
    List<Equality> result = Arrays.<Equality>asList(resultArray);
    assert result.size() > 0;
    return result;
  }

  /**
   * Create a List of Equality invariants based on the VarInfos in vis. Assumes that the VarInfos in
   * vis are not missing. The method is used exclusively for reversing optimizations in Daikon.
   *
   * <p>Requires: vis.size() &gt; 0
   *
   * <p>Ensures: result.size() &gt; 0
   *
   * @param vis the VarInfos that were different from leader
   * @param leader the original leader of VarInfos
   * @return a List of Equality invariants bundling together same values from vis
   */
  public List<Equality> createEqualityInvs(List<VarInfo> vis, Equality leader) {
    assert vis.size() > 0;

    // Why use an array?  Because we'll be sorting shortly
    /*NNC:@MonotonicNonNull*/ Equality[] resultArray = new Equality[vis.size()];
    for (int i = 0; i < vis.size(); i++) {
      VarInfo vi = vis.get(i);
      List<VarInfo> list = new ArrayList<>();
      list.add(vi);
      Equality eq = new Equality(list, this);
      eq.setSamples(leader.numSamples());
      resultArray[i] = eq;
    }
    resultArray = castNonNullDeep(resultArray); // https://tinyurl.com/cfissue/986

    // Sort for determinism
    Arrays.sort(resultArray, PptSliceEquality.EqualityComparator.theInstance);
    List<Equality> result = Arrays.<Equality>asList(resultArray);
    assert result.size() > 0;
    return result;
  }

  /**
   * Map maps keys to non-empty lists of elements. This method adds var to the list mapped by key,
   * creating a new list for key if one doesn't already exist.
   *
   * <p>Requires: Each value in map is a list of size 1 or greater
   *
   * <p>Ensures: Each value in map is a list of size 1 or greater
   *
   * @param map the map to add the bindings to
   * @param key if there is already a List associated with key, then add value to key. Otherwise
   *     create a new List associated with key and insert value.
   * @param value the value to insert into the List mapped to key
   */
  private <T> void addToBindingList(Map<T, List<VarInfo>> map, T key, VarInfo value) {
    if (key == null) {
      throw new IllegalArgumentException();
    }
    List<VarInfo> elements = map.computeIfAbsent(key, __ -> new ArrayList<VarInfo>());
    elements.add(value);
  }

  /**
   * Instantiate invariants from each inv's leader. This is like instantiate_invariants at the start
   * of reading the trace file, where we create new PptSliceNs. This is called when newVis have just
   * split off from leader, and we want the leaders of newVis to have the same invariants as leader.
   *
   * <p>post: Adds the newly instantiated invariants and slices to this.parent.
   *
   * @param leader the old leader
   * @param newVis a List of new VarInfos that used to be equal to leader. Actually, it's the list
   *     of canonical that were equal to leader, representing their own newly-created equality sets.
   */
  public List<Invariant> copyInvsFromLeader(VarInfo leader, List<VarInfo> newVis) {

    List<Invariant> falsified_invs = new ArrayList<>();
    List<PptSlice> newSlices = new ArrayList<>();
    if (debug.isLoggable(Level.FINE)) {
      debug.fine(
          "copyInvsFromLeader: "
              + parent.name()
              + ": leader "
              + leader.name()
              + ": new leaders = "
              + newVis);
      debug.fine("  orig slices count:" + parent.numViews());
    }

    // Copy all possible combinations from the current ppt (with repetition)
    // of replacing leader with different members of newVis.

    // Loop through each slice.
    // Uses old-style for loop and Iterator because it will be side-effected.
    for (Iterator<PptSlice> i = parent.views_iterator(); i.hasNext(); ) {
      PptSlice slice = i.next();

      if (debug.isLoggable(Level.FINE)) {
        debug.fine("  Slice is: " + slice.toString());
        debug.fine("  With invs: " + slice.invs);
      }

      // If this slice contains the old leader
      if (slice.containsVar(leader)) {

        // Substitute new leader for old leader and create new slices/invs
        VarInfo[] toFill = new VarInfo[slice.var_infos.length];
        copyInvsFromLeaderHelper(leader, newVis, slice, newSlices, 0, -1, toFill);

        // Remove any statically obvious invariants in the old slice.
        // This is called here because breaking up the equality set may
        // cause some invariants to become statically obvious (because
        // they will now be the only item in their set)
        for (Invariant inv : slice.invs) {
          if (!Daikon.dkconfig_undo_opts) {
            if (inv.isObviousStatically_AllInEquality()) {
              inv.falsify();
              falsified_invs.add(inv);
            }
          }
        }
        if (slice.invs.size() == 0) {
          i.remove();
        }
      }
    }

    // Add each new slice with invariants
    for (PptSlice slice : newSlices) {
      if (slice.invs.size() == 0) {
        continue;
      }
      assert (parent.findSlice(slice.var_infos) == null) : parent.findSlice(slice.var_infos);
      slice.repCheck();
      parent.addSlice(slice);
    }

    parent.repCheck();

    if (debug.isLoggable(Level.FINE)) {
      debug.fine("  new slices count:" + parent.numViews());
    }
    return falsified_invs;
  }

  /**
   * Clones slice (zero or more times) such that instances of leader are replaced by members of
   * newVis; places new slices in newSlices. The replacement is such that we get all combinations,
   * with repetition of newVis and leader in every slot in slice where there used to be leader. For
   * example, if slice contained (A1, A1, B) and A1 is leader and newVis contains A2 and A3, then
   * the slices we produce would be: (A1, A2, B), (A1, A3, B), (A2, A2, B) (A2, A3, B), (A3, A3, B).
   * We do not produce (A1, A1, B) because it is already there. We do not produce (A2, A1, B)
   * because it is the same as (A1, A2, B) wrt combinations. This method does the main work of
   * copyInvsFromLeader so that each new equality set that spawned off leader has the correct
   * slices. It works as a nested series of for loops, whose depth is equal to the length of
   * slice.var_infos. The position and loop arguments along with the call stack keep track of the
   * loop nesting. When position reaches the end of slice.var_infos, this method attempts to
   * instantiate the slice that has been produced. The standard start for position is 0, and for
   * loop is -1.
   *
   * @param leader the variable to replace in slice
   * @param newVis of VarInfos that will replace leader in combination in slice
   * @param slice the slice to clone
   * @param newSlices where to put the cloned slices
   * @param position the position currently being replaced in source. Starts at 0.
   * @param loop the iteration of the loop for this position. If -1, means the previous replacement
   *     is leader.
   * @param soFar buffer to which assignments temporarily go before becoming instantiated. Has to
   *     equal slice.var_infos in length.
   */
  private void copyInvsFromLeaderHelper(
      VarInfo leader,
      List<VarInfo> newVis,
      PptSlice slice,
      List<PptSlice> newSlices,
      int position,
      int loop,
      VarInfo[] soFar) {

    // Track debug if any variables are in newVis
    Debug dlog = null;
    if (Debug.logOn()) {
      dlog = new Debug(getClass(), parent, newVis);
    }

    if (position >= slice.var_infos.length) {
      // Done with assigning positions and recursion
      if (parent.findSlice_unordered(soFar) == null) {
        // If slice is already there, no need to clone.

        if (parent.is_slice_ok(soFar, slice.arity())) {
          PptSlice newSlice = slice.cloneAndPivot(soFar);
          // Debug.debugTrack.fine ("LeaderHelper: Created Slice " + newSlice);
          if (Debug.logOn()) {
            assert dlog != null : "@AssumeAssertion(nullness): dependent: set if Debug.logOn()";
            dlog.log(
                "Created slice " + newSlice + " Leader equality set = " + soFar[0].equalitySet);
            Debug.log(getClass(), newSlice, "Created this slice");
          }
          List<Invariant> invs = newSlice.invs;
          for (Iterator<Invariant> iInvs = invs.iterator(); iInvs.hasNext(); ) {
            Invariant inv = iInvs.next();
            if (!Daikon.dkconfig_undo_opts) {
              if (inv.isObviousStatically_AllInEquality()) {
                iInvs.remove();
              }
            }
          }
          if (newSlice.invs.size() == 0) {
            Debug.log(debug, getClass(), newSlice, soFar, "slice not added because 0 invs");
          } else {
            newSlices.add(newSlice);
          }
        }
      } else {
        if (Debug.logOn()) {
          assert dlog != null : "@AssumeAssertion(nullness): dependent: set if Debug.logOn()";
          dlog.log("Slice already existed " + parent.findSlice_unordered(soFar));
        }
      }
      return;
    } else {
      // Not yet done with recursion, keep assigning to soFar
      if (slice.var_infos[position] == leader) {
        // If leader does need replacing
        // newLoop starts at loop so that we don't have repeats
        for (int newLoop = loop; newLoop < newVis.size(); newLoop++) {
          VarInfo vi = newLoop == -1 ? leader : newVis.get(newLoop);
          soFar[position] = vi;
          // Advance position to next step, let next loop variable be
          // this loop's counter.
          copyInvsFromLeaderHelper(leader, newVis, slice, newSlices, position + 1, newLoop, soFar);
        }
      } else {
        // Non leader position, just keep going after assigning soFar
        soFar[position] = slice.var_infos[position];
        copyInvsFromLeaderHelper(leader, newVis, slice, newSlices, position + 1, loop, soFar);
      }
    }
  }

  @Override
  public void repCheck() {
    for (Invariant inv : invs) {
      inv.repCheck();
      assert inv.ppt == this;
    }
  }

  @SideEffectFree
  @Override
  public String toString(@GuardSatisfied PptSliceEquality this) {
    StringBuilder result = new StringBuilder("PptSliceEquality: [");
    for (Invariant inv : invs) {
      result.append(inv.repr());
      result.append(lineSep);
    }
    result.append("  ]");
    return result.toString();
  }

  /** Order Equality invariants by the indices of leaders. */
  public static final class EqualityComparator implements Comparator<Equality> {
    public static final EqualityComparator theInstance = new EqualityComparator();

    private EqualityComparator() {}

    @Pure
    @Override
    public int compare(Equality eq1, Equality eq2) {
      return VarInfo.IndexComparator.theInstance.compare(eq1.leader(), eq2.leader());
    }
  }

  /** Returns an array of all of the leaders sorted by varinfo_index for this equality view. */
  public VarInfo[] get_leaders_sorted() {
    List<VarInfo> leaders = new ArrayList<>(invs.size());
    for (Invariant inv : invs) {
      VarInfo leader = ((Equality) inv).leader();
      assert leader != null;
      leaders.add(leader);
    }
    Collections.sort(leaders, VarInfo.IndexComparator.getInstance());
    return leaders.toArray(new VarInfo[0]);
  }
}
