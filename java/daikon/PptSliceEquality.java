package daikon;

import daikon.inv.*;

import utilMDE.*;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.*;
import java.text.*;

/**
 * Holds Equality invariants.
 **/
public class PptSliceEquality
  extends PptSlice
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20021231L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.

  /**
   * Create one set for each variable.  This has the effect of turning
   * the equality optimization off, without actually removing the sets
   * themselves (which are presumed to exist in many parts of the code)
   */
  public static boolean dkconfig_set_per_var = false;

  public static final Logger debug =
    Logger.getLogger ("daikon.PptSliceEquality");

  public static final Logger debugGlobal
    = Logger.getLogger ("daikon.PptSliceEquality.Global");

  PptSliceEquality(PptTopLevel parent) {
     super(parent, parent.var_infos);
  }

  public final int arity() {
    throw new Error("Don't call arity on PptSliceEquality");
  }


  void init_po() {
    throw new Error("Shouldn't get called");
  }

  public void addInvariant(Invariant inv) {
    Assert.assertTrue(inv instanceof Equality);
    invs.add(inv);
  }

  // Not valid for this type of slice.  Always pretend there are enough.
  public int num_samples() { if (true) throw new Error(); return Integer.MAX_VALUE; }
  public int num_mod_samples() { if (true) throw new Error(); return Integer.MAX_VALUE; }
  public int num_values() { if (true) throw new Error(); return Integer.MAX_VALUE; }

  /**
   * Encapsulates a VarInfo and its Comparability so that the two can
   * be used to create sets of VarInfos that are initially equal. Two
   * VarInfoAndComparability's are true iff they are
   * VarComparability.comparable() to each other.
   **/
  private static class VarInfoAndComparability {
    public VarInfo vi;

    public int hashCode() {
      // This is about as good as we can do it.  Can't do hashcode of
      // the comparability because two comparabilities may be
      // comparable and yet be not the same
      // (e.g. VarComparabilityExplicit).
      return vi.file_rep_type.hashCode();
    }

    public boolean equals (Object o) {
      if (!(o instanceof VarInfoAndComparability)) return false;
      return equals ((VarInfoAndComparability) o);
    }

    /**
     * Whether two VarInfos can be set to be equal to each other is
     * whether they are comparableNWay.  Since we do not yet handle
     * inheritance, we require that the comptability go both ways.
     **/
    public boolean equals (VarInfoAndComparability o) {

      return (vi.comparableNWay (o.vi)
              && (vi.comparability.equality_set_ok (o.vi.comparability)));
    }

    public VarInfoAndComparability (VarInfo vi) {
      this.vi = vi;
    }

  }

  /**
   * Actually instantiate the equality sets.
   **/
  void instantiate_invariants() {

    // If each variable gets its own set, create those sets and return
    if (dkconfig_set_per_var) {
      // Debug.debugTrack.fine ("Vars for " + parent.name());
      for (int i = 0; i < var_infos.length; i++) {
        VarInfo vi = var_infos[i];
        List vi_list = new ArrayList(1);
        vi_list.add (vi);
        Equality eq = new Equality (vi_list, this);
        invs.add (eq);
        // System.out.println ("  eq set = " + eq.shortString());
      }
      return;
    }

    // Start with everything comparable being equal.
    if (debug.isLoggable(Level.FINE)) {
      debug.fine ("InstantiateInvariants: " + parent.name() + " vars:") ;
    }
    Map multiMap = new LinkedHashMap(); /* comparable -> List[VarInfo]*/
    for (int i = 0; i < var_infos.length; i++) {
      VarInfo vi = var_infos[i];
      VarInfoAndComparability viac = new VarInfoAndComparability(vi);
      addToBindingList (multiMap, viac, vi);
      if (debug.isLoggable(Level.FINE)) {
        debug.fine ("  " + vi.name.name() + ": " + vi.comparability);
      }
    }
    if (debug.isLoggable(Level.FINE)) {
      debug.fine (Integer.toString(multiMap.keySet().size()));
    }
    Equality[] newInvs = new Equality[multiMap.keySet().size()];
    int varCount = 0;
    int invCount = 0;
    for (Iterator i = multiMap.values().iterator(); i.hasNext(); ) {
      List list = (List) i.next();
      varCount += list.size();

      Equality eq = new Equality (list, this);
      newInvs[invCount] = eq;
      if (debug.isLoggable(Level.FINE)) {
        debug.fine (" Created: " + eq);
      }
      if (Debug.logOn())
        Debug.log (getClass(), parent, Debug.vis (eq.leader()), "Created");
      invCount ++;
    }
    // Ensure determinism
    Arrays.sort (newInvs, EqualityComparator.theInstance);
    invs.addAll (Arrays.asList (newInvs));
    Assert.assertTrue (varCount == var_infos.length); // Check that we get all vis
  }

  /**
   * Instantiate the full equality sets from a set of variable pairs where
   * each member of a pair is equal to the other.
   */

  public void instantiate_from_pairs (Set /* VarInfo.Pairs */ eset) {

    // Build a map from each variable to all those that are equal to it
    Map varmap = new LinkedHashMap();
    Map sample_cnt_map = new LinkedHashMap();
    for (Iterator i = eset.iterator(); i.hasNext(); ) {
      VarInfo.Pair cp = (VarInfo.Pair) i.next();
      ArrayList vlist = (ArrayList) varmap.get (cp.v1);
      if (vlist == null) {
        vlist = new ArrayList();
        vlist.add (cp.v1);
        varmap.put (cp.v1, vlist);
        sample_cnt_map.put (cp.v1, new Integer(cp.samples));
      }
      vlist.add (cp.v2);
      vlist = (ArrayList) varmap.get (cp.v2);
      if (vlist == null) {
        vlist = new ArrayList();
        vlist.add (cp.v2);
        varmap.put (cp.v2, vlist);
        sample_cnt_map.put (cp.v2, new Integer(cp.samples));
      }
      vlist.add (cp.v1);
    }

    // Loop through each variable, building the appropriate equality set
    // for each.  Note that variables that are distinct still have an
    // equality set (albeit with only the one variable)
    ArrayList newInvs = new ArrayList();
    for (int i = 0; i < var_infos.length; i++) {
      VarInfo v = var_infos[i];
      if (v.equalitySet != null)
        continue;
      ArrayList vlist = (ArrayList) varmap.get (v);
      if (vlist == null) {
        vlist = new ArrayList(1);
        vlist.add (v);
      }
      Equality eq = new Equality (vlist, this);
      Integer sample_cnt = (Integer) sample_cnt_map.get (v);
      if (sample_cnt != null)
        eq.setSamples (sample_cnt.intValue());
      v.equalitySet = eq;
      newInvs.add (eq);
    }
    invs.addAll (newInvs);
  }

  /**
   * Returns a List of Invariants that have been weakened/destroyed.
   * However, this handles the creation of new Equality invariants and
   * the instantiation of other invariants.
   * @return a List of invariants that have been weakened
   **/
  // The basic approach is as follows:
  //    - Loop through each equality set
  //        - look for any variables that are no longer equal
  //        - Create new equality sets (call createEqualityInvs)
  //        - Get the new leaders
  //        - Create new slices and invariants (call CopyInvsFromLeader)
  //
  public List/*Invariant*/  add(ValueTuple vt, int count) {

    LinkedList /*[Equality]*/ allNewInvs = new LinkedList();
    LinkedList /*Invariant*/ weakenedInvs = new LinkedList();

    // Loop through each existing equality invariant
    for (Iterator i = invs.iterator(); i.hasNext(); ) {
      Equality inv = (Equality) i.next();

      // Add this sample to the invariant and track any vars that fall
      // out of the set.
      List/*[VarInfo]*/ nonEqualVis = inv.add (vt, count);

      // If some vars fell out
      if (nonEqualVis.size() > 0) {

        // Create new equality sets for all of the non-equal vars
        List /*[Equality]*/ newInvs =
          createEqualityInvs (nonEqualVis, vt, inv, count);

        // Get a list of all of the new non-missing leaders
        List newInvsLeaders = new ArrayList (newInvs.size());
        for (Iterator iNewInvs = newInvs.iterator(); iNewInvs.hasNext(); ) {
          Equality eq = (Equality) iNewInvs.next();
          if ((parent.constants == null) || !parent.constants.is_missing (eq.leader()))
            newInvsLeaders.add (eq.leader());
        }

        //Debug print the new leaders
        if (Debug.logOn()) {
          for (int j = 0; j < newInvsLeaders.size(); j++) {
            Debug.log (getClass(), parent,
                       Debug.vis ((VarInfo) newInvsLeaders.get(j)),
              "Split off from previous leader " + inv.leader().name.name()
              + ": new set = " + ((VarInfo) newInvsLeaders.get(j)).equalitySet
              + ": old set = " + inv);
          }
        }

        // Create new slices and invariants for each new leader
        weakenedInvs.addAll (copyInvsFromLeader (inv.leader(),newInvsLeaders));

        // Keep track of all of the new invariants created.
        allNewInvs.addAll (newInvs);
      }
    }

    // Add all of the new equality sets to our list
    invs.addAll (allNewInvs);

    return weakenedInvs;
  }

  /**
   * Dummy value that's incomparable to everything else to indicate
   * missings in createEqualityInvs
   **/
  private static final Object dummyMissing = new StringBuffer("Dummy missing");

  /**
   * Create a List of Equality invariants based on the values given
   * by vt for the VarInfos in vis.  Any variables that are out
   * of bounds are forced into a separate equality set (since they
   * no longer make sense and certainly shouldn't be equal to anything
   * else)
   * @param vis The VarInfos that were different from leader
   * @param vt The ValueTuple associated with the VarInfos now
   * @param leader The original leader of VarInfos
   * @param count The number of samples seen (needed to set the number
   * of samples for the new Equality invariants)
   * @return a List of Equality invariants bundling together same
   * values from vis, and if needed, another representing all the
   * missing values.
   * @pre vis.size() > 0
   * @post result.size() > 0
   **/
  private List/*[Equality]*/ createEqualityInvs (List vis, ValueTuple vt,
                                                 Equality leader, int count
                                                 ) {
    Assert.assertTrue (vis.size() > 0);
    Map multiMap = new HashMap(); /* value -> List[VarInfo]*/
    List out_of_bounds = new ArrayList();
    for (Iterator i = vis.iterator(); i.hasNext(); ) {
      VarInfo vi = (VarInfo) i.next();
      if (vi.missingOutOfBounds())
        out_of_bounds.add (vi);
      else if (vt.isMissing (vi)) {
        addToBindingList (multiMap, dummyMissing, vi);
      } else {
        addToBindingList (multiMap, vi.getValue(vt), vi);
      }
    }
    // Why use an array?  Because we'll be sorting shortly
    Equality[] resultArray = new Equality[multiMap.values().size()
                                          + out_of_bounds.size()];
    int resultCount = 0;
    for (Iterator i = multiMap.keySet().iterator(); i.hasNext(); ) {
      Object key = i.next();
      List list = (List) multiMap.get (key);
      Assert.assertTrue (list.size() > 0);
      Equality eq = new Equality (list, this);
      if (key == dummyMissing) {
        eq.setSamples (leader.numSamples() - count);
      } else {
        eq.setSamples (leader.numSamples());
      }
      if (debug.isLoggable(Level.FINE)) {
        debug.fine ("  created new inv: " + eq + " samples: " + eq.numSamples());
      }
      resultArray[resultCount] = eq;
      resultCount++;
    }
    for (int i = 0; i < out_of_bounds.size(); i++) {
      List list = new LinkedList();
      list.add (out_of_bounds.get (i));
      resultArray[resultCount] = new Equality (list, this);
      resultCount++;
    }

    // Sort for determinism
    Arrays.sort (resultArray, EqualityComparator.theInstance);
    List result = Arrays.asList (resultArray);
    Assert.assertTrue (result.size() > 0);
    return result;
  }

  /**
   * Map maps keys to non-empty lists of elements.
   * This method adds var to the list mapped by key,
   * creating a new list for key if one doesn't already exist.
   * @param map The map to add the bindings to
   * @param key If there is already a List associated with key, then
   * add value to key.  Otherwise create a new List associated with
   * key and insert value.
   * @param value The value to insert into the List mapped to key.
   * @pre Each value in map is a list of size 1 or greater
   * @post Each value in map is a list of size 1 or greater
   **/
  private void addToBindingList (Map map, Object key, VarInfo value)
  {
    Assert.assertTrue (key != null);
    List elements = (List) map.get(key);
    if (elements == null) {
      elements = new LinkedList();
      map.put (key, elements);
    }
    elements.add (value);
  }

  /**
   * Instantiate invariants from each inv's leader.  This is like
   * instantiate_invariants at the start of reading the trace file,
   * where we create new PptSliceNs.  This is called when newVis have
   * just split off from leader, and we want the leaders of newVis to
   * have the same invariants as leader.
   * @param leader the old leader
   * @param newVis a List of new VarInfos that used to be equal to
   * leader.  Actually, it's the list of canonical that were equal to
   * leader, representing their own newly-created equality sets.
   * @post Adds the newly instantiated invariants and slices to
   * this.parent.
   **/
  private List /*Invariant*/ copyInvsFromLeader (VarInfo leader, List newVis) {


    List falsified_invs = new ArrayList();
    List newSlices = new LinkedList();
    if (debug.isLoggable(Level.FINE)) {
      debug.fine ("copyInvsFromLeader: " + parent.name() + ": leader "
                  + leader.name.name()
                  + ": new leaders = " + VarInfo.toString (newVis));
      debug.fine ("  orig slices count:" + parent.views_size());
    }

    // Copy all possible combinations from the current ppt (with repetition)
    // of replacing leader with different members of newVis.

    // Loop through each slice
    for (Iterator i = parent.views_iterator(); i.hasNext(); ) {
      PptSlice slice = (PptSlice) i.next();

      if (debug.isLoggable(Level.FINE)) {
        debug.fine ("  Slice is: " + slice.toString());
        debug.fine ("  With invs: " + slice.invs);
      }

      // If this slice contains the old leader
      if (slice.containsVar(leader)) {

        // Substitute new leader for old leader and create new slices/invs
        VarInfo[] toFill = new VarInfo[slice.var_infos.length];
        copyInvsFromLeaderHelper (leader, newVis, slice, newSlices,
                                  0, -1, toFill);

        // Remove any statically obvious invariants in the old slice.
        // This is called here because breaking up the equality set may
        // cause some invariants to become statically obvious (because
        // they will now be the only item in their set)
        for (Iterator j = slice.invs.iterator(); j.hasNext(); ) {
          Invariant inv = (Invariant) j.next();
          if (inv.isObviousStatically_AllInEquality()) {
            //            inv.destroyAndFlow();
            inv.falsify();
            falsified_invs.add (inv);
            if (!Daikon.dkconfig_df_bottom_up) {
              inv.ppt.addToChanged(inv);
              inv.ppt.addToFlow(inv);
            }
          }
        }
        if (slice.invs.size() == 0) i.remove();
      }
    }

    // Add each new slice with invariants
    for (Iterator itor = newSlices.iterator(); itor.hasNext(); ) {
      PptSlice slice = (PptSlice) itor.next();
      if (slice.invs.size() == 0) {
        continue;
      }
      Assert.assertTrue (parent.findSlice (slice.var_infos) == null);
      slice.repCheck();
      parent.addSlice (slice);
    }

    // Copy any invariants from the global ppt to here
    copy_invs_from_global_leader (leader, newVis);

    parent.repCheck();

    if (debug.isLoggable(Level.FINE)) {
      debug.fine ("  new slices count:" + parent.views_size());
    }
    return (falsified_invs);
  }

  /**
   * Clones slice (zero or more times) such that instances of leader
   * are replaced by members of newVis; places new slices in
   * newSlices.  The replacement is such that we get all combinations,
   * with repetition of newVis and leader in every slot in slice where
   * there used to be leader.  For example, if slice contained (A1,
   * A1, B) and A1 is leader and newVis contains A2 and A3, then the
   * slices we produce would be: (A1, A2, B), (A1, A3, B), (A2, A2, B)
   * (A2, A3, B), (A3, A3, B).  We do not produce (A1, A1, B) because
   * it is already there.  We do not produce (A2, A1, B) because it is
   * the same as (A1, A2, B) wrt combinations.  This method does the
   * main work of copyInvsFromLeader so that each new equality set
   * that spawned off leader has the correct slices.  It works as a
   * nested series of for loops, whose depth is equal to the length of
   * slice.var_infos.  The position and loop arguments along with the
   * call stack keep track of the loop nesting.  When position reaches
   * the end of slice.var_infos, this method attempts to instantiate
   * the slice that has been produced.  The standard start for
   * position is 0, and for loop is -1.
   * @param leader The variable to replace in slice
   * @param newVis of VarInfos that will replace leader in combination in slice
   * @param slice The slice to clone
   * @param newSlices Where to put the cloned slices
   * @param position The position currently being replaced in source.  Starts at 0.
   * @param loop The iteration of the loop for this position.  If -1,
   * means the previous replacement is leader.
   * @param soFar Buffer to which assignments temporarily go before
   * becoming instantiated.  Has to equal slice.var_infos in length.
   **/
  private void copyInvsFromLeaderHelper (VarInfo leader, List newVis,
                                         PptSlice slice, List newSlices,
                                         int position, int loop,
                                         VarInfo[] soFar) {

    // Track debug if any variables are in newVis
    Debug dlog = null;
    if (Debug.logOn())
      dlog = new Debug (getClass(), parent, newVis);

    if (position >= slice.var_infos.length) {
      // Done with assigning positions and recursion
      if (parent.findSlice_unordered (soFar) == null) {
        // If slice is already there, no need to clone.

        if (parent.is_slice_ok (soFar, slice.arity())) {
          PptSlice newSlice = slice.cloneAndPivot(soFar);
          // Debug.debugTrack.fine ("LeaderHelper: Created Slice " + newSlice);
          if (Debug.logOn()) {
            dlog.log ("Created slice " + newSlice + " Leader equality set = "
                      + soFar[0].equalitySet);
            Debug.log (getClass(), newSlice, "Created this slice");
          }
          List invs = newSlice.invs;
          for (Iterator iInvs = invs.iterator(); iInvs.hasNext(); ) {
            Invariant inv = (Invariant) iInvs.next();
            if (inv.isObviousStatically_AllInEquality()) {
              iInvs.remove();
            }
          }
          if (newSlice.invs.size() == 0) {
            Debug.log (debug, getClass(), newSlice, soFar,
                       "slice not added because 0 invs");
          } else {
            newSlices.add (newSlice);
          }
        }
      } else {
        if (Debug.logOn())
          dlog.log ("Slice already existed " +
                    parent.findSlice_unordered (soFar));
      }
      return;
    } else {
      // Not yet done with recursion, keep assigning to soFar
      if (slice.var_infos[position] == leader) {
        // If leader does need replacing
        // newLoop starts at loop so that we don't have repeats
        for (int newLoop = loop; newLoop < newVis.size(); newLoop++) {
          VarInfo vi = newLoop == -1 ? leader : (VarInfo) newVis.get(newLoop);
          soFar[position] = vi;
          // Advance position to next step, let next loop variable be
          // this loop's counter.
          copyInvsFromLeaderHelper (leader, newVis, slice, newSlices,
                                    position + 1, newLoop, soFar);
        }
      } else {
        // Non leader position, just keep going after assigning soFar
        soFar[position] = slice.var_infos[position];
          copyInvsFromLeaderHelper (leader, newVis, slice, newSlices,
                                    position + 1, loop, soFar);
      }
    }
  }

  /**
   * Copy invariants from global ppt slices over old_leader to each of
   * the new leaders at the local ppt.  The basic approach is as follows:
   *
   *    1)  Loop over each global slice that contains old_leader
   *
   *    2)  Copy each invariant that doesn't already exist to local
   *        slices for each combination of new_leaders replacing old_leader.
   *
   *        If the local slice doesn't already exist, it is created and
   *        added to the list of slices.
   */
  public void copy_invs_from_global_leader (VarInfo old_leader,
                                            List /*VarInfo*/ new_leaders) {

    if (Debug.logDetail())
      debugGlobal.fine ("old_leader = " + old_leader.name.name() +
                        " orig new leaders = " + new_leaders);

    // If the old leader is not a global, there is nothing to copy
    if (!old_leader.is_global())
      return;

    // The below was removed because we always need to copy from
    // an old_global_leader.  This is necessary because equality sets
    // can be different from local ppt to global ppt and thus the
    // invariants from one global to another should be different

    // We need  to copy global invariants only if the new leader is a
    // local or it is a global with a different (post/orig) transformation
    // the the old leader.  We build a new list of new_leaders containing
    // only these variables
//     List orig_new_leaders = new_leaders;
//     new_leaders = new ArrayList ();
//     for (Iterator i = orig_new_leaders.iterator(); i.hasNext(); ) {
//       VarInfo new_leader = (VarInfo) i.next();
//       if (!new_leader.is_global()
//           || (old_leader.is_post_global() != new_leader.is_post_global()))
//         new_leaders.add (new_leader);
//     }

    // If there are no new leaders, there is nothing to do
    if (new_leaders.size() == 0)
      return;

    // Get the leader var at the global ppt
    VarInfo old_leader_global = old_leader.global_var();
    boolean post_xform = old_leader.is_post_global();

    debugGlobal.fine ("old_leader = " + old_leader.name.name() +
                      " new leaders = " + new_leaders);

    // Loop through each slice at the global ppt that includes old_leader
    for (Iterator j = PptTopLevel.global.views_iterator(); j.hasNext(); ) {
      PptSlice gslice = (PptSlice) j.next();
      if (!gslice.containsVar (old_leader_global))
        continue;

      // Create a local version of this slices variables
      VarInfo[] local_vars = new VarInfo[gslice.var_infos.length];
      for (int i = 0; i < gslice.var_infos.length; i++) {
        if (post_xform)
          local_vars[i] = parent.local_postvar (gslice.var_infos[i]);
        else
          local_vars[i] = parent.local_origvar (gslice.var_infos[i]);
      }

      // Create all of the combinations of new_leaders over this slice
      List combos = sub_leaders (old_leader, new_leaders, local_vars);

      // Loop over each combination
      for (Iterator i = combos.iterator(); i.hasNext(); ) {
        VarInfo[] vis = (VarInfo[]) i.next();

        // Copy each invariant at gslice to the slice defined by vis
        gslice.copy_new_invs (parent, vis);

      }
    }
  }

  /**
   * Substitute each combination of new_leaders in vis for old_leader with
   * repetition but not permutation.  The new vis arrays are NOT sorted
   * but left as they are after substitution (which may or may not be in
   * varinfo_index order)
   */
  public List /* VarInfo[] */ sub_leaders (VarInfo old_leader,
                                           List /* VarInfo */ new_leaders,
                                           VarInfo[] vis) {

    List result = new ArrayList();

    // Determine the number of instances of old_leader in vis
    int cnt = 0;
    for (int i = 0; i < vis.length; i++)
      if (vis[i] == old_leader)
        cnt++;

    // The full list of variables whose combination is needed includes
    // old_leader.  For example, if new_leaders = {b, c} and old_leader = a
    // we need to create combinations like {a, b} and {a, c}.
    List /* VarInfo */ leaders = new ArrayList();
    leaders.add (old_leader);
    leaders.addAll (new_leaders);

    // Build all of the combinations of leaders over the number of times
    // that old_leader appeared in the slice.  This will also include
    // {a, a} (from the above example), but we will skip that one below.
    List combos = UtilMDE.create_combinations (cnt, 0, leaders);

    // Loop through each combination and build a corresponding vis array
    // Skip the first combination which will always include just old_leader.
    for (int i = 1; i < combos.size(); i++) {
      List combo = (List) combos.get(i);
      VarInfo[] newvis = new VarInfo[vis.length];
      int offset = 0;
      for (int j = 0; j < newvis.length; j++) {
        if (vis[j] == old_leader) {
          newvis[j] = (VarInfo) combo.get(offset);
          offset++;
        } else {
          newvis[j] = vis[j];
        }
      }
      result.add (newvis);
    }

  return (result);
  }


  public void repCheck() {
    for (Iterator i = invs.iterator(); i.hasNext(); ) {
      Invariant inv = (Invariant) i.next();
      inv.repCheck();
      Assert.assertTrue (inv.ppt == this);
    }
  }

  public String toString() {
    StringBuffer result = new StringBuffer("PptSliceEquality: [");
    for (Iterator i = invs.iterator(); i.hasNext(); ) {
      Equality inv = (Equality) i.next();
      result.append (inv.repr());
      result.append ("\n");
    }
    result.append ("  ]");
    return result.toString();
  }

  /**
   * Order Equality invariants by the indices of leaders.
   **/
  public static class EqualityComparator implements Comparator {
    public static final EqualityComparator theInstance = new EqualityComparator();
    private EqualityComparator() {

    }
    public int compare(Object o1,
                       Object o2) {
      Equality eq1 = (Equality) o1;
      Equality eq2 = (Equality) o2;
      return VarInfo.IndexComparator.theInstance.compare (eq1.leader(), eq2.leader());
    }

  }

}
