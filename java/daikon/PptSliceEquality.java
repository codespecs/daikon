package daikon;

import daikon.inv.*;

import utilMDE.*;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.*;

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

  public static final Logger debug =
    Logger.getLogger ("daikon.PptSliceEquality");

  PptSliceEquality(PptTopLevel parent) {
     super(parent, parent.var_infos);
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
  public int num_mod_non_missing_samples() { if (true) throw new Error(); return Integer.MAX_VALUE; }
  public int num_values() { if (true) throw new Error(); return Integer.MAX_VALUE; }
  public String tuplemod_samples_summary() {
    throw new Error();
  }

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
      return
        vi.comparableNWay (o.vi) &&
        VarComparability.comparable (vi.comparability, o.vi.comparability);
    }

    public VarInfoAndComparability (VarInfo vi) {
      this.vi = vi;
    }

  }

  /**
   * Actually instantiate the equality sets.
   **/
  void instantiate_invariants() {
    // Start with everything comparable being equal.
    if (debug.isLoggable(Level.FINE)) {
      debug.fine ("InstantiateInvariants: " + parent.ppt_name + " vars:") ;
    }
    Map multiMap = new HashMap(); /* comparable -> List[VarInfo]*/
    for (int i = 0; i < var_infos.length; i++) {
      VarInfo vi = var_infos[i];
      VarInfoAndComparability viac = new VarInfoAndComparability(vi);
      addToBindingList (multiMap, viac, vi);
      if (debug.isLoggable(Level.FINE)) {
        debug.fine ("  " + vi.name.name());
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
      invCount ++;
    }
    // Ensure determinism
    Arrays.sort (newInvs, EqualityComparator.theInstance);
    invs.addAll (Arrays.asList (newInvs));
    Assert.assertTrue (varCount == var_infos.length); // Check that we get all vis
  }

  /**
   * Returns a List of Invariants that have been weakened/destroyed.
   * However, this handles the creation of new Equality invariants and
   * the instantiation of other invariants.
   * @return a List of invariants that have been weakened
   **/
  public List add(ValueTuple vt, int count) {
    LinkedList /*[Equality]*/ allNewInvs = new LinkedList();
    LinkedList /*[Equality]*/ weakenedInvs = new LinkedList();
    if (debug.isLoggable(Level.FINE)) {
      debug.fine ("Doing add for " + parent.ppt_name + " count: " +
                   count + " starting invs:");
      for (Iterator i = invs.iterator(); i.hasNext(); ) {
        Equality inv = (Equality) i.next();
        debug.fine ("  " + inv.toString());
      }
    }
    for (Iterator i = invs.iterator(); i.hasNext(); ) {
      Equality inv = (Equality) i.next();
      List/*[VarInfo]*/ nonEqualVis = inv.add (vt, count);
      if (nonEqualVis.size() > 0) {
        if (debug.isLoggable(Level.FINE)) {
          debug.fine ("  Unequal VarInfos split off from " +
                       inv + " with count " + inv.numSamples());
          debug.fine ("  leader value: " +
                       ValueTuple.valToString(inv.leader().getValue(vt)));
          for (Iterator j = nonEqualVis.iterator(); j.hasNext(); ) {
            VarInfo vi = (VarInfo) j.next();
            debug.fine ("  " + vi.name.name() +
                         " value: " + ValueTuple.valToString(vi.getValue(vt)) +
                         " mod: " + vi.getModified(vt)
                         );
          }
        }
        // At this point, VarInfos in nonEqualVis still have their
        // equality field set to their old sets
        List /*[Equality]*/ newInvs =
          createEqualityInvs (nonEqualVis, vt, inv, count);
        // Now they don't anymore
        List newInvsLeaders = new ArrayList (newInvs.size());
        for (Iterator iNewInvs = newInvs.iterator(); iNewInvs.hasNext(); ) {
          Equality eq = (Equality) iNewInvs.next();
          newInvsLeaders.add (eq.leader());
        }
        copyInvsFromLeader (inv.leader(), newInvsLeaders, count);
        allNewInvs.addAll (newInvs);

        weakenedInvs.add (inv);
      }
    }
    invs.addAll (allNewInvs);

    if (debug.isLoggable(Level.FINE)) {
      debug.fine ("Finished add for " + parent.ppt_name);
    }
    return weakenedInvs;
  }

  /**
   * Dummy value that's incomparable to everything else to indicate
   * missings in createEqualityInvs
   **/
  private static final Object dummyMissing = new StringBuffer("Dummy missing");

  /**
   * Create a List of Equality invariants based on the values given
   * by vt for the VarInfos in vis.
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
    for (Iterator i = vis.iterator(); i.hasNext(); ) {
      VarInfo vi = (VarInfo) i.next();
      if (vt.isMissing (vi)) {
        addToBindingList (multiMap, dummyMissing, vi);
      } else {
        addToBindingList (multiMap, vi.getValue(vt), vi);
      }
    }
    // Why use an array?  Because we'll be sorting shortly
    Equality[] resultArray = new Equality[multiMap.values().size()];
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
   * @param leaderEq the Equality whose leader holds the
   * invariants to be instantiated(copied).
   * @param newVis a List of new VarInfos that used to be equal to
   * leader.  Actually, it's the list of canonical that were equal to
   * leader, representing their own newly-created equality sets.
   * @post Adds the newly instantiated invariants and slices to
   * this.parent.
   **/
  private void copyInvsFromLeader (VarInfo leader, List newVis, int count) {
    List newSlices = new LinkedList();
    if (debug.isLoggable(Level.FINE)) {
      debug.fine ("copyInvsFromLeader  leader:" + leader.name.name());
      debug.fine ("  orig slices count:" + parent.views_size());
    }
    int newSamples = leader.equalitySet.numSamples() - count;

    // Copy all possible combinations (with repetition) of replacing
    // leader with different members of newVis.
    for (Iterator i = parent.views_iterator(); i.hasNext(); ) {
      PptSlice slice = (PptSlice) i.next();
      // For each slice that contains leader
      if (debug.isLoggable(Level.FINE)) {
        debug.fine ("  Slice is: " + slice.toString());
        debug.fine ("  With invs: " + slice.invs);
      }

      if (slice.containsVar(leader)) {
        VarInfo[] toFill = new VarInfo[slice.var_infos.length];
        copyInvsFromLeaderHelper (leader, newVis, slice, newSlices,
                                  0, -1, toFill);
        for (Iterator iSliceInvs = slice.invs.iterator(); iSliceInvs.hasNext(); ) {
          Invariant inv = (Invariant) iSliceInvs.next();
          if (inv.isObviousStatically_AllInEquality()) {
            inv.destroyAndFlow();
          }
        }
        if (slice.invs.size() == 0) i.remove();
      }
    }

    for (Iterator itor = newSlices.iterator(); itor.hasNext(); ) {
      PptSlice slice = (PptSlice) itor.next();
      if (slice.invs.size() == 0) {
        continue;
      }
      Assert.assertTrue (parent.findSlice (slice.var_infos) == null);
      slice.repCheck();
      parent.addSlice (slice);
    }
    parent.repCheck();

    if (debug.isLoggable(Level.FINE)) {
      debug.fine ("  new slices count:" + parent.views_size());
    }
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
    if (position >= slice.var_infos.length) {
      // Done with assigning positions and recursion
      if (parent.findSlice_unordered (soFar) == null) {
        // If slice is already there, no need to clone.
        PptSlice newSlice = slice.cloneAndPivot(soFar);
        List invs = newSlice.invs;
        if (debug.isLoggable(Level.FINE)) {
          debug.fine ("  created new slice: " + newSlice.toString());
        }
        for (Iterator iInvs = invs.iterator(); iInvs.hasNext(); ) {
          Invariant inv = (Invariant) iInvs.next();
          if (inv.isObviousStatically_AllInEquality()) {
            iInvs.remove();
          }
        }
        if (newSlice.invs.size() == 0) {
          debug.fine ("  slice not added because 0 invs");
        } else {
          newSlices.add (newSlice);
        }
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
