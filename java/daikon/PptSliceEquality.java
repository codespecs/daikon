package daikon;

import daikon.inv.*;

import utilMDE.*;
import org.apache.log4j.Category;
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

  public static final Category debug =
    Category.getInstance ("daikon.PptSliceEquality");

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
   * @param excludeEquality Not used here.
   **/
  void instantiate_invariants(boolean excludeEquality) {
    // Start with everything comparable being equal.
    if (debug.isDebugEnabled()) {
      debug.debug ("InstantiateInvariants: " + parent.ppt_name + " vars:") ;
    }
    Map multiMap = new HashMap(); /* comparable -> List[VarInfo]*/
    for (int i = 0; i < var_infos.length; i++) {
      VarInfo vi = var_infos[i];
      VarInfoAndComparability viac = new VarInfoAndComparability(vi);
      addToBindingList (multiMap, viac, vi);
      if (debug.isDebugEnabled()) {
        debug.debug ("  " + vi.name.name());
      }
    }
    if (debug.isDebugEnabled()) {
      debug.debug (new Integer(multiMap.keySet().size()));
    }
    Equality[] newInvs = new Equality[multiMap.keySet().size()];
    int varCount = 0;
    int invCount = 0;
    for (Iterator i = multiMap.values().iterator(); i.hasNext(); ) {
      List list = (List) i.next();
      varCount += list.size();

      Equality eq = new Equality (list, this);
      newInvs[invCount] = eq;
      if (debug.isDebugEnabled()) {
        debug.debug (" Created: " + eq);
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
    if (debug.isDebugEnabled()) {
      debug.debug ("Doing add for " + parent.ppt_name + " count: " +
                   count + " starting invs:");
      for (Iterator i = invs.iterator(); i.hasNext(); ) {
        Equality inv = (Equality) i.next();
        debug.debug ("  " + inv.toString());
      }
    }
    for (Iterator i = invs.iterator(); i.hasNext(); ) {
      Equality inv = (Equality) i.next();
      List/*[VarInfo]*/ nonEqualVis = inv.add (vt, count);
      if (nonEqualVis.size() > 0) {
        if (debug.isDebugEnabled()) {
          debug.debug ("  Unequal VarInfos split off from " +
                       inv + " with count " + inv.numSamples());
          debug.debug ("  leader value: " +
                       ValueTuple.valToString(inv.leader().getValue(vt)));
          for (Iterator j = nonEqualVis.iterator(); j.hasNext(); ) {
            VarInfo vi = (VarInfo) j.next();
            debug.debug ("  " + vi.name.name() +
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

    if (debug.isDebugEnabled()) {
      debug.debug ("Finished add for " + parent.ppt_name);
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
      if (debug.isDebugEnabled()) {
        debug.debug ("  created new inv: " + eq + " samples: " + eq.numSamples());
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
   **/
  public void copyInvsFromLeader (VarInfo leader, List newVis, int count) {
    List newSlices = new LinkedList();
    if (debug.isDebugEnabled()) {
      debug.debug ("copyInvsFromLeader  leader:" + leader.name.name());
      debug.debug ("  orig slices count:" + parent.views_size());
    }
    int newSamples = leader.equalitySet.numSamples() - count;

    // Three phases: all variables used are *leaders* of their own
    // groups. Phase (a) copies existing slices from old
    // leader. Phases (b) and (c) create new slices such that they
    // contain at least two variables from the set {leader + newVis}.

    // a) Slices from substituting the leader with each of the new
    // variables: f(leader, x) => f(new, x)
    // This handles all unary invariants and some of the bi/ternary
    // invariants we want to copy.  The copied slices are of any
    // arity, but contain exactly one varInfo that is a new leader and
    // no varInfo that's the old leader.

    for (Iterator i = parent.views_iterator(); i.hasNext(); ) {
      PptSlice slice = (PptSlice) i.next();
      // For each slice that contains leader
      if (slice.containsVar(leader)) {
        // For each new leader group, clone over the slice
        for (Iterator iNewVis = newVis.iterator(); iNewVis.hasNext(); ) {
          VarInfo newLeader = (VarInfo) iNewVis.next();
          Assert.assertTrue (newLeader.comparableNWay(leader));
          PptSlice newSlice = slice.cloneOnePivot(leader, newLeader);
          if (debug.isDebugEnabled()) {
            debug.debug ("  result of cloneAndInvs: orig:" + slice);
            debug.debug ("                          new :" + newSlice);
            for (Iterator iDebug = newSlice.invs.iterator();
                 iDebug.hasNext(); ) {
              Invariant inv = (Invariant) iDebug.next();
              debug.debug ("  " + inv.repr());
            }
          }
          newSlice.repCheck();
          newSlices.add (newSlice);
        }
      }
    }


    // b) Do binary slices to produce the cartesian product of
    // {newVis + leader}.  E.g. f(new1, new2), f(new1, leader)
    if (debug.isDebugEnabled()) {
      debug.debug ("Doing binary views");
    }
    for (int i1=-1; i1 < newVis.size(); i1++) {
      VarInfo var1 = (i1 == -1) ?
        leader : (VarInfo) newVis.get(i1);
      for (int i2= i1 + 1; i2 < newVis.size(); i2++) {
        VarInfo var2 = (VarInfo) newVis.get(i2);

        // Why sort?  Because the order of the new invariants don't
        // have to be ordered by index, while slices have to be.
        PptSlice2 slice2 =
          (var1.varinfo_index < var2.varinfo_index) ?
          new PptSlice2(this.parent, var1, var2) :
          new PptSlice2(this.parent, var2, var1);
        Assert.assertTrue (parent.findSlice_unordered (var1, var2) == null);
        slice2.instantiate_invariants(newSamples > 0);
        slice2.repCheck();
        slice2.set_samples(newSamples);
        newSlices.add(slice2);
        if (debug.isDebugEnabled()) {
          debug.debug ("  adding new Slice2: " + slice2);
        }

      }
    }

    Set newVisSet = new HashSet (newVis); // This is for fast lookup later
    newVisSet.add (leader);
    // c) Create ternary slices of at least 2 variables that are in
    // the set {leader + newVis}.  Implementation: the first two
    // loops iterate over the {old + new leader} set to generate (b).
    // The third loop iterates over all the canonical variables,
    // including the old leader, the new leaders and other leaders
    // that were not in the original equality set.  That includes:
    // f(new1, new2, new3) and f(leader, new1, new2) f(new1, new2,
    // other1), f(leader, new1, other2).  The loops are structured to
    // prevent duplication of the same slice, so the inner two loops,
    // for example, do not contain the old leader.  Why do we include
    // old variables in the 3rd slot?  Because this achieves the right
    // cover of the varInfos so we don't miss any ternary slices.
    if (debug.isDebugEnabled()) {
      debug.debug ("Doing ternary views");
    }
    for (int i1=-1; i1 < newVis.size(); i1++) {
      VarInfo var1 = (i1 == -1) ?
        leader : (VarInfo) newVis.get(i1);
      for (int i2= i1 + 1; i2 < newVis.size(); i2++) {
        VarInfo var2 = (VarInfo) newVis.get(i2);
        // The third loop is split into two for performance.  The
        // first is for the vis in newVis.  The second is for the
        // other variables.  This ensures cover and prevents
        // duplication.

        for (int i3= i2 + 1; i3 < newVis.size(); i3++) {
          VarInfo var3 = (VarInfo) newVis.get(i3);
          VarInfo[] vars = new VarInfo[] {var1, var2, var3};
          Arrays.sort (vars, VarInfo.IndexComparator.theInstance);
          PptSlice3 slice3 = new PptSlice3(this.parent, vars);
          slice3.instantiate_invariants(newSamples > 0);
          slice3.repCheck();
          slice3.set_samples(newSamples);
          newSlices.add(slice3);
          if (debug.isDebugEnabled()) {
            debug.debug ("  adding new Slice3: " + slice3 + "from: ");
            debug.debug (var1.name.name() + ", " +
                         var2.name.name() + ", " +
                         var3.name.name());
          }
        }

        for (int i3 = 0; i3 < parent.var_infos.length; i3++) {
          VarInfo var3 = parent.var_infos[i3];
          if (var3 == var2 || var3 == var1) continue;
          if (!var3.isCanonical()) continue;
          // Only var infos who are not the old leader or new leader
          if (newVisSet.contains(var3)) continue;
          VarInfo[] vars = new VarInfo[] {var1, var2, var3};
          Arrays.sort (vars, VarInfo.IndexComparator.theInstance);
          PptSlice3 slice3 = new PptSlice3(this.parent, vars);
          slice3.instantiate_invariants(newSamples > 0);
          slice3.repCheck();
          slice3.set_samples(newSamples);
          newSlices.add(slice3);
          if (debug.isDebugEnabled()) {
            debug.debug ("  adding new Slice3: " + slice3 + " from: ");
            debug.debug (var1.name.name() + ", " +
                         var2.name.name() + ", " +
                         var3.name.name());
          }
        }
      }
    }

    for (Iterator itor = newSlices.iterator(); itor.hasNext(); ) {
      PptSlice slice = (PptSlice) itor.next();
      if (slice.invs.size() == 0) {
        continue;
      }
      // Make sure these slices were never previously instantiated.
      // Instantiation can only occur in two other ways: 1. on
      // canonical variables and 2. through flow.  It can't happen via
      // (1) because this is the first time newVis are canonical.
      // For (2) flow only instantiates slices if they were not part
      // of the same equality set.  Previously, newVis and leader
      // were in the same equality set.
      Assert.assertTrue (parent.findSlice (slice.var_infos) == null);
      slice.repCheck();
      parent.addSlice (slice);
    }
    parent.repCheck();

    if (debug.isDebugEnabled()) {
      debug.debug ("  new slices count:" + parent.views_size());
    }
  }

  PptSlice cloneOnePivot(VarInfo leader, VarInfo newLeader) {
    throw new Error("Shouldn't get called");
  }

  PptSlice cloneAllPivots () {
    throw new Error("Shouldn't get called");
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
