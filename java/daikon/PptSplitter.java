package daikon;

import daikon.split.*;
import daikon.inv.*;
import daikon.suppress.*;
import utilMDE.*;

import java.util.*;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.io.Serializable;


/**
 * PptSplitter contains the splitter and its associated
 * PptConditional ppts.  Currently all splitters are binary and this
 * is presumed in the implementation.  However, this could easily
 * be extended by extending this class with specific other implementations.
 */
public class PptSplitter implements Serializable {

  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20031031L;

  /**
   * Integer. A value of zero indicates that dummy invariants should
   * not be created. A value of one indicates that dummy invariants
   * should be created only when no suitable condition was found in
   * the regular output. A value of two indicates that dummy
   * invariants should be created for each splitting condition.
   **/
  public static int dkconfig_dummy_invariant_level = 0;

  /** General debug tracer. **/
  public static final Logger debug = Logger.getLogger ("daikon.PptSplitter");

  /** PptTopLevel that contains this split. */
  private PptTopLevel parent;

  /** Splitter that choses to which PptConditional a sample is applied. */
  public transient Splitter splitter;

  /**
   * PptConditionals for each splitter output.  ppts[0] is used when the
   * when the splitter is true, ppts[1] when the splitter is false.  The
   * contents are PptConditional objects if the splitter is valid, but are
   * PptTopLevel if the PptSplitter represents two exit points (for which
   * no splitter is required).
   **/
  public PptTopLevel[] ppts = new PptTopLevel[2];

  private static final Comparator icfp
                            = new Invariant.InvariantComparatorForPrinting();

  /**
   * Create a binary splitter with the specied splitter for the specified
   * PptTopLevel parent.  The parent should be a leaf (ie, a numbered
   * exit point)
   */
  public PptSplitter (PptTopLevel parent, Splitter splitter) {

    this.parent = parent;
    this.splitter = splitter;
    ppts[0] = new PptConditional (parent, splitter, false);
    ppts[1] = new PptConditional (parent, splitter, true);

    if (Debug.logDetail()) {
      debug.fine ("VarInfos for " + parent.name());
      for (int ii = 0; ii < parent.var_infos.length; ii++)
        debug.fine (parent.var_infos[ii].name.name() + " "
                            + ppts[0].var_infos[ii].name.name() + " "
                            + ppts[1].var_infos[ii].name.name());
    }
  }

  /**
   * Creats a PptSplitter over two exit points.  No splitter is required.
   */
  public PptSplitter (PptTopLevel parent, PptTopLevel exit1,
                      PptTopLevel exit2) {
    this.parent = parent;
    this.splitter = null;
    ppts[0] = exit1;
    ppts[1] = exit2;
  }


  /**
   * Returns true if the splitter is valid at this point, false otherwise.
   */
  public boolean splitter_valid() {

    if (!((PptConditional) ppts[0]).splitter_valid())
      return (false);
    Assert.assertTrue (((PptConditional)ppts[1]).splitter_valid());
    return (true);
  }

  /** Adds the sample to each conditional ppt in the split. */
  public void add_bottom_up (ValueTuple vt, int count) {

    // Choose the appropriate conditional point based on the condition result
    PptConditional ppt_cond = choose_conditional (vt, count);
    if (ppt_cond == null)
      return;

    // If any parent variables were missing out of bounds on this
    // sample, apply that to this conditional as well.  A more
    // efficient way to do this would be better.
    ppt_cond.get_missingOutOfBounds (parent, vt);

    // Add the point
    ppt_cond.add_bottom_up (vt, count);

    if (Debug.logDetail() && Debug.ppt_match (ppt_cond)) {
      System.out.println ("Adding sample to " + ppt_cond + " with vars "
                          + Debug.related_vars (ppt_cond, vt));
    }

  }

  /**
   * Chooses the correct conditional point based on the values in this sample.
   */
  public PptConditional choose_conditional (ValueTuple vt, int count) {

    boolean splitter_test;
    try {
      splitter_test = ((PptConditional)ppts[0]).splitter.test(vt);
    } catch (Throwable e) {
      // If an exception is thrown, don't put the data on either side
      // of the split.
      if (false) {              // need to add a debugging switch
        System.out.println ("Exception thrown in "
          + "PptSplitter.choose_conditional() for " + ppts[0].name());
        System.out.println ("Vars = " + Debug.related_vars (ppts[0], vt));
      }
      return (null);
    }

    // Choose the appropriate conditional point based on the condition result
    return ((PptConditional) ppts[splitter_test ? 0 : 1]);
  }

  public void add_implications() {

    // Currently only binary implications are supported
    Assert.assertTrue (ppts.length == 2);

    // Create any NIS suppressed invariants in each conditional
    List /*Invariant*/ suppressed_invs[] = new ArrayList[ppts.length];
    for (int i = 0; i < ppts.length; i++)
      suppressed_invs[i] = NIS.create_suppressed_invs (ppts[i]);

    add_implications_pair (false);

    // Remove all of the NIS suppressed invariants that we previously created
    for (int i = 0; i < ppts.length; i++)
      ppts[i].remove_invs (suppressed_invs[i]);
  }

  /**
   * Given a pair of conditional program points, form implications from the
   * invariants true at each one.  The algorithm divides the invariants
   * into two groups:
   * <ol>
   *   <li>the "same" invariants are true at both program points
   *   <li>the "different" invariants are all the others.
   * </ol>
   * The "exclusive" invariants (a subset of the "different" inviariants)
   * are true at one program point, and their negation is true at the other
   * program point
   * At the first program point, for each exclusive invariant and each
   * different invariant, create a conditional of the form "exclusive =>
   * different".  Do the same at the second program point.
   * <p>
   *
   * This method is correct only if the two conditional program points
   * fully partition the input space (their conditions are negations of one
   * another).  For instance, suppose there is a three-way split with the
   * following invariants detected at each:
   * <pre>
   *   {A,B}  {!A,!B}  {A,!B}
   * </pre>
   * Examining just the first two would suggest that "A <=> B" is valid,
   * but in fact that is a false inference.  Note that this situation can
   * occur if the splitting condition uses variables that can ever be missing.
   */
  private void add_implications_pair (boolean add_nonimplications) {

    debug.fine ("Adding Implications for " + parent.name);

    // Maps permuted invariants to their original invariants
    Map orig_invs = new LinkedHashMap();

    // elements are Invariants
    Vector same_invs_vec = new Vector();

    // elements are pairs of Invariants
    Vector exclusive_invs_vec = new Vector();

    // elements are pairs of Invariants
    Vector different_invs_vec = new Vector();

    // Loop through each possible parent slice
    List slices = possible_slices();

    slice_loop:
    for (Iterator itor = slices.iterator(); itor.hasNext(); ) {
      VarInfo[] vis = (VarInfo[]) itor.next();

      int num_children = ppts.length;
      // Each element is an invariant from the indexth child, permuted to
      // the parent (and with a parent slice as its ppt slot).
      Invariants[] invs = new Invariants[num_children];

      // find the parent slice
      PptSlice pslice = parent.get_or_instantiate_slice (vis);

      // Daikon.debugProgress.fine ("    slice: " + pslice.name());

      // Loop through each child ppt
      for (int childno = 0; childno < num_children; childno++) {
        PptTopLevel child_ppt = ppts[childno];

        Assert.assertTrue (child_ppt.equality_view != null);
        Assert.assertTrue (parent.equality_view != null);

        invs[childno] = new Invariants();

        // Get the child vis in the correct order
        VarInfo[] cvis_non_canonical = new VarInfo[vis.length];
        VarInfo[] cvis = new VarInfo[vis.length];
        VarInfo[] cvis_sorted = new VarInfo[vis.length];
        for (int kk = 0; kk < vis.length; kk++) {
          cvis_non_canonical[kk] = matching_var (child_ppt, parent, vis[kk]);
          cvis[kk] = matching_var (child_ppt, parent, vis[kk]).canonicalRep();
          cvis_sorted[kk] = cvis[kk];
        }
        Arrays.sort (cvis_sorted, VarInfo.IndexComparator.getInstance());

        // Look for an equality invariant in the non-canonical slice (if any).
        // Note that only an equality invariant can exist in a non-canonical
        // slice.  If it does exist,  we want it rather than the canonical
        // form (which for equality invariants will always be of the form
        // 'a == a').
        Invariant eq_inv = null;
        if (!Arrays.equals (cvis_non_canonical, cvis)) {
          PptSlice nc_slice = child_ppt.findSlice (cvis_non_canonical);
          if (nc_slice != null) {
            if (nc_slice.invs.size() != 1) {
              // Impossible: multiple invariants found
              System.out.println ("Found " + nc_slice.invs.size() +
                                  " invs at " + nc_slice);
              for (Iterator kk = nc_slice.invs.iterator(); kk.hasNext(); )
                System.out.println (" -- inv = " + kk.next());
              for (int kk = 0; kk < cvis_non_canonical.length; kk++)
                System.out.println (" -- equality set = " +
                      cvis_non_canonical[kk].equalitySet.shortString());
              throw new Error("nc_slice.invs.size() == " + nc_slice.invs.size());
            }
            eq_inv = (Invariant) nc_slice.invs.get (0);
            debug.fine ("Found eq inv " + eq_inv);
          }
        }

        // Find the corresponding slice
        PptSlice cslice = child_ppt.findSlice (cvis_sorted);
        if (cslice == null) {
          if (eq_inv != null) {
            for (int i = 0; i < cvis_sorted.length; i++)
              Fmt.pf ("con val = " + child_ppt.constants.all_vars[cvis_sorted[i].varinfo_index]);
            Assert.assertTrue (eq_inv == null, "found eq_inv " + eq_inv + " @"
                             + eq_inv.ppt + " but can't find slice for "
                             + VarInfo.toString (cvis_sorted));
          }
          continue;
        }

        // Copy each invariant permuted to the parent
        int[] permute = PptTopLevel.build_permute (cvis_sorted, cvis);
        for (int j = 0; j < cslice.invs.size(); j++) {
          Invariant orig_inv = (Invariant) cslice.invs.get (j);
          Invariant inv = orig_inv.clone_and_permute (permute);
          inv.ppt = pslice;
          invs[childno].add (inv);
          if ((eq_inv != null) && orig_inv.getClass().equals(eq_inv.getClass()))
            orig_inv = eq_inv;
          Assert.assertTrue (! orig_invs.containsKey (inv));
          orig_invs.put (inv, orig_inv);
        }
      } // children loop


      // If neither child slice has invariants there is nothing to do
      if ((invs[0].size() == 0) && (invs[1].size() == 0)) {
        if (pslice.invs.size() == 0)
          parent.removeSlice (pslice);
        continue;
      }


      if ((pslice.invs.size() == 0) && Debug.logDetail())
        debug.fine ("PptSplitter: created new slice " +
                            VarInfo.toString (vis) + " @" + parent.name);

      // Add any exclusive conditions for this slice to the list
      exclusive_invs_vec.addAll(exclusive_conditions(invs[0], invs[1]));

      // Add any invariants that are the same to the list
      same_invs_vec.addAll (same_invariants (invs[0], invs[1]));

      // Add any invariants that are different to the list
      different_invs_vec.addAll (different_invariants (invs[0], invs[1]));


    } // slice_loop: slices.iterator() loop


    // This is not tested.
    if (add_nonimplications) {
      // Add to the join point all invariants that appeared at both children.
      for (int i=0; i<same_invs_vec.size(); i++) {
        Invariant same_inv = (Invariant)same_invs_vec.elementAt(i);
        // This test doesn't seem to be productive.  (That comment may date
        // from the time that all not-worth-printing invariants were
        // already eliminated.)
        parent.joiner_view.addInvariant(same_inv);
      }
    }

    if (Debug.logOn() || debug.isLoggable (Level.FINE)) {
      debug.fine ("Found " + exclusive_invs_vec.size()
                  + " exclusive conditions ");
      for (Iterator i = exclusive_invs_vec.iterator(); i.hasNext(); ) {
        Invariant[] invs = (Invariant[]) i.next();
        invs[0].log ("exclusive condition with " + invs[1].format());
        invs[1].log ("exclusive condition with " + invs[0].format());
        debug.fine ("-- " + Invariant.toString (invs));
      }
      debug.fine ("Found " + different_invs_vec.size() + " different invariants ");
      for (Iterator i = different_invs_vec.iterator(); i.hasNext(); ) {
        Invariant[] invs = (Invariant[]) i.next();
        if (invs[0] != null)
          invs[0].log (invs[0] + " differs from "  + invs[1]);
        if (invs[1] != null)
          invs[1].log (invs[0] + " differs from "  + invs[1]);
        debug.fine ("-- " + Invariant.toString (invs));
      }
    }

    Vector dummies = new Vector();
    PptTopLevel ppt1 = ppts[0];
    PptTopLevel ppt2 = ppts[1];

    // Add the splitting condition as an exclusive condition if requested
    if ((splitter != null) && dkconfig_dummy_invariant_level > 0) {
      if (exclusive_invs_vec.size() == 0
          || dkconfig_dummy_invariant_level >= 2) {
        // As a last resort, try using the user's supplied DummyInvariant
        debug.fine ("addImplications: resorting to dummy");
        PptConditional cond1 = (PptConditional)ppt1;
        PptConditional cond2 = (PptConditional)ppt2;
        cond1.splitter.instantiateDummy(ppt1);
        cond2.splitter.instantiateDummy(ppt2);
        DummyInvariant dummy1 = cond1.dummyInvariant();
        DummyInvariant dummy2 = cond2.dummyInvariant();
        if (dummy1 != null && dummy1.valid && dummy2 != null && dummy2.valid) {
          Assert.assertTrue(!cond1.splitter_inverse);
          Assert.assertTrue(cond2.splitter_inverse);
          dummy2.negate();
          exclusive_invs_vec.add(new Invariant[] {dummy1, dummy2});
          dummies.add(new Invariant[] {dummy1, dummy2});
        }
      }
    }
    different_invs_vec.addAll(dummies);


    // If there are no exclusive conditions, we can do nothing here
    if (exclusive_invs_vec.size() == 0) {
      if (debug.isLoggable(Level.FINE)) {
        debug.fine ("addImplications: no exclusive conditions");
      }
      return;
    }


    // Create array versions of each
    Invariant[][] exclusive_invariants
      = (Invariant[][])exclusive_invs_vec.toArray(new Invariant[0][0]);
    Invariant[][] different_invariants
      = (Invariant[][])different_invs_vec.toArray(new Invariant[0][0]);

    // Add an implication from each of a pair of mutually exclusive
    // invariants to everything that differs (at all) about the two

    // split into two in order to use indexOf
    Invariant[] excls1 = new Invariant[exclusive_invariants.length];
    Invariant[] excls2 = new Invariant[exclusive_invariants.length];
    for (int i=0; i<exclusive_invariants.length; i++) {
      excls1[i] = exclusive_invariants[i][0];
      excls2[i] = exclusive_invariants[i][1];
    }

    // Keep track of all of the implications created below
    List imps = new ArrayList();

    for (int i=0; i < exclusive_invariants.length; i++) {
      Assert.assertTrue(exclusive_invariants[i].length == 2);
      Invariant excl1 = exclusive_invariants[i][0];
      Invariant excl2 = exclusive_invariants[i][1];
      Assert.assertTrue(excl1 != null);
      Assert.assertTrue(excl2 != null);

      for (int j=0; j < different_invariants.length; j++) {
        Assert.assertTrue(different_invariants[j].length == 2);
        Invariant diff1 = different_invariants[j][0];
        Invariant diff2 = different_invariants[j][1];

        Assert.assertTrue((diff1 == null) || (diff2 == null)
                      || (ArraysMDE.indexOf(excls1, diff1)
                          == ArraysMDE.indexOf(excls2, diff2)));

        // This adds an implication to itself; bad.
        // If one of the diffs implies the other, then should not add
        // an implication for the weaker one.
        if (diff1 != null) {
          int index1 = ArraysMDE.indexOf(excls1, diff1);
          if ((index1 == -1) || (index1 > i)) {
            boolean iff = (index1 != -1);
            add_implication (parent, excl1, diff1, iff, orig_invs);
          }
        }
        if (diff2 != null) {
          int index2 = ArraysMDE.indexOf(excls2, diff2);
          if ((index2 == -1) || (index2 > i)) {
            boolean iff = (index2 != -1);
            add_implication (parent, excl2, diff2, iff, orig_invs);
          }
        }
      }
    }

    if (debug.isLoggable (Level.FINE)) {
      debug.fine (" Orig Joiner View ");
      for (Iterator i = parent.joiner_view.invs.iterator(); i.hasNext(); )
        debug.fine ("-- " + i.next());
    }


    // Given bi-implications "A<=>B", "B<=>C", "C<=>D", etc., mark one of
    // the equivalent implications as the canonical one, to avoid
    // repeatedly printing the same fact (as both "A=>Z" and "B=>Z", for
    // example).
    // (Question:

    // Invariant -> Invariant
    HashMap canonical_inv = new LinkedHashMap();
    {
      // Invariant -> HashSet[Invariant]
      // The key is he canonical invariant for each element in the set.
      HashMap inv_group = new LinkedHashMap();

      // Problem: I am not iterating through the invariants in any
      // particular order that will guarantee that I don't see A and
      // B, then C and D, and then A and C (which both already have
      // different canonical versions).
      // System.out.println(name + "implication canonicalization");
      for (Iterator itor = parent.joiner_view.invs.iterator();
                                                            itor.hasNext(); ) {
        Invariant inv = (Invariant) itor.next();
        if ((inv instanceof Implication) && ((Implication) inv).iff) {
          Implication impl = (Implication) inv;
          // System.out.println("Bi-implication: " + impl.format());
          Invariant canon1 = (Invariant) canonical_inv.get(impl.predicate());
          Invariant canon2 = (Invariant) canonical_inv.get(impl.consequent());
          if ((canon1 != null) && (canon2 != null) && (canon1 != canon2)) {
            // Move all the invariants for canon2 over to canon1
            HashSet hs1 = (HashSet) inv_group.get(canon1);
            HashSet hs2 = (HashSet) inv_group.get(canon2);
            inv_group.remove(canon2);
            for (Iterator itor2=hs2.iterator(); itor2.hasNext(); ) {
              Invariant inv2 = (Invariant) itor2.next();
              hs1.add(inv2);
              canonical_inv.put(inv2, canon1);
            }
            // System.out.print("Current set:");
            // for (Iterator itor2=hs1.iterator(); itor2.hasNext(); ) {
            //   Invariant inv2 = (Invariant) itor2.next();
            //   System.out.print("    " + inv2.format());
            // }
            // System.out.println();
          } else {
            Invariant canon = (canon1 != null) ? canon1 : (canon2 != null) ? canon2 : impl.predicate();
            // System.out.println("Canonical: " + canon.format());
            canonical_inv.put(impl.predicate(), canon);
            canonical_inv.put(impl.consequent(), canon);
            HashSet hs = (HashSet) inv_group.get(canon);
            if (hs == null) {
              hs = new LinkedHashSet();
              inv_group.put(canon, hs);
            }
            hs.add(impl.predicate());
            hs.add(impl.consequent());
            // System.out.print("Current set (2):");
            // for (Iterator itor2=hs.iterator(); itor2.hasNext(); ) {
            //   Invariant inv2 = (Invariant) itor2.next();
            //   System.out.print("    " + inv2.format());
            // }
            // System.out.println();
          }
        }
      }

      // Now adjust which of the invariants are canonical.
      // (That is why inv_group was computed above.)

      for (Iterator itor=inv_group.keySet().iterator(); itor.hasNext(); ) {
        Invariant canon_orig = (Invariant) itor.next();
        // System.out.println("Outer loop: " + canon_orig.format());
        HashSet hs = (HashSet) inv_group.get(canon_orig);
        if (hs.size() == 1) {
          continue;
        }
        Invariant canon_new = null;
        String canon_new_formatted = null;
        for (Iterator cand_itor=hs.iterator(); cand_itor.hasNext(); ) {
          Invariant candidate = (Invariant) cand_itor.next();
          String candidate_formatted = candidate.format();
          // System.out.println("Comparing:" + lineSep + "    " + candidate_formatted + lineSep + "    " + canon_new_formatted);
          // It is also desirable to be over the prestate;
          // but that is only true for variables that are modified.
          // A variable without "orig()" is fine if it's not modified.
          boolean canon_new_undesirable
            = ((canon_new == null) // avoid NullPointerException
               || (canon_new_formatted.indexOf("\"null\"") != -1)
               || (canon_new_formatted.indexOf("return") != -1));
          boolean candidate_undesirable
            = ((candidate_formatted.indexOf("\"null\"") != -1)
               || (candidate_formatted.indexOf("return") != -1));
          if ((canon_new == null)
              || canon_new_undesirable
              || ((! candidate_undesirable)
                  && (candidate_formatted.length() < canon_new_formatted.length()))) {
            canon_new = candidate;
            canon_new_formatted = candidate_formatted;
          }
        }
        if (canon_new != canon_orig) {
          // Don't set inv_group, lest I get a ConcurrentModificationException
          // inv_group.put(canon_new, hs);
          // inv_group.remove(canon_orig);
          for (Iterator inv_itor=hs.iterator(); inv_itor.hasNext(); ) {
            Invariant inv = (Invariant) inv_itor.next();
            Assert.assertTrue(canonical_inv.get(inv) == canon_orig);
            canonical_inv.put(inv, canon_new);
          }
        }
      }
      // inv_group is no longer up-to-date now.
      // I could have created an inv_group_2 during the above computation
      // and set inv_group to it if I liked.
    }

    // Prune out implications over non-canonical invariants
    // TODO: Why not avoid introducing them in the first place?

    Vector to_remove = new Vector();
    for (Iterator itor = parent.joiner_view.invs.iterator(); itor.hasNext(); ) {
      Invariant inv = (Invariant) itor.next();
      if (inv instanceof Implication) {
        Implication impl = (Implication) inv;
        Invariant cpred = (Invariant) canonical_inv.get(impl.predicate());
        Invariant ccons = (Invariant) canonical_inv.get(impl.consequent());
        boolean pred_non_canon = ((cpred != null)
                                  && (impl.predicate() != cpred));
        boolean cons_non_canon = ((ccons != null)
                                  && (impl.consequent() != ccons));
        if ((! impl.iff)
            && (pred_non_canon || cons_non_canon)) {
          // TODO: This is an inefficient call.  It would be better to avoid
          // introducing the noncanonical invariants in the first place.
          to_remove.add(inv);
        }
      }
    }

    parent.joiner_view.removeInvariants(to_remove);

    if (Debug.logOn() || debug.isLoggable (Level.FINE)) {
      debug.fine ("Joiner View ");
      for (Iterator i = parent.joiner_view.invs.iterator(); i.hasNext(); ) {
        Implication imp = (Implication) i.next();
        imp.log ("In joiner view");
        debug.fine ("-- " + imp);
      }
      debug.fine ("Removed from Joiner View ");
      for (Iterator i = to_remove.iterator(); i.hasNext(); ) {
        Implication imp = (Implication) i.next();
        imp.log ("removed from joiner view");
        debug.fine ("-- " + imp);
      }
    }


  } // add_implications_pair


  /**
   * Returns a list of all possible slices that may appear at the parent.
   * The parent must have already been created by merging the invariants
   * from its child conditionals.
   *
   * This is different from the slices that actually exist at the parent
   * becaue there make be implications created from invariants in child
   * slices that only exist in one child.
   **/
  private List /*VarInfo[]*/ possible_slices() {

    List /*VarInfo[]*/ result = new ArrayList();

    // Get an array of leaders at the parent to build slices over
    VarInfo[] leaders = parent.equality_view.get_leaders_sorted();

    // Create unary views
    List unary_slices = new ArrayList();
    for (int i = 0; i < leaders.length; i++) {
      if (parent.is_slice_ok (leaders[i])) {
        result.add (new VarInfo[] {leaders[i]});
      }
    }

    // Create binary views
    List binary_slices = new ArrayList();
    for (int i = 0; i < leaders.length; i++) {
      for (int j = i; j < leaders.length; j++) {
        if (parent.is_slice_ok (leaders[i], leaders[j]))
          result.add (new VarInfo[] {leaders[i], leaders[j]});
      }
    }

    // Create ternary views
    List ternary_slices = new ArrayList();
    for (int i = 0; i < leaders.length; i++) {
      for (int j = i; j < leaders.length; j++) {
        for (int k = j; k < leaders.length; k++) {
          if (parent.is_slice_ok (leaders[i], leaders[j], leaders[k]))
            result.add (new VarInfo[] {leaders[i], leaders[j], leaders[k]});
        }
      }
    }

    return (result);
  }


  /**
   * Determine which elements of invs1 are mutually exclusive with
   * elements of invs2.  Result elements are pairs of Invariants.
   */
  Vector /*Invariants[2]*/ exclusive_conditions (Invariants invs1,
                                                 Invariants invs2) {

    Vector result = new Vector();
    for (int i1=0; i1 < invs1.size(); i1++) {
      for (int i2=0; i2 < invs2.size(); i2++) {
        Invariant inv1 = (Invariant) invs1.get(i1);
        Invariant inv2 = (Invariant) invs2.get(i2);
        // // This is a debugging tool, to make sure that various versions
        // // of isExclusiveFormula remain coordinated.  (That's also one
        // // reason we don't break out of the loop early:  also, there will
        // // be few invariants in a slice, so breaking out is of minimal
        // // benefit.)
        // Assert.assertTrue(inv1.isExclusiveFormula(inv2)
        //                   == inv2.isExclusiveFormula(inv1),
        //               "Bad exclusivity: " + inv1.isExclusiveFormula(inv2)
        //                + " " + inv2.isExclusiveFormula(inv1)
        //                + "    " + inv1.format() + "    " + inv2.format());
        if (inv1.isExclusiveFormula(inv2)) {
          result.add(new Invariant[] { inv1, inv2 });
        }
      }
    }
    return result;
  }

  /**
   * Determine which elements of invs1 differ from elements of invs2.
   * Result elements are pairs of Invariants (with one or the other
   * possibly null).
   */
  Vector /*Invariant[2]*/ different_invariants (Invariants invs1,
                                                Invariants invs2) {
    SortedSet ss1 = new TreeSet(icfp);
    ss1.addAll(invs1);
    SortedSet ss2 = new TreeSet(icfp);
    ss2.addAll(invs2);
    Vector result = new Vector();
    for (OrderedPairIterator opi = new OrderedPairIterator(ss1.iterator(),
                                    ss2.iterator(), icfp);
         opi.hasNext(); ) {
      Pair pair = (Pair) opi.next();
      if ((pair.a == null) || (pair.b == null)
          // || (icfp.compare(pair.a, pair.b) != 0)
          ) {
        result.add(new Invariant[] { (Invariant) pair.a, (Invariant) pair.b });
      }
    }
    return result;
  }


  /**
   * Determine which elements of invs1 are the same as elements of invs2.
   * Result elements are Invariants (from the invs1 list)
   */
  Vector /*Invariant*/ same_invariants(Invariants invs1, Invariants invs2) {

    SortedSet ss1 = new TreeSet(icfp);
    ss1.addAll(invs1);
    SortedSet ss2 = new TreeSet(icfp);
    ss2.addAll(invs2);
    Vector result = new Vector();
    for (OrderedPairIterator opi = new OrderedPairIterator(ss1.iterator(),
                                    ss2.iterator(), icfp);
         opi.hasNext(); ) {
      Pair pair = (Pair) opi.next();
      if (pair.a != null && pair.b != null) {
        Invariant inv1 = (Invariant) pair.a;
        Invariant inv2 = (Invariant) pair.b;
        result.add(inv1);
      }
    }
    return result;
  }


  // // Determine which invariants at the program points differ.
  // // Result elements are pairs of Invariants (with one or the other
  // // possibly null.)
  // Vector different_invariants(PptSlice[][] matched_views) {
  //   Vector result = new Vector();
  //   for (int i=0; i<matched_views.length; i++) {
  //     PptSlice cond1 = matched_views[i][0];
  //     PptSlice cond2 = matched_views[i][1];
  //     Invariants invs1 = (cond1 == null) ? new Invariants() : cond1.invs;
  //     Invariants invs2 = (cond2 == null) ? new Invariants() : cond2.invs;
  //     result.addAll(different_invariants(invs1, invs2));
  //   }
  //   return result;
  // }

  /**
   * Creates the invariant specified by predicate and consequent and
   * if it is a valid implication, adds it to the joiner view of
   * parent.
   * @param orig_invs Maps permuted invariants to their original invariants
   **/
  public void add_implication (PptTopLevel ppt, Invariant predicate,
                               Invariant consequent, boolean iff,
                               Map orig_invs) {

    Invariant orig_pred = (Invariant) orig_invs.get (predicate);
    Invariant orig_cons = (Invariant) orig_invs.get (consequent);
    Assert.assertTrue (orig_pred != null);
    Assert.assertTrue (orig_cons != null);

    // JHP: if this code is enabled, other implications are added.  It
    // is not at all clear to me how/why that happens.  This should be
    // investigated.  For now, though, the implication is created here
    // and noted as obvious in Implication.isObviousDynamically_SomeInEquality
    // Also, this could possibly be better implemented by changing the
    // way that we create the list of invariants that is one conditional
    // and not in the other to not include an invariant if it is suppressed
    // on the other side.  This would have the pleasant side effect of not
    // forcing all of the suppressed invariants to be created before
    // determining implications.
    if (false && orig_cons.is_ni_suppressed()) {
      if (Debug.logOn())
        orig_cons.log ("not creating implication, suppressed");
      return;
    }

    // System.out.println("add_implication:");
    // System.out.println("  predicate = " + predicate.format());
    // System.out.println("  consequent= " + consequent.format());
    // System.out.println("  orig_pred = " + orig_pred.format());
    // System.out.println("  orig_cons = " + orig_cons.format());
    Implication imp = Implication.makeImplication (ppt, predicate, consequent,
                                                   iff, orig_pred, orig_cons);
    if (imp == null)
      return;

    ppt.joiner_view.addInvariant (imp);
  }

  /**
   * Adds the specified relation from each conditional ppt in this
   * to the corresponding conditional ppt in ptp_split.  The relation
   * specified should be a relation from this.parent to ppt_split.parent.
   */
  public void add_relation (PptRelation rel, PptSplitter ppt_split) {

    for (int ii = 0; ii < ppts.length; ii++ ) {
      PptRelation cond_rel = rel.copy (ppts[ii], ppt_split.ppts[ii]);
      // System.out.println ("Added relation: " + cond_rel);
      // System.out.println ("with relations: "
      //                      + cond_rel.parent_to_child_var_string());
    }
  }

  /**
   * Returns the VarInfo in ppt1 that matches the specified VarInfo in ppt2
   * The variables at each point must match exactly.  This is reasonable
   * assumption for the ppts in PptSplitter and their parent.
   */
  private VarInfo matching_var (PptTopLevel ppt1, PptTopLevel ppt2,
                               VarInfo ppt2_var) {

    VarInfo v = ppt1.var_infos[ppt2_var.varinfo_index];
    Assert.assertTrue (v.name.equals (ppt2_var.name));
    return (v);
  }

  public String toString() {

    return "Splitter " + splitter + ": ppt1 " + ppts[0].name() + ": ppt2 "
            + ppts[1].name;
  }

}
