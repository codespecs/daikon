package daikon.suppress;

import daikon.*;
import daikon.inv.*;
import daikon.inv.unary.*;
import daikon.inv.binary.*;
import utilMDE.*;

import java.lang.reflect.*;
import java.util.logging.*;
import java.util.*;

/**
 * Class that defines a suppressor invariant for use in non-instantiating
 * suppressions.  In non-instantiating suppressions, suppressor invariants
 * are defined independent of specific variables.  Intstead, arguments
 * are identified by their variable index in the suppressee.
 */
public class NISuppressor {

  /** Debug tracer. **/
  public static final Logger debug
                          = Logger.getLogger ("daikon.inv.NISuppressor");


  /**
   * Argument indices used by the invariant.
   */
  int v1_index = -1;
  int v2_index = -1;
  int v3_index = -1;

  /** Invariant class. **/
  Class inv_class;

  /** True if the order of the variables was swapped. **/
  boolean swap = false;

  /** True if invariant permutes by changing its class. **/
  boolean swap_class = false;

  /**
   * State of the suppressor for the current check.  The state must be
   * one of the defined above.  They can always be compared with ==.
   **/
  String state = NIS.NONE;

  Invariant sample_inv;

  /**
   * Defines a unary suppressor.
   */
  public NISuppressor (int v1_index, Class cls) {

    debug.fine (Fmt.spf ("creating %s over arg %s", cls.getName(),
                         Fmt.i (v1_index)));

    this.v1_index = v1_index;
    this.inv_class = cls;

    // Create a sample invariant
    try {
      Method instantiate = inv_class.getMethod ("instantiate",
                                                new Class[] {PptSlice.class});
      sample_inv = (Invariant)instantiate.invoke (null, new Object[] {null});
    } catch (Exception e) {
      throw new RuntimeException ("error instantiating invariant "
                                  + inv_class.getName() + ": " + e);
    }

    debug.fine ("Created " + this);
  }

  /**
   * Defines a binary suppressor.
   */
  public NISuppressor (int v1_index, int v2_index, Class cls) {

    debug.fine (Fmt.spf ("creating %s over args %s and %s", cls.getName(),
                         Fmt.i (v1_index), Fmt.i(v2_index)));

    // put the variables in their standard order
    if (v1_index > v2_index) {
      this.v1_index = v2_index;
      this.v2_index = v1_index;
      swap = true;
    } else {
      this.v1_index = v1_index;
      this.v2_index = v2_index;
      swap = false;
    }

    // If the specified class handles swapping with a different class,
    // get the class
    swap_class = true;
    try {
      Method swap_method = cls.getMethod ("swap_class", null);
      if (swap)
        cls = (Class) swap_method.invoke (null, null);
    } catch (Exception e) {
      swap_class = false;
    }

    this.inv_class = cls;

    // Create a sample invariant
    try {
      Method instantiate = null;
      boolean has_swap_param = false;
      try {
        instantiate = inv_class.getMethod ("instantiate",
                               new Class[] {PptSlice.class, boolean.class});
        has_swap_param = true;
        sample_inv = (Invariant)instantiate.invoke (null,
                                  new Object[] {null, Boolean.valueOf(swap)});
      } catch (Exception e) {
      }
      if (instantiate == null) {
        instantiate = inv_class.getMethod ("instantiate",
                               new Class[] {PptSlice.class});
        sample_inv = (Invariant)instantiate.invoke (null, new Object[] {null});
      }
    } catch (Exception e) {
      throw new RuntimeException ("error instantiating invariant "
                                  + inv_class.getName() + ": " + e);
    }

    debug.fine ("Created " + this);
  }

  /**
   * Sets the status of this suppressor with regards to the specified
   * vis and falsified invariant.  The status consists of whether or
   * not the suppressor is valid (true) and whether or not it matches
   * the falsified invariant.
   *
   * Matching a suppressor is more complex than is apparent at first
   * glance.  The invariant AND its variables must match.  Since
   * suppressors are specified without variables, the variables are
   * taken from the specified vis.  The variable indices specify which
   * variables to consider.
   *
   * For example consider the suppressor {1, 2, IntLessEqual} and a
   * vis of {x, y, z}.  The suppressor is true if the IntLessEqual
   * invariant exists in the slice {y, z}.  This allows ternary
   * invariants to specify exactly the suppressor required for their
   * particular permutation ofarguments.  Invariants that have an
   * internal permute variable must match that as well.
   *
   * @param ppt     The top level program point
   * @param vis     The slice of the suppressee.  Thus, if the suppressee is
   *                ternary, vis, should specify three variables.
   * @param inv     The falsified invariant.  inv_match indicates whether
   *                or not inv matches this suppressor
   *
   * @return the state of this suppressor which is one of (NIS.MATCH,
   *         NIS.VALID, NIS.INVALID, NIS.MISSING)
   */

  public String check (PptTopLevel ppt, VarInfo[] vis, Invariant inv) {

    // Currently we only support unary and binary suppressors
    Assert.assertTrue (v3_index == -1);
    Assert.assertTrue (v1_index != -1);

    if (Debug.logDetail() && NIS.debug.isLoggable (Level.FINE))
      NIS.debug.fine ("checking suppressor " + this + " against inv "
                    + ((inv != null) ? inv.format() : "null") + " over vars "
                    + VarInfo.toString(vis) + " in ppt " + ppt.name);

    // If unary
    if (v2_index == -1) {

      VarInfo v1 = vis[v1_index];

      // Check to see if inv matches this suppressor.  The invariant class
      // and variables must match for this to be true
      if ((inv != null) &&
          (inv.getClass() == inv_class) && (v1 == inv.ppt.var_infos[0])) {
        return (state = NIS.MATCH);
      }

      // Check to see if the suppressor is true over all constants.
      if (ppt.is_prev_constant (v1)) {
        UnaryInvariant uinv = (UnaryInvariant) sample_inv;
        InvariantStatus status = uinv.check (ppt.constants.constant_value(v1),
                                           ValueTuple.MODIFIED, 1);
        boolean valid = (status == InvariantStatus.NO_CHANGE);
        if (NIS.debug.isLoggable(Level.FINE))
          NIS.debug.fine("constant args - " + valid);
        return (state = (valid ? NIS.VALID : NIS.INVALID));
      }

      // Check to see the variable is missing
      if (ppt.is_prev_missing(v1))
        return (state = NIS.MISSING);

      // Check to see if this suppressor is true.  Note that we don't check
      // to see if the invariant has been falsified.  That is because we
      // do this processing as falsified invariants are removed from the lists.
      // An invariant that is still in the list, but marked falsified, is true
      // for our purposes (we will process it later, when it is removed)
      PptSlice slice = ppt.findSlice (v1);
      if (slice != null) {
        for (Iterator i = slice.invs.iterator(); i.hasNext(); ) {
          Invariant slice_inv = (Invariant) i.next();
          if (match (slice_inv))
            return (state = NIS.VALID);
        }
      }
      if (ppt.global != null) {
        PptSlice gslice = PptSlice.find_global_slice (new VarInfo[] {v1});
        if (gslice != null) {
          for (Iterator i = gslice.invs.iterator(); i.hasNext(); ) {
            Invariant gslice_inv = (Invariant) i.next();
            if (match (gslice_inv)) {
              if (NIS.debug.isLoggable (Level.FINE))
                NIS.debug.fine ("suppressor matches global inv "
                          + gslice_inv.format() + " "+ !gslice_inv.is_false());
              return (state = NIS.VALID);
            }
          }
        }
      }
      return (state = NIS.INVALID);

    } else /* must be binary */ {
      VarInfo v1 = vis[v1_index];
      VarInfo v2 = vis[v2_index];

      // Check to see if inv matches this suppressor.  The invariant class,
      // variables, and swap must match for this to be true
      if ((inv != null) && match (inv) && (v1 == inv.ppt.var_infos[0])
          && (v2 == inv.ppt.var_infos[1])) {
        if (NIS.debug.isLoggable (Level.FINE))
          NIS.debug.fine ("Matches falsified inv " + inv.format());
        return (state = NIS.MATCH);
      }

      // Check to see if the suppressor is true over all constants.  This
      // code will not work for invariants with any state!
      if (ppt.is_prev_constant (v1) && ppt.is_prev_constant (v2)) {
        BinaryInvariant binv = (BinaryInvariant) sample_inv;
        InvariantStatus status = binv.check (ppt.constants.constant_value(v1),
                                           ppt.constants.constant_value(v2),
                                           ValueTuple.MODIFIED, 1);
        boolean valid = (status == InvariantStatus.NO_CHANGE);
        if (NIS.debug.isLoggable (Level.FINE))
          NIS.debug.fine (Fmt.spf ("constant args (%s, %s) = %s ",
                       Debug.toString (ppt.constants.constant_value(v1)),
                       Debug.toString (ppt.constants.constant_value(v2)),
                       "" + valid));
        return (state = (valid ? NIS.VALID : NIS.INVALID));
      }

      // Check to see if either variable is missing
      if (ppt.is_prev_missing(v1) || ppt.is_prev_missing(v2))
        return (state = NIS.MISSING);

      // Check to see if this suppressor is true.  Note that we don't checked
      // to see if the invariant has been falsified.  That is because we
      // do this processing as falsified invariants are removed from the lists.
      // An invariant that is still in the list, but marked falsified, is true
      // for our purposes (we will process it later, when it is removed)
      PptSlice slice = ppt.findSlice (v1, v2);
      if (slice != null) {
        for (Iterator i = slice.invs.iterator(); i.hasNext(); ) {
          Invariant slice_inv = (Invariant) i.next();
          // NIS.debug.fine (": processing inv " + slice_inv.format());
          if (match (slice_inv)) {
            if (NIS.debug.isLoggable (Level.FINE))
              NIS.debug.fine ("suppressor matches inv " + slice_inv.format()
                           + " " + !slice_inv.is_false());
            return (state = NIS.VALID);
          }
        }
      }
      if (ppt.global != null) {
        PptSlice gslice = PptSlice.find_global_slice (new VarInfo[] {v1, v2});
        if (gslice != null) {
          for (Iterator i = gslice.invs.iterator(); i.hasNext(); ) {
            Invariant gslice_inv = (Invariant) i.next();
            if (match (gslice_inv)) {
              if (NIS.debug.isLoggable (Level.FINE))
                NIS.debug.fine ("suppressor matches global inv "
                          + gslice_inv.format() + " "+ !gslice_inv.is_false());
              return (state = NIS.VALID);
            }
          }
        }
      }
      NIS.debug.fine ("suppressor not found");
      return (state = NIS.INVALID);
    }
  }

  /**
   * Returns true if inv matches this suppressor.  It is assummed that
   * inv's variables already match (ie, that it was looked up in
   * compatible slice
   */
  public boolean match (Invariant inv) {

    if (v2_index == -1)
      return (inv.getClass() == inv_class);
    else {
      if (inv.getClass() != inv_class)
        return (false);
      if (!swap_class) {
        BinaryInvariant binv = (BinaryInvariant) inv;
        return (binv.is_symmetric() || (swap == binv.get_swap()));
      }
      return (true);
    }
  }

  public Class get_inv_class() {
    return (inv_class);
  }

  public void clear_state() {
    state = NIS.NONE;
  }

  static String[] varname = new String[] { "x", "y", "z" };

  public String toString() {

    String cname = UtilMDE.unqualified_name (inv_class);

    String status = state;
    if (status == NIS.NONE)
      status = "";

    if (v2_index == -1)
      return (Fmt.spf ("%s(%s) [%s]", cname, varname[v1_index], status));
    else if (v3_index == -1) {
      if (swap && !swap_class)
        return (Fmt.spf ("%s(%s,%s) [%s]", cname, varname[v2_index],
               varname[v1_index], status));
      else
        return (Fmt.spf ("%s(%s,%s) [%s]", cname, varname[v1_index],
               varname[v2_index], status));
    } else
      return (Fmt.spf ("%s(%s,%s,%s) [%s]", cname, varname[v1_index],
                       varname[v2_index], varname[v3_index], status));
  }

}
