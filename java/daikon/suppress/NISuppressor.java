package daikon.suppress;

import static daikon.inv.Invariant.asInvClass;

import daikon.*;
import daikon.inv.*;
import daikon.inv.binary.*;
import daikon.inv.unary.*;
import java.lang.reflect.*;
import java.util.*;
import java.util.logging.*;
import plume.*;

/*>>>
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.dataflow.qual.*;
import typequals.*;
*/

/**
 * Class that defines a suppressor invariant for use in non-instantiating suppressions. In
 * non-instantiating suppressions, suppressor invariants are defined independent of specific
 * variables. Instead, arguments are identified by their variable index in the suppressee.
 */
public class NISuppressor {

  /** Debug tracer. */
  public static final Logger debug = Logger.getLogger("daikon.inv.NISuppressor");

  /** Argument indices used by the invariant. */
  int v1_index = -1;

  int v2_index = -1;
  int v3_index = -1;

  /** Invariant class. */
  Class<? extends Invariant> inv_class;

  /** True if the order of the variables was swapped. */
  boolean swap = false;

  /** True if invariant permutes by changing its class. */
  boolean swap_class = false;

  /**
   * State of the suppressor for the current check. The state must be one of the defined above. They
   * can always be compared with ==.
   */
  NIS.SuppressState state = NIS.SuppressState.NONE;

  /**
   * information about the suppressor for the current check. This is just used for debugging
   * purposes.
   */
  /*@Nullable*/ String current_state_str = null;

  /**
   * Sample invariant - used to check the suppressor over constants. this is a prototype invariant;
   * that is, sample_inv.ppt == null.
   */
  /*@Prototype*/ Invariant sample_inv;

  /** Defines a unary suppressor. */
  public NISuppressor(int v1_index, Class<? extends Invariant> cls) {

    debug.fine(String.format("creating %s over arg %d", cls.getName(), v1_index));

    this.v1_index = v1_index;
    this.inv_class = cls;

    // Create a sample invariant
    try {
      Method get_proto = inv_class.getMethod("get_proto", new Class<?>[] {});
      @SuppressWarnings({"nullness", "prototype"}) // reflective invocation is nullness-correct
      /*@NonNull*/ /*@Prototype*/ Invariant sample_inv_local =
          (/*@Prototype*/ Invariant) get_proto.invoke(null, new Object[] {});
      sample_inv = sample_inv_local;
      assert sample_inv != null;
    } catch (Exception e) {
      throw new RuntimeException("error instantiating invariant " + inv_class.getName() + ": " + e);
    }

    debug.fine("Created " + this);
  }

  /** Defines a binary suppressor. */
  public NISuppressor(int v1_index, int v2_index, Class<? extends Invariant> cls) {

    debug.fine(String.format("creating %s over args %d and %d", cls.getName(), v1_index, v2_index));

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
      Method swap_method = cls.getMethod("swap_class", (Class<?>[]) null);
      if (swap) {
        @SuppressWarnings("nullness") // static method, so null first arg is OK: swap_class()
        Class<? extends Invariant> tmp_cls =
            asInvClass(swap_method.invoke(null, (Object /*@Nullable*/ []) null));
        cls = tmp_cls;
      }

    } catch (Exception e) {
      swap_class = false;
    }

    this.inv_class = cls;

    // Create a sample invariant, by reflectively calling either
    // get_proto(boolean) or get_proto().
    try {
      try {
        Method get_proto = inv_class.getMethod("get_proto", new Class<?>[] {boolean.class});
        @SuppressWarnings({"nullness", "prototype"}) // reflective invocation is nullness-correct
        /*@NonNull*/ /*@Prototype*/ Invariant sample_inv_local =
            (/*@Prototype*/ Invariant) get_proto.invoke(null, new Object[] {Boolean.valueOf(swap)});
        sample_inv = sample_inv_local;
      } catch (NoSuchMethodException e) {
        Method get_proto = inv_class.getMethod("get_proto", new Class<?>[] {});
        @SuppressWarnings({"nullness", "prototype"}) // reflective invocation is nullness-correct
        /*@NonNull*/ /*@Prototype*/ Invariant sample_inv_local =
            (/*@Prototype*/ Invariant) get_proto.invoke(null, new Object[] {});
        sample_inv = sample_inv_local;
      }
    } catch (Exception e) {
      throw new RuntimeException("error getting proto invariant " + inv_class.getName() + ": " + e);
    }

    assert sample_inv != null;
    debug.fine("Created " + this);
  }

  /**
   * Returns a new suppressor that is the same as this one except with its variables swapped. Unary
   * suppressors have their variable index swapped from 0 to 1 or 1 to 0.
   */
  public NISuppressor swap() {

    if (v2_index == -1) {
      int new_index = 0;
      if (v1_index == 0) new_index = 1;
      return (new NISuppressor(new_index, inv_class));
    }
    assert v3_index == -1;

    if (swap) {
      return new NISuppressor(v1_index, v2_index, inv_class);
    } else {
      return new NISuppressor(v2_index, v1_index, inv_class);
    }
  }

  /**
   * Returns whether or not this suppressor is enabled. A suppressor is enabled if the invariant on
   * which it depends is enabled.
   */
  /*@Pure*/
  public boolean is_enabled() {
    return (sample_inv.enabled());
  }

  /**
   * Returns whether or not this suppressor invariant could be instantiated over the specified
   * variables. A suppressor that canot be instantiated over the variables cannot possibly suppress.
   * Consider the NonZero invariant. It is suppressed by EqualsOne. But while NonZero is valid over
   * all variables, EqualsOne is only valid over non-pointer variables. Thus the suppression is only
   * valid over non-pointer variables.
   */
  public boolean instantiate_ok(VarInfo[] vis) {
    return sample_inv.instantiate_ok(vis);
  }

  /**
   * Sets the status of this suppressor with regards to the specified vis and falsified invariant.
   * The status consists of whether or not the suppressor is valid (true) and whether or not it
   * matches the falsified invariant.
   *
   * <p>Matching a suppressor is more complex than is apparent at first glance. The invariant AND
   * its variables must match. Since suppressors are specified without variables, the variables are
   * taken from the specified vis. The variable indices specify which variables to consider.
   *
   * <p>For example consider the suppressor {1, 2, IntLessEqual} and a vis of {x, y, z}. The
   * suppressor is true if the IntLessEqual invariant exists in the slice {y, z}. This allows
   * ternary invariants to specify exactly the suppressor required for their particular permutation
   * ofarguments. Invariants that have an internal permute variable must match that as well.
   *
   * @param ppt the top level program point
   * @param vis the slice of the suppressee. Thus, if the suppressee is ternary, vis, should specify
   *     three variables.
   * @param inv the falsified invariant. inv_match indicates whether or not inv matches this
   *     suppressor.
   * @return the state of this suppressor which is one of (NIS.SuppressState.MATCH,
   *     NIS.SuppressState.VALID, NIS.SuppressState.INVALID, NIS.SuppressState.NONSENSICAL)
   */
  public NIS.SuppressState check(PptTopLevel ppt, VarInfo[] vis, /*@Nullable*/ Invariant inv) {

    // Currently we only support unary and binary suppressors
    assert v3_index == -1;
    assert v1_index != -1;

    // If the underlying invariant is not enabled, we can't possibly be true
    if (!is_enabled()) {
      return (state = NIS.SuppressState.INVALID);
    }

    if (Debug.logDetail() && NIS.debug.isLoggable(Level.FINE)) {
      NIS.debug.fine(
          "checking suppressor "
              + this
              + " against inv "
              + ((inv != null) ? inv.format() : "null")
              + " over vars "
              + VarInfo.arrayToString(vis)
              + " in ppt "
              + ppt.name);
    }

    // If unary
    if (v2_index == -1) {

      VarInfo v1 = vis[v1_index];

      // If the underlying inariant can't be instantiated over these variables,
      // this can't possibly be true
      if (!instantiate_ok(new VarInfo[] {v1})) {
        // System.out.printf ("suppressor %s invalid over variable %s\n",
        //                   this, v1);
        return (state = NIS.SuppressState.INVALID);
      }

      // Check to see if inv matches this suppressor.  The invariant class
      // and variables must match for this to be true.  This check is only
      // needed for the falsified method.
      if (!NIS.antecedent_method) {
        if ((inv != null) && (inv.getClass() == inv_class) && (v1 == inv.ppt.var_infos[0])) {
          return (state = NIS.SuppressState.MATCH);
        }
      }

      // Check to see if the suppressor is true over all constants.
      if (ppt.is_prev_constant(v1)) {
        assert ppt.constants != null : "@AssumeAssertion(nullness)";
        boolean valid = false;
        VarInfo[] sup_vis = new VarInfo[] {v1};
        assert sample_inv.valid_types(sup_vis);
        if (sample_inv.instantiate_ok(sup_vis)) {
          UnaryInvariant uinv = (UnaryInvariant) sample_inv;
          @SuppressWarnings("nullness") // elements of ppt.constants have a constant_value
          InvariantStatus status =
              uinv.check(ppt.constants.constant_value(v1), ValueTuple.MODIFIED, 1);
          valid = (status == InvariantStatus.NO_CHANGE);
        }
        if (NIS.debug.isLoggable(Level.FINE)) NIS.debug.fine("constant args - " + valid);
        if (valid) {
          current_state_str = "true over constant " + ppt.constants.constant_value(v1);
        } else {
          current_state_str = "invalid over constant " + ppt.constants.constant_value(v1);
        }
        return (state = (valid ? NIS.SuppressState.VALID : NIS.SuppressState.INVALID));
      }

      // Check to see the variable is missing
      if (ppt.is_prev_missing(v1)) {
        current_state_str = "nonsensical";
        return (state = NIS.SuppressState.NONSENSICAL);
      }

      // Check to see if this suppressor is true.  Note that we don't check
      // to see if the invariant has been falsified.  That is because we
      // do this processing as falsified invariants are removed from the lists.
      // An invariant that is still in the list, but marked falsified, is true
      // for our purposes (we will process it later, when it is removed)
      PptSlice slice = ppt.findSlice(v1);
      if (slice != null) {
        for (Invariant slice_inv : slice.invs) {
          if (match_true(slice_inv)) {
            current_state_str = "invariant " + slice_inv.format();
            return (state = NIS.SuppressState.VALID);
          }
        }
      }
      current_state_str = "invariant not found";
      return (state = NIS.SuppressState.INVALID);

    } else /* must be binary */ {
      if (v1_index >= vis.length || v2_index >= vis.length) {
        // Stringifying "this" is expensive, so only do it if one of the
        // assertions will fail
        assert (v1_index < vis.length)
            : "v1/len= " + v1_index + "/" + vis.length + " suppressor " + this;
        assert (v2_index < vis.length)
            : "v2/len= " + v2_index + "/" + vis.length + " suppressor " + this;
      }
      VarInfo v1 = vis[v1_index];
      VarInfo v2 = vis[v2_index];

      // If the underlying inariant can't be instantiated over these variables,
      // this can't possibly be true
      if (!instantiate_ok(new VarInfo[] {v1, v2})) {
        // System.out.printf ("suppressor %s invalid over variables %s & %s\n",
        //                  this, v1, v2);
        return (state = NIS.SuppressState.INVALID);
      }

      // Check to see if inv matches this suppressor.  The invariant class,
      // variables, and swap must match for this to be true.  This check is
      // only needed in the falsified method.
      if (!NIS.antecedent_method) {
        if ((inv != null)
            && match(inv)
            && (v1 == inv.ppt.var_infos[0])
            && (v2 == inv.ppt.var_infos[1])) {
          if (NIS.debug.isLoggable(Level.FINE)) {
            NIS.debug.fine("Matches falsified inv " + inv.format());
          }
          return (state = NIS.SuppressState.MATCH);
        }
      }

      // Check to see if the suppressor is true over all constants.  This
      // code only works for stateless invariants!
      if (ppt.is_prev_constant(v1) && ppt.is_prev_constant(v2)) {
        assert ppt.constants != null : "@AssumeAssertion(nullness)";
        boolean valid = false;
        VarInfo[] sup_vis = new VarInfo[] {v1, v2};
        assert sample_inv.valid_types(sup_vis);
        if (sample_inv.instantiate_ok(sup_vis)) {
          BinaryInvariant binv = (BinaryInvariant) sample_inv;
          InvariantStatus status =
              binv.check_unordered(
                  ppt.constants.constant_value(v1),
                  ppt.constants.constant_value(v2),
                  ValueTuple.MODIFIED,
                  1);
          valid = (status == InvariantStatus.NO_CHANGE);
        }
        if (NIS.debug.isLoggable(Level.FINE)) {
          NIS.debug.fine(
              String.format(
                  "constant args (%s, %s) = %b ",
                  Debug.toString(ppt.constants.constant_value(v1)),
                  Debug.toString(ppt.constants.constant_value(v2)),
                  valid));
        }
        current_state_str =
            "true over constants "
                + ppt.constants.constant_value(v1)
                + " and "
                + ppt.constants.constant_value(v2);
        if (!valid) current_state_str = "not " + current_state_str;
        return (state = (valid ? NIS.SuppressState.VALID : NIS.SuppressState.INVALID));
      }

      // Check to see if either variable is missing
      if (ppt.is_prev_missing(v1) || ppt.is_prev_missing(v2)) {
        current_state_str = "nonsensical";
        return (state = NIS.SuppressState.NONSENSICAL);
      }

      // Check to see if this suppressor is true.  Note that we don't check
      // to see if the invariant has been falsified.  That is because we
      // do this processing as falsified invariants are removed from the lists.
      // An invariant that is still in the list, but marked falsified, is true
      // for our purposes (we will process it later, when it is removed)
      PptSlice slice = ppt.findSlice(v1, v2);
      if (slice != null) {
        for (Invariant slice_inv : slice.invs) {
          // NIS.debug.fine (": processing inv " + slice_inv.format());
          if (match_true(slice_inv)) {
            if (NIS.debug.isLoggable(Level.FINE)) {
              NIS.debug.fine(
                  "suppressor matches inv " + slice_inv.format() + " " + !slice_inv.is_false());
            }
            current_state_str = "invariant " + slice_inv.format();
            return (state = NIS.SuppressState.VALID);
          }
        }
      }
      NIS.debug.fine("suppressor not found");
      return (state = NIS.SuppressState.INVALID);
    }
  }

  /**
   * Returns true if inv matches this suppressor and the invariant is not falsified.
   *
   * @see #match(Invariant)
   */
  public boolean match_true(Invariant inv) {
    if (NIS.antecedent_method) {
      return (match(inv) && !inv.is_false());
    } else {
      return (match(inv));
    }
  }

  /**
   * Returns true if inv matches this suppressor. It is assumed that inv's variables already match
   * (i.e., that it was looked up in compatible slice).
   */
  public boolean match(Invariant inv) {

    if (v2_index == -1) {
      return (inv.getClass() == inv_class);
    } else {
      if (inv.getClass() != inv_class) {
        return false;
      }
      if (!swap_class) {
        BinaryInvariant binv = (BinaryInvariant) inv;
        return (binv.is_symmetric() || (swap == binv.get_swap()));
      }
      return true;
    }
  }

  /**
   * Returns true if the suppressee matches this suppressor. Currently only checks that the class
   * matches but this will need to be expanded to check for a permutation match as well.
   */
  public boolean match(NISuppressee sse) {

    if (v2_index == -1) {
      return (sse.sup_class == inv_class);
    } else {
      if (sse.sup_class != inv_class) {
        return false;
      }
      if (!swap_class) {
        BinaryInvariant binv = (BinaryInvariant) sse.sample_inv;
        boolean match = (binv.is_symmetric() || (swap == binv.get_swap()));
        return match;
      }
      return true;
    }
  }

  /** Returns a copy of this suppressor translated to match the variable order in sor. */
  public NISuppressor translate(NISuppressor sor) {

    int new_v1 = sor.translate_index(v1_index);
    int new_v2 = sor.translate_index(v2_index);
    int new_v3 = sor.translate_index(v3_index);

    if (new_v2 == -1) {
      return new NISuppressor(new_v1, inv_class);
    } else if (new_v3 == -1) {
      return new NISuppressor(new_v1, new_v2, inv_class);
    } else {
      throw new Error("Unexpected ternary suppressor");
    }
  }

  /** Returns the variable index that corresponds to index */
  private int translate_index(int index) {

    if (index == 0) return v1_index;
    else if (index == 1) return v2_index;
    else if (index == 2) return v3_index;
    else return index;
  }

  /** Returns the invariant class of this suppressor */
  public Class<? extends Invariant> get_inv_class() {
    return inv_class;
  }

  /** clears the state of this suppressor to NIS.none */
  public void clear_state() {
    state = NIS.SuppressState.NONE;
    current_state_str = null;
  }

  static String[] varname = new String[] {"x", "y", "z"};

  /**
   * Returns a string representation of the suppressor. Rather than show var indices as numbers, the
   * variables x, y, and z are shown instead with indices 0, 1, and 2 respectively.
   */
  /*@SideEffectFree*/
  @Override
  public String toString(/*>>>@GuardSatisfied NISuppressor this*/) {

    String cname = inv_class.getCanonicalName();

    String status;
    if (state == NIS.SuppressState.NONE) {
      status = "";
    } else {
      status = state.toString().toLowerCase();
    }

    if (current_state_str != null) status = status + " [" + current_state_str + "]";

    if (v2_index == -1) {
      return (String.format("%s(%s) [%s]", cname, varname[v1_index], status));
    } else if (v3_index == -1) {
      if (swap && !swap_class) {
        return (String.format(
            "%s(%s,%s) [%s]", cname, varname[v2_index], varname[v1_index], status));
      } else {
        return (String.format(
            "%s(%s,%s) [%s]", cname, varname[v1_index], varname[v2_index], status));
      }
    } else {
      return (String.format(
          "%s(%s,%s,%s) [%s]",
          cname, varname[v1_index], varname[v2_index], varname[v3_index], status));
    }
  }
}
