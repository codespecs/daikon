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

  /**
   * Boolean.  If true, enabled non-instantiating supressions
   */
  public static boolean dkconfig_enabled = true;

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
  static Map suppressor_map = new LinkedHashMap(256);

  /**
   * List of invariants that are newly created.  This list is cleared
   * by apply_samples()
   */
  public static List new_invs = new ArrayList();

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
    List all_suppressions = FunctionBinary.get_all_ni_suppressions();
    all_suppressions.addAll (FunctionBinaryFloat.get_all_ni_suppressions());

    // map suppressor classes to suppression sets
    for (Iterator i = all_suppressions.iterator(); i.hasNext(); ) {
      NISuppressionSet suppression_set = (NISuppressionSet) i.next();
      suppression_set.add_to_suppressor_map (suppressor_map);
    }

    if (debug.isLoggable (Level.FINE))
      dump (debug);
  }

  /**
   * Instantiates any invariants that are no longer suppressed because
   * inv has been falsified.
   */
  public static void falsified (Invariant inv) {

    if (!dkconfig_enabled)
      return;

    // if (debug.isLoggable (Level.FINE))
    //  debug.fine ("inv " + inv.format() + " falsified");

    // Get the suppressor sets (if any) associated with this invariant
    List ss_list = (List) suppressor_map.get(inv.getClass());
    if (ss_list == null)
      return;

    // Process each suppression set
    for (Iterator i = ss_list.iterator(); i.hasNext(); ) {
      NISuppressionSet ss = (NISuppressionSet) i.next();
      ss.clear_state();
      if (debug.isLoggable (Level.FINE))
        debug.fine ("processing suppression set " + ss + " over falsified inv "
                    + inv.format());
      ss.falsified (inv, new_invs);
    }
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
      } else {
        if (Daikon.dkconfig_internal_check)
          Assert.assertTrue (inv.ppt.parent.findSlice(inv.ppt.var_infos)
                              == inv.ppt);
        inv.ppt.addInvariant (inv);
        if (Debug.logOn())
          inv.log (inv.format() + " added to slice");
      }
    }

    new_invs.clear();
  }

  /**
   * Dump out the suppressor map.
   */

  public static void dump (Logger debug) {

    if (!debug.isLoggable(Level.FINE))
      return;

    for (Iterator i = suppressor_map.keySet().iterator(); i.hasNext(); ) {
      Class sclass = (Class) i.next();
      List suppression_set_list = (List) suppressor_map.get (sclass);
      for (ListIterator j = suppression_set_list.listIterator(); j.hasNext();) {
        NISuppressionSet ss = (NISuppressionSet) j.next();
        if (j.previousIndex() > 0)
          debug.fine (Fmt.spf ("        : %s", ss));
        else
          debug.fine (Fmt.spf ("%s: %s", sclass, ss));
      }
    }
  }



}
