package daikon.suppress;

import daikon.*;
import daikon.inv.*;
import daikon.inv.binary.*;
import daikon.suppress.*;
import utilMDE.*;

import java.lang.reflect.*;
import java.util.logging.Logger;
import java.util.*;

/**
 * Class that defines a single non-instantiating suppression.  A suppression
 * consists of one or more suppressors and a suppressee.  If each of the
 * suppressors is true they imply the suppressee
 */
public class NISuppression {

  /** set of suppressor invariants **/
  NISuppressor[] suppressors;

  /** suppressee invariant **/
  NISuppressee suppressee;

  public NISuppression (NISuppressor[] suppressor_set,
                        NISuppressee suppressee) {

    suppressors = suppressor_set;
    this.suppressee = suppressee;
  }

  public NISuppression (NISuppressor sup1, NISuppressee suppressee) {

    this(new NISuppressor[] {sup1}, suppressee);
  }

  public NISuppression (NISuppressor sup1, NISuppressor sup2,
                        NISuppressee suppressee) {

    this(new NISuppressor[] {sup1, sup2}, suppressee);
  }

  public NISuppression (NISuppressor sup1, NISuppressor sup2, NISuppressor sup3,
                        NISuppressee suppressee) {

    this(new NISuppressor[] {sup1, sup2, sup3}, suppressee);
  }

  public NISuppression (NISuppressor sup1, NISuppressor sup2, NISuppressor sup3,
                        NISuppressor sup4, NISuppressee suppressee) {

    this(new NISuppressor[] {sup1, sup2, sup3, sup4}, suppressee);
  }

  public NISuppression (NISuppressor sup1, NISuppressor sup2, NISuppressor sup3,
                        NISuppressor sup4, NISuppressor sup5,
                        NISuppressee suppressee) {

    this(new NISuppressor[] {sup1, sup2, sup3, sup4, sup5}, suppressee);
  }

  public Iterator suppressor_iterator() {
    return Arrays.asList(suppressors).iterator();
  }

  /**
   * Checks this suppression.  Each suppressor is checked to see
   * if it matches inv and if not, whether or not it is valid.
   * The results are saved in each suppressor.  A null inv doesn't
   * match any suppressors
   *
   * @return VALID if the suppression is valid, MISSING if one or
   *         more suppressors were missing and the rest were valid,
   *         INVALID otherwise
   */
  public String check (PptTopLevel ppt, VarInfo[] vis, Invariant inv) {

    String status = NIS.VALID;
    boolean valid = true;
    for (int i = 0; i < suppressors.length; i++) {
      NISuppressor ssor = suppressors[i];
      String st = ssor.check (ppt, vis, inv);
      if (st == NIS.MISSING)
        status = NIS.MISSING;
      else if (st != NIS.VALID) {
        status = NIS.INVALID;
      }
    }
    return (status);
  }

  /**
   * Determines whether or not the falsified invariant was the
   * first suppressor to be falsified in this suppression.  This
   * depends on check() being called previously to fill in the state
   * for each suppressor.  If the falsified invariant is not involved
   * in this suppression, then it can't have been invalidated.
   */
  public boolean invalidated() {

    // We return true when every suppressor except the falsified
    // one is valid.  Note that match can be true on more than one
    // suppressor due to reflexive (x, x, x) invariants.  In this
    // code, the suppressor should never be missing, since we should
    // have never looked at a slice with missing variables.
    boolean inv_match = false;
    for (int i = 0; i < suppressors.length; i++) {
      NISuppressor ssor = suppressors[i];
      Assert.assertTrue (ssor.state != NIS.MISSING);
      if (ssor.state == NIS.MATCH) {
        // Assert.assertTrue (!inv_match);
        inv_match = true;
      } else if (ssor.state != NIS.VALID)
        return (false);
    }
    return (inv_match);
  }

  /**
   * clears the suppressor state in each suppressor
   */
  public void clear_state () {
    for (int i = 0; i < suppressors.length; i++) {
      suppressors[i].clear_state();
    }
  }

  /**
   * Returns 'suppressor && suppressor ... => suppressee'
   */
  public String toString() {

    String out = "";

    for (int i = 0; i < suppressors.length; i++) {
      if (out != "")
        out += " && ";
      out += suppressors[i];
    }

    return (out + " ==> " + suppressee);

  }

}
