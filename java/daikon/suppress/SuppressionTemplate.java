package daikon.suppress;

import daikon.*;
import daikon.inv.Invariant;

import java.util.logging.Logger;
import java.util.logging.Level;

import utilMDE.*;

/**
 * A SuppressionTemplate represents a set of invariants; it specifies the
 * types of the invariants and their VarInfos.  A SuppressionTemplate is
 * mutable, and is intended to be used only as a temporary data structure.
 * <p>
 *
 * SuppressionFactory uses a SuppressionTemplate to specify what invariants
 * (suppressors) it searches for.  SuppressionTemplate.fill does the actual
 * search, side-effecting this.  The invariants themselves may be found at
 * the given program point or any point higher in the hierarchy.  After
 * calling SuppressionTemplate.fill, additional checks may be performed; if
 * they are passed, then a SuppressionLink can be generated from the
 * SuppressionTemplate.
 **/

public class SuppressionTemplate {
  // I am *not* serializable.  I should never be stored to disk.

  /**
   * General debug tracer.
   **/
  public static final Logger debug = Logger.getLogger ("daikon.suppress.SuppresionTemplate");

  /** Debug tracer for SuppressionTemplate.fill. **/
  public static final Logger debugSuppressFill =
    Logger.getLogger ("daikon.suppress.fill");

  /**
   * List of invariant types we want to template for.  Comparison is
   * by == rather than instanceof.  Elements can't be null.
   * Use the "set" method to set its elements.
   **/
  private Class[] invTypes;

  /**
   * List of lists of VarInfos that the corresponding invariant (in
   * invTypes) should be for.  Elements can't be null.
   * Use the "set" method to set its elements.
   **/
  private VarInfo[][] varInfos;

  /**
   * The results from a suppression search -- that is, the suppressees.
   * Elements can be null (if the search was unsuccessful).
   **/
  public Invariant[] results;

  /**
   * The VarInfo that each VarInfo in varInfos maps to, when
   * transformed for results.  Elements can be null.
   **/
  public VarInfo[][] transforms;

  /**
   * Whether the template was successfully filled.
   **/
  public boolean filled;


  /**
   * Create a new SuppressionTemplate.  Generates a blank template.
   **/
  public SuppressionTemplate (int numInvs) {
    invTypes = new Class[numInvs];
    varInfos = new VarInfo[numInvs][];
    results = new Invariant[numInvs];
    transforms = new VarInfo[numInvs][];
    filled = false;
  }


  /**
   * Set results and transforms back to empty arrays.
   **/
  public void resetResults() {
    filled = false;
    results = new Invariant[0];
    transforms = new VarInfo[0][];
  }


  /**
   * Fill in the invariant and variable for unary invariants
   *
   * @param index    index in invTypes and varInfos to set
   * @param cls     Invariant to search for
   * @param var     The invariant's first VarInfo
   */
  public void set (int index, Class cls, VarInfo var) {
    Assert.assertTrue(cls != null);
    Assert.assertTrue(var != null);
    filled = false;
    invTypes[index] = cls;
    if (varInfos[index] == null) {
      varInfos[index] = new VarInfo[1];
    } else {
      Assert.assertTrue(varInfos[index].length == 1);
    }
    varInfos[index][0] = var;
  }

  /**
   * Fill in the invariant and variables for binary invariants
   *
   * @param index    index in invTypes and varInfos to set
   * @param cls     Invariant to search for
   * @param var1    The invariant's first VarInfo
   * @param var2    The invariant's second VarInfo
   */
  public void set (int index, Class cls, VarInfo var1, VarInfo var2) {
    Assert.assertTrue(cls != null);
    Assert.assertTrue(var1 != null);
    Assert.assertTrue(var2 != null);
    filled = false;
    invTypes[index] = cls;
    if (varInfos[index] == null) {
      varInfos[index] = new VarInfo[2];
    } else {
      Assert.assertTrue(varInfos[index].length == 2);
    }
    varInfos[index][0] = var1;
    varInfos[index][1] = var2;
  }

  /**
   * Fill in the invariant and variables for ternary invariants
   *
   * @param index    index in invTypes and varInfos to set
   * @param cls     Invariant to search for
   * @param var1    The invariant's first VarInfo
   * @param var2    The invariant's second VarInfo
   * @param var3    The invariant's third VarInfo
   */
  public void set (int index, Class cls, VarInfo var1, VarInfo var2, VarInfo var3) {
    Assert.assertTrue(cls != null);
    Assert.assertTrue(var1 != null);
    Assert.assertTrue(var2 != null);
    Assert.assertTrue(var3 != null);
    filled = false;
    invTypes[index] = cls;
    if (varInfos[index] == null) {
      varInfos[index] = new VarInfo[3];
    } else {
      Assert.assertTrue(varInfos[index].length == 3);
    }
    varInfos[index][0] = var1;
    varInfos[index][1] = var2;
    varInfos[index][1] = var3;
  }

  /**
   * Fill in the invariant and variables
   *
   * @param index    index in invTypes and varInfos to set
   * @param cls     Invariant to search for
   * @param var_infos Array of the invariant's VarInfos
   */
  public void set (int index, Class cls, VarInfo[] var_infos) {
    Assert.assertTrue(cls != null);
    Assert.assertTrue(! ArraysMDE.any_null(var_infos));
    filled = false;
    invTypes[index] = cls;
    if (varInfos[index] == null) {
      varInfos[index] = new VarInfo[var_infos.length];
    } else {
      Assert.assertTrue(varInfos[index].length == var_infos.length);
    }
    System.arraycopy(var_infos, 0, varInfos[index], 0, var_infos.length);
  }


  //////////////
  // Accessors

  /**
   * Debug output only.
   **/
  public String toString() {
    return "SuppressionTemplate: " + invTypes + " " + varInfos + " " + results;
  }

  /**
   * Returns a description of the searched for invariant(s).  The description
   * is of the form Invariant(arg1, arg2, arg3) || Invariant(arg1, arg2, arg3).
   * For debug purposes.  Note that invTypes and VarInfos must be filled in.
   */

  public String searchString() {
    // Only for debugging output, so inefficient string concatenations are OK.
    String str = "";

    for (int i = 0; i < invTypes.length; i++) {
      if (i > 0)
        str += " || ";
      str += UtilMDE.replaceString (invTypes[i].getName(),
                        invTypes[i].getPackage().getName() + ".", "")  + "(";
      for (int j = 0; j < varInfos[i].length; j++) {
        if (j > 0)
          str += ", ";
        str += varInfos[i][j].name.name();
      }
      str += ")";
    }

    return (str);
  }


  ///////////////////////////////////////////////////////////////////////////
  /// Filling suppression templates
  ///

  /**
   * Attempt to fill this SuppressionTemplate with invariants.  If
   * successful, returns true.  Called by SuppressionFactory.
   **/
  public boolean fill (PptTopLevel ppt) {
    return fill (ppt, true);
  }


  /**
   * Attempt to fill this SuppressionTemplate with invariants.  If
   * successful, returns true.  Called by SuppressionFactory.
   * @param checkSelf Whether to check in this ppt.  When false, skip
   * scanning this ppt.  This is useful for detecting identical
   * invariants (due to weakening) across ppts.
   **/
  public boolean fill (PptTopLevel ppt, boolean checkSelf) {
    // We do two loops for performance: attempt to fill locally, then
    // attempt to fill using upper ppts.
    SuppressionTemplate supTemplate = this;

    // boolean firstLoopFilled = false;
    supTemplate.filled = false;
    supTemplate.results = new Invariant[supTemplate.invTypes.length];
    supTemplate.transforms = new VarInfo[supTemplate.invTypes.length][];
    Assert.assertTrue (supTemplate.invTypes.length == supTemplate.varInfos.length,
                       "Template varInfos and invariant slots must be equal");
    debugSuppressFill.fine ("Starting template fill");

    // This is useful if this code is getting called more than expected.
    // System.out.println ("suppressionTemplate: " + supTemplate.searchString()
    //                    + " ppt: " + name());
    // Throwable stack = new Throwable("debug traceback");
    // stack.fillInStackTrace();
    // stack.printStackTrace();

    if (checkSelf) {
      firstLoop:
      // debugSuppressFill.fine ("  Entering first loop");
      for (int iInvs = 0; iInvs < supTemplate.invTypes.length; iInvs++) {
        supTemplate.results[iInvs] = null;
        Class clazz = supTemplate.invTypes[iInvs];
        VarInfo[] varInfos = supTemplate.varInfos[iInvs];
        PptSlice slice = ppt.findSlice_unordered (varInfos);
        if (slice != null) {
          // Here's where we actually find the potential invariant.  There are
          // two choices here: suppressed invariants can do more suppression, or
          // they can be forbidden to suppress others.
          Invariant inv =
            Daikon.suppress_with_suppressed ?
            Invariant.find (clazz, slice) :
            Invariant.findUnsuppressed (clazz, slice);
          if (inv != null) {
            // firstLoopFilled = true;
            supTemplate.results[iInvs] = inv;
            supTemplate.transforms[iInvs] = supTemplate.varInfos[iInvs];
          }
        }
      }
      // Formerly, we used to return null if the first loop didn't get
      // at least one invariant.  But there are some types of
      // suppression where this optimization would lower suppression
      // results.  For example, to show that
      // OBJECT:::NoDuplicates(this.array) implies
      // Method::NoDuplicates(this.array[0..i]), we need to search in
      // the OBJECT ppt, because the Method ppt won't have the
      // NoDuplicates(this.array) invariant, as it's in the OBJECT
      // ppt.  This is where i is a parameter.
      // if (!firstLoopFilled) return false;
    }

    if (Debug.logDetail())
      Debug.log (ppt.getClass(), ppt, supTemplate.varInfos[0],
                 ((ppt.dataflow_ppts == null) ? 0 : ppt.dataflow_ppts.length)
                 + " dataflow points to process ");

    // debugSuppressFill.fine ("  Entering second loop: ");
    secondLoop:
    for (int iInvs = 0; iInvs < supTemplate.invTypes.length; iInvs++) {
      Class clazz = supTemplate.invTypes[iInvs];
      //       if (debugSuppressFill.isLoggable(Level.FINE)) {
      //         debugSuppressFill.fine ("  InvType: " + clazz);
      //       }
      if (Daikon.dkconfig_df_bottom_up || ppt.dataflow_ppts == null) {
        // debugSuppressFill.fine ("  No dataflow_ppts");
        break;
      }
      if (supTemplate.results[iInvs] != null) {
        // debugSuppressFill.fine ("  Already filled");
        continue secondLoop;
      }

      VarInfo[] varInfos = supTemplate.varInfos[iInvs];


      forEachTransform:
      // Transform the VarInfos for each upper ppt
      // We go backwards so that we get the strongest invariants first.
      for (int iPpts = ppt.dataflow_ppts.length - (checkSelf ? 1 : 2);
           iPpts >= 0; iPpts--) {
        PptTopLevel dataflowPpt = ppt.dataflow_ppts[iPpts];
       if (Debug.logDetail() || debugSuppressFill.isLoggable(Level.FINE))
         Debug.log (debugSuppressFill, ppt.getClass(), ppt, varInfos,
                          "  Flow ppt: " + dataflowPpt.name);
        int[] dataflowTransform = ppt.dataflow_transforms[iPpts];
        if (Debug.logDetail()) {
          String new_vars = "";
          for (int ii = 0; ii < dataflowPpt.var_infos.length; ii++)
            new_vars += dataflowPpt.var_infos[ii].name.name() + " ";
          String cur_vars = "";
          for (int ii = 0; ii < ppt.var_infos.length; ii++)
            cur_vars += ppt.var_infos[ii].name.name() + " ";
          Debug.log (ppt.getClass(), ppt, varInfos, "dataflow transforms = "
                         + ArraysMDE.toString (dataflowTransform)
                         + ": new_vars = " + new_vars
                         + ": cur vars = " + cur_vars);
        }
        VarInfo[] newVarInfos = new VarInfo[varInfos.length];
        forEachVarInfo:
        for (int iVarInfos = 0; iVarInfos < varInfos.length; iVarInfos++) {
          int newIndex = dataflowTransform[varInfos[iVarInfos].varinfo_index];
          if (newIndex >= 0) {
            newVarInfos[iVarInfos] = dataflowPpt.var_infos[newIndex];
            if (Debug.logDetail() || debugSuppressFill.isLoggable(Level.FINE))
              Debug.log (debugSuppressFill, ppt.getClass(), ppt, varInfos,
                           "transformed "
                           + varInfos[iVarInfos].name.name() + " to "
                           + newVarInfos[iVarInfos].name.name());
          } else {
            continue forEachTransform;
          }
        }

        PptSlice slice = dataflowPpt.findSlice_unordered (newVarInfos);
        if (Debug.logDetail())
          Debug.log (ppt.getClass(), ppt, varInfos, "found slice = " + slice
                    + " Looking for class " + clazz);
        if (slice != null) {
          Invariant inv =
            Daikon.suppress_with_suppressed ?
            Invariant.find (clazz, slice) :
            Invariant.findUnsuppressed (clazz, slice);
          if (Debug.logDetail())
            Debug.log (ppt.getClass(), ppt, varInfos, "Found invariant " + inv);
          if (inv != null) {
            supTemplate.results[iInvs] = inv;
            supTemplate.transforms[iInvs] = newVarInfos;
            break;
          }
        }
      }
    }

    // Only for checking if template got filled
    thirdLoop:
    for (int iInvs = 0; iInvs < supTemplate.invTypes.length; iInvs++) {
      if (supTemplate.results[iInvs] == null) {
        debugSuppressFill.fine ("  Unsuccessful template fill");
        return false;
      }
    }

    supTemplate.filled = true;
    debugSuppressFill.fine ("  Successful template fill");
    return true;
  }

}
