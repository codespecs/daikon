package daikon.suppress;

import daikon.*;
import daikon.inv.Invariant;

import java.util.*;
import java.io.Serializable;

import org.apache.log4j.Logger;

import utilMDE.Assert;
import utilMDE.MathMDE;
import utilMDE.UtilMDE;

/**
 * ADT for a SuppressionFactory to specify what invariants
 * (suppressors) it searches for when it calls
 * PptTopLevel.fillSuppressionTemplate.  Results of search are stored
 * in this by fillSuppressionTemplate(), including VarInfos that are
 * the transformed versions of the desired VarInfos.  Mutable.  A
 * SuppressionFactory generates a SuppressionTemplate and gives it to
 * the potential suppressee's PptTopLevel to fill.
 **/

public class SuppressionTemplate {
  // I am *not* serializable.  I should never be stored to disk.

  /**
   * General debug tracer.
   **/
  public static final Logger debug = Logger.getLogger ("daikon.suppress.SuppresionTemplate");

  /**
   * List of invariant types we want to template for.  Comparison is
   * by == rather than instanceof.  Elements can't be null.
   **/
  public Class[] invTypes;

  /**
   * List of lists of VarInfos that the corresponding invariant (in
   * invTypes) should be for.  Elements can't be null.
   **/
  public VarInfo[][] varInfos;

  /**
   * Where to put the results from a suppression search.  Elements can
   * be null.
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
   * Set results and transforms back to empty arrays.
   **/
  public void resetResults() {
    filled = false;
    results = new Invariant[0];
    transforms = new VarInfo[0][];
  }

  /**
   * Fill in the invariant and argument for unary invariants
   *
   * @param indx    index in invTypes and varInfos to set
   * @param cls     Invariant to search for
   * @param arg     Argument to invariant
   */
  public void set (int indx, Class cls, VarInfo arg) {

    invTypes[indx] = cls;
    varInfos[indx][0] = arg;
  }

  /**
   * Fill in the invariant and argument for binary invariants
   *
   * @param indx    index in invTypes and varInfos to set
   * @param cls     Invariant to search for
   * @param arg1    First invariant arg
   * @param arg2    Second invariant arg
   */
  public void set (int indx, Class cls, VarInfo arg1, VarInfo arg2) {

    invTypes[indx] = cls;
    varInfos[indx][0] = arg1;
    varInfos[indx][1] = arg2;
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
  ////////////////
  // Constructors

  /**
   * Create a new SuppressionTemplate.  Generates a blank template.
   **/
  public SuppressionTemplate () {
    invTypes = new Class[0];
    varInfos = new VarInfo[0][];
    results = new Invariant[0];
    transforms = new VarInfo[0][];
    filled = false;
  }



}
