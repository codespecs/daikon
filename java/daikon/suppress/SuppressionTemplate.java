package daikon.suppress;

import daikon.*;
import daikon.inv.Invariant;

import java.util.*;
import java.io.Serializable;

import org.apache.log4j.Category;

import utilMDE.Assert;
import utilMDE.MathMDE;

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
  public static final Category debug = Category.getInstance ("daikon.suppress.SuppresionTemplate");

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

  //////////////
  // Accessors


  /**
   * Debug output only.
   **/
  public String toString() {
    return "SuppressionTemplate: " + invTypes + " " + varInfos + " " + results;
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
