package daikon.suppress;

import daikon.*;
import daikon.inv.Invariant;
import daikon.inv.binary.twoScalar.*;

import java.util.*;
import java.io.Serializable;

import org.apache.log4j.Category;

import utilMDE.Assert;
import utilMDE.MathMDE;

/**
 * Generates SuppressionLink objects.  Responsible for checking if an
 * invariant is suppressed.  Meant to be inherited from so each
 * subclass suppresses some classes of invariants.  Factory's should
 * be immutable.
 *
 * How child SuppressionFactories are added to the system: see
 * this package's description.
 **/

public abstract class SuppressionFactory implements Serializable {


  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020722L;

  /**
   * General debug tracer.
   **/
  public static final Category debug =
    Category.getInstance ("daikon.suppress.SuppresionFactory");



  /**
   * Check whether this type of suppression applies to a givern
   * invariant at a given program point.  If so, return a
   * SuppressionLink.  Otherwise return null.  A SuppressionFactory
   * should not attempt to search through the invariants by itself,
   * but should use PptTopLevel.fillSuppressionTemplate.
   *
   * Does not link the invariants.
   * @param inv the Invariant that may be suppressed.
   * @return null if suppression method doesn't apply.
   **/

  public abstract SuppressionLink generateSuppressionLink (Invariant inv);


  /**
   * Generate a SuppressionLink by asking an inv's PptTopLevel to fill
   * in the template.  If the template filling is successful, return a
   * SuppressionLink.  Else return null.
   * @param template The template to fill
   * @param inv the Invariant that is potentially being suppressed.
   * @return can be null.
   **/
  protected SuppressionLink byTemplate (SuppressionTemplate template, Invariant inv) {
    if (inv.ppt.parent.fillSuppressionTemplate(template)) {
      return linkFromTemplate (template, inv);
    } else {
        return null;
    }
  }

  /**
   * Generate a SuppressionLink from a filled template.  This is used
   * by Factory's that fill their templates in a way that the
   * byTemplate method is inappropriate for them (e.g. a
   * SuppressionLink might not be generated for all filled templates,
   * so a Factory scans a list of filled templates).
   * @param template a filled template.
   * @param inv the Invariant that is being suppressed
   **/
  protected SuppressionLink linkFromTemplate (SuppressionTemplate template,
                                              Invariant inv) {
    Assert.assertTrue (template.filled, "Template must be filled");
    List suppressors = new ArrayList();
    suppressors.addAll (Arrays.asList(template.results));
    SuppressionLink sl = new SuppressionLink (this, inv, suppressors);
    return sl;
  }


  /**
   * Attempt to find an Invariant of v1 < v2 or v1 <= v2 to use to
   * suppress arg.  Generate the appropariate SuppressionLink if
   * found.  Used as helper by many factories, so that checking v2 >
   * v1 is the same as v1 < v2, etc.  Only works on integers because
   * this is used for array indexing.
   * @param arg The Invariant to suppress.
   * @param interval The interval that the two VarInfos have to vary by (at least).
   * @param inv The invariant whose ppt in which we are looking for
   * the < relationship.
   **/

  public SuppressionLink findLessEqual (VarInfo v1, VarInfo v2,
                                        Invariant inv, int interval) {
    
    {
      SuppressionTemplate template = new SuppressionTemplate();
      template.invTypes = new Class[] {IntLessThan.class};
      template.varInfos = new VarInfo[][] {new VarInfo[] {v1, v2}};
      inv.ppt.parent.fillSuppressionTemplate(template);
      if (template.filled) {
        IntLessThan resultInv = (IntLessThan) template.results[0];
        VarInfo leftResult = template.transforms[0][0];
        if (leftResult == resultInv.var1()
            && interval <= 0) {
          // && resultInv.interval >= interval) {
          // Not used because having IntComparison invariants keep
          // track of their interval makes them weakening invariants,
          // which hurts performance.
          return linkFromTemplate (template, inv);
        }
      }
    }

    {
      SuppressionTemplate template = new SuppressionTemplate();
      template.invTypes = new Class[] {IntLessEqual.class};
      template.varInfos = new VarInfo[][] {new VarInfo[] {v1, v2}};
      inv.ppt.parent.fillSuppressionTemplate(template);
      if (template.filled) {
        IntLessEqual resultInv = (IntLessEqual) template.results[0];
        VarInfo leftResult = template.transforms[0][0];
        if (leftResult == resultInv.var1()
            && interval <= 0) {
          // && resultInv.interval >= interval) {
          return linkFromTemplate (template, inv);
        }
      }
    }

    {
      SuppressionTemplate template = new SuppressionTemplate();
      template.invTypes = new Class[] {IntGreaterThan.class};
      template.varInfos = new VarInfo[][] {new VarInfo[] {v1, v2}};
      inv.ppt.parent.fillSuppressionTemplate(template);
      if (template.filled) {
        IntGreaterThan resultInv = (IntGreaterThan) template.results[0];
        VarInfo leftResult = template.transforms[0][0];
        if (leftResult == resultInv.var2()
            && interval <= 0) {
          // && resultInv.interval >= interval) {
          return linkFromTemplate (template, inv);
        }
      }
    }

    {
      SuppressionTemplate template = new SuppressionTemplate();
      template.invTypes = new Class[] {IntGreaterEqual.class};
      template.varInfos = new VarInfo[][] {new VarInfo[] {v1, v2}};
      inv.ppt.parent.fillSuppressionTemplate(template);
      if (template.filled) {
        IntGreaterEqual resultInv = (IntGreaterEqual) template.results[0];
        VarInfo leftResult = template.transforms[0][0];
        if (leftResult == resultInv.var2()
            && interval <= 0) {
          // && resultInv.interval >= interval) {
          return linkFromTemplate (template, inv);
        }
      }
    }

    return null;
  }

}
