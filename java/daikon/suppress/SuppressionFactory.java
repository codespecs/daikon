package daikon.suppress;

import daikon.*;
import daikon.inv.Invariant;
import daikon.inv.binary.twoScalar.*;

import utilMDE.*;

import java.util.*;
import java.util.logging.*;
import java.io.Serializable;


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
  static final long serialVersionUID = 20031024L;

  /**
   * General debug tracer.
   **/
  public static final Logger debug =
    Logger.getLogger ("daikon.suppress.SuppresionFactory");


  // The default gives the hashcode, which is bad for comparisons.
  public String toString() {
    return this.getClass().getName();
  }


  /**
   * Check whether this type of suppression applies to a given
   * invariant at a given program point.  If so, return a
   * SuppressionLink.  Otherwise return null.  A SuppressionFactory
   * should not attempt to search through the invariants by itself,
   * but should use SuppressionTemplate.fill.
   *
   * Does not link the invariants.
   * @param inv the Invariant that may be suppressed.
   * @return null if suppression method doesn't apply.
   **/

  public abstract SuppressionLink generateSuppressionLink (Invariant inv);


  /**
   * Generate a SuppressionLink from an unfilled template, by asking an
   * inv's PptTopLevel to fill in the template.  If the template filling is
   * successful, return a SuppressionLink.
   *
   * @param supt The suppression template to fill
   * @param inv the Invariant that is potentially being suppressed.
   * @return the matching suppression link or null.
   **/
  public SuppressionLink linkFromUnfilledTemplate (SuppressionTemplate supt,
                                                   Invariant inv) {
    Assert.assertTrue(supt.filled == false);
    if (inv.logOn())
      inv.log ("Suppression Template - " + supt.searchString());
    SuppressionLink result = null;
    if (supt.fill(inv.ppt.parent)) {
      result = linkFromFilledTemplate (supt, inv);
    }
    if (inv.logOn()) {
      if (result != null) {
        inv.log ("Found Template Match " + supt.results[0].format());
      } else {
        inv.log ("No Template Match found");
      }
    }
    return result;
  }


  /**
   * Generate a SuppressionLink from a filled template.  Never returns null.
   * This is used by Factory's that fill their templates in a way that the
   * linkFromUnfilledTemplate method is inappropriate for them (e.g. a
   * SuppressionLink might not be generated for all filled templates,
   * so a Factory scans a list of filled templates).
   * @param supTemplate a filled template.
   * @param inv the Invariant that is being suppressed
   **/
  protected SuppressionLink linkFromFilledTemplate (SuppressionTemplate supTemplate,
                                                    Invariant inv) {
    Assert.assertTrue (supTemplate.filled, "Template must be filled");
    SuppressionLink sl = new SuppressionLink (this, inv, supTemplate.results);
    return sl;
  }


  private transient SuppressionTemplate supTemplate_findLessEqual = new SuppressionTemplate(1);

  /**
   * Attempt to find an Invariant of v1 < v2 or v1 <= v2 to use to
   * suppress arg.  Generate the appropariate SuppressionLink if
   * found.  Used as helper by many factories, so that checking v2 >
   * v1 is the same as v1 < v2, etc.  Only works on integers because
   * this is used for array indexing.
   * @param inv The invariant whose ppt in which we are looking for
   * the < relationship.
   * @param interval The interval that the two VarInfos have to vary by (at least).
   **/

  public SuppressionLink findLessEqual (VarInfo v1, VarInfo v2,
                                        Invariant inv, int interval) {

    SuppressionTemplate supTemplate = supTemplate_findLessEqual;
    {
      supTemplate.set(0, IntLessThan.class, v1, v2);
      supTemplate.fill(inv.ppt.parent);
      if (supTemplate.filled) {
        IntLessThan resultInv = (IntLessThan) supTemplate.results[0];
        VarInfo leftResult = supTemplate.transforms[0][0];
        if (leftResult == resultInv.var1()
            && interval <= 0) {
          // && resultInv.interval >= interval) {
          // Not used because having IntComparison invariants keep
          // track of their interval makes them weakening invariants,
          // which hurts performance.
          return linkFromFilledTemplate (supTemplate, inv);
        }
      }
    }

    {
      supTemplate.set(0, IntLessEqual.class, v1, v2);
      supTemplate.fill(inv.ppt.parent);
      if (supTemplate.filled) {
        IntLessEqual resultInv = (IntLessEqual) supTemplate.results[0];
        VarInfo leftResult = supTemplate.transforms[0][0];
        if (leftResult == resultInv.var1()
            && interval <= 0) {
          // && resultInv.interval >= interval) {
          return linkFromFilledTemplate (supTemplate, inv);
        }
      }
    }

    {
      supTemplate.set(0, IntGreaterThan.class, v1, v2);
      supTemplate.fill(inv.ppt.parent);
      if (supTemplate.filled) {
        IntGreaterThan resultInv = (IntGreaterThan) supTemplate.results[0];
        VarInfo leftResult = supTemplate.transforms[0][0];
        if (leftResult == resultInv.var2()
            && interval <= 0) {
          // && resultInv.interval >= interval) {
          return linkFromFilledTemplate (supTemplate, inv);
        }
      }
    }

    {
      supTemplate.set(0, IntGreaterEqual.class, v1, v2);
      supTemplate.fill(inv.ppt.parent);
      if (supTemplate.filled) {
        IntGreaterEqual resultInv = (IntGreaterEqual) supTemplate.results[0];
        VarInfo leftResult = supTemplate.transforms[0][0];
        if (leftResult == resultInv.var2()
            && interval <= 0) {
          // && resultInv.interval >= interval) {
          return linkFromFilledTemplate (supTemplate, inv);
        }
      }
    }

    return null;
  }

}
