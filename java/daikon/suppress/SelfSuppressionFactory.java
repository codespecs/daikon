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
 * Generic Factory that suppresses invariants in upper ppts that are
 * identical to it.
 **/

public class SelfSuppressionFactory extends SuppressionFactory  {

  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020801;

  /**
   * General debug tracer.
   **/
  public static final Category debug =
    Category.getInstance ("daikon.suppress.SelfSuppresionFactory");

  private static final SelfSuppressionFactory theInstance =
    new SelfSuppressionFactory();

  public static SelfSuppressionFactory getInstance() {
    return theInstance;
  }

  private SelfSuppressionFactory() {

  }
  
  public SuppressionLink generateSuppressionLink (Invariant inv) {
    PptSlice slice = inv.ppt;
    SuppressionTemplate template = new SuppressionTemplate();
    template.invTypes = new Class[] {inv.getClass()};
    template.varInfos = new VarInfo[][] {slice.var_infos};
    slice.parent.fillSuppressionTemplate (template, false);
    // Yeah, the argument has to be false, because otherwise we'll
    // suppress ourselves in the same ppt
    if (template.filled && template.results[0].isSameFormula(inv)) {
      Assert.assertTrue (template.transforms[0][0] != template.varInfos[0][0]);
      if (debug.isDebugEnabled()) {
        debug.debug ("Self template filled: " + template);
      }
      return linkFromTemplate (template, inv);
    } else {
      return null;
    }    
  }



}
