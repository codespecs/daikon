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
    template = new SuppressionTemplate();
    template.invTypes = new Class[1];
    template.varInfos = new VarInfo[1][];
  }
  
  private transient SuppressionTemplate template;

  public SuppressionLink generateSuppressionLink (Invariant inv) {
    if (debug.isDebugEnabled()) {
      debug.debug ("Attempting on: " + inv.repr());
      debug.debug ("  in ppt     : " + inv.ppt.parent);
    }

    PptSlice slice = inv.ppt;
  
    template.resetResults();
    template.invTypes[0] = inv.getClass();
    template.varInfos[0] = slice.var_infos;
    slice.parent.fillSuppressionTemplate (template, false);
    // Yeah, the argument has to be false, because otherwise we'll
    // suppress ourselves in the same ppt
    if (template.filled && template.results[0].isSameFormula(inv)) {
      Assert.assertTrue (template.transforms[0][0] != template.varInfos[0][0]);
      if (debug.isDebugEnabled()) {
        debug.debug ("Self template filled:");
        debug.debug ("  suppressee: " + inv.repr());
        debug.debug ("      in ppt: " + inv.ppt.parent.name);
        debug.debug ("  with      : " + template.results[0].repr());
        debug.debug ("      in ppt: " + template.results[0].ppt.parent.name);
      }
      return linkFromTemplate (template, inv);
    } else {
      return null;
    }    
  }



}
