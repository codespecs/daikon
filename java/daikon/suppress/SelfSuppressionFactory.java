package daikon.suppress;

import daikon.*;
import daikon.inv.Invariant;
import daikon.inv.binary.twoScalar.*;

import java.util.*;
import java.io.Serializable;

import org.apache.log4j.Logger;

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
  public static final Logger debug =
    Logger.getLogger ("daikon.suppress.SelfSuppresionFactory");

  private static final SelfSuppressionFactory theInstance =
    new SelfSuppressionFactory();

  public static SelfSuppressionFactory getInstance() {
    return theInstance;
  }

  private SelfSuppressionFactory() {
    supTemplate = new SuppressionTemplate();
    supTemplate.invTypes = new Class[1];
    supTemplate.varInfos = new VarInfo[1][];
  }

  private transient SuppressionTemplate supTemplate;

  public SuppressionLink generateSuppressionLink (Invariant inv) {
    if (debug.isDebugEnabled()) {
      debug.debug ("Attempting on: " + inv.repr());
      debug.debug ("  in ppt     : " + inv.ppt.parent.ppt_name);
    }
    if (inv.logOn())
      inv.log ("Attempting self suppression with sample count: " + inv.ppt.num_samples());

    PptSlice slice = inv.ppt;

    supTemplate.resetResults();
    supTemplate.invTypes[0] = inv.getClass();
    supTemplate.varInfos[0] = slice.var_infos;
    slice.parent.fillSuppressionTemplate (supTemplate, false);
    if (inv.logOn()) {
      inv.log ("Search for" + supTemplate.searchString());
      if (supTemplate.filled)
        inv.log ("Found " + supTemplate.results[0].format() + " in "
                  + supTemplate.results[0].ppt.ppt_name.getFullNamePoint());
    }
    // Yeah, the argument has to be false, because otherwise we'll
    // suppress ourselves in the same ppt
    if (supTemplate.filled && supTemplate.results[0].isSameFormula(inv)) {
      Assert.assertTrue (supTemplate.transforms[0][0] != supTemplate.varInfos[0][0]);
      if (inv.logOn()) {
        inv.log ("  Self template filled:");
        inv.log ("  suppressee: " + inv.repr());
        inv.log ("      in ppt: " + inv.ppt.parent.name);
        inv.log ("  with      : " + supTemplate.results[0].repr());
        inv.log ("      in ppt: " + supTemplate.results[0].ppt.parent.name);
      }
      return linkFromTemplate (supTemplate, inv);
    } else {
      if (supTemplate.filled) {
        inv.log ("Not same formula, returning null");
      }
      return null;
    }
  }



}
