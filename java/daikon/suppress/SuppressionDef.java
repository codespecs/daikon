package daikon.suppress;

import daikon.*;
import daikon.inv.*;
import daikon.inv.Invariant;
import daikon.inv.binary.twoScalar.*;
import daikon.inv.unary.scalar.*;
import daikon.inv.unary.sequence.*;
import daikon.derive.binary.*;


import utilMDE.*;

import java.util.*;
import java.util.logging.*;
import java.io.Serializable;

/**
 * Standard suppression definition.  Looks for one or more
 * Invariant definitions to perform the suppression
 */
public class SuppressionDef extends SuppressionFactory {

  private transient InvDef inv_def1;
  private transient InvDef inv_def2;
  private transient SuppressionTemplate sup_template;

  public SuppressionDef (InvDef inv_def1) {
    this.inv_def1 = inv_def1;
    sup_template = new SuppressionTemplate (1);
    inv_def1.set (sup_template, 0);
  }

  public SuppressionDef (InvDef inv_def1, InvDef inv_def2) {
    this.inv_def1 = inv_def1;
    this.inv_def2 = inv_def2;
    sup_template = new SuppressionTemplate (2);
    inv_def1.set (sup_template, 0);
    inv_def2.set (sup_template, 1);
  }

  public SuppressionLink generateSuppressionLink (Invariant inv) {

    sup_template.resetResults();
    SuppressionLink sl = linkFromUnfilledTemplate (sup_template, inv);

    if (sl == null)
      return (null);

    if (!inv_def1.check (sup_template.results[0]))
      return (null);

    if ((inv_def2 != null) && !inv_def2.check (sup_template.results[1]))
      return (null);

    return (sl);
  }

}
