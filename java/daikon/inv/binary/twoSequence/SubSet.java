package daikon.inv.binary.twoSequence;

import daikon.*;
import daikon.inv.*;
import daikon.derive.*;
import daikon.derive.binary.*;
import daikon.inv.unary.sequence.EltOneOf;
import daikon.VarInfoName.QuantHelper;
import daikon.VarInfoName.QuantHelper.QuantifyReturn;

import java.util.*;
import utilMDE.*;
import org.apache.log4j.Category;

public class SubSet
  extends TwoSequence
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  private static final Category debug =
    Category.getInstance("daikon.inv.binary.twoSequence.SubSet");

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /**
   * Boolean.  True iff SubSet invariants should be considered.
   **/
  public static boolean dkconfig_enabled = false;

  public boolean var1_in_var2 = true;
  public boolean var2_in_var1 = true;

  protected SubSet(PptSlice ppt) {
    super(ppt);
  }

  public static SubSet instantiate(PptSlice ppt) {
    if (!dkconfig_enabled) return null;

    VarInfo var1 = ppt.var_infos[0];
    VarInfo var2 = ppt.var_infos[1];
    // System.out.println("SubSet.isObviousDerived(" + format() + ") = "
    //                    + ((SubSet.isObviousDerived(var1(), var2()))
    //                       || (SubSet.isObviousDerived(var2(), var1()))));
    if ((SubSet.isObviousDerived(var1, var2))
        || (SubSet.isObviousDerived(var2, var1))) {
      Global.implied_noninstantiated_invariants++;
      if (debug.isDebugEnabled()) {
        debug.debug (var1 + ", " + var2);
        debug.debug ("Obvious derived, returning null");
      }
      return null;
    }


    if (debug.isDebugEnabled()) {
      debug.debug ("Instantiating " + var1.name + " and " + var2.name);
    }

    return new SubSet(ppt);
  }

  protected Invariant resurrect_done_swapped() {
    // was a swap
    boolean tmp = var1_in_var2;
    var1_in_var2 = var2_in_var1;
    var2_in_var1 = tmp;
    return this;
  }

  public String repr() {
    return "SubSet" + varNames() + ": "
      + "1in2=" + var1_in_var2
      + ",2in1=" + var2_in_var1
      + ",falsified=" + falsified;
  }

  public String format_using(OutputFormat format) {
    if (format == OutputFormat.DAIKON) return format();
    if (format == OutputFormat.IOA) return format_ioa();
    if (format == OutputFormat.JAVA) return format();
    if (format == OutputFormat.ESCJAVA) return format_esc();

    return format_unimplemented(format);
  }

  public String format() {
    String v1 = var1().name.name();
    String v2 = var2().name.name();
    if (var1_in_var2 && var2_in_var1) {
      return v1 + " is a {sub,super}set of " + v2;
    } else {
      String subvar = (var1_in_var2 ? v1 : v2);
      String supervar = (var1_in_var2 ? v2 : v1);
      return subvar + " is a subset of " + supervar;
    }
  }

  /* IOA */
  public String format_ioa() {
    String result;
    String v1 = var1().name.ioa_name();
    String v2 = var2().name.ioa_name();
    if (var1_in_var2 && var2_in_var1) {
      result = "("+v1+" \\subseteq "+v2+") /\\ ("+v2+" \\subseteq "+v1+")";
    } else {
      String subvar = (var1_in_var2 ? v1 : v2);
      String supervar = (var1_in_var2 ? v2 : v1);
      result = subvar + " \\subseteq " + supervar;
    }

    if (var1().isIOAArray() || var2().isIOAArray()) {
      // Temporarily disabled because IOA frontend outputs sets as
      // arrays for comparability.

      // result += " *** (Invalid syntax for arrays)";
    }

    return result;
  }

  public String format_esc() {
    String classname = this.getClass().toString().substring(6); // remove leading "class"
    return "warning: method " + classname + ".format_esc() needs to be implemented: " + format();
  }

  public String format_simplify() {
    String classname = this.getClass().toString().substring(6); // remove leading "class"
    return "warning: method " + classname + ".format_esc() needs to be implemented: " + format();
  }

  public void add_modified(long[] a1, long[] a2, int count) {
    if (var1_in_var2 && (!ArraysMDE.isSubset(a1, a2))) {
      var1_in_var2 = false;
      if (!var2_in_var1) {
        destroy();
        return;
      }
    }
    if (var2_in_var1 && (!ArraysMDE.isSubset(a2, a1))) {
      var2_in_var1 = false;
      if (!var1_in_var2) {
        destroy();
        return;
      }
    }
    Assert.assert(var1_in_var2 || var2_in_var1);
  }


  protected double computeProbability() {
    if (falsified)
      return Invariant.PROBABILITY_NEVER;
    else if (var1_in_var2 && var2_in_var1)
      return Invariant.PROBABILITY_UNJUSTIFIED;
    else
      return Invariant.PROBABILITY_JUSTIFIED;
  }

  // Convenience name to make this easier to find.
  public static boolean isObviousSubSet(VarInfo subvar, VarInfo supervar) {
    return isObviousDerived(subvar, supervar);
  }

  // This is abstracted out so it can be called by SuperSequence as well.
  public static boolean isObviousDerived(VarInfo subvar, VarInfo supervar) {
    return SubSequence.isObviousDerived (subvar, supervar);
  }


  // Look up a previously instantiated SubSet relationship.
  public static SubSet find(PptSlice ppt) {
    Assert.assert(ppt.arity == 2);
    for (Iterator itor = ppt.invs.iterator(); itor.hasNext(); ) {
      Invariant inv = (Invariant) itor.next();
      if (inv instanceof SubSet)
        return (SubSet) inv;
    }
    return null;
  }


  // Two ways to go about this:
  //   * look at all subseq relationships, see if one is over a variable of
  //     interest
  //   * look at all variables derived from the

  // (Seems overkill to check for other transitive relationships.
  // Eventually that is probably the right thing, however.)
  public boolean isObviousImplied() {

    // System.out.println("checking isObviousImplied for: " + format());

    if (var1_in_var2 && var2_in_var1) {
      // Suppress this invariant; we should get an equality invariant from
      // elsewhere.
      return true;
    } else {
      return false;
    }
  }


  public boolean isSameFormula(Invariant other)
  {
    Assert.assert(other instanceof SubSet);
    return true;
  }

}
