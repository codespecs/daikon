package daikon.inv.binary.twoSequence;

import daikon.*;
import daikon.inv.*;
import utilMDE.Assert;
import org.apache.log4j.Category;


/**
 * Where one sequence is the reverse of another.
 **/

public class Reverse
  extends TwoSequence
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  public static final Category debug =
    Category.getInstance("daikon.inv.binary.twoSequence.Reverse");

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /**
   * Boolean.  True iff Reverse invariants should be considered.
   **/
  public static boolean dkconfig_enabled = true;

  protected Reverse(PptSlice ppt) {
    super(ppt);
  }

  public static Reverse instantiate(PptSlice ppt) {
    if (!dkconfig_enabled) return null;
    Reverse result = new Reverse(ppt);
    // Don't instantiate if the variables can't have order
    if (!result.var1().aux.getFlag(VarInfoAux.HAS_ORDER) ||
	!result.var2().aux.getFlag(VarInfoAux.HAS_ORDER)) {
      if (debug.isDebugEnabled()) {
	debug.debug ("Not instantitating for because order has no meaning: " +
		     result.var1().name + " and " + result.var2().name);
      }
      return null;
    }
    return result;
  }

  protected Invariant resurrect_done_swapped() {
    // "reverse of" is symmetric
    return this;
  }

  public String repr() {
    return "Reverse" + varNames() + ": "
      + "falsified=" + falsified;
  }

  public String format_using(OutputFormat format) {
    if (format == OutputFormat.DAIKON) return format_daikon();
    if (format == OutputFormat.IOA) return format_ioa();
    if (format == OutputFormat.JAVA) return format_java();
    if (format == OutputFormat.JML) return format_jml();

    return format_unimplemented(format);
  }

  public String format_daikon() {
    return var1().name.name() + " is the reverse of " + var2().name.name();
  }

  /* IOA */
  public String format_ioa() {
    return "Not valid for Sets or Arrays: " + format();
  }

  public String format_java() {
    // ( (new StringBuffer (var1().name.name())).reverse().toString(
    //       ).equals (var2().name.name()))
    return "( (new StringBuffer (" + var1().name.name() + ")).reverse().toString().equals (" + var2().name.name() + ")";
  }

  public String format_jml() {
    // The following line of code does not correctly indicate that one array is the reverse of the other
    // return "(new StringBuffer(" + var1().name.name() + ")).reverse().toString().equals(" + var2().name.name() + ")";

    VarInfoName name1,name2;
    name1 = var1().name;
    name2 = var2().name;

    String upperExtent = ""; // Pacify javac with the initialization

    VarInfoName seq;

    if (name2 instanceof VarInfoName.Elements) {
      seq = name2;
    } else if (name2 instanceof VarInfoName.Slice) {
      seq = ((VarInfoName.Slice)name2).sequence;
    } else { // Should never happen
      throw new IllegalStateException();
    }

    if (name2 instanceof VarInfoName.Slice && ((VarInfoName.Slice)name2).j != null)
      upperExtent = ((VarInfoName.Slice)name2).j.jml_name();
    else
      upperExtent = seq.applySize().jml_name();

    VarInfoName.QuantHelper.QuantifyReturn qret = VarInfoName.QuantHelper.quantify(new VarInfoName [] {name1});
    String results[] = VarInfoName.QuantHelper.format_jml(qret,true);

    VarInfoName index = ((VarInfoName [])qret.bound_vars.get(0))[0];

    // Would normally create a new VarInfoName for the subscript and then use applySubscript, but
    // parse appears to choke on that output... until it works, using this
    return results[0] + results[1] + " == " + ((VarInfoName.Elements)seq).term.jml_name() + "[" + upperExtent + "-1-" + index.jml_name() + "]" + results[2];
  }

  public void add_modified(long[] a1, long[] a2, int count) {
    if (a1.length != a2.length) {
      flowThis();
      destroy();
      return;
    }
    int len = a1.length;
    for (int i=0, j=len-1; i<len; i++, j--)
      if (a1[i] != a2[j]) {
	flowThis();
        destroy();
        return;
      }
  }


  protected double computeProbability() {
    if (falsified)
      return Invariant.PROBABILITY_NEVER;
    else
      return Invariant.PROBABILITY_JUSTIFIED;
  }

  public boolean isSameFormula(Invariant other)
  {
    Assert.assert(other instanceof Reverse);
    return true;
  }

}
