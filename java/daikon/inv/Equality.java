package daikon.inv;

import daikon.*;
import utilMDE.*;
import java.util.*;

/**
 * The Equality invariant is used for displaying several equality
 * Comparison invariants ("x == y", "x == z") as one Equality
 * invariant ("x == y == z").  This class is created after the actual
 * invariant detection, and right before printing (eg, during the GUI
 * stage).  Hence this is not a real invariant class; it does not
 * implement many of the methods that most invariant classes do.
 * Furthermore, calling arbitrary methods on this class may not work.
 **/
public final class Equality
  extends Invariant
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  private VarInfo[] vars;

  /**
   * @param vars Variables which are equivalent, with the canonical
   * one first.  Elements must be of type VarInfo.
   **/
  public Equality(Collection variables, PptSlice ppt) {
    super(ppt);
    vars = (VarInfo[]) variables.toArray(new VarInfo[variables.size()]);
    Assert.assertTrue(vars.length >= 2);
    for (int i=0; i < vars.length; i++) {
      Assert.assertTrue(vars[0].ppt == vars[i].ppt);
      Assert.assertTrue(vars[0].rep_type.isArray() == vars[i].rep_type.isArray());
    }
  }

  /** @return the canonical VarInfo of this */
  public VarInfo leader() {
    return vars[0];
  }

  public boolean hasNonCanonicalVariable() {
    // In fact, we do have non-canonical variables, but it's our
    // little secret.
    return false;
  }

  /**
   * Always return JUSTIFIED because we aggregate Comparison
   * invariants that are all justified to the probability_limit
   * threshold.
   **/
  public double computeProbability() {
    return Invariant.PROBABILITY_JUSTIFIED;
  }

  public String repr() {
    return "Equality" + varNames();
  }

  public String format_using(OutputFormat format) {
    if (format == OutputFormat.DAIKON) return format_daikon();
    if (format == OutputFormat.IOA) return format_ioa();
    if (format == OutputFormat.JAVA) return format_java();
    if (format == OutputFormat.ESCJAVA) return format_esc();
    if (format == OutputFormat.SIMPLIFY) return format_simplify();

    return format_unimplemented(format);
  }

  public String format_daikon() {
    StringBuffer result = new StringBuffer(vars[0].name.name());
    for (int i=1; i < vars.length; i++) {
      result.append(" == ");
      result.append(vars[i].name.name());
    }
    return result.toString();
  }


  /* java */
  // daikon.inv.Equality
  public String format_java() {
    StringBuffer result = new StringBuffer ();
    String first = vars[0].name.name();
    for (int i = 1; i < vars.length; i++) {
      // appends " && ( v[0] == v[i] )" to the stringbuffer
      if (i > 1) result.append(" && ");
      result.append("( ").append(first).append(" == "); // "interned"
      result.append(vars[i].name.name()).append(" ) ");
    }
    return result.toString();
  }

  /* IOA */
  public String format_ioa() {
    StringBuffer result = new StringBuffer();
    // There are always at least two vars
    Assert.assertTrue(vars.length >= 2);
    for (int i = 0; i < vars.length - 1; i++) {
      result.append (vars[i].name.ioa_name());
      result.append (" = ");
      result.append (vars[i+1].name.ioa_name());
      if (i < vars.length - 2) {
        result.append (" /\\ ");
      }
    }

    return result.toString();
  }


  public String format_esc() {
    String result = "";

    List valid_equiv = new ArrayList(); // [VarInfo]
    List invalid_equiv = new ArrayList(); // [VarInfo]

    List equal_vars = new Vector();
    List obviously_equal = new Vector();

    for (int j=0; j<this.vars.length; j++) {
      VarInfo other = this.vars[j];
      if (other.isDerivedSequenceMinMaxSum()) {
	break;
      }
      if (other.isValidEscExpression()) {
	valid_equiv.add(other);
      } else {
	invalid_equiv.add(other);
      }
    }
    // Choose a leader, preferring the valid variables.
    VarInfo leader;
    if (valid_equiv.size() > 0) {
      leader = (VarInfo) valid_equiv.get(0);
    } else {
      Assert.assertTrue(invalid_equiv.size() > 0);
      leader = (VarInfo) invalid_equiv.get(0);
    }
    // Print the equality statements, stating expressible ones first.
    equal_vars.clear();
    equal_vars.addAll(valid_equiv);
    equal_vars.addAll(invalid_equiv);
    for (int j=0; j<equal_vars.size(); j++) {
      VarInfo other = (VarInfo) equal_vars.get(j);
      if (other == leader) continue;
      if (leader.name.applyPrestate().equals(other.name)) continue;
      if (other.name.applyPrestate().equals(leader.name)) continue;
      if (j >= valid_equiv.size()) {
	result = result + "warning: method 'equality'.format_esc() needs to be implemented: " + format();
      }
      if (leader.rep_type.isArray()) {
	String[] form =
	  VarInfoName.QuantHelper.format_esc(new VarInfoName[]
	    { leader.name, other.name }, true); // elementwise
	result = result + form[0].toString() + "( " + form[1].toString() + " == " + form[2].toString() + " )" + form[3].toString();
      } else {
	if (j > 1) {
	  result += Global.lineSep;
	}
	result = result + leader.name.esc_name() + " == " + other.name.esc_name();
      }

      if (obviously_equal.contains(other)) {
	result += "    (obviously)";
      }
    }
    return(result);

//      StringBuffer result = new StringBuffer();
//      if (vars[0].rep_type.isArray()) {
//        for (int i=1; i < vars.length; i++) {
//  	if (i > 1) {
//  	  result.append(Global.lineSep);
//  	}
//  	String[] form =
//  	  VarInfoName.QuantHelper.format_esc(new VarInfoName[]
//  	    { vars[0].name, vars[i].name }, true); // elementwise
//  	result.append(form[0] + "( " + form[1] + " == " + form[2] + " )" + form[3]);
//        }
//      } else {
//        for (int i=1; i < vars.length; i++) {
//  	if (i > 1) {
//  	  result.append(" && ");
//  	}
//  	result.append("");   // formerly "("
//  	result.append(vars[0].name.esc_name());
//  	result.append(" == ");
//  	result.append(vars[i].name.esc_name());
//  	result.append("");  //formerly ")"
//        }
//      }
//      return result.toString();
  }




  // This probably belongs in ProglangType proper (?)
  public boolean is_reference() {
    VarInfo foo = vars[0];

    // If the program type has a higher dimension than the rep type,
    // we are taking a hash or something.
    if (foo.type.pseudoDimensions() > foo.rep_type.pseudoDimensions()) {
      return true;
    }

    // The dimensions are the same.  If the rep type is integral but
    // the program type isn't primitive, we have a hash, too.
    if (foo.rep_type.baseIsIntegral() && (!foo.type.baseIsPrimitive())) {
      return true;
    }

    return false;
  }

  // When A and B are pointers, don't say (EQ A B); instead say (EQ
  // (hash A) (hash B)).  If we said the former, Simplify would
  // presume that A and B were always interchangeable, which is not
  // the case when your programming language involves mutation.
  private String format_elt(String simname) {
    String result = simname;
    if (is_reference()) {
      result = "(hash " + result + ")";
    }
    return result;
  }

  public String format_simplify() {
    StringBuffer result = new StringBuffer("(AND");
    if (vars[0].rep_type.isArray()) {
      for (int i=1; i < vars.length; i++) {
        String[] form =
          VarInfoName.QuantHelper.format_simplify(new VarInfoName[]
            { vars[0].name, vars[i].name }, true); // elementwise
        String a = format_elt(form[1]);
        String b = format_elt(form[2]);
        result.append(" " + form[0] + "(EQ " + a + " " + b + ")" + form[3]);
      }
    } else {
      for (int i=1; i < vars.length; i++) {
        String a = format_elt(vars[0].name.simplify_name());
        String b = format_elt(vars[i].name.simplify_name());
        result.append(" (EQ ");
        result.append(a);
        result.append(" ");
        result.append(b);
        result.append(")");
      }
    }
    result.append(")");
    return result.toString();
  }


  //  This method isn't going to be called, but it's declared abstract in Invariant.
  protected Invariant resurrect_done(int[] permutation) {
    throw new UnsupportedOperationException();
  }

  //  This method isn't going to be called, but it's declared abstract in Invariant.
  public boolean isSameFormula( Invariant other ) {
    throw new UnsupportedOperationException( "Equality.isSameFormula(): this method should not be called" );
  }
}
