package daikon.inv;

import java.io.*;

/** DiscardInvariant is an enumeration type where the instances
    represent reasons why an invariant is falsified or disregarded.
    Methods that decide whether an Invariant should be printed later
    (such as isObviousImplied()), side effect Invariants to contain
    DiscardInvariant instances in their discardCode field slot.

    The different elements of the enumeration are:

    not_discarded // has not been discarded

    obvious // is implied by other already known invariants

    bad_sample // is falsified by a seen example

    bad_probability // has an unjustified probability

    few_modified_samples // has had few modified samples

    not_enough_samples // not enough samples seen for the Invariant

    non_canonical_var // expression involves a non-canonical variable

    implied_post_condition // implied by some prestate conditions

    only_constant_vars // expression for invariant only contains constant variables

    derived_param // has a VarInfo that has derived and uninteresting param

    unmodified_var // invariant discarded because it says some var hasn't been modified

    control_check // if discarded due to the ControlledInvariantFilter

    exact // isExact() fails

    var_filtered // Doesn't contain a desirable variable
 */

public class DiscardInvariant implements Comparable,Serializable {

  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20030612L;

  /** used when an invariant has not been discarded */
  public static final DiscardInvariant not_discarded = new DiscardInvariant(-1);

  /** used when an invariant is implied by other known invariants */
  public static final DiscardInvariant obvious = new DiscardInvariant(0);

  /** used when an invariant is falsified by a seen example */
  public static final DiscardInvariant bad_sample = new DiscardInvariant(1);

  /** used when an invariant has an unjustified probability */
  public static final DiscardInvariant bad_probability = new DiscardInvariant(2);

  /** used when an invariant has few modified samples */
  public static final DiscardInvariant few_modified_samples = new DiscardInvariant(3);

  /** used when an invariant has not had enough samples */
  public static final DiscardInvariant not_enough_samples = new DiscardInvariant(4);

  /** used when an invariant contains a non-canonical variable */
  public static final DiscardInvariant non_canonical_var = new DiscardInvariant(5);

  /** used when an invariant is implied by some prestate conditions */
  public static final DiscardInvariant implied_post_condition = new DiscardInvariant(6);

  /** used when an invariant's expression contains only constant variables */
  public static final DiscardInvariant only_constant_vars = new DiscardInvariant(7);

  /** used when an invariant's VarInfo returns true for isDerivedParamAndUninteresting() */
  public static final DiscardInvariant derived_param = new DiscardInvariant(8);

  /** used for invariants that describe unmodified variables */
  public static final DiscardInvariant unmodified_var = new DiscardInvariant(9);

  /** used for invariants discarded because of the ControlledInvariantsFilter */
  public static final DiscardInvariant control_check = new DiscardInvariant(10);

  /** used for invariants discarded when isExact() fails */
  public static final DiscardInvariant exact = new DiscardInvariant(11);

  /** used for invariants that don't contain desired variables */
  public static final DiscardInvariant var_filtered = new DiscardInvariant(12);

  /** Each member of the enumeration is associated with a distinct int for comparability */
  public final int enumValue;

  // Prevents the user from using DiscardInvariant types not in the enumeration
  // by making the default constructor private.  This constructor should never be
  // used to make new elements of the enumeration.
  private DiscardInvariant() {
    this.enumValue = -1;
  }

  // Makes it easier to add a new DiscardInvariant type by simply constructing it as a field with the next non-used integer value
  private DiscardInvariant(int enumValue) {
    this.enumValue = enumValue;
  }

  /** The enumeration members in assorted order: <br>
      not_discarded, obvious, bad_sample, bad_probability, few_modified_samples, not_enough_samples, non_canonical_var,<br>
      implied_post_condition, only_constant_vars, derived_param, unmodified_var, control_check, exact, var filter
   * @returns this.enumValue.compareTo(o.enumValue) where the enumValue are treated as Integers
   * @throws ClassCastException iff !(o instanceof DiscardInvariant)
   */
  public int compareTo(Object o) throws ClassCastException {
    if (!(o instanceof DiscardInvariant))
      throw new ClassCastException("Tried to compare instance of DiscardInvariant with incompatible type");
    else {
      Integer thisValue = new Integer(this.enumValue);
      Integer oValue = new Integer( ((DiscardInvariant) o).enumValue );
      return thisValue.compareTo(oValue);
    }
  }

  /** Prints out a string describing the reason for discard
   * @returns one of {"Not discarded","Obvious,"Bad sample seen","Unjustified probability","Few modified samples","Not enough samples",
                      "Non-canonical variable","Implied post state","Only constant variables in this expression","Derived Param","Control Check"
                      ,Exact, Variable Filter}
   */
  public String toString() {
    if (this.enumValue==-1)
      return "Not discarded";
    else if (this.enumValue==0)
      return "Obvious";
    else if (this.enumValue==1)
      return "Bad sample seen";
    else if (this.enumValue==2)
      return "Unjustified probability";
    else if (this.enumValue==3)
      return "Few modified samples";
    else if (this.enumValue==4)
      return "Not enough samples";
    else if (this.enumValue==5)
      return "Non-canonical variable";
    else if (this.enumValue==6)
      return "Implied post state";
    else if (this.enumValue==7)
      return "Only constant variables in this expression";
    else if (this.enumValue==8)
      return "Derived Param";
    else if (this.enumValue==9)
      return "Unmodified var";
    else if (this.enumValue==10)
      return "Control Check";
    else if (this.enumValue==11)
      return "Exact";
    else if (this.enumValue==12)
      return "Variable Filter";
    else { // this should never happen since the constructor is private
      return "Unknown instance of DiscardInvariant used";
    }
  }

  /** To prevent deserialization causing more DiscardInvariants to be instantiated
   *@returns one of the static DiscardInvariant instances
   *@throws ObjectStreamException
   **/
  public Object readResolve() throws ObjectStreamException {
    if (enumValue==-1)
      return not_discarded;
    else if (enumValue==0)
      return obvious;
    else if (enumValue==1)
      return bad_sample;
    else if (enumValue==2)
      return bad_probability;
    else if (enumValue==3)
      return few_modified_samples;
    else if (enumValue==4)
      return not_enough_samples;
    else if (enumValue==5)
      return non_canonical_var;
    else if (enumValue==6)
      return implied_post_condition;
    else if (enumValue==7)
      return only_constant_vars;
    else if (enumValue==8)
      return derived_param;
    else if (enumValue==9)
      return unmodified_var;
    else if (enumValue==10)
      return control_check;
    else if (enumValue==11)
      return exact;
    else if (enumValue==12)
      return var_filtered;
    else {//this should never happen
      return null;
    }
  }
}
