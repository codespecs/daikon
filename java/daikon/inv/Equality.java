package daikon.inv;

import daikon.*;
import daikon.inv.binary.twoScalar.IntEqual;
import daikon.inv.binary.twoString.StringComparison;
import daikon.inv.binary.twoScalar.FloatEqual;
import daikon.inv.binary.twoSequence.SeqComparison;
import daikon.inv.binary.twoSequence.SeqComparisonFloat;
import daikon.suppress.*;

import utilMDE.*;
import org.apache.log4j.Category;
import java.util.*;


// Note that this Invariant is used in a *very* different way from
// the same-named on in V2.  In V2, this is just for printing.  In V3,
// this does all the canonicalizing, etc.
/**
 * This class is used for two things.  First, during checking, it
 * keeps track of VarInfos that are comparable and equal, so we only
 * need to instantiate invariants for one member of each Equal set.
 * For the first purpose, see equality notes in this directory.
 * Second, during printing, Equality is used for displaying several
 * equality Comparison invariants ("x == y", "x == z") as one Equality
 * invariant ("x == y == z").
 **/
public final class Equality
  extends Invariant
{

  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  public static final Category debug =
    Category.getInstance ("daikon.inv.Equality");

  public static final Category debugPostProcess =
    Category.getInstance ("daikon.inv.Equality.postProcess");

  /**
   * How many samples this has seen.
   **/
  private int numSamples;

  public int numSamples() {
    return numSamples;
  }

  public void setSamples(int arg) {
    numSamples = arg;
  }
  
  /**
   * The Set of VarInfos that this represents equality for.  Can change
   * over time as invariant weakens.
   **/
  private Set/*VarInfo*/ vars;

  public Set getVars() {
    return Collections.unmodifiableSet (vars);
  }

  /**
   * @param vars Variables which are equivalent, with the canonical
   * one first.  Elements must be of type VarInfo.
   **/
  public Equality(Collection variables, PptSlice ppt) {
    super(ppt);
    if (debug.isDebugEnabled()) {
      debug.debug ("Creating at " + ppt.parent.ppt_name + " vars: ");
    }

    numSamples = 0;
    vars = new TreeSet(VarInfo.IndexComparator.theInstance);
    vars.addAll (variables);
    VarInfo leader = leader();

    // ensure well-formedness and set equality slots
    Assert.assertTrue (variables.size() > 0);
    Assert.assertTrue (vars.size() == variables.size());
    for (Iterator i = variables.iterator(); i.hasNext(); ) {
      VarInfo vi = (VarInfo) i.next();
      if (debug.isDebugEnabled()) {
        debug.debug ("  " + vi.name.name());
      }
      Assert.assertTrue(vi.ppt == leader.ppt);
      Assert.assertTrue(vi.type == leader.type);
      Assert.assertTrue(vi.rep_type == leader.rep_type);
      //      Assert.assertTrue(vi.rep_type.isArray() == leader.rep_type.isArray());
      vi.equalitySet = this;
    }
  }

  ////////////////////////
  // Accessors


  private VarInfo leaderCache = null;
  /**
   * Return the canonical VarInfo of this.  Note that the leader never
   * changes.
   * @return the canonical VarInfo of this.
   **/
  public VarInfo leader() {
    if (leaderCache == null) {
      leaderCache = (VarInfo) vars.iterator().next();
      return leaderCache;
    } else {
      return leaderCache;
    }
  }

  //   public boolean hasNonCanonicalVariable() {
  //     // In fact, we do have non-canonical variables, but it's our
  //     // little secret.
  //     return false;
  //   }
  
  /**
   * Always return JUSTIFIED because we aggregate Comparison
   * invariants that are all justified to the probability_limit
   * threshold.
   **/
  public double computeProbability() {
    return Invariant.PROBABILITY_JUSTIFIED;
  }

  ////////////////////////
  // Functions called during actual checking

  private void flow(Invariant flowed) {
    throw new UnsupportedOperationException("Equality invariants don't flow");
  }

  ////////////////////////
  // Printing

  public String repr() {
    return "Equality: leader: " + leader().name.name() + " with " +
      format_daikon() + " samples: " + numSamples();
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
    StringBuffer result = new StringBuffer();
    boolean start = true;
    for (Iterator i = vars.iterator(); i.hasNext(); ) {
      VarInfo var = (VarInfo) i.next();
      if (!start) {
        result.append(" == ");
      } else {
        start = false;
      }
      result.append(var.name.name());
    }
    return result.toString();
  }


  // Most of these methods aren't called, because for output, we
  // convert to normal two-way IntEqual type invariants.
  /* java */
  // daikon.inv.Equality
  public String format_java() {
    StringBuffer result = new StringBuffer ();
    VarInfo leader = leader();
    String leaderName = leader.name.name();
    for (Iterator i = vars.iterator(); i.hasNext(); ) {
      VarInfo var = (VarInfo) i.next();
      if (leader == var) continue;
      result.append("(").append(leaderName).append(" == "); // "interned"
      result.append(var.name.name()).append(")");
      if (i.hasNext()) result.append(" && ");
    }
    return result.toString();
  }

  /* IOA */
  public String format_ioa() {
    StringBuffer result = new StringBuffer();
    VarInfo leader = leader();
    String leaderName = leader.name.ioa_name();
    for (Iterator i = vars.iterator(); i.hasNext(); ) {
      VarInfo var = (VarInfo) i.next();
      if (leader == var) continue;
      result.append (var.name.ioa_name());
      result.append (" = ");
      result.append (leaderName);
      if (i.hasNext()) result.append (" /\\ ");
    }

    return result.toString();
  }


  public String format_esc() {
    String result = "";

    List valid_equiv = new ArrayList(); // [VarInfo]
    List invalid_equiv = new ArrayList(); // [VarInfo]

    List equal_vars = new Vector();
    List obviously_equal = new Vector();

    for (Iterator i = vars.iterator(); i.hasNext(); ) {
      VarInfo other = (VarInfo) i.next();
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
//      if (leader().rep_type.isArray()) {
//        for (int i=1; i < vars.length; i++) {
//      if (i > 1) {
//        result.append(Global.lineSep);
//      }
//      String[] form =
//        VarInfoName.QuantHelper.format_esc(new VarInfoName[]
//          { vars[0].name, vars[i].name }, true); // elementwise
//      result.append(form[0] + "( " + form[1] + " == " + form[2] + " )" + form[3]);
//        }
//      } else {
//        for (int i=1; i < vars.length; i++) {
//      if (i > 1) {
//        result.append(" && ");
//      }
//      result.append("");   // formerly "("
//      result.append(vars[0].name.esc_name());
//      result.append(" == ");
//      result.append(vars[i].name.esc_name());
//      result.append("");  // formerly ")"
//        }
//      }
//      return result.toString();
  }




  // This probably belongs in ProglangType proper (?)
  public boolean is_reference() {
    VarInfo foo = leader();

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
    VarInfo leader = leader();
    String leaderName = leader.name.simplify_name();
    if (leader.rep_type.isArray()) {
      for (Iterator i = vars.iterator(); i.hasNext(); ) {
        VarInfo var = (VarInfo) i.next();
        if (var == leader) continue;
        String[] form =
          VarInfoName.QuantHelper.format_simplify(new VarInfoName[]
            { leader().name, var.name }, true); // elementwise
        String a = format_elt(form[1]);
        String b = format_elt(form[2]);
        result.append(" " + form[0] + "(EQ " + a + " " + b + ")" + form[3]);
      }
    } else {
      for (Iterator i = vars.iterator(); i.hasNext(); ) {
        VarInfo var = (VarInfo) i.next();
        if (var == leader) continue;
        String a = format_elt(leaderName);
        String b = format_elt(var.name.simplify_name());
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

  public String toString() {
    return repr();
  }


  /**
   * @return a List of VarInfos that do not fit into this set anymore
   **/

  // Need to handle specially if leader is missing.
  public List add(ValueTuple vt, int count) {
    VarInfo leader = leader();
    Object leaderValue = leader.getValue(vt);
    int leaderMod = leader.getModified(vt);
    if (leaderMod == ValueTuple.MISSING_NONSENSICAL ||
        leaderMod == ValueTuple.MISSING_FLOW) {
    } else {
      numSamples += count;
    }

    List result = new LinkedList();
    if (debug.isDebugEnabled()) {
      debug.debug ("Doing add at " + this.ppt.parent.ppt_name + " for " + this);
    }
    for (Iterator i = vars.iterator(); i.hasNext(); ) {
      VarInfo vi = (VarInfo) i.next();
      Object viValue = vi.getValue(vt);
      if (leaderValue == viValue) continue;
//       if (debug.isDebugEnabled()) {
//         debug.debug("  vi name: " + vi.name.name());
//         debug.debug("  vi value: " + viValue);
//         debug.debug("  le value: " + leaderValue);
//       }
      // The following or expression *must* be done in this specific
      // order so null values are taken into account
      if (leaderValue == null ||
          viValue == null ||
          !(viValue.equals(leaderValue))) {
        // We should be interning here and using == only
        // 
        // To do this, we'd have to intern at the ValueTuple level, in
        // FileIO when the tuples get generated.  This would be good for
        // two reasons:
        //   The comparison here would be faster
        //   Avoid double interning at the Invariant level when f(a, b) 
        //   and f(a, c).
        // This would be bad because:
        //   Some VarInfos that are never checked would have to get
        //   interned.  Right now, only VarInfos with invariants on them
        //   are interned. [TNW after talking with MDE 26 Sep 2002]
        
        result.add (vi);
        i.remove();
      }
    }

    return result;
  }

  //  This method isn't going to be called, but it's declared abstract in Invariant.
  protected Invariant resurrect_done(int[] permutation) {
    throw new UnsupportedOperationException();
  }

  //  This method isn't going to be called, but it's declared abstract in Invariant.
  public boolean isSameFormula( Invariant other ) {
    throw new UnsupportedOperationException( "Equality.isSameFormula(): this method should not be called" );
  }

  /**
   * Convert Equality invariants into normal IntEqual type for
   * filtering, printing, etc.  Add these to parent.
   * @modifies this.ppt.parent Will get new IntEqual,SeqEqual,etc. invariants added to it
   **/
  public void postProcess () {
    if (this.numSamples() == 0) return; // All were missing or not present
    PptTopLevel parent = this.ppt.parent;
    VarInfo[] vars = (VarInfo[]) this.vars.toArray(new VarInfo[0]);
    VarInfo[] sliceVars = new VarInfo[2];
    if (debugPostProcess.isDebugEnabled()) {
      debugPostProcess.debug ("Doing postProcess: " + this.format_daikon());
      debugPostProcess.debug ("  at: " + this.ppt.parent.ppt_name);
    }
    sliceVars[0] = leader();
    ProglangType rep = sliceVars[0].rep_type;
    boolean rep_is_scalar = rep.isScalar();
    boolean rep_is_float = rep.isFloat();

    if (debugPostProcess.isDebugEnabled()) {
      debugPostProcess.debug ("  var1: " + sliceVars[0].name.name());
    }
    for (int i = 0; i < vars.length; i++) {
//       sliceVars[0] = vars[i];
//       for (int j = i+1; j < vars.length; j++) {
//        Assert.assertTrue (vars[i] != vars[j]);
        sliceVars[1] = vars[i];
        if (sliceVars[1] == sliceVars[0]) continue;
        if (debugPostProcess.isDebugEnabled()) {
          debugPostProcess.debug ("  var2: " + sliceVars[1].name.name());
        }

        PptSlice newSlice = parent.get_or_instantiate_slice (sliceVars);

        newSlice.set_samples (this.numSamples());
        Invariant invEquals = null;

        // This is almost directly copied from PptSlice2's instantiation
        // of factories
        if (rep_is_scalar) {
          invEquals = IntEqual.instantiate (newSlice);
          debugPostProcess.debug ("  intEqual");
        } else if ((rep == ProglangType.STRING)) {
          invEquals = StringComparison.instantiate (newSlice, true);
          debugPostProcess.debug ("  seqEqual");
        } else if ((rep == ProglangType.INT_ARRAY)) {
          invEquals = SeqComparison.instantiate (newSlice, true);
          if (invEquals != null) {
            ((SeqComparison) invEquals).can_be_eq = true;
          }
          debugPostProcess.debug ("  seqEqual");
        } else if ((rep == ProglangType.STRING_ARRAY)) {
//           invEquals = StringComparison.instantiate (newSlice, true);
//           if (invEquals != null) {
//             ((SeqComparison) invEquals).can_be_eq = true;
//           }
          debugPostProcess.debug ("  stringEqual");
        } else if (Daikon.dkconfig_enable_floats
                   && rep_is_float) {
          invEquals = FloatEqual.instantiate (newSlice);
          debugPostProcess.debug ("  floatEqual");
        } else if (Daikon.dkconfig_enable_floats
                   && (rep == ProglangType.DOUBLE_ARRAY)) {
          debugPostProcess.debug ("  seqFloatEqual");
          invEquals = SeqComparisonFloat.instantiate (newSlice, true);
          if (invEquals != null) {
            ((SeqComparisonFloat) invEquals).can_be_eq = true;
          }
        } else {
          // Do nothing; do not even complain
        }

        if (invEquals != null) {
          if (debugPostProcess.isDebugEnabled()) {
            debugPostProcess.debug ("  adding invariant: " + invEquals.repr());
          }
          SuppressionTemplate template = new SuppressionTemplate();
          template.invTypes = new Class[] {invEquals.getClass()};
          template.varInfos = new VarInfo[][]
            {new VarInfo[] {sliceVars[0], sliceVars[1]}};
          newSlice.parent.fillSuppressionTemplate (template);
          if (template.filled) {
            if (debugPostProcess.isDebugEnabled()) {
              debugPostProcess.debug ("  suppressed by another equality: " +
                           template.results[0].repr());
            }

          } else {
            newSlice.addInvariant (invEquals);
          }
        }
//       }
    }
  }

  /**
   * Switch the leader of this invariant, if possible, to a VarInfo
   * that is not isDerivedParamAndUninteresting.  If not, keep the
   * same leader.  Call this only after postProcess has been called.
   * We do a pivot so that anything that's interesting to be printed
   * gets printed and not filtered out.  For example, of a == b and a
   * is the leader, but not interesting, we still want to print f(b)
   * as an invariant.  Thus we pivot b to be the leader.  Later on,
   * each relevant PptSlice gets pivoted.  But not here.
   **/
  public void pivot() {
    if (!leader().isDerivedParamAndUninteresting()) return;
    VarInfo newLeader = leader();
    for (Iterator iVars = vars.iterator(); iVars.hasNext(); ) {
      VarInfo var = (VarInfo) iVars.next();
      if (!var.isDerivedParamAndUninteresting()) {
        newLeader = var;
        break;
      }
    }
    if (leader() == newLeader) return;
    leaderCache = newLeader;
  }

  public void repCheck() {
    super.repCheck();
    VarInfo leader = leader();
    for (Iterator i = vars.iterator(); i.hasNext(); ) {
      VarInfo var = (VarInfo) i.next();
      Assert.assertTrue (var.type == leader.type);
    }
  }

}
