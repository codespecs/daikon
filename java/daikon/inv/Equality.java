package daikon.inv;

import daikon.*;
import daikon.inv.binary.twoScalar.IntEqual;
import daikon.inv.binary.twoString.StringComparison;
import daikon.inv.binary.twoScalar.FloatEqual;
import daikon.inv.binary.twoSequence.*;
import daikon.suppress.*;

import utilMDE.*;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.*;


// Note that this Invariant is used in a *very* different way from
// the same-named on in V2.  In V2, this is just for printing.  In V3,
// this does all the canonicalizing, etc.
/**
 * Keeps track of sets of variables that are equal.<p>
 *
 * During checking, Equality keeps track of VarInfos that are
 * comparable and equal, so we only need to instantiate (other)
 * invariants for one member of each Equal set, the leader.  See
 * equality notes in this directory.<p>
 *
 * During postProcessing, each instance of Equality instantiates into
 * displaying several equality Comparison invariants ("x == y", "x ==
 * z").  Equality invariants have leaders, which are the canonical
 * forms of their variables.  In the previous example, x is the
 * leader.  Equality invariants sort their variables by index ordering
 * during checking.  During printing, however, equality invariants may
 * "pivot" -- that is, switch leaders if the current leader wouldn't
 * be printed because it was not an interesting variable.  Notice that
 * when pivoting, all the other invariants based on this.leader also
 * need to be pivoted.
 **/
public final class Equality
  extends Invariant
{
   // We are Serializable, so we specify a version to allow changes to
   // method signatures without breaking serialization.  If you add or
   // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20021231L;

  public static final Logger debug =
    Logger.getLogger ("daikon.inv.Equality");

  public static final Logger debugPostProcess =
    Logger.getLogger ("daikon.inv.Equality.postProcess");

  /**
   * How many samples this has seen.
   **/
  private int numSamples;

  public void setSamples (int sample_cnt) {
    numSamples = sample_cnt;
  }

  public int numSamples() {
    return numSamples;
  }

  /**
   * The Set of VarInfos that this represents equality for.  Can
   * change over time as this invariant weakens.  Sorted by index
   * until pivoting.
   **/
  private TreeSet/*VarInfo*/ vars;

  /**
   * Returns the variables in their index order.  Unmodifiable.
   **/
  public Set getVars() {
    return Collections.unmodifiableSet (vars);
  }

  /**
   * @param vars Variables which are equivalent, with the canonical
   * one first.  Elements must be of type VarInfo.
   **/
  public Equality(Collection variables, PptSlice ppt) {
    super(ppt);
    if (debug.isLoggable(Level.FINE)) {
      debug.fine ("Creating at " + ppt.parent.ppt_name + " vars: ");
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
      if (debug.isLoggable(Level.FINE)) {
        debug.fine ("  " + vi.name.name());
      }
      Assert.assertTrue(vi.ppt == leader.ppt);
      Assert.assertTrue(vi.comparableNWay (leader));
      Assert.assertTrue(vi.rep_type.isArray() == leader.rep_type.isArray());
      vi.equalitySet = this;
    }
  }

  ////////////////////////
  // Accessors


  private VarInfo leaderCache = null;
  /**
   * Return the canonical VarInfo of this.  Note that the leader never
   * changes.
   * @return the canonical VarInfo of this
   **/
  public VarInfo leader() {
    if (leaderCache == null) {
      leaderCache = (VarInfo) vars.iterator().next();
    }
    return leaderCache;
  }

  public boolean hasNonCanonicalVariable() {
    throw new Error("Illegal operation on Equality invariant");
  }

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
    // Commented out by MDE 7/27/2003.  I can't figure out whether
    // to just change JAVA_IDENTIFIER to IDENTIFIER, or whether other
    // changes are also necessary.
    // if (format == OutputFormat.JAVA_IDENTIFIER) return format_java();
    if (format == OutputFormat.SIMPLIFY) return format_simplify();
    if (format == OutputFormat.DBCJAVA) return format_dbc();
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


  // These format methods aren't called, because for output, we
  // convert to normal two-way IntEqual type invariants.  However,
  // they can be called if desired.
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
    int numprinted = 0;
    for (int j=0; j<equal_vars.size(); j++) {
      VarInfo other = (VarInfo) equal_vars.get(j);
      if (other == leader) continue;
      if (leader.name.applyPrestate().equals(other.name)) continue;
      if (other.name.applyPrestate().equals(leader.name)) continue;
      if (numprinted > 0) {
        result += Global.lineSep;
      }
      numprinted++;
      if (j >= valid_equiv.size()) {
        result = result + "warning: method Equality.format_esc() needs to be implemented: " + format();
      }
      if (leader.rep_type.isArray()) {
        String[] form =
          VarInfoName.QuantHelper.format_esc(new VarInfoName[]
            { leader.name, other.name }, true); // elementwise
        result = result + form[0].toString() + "( " + form[1].toString() + " == " + form[2].toString() + " )" + form[3].toString();
      } else {
        result = result + leader.name.esc_name() + " == " + other.name.esc_name();
      }

      // if (obviously_equal.contains(other)) {
      //   result += "    (obviously)";
      // }

    }
    return result;

//      StringBuffer result = new StringBuffer();
//      if (leader().rep_type.isArray()) {
//        for (int i=1; i < vars.length; i++) {
//      if (i > 1) {
//        result.append(Global.lineSep);
//      }
//      String[] form =
//        VarInfoName.QuantHelper.format_esc(new VarInfoName[]
//          { leader().name, vars[i].name }, true); // elementwise
//      result.append(form[0] + "( " + form[1] + " == " + form[2] + " )" + form[3]);
//        }
//      } else {
//        for (int i=1; i < vars.length; i++) {
//      if (i > 1) {
//        result.append(" && ");
//      }
//      result.append("");   // formerly "("
//      result.append(leader().name.esc_name());
//      result.append(" == ");
//      result.append(vars[i].name.esc_name());
//      result.append("");  // formerly ")"
//        }
//      }
//      return result.toString();
  }

  // When A and B are pointers, don't say (EQ A B); instead say (EQ
  // (hash A) (hash B)).  If we said the former, Simplify would
  // presume that A and B were always interchangeable, which is not
  // the case when your programming language involves mutation.
  private String format_elt(String simname) {
    String result = simname;
    if (leader().is_reference()) {
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
            { leader.name, var.name }, true); // elementwise
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

  public String shortString() {
    return format_daikon();
  }

  //@tx
  // daikon.inv.Equality
  public String format_dbc() {
    StringBuffer result = new StringBuffer ();
    String first = null; // = vars[0].name.name();
    Iterator i = vars.iterator();
    if (i.hasNext()) {
      first = ((VarInfo) i.next()).name.name();
    } else {
      return "";
    }
    boolean firstLoop = true;
    while (i.hasNext()) {

      // for (int i = 1; i < vars.length; i++) {
      // // appends " && ( v[0] == v[i] )" to the stringbuffer
      if (! firstLoop) result.append(" && ");
      result.append("( ").append(first).append(" == "); // "interned"
      result.append(((VarInfo) i.next()).name.name()).append(" ) ");
      firstLoop = false;
    }
    return result.toString();
  }

  public String toString() {
    return repr();
  }

  //////////////////////////////////////////////////////////////////////
  /// Processing of data

  /**
   * @return a List of VarInfos that do not fit into this set anymore
   *
   * Originally (8/14/2003), this did not check for the modified bits.
   * It seems however, quite wrong to leave variables in the same equality
   * set when one is missing and the other is not.  Its possible we should
   * go farther and break out of the equality set any variable that is
   * missingOutOfBounds (JHP)
   **/
  public List add(ValueTuple vt, int count) {
    // Need to handle specially if leader is missing.
    VarInfo leader = leader();
    Object leaderValue = leader.getValue(vt);
    int leaderMod = leader.getModified(vt);
    boolean leaderOutOfBounds = leader.missingOutOfBounds();
    if (leaderMod == ValueTuple.MISSING_NONSENSICAL ||
        leaderMod == ValueTuple.MISSING_FLOW) {
    } else {
      numSamples += count;
    }

    List result = new LinkedList();
    if (debug.isLoggable(Level.FINE)) {
      debug.fine ("Doing add at " + this.ppt.parent.ppt_name + " for " + this);
    }
    for (Iterator i = vars.iterator(); i.hasNext(); ) {
      VarInfo vi = (VarInfo) i.next();
      if (vi == leader)
        continue;
      Assert.assertTrue (vi.comparableNWay (leader));
      Object viValue = vi.getValue(vt);
      int viMod = vi.getModified(vt);
      // The following is possible because values are interned.  The
      // test also takes into account missing values, since they are
      // null.
       if ((leaderValue == viValue) && (leaderMod == viMod)
        && !leaderOutOfBounds && !vi.missingOutOfBounds()) continue;
      //       if (debug.isLoggable(Level.FINE)) {
      //         debug.fine ("  vi name: " + vi.name.name());
      //         debug.fine ("  vi value: " + viValue);
      //         debug.fine ("  le value: " + leaderValue);
      //       }
      result.add (vi);
      i.remove();
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
    if (debugPostProcess.isLoggable(Level.FINE)) {
      debugPostProcess.fine ("Doing postProcess: " + this.format_daikon());
      debugPostProcess.fine ("  at: " + this.ppt.parent.ppt_name);
    }
    VarInfo leader = leader();
    ProglangType rep = leader.rep_type;
    boolean rep_is_scalar = rep.isScalar();
    boolean rep_is_float = rep.isFloat();

    if (debugPostProcess.isLoggable(Level.FINE)) {
      debugPostProcess.fine ("  var1: " + leader.name.name());
    }
    for (int i = 0; i < vars.length; i++) {
      if (vars[i] == leader) continue;
      if (debugPostProcess.isLoggable(Level.FINE)) {
        debugPostProcess.fine ("  var2: " + vars[i].name.name());
      }

      PptSlice newSlice = parent.get_or_instantiate_slice (leader, vars[i]);
      // Copy over the number of samples from this to the new slice,
      // so that all invariants on the slice report the right number
      // of samples.
      newSlice.set_samples (this.numSamples());
      Invariant invEquals = null;

      // This is almost directly copied from PptSlice2's instantiation
      // of factories
      if (rep_is_scalar) {
        invEquals = IntEqual.instantiate (newSlice);
        debugPostProcess.fine ("  intEqual");
      } else if ((rep == ProglangType.STRING)) {
        invEquals = StringComparison.instantiate (newSlice, true);
        debugPostProcess.fine ("  stringEqual");
        ((StringComparison) invEquals).core.can_be_eq = true;
      } else if ((rep == ProglangType.INT_ARRAY)) {
        invEquals = SeqSeqIntEqual.instantiate (newSlice, true);
        debugPostProcess.fine ("  seqEqual");
      } else if ((rep == ProglangType.STRING_ARRAY)) {
        // JHP commented out to see what diffs are coming from here (5/3/3)
//         invEquals = SeqComparisonString.instantiate (newSlice, true);
//         if (invEquals != null) {
//           ((SeqComparisonString) invEquals).can_be_eq = true;
//         }
//         debugPostProcess.fine ("  seqStringEqual");
      } else if (Daikon.dkconfig_enable_floats) {
        if (rep_is_float) {
          invEquals = FloatEqual.instantiate (newSlice);
          debugPostProcess.fine ("  floatEqual");
        } else if (rep == ProglangType.DOUBLE_ARRAY) {
          debugPostProcess.fine ("  seqFloatEqual");
          invEquals = SeqSeqFloatEqual.instantiate (newSlice, true);
        }
      } else {
        throw new Error ("No known Comparison invariant to convert equality into");
      }

      if (invEquals != null) {
        if (debugPostProcess.isLoggable(Level.FINE)) {
          debugPostProcess.fine ("  adding invariant: " + invEquals.repr());
        }
        SuppressionLink sl = SelfSuppressionFactory.getInstance().generateSuppressionLink (invEquals);
        if (sl != null) {
          if (debugPostProcess.isLoggable(Level.FINE)) {
            debugPostProcess.fine ("  suppressed by another equality: " +
                                    sl);
          }
        } else {
          newSlice.addInvariant (invEquals);
        }
      } else {
        newSlice.parent.removeSlice (newSlice);
      }
    }
  }

  /**
   * Switch the leader of this invariant, if possible, to a VarInfo
   * that is not isDerivedParamAndUninteresting.  If not, keep the
   * same leader.  Call this only after postProcess has been called.
   * We do a pivot so that anything that's interesting to be printed
   * gets printed and not filtered out.  For example, if a == b and a
   * is the leader, but not interesting, we still want to print f(b)
   * as an invariant.  Thus we pivot b to be the leader.  Later on,
   * each relevant PptSlice gets pivoted.  But not here.
   **/
  public void pivot() {
    if (!leader().isDerivedParamAndUninteresting()) return;
    VarInfo newLeader = null;
    for (Iterator iVars = vars.iterator(); iVars.hasNext(); ) {
      VarInfo var = (VarInfo) iVars.next();
      if (!var.isDerivedParamAndUninteresting()) {
        newLeader = var;
        break;
      }
    }
    if (newLeader != null) {
      leaderCache = newLeader;
    }
  }

  public void repCheck() {
    super.repCheck();
    VarInfo leader = leader();
    for (Iterator i = vars.iterator(); i.hasNext(); ) {
      VarInfo var = (VarInfo) i.next();
      Assert.assertTrue (VarComparability.comparable (leader, var));
    }
  }

}
