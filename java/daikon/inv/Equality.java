package daikon.inv;

import daikon.Debug;
import daikon.Global;
import daikon.PptSlice;
import daikon.PptTopLevel;
import daikon.Quantify.QuantFlags;
import daikon.ValueTuple;
import daikon.VarComparability;
import daikon.VarInfo;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.checkerframework.checker.initialization.qual.UnknownInitialization;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;
import typequals.prototype.qual.NonPrototype;
import typequals.prototype.qual.Prototype;

// Note that this Invariant is used in a *very* different way from
// the same-named one in V2.  In V2, this is just for printing.  In V3,
// this does all the canonicalizing, etc.

// This is a Java (not Javadoc) comment because the Daikon user manual
// reproduces the Javadoc but doesn't need all these implementation
// details.
//
// During checking, Equality keeps track of variables that are
// comparable and equal, so we only need to instantiate (other)
// invariants for one member of each Equal set, the leader.
//
// During postProcessing, each instance of Equality instantiates into
// displaying several EqualityComparison invariants ("x == y", "x ==
// z").  Equality invariants have leaders, which are the canonical
// forms of their variables.  In the previous example, x is the
// leader.  Equality invariants sort their variables by index ordering
// during checking.  During printing, however, equality invariants may
// "pivot" -- that is, switch leaders if the current leader wouldn't
// be printed because it was not an interesting variable.  Notice that
// when pivoting, all the other invariants based on this.leader also
// need to be pivoted.

/**
 * Keeps track of sets of variables that are equal. Other invariants are instantiated for only one
 * member of the Equality set, the leader. If variables {@code x}, {@code y}, and {@code z} are
 * members of the Equality set and {@code x} is chosen as the leader, then the Equality will
 * internally convert into binary comparison invariants that print as {@code x == y} and {@code x ==
 * z}.
 */
public final /*(at)Interned*/ class Equality extends Invariant {
  static final long serialVersionUID = 20021231L;

  public static final Logger debug = Logger.getLogger("daikon.inv.Equality");

  public static final Logger debugPostProcess = Logger.getLogger("daikon.inv.Equality.postProcess");

  /** How many samples this has seen. */
  private int numSamples;

  public void setSamples(int sample_cnt) {
    numSamples = sample_cnt;
  }

  public int numSamples(@GuardSatisfied Equality this) {
    return numSamples;
  }

  /**
   * The Set of VarInfos that this represents equality for. Can change over time as this invariant
   * weakens. Sorted by index until pivoting.
   */
  private TreeSet<VarInfo> vars;

  /** Returns the number of variables in the set. */
  @Pure
  public int size(@GuardSatisfied Equality this) {
    return vars.size();
  }

  /** Returns the variables in their index order. Unmodifiable. */
  public Set<VarInfo> getVars() {
    return Collections.unmodifiableSet(vars);
  }

  /**
   * Creates a new Equality invariant.
   *
   * @param variables variables that are equivalent, with the canonical one first
   * @param ppt the program point
   */
  @SuppressWarnings("initialization.field.write.initialized") // weakness of FBC type system
  public Equality(Collection<VarInfo> variables, PptSlice ppt) {
    super(ppt);
    if (debug.isLoggable(Level.FINE)) {
      debug.fine("Creating at " + ppt.parent.name() + " vars: ");
    }

    numSamples = 0;
    vars = new TreeSet<VarInfo>(VarInfo.IndexComparator.theInstance);
    vars.addAll(variables);
    VarInfo leader = leader();

    // ensure well-formedness and set equality slots
    assert variables.size() > 0;
    assert vars.size() == variables.size();

    for (VarInfo vi : variables) {
      if (debug.isLoggable(Level.FINE)) {
        debug.fine("  " + vi.name() + " [" + vi.comparability + "]");
      }
      assert vi.ppt == leader.ppt;
      assert vi.comparableNWay(leader);
      assert VarComparability.comparable(leader, vi)
          : "not comparable " + leader.name() + " " + vi.name() + " at ppt " + ppt.parent.name();
      assert vi.rep_type.isArray() == leader.rep_type.isArray();
      vi.equalitySet = this;
    }
  }

  // ////////////////////////
  // Accessors

  private @Nullable VarInfo leaderCache = null;

  /**
   * Return the canonical VarInfo of this. Note that the leader never changes.
   *
   * @return the canonical VarInfo of this
   */
  @SuppressWarnings("all:purity") // set cache field
  @Pure
  public VarInfo leader(@GuardSatisfied @UnknownInitialization(Equality.class) Equality this) {
    if (leaderCache == null) {
      leaderCache = vars.iterator().next();
    }
    return leaderCache;
  }

  public boolean hasNonCanonicalVariable() {
    throw new Error("Illegal operation on Equality invariant");
  }

  /**
   * Always return JUSTIFIED because we aggregate EqualityComparison invariants that are all
   * justified to the confidence_limit threshold.
   */
  @Override
  public double computeConfidence() {
    return Invariant.CONFIDENCE_JUSTIFIED;
  }

  // ////////////////////////
  // Printing

  // The format methods aren't called, because for output, we
  // convert to normal two-way IntEqual type invariants.  However,
  // they can be called if desired.

  @Override
  public String repr(@GuardSatisfied Equality this) {
    return "Equality: size="
        + size()
        + " leader: "
        + leader().name()
        + " with "
        + format_daikon()
        + " samples: "
        + numSamples();
  }

  @SideEffectFree
  @Override
  public String format_using(@GuardSatisfied Equality this, OutputFormat format) {

    if (format.isJavaFamily()) {
      return format_java_family(format);
    }

    if (format == OutputFormat.DAIKON) {
      return format_daikon();
    }
    if (format == OutputFormat.ESCJAVA) {
      return format_esc();
    }
    // Commented out by MDE 7/27/2003.  I can't figure out whether
    // to just change JAVA_IDENTIFIER to IDENTIFIER, or whether other
    // changes are also necessary.
    // if (format == OutputFormat.JAVA_IDENTIFIER) {
    //   return format_java();
    // }
    if (format == OutputFormat.SIMPLIFY) {
      return format_simplify();
    }
    return format_unimplemented(format);
  }

  public String format_daikon(@GuardSatisfied Equality this) {
    StringBuilder result = new StringBuilder();
    boolean start = true;
    for (VarInfo var : vars) {
      if (!start) {
        result.append(" == ");
      } else {
        start = false;
      }
      result.append(var.name());
      result.append("[" + var.varinfo_index + "]");
      // result.append("[" + var.comparability + "]");
      if (var == leader()) {
        result.append("L");
      }
    }
    return result.toString();
  }

  public String format_java() {
    VarInfo leader = leader();
    String leaderName = leader.name();
    List<String> clauses = new ArrayList<>();
    for (VarInfo var : vars) {
      if (leader == var) {
        continue;
      }
      clauses.add(String.format("(%s == %s)", leaderName, var.name()));
    }
    return String.join(" && ", clauses);
  }

  public String format_esc(@GuardSatisfied Equality this) {
    String result = "";

    List<VarInfo> valid_equiv = new ArrayList<>();
    List<VarInfo> invalid_equiv = new ArrayList<>();

    List<VarInfo> equal_vars = new ArrayList<>();

    for (VarInfo other : vars) {
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
      leader = valid_equiv.get(0);
    } else {
      assert invalid_equiv.size() > 0;
      leader = invalid_equiv.get(0);
    }
    // Print the equality statements, stating expressible ones first.
    equal_vars.clear();
    equal_vars.addAll(valid_equiv);
    equal_vars.addAll(invalid_equiv);
    int numprinted = 0;
    for (int j = 0; j < equal_vars.size(); j++) {
      VarInfo other = equal_vars.get(j);
      if (other == leader) {
        continue;
      }
      if (leader.prestate_name().equals(other.name())) {
        continue;
      }
      if (other.prestate_name().equals(leader.name())) {
        continue;
      }
      if (numprinted > 0) {
        result += Global.lineSep;
      }
      numprinted++;
      if (j >= valid_equiv.size()) {
        result =
            result + "warning: method Equality.format_esc() needs to be implemented: " + format();
      }
      if (leader.rep_type.isArray()) {
        String[] form = VarInfo.esc_quantify(leader, other);
        result = result + form[0] + "( " + form[1] + " == " + form[2] + " )" + form[3];
      } else {
        result = result + leader.esc_name() + " == " + other.esc_name();
      }
    }
    return result;
  }

  // When A and B are pointers, don't say (EQ A B); instead say (EQ
  // (hash A) (hash B)).  If we said the former, Simplify would
  // presume that A and B were always interchangeable, which is not
  // the case when your programming language involves mutation.
  private String format_elt(@GuardSatisfied Equality this, String simname) {
    String result = simname;
    if (leader().is_reference()) {
      result = "(hash " + result + ")";
    }
    return result;
  }

  public String format_simplify(@GuardSatisfied Equality this) {
    StringBuilder result = new StringBuilder("(AND");
    VarInfo leader = leader();
    String leaderName = leader.simplify_name();
    if (leader.rep_type.isArray()) {
      for (VarInfo var : vars) {
        if (var == leader) {
          continue;
        }
        String[] form = VarInfo.simplify_quantify(QuantFlags.element_wise(), leader, var);
        String a = format_elt(form[1]);
        String b = format_elt(form[2]);
        result.append(" " + form[0] + "(EQ " + a + " " + b + ")" + form[3]);
      }
    } else {
      for (VarInfo var : vars) {
        if (var == leader) {
          continue;
        }
        String a = format_elt(leaderName);
        String b = format_elt(var.simplify_name());
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

  public String format_java_family(@GuardSatisfied Equality this, OutputFormat format) {
    VarInfo leader = leader();
    String leaderName = leader.name_using(format);
    List<String> clauses = new ArrayList<>();
    for (VarInfo var : vars) {
      if (leader == var) {
        continue;
      }
      if (leader.rep_type.isArray()) {
        clauses.add(
            String.format(
                "(daikon.Quant.pairwiseEqual(%s, %s)", leaderName, var.name_using(format)));
      } else {
        if (leader.type.isFloat()) {
          clauses.add("(" + Invariant.formatFuzzy("eq", leader, var, format) + ")");
        } else {
          if ((leaderName.indexOf("daikon.Quant.collectObject") != -1)
              || (var.name_using(format).indexOf("daikon.Quant.collectObject") != -1)) {
            clauses.add(
                "(warning: it is meaningless to compare hashcodes for values "
                    + "obtained through daikon.Quant.collect... methods.");
          } else {
            clauses.add(String.format("(%s == %s)", leaderName, var.name_using(format)));
          }
        }
      }
    }
    return String.join(" && ", clauses);
  }

  @SideEffectFree
  @Override
  public String toString(@GuardSatisfied Equality this) {
    return repr();
  }

  // //////////////////////////////////////////////////////////////////////
  // Processing of data

  /**
   * Return a List of VarInfos that do not fit into this set anymore.
   *
   * <p>Originally (8/14/2003), this did not check for the modified bits. It seems however, quite
   * wrong to leave variables in the same equality set when one is missing and the other is not.
   * It's possible we should go farther and break out of the equality set any variable that is
   * missingOutOfBounds (JHP).
   *
   * @param vt the newly-observed sample
   * @param count the number of times the sample is seen
   * @return a List of VarInfos that do not fit into this set anymore
   */
  public List<VarInfo> add(ValueTuple vt, int count) {
    // Need to handle specially if leader is missing.
    VarInfo leader = leader();
    Object leaderValue = leader.getValueOrNull(vt);
    int leaderMod = leader.getModified(vt);
    boolean leaderOutOfBounds = leader.missingOutOfBounds();
    if (leader.isMissing(vt)) {
    } else {
      numSamples += count;
    }

    List<VarInfo> result = new ArrayList<>();
    if (debug.isLoggable(Level.FINE)) {
      debug.fine("Doing add at " + this.ppt.parent.name() + " for " + this);
    }
    for (Iterator<VarInfo> i = vars.iterator(); i.hasNext(); ) {
      VarInfo vi = i.next();
      if (vi == leader) {
        continue;
      }
      assert vi.comparableNWay(leader);
      Object viValue = vi.getValueOrNull(vt);
      int viMod = vi.getModified(vt);
      // The following is possible because values are interned.  The
      // test also takes into account missing values, since they are
      // null.
      if ((leaderValue == viValue)
          && (leaderMod == viMod)
          && !leaderOutOfBounds
          && !vi.missingOutOfBounds()
          // If the values are NaN, treat them as different.
          && !((leaderValue instanceof Double) && ((Double) leaderValue).isNaN())) {
        // The values are the same.
        continue;
      }
      // The values differ.  Remove this from the equality set.

      //       if (debug.isLoggable(Level.FINE)) {
      //         debug.fine ("  vi name: " + vi.name.name());
      //         debug.fine ("  vi value: " + viValue);
      //         debug.fine ("  le value: " + leaderValue);
      //       }
      if (Debug.logOn()) {
        Debug.log(
            getClass(),
            ppt.parent,
            Debug.vis(vi),
            "Var "
                + vi.name()
                + " ["
                + viValue
                + ","
                + viMod
                + "] split from leader "
                + leader.name()
                + " ["
                + Debug.toString(leaderValue)
                + ","
                + leaderMod
                + "]");
      }

      result.add(vi);
      i.remove();
    }

    return result;
  }

  //  This method isn't going to be called, but it's declared abstract in Invariant.
  @Override
  protected Invariant resurrect_done(int[] permutation) {
    throw new UnsupportedOperationException();
  }

  //  This method isn't going to be called, but it's declared abstract in Invariant.
  @Pure
  @Override
  public boolean isSameFormula(Invariant other) {
    throw new UnsupportedOperationException(
        "Equality.isSameFormula(): this method should not be called");
  }

  /**
   * Convert Equality invariants into normal IntEqual type for filtering, printing, etc. Add these
   * to parent.
   *
   * <p>If the leader was changed to not be the first member of the group adds leader == leader
   * invariant as well since that invariant is used in suppressions and obvious tests.
   */
  public void postProcess() {
    if (this.numSamples() == 0) {
      // All were missing or not present
      return;
    }
    PptTopLevel parent = this.ppt.parent;
    VarInfo[] varArray = this.vars.toArray(new VarInfo[0]);
    if (debugPostProcess.isLoggable(Level.FINE)) {
      debugPostProcess.fine("Doing postProcess: " + this.format_daikon());
      debugPostProcess.fine("  at: " + this.ppt.parent.name());
    }
    VarInfo leader = leader();

    if (debugPostProcess.isLoggable(Level.FINE)) {
      debugPostProcess.fine("  var1: " + leader.name());
    }
    for (int i = 0; i < varArray.length; i++) {
      if (varArray[i] == leader) {
        continue;
      }
      if (debugPostProcess.isLoggable(Level.FINE)) {
        debugPostProcess.fine("  var2: " + varArray[i].name());
      }

      // Guard to prevent creating unnecessary Equality invariants related to
      // purity method black boxing
      if (leader.function_args != null
          && varArray[i].function_args != null
          && leader.function_args.size() > 1
          && leader.function_args.size() == varArray[i].function_args.size()) {
        boolean allEqual = true;
        for (int j = 0; j < leader.function_args.size(); j++) {
          if (!leader.function_args.get(j).isEqualTo(varArray[i].function_args.get(j))) {
            allEqual = false;
            break;
          }
        }
        if (allEqual) {
          continue;
        }
      }

      parent.create_equality_inv(leader, varArray[i], numSamples());
    }
  }

  /**
   * Switch the leader of this invariant, if possible, to a more canonical VarInfo: a VarInfo that
   * is not isDerived() is better than one that is; one that is not isDerivedParamAndUninteresting()
   * is better than one that is; and other things being equal, choose the least complex name. Call
   * this only after postProcess has been called. We do a pivot so that anything that's interesting
   * to be printed gets printed and not filtered out. For example, if a == b and a is the leader,
   * but not interesting, we still want to print f(b) as an invariant. Thus we pivot b to be the
   * leader. Later on, each relevant PptSlice gets pivoted. But not here.
   */
  public void pivot() {
    VarInfo newLeader = null;
    for (VarInfo var : vars) {
      // System.out.printf("  processing %s%n", var);
      if (newLeader == null) {
        newLeader = var;
      } else if (newLeader.isDerivedParamAndUninteresting()
          && !var.isDerivedParamAndUninteresting()) {
        // System.out.printf("%s derived and uninteresting, %s is leader%n",
        //                   newLeader, var);
        newLeader = var;
      } else if (var.isDerivedParamAndUninteresting()
          && !newLeader.isDerivedParamAndUninteresting()) {
        // do nothing
      } else if (var.derivedDepth() < newLeader.derivedDepth()) {
        // System.out.printf("%s greater depth, %s is leader%n",
        //                    newLeader, var);
        newLeader = var;
      } else if (var.derivedDepth() > newLeader.derivedDepth()) {
        // do nothing
      }
      // if we got here, this is the "all other things being equal" case
      else if (var.complexity() < newLeader.complexity()) {
        // System.out.printf("%s greater comlexity, %s is leader%n",
        //                   newLeader, var);
        newLeader = var;
      }
    }
    // System.out.printf("%s complexity = %d, %s complexity = %d%n", leaderCache,
    //                    leaderCache.complexity(), newLeader,
    //                    newLeader.complexity());
    leaderCache = newLeader;
  }

  @Override
  public void repCheck() {
    super.repCheck();
    VarInfo leader = leader();
    for (VarInfo var : vars) {
      assert VarComparability.comparable(leader, var)
          : "not comparable: " + leader.name() + " " + var.name() + " at ppt " + ppt.parent.name();
    }
  }

  @Override
  public boolean enabled(@Prototype Equality this) {
    throw new Error("do not invoke " + getClass() + ".enabled()");
  }

  @Override
  public boolean valid_types(@Prototype Equality this, VarInfo[] vis) {
    throw new Error("do not invoke " + getClass() + ".valid_types()");
  }

  @Override
  protected @NonPrototype Equality instantiate_dyn(@Prototype Equality this, PptSlice slice) {
    throw new Error("do not invoke " + getClass() + ".instantiate_dyn()");
  }

  @Override
  public @Nullable @NonPrototype Equality merge(
      @Prototype Equality this, List<@NonPrototype Invariant> invs, PptSlice parent_ppt) {
    throw new Error("Don't merge Equality invariants");
  }
}
