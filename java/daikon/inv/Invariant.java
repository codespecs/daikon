package daikon.inv;

import daikon.*;

import java.util.*;
import java.io.Serializable;

import org.apache.log4j.Category;

import utilMDE.Assert;
import utilMDE.MathMDE;
import daikon.*;

// Base implementation for Invariant objects.
// Intended to be subclassed but not to be directly instantiated.
// I should probably rename this to "Invariant" and get rid of that interface.o

public abstract class Invariant
  implements Serializable, Cloneable // but don't YOU clone it
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  /**
   * General debug tracer.
   **/
  public static final Category debug = Category.getInstance ("daikon.inv.Invariant");

  /**
   * Debug tracer for printing invariants
   **/
  public static final Category debugPrint = Category.getInstance ("daikon.inv.Invariant.print");

  /**
   * Debug tracer for isWorthPrinting() checks.
   **/
  public static final Category debugIsWorthPrinting = Category.getInstance ("daikon.inv.Invariant.isWorthPrinting");


  /**
   * Real number between 0 and 1.  The probability that the invariant
   * occurred by chance must be less than this in order for it to be
   * displayed.  (May also be set via --prob_limit switch to Daikon;
   * refer to manual.)
   **/
  public static double dkconfig_probability_limit = .01;

  /**
   * The program point for this invariant, includes values, number of
   * samples, VarInfos, etc.
   **/
  public PptSlice ppt;

  // Has to be public so wrappers can read it.
  /**
   * True exactly if the invariant is guaranteed never to hold (and should
   * be either in the process of being destroyed or about to be
   * destroyed.  This should never be set directly; instead, call destroy().
   **/
  public boolean no_invariant = false;

  /**
   * True if we've seen all values and should ignore further add() methods.
   * This is rather a hack and should be removed later.
   * Actually, it's not used any longer, except to be checked in assertions.
   **/
  // public boolean finished = false; // [INCR] (was just in assertions, is now bogus anyway)

  /**
   * The probability that this could have happened by chance alone. <br>
   *   0 = could never have happened by chance; that is, we are fully confident
   *       that this invariant is a real invariant
   **/
  public final static double PROBABILITY_JUSTIFIED = 0;

  /**
   * (0..1) = greater to lesser likelihood of coincidence <br>
   *      1 = must have happened by chance
   **/
  public final static double PROBABILITY_UNJUSTIFIED = 1;

  /**
   * 3 = delete this invariant; we know it's not true
   **/
  public final static double PROBABILITY_NEVER = 3;

  /**
   * Return 0 if x>=goal.
   * This value is 0 if x>=goal, 1 if x<=1, and otherwise grades between.
   **/
  public static final double prob_is_ge(double x, double goal) {
    if (x>=goal)
      return 0;
    if (x<=1)
      return 1;
    double result = (goal - x)/(goal-1);
    Assert.assert(0 <= result && result <= 1, "prob_and: bad result = " + result + " for (x=" + x + ", goal=" + goal + ")");
    return result;
  }

  /** Return the probability that both conditions are satisfied. */
  public static final double prob_and(double p1, double p2) {
    Assert.assert(0 <= p1 && p1 <= 1, "prob_and: bad p1 = " + p1);
    Assert.assert(0 <= p2 && p2 <= 1, "prob_and: bad p2 = " + p2);

    // 1 - (1-p1)*(1-p2)
    double result = p1 + p2 - p1*p2;

    Assert.assert(0 <= result && result <= 1, "prob_and: bad result = " + result);
    return result;
  }

  /** Return the probability that all three conditions are satisfied. */
  public static final double prob_and(double p1, double p2, double p3) {
    Assert.assert(0 <= p1 && p1 <= 1, "prob_and: bad p1 = " + p1);
    Assert.assert(0 <= p2 && p2 <= 1, "prob_and: bad p2 = " + p1);
    Assert.assert(0 <= p3 && p3 <= 1, "prob_and: bad p3 = " + p1);

    double result =  1 - (1 - p1) * (1 - p2) * (1 - p3);

    Assert.assert(0 <= result && result <= 1, "prob_and: bad result = " + result);
    return result;
  }

  /** Return the probability that eithr conditions is satisfied. */
  public static final double prob_or(double p1, double p2) {
    // Not "p1*p2" because that can produce a value too small; we don't
    // want the result to be smaller than the smaller argument.
    return Math.min(p1, p2);
  }


  // Subclasses should set these; Invariant never does.

  /**
   * At least this many samples are required, or else we don't report any
   * invariant at all.  (Except that OneOf invariants are treated differently.)
   **/
  public final static int min_mod_non_missing_samples = 5;

  /**
   * @return true if the invariant has enough samples to have its
   * computed constants well-formed.  Is overridden in classes like
   * LinearBinary/Ternary and Upper/LowerBound.
   **/
  public boolean enoughSamples() {
    return true;
  }

  // If probability == PROBABILITY_NEVER, then this invariant can be eliminated.
  /**
   * Given that this invariant has been true for all values seen so far,
   * this method returns the probability that that situation has occurred
   * by chance alone.  The result is a value between 0 and 1 inclusive.  0
   * means that this invariant could never have occurred by chance alone;
   * we are fully confident that its truth is no coincidence.  1 means that
   * the invariant is certainly a happenstance, so the truth of the
   * invariant is not relevant and it should not be reported.  Values
   * between 0 and 1 give differing confidences in the invariant.
   * <p>
   *
   * As an example, if the invariant is "x!=0", and only one value, 22, has
   * been seen for x, then the conclusion "x!=0" is not justified.  But if
   * there have been 1,000,000 values, and none of them were 0, then we may
   * be confident that the property "x!=0" is not a coincidence.
   * <p>
   *
   * This method need not check the value of variable no_invariant, as the
   * caller does that.
   **/
  public double getProbability() {
    if (no_invariant)
      return PROBABILITY_NEVER;
    double result = computeProbability();
    if (result > PROBABILITY_NEVER) {
      // Can't print this.repr_prob(), as it may compute the probability!
      System.out.println("Bad invariant probability " + result + ": ");
      System.out.println(this.getClass());
      System.out.println(repr());
      System.out.println(this.format());
    }
    // System.out.println("getProbability: " + getClass().getName() + " " + ppt.varNames());
    Assert.assert((result == PROBABILITY_JUSTIFIED)
		  || (result == PROBABILITY_UNJUSTIFIED)
		  || (result == PROBABILITY_NEVER)
		  || ((0 <= result) && (result <= 1))
                  // This can be expensive, so comment out.
                  // , getClass().getName() + ": " + repr()
                  );
    return result;
  }

  /**
   * This method computes the probability that this invariant occurred by chance.
   * Users should use getProbability() instead.
   * @see     #getProbability()
   **/
  protected abstract double computeProbability();

  public boolean justified() {
    return (!no_invariant) && enoughSamples()
      && (getProbability() <= dkconfig_probability_limit);
  }

  /**
   * Subclasses should override.  An exact invariant indicates that given
   * all but one variable value, the last one can be computed.  (I think
   * that's correct, anyway.)  Examples are IntComparison (when only
   * equality is possible), LinearBinary, FunctionUnary.
   * OneOf is treated differently, as an interface.
   * The result of this method does not depend on whether the invariant is
   * justified, destroyed, etc.
   **/
  public boolean isExact() {
    return false;
  }

  // Implementations of this need to examine all the data values already
  // in the ppt.  Or, don't put too much work in the constructor and instead
  // have the caller do that.
  protected Invariant(PptSlice ppt) {
    this.ppt = ppt;
  }

  /**
   * Mark this invariant as falsified.
   * Invariants must also call flow when they are falsified
   * Has to be public because of wrappers; do not call from outside world.
   * @see flow
   **/
  public void destroy() {
    no_invariant = true;
    PptSlice.debugFlow.debug("Invariant destoroyed " + format() + " at " + ppt.parent.name);
    // ppt.removeInvariant(this);
  }

  /**
   * Flow argument to all lower program points.
   * Has to be public because of wrappers (?); do not call from outside world.
   * @see destroy
   **/
  public void flow(Invariant flowed) {
    ppt.addToFlow(flowed);
  }

  /**
   * Essentially the same as flow(this).  Useful way to flow oneself
   * without much hassle (as long as internal state is still OK).
   * Nice point of control in case we later have to tweak things when
   * flowing outselves.
   **/
  public void flowThis() {
    flow(this);
  }

  /**
   * Essentially the same as flow(this.clone()).  Useful way to flow
   * oneself without much hassle (as long as internal state is still
   * OK).  Nice point of control in case we later have to tweak things
   * when flowing outselves.
   **/
  public void flowClone() {
    Invariant flowed = (Invariant) this.clone();
    flow(flowed);
  }

  /** Do nothing special.  Overridden to remove exception from declaration */
  protected Object clone() {
    try {
      return super.clone();
    } catch (CloneNotSupportedException e) {
      throw new Error(); // can never happen
    }
  }

  /**
   * Take a falsified invariant and resurrect it in a new PptSlice.
   * @param new_ppt must have the same arity and types
   * @param permutation gives the varinfo array index mapping
   **/
  public Invariant resurrect(PptSlice new_ppt,
			     int[] permutation
			     )
  {
    // Check some sanity conditions
    Assert.assert(no_invariant);
    Assert.assert(new_ppt.arity == ppt.arity);
    Assert.assert(permutation.length == ppt.arity);
    for (int i=0; i < ppt.arity; i++) {
      VarInfo oldvi = ppt.var_infos[i];
      VarInfo newvi = new_ppt.var_infos[permutation[i]];
      Assert.assert(oldvi.type == newvi.type);
      Assert.assert(oldvi.rep_type == newvi.rep_type);
      Assert.assert(oldvi.file_rep_type == newvi.file_rep_type);
    }

    Invariant result;
    // Clone it
    result = (Invariant) this.clone();

    // Fix up the fields
    result.no_invariant = false;
    result.ppt = new_ppt;
    // Let subclasses fix what they need to
    result = result.resurrect_done(permutation);

    return result;
  }

  /**
   * Called on the new invariant just before resurrect() returns it to
   * allow subclasses to fix any information they might have cached
   * from the old Ppt and VarInfos.
   **/
  protected abstract Invariant resurrect_done(int[] permutation);

  // Regrettably, I can't declare a static abstract method.
  // // The return value is probably ignored.  The new Invariant installs
  // // itself on the PptSlice, and that's what really matters (right?).
  // public static abstract Invariant instantiate(PptSlice ppt);

  public boolean usesVar(VarInfo vi) {
    return ppt.usesVar(vi);
  }

  public boolean usesVar(String name) {
    return ppt.usesVar(name);
  }

  // Not used as of 1/31/2000.
  // // For use by subclasses.
  // /** Put a string representation of the variable names in the StringBuffer. */
  // public void varNames(StringBuffer sb) {
  //   // sb.append(this.getClass().getName());
  //   ppt.varNames(sb);
  // }

  /** Return a string representation of the variable names. */
  final public String varNames() {
    return ppt.varNames();
  }

  final public String name() {
    return this.getClass().getName() + varNames();
  }

  // Should not include result of getProbability because this may be called
  // from computeProbability or elsewhere for debugging purposes.
  /**
   * For printing invariants, there are two interfaces:
   * repr gives a low-level representation
   * (repr_prop also prints the probability), and
   * format gives a high-level representation for user output.
   **/
  public String repr() {
    // A better default would be to use reflection and print out all
    // the variable names.
    return getClass() + varNames() + ": " + format();
  }

  /**
   * For printing invariants, there are two interfaces:
   * repr gives a low-level representation
   * (repr_prop also prints the probability), and
   * format gives a high-level representation for user output.
   **/
  public String repr_prob() {
    return repr() + "; probability = " + getProbability();
  }

  /**
   * Enumeration type for output style.
   * (Should this be somewhere else?)
   **/
  public final static class OutputFormat
  {
    /* The standard, concise daikon output format */
    public static final OutputFormat DAIKON = new OutputFormat("Daikon");
    /* ESC/Java's annotation language */
    public static final OutputFormat ESCJAVA = new OutputFormat("ESC/Java");
    /* Simplify theorem prover */
    public static final OutputFormat SIMPLIFY = new OutputFormat("Simplify");
    /* IOA language */
    public static final OutputFormat IOA = new OutputFormat("IOA");
    /* Java boolean expression */
    public static final OutputFormat JAVA = new OutputFormat("Java");

    private final String name;
    public final String toString() { return "OutputFormat:" + name; }

    // Nobody should ever construct these
    private OutputFormat(String name) {
      this.name = name;
    }
  }

  /**
   * For printing invariants, there are two interfaces:
   * repr gives a low-level representation
   * (repr_prop also prints the probability), and
   * format gives a high-level representation for user output.
   **/
  public String format() {
    return format_using(OutputFormat.DAIKON);
  }

  public abstract String format_using(OutputFormat format);

  /**
   * @return conjuction of mapping the same function of our
   * expresssions's VarInfos, in general.  Subclasses may override if
   * they are able to handle generally-inexpressible properties in
   * special-case ways.
   *
   * @see VarInfo.isValidEscExpression
   **/
  public boolean isValidEscExpression() {
    for (int i=0; i < ppt.var_infos.length; i++) {
      if (! ppt.var_infos[i].isValidEscExpression()) {
	return false;
      }
    }
    return true;
  }

  /**
   * @return standard "format needs to be implemented" for the given
   * requested format.  Made public so cores can call it.
   **/
  public String format_unimplemented(OutputFormat request) {
    if ((request == OutputFormat.IOA) && debugPrint.isDebugEnabled()) {
      debugPrint.debug ("Format_ioa: " + this.toString());
    }
    String classname = this.getClass().getName();
    return "warning: method " + classname + ".format(" + request + ")"
      + "needs to be implemented: " + format();
  }

  // This should perhaps be merged with some kind of PptSlice comparator.
  /**
   * Compare based on arity, then printed representation.
   **/
  public static final class InvariantComparatorForPrinting implements Comparator {
    public int compare(Object o1, Object o2) {
      if (o1 == o2)
	return 0;
      Invariant inv1 = (Invariant)o1;
      Invariant inv2 = (Invariant)o2;
      // Assert.assert(inv1.ppt.parent == inv2.ppt.parent);
      VarInfo[] vis1 = inv1.ppt.var_infos;
      VarInfo[] vis2 = inv2.ppt.var_infos;
      int arity_cmp = vis1.length - vis2.length;
      if (arity_cmp != 0)
	return arity_cmp;
      // Comparing on variable index is wrong in general:  variables of the
      // same name may have different indices at different program points.
      // However, it's safe if the invariants are from the same program
      // point.  Also, it is nice to avoid changing the order of variables
      // from that of the data trace file.

      if (inv1.ppt.parent == inv2.ppt.parent) {
        for (int i=0; i<vis1.length; i++) {
          int tmp = vis1[i].varinfo_index - vis2[i].varinfo_index;
          if (tmp != 0) {
            // This can happen when variable names have been changed by
            // VarInfo.simplify_expression().  For now, hope for the best.
            // (That is, hope it doesn't produce multiple invariants or
            // confused formatting.)
            // if (inv1.format().equals(inv2.format())) {
            //   System.out.println("ICFP says different, but same formatting:");
            //   System.out.println("  " + inv1.format() + " " + inv1.repr() + " at " + inv1.ppt.name);
            //   System.out.println(" var #" + vis1[i].varinfo_index + " = " + vis1[i].name + " = " + vis1[i]);
            //   System.out.println("  " + inv2.format() + " " + inv2.repr() + " at " + inv2.ppt.name);
            //   System.out.println(" var #" + vis2[i].varinfo_index + " = " + vis2[i].name + " = " + vis2[i]);
            // }

            // // Debugging
            // System.out.println("ICFP: compare var "
            //                    + vis1[i].name.name() + " (index " + vis1[i].varinfo_index + ")"
            //                    + " to " + vis2[i].name.name() + " (index " + vis2[i].varinfo_index + ")"
            //                    + " => " + tmp
            //                    + "\t\tfor " + inv1.format() + ", " + inv2.format());
            // System.out.println("Vars for " + inv1.format() + ": " + inv1.repr());
            // System.out.println("Vars for " + inv2.format() + ": " + inv2.repr());

            return tmp;
          }
        }
      } else {
        // // Debugging
        // System.out.println("ICFP: different parents for " + inv1.format() + ", " + inv2.format());

        for (int i=0; i<vis1.length; i++) {
          String name1 = vis1[i].name.name();
          String name2 = vis2[i].name.name();
          if (name1.equals(name2)) {
            continue;
          }
          int name1in2 = ((PptTopLevel)inv2.ppt.parent).indexOf(name1);
          int name2in1 = ((PptTopLevel)inv1.ppt.parent).indexOf(name2);
          int cmp1 = (name1in2 == -1) ? 0 : vis1[i].varinfo_index - name1in2;
          int cmp2 = (name2in1 == -1) ? 0 : vis2[i].varinfo_index - name2in1;
          int cmp = MathMDE.sign(cmp1) + MathMDE.sign(cmp2);
          if (cmp != 0)
            return cmp;
        }
      }

      // Sort OneOf invariants earlier than others
      if ((inv1 instanceof OneOf) && (! (inv2 instanceof OneOf)))
        return -1;
      if ((! (inv1 instanceof OneOf)) && (inv2 instanceof OneOf))
        return 1;

      // System.out.println("ICFP: default rule yields "
      //                    + inv1.format().compareTo(inv2.format())
      //                    + " for " + inv1.format() + ", " + inv2.format());
      return inv1.format().compareTo(inv2.format());
    }
  }

  /**
   * @return true iff the two invariants represent the same
   * mathematical formula.  Does not consider the context such as
   * variable names, confidences, sample counts, value counts, or
   * related quantities.  As a rule of thumb, if two invariants format
   * the same, this method returns true.  Furthermore, in many cases,
   * if an invariant does not involve computed constants (as "x&gt;c" and
   * "y=ax+b" do for constants a, b, and c), then this method vacuously
   * returns true.
   *
   * @exception RuntimeException if other.getClass() != this.getClass()
   **/
  public boolean isSameFormula(Invariant other) {
    return false;
  }

  public static interface IsSameInvariantNameExtractor
  {
    public VarInfoName getFromFirst(VarInfo var1);
    public VarInfoName getFromSecond(VarInfo var2);
  }

  public static class DefaultIsSameInvariantNameExtractor
    implements IsSameInvariantNameExtractor
  {
    public VarInfoName getFromFirst(VarInfo var1)  { return var1.name; }
    public VarInfoName getFromSecond(VarInfo var2) { return var2.name; }
  }
  private static final IsSameInvariantNameExtractor defaultIsSameInvariantNameExtractor = new DefaultIsSameInvariantNameExtractor();

  /**
   * @return true iff the argument is the "same" invariant as this.
   * Same, in this case, means a matching type, formula, and variable
   * names.
   **/
  public boolean isSameInvariant(Invariant inv2)
  {
    return isSameInvariant(inv2, defaultIsSameInvariantNameExtractor);
  }

  /**
   * @param name_extractor lambda to extract the variable name from the VarInfos
   * @return true iff the argument is the "same" invariant as this.
   * Same, in this case, means a matching type, formula, and variable
   * names.
   **/
  public boolean isSameInvariant(Invariant inv2,
				 IsSameInvariantNameExtractor name_extractor)
  {
    Invariant inv1 = this;

    // Can't be the same if they aren't the same type
    if (!inv1.getClass().equals(inv2.getClass())) {
      return false;
    }

    // Can't be the same if they aren't the same formula
    if (!inv1.isSameFormula(inv2)) {
      return false;
    }

    // System.out.println("isSameInvariant(" + inv1.format() + ", " + inv2.format() + ")");

    // The variable names much match up, in order

    VarInfo[] vars1 = inv1.ppt.var_infos;
    VarInfo[] vars2 = inv2.ppt.var_infos;

    Assert.assert(vars1.length == vars2.length); // due to inv type match already
    for (int i=0; i < vars1.length; i++) {
      VarInfo var1 = vars1[i];
      VarInfo var2 = vars2[i];

      // Do the easy check first
      if (name_extractor.getFromFirst(var1).equals(name_extractor.getFromSecond(var2))) {
	continue;
      }

      // The names "match" iff there is an intersection of the names
      // of equal variables.
      Vector all_vars1 = new Vector(); // var1.canonicalRep().equalTo(); // [INCR]
      Vector all_vars2 = new Vector(); // var2.canonicalRep().equalTo(); // [INCR]
      all_vars1.add(var1); // all_vars1.add(var1.canonicalRep()); // [INCR]
      all_vars1.add(var2); // all_vars2.add(var2.canonicalRep()); // [INCR]
      Vector all_vars_names1 = new Vector(all_vars1.size());
      for (Iterator iter = all_vars1.iterator(); iter.hasNext(); ) {
	VarInfo elt = (VarInfo) iter.next();
	VarInfoName name = name_extractor.getFromFirst(elt);
	all_vars_names1.add(name);
      }
      boolean intersection = false;
      for (Iterator iter = all_vars2.iterator(); iter.hasNext(); ) {
	VarInfo elt = (VarInfo) iter.next();
	VarInfoName name = name_extractor.getFromSecond(elt);
	intersection = all_vars_names1.contains(name);
	if (intersection) {
	  break;
	}
      }
      if (!intersection) {
	return false;
      }
    }

    // System.out.println("TRUE: isSameInvariant(" + inv1.format() + ", " + inv2.format() + ")");

    // the type, formula, and vars all matched
    return true;
  }


  /**
   * @return true iff the two invariants represent mutually exclusive
   * mathematical formulas -- that is, if one of them is true, then the
   * other must be false.  This method does not consider the context such
   * as variable names, confidences, sample counts, value counts, or
   * related quantities.
   **/
  public boolean isExclusiveFormula(Invariant other) {
    return false;
  }


  // This is a little grody; stick with code cut-and-paste for now.
  // // Look up a previously instantiated Invariant.
  // // Should this implementation be made more efficient?
  // public static Invariant find(Class invclass, PptSlice ppt) {
  //   for (Iterator itor = ppt.invs.iterator(); itor.hasNext(); ) {
  //     Invariant inv = (Invariant) itor.next();
  //     if (inv instanceof invclass)
  //       return inv;
  //   }
  //   return null;
  // }



  // Diff replaced by package daikon.diff

  //    String diff(Invariant other) {
  //      throw new Error("Unimplemented invariant diff for " + this.getClass() + " and " + other.getClass() + ": " + this.format() + " " + other.format());
  //    }

  //     # Possibly add an optional "args=None" argument, for formatting.
  //     def diff(self, other):
  //         """Returns None or a description of the difference."""
  //         # print "diff(invariant)"
  //         inv1 = self
  //         inv2 = other
  //         assert inv1.__class__ == inv2.__class__
  //         if inv1.is_unconstrained() and inv2.is_unconstrained():
  //             return None
  //         if inv1.is_unconstrained() ^ inv2.is_unconstrained():
  //             return "One is unconstrained but the other is not"
  //         if inv1.one_of and inv2.one_of and inv1.one_of != inv2.one_of:
  //             return "Different small number of values"
  //         if inv1.can_be_None ^ inv2.can_be_None:
  //             return "One can be None but the other cannot"
  //         # return "invariant.diff: no differences"  # debugging
  //         return None



  ///////////////////////////////////////////////////////////////////////////
  /// Tests about the invariant (for printing)
  ///

  // DO NOT OVERRIDE.  Should be declared "final", but the "final" is
  // omitted to allow for easier testing.
  public boolean isWorthPrinting()
  {
    if (debugIsWorthPrinting.isDebugEnabled()) {
      debugIsWorthPrinting.debug("isWorthPrinting: " + format() + " at " + ppt.name);
    }

    // It's hard to know in exactly what order to do these checks that
    // eliminate some invariants from consideration.  Which is cheapest?
    // Which is most often successful?  Which assume others have already
    // been performed?
    if (! isWorthPrinting_sansControlledCheck()) {
      if (debugIsWorthPrinting.isDebugEnabled()) {
        debugIsWorthPrinting.debug("  not worth printing, sans controlled check: " + format() + " at " + ppt.name);
      }
      return false;
    }

    // [INCR] ...
    // The invariant is worth printing on its own merits, but it may be
    // controlled.  If any (transitive) controller is worth printing, don't
    // print this one.
    // Use _sorted version for reproducibility.  (There's a bug, but I can't find it.)
    /*
    if (debugIsWorthPrinting.isDebugEnabled()) {
      debugIsWorthPrinting.debug("Calling find_controlling_invariants_sorted(" + format() + ")");
    }
    Vector contr_invs = find_controlling_invariants_sorted();
    if (debugIsWorthPrinting.isDebugEnabled()) {
      if (contr_invs.size() == 0) {
        debugIsWorthPrinting.debug("No controllers for " + format());
      }
    }

    Vector processed = new Vector();
    while (contr_invs.size() > 0) {
      Invariant contr_inv = (Invariant) contr_invs.remove(0);
      if (debugIsWorthPrinting.isDebugEnabled()) {
        debugIsWorthPrinting.debug("Controller " + contr_inv.format() + " at " +
				   contr_inv.ppt.name + " for: " + format() + " at " + ppt.name);
      }

      processed.add(contr_inv);
      if (contr_inv.isWorthPrinting_sansControlledCheck()) {
	// we have a printable controller, so we shouldn't print
        if (debugIsWorthPrinting.isDebugEnabled()) {
          debugIsWorthPrinting.debug("  not worth printing, sans controlled check, due to controller " +
				     contr_inv.format() + " at " + contr_inv.ppt.name + ": " + format() + " at " + ppt.name);
        }
        return false;
      }
      // find the controlling invs of contr_inv and add them to the
      // working set iff the are not already in it and they have not
      // been processed already
      Iterator iter = contr_inv.find_controlling_invariants().iterator();
      while (iter.hasNext()) {
	Object elt = iter.next();
	if (!processed.contains(elt) && !contr_invs.contains(elt)) {
	  contr_invs.add(elt);
	}
      }
    }

    // No controller was worth printing
    if (debugIsWorthPrinting.isDebugEnabled()) {
      debugIsWorthPrinting.debug("isWorthPrinting => true for: " + format() + " at " + ppt.name);
    }
    */
    // ... [INCR]
    return true;
  }


  /**
   * Like isWorthPrinting, but doesn't check whether the invariant is controlled.
   **/
  final public boolean isWorthPrinting_sansControlledCheck() {
    if (this instanceof Implication) {
      Implication impl = (Implication) this;
      if (debugIsWorthPrinting.isDebugEnabled()) {
        debugIsWorthPrinting.debug("iwpscc(" + format() + ") dispatching");
      }
      return impl.predicate.isWorthPrinting() && impl.consequent.isWorthPrinting();
    }

    if (debugIsWorthPrinting.isDebugEnabled()) {
      System.out.println(isWorthPrinting_sansControlledCheck_debug());
    }
    boolean result
      = ((! hasFewModifiedSamples())
         && enoughSamples()       // perhaps replaces hasFewModifiedSamples
         // && (! hasNonCanonicalVariable()) [INCR]
         // && (! hasOnlyConstantVariables()) [INCR]
         && (! isObvious())
	 && justified()
         // && isWorthPrinting_PostconditionPrestate() [INCR]
	 );
    return result;
  }

  final public String isWorthPrinting_sansControlledCheck_debug() {
    return
      "iwpscc(" + format() + " @ " + ppt.name
      + ") <= " + (! hasFewModifiedSamples())
      + " " + enoughSamples()
      // + " " + (! hasNonCanonicalVariable()) [INCR]
      // + " " + (! hasOnlyConstantVariables()) [INCR]
      + " " + (! isObvious())
      + " " + justified()
      // + " " + isWorthPrinting_PostconditionPrestate() [INCR]
      ;
  }

  /**
   * @return true if this invariant has few modified (non-repeated) samples.
   * An exception is made for OneOf invariants.
   **/
  public final boolean hasFewModifiedSamples() {
    int num_mod_non_missing_samples = ppt.num_mod_non_missing_samples();

    if (this instanceof OneOf) {
      // A OneOf should have at least as many samples as it has values.
      // Was an assert...
      /* [INCR]
      if (((OneOf) this).num_elts() > num_mod_non_missing_samples) {
        System.out.println("OneOf problem: num_elts " + ((OneOf) this).num_elts() + ", num_mod " + num_mod_non_missing_samples + ": " + format());
      }
      */
      return false;
    } else {
      boolean result = (num_mod_non_missing_samples < Invariant.min_mod_non_missing_samples);
      // if (! result) {
      //   System.out.println("hasFewModifiedSamples: " + format());
      // }
      return result;
    }
  }

  // This used to be final, but I want to override in EqualityInvariant
  /** @return true if this invariant involves a non-canonical variable **/
  /* [INCR] ...
  public boolean hasNonCanonicalVariable() {
    VarInfo[] vis = ppt.var_infos;
    for (int i=0; i<vis.length; i++) {
      if (! vis[i].isCanonical()) {
        return true;
      }
    }
    return false;
  }
  */ // ... [INCR]

  /**
   * @return true if this invariant involves only constant variables
   *         and is a comparison
   **/
  /* [INCR] ...
  public boolean hasOnlyConstantVariables() {
    VarInfo[] varInfos = ppt.var_infos;
    for (int i=0; i < varInfos.length; i++) {
      if (! varInfos[i].isConstant())
	return false;
    }

    // At this point, we know all variables are constant.
    Assert.assert(this instanceof OneOf ||
		  this instanceof Comparison ||
		  this instanceof Equality
		  , "Unexpected invariant with all vars constant: "
		  + this + "  " + repr_prob() + "  " + format()
		  );
    if (this instanceof Comparison) {
      //      Assert.assert(! IsEqualityComparison.it.accept(this));
      if (debugPrint.isDebugEnabled())
	debugPrint.debug("  [over constants:  " + this.repr_prob() + " ]");
      return true;
    }
    return false;
  }
  */ // ... [INCR]

  /**
   * @return true if this invariant is necessarily true, due to derived
   * variables, other invariants, etc.
   * Intended to be overridden by subclasses.
   **/
  public final boolean isObvious() {
    // Actually actually, we'll eliminate invariants as they become obvious
    // rather than on output; the point of this is to speed up computation.
    // // Actually, we do need to check isObviousDerived after all because we
    // // add invariants that might be obvious, but might also turn out to be
    // // even stronger (and so not obvious).  We don't know how the invariant
    // // turns out until after testing it.
    // // // We don't need to check isObviousDerived because we won't add
    // // // obvious-derived invariants to lists in the first place.
    if (isObviousDerived() || isObviousImplied()) {
      if (debugPrint.isDebugEnabled())
	debugPrint.debug("  [obvious:  " + repr_prob() + " ]");
      return true;
    }
    return false;
  }

  /**
   * @return true if this invariant is necessarily true, due to being implied
   * by other (more basic or preferable to report) invariants.
   * Intended to be overridden by subclasses.
   **/
  public boolean isObviousDerived() {
    return false;
  }

  /**
   * @return true if this invariant is necessarily true, due to being implied
   * by other (more basic or preferable to report) invariants.
   * Intended to be overridden by subclasses.
   **/
  public boolean isObviousImplied() {
    return false;
  }

  // [INCR] ...
  /**
   * @return true if this invariant is controlled by another invariant
   **/
  /*
  public boolean isControlled() {
    Vector controllers = this.find_controlling_invariants();
    return (controllers.size() > 0);
  }
  */

  /**
   * @return true if this invariant is a postcondition that is implied
   * by prestate invariants.  For example, if an entry point has the
   * invariant x+3=y, and this invariant is the corresponding exit
   * point invariant orig(x)+3=orig(y), then this methods returns
   * true.
   **/
  /*
  public boolean isImpliedPostcondition() {
    PptTopLevel topLevel = (PptTopLevel) ppt.parent;
    if (topLevel.entry_ppt() != null) { // if this is an exit point invariant
      Iterator entryInvariants = topLevel.entry_ppt().invariants_vector().iterator(); // unstable
      while (entryInvariants.hasNext()) {
	Invariant entryInvariant = (Invariant) entryInvariants.next();
	// If entryInvariant with orig() applied to everything matches this invariant
	if (entryInvariant.isSameInvariant( this, preToPostIsSameInvariantNameExtractor))
	  return true;
      }
    }
    return false;
  }

  private boolean isWorthPrinting_PostconditionPrestate()
  {
    PptTopLevel pptt = (PptTopLevel) ppt.parent;

    if (Daikon.suppress_implied_postcondition_over_prestate_invariants) {
      if (pptt.entry_ppt != null) {
	Iterator entry_invs = pptt.entry_ppt.invariants_iterator(); // unstable
	while (entry_invs.hasNext()) {
	  Invariant entry_inv = (Invariant) entry_invs.next();
	  // If entry_inv with orig() applied to everything matches this
	  if (entry_inv.isSameInvariant(this, preToPostIsSameInvariantNameExtractor)) {
	    if (entry_inv.isWorthPrinting_sansControlledCheck()) {
              if (debugIsWorthPrinting.isDebugEnabled()) {
                debugIsWorthPrinting.debug("isWorthPrinting_PostconditionPrestate => false for " + format());
              }
	      return false;
	    }
	  }
	}
      }
    }
    return true;
  }
  */

  /**
   * Used in isImpliedPostcondition() and isWorthPrinting_PostconditionPrestate().
   **/
  /*
  private final static IsSameInvariantNameExtractor preToPostIsSameInvariantNameExtractor =
    new DefaultIsSameInvariantNameExtractor() {
	public VarInfoName getFromFirst(VarInfo var)
	{ return super.getFromFirst(var).applyPrestate(); }
      };
  */

  /**
   * Returns a Vector[Invariant] which are the sameInvariant as this,
   * drawn from the invariants of this.ppt.parent.controllers.
   **/
  /*
  public Vector find_controlling_invariants()
  {
    // We used to assume there was at most one of these, but that
    // turned out to be wrong.  If this ppt has more equality
    // invariants than the controller, two different invariants can
    // match.  For example, a controller might say "a > 0", "b > 0" as
    // two different invariants, but if this ppt also has "a == b"
    // then both invariants should be returned.  This especailly
    // matters if, for example, "a > 0" was obvious (and thus wouldn't
    // suppress this invariant).
    Vector results = new Vector();

    if (debugIsWorthPrinting.isDebugEnabled()) {
      debugIsWorthPrinting.debug("find_controlling_invariants: " + format());
    }
    PptTopLevel pptt = (PptTopLevel) ppt.parent;

    // Try to match inv against all controlling invariants
    Iterator controllers = pptt.controlling_ppts.iterator();
    while (controllers.hasNext()) {
      PptTopLevel controller = (PptTopLevel) controllers.next();
      if (debugIsWorthPrinting.isDebugEnabled()) {
        debugIsWorthPrinting.debug("Looking for controller of " + format() + " in " + controller.name);
      }
      Iterator candidates = controller.invariants_iterator(); // unstable
      while (candidates.hasNext()) {
	Invariant cand_inv = (Invariant) candidates.next();
	if (isSameInvariant(cand_inv)) {
          if (debugIsWorthPrinting.isDebugEnabled()) {
            debugIsWorthPrinting.debug("Controller found: " + cand_inv.format() + " [worth printing: " + cand_inv.isWorthPrinting() + "]]");
          }
	  results.add(cand_inv);
	}
        if (debugIsWorthPrinting.isDebugEnabled()) {
          debugIsWorthPrinting.debug("Failed candidate: " + cand_inv.format());
        }
      }
    }

    return results;
  }
  */
  // For reproducible results when debugging
  /*
  static Comparator invComparator = new Invariant.ClassVarnameComparator();
  public Vector find_controlling_invariants_sorted() {
    Vector unsorted = find_controlling_invariants();
    Invariant[] invs = (Invariant[]) unsorted.toArray(new Invariant[0]);
    Arrays.sort(invs, invComparator);
    Vector result = new Vector(invs.length);
    for (int i=0; i<invs.length; i++)
      result.add(invs[i]);
    return result;
  }
  */
  // ... [INCR]


  // Uninteresting invariants will override this method to return
  // false
  public boolean isInteresting() {
    return true;
  }

  // Orders invariants by class, then by variable names.  If the
  // invariants are both of class Implication, they are ordered by
  // comparing the predicate, then the consequent.
  public static final class ClassVarnameComparator implements Comparator {
    public int compare(Object o1, Object o2) {
      Invariant inv1 = (Invariant) o1;
      Invariant inv2 = (Invariant) o2;

      if (inv1 instanceof Implication && inv2 instanceof Implication)
        return compareImplications((Implication) inv1, (Implication) inv2);

      int compareClass = compareClass(inv1, inv2);
      if (compareClass != 0)
        return compareClass;

      return compareVariables(inv1, inv2);
    }

    // Returns 0 if the invariants are of the same class.  Else,
    // returns the comparison of the class names.
    private int compareClass(Invariant inv1, Invariant inv2) {
      if (inv1.getClass().equals(inv2.getClass())) {
        return 0;
      } else {
        String classname1 = inv1.getClass().getName();
        String classname2 = inv2.getClass().getName();
        return classname1.compareTo(classname2);
      }
    }

    // Returns 0 if the invariants have the same variable names.
    // Else, returns the comparison of the first variable names that
    // differ.  Requires that the invariants be of the same class.
    private int compareVariables(Invariant inv1, Invariant inv2) {
      VarInfo[] vars1 = inv1.ppt.var_infos;
      VarInfo[] vars2 = inv2.ppt.var_infos;

      // due to inv type match already
      Assert.assert(vars1.length == vars2.length);

      for (int i=0; i < vars1.length; i++) {
        VarInfo var1 = vars1[i];
        VarInfo var2 = vars2[i];
        int compare = var1.name.compareTo(var2.name);
        if (compare != 0) return compare;
      }

      // All the variable names matched
      return 0;
    }

    private int compareImplications(Implication inv1, Implication inv2) {
      int comparePredicate = compare(inv1.predicate, inv2.predicate);
      if (comparePredicate != 0)
        return comparePredicate;

      return compare(inv1.consequent, inv2.consequent);
    }
  }

  /**
   * Orders invariants by class, then variable names, then formula.
   * If the formulas are the same, compares the printed representation
   * obtained from the format() method.
   **/
  public static final class ClassVarnameFormulaComparator
    implements Comparator {

    Comparator classVarnameComparator = new ClassVarnameComparator();

    public int compare(Object o1, Object o2) {
      int compareClassVarname = classVarnameComparator.compare(o1, o2);

      if (compareClassVarname != 0) {
        return compareClassVarname;
      }

      Invariant inv1 = (Invariant) o1;
      Invariant inv2 = (Invariant) o2;

      if (inv1.isSameInvariant(inv2)) {
        return 0;
      }

      int result = inv1.format().compareTo(inv2.format());

      Assert.assert(result != 0, "isSameInvariant() returned false, " +
                    "but compareTo() returned 0");

      return result;
    }
  }

}


//     def format(self, args=None):
//         if self.one_of:
//             # If it can be None, print it only if it is always None and
//             # is an invariant over non-derived variable.
//             if self.can_be_None:
//                 if ((len(self.one_of) == 1)
//                     and self.var_infos):
//                     some_nonderived = false
//                     for vi in self.var_infos:
//                      some_nonderived = some_nonderived or not vi.is_derived
//                     if some_nonderived:
//                         return "%s = uninit" % (args,)
//             elif len(self.one_of) == 1:
//                 return "%s = %s" % (args, self.one_of[0])
//             ## Perhaps I should unconditionally return this value;
//             ## otherwise I end up printing ranges more often than small
//             ## numbers of values (because when few values and many samples,
//             ## the range always looks justified).
//             # If few samples, don't try to infer a function over the values;
//             # just return the list.
//             elif (len(self.one_of) <= 3) or (self.samples < 100):
//                 return "%s in %s" % (args, util.format_as_set(self.one_of))
//         return None
