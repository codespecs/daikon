package daikon.inv;

import daikon.*;

import java.util.*;

import utilMDE.*;

// Base implementation for Invariant objects.
// Intended to be subclassed but not to be directly instantiated.
// I should probably rename this to "Invariant" and get rid of that interface.o

public abstract class Invariant implements java.io.Serializable {
  public PptSlice ppt;      // includes values, number of samples, VarInfos, etc.

  // Has to be public so wrappers can read it.
  /**
   * True exactly if the invariant is guaranteed never to hold (and should
   * be either in the process of being destroyed or about to be
   * destroyed.  This should never be set directly; instead, call destroy().
   **/
  public boolean no_invariant = false;

  // True if we've seen all values and should ignore further add() methods.
  // This is rather a hack and should be removed later.
  // Actually, it's not used any longer, except to be checked in assertions.
  public boolean finished = false;

  // Subclasses should set these; Invariant never does.

  // The probability that this could have happened by chance alone.
  //   0 = could never have happened by chance; that is, we are fully confident
  //       that this invariant is a real invariant
  public final static double PROBABILITY_JUSTIFIED = 0;
  //   (0..1) = greater to lesser likelihood of coincidence
  //   1 = must have happened by chance
  public final static double PROBABILITY_UNJUSTIFIED = 1;
  //   2 = we haven't yet seen enough data to know whether this invariant is
  //       true, much less its justification
  public final static double PROBABILITY_UNKNOWN = 2;
  //   3 = delete this invariant; we know it's not true
  public final static double PROBABILITY_NEVER = 3;

  // The probability that the invariant occurred by chance must be less
  // than this in order for it to be displayed.
  public static double probability_limit = .01;

  /**
   * At least this many samples are required, or else we don't report any
   * invariant at all.  (Except that OneOf invariants are treated differently.)
   **/
  public final static int min_mod_non_missing_samples = 5;

  // // Do I want to have this cache at all?  It may not be that expensive
  // // to compute from scratch, and I may not be interested in it that often.
  // protected double probability_cache = 0;
  // protected boolean probability_cache_accurate = false;

  // If probability == PROBABILITY_NEVER, then this invariant can be eliminated.
  public double getProbability() {
    if (no_invariant)
      return PROBABILITY_NEVER;
    double result = computeProbability();
    if (result > PROBABILITY_NEVER) {
      // Can't print this.repr(), as it may compute the probability!
      System.out.println("Bad invariant probability " + result + ": \n" + this.getClass() + "\n" +
			 this.format() + "\n");
    }
    Assert.assert((result == PROBABILITY_JUSTIFIED)
		  || (result == PROBABILITY_UNJUSTIFIED)
		  || (result == PROBABILITY_UNKNOWN)
		  || (result == PROBABILITY_NEVER)
		  || ((0 <= result) && (result <= 1)));
    return result;
    // if (!probability_cache_accurate) {
    //   probability_cache = computeProbability();
    //   probability_cache_accurate = true;
    // }
    // return probability_cache;
  }
  /** No need to check for no_invariant, as caller does that. **/
  protected abstract double computeProbability();

  public boolean justified() {
    return (!no_invariant) && (getProbability() <= probability_limit);
  }

  /**
   * Subclasses should override.  An exact invariant indicates taht given
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
    // probability_cache_accurate = false;

    // We don't want to add the invariant yet, as this constructor is
    // called from the constructors for subclasses of Invariant.
    //     if (Global.debugInfer)
    //       System.out.println("Adding invariant " + this + " to Ppt " + ppt.name + " = " + ppt + "; now has " + ppt.invs.size() + " invariants in " + ppt.invs);
    //     ppt.addInvariant(this);
    //     if (Global.debugInfer)
    //       System.out.println("Added invariant " + this + " to Ppt " + ppt.name + " = " + ppt + "; now has " + ppt.invs.size() + " invariants in " + ppt.invs);
  }

  // Has to be public because of wrappers.
  public void destroy() {
    no_invariant = true;
    ppt.removeInvariant(this);
  }

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

  /**
   * For printing invariants, there are two interfaces:
   * repr gives a low-level representation, and
   * format gives a high-level representation for user output.
   **/
  public abstract String repr();
  /**
   * For printing invariants, there are two interfaces:
   * repr gives a low-level representation, and
   * format gives a high-level representation for user output.
   **/
  public abstract String format();


  // This should perhaps be merged with some kind of PptSlice comparator.
  /**
   * Compare based on arity, then variable index, then printed representation.
   *
   * Note: this comparator imposes orderings that are inconsistent with
   * equals.  That is, it may return 0 if the objects are not equal (but do
   * format identically).  That can happen if neither invariant is
   * justified; then both output as null.
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
      for (int i=0; i<vis1.length; i++) {
	int tmp = vis1[i].varinfo_index - vis2[i].varinfo_index;
	if (tmp != 0)
	  return tmp;
      }
      String format1 = inv1.format();
      String format2 = inv2.format();
      // Put nulls at the end of the list (or at the beginning; it doesn't
      // matter).
      if ((format1 == null) && (format2 == null))
	return 0;
      if ((format1 == null) && (format2 != null))
	return 1;
      if ((format1 != null) && (format2 == null))
	return -1;
      return format1.compareTo(format2);
    }
  }

  /**
   * @return true iff the two invariants represent the same
   * mathematical formula.  Does not consider the context such as
   * variable names, confidences, sample counts, value counts, or
   * related quantities.  As a rule of thumb, if two invariants format
   * the same, this method returns true.
   *
   * @exception RuntimeException if other.class != this.class
   **/
  public abstract boolean isSameFormula(Invariant other);


  public static interface IsSameInvariantNameExtractor
  {
    public String getFromFirst(VarInfo var1);
    public String getFromSecond(VarInfo var2);
  }

  public static class DefaultIsSameInvariantNameExtractor
    implements IsSameInvariantNameExtractor
  {
    public String getFromFirst(VarInfo var1)  { return var1.name; }
    public String getFromSecond(VarInfo var2) { return var2.name; }
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
      // of aliased variables
      Vector all_vars1 = var1.canonicalRep().equalTo();
      Vector all_vars2 = var2.canonicalRep().equalTo();
      all_vars1.add(var1.canonicalRep());
      all_vars2.add(var2.canonicalRep());
      Vector all_vars_names1 = new Vector(all_vars1.size());
      for (Iterator iter = all_vars1.iterator(); iter.hasNext(); ) {
	VarInfo elt = (VarInfo) iter.next();
	String name = name_extractor.getFromFirst(elt);
	all_vars_names1.add(name);
      }
      boolean intersection = false;
      for (Iterator iter = all_vars2.iterator(); iter.hasNext(); ) {
	VarInfo elt = (VarInfo) iter.next();
	String name = name_extractor.getFromSecond(elt);
	intersection = all_vars_names1.contains(name);
	if (intersection) {
	  break;
	}
      }
      if (!intersection) {
	return false;
      }
    }

    // the type, formula, and vars all matched
    return true;
  }


  /**
   * @return true iff the two invariants represent mutually exclusive
   * mathematical formulas.  Does not consider the context such as
   * variable names, confidences, sample counts, value counts, or
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



  String diff(Invariant other) {
    throw new Error("Unimplemented invariant diff for " + this.getClass() + " and " + other.getClass() + ": " + this.format() + " " + other.format());
  }

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

  public final boolean isWorthPrinting()
  {
    // It's hard to know in exactly what order to do these checks that
    // eliminate some invariants from consideration.  Which is cheapest?
    // Which is most often successful?  Which assume others have already
    // been performed?
    if (! isWorthPrinting_sansControlledCheck())
      return false;
    // The invariant is worth printing on its own merits, but it may be
    // controlled.  If any (transitive) controller is worth printing, don't
    // print this one.
    Invariant cont_inv = find_controlling_invariant();
    while (cont_inv != null) {
      if (cont_inv.isWorthPrinting_sansControlledCheck())
        return false;
      cont_inv = cont_inv.find_controlling_invariant();
    }
    // No controller was worth printing
    return true;
  }


  /**
   * Like isWorthPrinting, but doesn't check whether the invariant is controlled.
   **/
  final public boolean isWorthPrinting_sansControlledCheck() {
    return
      ((! hasFewModifiedSamples())
       && (! hasNonCanonicalVariable())
       && (! hasOnlyConstantVariables())
       && (! isObvious())
       && justified()
       && isWorthPrinting_PostconditionPrestate());
  }

  /**
   * @return true if this invariant has few modified (non-repeated) samples.
   * An exception is made for OneOf invariants.
   **/
  public final boolean hasFewModifiedSamples() {
    int num_mod_non_missing_samples = ppt.num_mod_non_missing_samples();

    if (this instanceof OneOf) {
      // A OneOf should have at least as many samples as it has values.
      Assert.assert(((OneOf) this).num_elts() <= num_mod_non_missing_samples);
      return false;
    } else {
      return (num_mod_non_missing_samples < Invariant.min_mod_non_missing_samples);
    }
  }

  /** @return true if this invariant involves a non-canonical variable **/
  public final boolean hasNonCanonicalVariable() {
    VarInfo[] vis = ppt.var_infos;
    for (int i=0; i<vis.length; i++) {
      if (! vis[i].isCanonical()) {
        return true;
      }
    }
    return false;
  }


  /**
   * @return true if this invariant involves only constant variables
   *         and is a comparison
   **/
  public boolean hasOnlyConstantVariables() {
    VarInfo[] varInfos = ppt.var_infos;
    for (int i=0; i < varInfos.length; i++) {
      if (! varInfos[i].isConstant())
	return false;
    }

    // At this point, we know all variables are constant.
    Assert.assert(this instanceof OneOf  ||  this instanceof Comparison
		  , "Unexpected invariant with all vars constant: "
		  + this + "  " + repr() + "  " + format()
		  );
    if (this instanceof Comparison) {
      //      Assert.assert(! IsEquality.it.accept(this));
      if (Global.debugPrintInvariants)
	System.out.println("  [over constants:  " + this.repr() + " ]");
      return true;
    }
    return false;
  }

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
      if (Global.debugPrintInvariants)
	System.out.println("  [obvious:  " + repr() + " ]");
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

  /**
   * @return true if this invariant is controlled by another invariant
   **/
  public boolean isControlled() {
    Invariant controller = this.find_controlling_invariant();
    return (controller != null);
  }

  /**
   * @return true if this invariant is a postcondition that is implied by prestate
   * invariants.  For example, if an entry point has the invariant orig(x)+3=orig(y), and
   * this invariant is the corresponding exit point invariant x+3=y, then this methods
   * returns true.
   **/
  public boolean isImpliedPostcondition() {
    PptTopLevel topLevel = (PptTopLevel) ppt.parent;
    if (topLevel.entry_ppt() != null) { // if this is an exit point invariant
      Iterator entryInvariants = topLevel.entry_ppt().invariants_vector().iterator();
      while (entryInvariants.hasNext()) {
	Invariant entryInvariant = (Invariant) entryInvariants.next();
	// If entryInvariant with orig() applied to everything matches this invariant
	if (entryInvariant.isSameInvariant( this, preToPostIsSameInvariantNameExtractor))
	  return true;
      }
    }
    return false;
  }

  /**
   * Used in isImpliedPostcondition().
   **/
  private final static IsSameInvariantNameExtractor preToPostIsSameInvariantNameExtractor =
    new DefaultIsSameInvariantNameExtractor() {
	public String getFromFirst(VarInfo var)
	{ return "orig(" + super.getFromFirst(var) + ")"; }
      };


  // Not used as of 1/31/2000.
  // /**
  //  * Returns true if this invariant implies the argument invariant.
  //  * Intended to be overridden by subclasses.
  //  */
  // public boolean implies(Invariant inv) {
  //   return false;
  // }




  private boolean isWorthPrinting_PostconditionPrestate()
  {
    PptTopLevel pptt = (PptTopLevel) ppt.parent;

    if (Daikon.suppress_implied_postcondition_over_prestate_invariants) {
      if (pptt.entry_ppt != null) {
	Iterator entry_invs = pptt.entry_ppt.invariants_iterator();
	while (entry_invs.hasNext()) {
	  Invariant entry_inv = (Invariant) entry_invs.next();
	  // If entry_inv with orig() applied to everything matches this
	  if (entry_inv.isSameInvariant(this, preToPostIsSameInvariantNameExtractor)) {
	    if (pptt.entry_ppt.isWorthPrinting(entry_inv)) {
	      return false;
	    }
	  }
	}
      }
    }
    return true;
  }



  public Invariant find_controlling_invariant()
  {
    PptTopLevel pptt = (PptTopLevel) ppt.parent;

    // Try to match inv against all controlling invariants
    Iterator controllers = pptt.controlling_ppts.iterator();
    while (controllers.hasNext()) {
      PptTopLevel controller = (PptTopLevel) controllers.next();
      // System.out.println("Looking for controller of " + inv.format() + " in " + controller.name);
      Iterator candidates = controller.invariants_iterator();
      while (candidates.hasNext()) {
	Invariant cand_inv = (Invariant) candidates.next();
	if (cand_inv.isSameInvariant(cand_inv)) {
          // System.out.println("Controller found: " + cand_inv.format() + "  [worth printing: " + ((PptTopLevel)cand_inv.ppt.parent).isWorthPrinting(cand_inv) + "]");
	  return cand_inv;
	}
      }
    }

    return null;
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
