package daikon;

import daikon.derive.*;
import daikon.inv.*;
import utilMDE.*;

import java.util.*;

public class VarInfo {

  // Name and type
  public String name;		// interned
  public ProglangType type;	// as declared in the program
  public ProglangType rep_type;	// as written to the data trace file
  // Turn this back into "VarComparability" if we ever need to support multiple types.  Or something.
  public ExplicitVarComparability comparability; // in Python, this could also be a VarInfo
				//   (meaning treat this variable like that one)

  // Obtaining values
  public int value_index;	// index in lists of values, VarTuple objects
  // Can I eliminate this slot?
  public int varinfo_index;	// index in lists of VarInfo objects
  Object constant_value;	// null if not statically constant
				// if statically constant, then index == -1

  // Derived variables
  public Derivation derived;	// whether (and how) derived
  public Vector derivees;	// vector of Derivation objects

  // This needs to get set somehow
  Ppt ppt;

  // To find the invariant over a pair of variables, do a double-dispatch:
  // first look up the "invariants" field of one of the variables, then
  // look up the other variable in that map.
  // The entry for a variable can sometimes be
  // missing, which I think is equivalent to it being
  // a trivial invariant satisfying no properties; but
  // I'm not sure exactly when each is the case.
  // [use same data structure as before (ie, list of indices)?]
  // [merge "invariant" and "invariants" data structures?  Or put in
  // some more global data structure?  Main goal:  given
  // a set of variables, return a list of all their micro-invariants,
  // possibly partially ordered according to implication.]

  // Perhaps it would be better (more efficient) to just point at the
  // representative object.  This does make printing a touch easier.
  // Public temporarily, for debugging (and to let
  // PptTopLevel.computeCanonical set them).
  public Vector equal_to;		// list of other variables equal to this one
				// Do I want this?
  public boolean canonical;    // private so clients will use isCanonical()

  // list of indices of equal variables;
  //   could be derived from invariants
  //   by checking for inv.comparison == "=".
  //   The variable itself is not on this list.


  // Set common values
  private VarInfo() {
    // // info about derived variables: derivees from this and derivers of this
    // self.derived = {}               // will be variables derived from this
    // 				//   one; presently not used.
    // self.derived_len = None         // index of len variable
    // self.is_derived = is_derived    // boolean (for now)

    // // To find the invariant over a pair of variables, do a double-dispatch:
    // // first look up the "invariants" field of one of the variables, then
    // // look up the other variable in that map.
    // self.invariant = None
    // self.invariants = {}	// map from indices to multiple-arity invariants
    // self.equal_to = []        // list of indices of equal variables;
    // 				//   could be derived from invariants
    // 				//   by checking for inv.comparison == "="
    // 				//   the variable itself is not on this list
  }

  public VarInfo(String name_, ProglangType type_, ProglangType rep_type_, ExplicitVarComparability comparability_, Object constant_value_) {
    // Possibly the call to intern() isn't necessary; but it's safest to
    // make the call to intern() rather than running the risk that a caller
    // didn't.
    name = name_.intern();
    type = type_;
    rep_type = rep_type_;
    comparability = comparability_;
    constant_value = constant_value_;

    // Indicates that these haven't yet been set to reasonable values.
    value_index = -1;
    varinfo_index = -1;

    derivees = new Vector(3);
    // Don't set equal_to or canonical yet; leave it null until it's set.
    // equal_to = new Vector(3);
  }

  public VarInfo(String name_, ProglangType type_, ProglangType rep_type_, ExplicitVarComparability comparability_) {
    this(name_, type_, rep_type_, comparability_, null);
  }

  public VarInfo(VarInfo vi) {
    this(vi.name, vi.type, vi.rep_type, vi.comparability, vi.constant_value);
  }


  boolean repOK() {
    // If statically constant, then value_index is not meaningful
    if ((constant_value != null) != (value_index == -1))
      return false;

    return true;
  }


  String repr() {
    return "<VarInfo " + name + ": "
      + "type=" + type
      + ",rep_type=" + rep_type
      + ",comparability=" + comparability
      + ",value_index=" + value_index
      + ",varinfo_index=" + varinfo_index
      + ",constant_value=" + constant_value
      + ",derived=" + derived
      + ",derivees=" + derivees
      + ",ppt=" + ppt
      + ",equal_to=" + equal_to;
  }

  public boolean isConstant() {
    return (constant_value != null);
  }
  public boolean isDerived() {
    return (derived != null);
  }

  public int getModified(ValueTuple vt) {
    if (constant_value != null)
      return ValueTuple.UNMODIFIED;
    else
      return vt.getModified(value_index);
  }
  public boolean isUnmodified(ValueTuple vt) { return ValueTuple.modIsUnmodified(getModified(vt)); }
  public boolean isModified(ValueTuple vt) { return ValueTuple.modIsModified(getModified(vt)); }
  public boolean isMissing(ValueTuple vt) { return ValueTuple.modIsMissing(getModified(vt)); }

  public Object getValue(ValueTuple vt) {
    if (constant_value != null)
      return constant_value;
    else
      return vt.getValue(value_index);
  }
  public int getIntValue(ValueTuple vt) {
    return ((Integer)getValue(vt)).intValue();
  }

  static class usesVarFilter implements Filter {
    VarInfo var;
    public usesVarFilter(VarInfo var_) { var = var_; }
    public boolean accept(Object o) { return ((Invariant) o).usesVar(var); }
  }

  Iterator invariants() {
    // This assertion will need to be relaxed eventually.
    Assert.assert(ppt instanceof PptTopLevel,
                  "Ppt " + ppt + " is not instanceof PptTopLevel");
    // Could alternately have used ppt.invs.lookup(vi).
    // In fact, that's better, because it doesn't look at so many variables.
    Iterator all_invs = ((PptTopLevel) ppt).invariants();
    return new UtilMDE.FilteredIterator(all_invs, new usesVarFilter(this));
  }

  // def canonical_var(self):
  //     """Return index of the canonical variable that is always equal to this one.
  //     Return None if no such variable exists."""

  //     assert util.sorted(self.equal_to)
  //     if self.equal_to == []:
  // 	return self.index
  //     else:
  // 	return min(self.index, self.equal_to[0])

  // This sets the equal_to slot and caches its result.  That means that if
  // any more values come in, then the equal_to slot should probably be
  // reset to null so that whether the variable is canonical is recomputed.
  public boolean isCanonical() {
    Assert.assert(equal_to != null);
    return canonical;

    /// The above is adequate, if PptTopLevel.computeCanonical() has been called.
    // // This is the wrong approach (too inefficient).  If equal_to isn't
    // // set, then we should compute the equal_to for all variables at the
    // // PptTopLevel, simultaneously.
    // equal_to = new Vector();
    // int min_index = varinfo_index;
    // for (Iterator equal_invs = new UtilMDE.FilteredIterator(invariants(), IsEquality.it) ; equal_invs.hasNext() ; ) {
    //   Comparison inv = (Comparison) equal_invs.next();
    //   Assert.assert(inv.ppt.var_infos.length == 2);
    //   // we know that var_infos[0] has a lower index than var_infos[1]
    //   Assert.assert(inv.var1().varinfo_index
    //                 < inv.var2().varinfo_index);
    //   min_index = Math.min(min_index, inv.var1().varinfo_index);
    // }
    // equal_to.trimToSize();
    // canonical = (min_index == varinfo_index);
    // return canonical;
  }

  /**
   * Returns non-nil if this variable is derived from non-canonical variables.
   * There might be other reasons for a variable never to appear, also;
   * I will add them as I discover them.
   */
  public boolean isVacuous() {
    if (! isDerived())
      return false;
    if (derived.isDerivedFromNonCanonical())
      return true;
    return false;
  }

  static boolean compatible(VarInfo[] vis1, VarInfo[] vis2) {
    // This just checks that the names are the same.
    if (vis1.length != vis2.length)
      return false;

    for (int i=0; i<vis1.length; i++)
      if (!vis1[i].compatible(vis2[i]))
	return false;

    return true;
  }

  // simplistic implementation, just checks that the names are the same
  boolean compatible(VarInfo other) {
    if (name != other.name)
      return false;
    return true;
  }

}
