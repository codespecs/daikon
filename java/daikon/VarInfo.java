package daikon;

import daikon.derive.*;
import java.util.*;

public class VarInfo {

  // Name and type
  public String name;		// interned
  public ProglangType rep_type;	// as written to the data trace file
  public ProglangType type;	// as declared in the program
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

  // Perhaps it would be better to just point at the representative object
  Vector equal_to;		// list of other variables equal to this one
				// Do I want this?
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


  // def is_sequence(self):
  //     return self.type.is_array()

  // def canonical_var(self):
  //     """Return index of the canonical variable that is always equal to this one.
  //     Return None if no such variable exists."""

  //     assert util.sorted(self.equal_to)
  //     if self.equal_to == []:
  // 	return self.index
  //     else:
  // 	return min(self.index, self.equal_to[0])

  boolean canonical() {
    // assert self.index != None
    // assert self.equal_to == [] or self.canonical_var() != None
    // return self.index == self.canonical_var()
    throw new Error("to be implemented");
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
