package daikon;

import daikon.derive.*;
import daikon.derive.unary.*;
import daikon.derive.binary.*;
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

  Ppt ppt;

  // Use the simpler canBeMissing instead
  // PptSliceGeneric ppt_unary;    // For unary invariants over this variable
  boolean canBeMissing = false;

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

  private VarInfo equal_to;      // the canonical representative to which
                                // this variable is equal; points to itself
                                // if it is canonical.

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
    // Don't set equal_to yet; leave it null until it's set.
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

  public int derivedDepth() {
    if (derived == null)
      return 0;
    else
      return derived.derivedDepth();
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
    Object raw = getValue(vt);
    if (raw == null)
      // Perhaps use some distinguished value here, so
      // that I have some indication when things are going wrong.
      // return 0;
      return 222222;
    return ((Integer)raw).intValue();
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
    if (equal_to == null)
      computeCanonical();
    return (equal_to == this);
  }

  // Canonical representative that's equal to this variable.
  public VarInfo canonicalRep() {
    if (equal_to == null)
      computeCanonical();
    return equal_to;
  }

  public Vector equalTo() {
    // should only call this for canonical variables
    Assert.assert(isCanonical());

    Vector result = new Vector();

    VarInfo[] vis = ppt.var_infos;
    for (int i=0; i<vis.length; i++) {
      if (i == varinfo_index)
        continue;
      if (vis[i].equal_to == this)
        result.add(vis[i]);
    }

    return result;

  }

  // A bit inefficient (it would be better to compute them all at once),
  // but so be it.  I could change this later.
  public void computeCanonical() {
    Assert.assert(equal_to == null);

    // System.out.println("computeCanonical(" + name + ")");

    Vector noncanonical_equal = new Vector(3);

    for (Iterator equal_invs = new UtilMDE.FilteredIterator(invariants(), IsEquality.it) ; equal_invs.hasNext() ; ) {
      Comparison inv = (Comparison) equal_invs.next();
      VarInfo vi1 = inv.var1();
      VarInfo vi2 = inv.var2();
      Assert.assert((vi1 == this) || (vi2 == this));
      VarInfo other = (vi1 == this) ? vi2 : vi1;
      if (other.equal_to == null) {
        noncanonical_equal.add(other);
      } else {
        VarInfo rep = other.equal_to;
        this.equal_to = rep;
        for (int i=0; i<noncanonical_equal.size(); i++)
          ((VarInfo)noncanonical_equal.elementAt(i)).equal_to = rep;
        return;
      }
    }
    if (noncanonical_equal.size() == 0) {
      // System.out.println("Found no other");
      this.equal_to = this;
      return;
    }
    int min_index = this.varinfo_index;
    for (int i=0; i<noncanonical_equal.size(); i++) {
      VarInfo other = (VarInfo)noncanonical_equal.elementAt(i);
      min_index = Math.min(min_index, other.varinfo_index);
    }
    VarInfo rep = ppt.var_infos[min_index];
    this.equal_to = rep;
    for (int i=0; i<noncanonical_equal.size(); i++) {
      VarInfo other = (VarInfo)noncanonical_equal.elementAt(i);
      other.equal_to = rep;
    }
  }


  // Shouldn't have any vacuous variables, so comment this out.
  // /**
  //  * Returns non-nil if this variable is derived from non-canonical variables.
  //  * (We didn't konw at the time we derived the variables that the derivees
  //  * were non-canonical.)
  //  * There might be other reasons for a variable never to appear, also;
  //  * I will add them as I discover them.
  //  */
  // public boolean isVacuous() {
  //   if (! isDerived())
  //     return false;
  //   // This shouldn't be happening, so comment it out to see the result.
  //   // if (derived.isDerivedFromNonCanonical())
  //   //   return true;
  //   return false;
  // }

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

  // Returns the VarInfo for the sequence from which this was derived,
  // or null if this wasn't derived from a sequence.
  public VarInfo isObviousSequenceMember() {
    if (derived == null)
      return null;

    if (derived instanceof SequenceScalarSubscript) {
      SequenceScalarSubscript sss = (SequenceScalarSubscript) derived;
      return sss.seqvar();
    } else if (derived instanceof SequenceExtremum) {
      SequenceExtremum se = (SequenceExtremum) derived;
      return se.seqvar();
    } else if (derived instanceof SequenceMax) {
      SequenceMax sm = (SequenceMax) derived;
      return sm.var_info;
    } else if (derived instanceof SequenceMin) {
      SequenceMin sm = (SequenceMin) derived;
      return sm.var_info;
    } else {
      return null;
    }
  }

  public VarInfo isObviousSubSequenceOf() {
    // System.out.println("isObviousSubSequenceOf(" + name + "); derived=" + derived);

    if (derived == null)
      return null;

    if (derived instanceof SequenceScalarSubsequence) {
      SequenceScalarSubsequence sss = (SequenceScalarSubsequence) derived;
      // System.out.println("isObviousSubSequenceOf returning " + sss.seqvar().name);
      return sss.seqvar();
    } else {
      return null;
    }
  }

  public VarInfo sequenceSize() {
    Assert.assert(rep_type.isArray());
    // we know the size follows the variable itself in the list
    VarInfo[] vis = ppt.var_infos;
    for (int i=varinfo_index+1; i<vis.length; i++) {
      VarInfo vi = vis[i];
      if ((vi.derived instanceof SequenceLength)
          && (((SequenceLength) vi.derived).var_info == this))
        return vi;
    }
    throw new Error("Couldn't find size");
  }

}
