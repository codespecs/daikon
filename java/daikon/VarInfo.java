package daikon;

import daikon.derive.*;
import daikon.derive.unary.*;
import daikon.derive.binary.*;
import daikon.inv.*;
import utilMDE.*;

import java.util.*;

public class VarInfo implements Cloneable {

  // Name and type
  public String name;		// interned
  public ProglangType type;	// as declared in the program
  public ProglangType rep_type;	// as written to the data trace file
  public VarComparability comparability; // in Python, this could also be a VarInfo
				//   (meaning treat this variable like that one)

  // Obtaining values
  public int value_index;	// index in lists of values, VarTuple objects
  // Can I eliminate this slot?
  public int varinfo_index;	// index in lists of VarInfo objects
  Object static_constant_value;	// null if not statically constant
				// if statically constant, then index == -1

  // Derived variables
  public Derivation derived;	// whether (and how) derived
  public Vector derivees;	// vector of Derivation objects

  public Ppt ppt;

  // Use the simpler canBeMissing instead
  // PptSlice ppt_unary;    // For unary invariants over this variable
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

  // Only public so that PptTopLevel can access it.
  // Clients should use isCanonical() or canonicalRep() or equalTo().
  public VarInfo equal_to;      // the canonical representative to which
                                // this variable is equal; points to itself
                                // if it is canonical.
  public Object dynamic_constant;

  // Does not include equal_to, which is dealt with elsewhere.
  // An invariant is only listed on the first VarInfo, not all VarInfos.
  public Vector exact_nonunary_invariants;

  public Vector views;          // All views containing this object.
                                // This is needed because findSlice can be
                                //   so slow.

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

  public VarInfo(String name_, ProglangType type_, ProglangType rep_type_, VarComparability comparability_, Object static_constant_value_) {
    // Watch out:  some Lisp and C .decls files have other (unsupported) types.
    Assert.assert(rep_type_ != null);
    Assert.assert((rep_type_ == ProglangType.INT)
                  || (rep_type_ == ProglangType.STRING)
                  || (rep_type_ == ProglangType.INT_ARRAY)
                  || (rep_type_ == ProglangType.STRING_ARRAY),
                  "Unsupported representation type " + rep_type_.format() + " for variable " + name_);

    // Possibly the call to intern() isn't necessary; but it's safest to
    // make the call to intern() rather than running the risk that a caller
    // didn't.
    name = name_.intern();
    type = type_;
    rep_type = rep_type_;
    comparability = comparability_;
    static_constant_value = static_constant_value_;

    // Indicates that these haven't yet been set to reasonable values.
    value_index = -1;
    varinfo_index = -1;

    derivees = new Vector(3);
    // Don't set equal_to yet; leave it null until it's set.
    // equal_to = new Vector(3);

    exact_nonunary_invariants = new Vector(2);
  }

  public VarInfo(String name_, ProglangType type_, ProglangType rep_type_, VarComparability comparability_) {
    this(name_, type_, rep_type_, comparability_, null);
  }

  public VarInfo(VarInfo vi) {
    this(vi.name, vi.type, vi.rep_type, vi.comparability, vi.static_constant_value);
  }

  // I *think* I don't need to implement VarInfo.clone(), as the java.lang.Object
  // version is sufficient.
  // protected Object clone() { ... }

  /// I'm not currently using this because doing this would prevent
  /// any new variable derivation, and I do want such derivation to occur.
  /// Furthermore, I don't want to try to deal with figuring out what
  /// occurred and only doing what's new, etc.
  // Given an array of VarInfo objects, return an array of clones, where
  // references to the originals have been modified into references to the
  // new ones (so that the new set is self-consistent).  The originals
  // should not be modified by this operation.
  public static VarInfo[] arrayclone_clever(VarInfo[] a_old) {
    VarInfo[] a_new = new VarInfo[a_old.length];
    for (int i=0; i<a_new.length; i++) {
      try {
        a_new[i] = (VarInfo) a_old[i].clone();
      } catch (CloneNotSupportedException e) {
        e.printStackTrace();
        throw new Error(e.toString());
      }
      a_new[i].canBeMissing = false;
      // I must set this; even though the specified variables will still
      // be equal, they may have a different canonical representative.
      a_new[i].equal_to = null;
      // this doesn't really need to be set; if non-null, it's guaranteed
      // to be the same in the child
      // a_new[i].dynamic_constant = null;
      a_new[i].ppt = null;
    }
    // I need to fix both of these slots:
    //   public Derivation derived;	// whether (and how) derived
    //   public Vector derivees;	// vector of Derivation objects
    HashMap deriv_map = new HashMap();
    for (int i=0; i<a_new.length; i++) {
      Derivation deriv_old = a_old[i].derived;
      Derivation deriv_new = deriv_old.switchVars(a_old, a_new);
      deriv_map.put(deriv_old, deriv_new);
      a_new[i].derived = deriv_new;
    }
    for (int i=0; i<a_new.length; i++) {
      Vector derivees_old = a_old[i].derivees;
      Vector derivees_new = new Vector(derivees_old.size());
      for (int j=0; j<derivees_old.size(); j++) {
        Derivation deriv_old = (Derivation) derivees_old.elementAt(j);
        Derivation deriv_new = (Derivation) deriv_map.get(deriv_old);
        Assert.assert(deriv_new != null);
        derivees_new.add(deriv_new);
      }
    }
    return a_new;
  }

  // Given an array of VarInfo objects, return an array of clones, where
  // references to the originals have been modified into references to the
  // new ones (so that the new set is self-consistent).  The originals
  // should not be modified by this operation.
  public static VarInfo[] arrayclone_simple(VarInfo[] a_old) {
    int len = a_old.length;
    VarInfo[] a_new = new VarInfo[len];
    for (int i=0; i<len; i++)
      a_new[i] = new VarInfo(a_old[i]);
    return a_new;
  }


  boolean repOK() {
    // If statically constant, then value_index is not meaningful
    if ((static_constant_value != null) != (value_index == -1))
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
      + ",static_constant_value=" + static_constant_value
      + ",derived=" + derived
      + ",derivees=" + derivees
      + ",ppt=" + ppt
      + ",equal_to=" + equal_to;
  }

  public boolean isConstant() {
    return (isStaticConstant() || isDynamicConstant());
  }
  public boolean isDynamicConstant() {
    return (dynamic_constant != null);
  }
  public boolean isStaticConstant() {
    return (static_constant_value != null);
  }
  public Object constantValue() {
    if (isStaticConstant()) {
      return static_constant_value;
    } else if (isDynamicConstant()) {
      return dynamic_constant;
    } else {
      throw new Error("Variable " + name + " is not constant");
    }
  }

  public boolean hasExactInvariant(VarInfo other) {
    for (int i=0; i<exact_nonunary_invariants.size(); i++) {
      Invariant inv = (Invariant) exact_nonunary_invariants.elementAt(i);
      Assert.assert(inv.ppt.var_infos[0] == this);
      Assert.assert(inv.isExact());
      if ((inv.ppt.arity == 2) && (inv.ppt.var_infos[1] == other)) {
        return true;
      }
    }
    return false;
  }

  public boolean hasExactInvariant(VarInfo other1, VarInfo other2) {
    for (int i=0; i<exact_nonunary_invariants.size(); i++) {
      Invariant inv = (Invariant) exact_nonunary_invariants.elementAt(i);
      Assert.assert(inv.ppt.var_infos[0] == this);
      Assert.assert(inv.isExact());
      if ((inv.ppt.arity == 3)
          && (inv.ppt.var_infos[1] == other1)
          && (inv.ppt.var_infos[2] == other2)) {
        return true;
      }
    }
    return false;
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
    if (static_constant_value != null)
      return ValueTuple.UNMODIFIED;
    else
      return vt.getModified(value_index);
  }
  public boolean isUnmodified(ValueTuple vt) { return ValueTuple.modIsUnmodified(getModified(vt)); }
  public boolean isModified(ValueTuple vt) { return ValueTuple.modIsModified(getModified(vt)); }
  public boolean isMissing(ValueTuple vt) { return ValueTuple.modIsMissing(getModified(vt)); }

  public Object getValue(ValueTuple vt) {
    if (static_constant_value != null)
      return static_constant_value;
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

  public String getStringValue(ValueTuple vt) {
    return (String) getValue(vt);
  }

  public int[] getIntArrayValue(ValueTuple vt) {
    Object raw = getValue(vt);
    if (raw == null)
      // Perhaps use some distinguished value here, so
      // that I have some indication when things are going wrong.
      // return 0;
      return new int[] { };
    return (int[])raw;
  }

  static class usesVarFilter implements Filter {
    VarInfo var;
    public usesVarFilter(VarInfo var_) { var = var_; }
    public boolean accept(Object o) { return ((Invariant) o).usesVar(var); }
  }

//   Iterator invariants() {
//     // This assertion will need to be relaxed eventually.
//     Assert.assert(ppt instanceof PptTopLevel,
//                   "Ppt " + ppt + " is not instanceof PptTopLevel");
//     // Could alternately have used ppt.invs.lookup(vi).
//     // In fact, that's better, because it doesn't look at so many variables.
//     Iterator all_invs = ((PptTopLevel) ppt).invariants();
//     return new UtilMDE.FilteredIterator(all_invs, new usesVarFilter(this));
//   }

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
    return (equal_to == this);
  }

  // Canonical representative that's equal to this variable.
  public VarInfo canonicalRep() {
    Assert.assert(equal_to != null);
    return equal_to;
  }

  public Vector equalTo() {
    // should only call this for canonical variables
    Assert.assert(isCanonical());

    Vector result = new Vector();

    VarInfo[] vis = ppt.var_infos;
    for (int i=0; i<vis.length; i++) {
      Assert.assert(vis[i].equal_to == vis[i].equal_to.equal_to);
      if (i == varinfo_index)
        continue;
      if (vis[i].equal_to == this)
        result.add(vis[i]);
    }

    return result;
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
  public VarInfo isDerivedSequenceMember() {
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

  public VarInfo isDerivedSubSequenceOf() {
    // System.out.println("isDerivedSubSequenceOf(" + name + "); derived=" + derived);

    if (derived == null)
      return null;

    if (derived instanceof SequenceScalarSubsequence) {
      SequenceScalarSubsequence sss = (SequenceScalarSubsequence) derived;
      // System.out.println("isDerivedSubSequenceOf returning " + sss.seqvar().name);
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
    throw new Error("Couldn't find size of " + name);
  }

  // Returns true if the type in the original program is integer
  public boolean isIndex() {
    return ((rep_type == ProglangType.INT)
            && type.isIndex());
  }

}
