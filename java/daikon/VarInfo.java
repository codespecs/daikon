package daikon;

import daikon.derive.*;
import daikon.derive.unary.*;
import daikon.derive.binary.*;
import daikon.inv.*;
import daikon.inv.binary.twoScalar.*;
import utilMDE.*;

import java.util.*;

public final class VarInfo implements Cloneable, java.io.Serializable {

  // Name and type
  public String name;		// interned
  public ProglangType type;	// as declared in the program
  public ProglangType rep_type;	// as written to the data trace file
  public VarComparability comparability;

  // Obtaining values
  public int varinfo_index;	// index in lists of VarInfo objects
  public int value_index;	// index in lists of values, VarTuple objects
  public boolean is_static_constant;  // required if static_constant_value==null
				//   (is_static_constant
                                //   iff (value_index == -1)
  Object static_constant_value;	// null if not statically constant
                                // once upon a time (still?):
  				//   (static_constant_value != null)
                                //   iff (value_index == -1)


  // Derived variables
  public Derivation derived;	// whether (and how) derived
  public Vector derivees;	// vector of Derivation objects

  public PptTopLevel ppt;

  boolean canBeMissing = false;
  public boolean canBeNull = false;    // relevant only for arrays, really

  // It can be expensive to find an arbitrary invariant.  These fields
  // cache invariants that we want to be able to look up quickly.

  // Only public so that PptTopLevel can access it.
  // Clients should use isCanonical() or canonicalRep() or equalTo().
  public VarInfo equal_to;      // the canonical representative to which
                                // this variable is equal; may be itself.
  public boolean is_dynamic_constant;  // required if dynamic_constant==null
  public Object dynamic_constant;
  VarInfo sequenceSize;         // if null, not yet computed (or this VarInfo
                                //   is not a sequence)

  // Does not include equal_to, which is dealt with elsewhere.
  // An invariant is only listed on the first VarInfo, not all VarInfos.
  public Vector exact_nonunary_invariants;

  public Vector views;          // All views containing this object.
                                // This is needed because findSlice can be
                                //   so slow.


  static boolean legalRepType(ProglangType rep_type) {
    return ((rep_type == ProglangType.INT)
            || (rep_type == ProglangType.DOUBLE)
            || (rep_type == ProglangType.STRING)
            || (rep_type == ProglangType.INT_ARRAY)
            || (rep_type == ProglangType.DOUBLE_ARRAY)
            || (rep_type == ProglangType.STRING_ARRAY));
  }

  public VarInfo(String name, ProglangType type, ProglangType rep_type, VarComparability comparability, boolean is_static_constant, Object static_constant_value) {
    // Watch out:  some Lisp and C .decls files have other (unsupported) types.
    Assert.assert(rep_type != null);
    Assert.assert(legalRepType(rep_type),
                  "Unsupported representation type " + rep_type.format() + " for variable " + name);

    // Possibly the call to intern() isn't necessary; but it's safest to
    // make the call to intern() rather than running the risk that a caller
    // didn't.
    this.name = name.intern();
    this.type = type;
    this.rep_type = rep_type;
    this.comparability = comparability;
    this.is_static_constant = is_static_constant;
    this.static_constant_value = static_constant_value;

    // Indicates that these haven't yet been set to reasonable values.
    value_index = -1;
    varinfo_index = -1;

    derivees = new Vector(3);

    exact_nonunary_invariants = new Vector(2);
  }

  public VarInfo(String name, ProglangType type, ProglangType rep_type, VarComparability comparability) {
    this(name, type, rep_type, comparability, false, null);
  }

  public VarInfo(VarInfo vi) {
    this(vi.name, vi.type, vi.rep_type, vi.comparability, vi.is_static_constant, vi.static_constant_value);
  }

  // I *think* I don't need to implement VarInfo.clone(), as the java.lang.Object
  // version is sufficient.
  // protected Object clone() { ... }

  // I'm not currently using this because doing this would prevent any new
  // variable derivation [why?], and I do want such derivation to occur.
  // Furthermore, I don't want to try to deal with figuring out what
  // occurred and only doing what's new, etc.
  /**
   * Given an array of VarInfo objects, return an array of clones, where
   * references to the originals have been modified into references to the
   * new ones (so that the new set is self-consistent).  The originals
   * should not be modified by this operation.
   **/
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

  /**
   * Given an array of VarInfo objects, return an array of clones, where
   * references to the originals have been modified into references to the
   * new ones (so that the new set is self-consistent).  The originals
   * should not be modified by this operation.
   **/
  public static VarInfo[] arrayclone_simple(VarInfo[] a_old) {
    int len = a_old.length;
    VarInfo[] a_new = new VarInfo[len];
    for (int i=0; i<len; i++) {
      a_new[i] = new VarInfo(a_old[i]);
      a_new[i].varinfo_index = a_old[i].varinfo_index;
      a_new[i].value_index = a_old[i].value_index;
    }
    return a_new;
  }


  boolean repOK() {
    // If statically constant, then value_index is not meaningful
    if (is_static_constant != (value_index == -1))
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
      + ",is_static_constant=" + is_static_constant
      + ",static_constant_value=" + static_constant_value
      + ",derived=" + derived
      + ",derivees=" + derivees
      + ",ppt=" + ppt
      + ",equal_to=" + equal_to
      + ">";
  }

  public boolean isConstant() {
    return (isStaticConstant() || isDynamicConstant());
  }
  public boolean isDynamicConstant() {
    // return (dynamic_constant != null);
    return is_dynamic_constant;
  }
  public boolean isStaticConstant() {
    // return (static_constant_value != null);
    return is_static_constant;
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
    Assert.assert(this.varinfo_index < other.varinfo_index);
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
    Assert.assert(this.varinfo_index < other1.varinfo_index);
    Assert.assert(other1.varinfo_index < other2.varinfo_index);
    Assert.assert(this.ppt == other1.ppt);
    Assert.assert(this.ppt == other2.ppt);
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
  public boolean isClosure() {
    // This should eventually turn into
    //   return name.indexOf("closure(") != -1;
    // when I rename those variables to "closure(...)".
    return name.indexOf("~") != -1;
  }


  public int getModified(ValueTuple vt) {
    if (is_static_constant)
      return ValueTuple.STATIC_CONSTANT;
    else
      return vt.getModified(value_index);
  }
  public boolean isUnmodified(ValueTuple vt) { return ValueTuple.modIsUnmodified(getModified(vt)); }
  public boolean isModified(ValueTuple vt) { return ValueTuple.modIsModified(getModified(vt)); }
  public boolean isMissing(ValueTuple vt) { return ValueTuple.modIsMissing(getModified(vt)); }

  public Object getValue(ValueTuple vt) {
    if (is_static_constant)
      return static_constant_value;
    else
      return vt.getValue(value_index);
  }

  public long getIntValue(ValueTuple vt) {
    Object raw = getValue(vt);
    if (raw == null) {
      // Use a distinguished value to indicate when things are going wrong.
      return 222222;
    }
    return ((Long)raw).longValue();
  }

  public int getIndexValue(ValueTuple vt) {
    Object raw = getValue(vt);
    if (raw == null) {
      // Use a distinguished value to indicate when things are going wrong.
      return 222222;
    }
    return ((Long)raw).intValue();
  }

  public String getStringValue(ValueTuple vt) {
    return (String) getValue(vt);
  }

  public long[] getIntArrayValue(ValueTuple vt) {
    Object raw = getValue(vt);
    if (raw == null) {
      // I am temporarily experimenting with throwing an error instead.
      // // Perhaps use some distinguished value here, so
      // // that I have some indication when things are going wrong.
      // // return 0;
      // return new int[0];
      throw new Error(this + "getIntArrayValue(" + vt + ")");
    }
    return (long[])raw;
  }

  static final class usesVarFilter implements Filter {
    VarInfo var;
    public usesVarFilter(VarInfo var) { this.var = var; }
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

  public boolean isCanonical() {
    Assert.assert(equal_to != null);
    return (equal_to == this);
  }

  // Canonical representative that's equal to this variable.
  public VarInfo canonicalRep() {
    Assert.assert(equal_to != null);
    return equal_to;
  }

  /**
   * Returns all other variables that are equal to this variable.
   * The result Vector does not include this.
   * Also see @link{equalToNonobvious}.
   **/
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

  // Like equalTo, but drops out things which can be inferred to be equal
  // to the first.

  public Vector equalToNonobvious() {
    // should only call this for canonical variables
    Assert.assert(isCanonical());

    Vector result = new Vector();

    VarInfo[] vis = ppt.var_infos;
    for (int i=0; i<vis.length; i++) {
      Assert.assert(vis[i].equal_to == vis[i].equal_to.equal_to);
      if (i == varinfo_index)
        continue;
      VarInfo vi = vis[i];
      if (vi.equal_to != this)
        continue;

      // System.out.println("Considering " + vi.name);
      // Special cases of variables to omit.
      {
        // An element b.class is omitted if:
        //  * "b.class" is non-canonical
        //    We know this because we are only examining non-canonical
        //    variables which are equal to this, which is canonical.
        //  * "b" is non-canonical
        //  * there exists an a such that a=b (ie, equal_to slot of "b"'s
        //     varinfo is non-null); also, assert that "a.class" is in equalTo

        String sansclassname = null;
        if (vi.name.endsWith(".class")) {
          sansclassname = vi.name.substring(0, vi.name.length() - 6);
        } else if (vi.name.endsWith(".class)") && vi.name.startsWith("orig(")) {
          // parent of "orig(x.class)" is "orig(x)"
          sansclassname = vi.name.substring(0, vi.name.length() - 7) + ")";
        }
        if (sansclassname != null) {
          // System.out.println("Considering .class: " + vi.name + "sansclass=" + sansclassname);

          // "parent" is "b" in the above comment; "vi" is "b.class".
          // don't bother to intern, as findVar doesn't need it.
          VarInfo sansclass = ppt.findVar(sansclassname);
          Assert.assert(sansclass != null);
          if (! sansclass.isCanonical()) {
            // We will omit vi.
            VarInfo a = sansclass.equal_to;
            VarInfo a_class = ppt.findVar(a.name + ".class");
            Assert.assert(a_class != null);
            Assert.assert(a_class.equal_to == this);
            continue;
          }
        }
      }
      // Add any additional special cases here.

      result.add(vi);
    }

    return result;
  }


  // Shouldn't have any vacuous variables, so comment this out.
  // /**
  //  * Returns non-nil if this variable is derived from non-canonical variables.
  //  * (We didn't know at the time we derived the variables that the derivees
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
    Assert.assert(type.equals(other.type));
    Assert.assert(rep_type.equals(other.rep_type));
    // One of the VarInfos might be at a program point with more variables,
    // so the list of variables to which it is comparable could be larger.
    // Assert.assert(comparability.equals(other.comparability));
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
    } else if (derived instanceof SequenceInitial) {
      SequenceInitial se = (SequenceInitial) derived;
      return se.seqvar();
    } else if (derived instanceof SequenceMax) {
      SequenceMax sm = (SequenceMax) derived;
      return sm.base;
    } else if (derived instanceof SequenceMin) {
      SequenceMin sm = (SequenceMin) derived;
      return sm.base;
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
    if (sequenceSize != null)
      return sequenceSize;
    Assert.assert(rep_type.isArray());
    // we know the size follows the variable itself in the list
    VarInfo[] vis = ppt.var_infos;
    for (int i=varinfo_index+1; i<vis.length; i++) {
      VarInfo vi = vis[i];
      if ((vi.derived instanceof SequenceLength)
          && (((SequenceLength) vi.derived).base == this)) {
        sequenceSize = vi;
        return sequenceSize;
      }
    }
    throw new Error("Couldn't find size of " + name);
  }

  // Returns true if the type in the original program is integer
  public boolean isIndex() {
    return ((rep_type == ProglangType.INT)
            && type.isIndex());
  }

  // Debugging
  public boolean isDerivedFromNonCanonical() {
    return ((derived != null) && (derived.isDerivedFromNonCanonical()));
  }

  ///////////////////////////////////////////////////////////////////////////
  /// Utility functions
  ///

  // Where do these really belong?


  // Given two variables I and J, indicate whether it is necessarily the
  // case that i<=j or i>=j.  The variables also each have a shift, so the
  // test is really for whether (i+1)<=(j-1).
  // The test is either:  i<=j or i>=j.
  public static boolean compare_vars(VarInfo scl_index, int scl_shift, VarInfo seq_index, int seq_shift, boolean test_lessequal) {
    if (scl_index == seq_index) {
      // same variable: B[I] in B[0..I] or B[I] in B[I..]
      return (test_lessequal
              ? (scl_shift <= seq_shift)
              : (scl_shift >= seq_shift));
    }
    // different variables: B[I] in B[0..J] or B[I] in B[J..]
    Assert.assert(scl_index.ppt == seq_index.ppt);
    PptSlice indices_ppt = scl_index.ppt.getView(scl_index, seq_index);
    if (indices_ppt == null)
      return false;

    boolean scl_is_var1 = (scl_index == indices_ppt.var_infos[0]);
    LinearBinary lb = LinearBinary.find(indices_ppt);
    long index_scl_minus_seq = -2222;          // valid only if lb != null
    if (lb != null) {
      if (!lb.justified()) {
        lb = null;
      } else if (lb.core.a != 1) {
        // Do not attempt to deal with anything but y=x+b.
        lb = null;
      } else {
        // lb.b is var2()-var1().
        index_scl_minus_seq = (scl_is_var1 ? -lb.core.b : lb.core.b);
        index_scl_minus_seq += scl_shift - seq_shift;
      }
    }
    // The LinearBinary gives more info than IntComparison would,
    // so only compute the IntComparison if no LinearBinary.
    IntComparison ic = (lb != null) ? null : IntComparison.find(indices_ppt);
    boolean scl_can_be_lt = false;		// valid only if ic != null
    boolean scl_can_be_eq = false;		// valid only if ic != null
    boolean scl_can_be_gt = false;		// valid only if ic != null
    if (ic != null) {
      if (! ic.justified()) {
        ic = null;
      } else {
        scl_can_be_eq = ic.core.can_be_eq;
        if (scl_is_var1) {
          scl_can_be_lt = ic.core.can_be_lt;
          scl_can_be_gt = ic.core.can_be_gt;
        } else {
          scl_can_be_lt = ic.core.can_be_gt;
          scl_can_be_gt = ic.core.can_be_lt;
        }
      }
    }

    if (test_lessequal) {
      // different variables: B[I] in B[0..J]
      if (lb != null) {
        return (index_scl_minus_seq <= 0);
      } else if (ic != null) {
        // 4 cases:
        // (a) B[I] in B[0..J]
        // (b) B[I] in B[0..J-1]
        // (c) B[I-1] in B[0..J]
        // (d) B[I-1] in B[0..J-1]
        // 4 possible comparisons:
        // (e) I < J
        // (f) I <= J
        // (g) I >= J
        // (h) I > J
        // Combinations:  (filled in means true)
        //   abcd
        // e abcd
        // f a cd
        // g
        // h
        return ((scl_can_be_lt && ! scl_can_be_eq)
                || (scl_can_be_lt && scl_can_be_eq
                    && (scl_shift <= seq_shift)));
      } else {
        return false;
      }
    } else {
      // different variables: B[I] in B[J..]
      if (lb != null) {
        return (index_scl_minus_seq >= 0);
      } else if (ic != null) {
        // 4 cases:
        // (a) B[I] in B[J..]
        // (b) B[I] in B[J+1..]
        // (c) B[I-1] in B[J..]
        // (d) B[I-1] in B[J+1..]
        // 4 possible comparisons:
        // (e) I < J
        // (f) I <= J
        // (g) I >= J
        // (h) I > J
        // Combinations:  (filled in means true)
        //   abcd
        // e
        // f
        // g a
        // h abc
        return ((scl_can_be_gt && (! scl_can_be_eq)
                 && (scl_shift + 1 >= seq_shift))
                || (scl_can_be_gt && scl_can_be_eq
                    && (scl_shift == seq_shift)));
      } else {
        return false;
      }
    }
  }


  public static boolean seqs_overlap(VarInfo seq1, VarInfo seq2) {
    // Very limited implementation as of now.
    VarInfo super1 = seq1.isDerivedSubSequenceOf();
    VarInfo super2 = seq2.isDerivedSubSequenceOf();
    Assert.assert(super1 == super2);
    SequenceScalarSubsequence sss1 = (SequenceScalarSubsequence) seq1.derived;
    SequenceScalarSubsequence sss2 = (SequenceScalarSubsequence) seq2.derived;

    Assert.assert(sss1.seqvar() == sss2.seqvar());
    VarInfo index1 = sss1.sclvar();
    int shift1 = sss1.index_shift;
    boolean start1 = sss1.from_start;
    VarInfo index2 = sss2.sclvar();
    int shift2 = sss2.index_shift;
    boolean start2 = sss2.from_start;

    if (start1 == start2) {
      return compare_vars(index1, shift1, index2, shift2, start1);
    } else {
      // start1 != start2
      return compare_vars(index1, shift1, index2, shift2, start2);
    }
  }


}
