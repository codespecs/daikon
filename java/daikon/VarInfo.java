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
  public String esc_name;       // interned
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

  // DO NOT TEST EQUALITY!  ONLY USE ITS .name SLOT!!   -MDE 3/9/2001
  public VarInfo postState;     // non-null if this is an orig() variable

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

  public VarInfo(String name, String esc_name, ProglangType type, ProglangType rep_type, VarComparability comparability, boolean is_static_constant, Object static_constant_value) {
    // Watch out:  some Lisp and C .decls files have other (unsupported) types.
    Assert.assert(rep_type != null);
    Assert.assert(legalRepType(rep_type),
                  "Unsupported representation type " + rep_type.format() + " for variable " + name);

    // Possibly the call to intern() isn't necessary; but it's safest to
    // make the call to intern() rather than running the risk that a caller
    // didn't.
    this.name = name.intern();
    this.esc_name = esc_name.intern();
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

  public VarInfo(String name, String esc_name, ProglangType type, ProglangType rep_type, VarComparability comparability) {
    this(name, esc_name, type, rep_type, comparability, false, null);
  }

  public VarInfo(VarInfo vi) {
    this(vi.name, vi.esc_name, vi.type, vi.rep_type, vi.comparability, vi.is_static_constant, vi.static_constant_value);
    postState = vi.postState;
  }

  public static VarInfo origVarInfo(VarInfo vi) {
    VarInfo result = new VarInfo(VarInfo.makeOrigName(vi.name),
                                 VarInfo.makeOrigName_esc(vi.esc_name),
                                 vi.type, vi.rep_type,
                                 vi.comparability.makeAlias(vi.name));
    result.postState = vi;
    return result;
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

  // Returns true if this in an "orig()" variable
  public boolean isOrigVar() {
    return postState != null;
  }
  public static String makeOrigName(String s) {
    return "orig(" + s + ")";
  }
  public static String makeOrigName_esc(String s) {
    return "\\old(" + s + ")";
  }
  public static String unOrigName(String s) {
    int origpos = s.indexOf("orig(");
    Assert.assert(origpos != -1);
    int rparenpos = s.lastIndexOf(")");
    return s.substring(0, origpos)
      + s.substring(origpos+5, rparenpos)
      + s.substring(rparenpos+1);
  }
  public static String unOrigName_esc(String s) {
    int origpos = s.indexOf("\\old(");
    Assert.assert(origpos != -1);
    int rparenpos = s.lastIndexOf(")");
    return s.substring(0, origpos)
      + s.substring(origpos+5, rparenpos)
      + s.substring(rparenpos+1);
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
   * Returns a fresh Vector.
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
        } else if (vi.isOrigVar()
                   && vi.postState.name.endsWith(".class")) {
          // parent of "orig(x.class)" is "orig(x)"
          String post_name = vi.postState.name;
          sansclassname = VarInfo.makeOrigName(post_name.substring(0, post_name.length() - 6));
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
      // If derived from non-canonical, omit.
      // (This can happen for "size(...)" varables, which are always
      // introduced so that sequenceSize() will not fail.)
      if ((vi.derived != null)
          && vi.derived.isDerivedFromNonCanonical()) {
        continue;
      }
      // If size of a non-canonical array, omit.
      if ((vi.derived != null)
          && (vi.derived instanceof SequenceLength)) {
        VarInfo seq_contents = ((SequenceLength) vi.derived).base;
        String seq_contents_name = seq_contents.name;
        String seq_object_name = null;
        if (seq_contents_name.endsWith("[]")) {
          seq_object_name = seq_contents_name.substring(0, seq_contents_name.length()-2);
        } else if (seq_contents.isOrigVar()
                   && seq_contents.postState.name.endsWith("[]")) {
          String post_name = seq_contents.postState.name;
          seq_object_name = VarInfo.makeOrigName(post_name.substring(0, post_name.length()-2));
        } else if (seq_contents.isOrigVar()
                   && seq_contents.postState.name.endsWith("[].class")) {
          continue;
        } else {
          throw new Error("What object name? " + seq_contents_name
                          + (seq_contents.isOrigVar() ? " : " + seq_contents.postState.name : ""));
        }
        VarInfo seq_object = ppt.findVar(seq_object_name);
        if (! seq_object.isCanonical())
          continue;
      }
      // For esc_output, omit noting that varibles are unmodified.
      // Add any additional special cases here.
      if (Daikon.esc_output) {
        if ((vi.postState != null) && vi.postState.name.equals(this.name)) {
          continue;
        }
      }


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
  public static boolean compare_vars(VarInfo vari, int vari_shift, VarInfo varj, int varj_shift, boolean test_lessequal) {
    // System.out.println("compare_vars(" + vari.name + ", " + vari_shift + ", "+ varj.name + ", " + varj_shift + ", " + (test_lessequal?"<=":">=") + ")");
    if (vari == varj) {
      // same variable
      return (test_lessequal
              ? (vari_shift <= varj_shift)
              : (vari_shift >= varj_shift));
    }
    // different variables
    Assert.assert(vari.ppt == varj.ppt);
    PptSlice indices_ppt = vari.ppt.getView(vari, varj);
    if (indices_ppt == null)
      return false;

    boolean vari_is_var1 = (vari == indices_ppt.var_infos[0]);
    LinearBinary lb = LinearBinary.find(indices_ppt);
    long index_vari_minus_seq = -2222;          // valid only if lb != null
    if (lb != null) {
      if (!lb.justified()) {
        lb = null;
      } else if (lb.core.a != 1) {
        // Do not attempt to deal with anything but y=x+b.
        lb = null;
      } else {
        // System.out.println("justified LinearBinary: " + lb.format());
        // lb.b is var2()-var1().
        index_vari_minus_seq = (vari_is_var1 ? -lb.core.b : lb.core.b);
        index_vari_minus_seq += vari_shift - varj_shift;
      }
    }
    // The LinearBinary gives more info than IntComparison would,
    // so only compute the IntComparison if no LinearBinary.
    IntComparison ic = (lb != null) ? null : IntComparison.find(indices_ppt);
    boolean vari_can_be_lt = false;		// valid only if ic != null
    boolean vari_can_be_eq = false;		// valid only if ic != null
    boolean vari_can_be_gt = false;		// valid only if ic != null
    if (ic != null) {
      if (! ic.justified()) {
        ic = null;
      } else {
        // System.out.println("justified IntComparison: " + ic.format());
        vari_can_be_eq = ic.core.can_be_eq;
        if (vari_is_var1) {
          vari_can_be_lt = ic.core.can_be_lt;
          vari_can_be_gt = ic.core.can_be_gt;
        } else {
          vari_can_be_lt = ic.core.can_be_gt;
          vari_can_be_gt = ic.core.can_be_lt;
        }
      }
    }

    // System.out.println("test_lessequal=" + test_lessequal
    //                    + ", vari_can_be_lt=" + vari_can_be_lt
    //                    + ", vari_can_be_eq=" + vari_can_be_eq
    //                    + ", vari_can_be_gt=" + vari_can_be_gt);

    if (test_lessequal) {
      if (lb != null) {
        return (index_vari_minus_seq <= 0);
      } else if (ic != null) {
        return ((vari_can_be_lt && vari_can_be_eq
                    && (vari_shift <= varj_shift))
                || ((vari_can_be_lt && ! vari_can_be_eq)
                    && (vari_shift - 1 <= varj_shift)));
      } else {
        return false;
      }
    } else {
      if (lb != null) {
        return (index_vari_minus_seq >= 0);
      } else if (ic != null) {
        return ((vari_can_be_gt && vari_can_be_eq
                 && (vari_shift >= varj_shift))
                || (vari_can_be_gt && (! vari_can_be_eq)
                    && (vari_shift + 1 >= varj_shift)));
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
      // This test is too sophisticated (that is, it is wrong):
      //   return compare_vars(index1, shift1, index2, shift2, start1);
      // A[i..] always overlaps with A[j..]:  they share the same last element!
      return true;
    } else {
      // start1 != start2
      return compare_vars(index1, shift1, index2, shift2, start2);
    }
  }

//   /**
//    * @return ESC-formatted name for this variable, or null if variable is not describable
//    **/
//   public String esc_name()
//   {
//     String pre_wrapper = "";
//     String post_wrapper = "";
//     String result = name;
//     String previous_result = "";
//
//     // "size(array[])" -> "array.length"
//     while (result.indexOf("size(") != -1) {
//       int sizelp = result.lastIndexOf("size(");
//       int brackrp = result.indexOf("[])", sizelp);
//       if (sizelp >= 0) {
// 	Assert.assert(brackrp >= sizelp, "[]) follows size(");
// 	result =
// 	  result.substring(0, sizelp) +
// 	  result.substring(sizelp+5, brackrp) +
// 	  ".length" +
// 	  result.substring(brackrp+3);
//       }
//     }
//
//     while (!result.equals(previous_result)) {
//       previous_result = result;
//
//       // "orig(var)" -> "\old(var)"
//       if (result.startsWith("orig(")) {
//         pre_wrapper += "\\old(";
//         int rparen_pos = result.lastIndexOf(")");
//         post_wrapper = result.substring(rparen_pos) + post_wrapper;
//         result = result.substring(5, rparen_pos);
//       }
//
//       // "var.class" -> "\typeof(var)"
//       if (result.endsWith(".class")) {
//         pre_wrapper += "\\typeof(";
//         result = result.substring(0, result.length() - 6);
//         post_wrapper = ")" + post_wrapper;
//       }
//     }
//
//     // "return" -> "\result"
//     if ("return".equals(result)) {
//       result = "\\result";
//     }
//
//     System.out.println("esc_name = " + pre_wrapper + result + post_wrapper + "    for " + name);
//
//     return pre_wrapper + result + post_wrapper;
//   }

  /**
   * This is intended (only) for variable names read from files or other external sources.
   * @return ESC-formatted name for this variable, or null if variable is not describable
   **/
  public static String esc_name(String name)
  {
    String pre_wrapper = "";
    String post_wrapper = "";
    String result = name;
    String previous_result = "";

    // "size(array[])" -> "array.length"
    while (result.indexOf("size(") != -1) {
      int sizelp = result.lastIndexOf("size(");
      int brackrp = result.indexOf("[])", sizelp);
      if (sizelp >= 0) {
	Assert.assert(brackrp >= sizelp, "[]) follows size(");
	result =
	  result.substring(0, sizelp) +
	  result.substring(sizelp+5, brackrp) +
	  ".length" +
	  result.substring(brackrp+3);
      }
    }

    while (!result.equals(previous_result)) {
      previous_result = result;

      // "orig(var)" -> "\old(var)"
      if (result.startsWith("orig(")) {
        System.out.println("I didn't expect this to happen: esc_name(" + result + ")");
        pre_wrapper += "\\old(";
        int rparen_pos = result.lastIndexOf(")");
        post_wrapper = result.substring(rparen_pos) + post_wrapper;
        result = result.substring(5, rparen_pos);
      }

      // "var.class" -> "\typeof(var)"
      if (result.endsWith(".class")) {
        pre_wrapper += "\\typeof(";
        result = result.substring(0, result.length() - 6);
        post_wrapper = ")" + post_wrapper;
      }
    }

    // "return" -> "\result"
    if ("return".equals(result)) {
      result = "\\result";
    }

    if (result.endsWith("[]")) {
      result = result.substring(0, result.length()-2);
    }

    // System.out.println("esc_name = " + pre_wrapper + result + post_wrapper + "    for " + name);

    return pre_wrapper + result + post_wrapper;
  }


  /**
   * @return three-element array indicating upper and lower (inclusive)
   * bounds of the range of this array variable, and a canonical element at
   * index i.
   **/
  public String[] index_range() {
    String working_name = esc_name;
    String pre_wrapper = "";
    String post_wrapper = "";
    while (working_name.startsWith("\\") && working_name.endsWith(")")) {
      int open_paren_pos = working_name.indexOf("(");
      pre_wrapper += working_name.substring(0, open_paren_pos+1);
      post_wrapper += ")";
      working_name = working_name.substring(open_paren_pos+1, working_name.length()-1);
    }
    String minindex;
    String maxindex;
    String arrayname;
    if (working_name.endsWith("[]")) {
      minindex = "";
      maxindex = "";
      arrayname = working_name.substring(0, working_name.length()-2);
    } else if (! working_name.endsWith("]")) {
      minindex = "";
      maxindex = "";
      arrayname = working_name;
    } else {
      int open_bracket_pos = working_name.lastIndexOf("[");
      arrayname = working_name.substring(0, open_bracket_pos);
      String subscripts = working_name.substring(open_bracket_pos+1, working_name.length()-1);
      int dots_pos = subscripts.indexOf("..");
      if (dots_pos == -1) {
        throw new Error("can't find \"..\" in " + working_name);
      }
      minindex = subscripts.substring(0, dots_pos);
      maxindex = subscripts.substring(dots_pos+2);

    }
    if (minindex.equals("")) minindex = "0";
    if (maxindex.equals("")) maxindex = arrayname + ".length-1";
    String arrayelt = pre_wrapper + arrayname + "[i]" + post_wrapper;
    // System.out.println("index_range: " + name + " ( = " + esc_name + " ) ");
    // System.out.println("    => " + minindex + ", " + maxindex + ", " + arrayelt);

    return new String[] { minindex, maxindex, arrayelt };
  }

  /**
   * Return an array of two strings:
   * an esc forall quantifier, and
   * the expression for the element at index i of the array
   **/
  public String[] esc_forall() {
    String[] index_range = index_range();
    if (index_range.length != 3) {
      throw new Error("index_range failed for " + name);
    }
    return new String[] {
      "\\forall int i; (" + index_range[0] + " <= i & i <= " + index_range[1] + ") ==> ",
      index_range[2],
    };
  }

  /**
   * Return an array of three strings:
   * an esc forall quantifier, and
   * the expressions for the elements at index i of the two arrays
   **/
  public static String[] esc_forall_2(VarInfo var1, VarInfo var2) {
    String[] index_range1 = var1.index_range();
    String[] index_range2 = var2.index_range();
    Assert.assert(index_range1.length == 3, "no index_range: " + var1.name);
    Assert.assert(index_range2.length == 3, "no index_range: " + var2.name);
    String[] esc_forall1 = var1.esc_forall();
    String elt2 = index_range2[2];
    if (! index_range1[0].equals(index_range2[0])) {
      int i_pos = elt2.lastIndexOf("[i]");
      elt2 = elt2.substring(0, i_pos+2)
        + "-" + index_range1[0] + "+" + index_range2[0] + "]"
        + elt2.substring(i_pos+3);
    }
    return new String[] {
      esc_forall1[0],
      esc_forall1[1],
      elt2,
    };
  }

  // public static boolean isOrigVarName(String s) {
  //   return ((s.startsWith("orig(") && s.endsWith(")"))
  //           || (s.startsWith("\\old(") && s.endsWith(")")));
  // }

  // takes an "orig()" var and gives a pair of [name, esc_name] for a
  // variable or expression in the post-state which is equal to this one.
  public String[] postStateEquivalent() {
    return otherStateEquivalent(true);
  }

  public String[] preStateEquivalent() {
    return otherStateEquivalent(false);
  }

  public String[] otherStateEquivalent(boolean post) {

    // Below is equivalent to:
    // Assert.assert(post == isOrigVar());
    if (post != isOrigVar()) {
      throw new Error("Shouldn't happen (should it?): "
                      + (post ? "post" : "pre") + "StateEquivalent(" + name + ")");
      // return new String[] { name, esc_name };
    }

    Assert.assert(isCanonical());
    Vector equal_vars = equalTo();
    for (int i=0; i<equal_vars.size(); i++) {
      VarInfo vi = (VarInfo)equal_vars.elementAt(i);
      if (post != vi.isOrigVar()) {
        // System.out.println("postStateEquivalent(" + name + ") = " + vi.name);
        return new String[] { vi.name, vi.esc_name };
      }
    }

    // Didn't find an exactly equal variable; try LinearBinary.
    // (Should also try other exact invariants.)
    {
      Vector lbs = LinearBinary.findAll(this);
      for (int i=0; i<lbs.size(); i++) {
        LinearBinary lb = (LinearBinary) lbs.elementAt(i);
        String lb_format = null;
        String lb_format_esc = null;
        if (this.equals(lb.var2())
            && (post != lb.var1().isOrigVar())) {
          lb_format = lb.format();
          lb_format_esc = lb.format_esc();
        } else if (this.equals(lb.var1())
                   && (post != lb.var2().isOrigVar())) {
          Assert.assert((lb.core.a == 1) || (lb.core.a == -1));
          lb_format = lb.format_reversed();
          lb_format_esc = lb.format_esc_reversed();
        }
        if (lb_format != null) {
          int eq_pos;
          eq_pos = lb_format.indexOf(" == "); // "interned"
          Assert.assert(eq_pos != -1);
          lb_format = lb_format.substring(eq_pos + 4);
          eq_pos = lb_format_esc.indexOf(" == "); // "interned"
          Assert.assert(eq_pos != -1);
          lb_format_esc = lb_format_esc.substring(eq_pos + 4);
          return new String[] { lb_format, lb_format_esc };
        }
      }
    }

    // Can't find post-state equivalent.
    return null;
  }


}
