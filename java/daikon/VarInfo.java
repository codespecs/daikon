package daikon;

import daikon.derive.*;
import daikon.derive.unary.*;
import daikon.derive.binary.*;
import daikon.inv.*;
import daikon.inv.binary.twoScalar.*;
import utilMDE.*;

import java.util.*;
import java.io.Serializable;
import java.io.ObjectInputStream;
import java.io.IOException;
  
/**
 * Represents information about a particular variable for a program
 * point.  This object doesn't hold the value of the variable at a
 * particular step of the program point, but can get the value it
 * holds when given a ValueTuple using the getValue() method.  VarInfo
 * also includes info on the variable's name, its declared type, its
 * file representation type, its internal type and its comparability.
 **/
public final class VarInfo
  implements Cloneable, Serializable
{

  /**
   * The program point this variable is in.
   **/
  public PptTopLevel ppt;

  // Name and type

  /**
   * Expression of this variable's name.
   **/
  public VarInfoName name;      // interned

  /**
   * Type as declared in the program.
   **/
  public ProglangType type;      // interned

  /**
   * Type as written in the data trace file.  This is an interface
   * detail.
   **/
  public ProglangType file_rep_type;      // interned

  /**
   * Type as internally stored.  This is an interface detail.
   **/
  public ProglangType rep_type;      // interned

  /**
   * Comparability info
   **/
  public VarComparability comparability;

  // Obtaining values

  /**
   * The index in lists of VarInfo objects.
   **/
  public int varinfo_index;

  /**
   * The actual value that this variable would point to in a
   * ValueTuple
   **/
  public int value_index;	// index in lists of values, VarTuple objects

  public boolean is_static_constant;  // required if static_constant_value==null
				//   (is_static_constant
                                //   iff (value_index == -1)
  Object static_constant_value;	// null if not statically constant
                                // once upon a time (still?):
  				//   (static_constant_value != null)
                                //   iff (value_index == -1)

  // Partial ordering relationships between variables.
  // If A is higher than B then every value seen at B is seen at A.
  // Use mixed-type rep to save space.

  private Object po_higher; // either null, VarInfo, or VarInfo[] with no duplicates
  private Object po_lower;  // either null, VarInfo, or VarInfo[] with no duplicates
  private int[] po_higher_nonce; // null iff po_higher is null, else length == po_higher().size()

  // Derived variables

  /**
   * Whether and how derived.  Null if this is not derived.
   **/
  public Derivation derived;

  /**
   * Vector of Derivation objects
   **/
  /* [INCR] is now computed on the fly by derivees() method
  public Vector derivees;
  */

  // We don't know about canBeMissing or canBeNull anymore, since we
  // see data incrementally, instead of slurping it all first.
  // [[INCR]] ....
  // boolean canBeMissing = false;
  // public boolean canBeNull = false;    // relevant only for arrays, really
  // .... [[INCR]]

  // It can be expensive to find an arbitrary invariant.  These fields
  // cache invariants that we want to be able to look up quickly.

  /* [INCR]
  // Only public so that PptTopLevel can access it.
  // Clients should use isCanonical() or canonicalRep() or equalTo().
  public VarInfo equal_to;      // the canonical representative to which
                                // this variable is equal; may be itself;
                                // should not be null.
  */
  /* [INCR]
  public boolean is_dynamic_constant;  // required if dynamic_constant==null
  public Object dynamic_constant;
  */
  VarInfo sequenceSize;         // if null, not yet computed (or this VarInfo
                                //   is not a sequence)

  // DO NOT TEST EQUALITY!  ONLY USE ITS .name SLOT!!   -MDE 3/9/2001
  public VarInfo postState;     // non-null if this is an orig() variable

  // Does not include equal_to, which is dealt with elsewhere.
  // An invariant is only listed on the first VarInfo, not all VarInfos.
  // public Vector exact_nonunary_invariants; // [INCR]

  /**
   * @exception RuntimeException if representation invariant on this is broken
   */
  public void checkRep() {
    Assert.assert(ppt != null);
    Assert.assert(name != null);
    Assert.assert(name == name.intern());
    Assert.assert(type != null);
    Assert.assert(file_rep_type != null);
    Assert.assert(rep_type != null);
    Assert.assert(comparability != null); // anything else ??
    Assert.assert(0 <= varinfo_index && varinfo_index < ppt.var_infos.length);
    Assert.assert(-1 <= value_index && value_index < varinfo_index);
    Assert.assert(is_static_constant == (value_index == -1));
    Assert.assert(is_static_constant || (static_constant_value == null));
    if (po_higher == null) {
      Assert.assert(po_higher_nonce == null);
    } else if (po_higher instanceof VarInfo) {
      Assert.assert(po_higher_nonce != null);
      Assert.assert(po_higher_nonce.length == 1);
    } else {
      Assert.assert(po_higher instanceof VarInfo[]);
      VarInfo[] ary = (VarInfo[]) po_higher;
      Assert.assert(! ArraysMDE.any_null(ary));
      Assert.assert(po_higher_nonce != null);
      Assert.assert(po_higher_nonce.length == ary.length);      
    }
    if (po_lower == null) {
      Assert.assert(po_higher_nonce == null);
    } else if (po_higher instanceof VarInfo) {
    } else {
      Assert.assert(po_higher instanceof VarInfo[]);
      VarInfo[] ary = (VarInfo[]) po_higher;
      Assert.assert(! ArraysMDE.any_null(ary));
    }
    // check lower/higher rep types matching, too ??
    // Derivation derived; ??
    // VarInfo sequenceSize; ??
    // VarInfo postState; // non-null if this is an orig() variable
  }

  static boolean legalRepType(ProglangType rep_type) {
    return ((rep_type == ProglangType.INT)
            || (rep_type == ProglangType.DOUBLE)
            || (rep_type == ProglangType.STRING)
            || (rep_type == ProglangType.INT_ARRAY)
            || (rep_type == ProglangType.DOUBLE_ARRAY)
            || (rep_type == ProglangType.STRING_ARRAY));
  }

  static boolean legalFileRepType(ProglangType file_rep_type) {
    return (legalRepType(file_rep_type)
            || (file_rep_type == ProglangType.HASHCODE)
            || (file_rep_type == ProglangType.HASHCODE_ARRAY)
            || ((file_rep_type.dimensions() <= 1)
                && file_rep_type.baseIsPrimitive())
            );
  }

  public VarInfo(VarInfoName name, ProglangType type, ProglangType file_rep_type, VarComparability comparability, boolean is_static_constant, Object static_constant_value) {
    Assert.assert(file_rep_type != null);
    Assert.assert(legalFileRepType(file_rep_type),
                  "Unsupported representation type " + file_rep_type.format()
                  + " for variable " + name);

    // Ensure that the type and rep type are somewhat consistent
    Assert.assert(type.pseudoDimensions() >= file_rep_type.dimensions(),
		  "Types dimensions incompatibility: " + type + " vs. " + file_rep_type);

    // Possibly the call to intern() isn't necessary; but it's safest to
    // make the call to intern() rather than running the risk that a caller
    // didn't.
    this.name = name.intern();
    this.type = type;
    this.file_rep_type = file_rep_type;
    this.rep_type = file_rep_type.fileTypeToRepType();
    this.comparability = comparability;
    this.is_static_constant = is_static_constant;
    this.static_constant_value = static_constant_value;

    // Indicates that these haven't yet been set to reasonable values.
    value_index = -1;
    varinfo_index = -1;

    // derivees = new Vector(3); // [INCR]

    // exact_nonunary_invariants = new Vector(2); // [INCR]
  }

  public VarInfo(VarInfoName name, ProglangType type, ProglangType file_rep_type, VarComparability comparability) {
    this(name, type, file_rep_type, comparability, false, null);
  }

  public VarInfo(VarInfo vi) {
    this(vi.name, vi.type, vi.file_rep_type, vi.comparability, vi.is_static_constant, vi.static_constant_value);
    postState = vi.postState;
  }

  // Create the prestate, or "orig()", version of the variable.
  public static VarInfo origVarInfo(VarInfo vi) {
    VarInfo result = new VarInfo(vi.name.applyPrestate(),
                                 vi.type, vi.file_rep_type,
                                 vi.comparability.makeAlias(vi.name));
    result.postState = vi;
    return result;
  }

  // I *think* I don't need to implement VarInfo.clone(), as the
  // java.lang.Object version is sufficient.
  // protected Object clone() { ... }
  // ^^^ [INCR] Not sure if this is true anymore, or ever where we clone VarInfos

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
  public static VarInfo[] arrayclone_clever_not_currently_used(VarInfo[] a_old) {
    VarInfo[] a_new = new VarInfo[a_old.length];
    for (int i=0; i<a_new.length; i++) {
      try {
        a_new[i] = (VarInfo) a_old[i].clone();
      } catch (CloneNotSupportedException e) {
        e.printStackTrace();
        throw new Error(e.toString());
      }
      // a_new[i].canBeMissing = false; // [[INCR]]
      // I must set this; even though the specified variables will still
      // be equal, they may have a different canonical representative.
      // a_new[i].equal_to = null; // [INCR]
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
      if (deriv_old != null) {
        Derivation deriv_new = deriv_old.switchVars(a_old, a_new);
        deriv_map.put(deriv_old, deriv_new);
        a_new[i].derived = deriv_new;
      }
    }
    /* [INCR]
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
    */
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

  /** Trims the collections used by this VarInfo */
  public void trimToSize() {
    // ((ArrayList) private_po_higher).trimToSize();
    // ((ArrayList) private_po_lower).trimToSize();
    // if (derivees != null) { derivees.trimToSize(); }
    // Derivation derived; probably can't be trimmed
  }


  /**
   * @return read-only collection over variables immediately higher in
   * the partial order (compared to this).  Not a view (sees no updates).
   * If A is higher than B then every value seen at B is seen at A.
   * Elements are VarInfos.  Contains no duplicates.
   * @see po_lower()
   * @see po_higher_nonce()   
   **/
  public List po_higher() {
    if (po_higher == null) {
      return Collections.EMPTY_LIST;
    }
    if (po_higher instanceof VarInfo) {
      return Collections.singletonList(po_higher);
    }
    VarInfo[] ary = (VarInfo[]) po_higher;
    return Collections.unmodifiableList(Arrays.asList(ary));
  }

  /**
   * @return read-only int[] giving the nonces for po_higher()
   * @see po_higher()
   **/
  public int[] po_higher_nonce() {
    return po_higher_nonce; // writable; oh well
  }

  /**
   * @return read-only collection over variables immediately lower in
   * the partial order (compared to this).  Not a view (sees no updates).
   * If A is higher than B then every value seen at B is seen at A.
   * Elements are VarInfos.  Contains no duplicates.
   * @see po_higher()
   **/
  public List po_lower() {
    if (po_lower == null) {
      return Collections.EMPTY_LIST;
    }
    if (po_lower instanceof VarInfo) {
      return Collections.singletonList(po_lower);
    }
    VarInfo[] ary = (VarInfo[]) po_lower;
    return Collections.unmodifiableList(Arrays.asList(ary));
  }

  /**
   * @return read-only int[] giving the nonces for po_higher()
   * @see po_higher()
   **/
  public int[] po_lower_nonce() {
    List lo = po_lower();
    int[] result = new int[lo.size()];
    for (int i = 0; i < result.length; i++) {
      VarInfo lower = (VarInfo) lo.get(i);
      List hi = lower.po_higher();
      int index = hi.indexOf(this);
      Assert.assert(index >= 0);
      int nonce = lower.po_higher_nonce[index];
      result[i] = nonce;
    }
    return result;
  }

  /**
   * Adds a link in the partial order, setting "higher" to be higher
   * than "this".  The given nonce is associated with the link.  It is
   * an error if the link already exists.
   **/
  public void addHigherPO(VarInfo higher,
			  int nonce)
  {
    VarInfo lower = this;

    Assert.assert(lower != higher);
    Assert.assert(lower.ppt != higher.ppt);
    Assert.assert(lower.type == higher.type);
    Assert.assert(lower.rep_type == higher.rep_type);
    Assert.assert(lower.file_rep_type == higher.file_rep_type);

    boolean already = lower.po_higher().contains(higher);
    Assert.assert(already == higher.po_lower().contains(lower));
    if (already) 
      throw new IllegalArgumentException("Relation already exists");

    // lower.po_higher.add(higher)
    // lower.po_higher_nonce.add(nonce)
    if (lower.po_higher == null) {
      lower.po_higher = higher;
      lower.po_higher_nonce = new int[] { nonce };
    } else if (lower.po_higher instanceof VarInfo) {
      lower.po_higher = new VarInfo[] { (VarInfo) lower.po_higher, higher };
      lower.po_higher_nonce = new int[] { lower.po_higher_nonce[0], nonce };
    } else {
      VarInfo[] po_old = (VarInfo[]) lower.po_higher;
      int[] po_old_nonce = lower.po_higher_nonce;
      VarInfo[] po_new = new VarInfo[po_old.length + 1];
      int[] po_new_nonce = new int[po_old.length + 1];
      System.arraycopy(po_old, 0, po_new, 0, po_old.length);
      System.arraycopy(po_old_nonce, 0, po_new_nonce, 0, po_old.length);
      po_new[po_old.length] = higher;
      po_new_nonce[po_old.length] = nonce;
    }
    // higher.po_lower.add(lower)
    if (higher.po_lower == null) {
      higher.po_lower = lower;
    } else if (higher.po_lower instanceof VarInfo) {
      higher.po_lower = new VarInfo[] { (VarInfo) higher.po_lower, lower };
    } else {
      VarInfo[] po_old = (VarInfo[]) higher.po_lower;
      VarInfo[] po_new = new VarInfo[po_old.length + 1];
      System.arraycopy(po_old, 0, po_new, 0, po_old.length);
      po_new[po_old.length] = lower;
    }
  }

  /**
   * @param lower true iff the closure is over lower elements
   * @return stable BFS iterator
   **/
  public Iterator closurePO(boolean lower) {
    List result = new ArrayList();
    LinkedList worklist = new LinkedList(lower ? po_lower() : po_higher());
    while (! worklist.isEmpty()) {
      VarInfo head = (VarInfo) worklist.removeFirst();
      if (! result.contains(head)) {
	result.add(head);
	worklist.addAll(lower ? head.po_lower() : head.po_higher());
      }
    }
    Assert.assert(! result.contains(this));
    return Collections.unmodifiableList(result).iterator();
  }

  public String toString() { return repr(); }

  public String repr() {
    return "<VarInfo " + name + ": "
      + "type=" + type
      + ",file_rep_type=" + file_rep_type
      + ",rep_type=" + rep_type
      + ",comparability=" + comparability
      + ",value_index=" + value_index
      + ",varinfo_index=" + varinfo_index
      + ",is_static_constant=" + is_static_constant
      + ",static_constant_value=" + static_constant_value
      + ",derived=" + derived
      + ",derivees=" + derivees()
      + ",ppt=" + ppt.name
      // + ",equal_to=" + equal_to // [INCR]
      + ">";
  }

  /* [INCR]
  public boolean isConstant() {
    return (isStaticConstant() || isDynamicConstant());
  }
  public boolean isDynamicConstant() {
    // return (dynamic_constant != null);
    return is_dynamic_constant;
  }
  */
  public boolean isStaticConstant() {
    // return (static_constant_value != null);
    return is_static_constant;
  }
  public Object constantValue() {
    if (isStaticConstant()) {
      return static_constant_value;
    /* [INCR]
    } else if (isDynamicConstant()) {
      return dynamic_constant;
    */
    } else {
      throw new Error("Variable " + name + " is not constant");
    }
  }

  // Returns true if this is an "orig()" variable
  public boolean isPrestate() {
    return postState != null;
  }

  /* [INCR] ...
  public boolean hasExactInvariant(VarInfo other) {
    Assert.assert(this.varinfo_index < other.varinfo_index);
    for (int i=0; i<exact_nonunary_invariants.size(); i++) {
      Invariant inv = (Invariant) exact_nonunary_invariants.elementAt(i);
      if (inv.ppt.var_infos[0] != this) {
        System.out.println("Problem: " + inv.ppt.var_infos[0].name + ", " + this.name + " in " + this.ppt.name + ", " + inv.ppt.name);
      }
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
  */ // ... [INCR]

  public boolean isDerived() {
    return (derived != null);
  }
  public int derivedDepth() {
    if (derived == null)
      return 0;
    else
      return derived.derivedDepth();
  }

  public List derivees() {
    ArrayList result = new ArrayList();
    VarInfo[] vis = ppt.var_infos;
    for (int i=0; i < vis.length; i++) {
      VarInfo vi = vis[i];
      Derivation der = vi.derived;
      if (der == null) continue;
      if (ArraysMDE.indexOf(der.getBases(), this) >= 0) {
	result.add(der);
      }
    }
    return result;
  }

  public boolean isClosure() {
    // This should eventually turn into
    //   return name.indexOf("closure(") != -1;
    // when I rename those variables to "closure(...)".
    return name.name().indexOf("~") != -1; // XXX
  }

  // [[INCR]] ....
  // We don't know this anymore, since we see data incrementally,
  // instead of all at once.
  //    public boolean canBeMissingCheck() {
  //      return (canBeMissing
  //              && (Daikon.invariants_check_canBeMissing
  //                  || (Daikon.invariants_check_canBeMissing_arrayelt
  //                      // Probably bad to repeat this all the time at runtime.
  //                      && (name.name().indexOf("[") != -1)))); // XXX ???
  //    }
  // .... [[INCR]]

  public int getModified(ValueTuple vt) {
    if (is_static_constant)
      return ValueTuple.STATIC_CONSTANT;
    else
      return vt.getModified(value_index);
  }
  public boolean isUnmodified(ValueTuple vt) { return ValueTuple.modIsUnmodified(getModified(vt)); }
  public boolean isModified(ValueTuple vt) { return ValueTuple.modIsModified(getModified(vt)); }
  public boolean isMissing(ValueTuple vt) { return ValueTuple.modIsMissing(getModified(vt)); }

  /**
   * Get the value of this variable at a particular step (i.e. ValueTuple)
   * @param vt The ValueTuple from which to extract the value
   **/

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

  public String[] getStringArrayValue(ValueTuple vt) {
    return(String[]) getValue(vt);
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

  /* [INCR]
  public boolean isCanonical() {
    Assert.assert(equal_to != null);
    return (equal_to == this);
  }
  */

  // Canonical representative that's equal to this variable.
  /* [INCR]
  public VarInfo canonicalRep() {
    Assert.assert(equal_to != null);
    return equal_to;
  }
  */

  /**
   * Returns all other variables that are equal to this variable.
   * Returns a fresh Vector.
   * The result Vector does not include this.
   * Also see @link{equalToNonobvious}.
   **/
  /* [INCR] ...
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
  */ // ... [INCR]

  /* [INCR] ...
  // Like equalTo, but drops out things which can be inferred to be equal
  // to the first.  This is called only while printing invariants.
  public Vector equalToNonobvious() {
    // should only call this for canonical variables
    Assert.assert(isCanonical());

    Vector result = new Vector();

    HashSet controlling_equalTo = new HashSet(); // of VarInfoName
    {
      Iterator controllers = ppt.controlling_ppts.iterator();
      while (controllers.hasNext()) {
        PptTopLevel controller = (PptTopLevel) controllers.next();
        VarInfo controller_var = controller.findVar(name);
        if (controller_var != null) {
          // System.out.println("Considering " + name + " in " + controller.name);
          // This can fail if there are no :::OBJECT program points in the .dtrace file.
	  if (controller_var.equal_to != null) { // XXX is this a good thing?
	    Vector this_equalTo = controller_var.equal_to.equalTo();
	    for (int i=0; i<this_equalTo.size(); i++) {
	      controlling_equalTo.add(((VarInfo)this_equalTo.elementAt(i)).name);
	    }
	  }
        }
      }
    }

    VarInfo[] vis = ppt.var_infos;
    for (int i=0; i<vis.length; i++) {
      Assert.assert(vis[i].equal_to == vis[i].equal_to.equal_to);
      if (i == varinfo_index)
        continue;
      VarInfo vi = vis[i];
      if (vi.equal_to != this)
        continue;
      if (controlling_equalTo.contains(vi.name))
        continue;

      // System.out.println("Considering " + vi.name);

      // Special cases of variables to omit.

      // Variables such that both are one less than something else
      // (or generalized, the same shift from something else
      if ((name instanceof VarInfoName.Add) && (vi.name instanceof VarInfoName.Add) &&
	  ((((VarInfoName.Add) name).amount) == (((VarInfoName.Add) vi.name).amount))) {
        continue;
      }

      {
        // An element b.class is omitted if:
        //  * "b.class" is non-canonical
        //    We know this because we are only examining non-canonical
        //    variables which are equal to this, which is canonical.
        //  * "b" is non-canonical
        //  * there exists an a such that a=b (ie, equal_to slot of "b"'s
        //     varinfo is non-null); also, assert that "a.class" is in equalTo

	VarInfoName sansclassname = null;
	if (vi.name instanceof VarInfoName.TypeOf) {
	   sansclassname = ((VarInfoName.TypeOf) vi.name).term;
	} else if (vi.isPrestate()) {
	  VarInfoName post = vi.postState.name;
	  if (post instanceof VarInfoName.TypeOf) {
	    // parent of "orig(x.class)" is "orig(x)"
	    sansclassname = ((VarInfoName.TypeOf) post).term.applyPrestate();
	  }
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
            VarInfo a_class = ppt.findVar(a.name.applyTypeOf());
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
        // This is the size of an array; is that array non-canonical?

        VarInfo seq_contents = ((SequenceLength) vi.derived).base;
        VarInfoName seq_object_name = null;

        // We expect the variable name to end with "[]", possibly wrapped
        // in "orig()" and/or suffixed by ".class".
        if (seq_contents.isPrestate()) {
	  VarInfoName unorig = seq_contents.postState.name;
	  // orig(a[].class) -> skip
	  if (unorig instanceof VarInfoName.TypeOf) continue;
	  // orig(a[]) -> orig(a)
	  if (unorig instanceof VarInfoName.Elements) {
	    seq_object_name = ((VarInfoName.Elements) unorig).term.applyPrestate();
	  }
	} else {
	  // a[] -> a
	  if (seq_contents.name instanceof VarInfoName.Elements) {
	    seq_object_name = ((VarInfoName.Elements) seq_contents.name).term;
	  }
        }

	if (seq_object_name == null) {
	  // This can happen with e.g. this.seq[].field
	  // Used to be error; dfec actually does this though (?)
          // System.out.println("equalToNonobvious: cannot handle sequence variable " + seq_contents.name);
	  seq_object_name = seq_contents.name;
	}

        VarInfo seq_object = ppt.findVar(seq_object_name);

        // First part of test is for Lisp output files; shouldn't happen in general
        if (seq_object != null && ! seq_object.isCanonical())
          continue;
      }

      // For esc_output, omit noting that varibles are unmodified.
      // Add any additional special cases here.
      if (Daikon.output_style == Daikon.OUTPUT_STYLE_ESC) {
        if ((vi.postState != null) && vi.postState.name.equals(this.name)) {
          continue;
        }
      }

      result.add(vi);
    }

    return result;
  }
  */ // ... [INCR]

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

  static boolean comparable2(VarInfo[] vis1, VarInfo[] vis2) {
    if (vis1.length != vis2.length)
      return false;

    for (int i=0; i<vis1.length; i++)
      if (!vis1[i].comparable2(vis2[i]))
	return false;

    return true;
  }

  // simplistic implementation, just checks that the names are the same
  boolean comparable2(VarInfo other) {
    if (this.name != other.name)
      return false;
    Assert.assert(type.equals(other.type), "type matches");
    Assert.assert(file_rep_type.equals(other.file_rep_type),
		  "file_rep_type matches (" +
		  name + ":" + file_rep_type + "," +
		  other.name + ":" + other.file_rep_type +
		  ")");
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

  public boolean isDerivedSequenceMinMaxSum() {
    return (derived != null)
      && ((derived instanceof SequenceMax) ||
	  (derived instanceof SequenceMin) ||
	  (derived instanceof SequenceSum));
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
    // It is possible that this VarInfo never had its size derived,
    // since it looked something like this.ary[].field.  In this case,
    // we should return size(this.ary[]), since it was derived and
    // must be the same values.
    {
      VarInfoName search = this.name;
      boolean pre = false;
      if (search instanceof VarInfoName.Prestate) {
	search = ((VarInfoName.Prestate) search).term;
	pre = true;
      }
      while (search instanceof VarInfoName.Field) {
	search = ((VarInfoName.Field) search).term;
      }
      if (pre) {
	search = search.applyPrestate();
      }
      search = search.applySize();
      VarInfo result = ppt.findVar(search);
      if (result != null) {
	return result;
//        } else {
//  	System.out.println("Warning: Size variable " + search + " not found.");
//  	System.out.print("Variables: ");
//  	for (int i=0; i<ppt.var_infos.length; i++) {
//  	  VarInfo vi = ppt.var_infos[i];
//  	  System.out.print(vi.name + " ");
//  	}
//  	System.out.println();
      }
    }
//    throw new Error("Couldn't find size of " + name);
    return null;
  }

  // Returns true if the type in the original program is integer.
  // Should perhaps check Daikon.check_program_types and behave differently
  // depending on that.
  public boolean isIndex() {
    return ((file_rep_type == ProglangType.INT)
            && type.isIndex());
  }

  /* [INCR]
  // Debugging
  public boolean isDerivedFromNonCanonical() {
    return ((derived != null) && (derived.isDerivedFromNonCanonical()));
  }
  */


  ///////////////////////////////////////////////////////////////////////////
  /// IOA functions
  ///

  /**
   * return true if declared type is Set (IOA syntax)
   **/
  public boolean isIOASet() {
    return type.base().startsWith("Set");
  }

  /**
   * return true if declared type is Set (IOA syntax)
   **/
  public boolean isIOAArray() {
    return type.base().startsWith("Array");
  }

  /**
   * return declared element type (in string) of IOA Set or Array
   **/
  public String elementTypeIOA() {
    String result;
    int begin;
    int end = type.base().indexOf(')');
    if (this.isIOASet())
      begin = type.base().indexOf('(') + 1;
    else if (this.isIOAArray())
      begin = type.base().indexOf(',') + 1;
    else
      return null;
    return type.base().substring(begin, end);
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
      if (!lb.enoughSamples()) {
        lb = null;
      } else if (lb.core.a != 1) {
        // Do not attempt to deal with anything but y=x+b.
        lb = null;
      } else {
        // System.out.println("justified LinearBinary: " + lb.format());
        // lb.b is var2()-var1().

        // a is 1 or -1, and the values are integers, so be must be an integer
        long b_int = (long)lb.core.b;
        Assert.assert(lb.core.b == b_int);
        index_vari_minus_seq = (vari_is_var1 ? -b_int : b_int);
        index_vari_minus_seq += vari_shift - varj_shift;
      }
    }
    // // The LinearBinary gives more info than IntComparison would,
    // // so only compute the IntComparison if no LinearBinary.
    // IntComparison ic = (lb != null) ? null : IntComparison.find(indices_ppt);
    // boolean vari_can_be_lt = false;              // valid only if ic != null
    // boolean vari_can_be_eq = false;         // valid only if ic != null
    // boolean vari_can_be_gt = false;         // valid only if ic != null
    // if (ic != null) {
    //   if (! ic.enoughSamples()) {
    //     ic = null;
    //   } else {
    //     // System.out.println("justified IntComparison: " + ic.format());
    //     vari_can_be_eq = ic.core.can_be_eq;
    //     if (vari_is_var1) {
    //       vari_can_be_lt = ic.core.can_be_lt;
    //       vari_can_be_gt = ic.core.can_be_gt;
    //     } else {
    //       vari_can_be_lt = ic.core.can_be_gt;
    //       vari_can_be_gt = ic.core.can_be_lt;
    //     }
    //   }
    // }

    boolean vari_lt = false;
    boolean vari_le = false;
    boolean vari_gt = false;
    boolean vari_ge = false;
    {
      IntLessEqual ile = IntLessEqual.find(indices_ppt);
      IntLessThan ilt = IntLessThan.find(indices_ppt);
      IntGreaterEqual ige = IntGreaterEqual.find(indices_ppt);
      IntGreaterThan igt = IntGreaterThan.find(indices_ppt);
      if (ile != null && ! ile.enoughSamples()) { ile = null; }
      if (ilt != null && ! ilt.enoughSamples()) { ilt = null; }
      if (ige != null && ! ige.enoughSamples()) { ige = null; }
      if (igt != null && ! igt.enoughSamples()) { igt = null; }

      if (vari_is_var1) {
        vari_lt = ilt != null;
        vari_le = ile != null;
        vari_gt = igt != null;
        vari_ge = ige != null;
      } else {
        vari_lt = igt != null;
        vari_le = ige != null;
        vari_gt = ilt != null;
        vari_ge = ile != null;
      }
    }

    // System.out.println("test_lessequal=" + test_lessequal
    //                    + ", vari_can_be_lt=" + vari_can_be_lt
    //                    + ", vari_can_be_eq=" + vari_can_be_eq
    //                    + ", vari_can_be_gt=" + vari_can_be_gt);

    if (test_lessequal) {
      if (lb != null) {
        return (index_vari_minus_seq <= 0);
      } else {
        return ((vari_le
                 && (vari_shift <= varj_shift))
                || (vari_lt
                    && (vari_shift - 1 <= varj_shift)));
      }
    } else {
      if (lb != null) {
        return (index_vari_minus_seq >= 0);
      } else {
        return ((vari_ge
                 && (vari_shift >= varj_shift))
                || (vari_gt
                    && (vari_shift + 1 >= varj_shift)));
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

  // takes an "orig()" var and gives a VarInfoName for a variable or
  // expression in the post-state which is equal to this one.
  public VarInfoName postStateEquivalent() {
    return otherStateEquivalent(true);
  }

  // takes a non-"orig()" var and gives a VarInfoName for a variable
  // or expression in the pre-state which is equal to this one.
  public VarInfoName preStateEquivalent() {
    return otherStateEquivalent(false);
  }

  public VarInfoName otherStateEquivalent(boolean post) {

    // Below is equivalent to:
    // Assert.assert(post == isPrestate());
    if (post != isPrestate()) {
      throw new Error("Shouldn't happen (should it?): "
                      + (post ? "post" : "pre") + "StateEquivalent(" + name + ")");
    }

    // First look for equality invariants
    /* [INCR]
    Assert.assert(isCanonical());
    Vector equal_vars = equalTo();
    for (int i=0; i<equal_vars.size(); i++) {
      VarInfo vi = (VarInfo)equal_vars.elementAt(i);
      if (post != vi.isPrestate()) {
        return vi.name;
      }
    }
    */

    // Didn't find an exactly equal variable; try LinearBinary.
    {
      Vector lbs = LinearBinary.findAll(this);
      for (int i=0; i<lbs.size(); i++) {
        LinearBinary lb = (LinearBinary) lbs.elementAt(i);

        if (this.equals(lb.var2()) && (post != lb.var1().isPrestate())) {
	  // this = a * v1 + b
	  double a = lb.core.a, b = lb.core.b;
	  if (a == 1) {
	    // this = v1 + b
	    int add = (int) b;
	    return lb.var1().name.applyAdd(add);
	  }
        }

	if (this.equals(lb.var1()) && (post != lb.var2().isPrestate())) {
	  // v2 = a * this + b
	  double a = lb.core.a, b = lb.core.b;
	  if (a == 1) {
	    // this = v2 - b
	    int add = -((int) b);
	    return lb.var2().name.applyAdd(add);
	  }
        }
      }

      // Should also try other exact invariants...
    }

    // Can't find post-state equivalent.
    return null;
  }

  private final static boolean debug_simplify_expression = false;
  /**
   * Change the name of this VarInfo into a more simplified form,
   * which is easier to read on display.  Don't call this during
   * processing, as I think the system assumes that names don't change
   * over time (?).
   **/
  public void simplify_expression() {
    if (debug_simplify_expression)
      System.out.println("** Simplify: " + name);

    if (!isDerived()) {
      if (debug_simplify_expression)
	System.out.println("** Punt because not derived variable");
      return;
    }

    // find a ...post(...)... expression to simplify
    VarInfoName.Poststate postexpr = null;
    Iterator nodes = (new VarInfoName.InorderFlattener(name)).nodes().iterator();
    while (nodes.hasNext()) {
      Object node = nodes.next();
      if (node instanceof VarInfoName.Poststate) {
	postexpr = (VarInfoName.Poststate) node;
	break;
      }
    }
    if (postexpr == null) {
      if (debug_simplify_expression) System.out.println("** Punt because no post()");
      return;
    }

    // if we have post(...+k) rewrite as post(...)+k
    if (postexpr.term instanceof VarInfoName.Add) {
      VarInfoName.Add add = (VarInfoName.Add) postexpr.term;
      VarInfoName swapped = add.term.applyPoststate().applyAdd(add.amount);
      name = (new VarInfoName.Replacer(postexpr, swapped)).replace(name);
      // start over
      simplify_expression();
      return;
    }

    // [[ find the ppt context for the post() term ]] (I used to
    // search the expression for this, but upon further reflection,
    // there is only one EXIT point which could possibly be associated
    // with this VarInfo, so "this.ppt" must be correct.
    PptTopLevel post_context = this.ppt;

    // see if the contents of the post(...) have an equivalent orig()
    // expression.
    VarInfo postvar = post_context.findVar(postexpr.term);
    if (postvar == null) {
      if (debug_simplify_expression)
	System.out.println("** Punt because no VarInfo for postvar " + postexpr.term);
      return;
    }
    VarInfoName pre_expr = postvar.preStateEquivalent();
    if (pre_expr != null) {
      // strip off any orig() so we don't get orig(a[orig(i)])
      if (pre_expr instanceof VarInfoName.Prestate) {
	pre_expr = ((VarInfoName.Prestate) pre_expr).term;
      } else if (pre_expr instanceof VarInfoName.Add) {
	VarInfoName.Add add = (VarInfoName.Add) pre_expr;
	if (add.term instanceof VarInfoName.Prestate) {
	  pre_expr = ((VarInfoName.Prestate) add.term).term.applyAdd(add.amount);
	}
      }
      name = (new VarInfoName.Replacer(postexpr, pre_expr)).replace(name);
      if (debug_simplify_expression)
	System.out.println("** Replaced with: " + name);
    }

    if (debug_simplify_expression)
      System.out.println("** Nothing to do (no state equlivalent)");
  }

  /**
   * Two variables are "compatible" if their declared types are castable
   * and their comparabilities are comparable.
   **/
  public boolean compatible(VarInfo var2) {
    VarInfo var1 = this;
    if (Daikon.check_program_types
        && (! var1.type.castable(var2.type))) {
      return false;
    }
    if ((! Daikon.ignore_comparability)
        && (! VarComparability.comparable(var1, var2))) {
      return false;
    }
    return true;
  }

  /**
   * Return true if this sequence variable's element type is compatible
   * with the scalar variable.
   **/
  public boolean eltsCompatible(VarInfo sclvar) {
    VarInfo seqvar = this;
    if (Daikon.check_program_types) {
      if (! seqvar.type.elementType().castable(sclvar.type)) {
        return false;
      }
    }
    if (! Daikon.ignore_comparability) {
      if (! VarComparability.comparable(seqvar.comparability.elementType(),
                                        sclvar.comparability)) {
        return false;
      }
    }
    return true;
  }

  /**
   * Return true if this sequence's first index type is compatible
   * with the scalar variable.
   **/
  public boolean indexCompatible(VarInfo sclvar) {
    VarInfo seqvar = this;
    if (Daikon.check_program_types) {
      if (! (seqvar.type.isPseudoArray()
             && sclvar.isIndex())) {
        return false;
      }
    }
    if (! Daikon.ignore_comparability) {
      if (! VarComparability.comparable(seqvar.comparability.indexType(0),
                                        sclvar.comparability)) {
        return false;
      }
    }
    return true;
  }


  // Interning is lost when an object is serialized and deserialized.
  // Manually re-intern any interned fields upon deserialization.
  private void readObject(ObjectInputStream in)
    throws IOException, ClassNotFoundException
  {
    in.defaultReadObject();
    name = name.intern();
  }

}
