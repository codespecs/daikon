package daikon;


import daikon.derive.*;
import daikon.derive.unary.*;
import daikon.derive.binary.*;
import daikon.derive.ternary.*;
import daikon.VarInfoName.*;
import daikon.inv.*;
import daikon.inv.unary.scalar.NonZero;
import daikon.inv.binary.twoScalar.*;
import utilMDE.*;
import java.util.logging.Logger;
import java.util.logging.Level;

import java.util.*;
import java.io.*;

/**
 * Represents information about a particular variable for a program
 * point.  This object doesn't hold the value of the variable at a
 * particular step of the program point, but can get the value it
 * holds when given a ValueTuple using the getValue() method.  VarInfo
 * also includes info about the variable's name, its declared type, its
 * file representation type, its internal type, and its comparability.
 **/
public final class VarInfo
  implements Cloneable, Serializable
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020629L;

  /** Debug missing vals. **/
  public static final Logger debugMissing
                                = Logger.getLogger("daikon.VarInfo.missing");

  /**
   * The program point this variable is in.
   **/
  public PptTopLevel ppt;

  /**
   * Name.  Do not compare names of invariants from different program
   * points, because two different program points could contain unrelated
   * variables named "x".
   **/
  public VarInfoName name;      // interned

  public String name() {
    return (name.name());
  }

  /** Type as declared in the program. **/
  public ProglangType type;      // interned

  /**
   * Type as written in the data trace file.  This is an interface
   * detail.
   **/
  public ProglangType file_rep_type;      // interned

  /**
   * Type as internally stored.  This is an interface detail and is never
   * visible to clients.
   * @see ProglangType#fileTypeToRepType()
   **/
  public ProglangType rep_type;      // interned

  /** Comparability info. **/
  public VarComparability comparability;

  /** Auxiliary info. **/
  public VarInfoAux aux;

  // Obtaining values

  /** The index in lists of VarInfo objects. **/
  public int varinfo_index;

  /**
   * The index in a ValueTuple (more generally, in a list of values).
   * It can differ from varinfo_index due to
   * constants (and possibly other factors).
   * It is -1 iff is_static_constant or not yet set.
   **/
  public int value_index;

  /**
   * is_static_constant iff (value_index == -1);
   * is_static_constant == (static_constant_value != null).
   **/
  public boolean is_static_constant;

  /** Null if not statically constant. **/
  Object static_constant_value;

  // Partial ordering relationships between variables.
  // If A is higher than B then every value seen at B is seen at A.
  // Use mixed-type rep to save space.

  private Object po_higher; // either null, VarInfo, or VarInfo[] with no duplicates
  private Object po_lower;  // either null, VarInfo, or VarInfo[] with no duplicates
  private int[] po_higher_nonce; // null iff po_higher is null, else length == po_higher().size()

  /**
   * Index of this variable in the global ppt, -1 if there is no transform
   * to the global ppt for this variable.
   */
  public short global_index = -1;

  // Derived variables

  /** Whether and how derived.  Null if this is not derived. **/
  public Derivation derived;

  /**
   * Returns whether or not we have encountered to date any missing values
   * due to array indices being out of bounds.  This can happen with both
   * subscripts and subsequences.  Note that this becomes true as we are
   * running, it cannot be set in advance without a first pass.
   *
   * This is used as we are processing data to destroy any invariants
   * that use this variable.
   *
   * @see Derivation#missingOutOfBounds()
   **/
  public boolean missingOutOfBounds() {
    if (derived != null) {
      if (derived.missingOutOfBounds())
        return (true);
      if (Daikon.dkconfig_df_bottom_up)
        return (false);
      if (po_lower == null)
        return (false);
      if (po_lower instanceof VarInfo) {
        VarInfo lower = (VarInfo) po_lower;
        if (lower.missingOutOfBounds()) {
          debugMissing.fine ("Var " + lower.ppt.name() + " "
                              + lower.name.name()
                              + " out of bounds implies " + ppt.name() + " "
                              + name.name() + " out of bounds. ");
          derived.missing_array_bounds = true; // don't force us to recalc this
          return (true);
        }
        return (false);
      }
      VarInfo[] vis = (VarInfo[]) po_lower;
      for (int i = 0; i < vis.length; i++) {
        if (vis[i].missingOutOfBounds()) {
          debugMissing.fine ("Var " + vis[i].ppt.name() + " "
                              + vis[i].name.name()
                              + " out of bounds implies " + ppt.name() + " "
                              + name.name() + " out of bounds. ");
          derived.missing_array_bounds = true; // don't force us to recalc this
          return (true);
        }
      }
    } else
      return (false);
    return (false);
  }

  public boolean canBeMissing = false;

  /**
   * Which equality group this belongs to.  Replaces equal_to.  Never null
   * after this is put inside equalitySet.
   **/
  public Equality equalitySet;

  VarInfo sequenceSize;         // if null, not yet computed (or this VarInfo
                                //   is not a sequence)

  // DO NOT TEST EQUALITY!  ONLY USE ITS .name SLOT!!   -MDE 3/9/2001
  public VarInfo postState;     // non-null if this is an orig() variable

  /**
   * @exception RuntimeException if representation invariant on this is broken
   */
  public void checkRep() {
    Assert.assertTrue(ppt != null);
    Assert.assertTrue(name != null);
    Assert.assertTrue(name == name.intern());
    Assert.assertTrue(type != null);
    Assert.assertTrue(file_rep_type != null);
    Assert.assertTrue(rep_type != null);
    Assert.assertTrue(comparability != null); // anything else ??
    Assert.assertTrue(0 <= varinfo_index && varinfo_index < ppt.var_infos.length);
    Assert.assertTrue(-1 <= value_index && value_index < varinfo_index);
    Assert.assertTrue(is_static_constant == (value_index == -1));
    Assert.assertTrue(is_static_constant || (static_constant_value == null));
    if (po_higher == null) {
      Assert.assertTrue(po_higher_nonce == null);
    } else if (po_higher instanceof VarInfo) {
      Assert.assertTrue(po_higher_nonce != null);
      Assert.assertTrue(po_higher_nonce.length == 1);
    } else {
      Assert.assertTrue(po_higher instanceof VarInfo[]);
      VarInfo[] ary = (VarInfo[]) po_higher;
      Assert.assertTrue(! ArraysMDE.any_null(ary));
      Assert.assertTrue(po_higher_nonce != null);
      Assert.assertTrue(po_higher_nonce.length == ary.length);
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
            // The below types are converted into one of the rep types
            // by ProglangType.fileTypeToRepType().
            || (file_rep_type == ProglangType.HASHCODE)
            || (file_rep_type == ProglangType.HASHCODE_ARRAY)
            || ((file_rep_type.dimensions() <= 1)
                && file_rep_type.baseIsPrimitive())
            );
  }

  public VarInfo(VarInfoName name, ProglangType type, ProglangType file_rep_type,
                 VarComparability comparability, boolean is_static_constant,
                 Object static_constant_value, VarInfoAux aux) {
    Assert.assertTrue(file_rep_type != null);
    Assert.assertTrue(legalFileRepType(file_rep_type),
                  "Unsupported representation type " + file_rep_type.format()
                  + " for variable " + name);
    // Ensure that the type and rep type are somewhat consistent
    Assert.assertTrue(type.pseudoDimensions() >= file_rep_type.dimensions(),
                  "Types dimensions incompatibility: " + type + " vs. " + file_rep_type);
    Assert.assertTrue(aux != null);

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
    this.aux = aux;

    if (debug.isLoggable(Level.FINE)) {
      debug.fine ("Var " + name + " aux: " + aux);
    }

    // Indicates that these haven't yet been set to reasonable values.
    value_index = -1;
    varinfo_index = -1;

    canBeMissing = false;
  }

  public VarInfo(VarInfoName name, ProglangType type, ProglangType file_rep_type,
                 VarComparability comparability, VarInfoAux aux) {
    this(name, type, file_rep_type, comparability, false, null, aux);
  }

  public VarInfo(VarInfo vi) {
    this(vi.name, vi.type, vi.file_rep_type, vi.comparability,
         vi.is_static_constant, vi.static_constant_value, vi.aux);
    canBeMissing = vi.canBeMissing;
    postState = vi.postState;
    equalitySet = vi.equalitySet;
  }


  /** Create the prestate, or "orig()", version of the variable. **/
  public static VarInfo origVarInfo(VarInfo vi) {
    // At an exit point, parameters are uninteresting, but orig(param) is not.
    // So don't call orig(param) a parameter.
    VarInfoAux aux_nonparam = vi.aux.setValue(VarInfoAux.IS_PARAM,
                                              VarInfoAux.FALSE);

    VarInfoName newname = vi.name.applyPrestate();
    VarInfo result = new VarInfo(newname,
                                 vi.type, vi.file_rep_type,
                                 vi.comparability.makeAlias(vi.name),
                                 aux_nonparam);
    result.canBeMissing = vi.canBeMissing;
    result.postState = vi;
    result.equalitySet = vi.equalitySet;
    return result;
  }

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
      a_new[i].ppt = null;
    }
    // I need to fix both of these slots:
    //   public Derivation derived;     // whether (and how) derived
    //   public Vector derivees;        // vector of Derivation objects
    HashMap deriv_map = new HashMap();
    for (int i=0; i<a_new.length; i++) {
      Derivation deriv_old = a_old[i].derived;
      if (deriv_old != null) {
        Derivation deriv_new = deriv_old.switchVars(a_old, a_new);
        deriv_map.put(deriv_old, deriv_new);
        a_new[i].derived = deriv_new;
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
      if (a_old[i].derived != null)
        Assert.assertTrue (a_new[i].derived != null);
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

  /** Trims the collections used by this VarInfo. */
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
   * @see #po_lower()
   * @see #po_higher_nonce()
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
   * @see #po_higher()
   **/
  public int[] po_higher_nonce() {
    return po_higher_nonce; // writable; oh well
  }

  /**
   * @return read-only collection over variables immediately lower in
   * the partial order (compared to this).  Not a view (sees no updates).
   * If A is higher than B then every value seen at B is seen at A.
   * Elements are VarInfos.  Contains no duplicates.
   * @see #po_higher()
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
   * @return read-only int[] giving the nonces for po_lower()
   *
   * (This is an expensive operation, but we don't cache the result,
   * since this is only called from daikon.Dataflow, which is only
   * used during decls file setup.)
   *
   * @see #po_lower()
   **/
  public int[] po_lower_nonce() {
    List lo = po_lower();
    int[] result = new int[lo.size()];
    for (int i = 0; i < result.length; i++) {
      VarInfo lower = (VarInfo) lo.get(i);
      List hi = lower.po_higher();
      int index = hi.indexOf(this);
      Assert.assertTrue(index >= 0);
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
                          int nonce) {
    VarInfo lower = this;

    if (lower == higher) {
      System.err.println ("Error, lower != higher in:");
      System.err.println ("   lower: " + lower.name.name());
      System.err.println ("  higher: " + higher.name.name());
      System.err.println ("     ppt: " + this.ppt.name());
      Assert.assertTrue(lower != higher, "lower != higher");

    }

    if (Dataflow.debugInit.isLoggable(Level.FINE))
      Dataflow.debugInit.fine ("addHigherPO " + higher.name.name()
                                + " to " + name.name());

    // We remove this assertion because it could be that A has a member a
    // of type A, so A::this should be < A::this.a.  The only thing we want
    // to prevent is cycles, so the first assertion above is sufficient.
    // Presumably, if the above situation happens, the transformers will
    // handle recursing properly.
    // Assert.assertTrue(lower.ppt != higher.ppt, "lower.ppt != higher.ppt");


    Assert.assertTrue(lower.type == higher.type, "lower.type == higher.type");
    Assert.assertTrue(lower.rep_type == higher.rep_type, "lower.rep_type == higher.rep_type");
    Assert.assertTrue(lower.file_rep_type == higher.file_rep_type, "lower.file_rep_type == higher.file_rep_type");

    boolean already = lower.po_higher().contains(higher);
    Assert.assertTrue(already == higher.po_lower().contains(lower));
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
      lower.po_higher = po_new;
      lower.po_higher_nonce = po_new_nonce;
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
      higher.po_lower = po_new;
    }
  }

  /**
   * @param lower true iff the closure is over lower elements
   * @return BFS iterator that is stable (it returns unordered
   * elements in the same order each time)
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
    Assert.assertTrue(! result.contains(this));
    return Collections.unmodifiableList(result).iterator();
  }

  public String toString() { return repr(); }

  /** Helper function for repr(). **/
  private Object checkNull (Object o) {
    return (o == null) ? "null" : o;
  }

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
      + ",derived=" + checkNull(derived)
      + ",derivees=" + derivees()
      + ",ppt=" + ppt.name()
      + ",canBeMissing=" + canBeMissing
      + (",equal_to=" + (equalitySet==null ? "null" : equalitySet.toString()))
      + ",isCanonical()=" + isCanonical()
      + ">";
  }

  public boolean isStaticConstant() {
    return is_static_constant;
  }
  public Object constantValue() {
    if (isStaticConstant()) {
      return static_constant_value;
    } else {
      throw new Error("Variable " + name + " is not constant");
    }
  }

  // Returns true if this is an "orig()" variable
  public boolean isPrestate() {
    return postState != null;
  }

  public boolean isPrestateDerived() {
    if (postState != null)
      return true;
    return name.isAllPrestate();
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

  /** Return all derived variables that build off this one. **/
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


  /**
   * Cached value for getDerivedParam().
   **/
  public VarInfo derivedParamCached = null;

  /**
   * Cached value for isDerivedParam().
   **/
  // Boolean rather than boolean so we can use "null" to indicate "not yet set".
  public Boolean isDerivedParamCached = null;

  /**
   * Returns true if this is a param according to aux info, or this is
   * a front end derivation such that one of its bases is a param.  To
   * figure this out, what we do is get all the param variables at
   * this's program point.  Then we search in this's name to see if
   * the name contains any of the variables.  We have to do this
   * because we only have name info, and we assume that x and x.a are
   * related from the names alone.
   * Effects: Sets isDerivedParamCached and derivedParamCached to
   * values the first time this method is called.  Subsequent calls
   * use these cached values.
   **/
  public boolean isDerivedParam() {
    if (isDerivedParamCached != null) return isDerivedParamCached.booleanValue();

    boolean result = false;
    if (aux.getFlag(VarInfoAux.IS_PARAM) && !isPrestate())
      result = true;

    Set paramVars/*VarInfoName*/ = ppt.getParamVars();

    VarInfoName.Finder finder = new VarInfoName.Finder(paramVars);
    Object baseMaybe = finder.getPart(name);
    if (baseMaybe != null) {
      VarInfoName base = (VarInfoName) baseMaybe;
      derivedParamCached = this.ppt.findVar(base);
      if (Global.debugSuppressParam.isLoggable(Level.FINE)) {
        Global.debugSuppressParam.fine (name.name() + " is a derived param");
        Global.debugSuppressParam.fine ("derived from " + base.name());
        Global.debugSuppressParam.fine (paramVars.toString());
      }
      result = true;
    }

    isDerivedParamCached = result ? Boolean.TRUE : Boolean.FALSE;
    return result;
  }


  /**
   * Return a VarInfo that has two properties: this is a derivation of
   * it, and it is a parameter variable.  If this is a parameter, then
   * this is returned.  For example, "this" is always a parameter.
   * The return value of getDerivedParam for "this.a" (which is not a
   * parameter) is "this".
   * Effects: Sets isDerivedParamCached and derivedParamCached to
   * values the first time this method is called.  Subsequent calls
   * use these cached values.
   * @return null if the above condition doesn't hold.
   **/
  public VarInfo getDerivedParam() {
    if (isDerivedParamCached == null) {
      isDerivedParam();
    }
    return derivedParamCached;
  }


  private Boolean isDerivedParamAndUninterestingCached = null;

  /**
   * Returns true if a given VarInfo is a parameter or derived from
   * one in such a way that changes to it wouldn't be visible to the
   * method's caller. There are 3 such cases:
   *
   * <li> The variable is a pass-by-value parameter "p".
   * <li> The variable is of the form "p.prop" where "prop" is an
   * immutable property of an object, like its type, or (for a Java
   * array) its size.
   * <li> The variable is of the form "p.prop", and "p" has been
   * modified to point to a different object. We assume "p" has been
   * modified if we don't have an invariant "orig(p) == p".
   *
   * In any case, the variable must have a postState VarInfoName, and
   * equality invariants need to have already been computed.
   **/
  public boolean isDerivedParamAndUninteresting() {
    // if (isDerivedParamAndUninterestingCached != null) {
    //  PrintInvariants.debugFiltering.fine ("\t\t\tusing cached " + isDerivedParamAndUninterestingCached.toString() + "\n");
    //  return isDerivedParamAndUninterestingCached.booleanValue();
    // }
    if (isDerivedParamAndUninterestingCached != null) {
      return isDerivedParamAndUninterestingCached.booleanValue();
    } else {
      isDerivedParamAndUninterestingCached =
        _isDerivedParamAndUninteresting() ? Boolean.TRUE : Boolean.FALSE;
      return isDerivedParamAndUninterestingCached.booleanValue();
    }
  }



  private boolean _isDerivedParamAndUninteresting() {
    if (PrintInvariants.debugFiltering.isLoggable(Level.FINE)) {
      PrintInvariants.debugFiltering.fine ("isDPAU: name is " + name.name() + "\n");
      PrintInvariants.debugFiltering.fine ("  isPrestate is " + String.valueOf(isPrestate()) + "\n");
      PrintInvariants.debugFiltering.fine ("  name is prestate " + String.valueOf(name instanceof VarInfoName.Prestate) + "\n");
    }

    if (isPrestate() || name instanceof VarInfoName.Prestate) {
      // The second part of the || is needed because derived variables
      // don't match to their orig() values.
      return false;
    }

    if (aux.getFlag(VarInfoAux.IS_PARAM)) {
      PrintInvariants.debugFiltering.fine ("  not interesting, IS_PARAM == true for " + name.name() + "\n");
      return true;
    }
    if (Global.debugSuppressParam.isLoggable(Level.FINE)) {
      Global.debugSuppressParam.fine ("Testing isDerivedParamAndUninteresting for: " + name.name());
      Global.debugSuppressParam.fine (aux.toString());
      Global.debugSuppressParam.fine ("At ppt " + ppt.name());
    }
    if (isDerivedParam()) {
      // I am uninteresting if I'm a derived param from X and X's
      // type or X's size, because these things are boring if X
      // changes (the default for the rest of the code here), and
      // boring if X stays the same (because it's obviously true).
      if (name instanceof VarInfoName.TypeOf) {
        VarInfoName base = ((VarInfoName.TypeOf) name).term;
        VarInfo baseVar = ppt.findVar(base);
        if (baseVar != null && baseVar.aux.getFlag(VarInfoAux.IS_PARAM)) {
          Global.debugSuppressParam.fine ("TypeOf returning true");
          PrintInvariants.debugFiltering.fine ("  not interesting, first dpf case\n");
          return true;
        }
      }
      if (name instanceof VarInfoName.SizeOf) {
        VarInfoName base = ((VarInfoName.SizeOf) name).sequence.term;
        VarInfo baseVar = ppt.findVar(base);
        if (baseVar != null && baseVar.aux.getFlag(VarInfoAux.IS_PARAM)) {
          Global.debugSuppressParam.fine ("SizeOf returning true");
          PrintInvariants.debugFiltering.fine ("  not interesting, second dpf case\n");
          return true;
        }
      }


      VarInfo base = getDerivedParam();
      // Actually we should be getting all the derivations that could
      // be params, and if any of them are uninteresting, this is
      // uninteresting.

      // Remember that if this is derived from a true param, then this
      // is a param too, so we don't need to worry.  However, if this
      // is derived from a derivedParam, then we need to find all
      // derivation parents that could possibly fail under these
      // rules.  Right now, we just get the first one.

      // So if x = Foo(this.y, p.y) and this hasn't changed then we
      // will be ignoring the fact that y has changed.

      // Henceforth only interesting if it's true that base = orig(base)
      if (base.name.name().equals("this")) return false;
      Global.debugSuppressParam.fine ("Base is " + base.name.name());
      VarInfo origBase = ppt.findVar(base.name.applyPrestate());
      if (origBase == null) {
        Global.debugSuppressParam.fine ("No orig variable for base, returning true ");
        PrintInvariants.debugFiltering.fine ("  not interesting, no orig variable for base\n");
        return true; // There can't be an equal invariant without orig
      }
      if (base.isEqualTo(origBase)) {
        Global.debugSuppressParam.fine ("Saw equality.  Derived worth printing.");
        return false;
      } else {
         Global.debugSuppressParam.fine ("Didn't see equality in base, so uninteresting");
         PrintInvariants.debugFiltering.fine ("  didn't see equality in base\n");
        return true;
      }
      //       PptSlice2 slice = ppt.findSlice_unordered (base, origBase);
      //       if (slice == null) {
      //         Global.debugSuppressParam.fine ("No slice for equality in base, so uninteresting");
      //         PrintInvariants.debugFiltering.fine ("  equal inv in null slice\n");
      //         return true; // There can't be an equal invariant in a null slice
      //       }
      //       if (Global.debugSuppressParam.isLoggable(Level.FINE)) {
      //         Global.debugSuppressParam.fine ("Parent and orig slice for finding equality: " + slice.name());
      //       }
      //       boolean seenEqual = false;
      //       for (Iterator iInvs = slice.invs.iterator(); iInvs.hasNext(); ) {
      //         Invariant sliceInv = (Invariant) iInvs.next();
      //         if (IsEqualityComparison.it.accept(sliceInv)) seenEqual = true;
      //       }
      //       if (!seenEqual) {
      //         Global.debugSuppressParam.fine ("Didn't see equality in base, so uninteresting");
      //         PrintInvariants.debugFiltering.fine ("  didn't see equality in base\n");
      //         return true;
      //       }


    } else {
      Global.debugSuppressParam.fine ("  Not a derived param.");
    }
    return false;
  }


  /** Convenience methods that return information from the ValueTuple. **/
  public int getModified(ValueTuple vt) {
    if (is_static_constant)
      // return ValueTuple.STATIC_CONSTANT;
      return ValueTuple.MODIFIED;
    else
      return vt.getModified(value_index);
  }
  public boolean isUnmodified(ValueTuple vt) {
    return ValueTuple.modIsUnmodified(getModified(vt));
  }
  public boolean isModified(ValueTuple vt) {
    return ValueTuple.modIsModified(getModified(vt));
  }
  public boolean isMissingNonsensical(ValueTuple vt) {
    return ValueTuple.modIsMissingNonsensical(getModified(vt));
  }
  public boolean isMissingFlow(ValueTuple vt) {
    return ValueTuple.modIsMissingFlow(getModified(vt));
  }
  public boolean isMissing(ValueTuple vt) {
    return isMissingNonsensical(vt) || isMissingFlow(vt);
  }
  /**
   * Get the value of this variable from a particular sample (ValueTuple).
   * @param vt the ValueTuple from which to extract the value
   **/
  public Object getValue(ValueTuple vt) {
    if (is_static_constant)
      return static_constant_value;
    else
      return vt.getValue(value_index);
  }

  public int getIndexValue(ValueTuple vt) {
    Object raw = getValue(vt);
    if (raw == null) {
      throw new Error("getIndexValue: getValue returned null " + this.name.name() + " index=" + this.varinfo_index + " vt=" + vt);
    }
    return ((Long)raw).intValue();
  }

  public long getIntValue(ValueTuple vt) {
    Object raw = getValue(vt);
    if (raw == null) {
      throw new Error("getIntValue: getValue returned null " + this.name.name() + " index=" + this.varinfo_index + " vt=" + vt);
    }
    return ((Long)raw).longValue();
  }

  // Retrieve a non-null array.
  public long[] getIntArrayValue(ValueTuple vt) {
    Object raw = getValue(vt);
    if (raw == null) {
      throw new Error("getIntArrayValue: getValue returned null " + this.name.name() + " index=" + this.varinfo_index + " vt=" + vt);
    }
    return (long[])raw;
  }

  public double getDoubleValue(ValueTuple vt) {
    Object raw = getValue(vt);
    if (raw == null) {
      throw new Error("getDoubleValue: getValue returned null " + this.name.name() + " index=" + this.varinfo_index + " vt=" + vt);
    }
    return ((Double)raw).doubleValue();
  }

  // Retrieve a non-null array.
  public double[] getDoubleArrayValue(ValueTuple vt) {
    Object raw = getValue(vt);
    if (raw == null) {
      throw new Error("getDoubleArrayValue: getValue returned null " + this.name.name() + " index=" + this.varinfo_index + " vt=" + vt);
    }
    return (double[])raw;
  }

  public String getStringValue(ValueTuple vt) {
    return (String) getValue(vt);
  }

  // Retrieve a non-null array.
  public String[] getStringArrayValue(ValueTuple vt) {
    Object raw = getValue(vt);
    if (raw == null) {
      throw new Error("getDoubleArrayValue: getValue returned null " + this.name.name() + " index=" + this.varinfo_index + " vt=" + vt);
    }
    return (String[]) raw;
  }

  static final class UsesVarFilter implements Filter {
    VarInfo var;
    public UsesVarFilter(VarInfo var) { this.var = var; }
    public boolean accept(Object o) { return ((Invariant) o).usesVar(var); }
  }

//   Iterator invariants() {
//     // This assertion will need to be relaxed eventually.
//     Assert.assertTrue(ppt instanceof PptTopLevel,
//                       "Ppt " + ppt + " is not instanceof PptTopLevel");
//     // Could alternately have used ppt.invs.lookup(vi).
//     // In fact, that's better, because it doesn't look at so many variables.
//     Iterator all_invs = ((PptTopLevel) ppt).invariants();
//     return new UtilMDE.FilteredIterator(all_invs, new UsesVarFilter(this));
//   }

  /**
   * Whether this VarInfo is the leader of its equality set.
   **/
  public boolean isCanonical() {
    if (!Daikon.use_equality_optimization) return true;
    if (equalitySet == null)
      System.out.println ("var " + name.name() + " in ppt " + ppt.name());
    return (equalitySet.leader() == this);
    }

  /**
   * Canonical representative that's equal to this variable.
   **/
  public VarInfo canonicalRep() {
    if (equalitySet == null) {
      System.out.println ("equality sets = " + ppt.equality_sets_txt());
      Assert.assertTrue (equalitySet != null, "Variable " + name.name()
                     + " in ppt " + ppt.name() + " index = " + varinfo_index);
    }
    return equalitySet.leader();
  }

  /**
   * Return true if this is a pointer or reference to another object.
   **/
  public boolean is_reference() {
    // If the program type has a higher dimension than the rep type,
    // we are taking a hash or something.
    if (type.pseudoDimensions() > rep_type.pseudoDimensions()) {
      return true;
    }

    // The dimensions are the same.  If the rep type is integral but
    // the program type isn't primitive, we have a hash, too.
    if (rep_type.baseIsIntegral() && (!type.baseIsPrimitive())) {
      return true;
    }

    return false;
  }

  /** Return true if the two lists represent the same variables. **/
  // Dead code as of 6/2002 (and probably much earlier).
  static boolean isSimilarVarInfo(VarInfo[] vis1, VarInfo[] vis2) {
    if (vis1.length != vis2.length)
      return false;

    for (int i=0; i<vis1.length; i++)
      if (!vis1[i].isSimilarVarInfo(vis2[i]))
        return false;

    return true;
  }

  /**
   * Return true if the argument VarInfo represents the same variable as
   * this.  Used when combining multiple program points into one, to find
   * the common variables.
   **/
  // simplistic implementation, just checks that the names are the same
  boolean isSimilarVarInfo(VarInfo other) {
    if (this.name != other.name)
      return false;
    Assert.assertTrue(type.equals(other.type), "type matches");
    Assert.assertTrue(file_rep_type.equals(other.file_rep_type),
                  "file_rep_type matches (" +
                  name + ":" + file_rep_type + "," +
                  other.name + ":" + other.file_rep_type +
                  ")");
    // One of the VarInfos might be at a program point with more variables,
    // so the list of variables to which it is comparable could be larger.
    // Assert.assertTrue(comparability.equals(other.comparability));
    return true;
  }

  /**
   * Returns the VarInfo for the sequence from which this was derived,
   * or null if this wasn't derived from a sequence.
   * Only works for scalars.
   **/
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
    return ((derived != null)
            && ((derived instanceof SequenceMax)
                || (derived instanceof SequenceMin)
                || (derived instanceof SequenceSum)));
  }

  /**
   * Return the original sequence variable from which this derived sequence
   * was derived.
   * Only works for sequences.
   **/
  public VarInfo isDerivedSubSequenceOf() {
    // System.out.println("isDerivedSubSequenceOf(" + name + "); derived=" + derived);

    if (derived == null)
      return null;

    if (derived instanceof SequenceScalarSubsequence) {
      SequenceScalarSubsequence sss = (SequenceScalarSubsequence) derived;
      // System.out.println("isDerivedSubSequenceOf returning " + sss.seqvar().name);
      return sss.seqvar();
    } else if (derived instanceof SequenceScalarArbitrarySubsequence) {
      SequenceScalarArbitrarySubsequence ssas =
        (SequenceScalarArbitrarySubsequence) derived;
      return ssas.seqvar();
    } else {
      return null;
    }
  }

  public VarInfo sequenceSize() {
    if (sequenceSize != null)
      return sequenceSize;
    Assert.assertTrue(rep_type.isArray());
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
//      System.out.println("Warning: Size variable " + search + " not found.");
//      System.out.print("Variables: ");
//      for (int i=0; i<ppt.var_infos.length; i++) {
//        VarInfo vi = ppt.var_infos[i];
//        System.out.print(vi.name + " ");
//      }
//      System.out.println();
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

  /**
   * @return false if this variable expression is not legal ESC
   * syntax, except for any necessary quantifications (subscripting).
   * We err on the side of returning true, for now.
   **/
  public boolean isValidEscExpression() {
    // "myVector.length" is invalid
    boolean is_length = (derived instanceof SequenceLength);
    boolean is_array_length = is_length && ((SequenceLength) derived).base.type.isArray();
    if (is_length && (! is_array_length)) {
      return false;
    }

    // "myVector[]" is invalid, as is myVector[foo]
    for (Iterator i = name.inOrderTraversal().iterator(); i.hasNext(); ) {
      Object next = i.next();
      if (next instanceof VarInfoName.Elements) {
        VarInfoName.Elements elems = (VarInfoName.Elements) next;
        VarInfo seq = ppt.findVar(elems.term);
        if (! seq.type.isArray()) {
          return false;
        }
      }
    }

    return true;
  }

  /**
   * Return true if invariants about this quantity are really
   * properties of a pointer, but derived variables can refer to
   * properties of the thing pointed to. This distinction is important
   * when making logical statements about the object, because in the
   * presence of side effects, the pointed-to object can change even
   * when the pointer doesn't. For instance, we might have "obj ==
   * orig(obj)", but "obj.color != orig(obj.color)". In such a case,
   * isPointer() would be true of obj, and for some forms of output
   * we'd need to translate "obj == orig(obj)" into something like
   * "location(obj) == location(orig(obj))".
   */
  public boolean isPointer() {
    // This used to check whether the program type had a higher
    // dimension than the rep type, or if the rep type was integral
    // but the program type wasn't primitive. These rules worked
    // pretty well for Java, but not so well for C, where for instance
    // you might have rep_type = int and type = size_t.

    return file_rep_type.isPointerFileRep();
  }

  /**
   * A wrapper around VarInfoName.simplify_name() that also uses
   * VarInfo information to guess whether "obj" should logically be
   * treated as just the hash code of "obj", rather than the whole
   * object.
   **/
  public String simplifyFixup(String str) {
    if (isPointer()) {
      str = "(hash " + str + ")";
    }
    return str;
  }

  public String simplifyFixedupName() {
    return simplifyFixup(name.simplify_name());
  }

  ///////////////////////////////////////////////////////////////////////////
  /// IOA functions
  ///

  /** Return true if declared type is Set (IOA syntax). **/
  public boolean isIOASet() {
    return type.base().startsWith("Set");// && type.base().indexOf('(') >= 0;
  }

  /** Return true if declared type is Set (IOA syntax). **/
  public boolean isIOAArray() {
    return type.base().startsWith("Array");//  && type.base().indexOf('(') >= 0;
  }

  /** Return declared element type (in string) of IOA Set or Array. **/
  public String elementTypeIOA() {
    String result;
    int begin;
    int end = type.base().indexOf(')');
    if (this.isIOASet()) {
      begin = type.base().indexOf('(') + 1;
    } else if (this.isIOAArray()) {
      begin = type.base().indexOf(',') + 2;
    } else {
      return null;
    }
    return type.base().substring(begin, end);
  }

  /** Return declared domain type (in string) of an IOA Array. **/
  public String domainTypeIOA() {
    String result;
    if (this.isIOAArray()) {
      int begin = type.base().indexOf('(');
      int end = type.base().indexOf(',');
      result = type.base().substring(begin+1, end);
      daikon.inv.Invariant.debugPrint.fine ("domainTypeIOA: " + result);
      return result;
    } else {
      // Always the same domain otherwise
      return "Int";
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  /// Utility functions
  ///

  // Where do these really belong?


  /**
   *  Given two variables I and J, indicate whether it is necessarily the
   *  case that i<=j or i>=j.  The variables also each have a shift, so the
   *  test can really be something like (i+1)<=(j-1).
   *  The test is either:  i + i_shift <= j + j_shift (if test_lessequal)
   *                       i + i_shift >= j + j_shift (if !test_lessequal)
   *  This is a dynamic check, and so must not be called while Daikon is
   *  inferencing.
   **/
  public static boolean compare_vars(VarInfo vari, int vari_shift, VarInfo varj, int varj_shift, boolean test_lessequal) {
    Assert.assertTrue (!Daikon.isInferencing);
    // System.out.println("compare_vars(" + vari.name + ", " + vari_shift + ", "+ varj.name + ", " + varj_shift + ", " + (test_lessequal?"<=":">=") + ")");
    if (vari == varj) {
      // same variable
      return (test_lessequal
              ? (vari_shift <= varj_shift)
              : (vari_shift >= varj_shift));
    }
    // different variables
    Assert.assertTrue(vari.ppt == varj.ppt);
    PptSlice indices_ppt = vari.ppt.findSlice_unordered(vari, varj);
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
        Assert.assertTrue(lb.core.b == b_int);
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

  /**
   * Return some variable in the other state (pre-state if this is
   * post-state, or vice versa) that equals this one, or null if no equal
   * variable exists.
   **/
  public VarInfoName otherStateEquivalent(boolean post) {

    // Below is equivalent to:
    // Assert.assertTrue(post == isPrestate());
    if (post != isPrestate()) {
      throw new Error("Shouldn't happen (should it?): "
                      + (post ? "post" : "pre") + "StateEquivalent(" + name + ")");
    }

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

  /**
   * Check if two VarInfos are truly (non guarded) equal to each other
   * right now.
   **/
  public boolean isEqualTo (VarInfo other) {
    Assert.assertTrue (equalitySet != null);
    return this.equalitySet == other.equalitySet;
  }


  /** Debug tracer. **/
  private static final Logger debug = Logger.getLogger("daikon.VarInfo");


  /** Debug tracer for simplifying expressions. **/
  private static final Logger debugSimplifyExpression = Logger.getLogger("daikon.VarInfo.simplifyExpression");

  /**
   * Change the name of this VarInfo by side effect into a more simplified
   * form, which is easier to read on display.  Don't call this during
   * processing, as I think the system assumes that names don't change
   * over time (?).
   **/
  public void simplify_expression() {
    if (debugSimplifyExpression.isLoggable(Level.FINE))
      debugSimplifyExpression.fine ("** Simplify: " + name);

    if (!isDerived()) {
      if (debugSimplifyExpression.isLoggable(Level.FINE))
        debugSimplifyExpression.fine ("** Punt because not derived variable");
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
      if (debugSimplifyExpression.isLoggable(Level.FINE))
        debugSimplifyExpression.fine ("** Punt because no post()");
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
      if (debugSimplifyExpression.isLoggable(Level.FINE))
        debugSimplifyExpression.fine ("** Punt because no VarInfo for postvar " + postexpr.term);
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
      if (debugSimplifyExpression.isLoggable(Level.FINE))
        debugSimplifyExpression.fine ("** Replaced with: " + name);
    }

    if (debugSimplifyExpression.isLoggable(Level.FINE))
      debugSimplifyExpression.fine ("** Nothing to do (no state equlivalent)");
  }

  /**
   * Two variables are "compatible" if their declared types are
   * castable and their comparabilities are comparable.  This is a
   * reflexive relationship, because it calls
   * ProglangType.comparableOrSuperclassEitherWay.  However, it is not
   * transitive because it might not hold for two children of a
   * superclass, even though it would for each child and the superclass.
   **/
  public boolean compatible(VarInfo var2) {
    VarInfo var1 = this;
    // Can only compare in the same ppt because otherwise
    // comparability info may not make sense.
    Assert.assertTrue (var1.ppt == var2.ppt);
    if (!comparable2Way (var2)) {
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
      if (! seqvar.type.elementType().comparableOrSuperclassEitherWay(sclvar.type)) {
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
   * Without using comparability info, check that this is comparable
   * to var2.  This is a reflexive relationship, because it calls
   * ProglangType.comparableOrSuperclassEitherWay.  However, it is not
   * transitive because it might not hold for two children of a
   * superclass, even though it would for each child and the
   * superclass.  Does not check comparabilities.
   **/
  public boolean comparable2Way (VarInfo var2) {
    VarInfo var1 = this;
    if (Daikon.check_program_types
        && (! var1.type.comparableOrSuperclassEitherWay(var2.type))) {
      return false;
    }
    if (Daikon.check_program_types
        && (var1.file_rep_type != var2.file_rep_type)) {
      return false;
    }
    return true;
  }

  /**
   * Without using comparability info, check that this is comparable
   * to var2.  This is a reflexive and transitive relationship.  Does
   * not check comparabilities.
   **/
  public boolean comparableNWay (VarInfo var2) {
    VarInfo var1 = this;
    if (Daikon.check_program_types
        && (! var1.type.comparableOrSuperclassOf(var2.type))) {
      return false;
    }
    if (Daikon.check_program_types
        && (! var2.type.comparableOrSuperclassOf(var1.type))) {
      return false;
    }
    if (Daikon.check_program_types
        && (var1.file_rep_type != var2.file_rep_type)) {
      return false;
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

  /**
   * It is <b>not</b> safe in general to compare based on VarInfoName
   * alone, because it is possible for two different program points to have
   * unrelated variables of the same name.
   **/
  public static class LexicalComparator implements Comparator {
    public int compare(Object o1, Object o2) {
      VarInfoName name1 = ((VarInfo)o1).name;
      VarInfoName name2 = ((VarInfo)o2).name;
      return name1.compareTo(name2);
    }
  }


  /**
   * Create a guarding predicate for this VarInfo, that is, an
   * invariant that ensures that this object is available for access
   * to variables that reference it, such as fields.
   **/
  public Invariant createGuardingPredicate() {
    // Later for the array, make sure index in bounds
    if (type.isArray() || type.isObject()) {
      // For now associating with the variable's PptSlice
      PptSlice associateWith = ppt.get_or_instantiate_slice(this);

      Invariant prevInstantiation;
      try {
        prevInstantiation = Invariant.find(Class.forName("daikon.inv.unary.scalar.NonZero"),
                                                     associateWith);
      } catch (ClassNotFoundException e) {
        throw new Error("Could not locate class object for daikon.inv.unary.scalar.NonZero");
      }

      Invariant retval;

      // Check whether the predicate already exists
      if (prevInstantiation == null) {
        // If it doesn't, create a "fake" invariant, which should
        // never be printed.  Is it a good idea even to set
        // retval.falsified to true?  We know it's true because
        // retval's children were missing.  However, some forms of
        // filtering might remove it from associateWith.
        retval = NonZero.instantiate(associateWith);
        retval.isGuardingPredicate = true;
        associateWith.addInvariant(retval);
      } else {
        retval = prevInstantiation;
      }

      return retval;
    } else {
      System.err.println("Unexpected guarding based on " + name.name());
      Assert.assertTrue(false);
      return null;
    }
  }

  // Finds a list of variables that must be guarded for a VarInfo to
  // be guaranteed to not be missing. The variables are returned in
  // the order in which their guarding preficies are supposed to print
  public List getGuardingList() {
    class GuardingVisitor
      implements Visitor
    {
      public Object visitSimple(Simple o) {
        return takeActionOnDerived(ppt.findVar(o));
      }
      public Object visitSizeOf(SizeOf o) {
        List result = (List)o.sequence.accept(this);
        return result;
      }
      public Object visitFunctionOf(FunctionOf o) {
        List result = (List)o.argument.accept(this);
        return result;
      }
      public Object visitFunctionOfN(FunctionOfN o) {
        List args = o.args;

        List result = (List)((VarInfoName)args.get(0)).accept(this);

        for (int i=1; i<args.size(); i++) {
          result.addAll((List)((VarInfoName)args.get(i)).accept(this));
        }

        return result;
      }
      public Object visitField(Field o) {
        List result = (List)o.term.accept(this);

        VarInfo vi = ppt.findVar(o);
        Invariant.debugGuarding.fine(vi != null ? vi.name + " exists and can "
                                     + (vi.canBeMissing ? "" : "not ")
                                     + "be missing" : o + " does not exist");
        if (vi != null && vi.canBeMissing)
          result.add(ppt.findVar(o.term));

        return result;
      }
      public Object visitTypeOf(TypeOf o) {
        List result = (List)o.term.accept(this);

        VarInfo vi = ppt.findVar(o);
        Invariant.debugGuarding.fine(vi != null ? vi.name + " exists and can "
                                     + (vi.canBeMissing ? "" : "not ")
                                     + "be missing" : o + " does not exist");
        if (vi != null && vi.canBeMissing)
          result.add(ppt.findVar(o.term));

        return result;
      }
      public Object visitPrestate(Prestate o) {
        List result = (List)o.term.accept(this);
        return result;
      }
      public Object visitPoststate(Poststate o) {
        List result = (List)o.term.accept(this);
        return result;
      }
      public Object visitAdd(Add o) {
        List result = (List)o.term.accept(this);
        return result;
      }
      public Object visitElements(Elements o) {
        VarInfo vi = ppt.findVar(o);
        List result = (List)o.term.accept(this);
        result.addAll(takeActionOnDerived(vi));

        Invariant.debugGuarding.fine(vi != null ? vi.name + " exists and can "
                                     + (vi.canBeMissing ? "" : "not ")
                                     + "be missing" : o + " does not exist");
        if (vi != null && vi.canBeMissing)
          result.add(ppt.findVar(o.term));

        return result;
      }
      public Object visitSubscript(Subscript o) {
        List result = (List)o.sequence.accept(this);
        result.addAll((List)o.index.accept(this));

        VarInfo vi = ppt.findVar(o);
        Invariant.debugGuarding.fine(vi != null ? vi.name + " exists and can "
                                     + (vi.canBeMissing ? "" : "not ")
                                     + "be missing" : o + " does not exist");
        if (vi != null && ppt.findVar(o).canBeMissing) {
          result.add(ppt.findVar(o.sequence));
          result.add(ppt.findVar(o.index));
        }

        return result;
      }
      public Object visitSlice(Slice o) {
        List result = (List)o.sequence.accept(this);
        if (o.i != null)
          result.addAll((List)o.i.accept(this));
        if (o.j != null)
          result.addAll((List)o.j.accept(this));

        VarInfo vi = ppt.findVar(o);
        Invariant.debugGuarding.fine(vi != null ? vi.name + " exists and can "
                                     + (vi.canBeMissing ? "" : "not ")
                                     + "be missing" : o + " does not exist");
        if (vi != null && vi.canBeMissing) {
          result.add(ppt.findVar(o.sequence));
          if (o.i != null)
            result.add(ppt.findVar(o.i));
          if (o.j != null)
            result.add(ppt.findVar(o.j));
        }

        return result;
      }
      public List takeActionOnDerived(VarInfo vi) {
        List result = new GuardingVariableList();

        if (vi != null && vi.isDerived()) {
          VarInfo[] bases = vi.derived.getBases();

          for (int i=0; i<bases.length; i++) {
            result.addAll((List)(bases[i].name.accept(this)));
          }
        }
        return result;
      }
    }
    List result = (List)name.accept(new GuardingVisitor());
    if (Invariant.debugGuarding.isLoggable(Level.FINE)) {
      Invariant.debugGuarding.fine ("VarInfo.getGuardingList: ");
      Invariant.debugGuarding.fine ("  for variable " + this.name.name());
      // Invariant.debugGuarding.fine ("        " + this.repr());
      String str = "[ ";
      for (int i = 0; i < result.size(); i++) {
        str += ((VarInfo)result.get(i)).name.name() + " ";
      }
      str += "]";
      Invariant.debugGuarding.fine ("  list is " + str);
    }

    return result;
  }

  /**
   * Compare names by index.
   **/
  public static final class IndexComparator
    implements Comparator, Serializable {
    // This needs to be serializable because Equality invariants keep
    // a TreeSet of variables sorted by theInstance.

    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20031204L;

    private IndexComparator() {
    }

    public int compare(Object o1, Object o2) {
      VarInfo vi1 = (VarInfo) o1;
      VarInfo vi2 = (VarInfo) o2;
      if (vi1.varinfo_index < vi2.varinfo_index) {
        return -1;
      } else if (vi1.varinfo_index == vi2.varinfo_index) {
        return 0;
      } else {
        return 1;
      }
    }

    public static IndexComparator getInstance() {
      return theInstance;
    }

    public static final IndexComparator theInstance = new IndexComparator();
  }

  /**
   * Looks for an OBJECT ppt that corresponds to the type of this
   * variable.  Returns null if such a point is not found.
   *
   * @param all_ppts    map of all program points
   */
  public PptTopLevel find_object_ppt (PptMap all_ppts) {

    // Pseudo arrays don't have types
    if (type.isPseudoArray())
      return (null);

    // build the name of the object ppt based on the variable type
    String type_str = type.base().replaceFirst ("\\$", ".");
    PptName objname = new PptName (type_str, null, FileIO.object_suffix);
    return (all_ppts.get (objname));
  }

  /**
   * Class used to contain a pair of VarInfos and their sample count.
   * Currently used for equality set merging as a way to store pairs
   * of equal variables.  The variable with the smaller index is
   * always stored first.
   *
   * Pairs are equal if both of their VarInfos are identical.  Note
   * that the content of the VarInfos are not compared, only their
   * pointer values.
   */
  public static class Pair {

    public VarInfo v1;
    public VarInfo v2;
    public int samples;

    public Pair (VarInfo v1, VarInfo v2, int samples) {
      if (v1.varinfo_index < v2.varinfo_index) {
        this.v1 = v1;
        this.v2 = v2;
      } else {
        this.v1 = v2;
        this.v2 = v1;
      }
      this.samples = samples;
    }

    public boolean equals (Object obj) {
      if (!(obj instanceof Pair))
        return (false);

      Pair o = (Pair) obj;
      return ((o.v1 == v1) && (o.v2 == v2));
    }

    public int hashCode() {
      return (v1.hashCode() + v2.hashCode());
    }

    public String toString() {
      return (v1.name.name() + " = " + v2.name.name());
    }
  }

  /** Returns a string containing the names of the vars in the array. **/
  public static String toString (VarInfo[] vis) {

    if (vis == null)
      return ("null");
    ArrayList/*String*/ vars = new ArrayList(vis.length);
    for (int i = 0; i < vis.length; i++) {
      if (vis[i] == null)
        vars.add("null");
      else
        vars.add(vis[i].name.name());
    }
    return UtilMDE.join(vars, ", ");
  }

  /** Returns a string containing the names of the vars in the list. **/
  public static String toString (List /* VarInfo */ vlist) {

    if (vlist == null)
      return ("null");
    ArrayList/*String*/ vars = new ArrayList(vlist.size());
    for (int i = 0; i < vlist.size(); i++) {
      VarInfo v = (VarInfo) vlist.get(i);
      if (v == null)
        vars.add("null");
      else
        vars.add(v.name.name());
    }
    return UtilMDE.join(vars, ", ");
  }

  /**
   * Returns true if this variable is linked to the global ppt via the
   * orig transform.
   */
  public boolean is_orig_global() {

    if (global_index == -1)
      return (false);

    if (ppt.global_transform_orig[global_var().value_index] == value_index)
      return (true);
    else
      return (false);
  }
  /**
   * Returns true if this variable is linked to the global ppt via the
   * post transform.
   */
  public boolean is_post_global() {

    if (global_index == -1)
      return (false);

    if (ppt.global_transform_post[global_var().value_index] == value_index)
      return (true);
    else
      return (false);
  }

  /**
   * Returns true if this variable is linked to the global ppt via either
   * the orig or post transforms.
   */
  public boolean is_global() {
    return (global_index != -1);
  }

  /**
   * Returns the global variable that corresponds to this one.  Null if
   * there is no transformation
   */
  public VarInfo global_var() {
    if (global_index == -1)
      return (null);
    else
      return (PptTopLevel.global.var_infos[global_index]);
  }

  public ValueSet get_value_set() {

    // Static constants don't have value sets, so we must make one
    if (is_static_constant) {
      ValueSet vs = ValueSet.factory (this);
      vs.add (static_constant_value);
      return (vs);
    }

    return (ppt.value_sets[value_index]);
  }

  public String get_value_info () {
    return name.name() + "- " + get_value_set().repr_short();
  }

}
