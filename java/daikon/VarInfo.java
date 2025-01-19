package daikon;

import static daikon.FileIO.VarDefinition;

import daikon.Quantify.QuantFlags;
import daikon.Quantify.QuantifyReturn;
import daikon.VarInfoName.Add;
import daikon.VarInfoName.Elements;
import daikon.VarInfoName.ElementsFinder;
import daikon.VarInfoName.Field;
import daikon.VarInfoName.FunctionOf;
import daikon.VarInfoName.FunctionOfN;
import daikon.VarInfoName.Poststate;
import daikon.VarInfoName.Prestate;
import daikon.VarInfoName.Simple;
import daikon.VarInfoName.SizeOf;
import daikon.VarInfoName.Slice;
import daikon.VarInfoName.Subscript;
import daikon.VarInfoName.TypeOf;
import daikon.VarInfoName.Visitor;
import daikon.chicory.DaikonVariableInfo;
import daikon.derive.Derivation;
import daikon.derive.binary.SequenceScalarSubscript;
import daikon.derive.binary.SequenceScalarSubsequence;
import daikon.derive.binary.SequenceSubsequence;
import daikon.derive.ternary.SequenceScalarArbitrarySubsequence;
import daikon.derive.unary.SequenceInitial;
import daikon.derive.unary.SequenceLength;
import daikon.derive.unary.SequenceMax;
import daikon.derive.unary.SequenceMin;
import daikon.derive.unary.SequenceSum;
import daikon.inv.Equality;
import daikon.inv.Invariant;
import daikon.inv.OutputFormat;
import daikon.inv.ValueSet;
import daikon.inv.binary.twoScalar.IntGreaterEqual;
import daikon.inv.binary.twoScalar.IntGreaterThan;
import daikon.inv.binary.twoScalar.IntLessEqual;
import daikon.inv.binary.twoScalar.IntLessThan;
import daikon.inv.binary.twoScalar.LinearBinary;
import daikon.inv.unary.scalar.NonZero;
import daikon.inv.unary.scalar.SingleScalar;
import daikon.inv.unary.sequence.EltNonZero;
import daikon.inv.unary.sequence.SingleScalarSequence;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringJoiner;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.checkerframework.checker.formatter.qual.FormatMethod;
import org.checkerframework.checker.interning.qual.Interned;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.EnsuresNonNullIf;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.signedness.qual.PolySigned;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;
import org.plumelib.util.ArraysPlume;

/**
 * Represents information about a particular variable for a program point. This object doesn't hold
 * the value of the variable at a particular step of the program point, but can get the value it
 * holds when given a ValueTuple using the getValue() method. VarInfo also includes info about the
 * variable's name, its declared type, its file representation type, its internal type, and its
 * comparability.
 */
@SuppressWarnings({
  "nullness", // nullness properties in this file are hairy; save for later
  "interning"
})
public final @Interned class VarInfo implements Cloneable, Serializable {
  static final long serialVersionUID = 20060815L;

  /**
   * If true, then variables are only considered comparable if they are declared with the same type.
   * For example, java.util.List is not comparable to java.util.ArrayList and float is not
   * comparable to double. This may miss valid invariants, but significant time can be saved and
   * many variables with different declared types are not comparable (e.g., java.util.Date and
   * java.util.ArrayList).
   */
  public static boolean dkconfig_declared_type_comparability = true;

  /**
   * If true, the treat static constants (such as MapQuick.GeoPoint.FACTOR) as fields within an
   * object rather than as a single name. Not correct, but used to obtain compatibility with
   * VarInfoName.
   */
  public static boolean dkconfig_constant_fields_simplify = true;

  /** Debug missing vals. */
  public static final Logger debugMissing = Logger.getLogger("daikon.VarInfo.missing");

  // Ppts read from version 1 files never have their ppt set.  We should stop supporting version 1
  // files so that the invariants of this class are better maintained.
  /**
   * The program point this variable is in. Is null until set by {@link PptTopLevel#init_vars} and
   * {@link PptTopLevel#addVarInfos}.
   */
  public PptTopLevel ppt;

  /**
   * Name. Do not compare names of invariants from different program points, because two different
   * program points could contain unrelated variables named "x".
   */
  private VarInfoName var_info_name; // interned

  /**
   * Name as specified in the program point declaration. VarInfoName sometimes changes this name as
   * part of parsing so that VarInfoName.name() doesn't return the original name.
   */
  private @Interned String str_name; // interned

  /** returns the interned name of the variable. */
  @Pure
  public @Interned String name(@GuardSatisfied VarInfo this) {
    if (FileIO.new_decl_format) {
      return str_name;
    } else {
      return var_info_name.name().intern(); // vin ok
    }
  }

  /** Returns the original name of the variable from the program point declaration. */
  public @Interned String str_name() {
    return str_name;
  }

  /**
   * Type as declared in the target program. This is seldom used within Daikon as these types vary
   * with program language and the like. It's here more for information than anything else.
   */
  public ProglangType type; // interned (as are all ProglangType objects)

  /**
   * Type as written in the data trace file -- i.e., it is the source variable type mapped into the
   * set of basic types recognized by Daikon. In particular, it includes boolean and hashcode
   * (pointer). This is the type that is normally used when determining if an invariant is
   * applicable to a variable. For example, the less-than invariant is not applicable to booleans or
   * hashcodes, but is applicable to integers (of various sizes) and floats. (In the variable name,
   * "rep" stands for "representation".)
   */
  public ProglangType file_rep_type; // interned (as are all ProglangType objects)

  /**
   * Type as internally stored by Daikon. It contains less information than file_rep_type (for
   * example, boolean and hashcode are both stored as integers). (In the variable name, "rep" stands
   * for "representation".)
   *
   * @see ProglangType#fileTypeToRepType()
   */
  public ProglangType rep_type; // interned (as are all ProglangType objects)

  /** Comparability info. */
  @SuppressWarnings("serial")
  public VarComparability comparability;

  /** Auxiliary info. */
  public VarInfoAux aux;

  /** The index in lists of VarInfo objects. */
  public int varinfo_index;

  /**
   * The index in a ValueTuple (more generally, in a list of values). It can differ from
   * varinfo_index due to constants (and possibly other factors). It is -1 iff is_static_constant or
   * not yet set.
   */
  public int value_index;

  /**
   * Invariants: <br>
   * is_static_constant == (value_index == -1);<br>
   * is_static_constant == (static_constant_value != null).
   */
  // queried via isStaticConstant() method, so consider making this field private
  public boolean is_static_constant;

  /** Null if not statically constant. */
  @SuppressWarnings("serial")
  @Nullable @Interned Object static_constant_value;

  /** Whether and how derived. Null if this is not derived. */
  public @MonotonicNonNull Derivation derived;

  // Various enums used for information about variables
  public enum RefType {
    POINTER,
    OFFSET
  };

  public enum LangFlags {
    PUBLIC,
    PRIVATE,
    PROTECTED,
    STATIC,
    FINAL,
    SYNCHRONIZED,
    VOLATILE,
    TRANSIENT,
    ANNOTATION,
    ENUM
  };

  // These enums are intentionally duplicated in Chicory and other
  // front-ends. These values are written into decl files, and as
  // such, should stay constant between front-ends. They should not be
  // changed without good reason; if you do change them, make sure to
  // also change the corresponding constants in Daikon front ends!
  // Adding a new enum is fine, but should also be done in all locations.
  public enum VarKind {
    FIELD,
    FUNCTION,
    ARRAY,
    VARIABLE,
    RETURN
  };

  public enum VarFlags {
    IS_PARAM,
    NO_DUPS,
    NOT_ORDERED,
    NO_SIZE,
    NOMOD,
    SYNTHETIC,
    CLASSNAME,
    TO_STRING,
    NON_NULL,
    IS_PROPERTY,
    IS_ENUM,
    IS_READONLY
  };

  public @Nullable RefType ref_type;
  public VarKind var_kind;
  public EnumSet<VarFlags> var_flags = EnumSet.noneOf(VarFlags.class);
  public EnumSet<LangFlags> lang_flags = EnumSet.noneOf(LangFlags.class);

  public VarDefinition vardef;

  /**
   * For documentation, see {@link #get_enclosing_var()}. Null if no variable encloses this one --
   * that is, this is not a field of another variable, nor a "method call" like tostring or class.
   */
  public @Nullable VarInfo enclosing_var;

  /** Number of array dimensions (0 or 1). */
  public int arr_dims;

  /**
   * The arguments that were used to create this function application. Null if this variable is not
   * a function application.
   */
  @SuppressWarnings("serial")
  public @MonotonicNonNull List<VarInfo> function_args = null;

  /** Parent program points in ppt hierarchy (optional) */
  @SuppressWarnings("serial")
  public List<VarParent> parents;

  /**
   * The relative name of this variable with respect to its enclosing variable. Field name for
   * fields, method name for instance methods.
   */
  public @Nullable String relative_name = null;

  /**
   * Returns whether or not we have encountered to date any missing values due to array indices
   * being out of bounds. This can happen with both subscripts and subsequences. Note that this
   * becomes true as we are running, it cannot be set in advance without a first pass.
   *
   * <p>This is used as we are processing data to destroy any invariants that use this variable.
   *
   * @see Derivation#missingOutOfBounds()
   */
  public boolean missingOutOfBounds() {
    if ((derived != null) && derived.missingOutOfBounds()) {
      return true;
    }
    return false;
  }

  /**
   * True if a missing/nonsensical value was ever observed for this variable. This starts out false
   * and is set dynamically, while reading the trace file.
   */
  public boolean canBeMissing = false;

  /**
   * Which equality group this belongs to. Replaces equal_to. Never null after this is put inside
   * equalitySet.
   */
  public Equality equalitySet;

  /** Cached value for sequenceSize() */
  private VarInfo sequenceSize;

  /**
   * non-null if this is an orig() variable.
   *
   * <p><b>Do not test equality! Only use its .name slot.</b>
   */
  public @Nullable VarInfo postState;

  /**
   * Throws an exception if this object is malformed. Requires that the VarInfo has been installed
   * into a program point (the {@code ppt} field is set).
   *
   * @exception RuntimeException if representation invariant on this is broken
   */
  public void checkRep() {
    checkRepNoPpt();
    assert ppt != null;
    assert 0 <= varinfo_index && varinfo_index < ppt.var_infos.length;
    assert -1 <= value_index && value_index <= varinfo_index
        : "" + this + " value_index=" + value_index + ", varinfo_index=" + varinfo_index;
    assert is_static_constant == (value_index == -1);
    assert is_static_constant || (static_constant_value == null);
  }

  /**
   * Throws an exception if this object is malformed.
   *
   * <p>Does not require the {@code ppt} field to be set; can be called on VarInfos that have not
   * been installed into a program point.
   *
   * @exception RuntimeException if representation invariant on this is broken
   */
  public void checkRepNoPpt() {
    try {
      assert var_info_name != null; // vin ok
      assert var_info_name == var_info_name.intern(); // vin ok
      assert type != null;
      assert file_rep_type != null;
      assert rep_type != null;
      assert var_kind != null;
      assert comparability != null; // anything else ??
      assert (comparability.alwaysComparable()
              || (((VarComparabilityImplicit) comparability).dimensions
                  == file_rep_type.dimensions()))
          : "Dimensions mismatch for "
              + this
              + ": "
              + ((VarComparabilityImplicit) comparability).dimensions
              + " "
              + file_rep_type.dimensions();
      if ((var_kind == VarKind.FIELD || var_kind == VarKind.ARRAY) && enclosing_var == null) {
        throw new AssertionError(
            "enclosing-var not specified for variable " + var_info_name + " of kind " + var_kind);
      }
    } catch (Throwable e) {
      throw new AssertionError(
          "checkRepNoPpt failed for variable "
              + var_info_name
              + (ppt == null ? "" : (" in " + ppt))
              + ": "
              + repr(),
          e);
    }
  }

  /** Returns whether or not rep_type is a legal type. */
  static boolean legalRepType(ProglangType rep_type) {
    return ((rep_type == ProglangType.INT)
        || (rep_type == ProglangType.DOUBLE)
        || (rep_type == ProglangType.STRING)
        || (rep_type == ProglangType.INT_ARRAY)
        || (rep_type == ProglangType.DOUBLE_ARRAY)
        || (rep_type == ProglangType.STRING_ARRAY));
  }

  /** Returns whether or not constant_value is a legal constant. */
  @EnsuresNonNullIf(result = false, expression = "#1")
  static boolean legalConstant(@Nullable Object constant_value) {
    return ((constant_value == null)
        || (constant_value instanceof Long)
        || (constant_value instanceof Double));
  }

  /**
   * Returns whether or not file_rep_type is a legal file_rep_type. The file_rep_type matches
   * rep_type except that it also allows the more detailed scalar types (HASHCODE, BOOLEAN, etc).
   */
  static boolean legalFileRepType(ProglangType file_rep_type) {
    return (legalRepType(file_rep_type)
        // The below types are converted into one of the rep types
        // by ProglangType.fileTypeToRepType().
        || (file_rep_type == ProglangType.HASHCODE)
        || (file_rep_type == ProglangType.HASHCODE_ARRAY)
        || ((file_rep_type.dimensions() <= 1) && file_rep_type.baseIsPrimitive()));
  }

  /**
   * Create VarInfo from VarDefinition.
   *
   * <p>This does not create a fully initialized VarInfo. For example, its ppt and enclosing_var
   * fields are not yet set. Callers need to do some work to complete the construction of the
   * VarInfo.
   */
  public VarInfo(VarDefinition vardef) {
    vardef.checkRep();

    this.vardef = vardef;

    // Create a VarInfoName from the external name.  This probably gets
    // removed in the long run.
    try {
      var_info_name = VarInfoName.parse(vardef.name); // vin ok
    } catch (Exception e) {
      @SuppressWarnings("nullness") // error case, likely to crash later anyway
      @NonNull VarInfoName vin = null;
      var_info_name = vin;
      System.out.printf("Warning: Can't parse %s as a VarInfoName", vardef.name);
    }
    str_name = vardef.name.intern();

    // Copy info from vardef
    var_kind = vardef.kind;
    relative_name = vardef.relative_name;
    ref_type = vardef.ref_type;
    arr_dims = vardef.arr_dims;
    comparability = vardef.comparability;
    file_rep_type = vardef.rep_type;
    type = vardef.declared_type;
    var_flags = vardef.flags;
    lang_flags = vardef.lang_flags;
    parents = new ArrayList<VarParent>(vardef.parents);

    // If a static constant value was specified, set it
    if (vardef.static_constant_value != null) {
      is_static_constant = true;
      static_constant_value = vardef.static_constant_value;
    } else {
      is_static_constant = false;
    }

    // Create the rep_type from the file rep type
    rep_type = file_rep_type.fileTypeToRepType();

    // Create the VarInfoAux information
    final List<String> auxstrs = new ArrayList<>();
    if (var_flags.contains(VarFlags.IS_PARAM)) {
      auxstrs.add(VarInfoAux.IS_PARAM + "=true");
    }
    if (var_flags.contains(VarFlags.NON_NULL)) {
      auxstrs.add(VarInfoAux.IS_NON_NULL + "=true");
    }
    if (vardef.min_value != null) {
      auxstrs.add(VarInfoAux.MINIMUM_VALUE + "=" + vardef.min_value);
    }
    if (vardef.max_value != null) {
      auxstrs.add(VarInfoAux.MAXIMUM_VALUE + "=" + vardef.max_value);
    }
    if (vardef.min_length != null) {
      auxstrs.add(VarInfoAux.MINIMUM_LENGTH + "=" + vardef.min_length);
    }
    if (vardef.max_length != null) {
      auxstrs.add(VarInfoAux.MAXIMUM_LENGTH + "=" + vardef.max_length);
    }
    if (vardef.valid_values != null) {
      auxstrs.add(VarInfoAux.VALID_VALUES + "=" + vardef.valid_values);
    }
    // Sadly, String.join is only available from Java 8:
    // https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#join-java.lang.CharSequence-java.lang.Iterable-
    final String auxstr = String.join(", ", auxstrs);

    try {
      aux = VarInfoAux.parse(auxstr);
    } catch (Exception e) {
      throw new RuntimeException("unexpected aux error", e);
    }
  }

  /**
   * Finishes defining the variable by relating it to other variables. This cannot be done when
   * creating the variable because the other variables it is related to, may not yet exist.
   * Variables are related to their enclosing variables (for fields, arrays, and functions) and to
   * their parent variables in the PptHierarchy. RuntimeExceptions are thrown if any related
   * variables do not exist.
   */
  public void relate_var() {

    if (vardef == null) {
      return;
    }

    // System.out.printf("enclosing var for %s is %s%n", str_name,
    //                   vardef.enclosing_var);

    // Find and set the enclosing variable (if any)
    if (vardef.enclosing_var_name != null) {
      enclosing_var = ppt.find_var_by_name(vardef.enclosing_var_name);
      if (enclosing_var == null) {
        for (int i = 0; i < ppt.var_infos.length; i++) {
          System.out.printf("var = '%s'%n", ppt.var_infos[i]);
        }
        throw new RuntimeException(
            String.format(
                "enclosing variable '%s' for variable '%s' in ppt '%s' cannot be found",
                vardef.enclosing_var_name, vardef.name, ppt.name));
      }
    }

    // Convert vardef.function_args, which is a list of Strings,
    // into this.function_args, which is a list of VarInfos.
    if (vardef.function_args != null) {
      function_args = new ArrayList<VarInfo>(vardef.function_args.size());
      for (String varname : vardef.function_args) {
        VarInfo vi = ppt.find_var_by_name(varname);
        if (vi == null) {
          throw new RuntimeException(
              String.format(
                  "function argument '%s' for variable '%s'  in ppt '%s' cannot be found",
                  varname, vardef.name, ppt.name));
        }
        function_args.add(vi);
      }
    }

    // do something appropriate with the ppt/var hierarchy.  It may be
    // that  this is better done within PptRelation
  }

  /**
   * Setup information normally specified in the declaration record for derived variables where the
   * new variable is the result of applying a function to the other variables. Much of the
   * information is inferred from (arbitrarily) the first argument to the function.
   *
   * <p>The parent_ppt field is set if each VarInfo in the derivation has the same parent. The
   * parent_variable field is set if there is a parent_ppt and one or more of the bases has a
   * non-default parent variable. The parent variable name is formed as function_name(arg1,arg2,...)
   * where arg1, arg2, etc are the parent variable names of each of the arguments.
   */
  public void setup_derived_function(String name, VarInfo... bases) {

    // Copy variable info from the base.
    // Might some of these need to be overridden later, because they are just guesses?
    VarInfo base = bases[0];
    ref_type = null;
    var_flags = base.var_flags.clone();
    lang_flags = base.lang_flags.clone();
    for (int ii = 1; ii < bases.length; ii++) {
      var_flags.retainAll(bases[ii].var_flags);
      lang_flags.retainAll(bases[ii].lang_flags);
    }
    enclosing_var = null;
    arr_dims = base.arr_dims;
    var_kind = VarKind.FUNCTION;
    function_args = Arrays.asList(bases);

    // Build the string name
    List<String> arg_names = new ArrayList<>();
    for (VarInfo vi : bases) {
      arg_names.add(vi.name());
    }
    str_name = String.format("%s(%s)", name, String.join(",", arg_names)).intern();

    // The parent ppt is the same as the base if each varinfo in the
    // derivation has the same parent
    for (VarParent bp : base.parents) {
      int parent_relation_id = bp.parent_relation_id;
      String parent_ppt = bp.parent_ppt;

      if (allHaveRelation(parent_relation_id)) {
        parents.add(new VarParent(parent_ppt, parent_relation_id, null));
      }
    }

    // If there is a parent_ppt, determine the parent_variable name.
    // If all of the argument names are the default, then the parent_variable
    // is the default as well.  Otherwise, build up the name from the
    // function name and the name of each arguments parent variable name.
    // TWS: the code doesn't appear to handle the case where some are default and some aren't.
    for (VarParent p : parents) {
      boolean parent_vars_specified = false;
      for (VarInfo vi : bases) {
        for (VarParent vp : vi.parents) {
          if (vp.parent_variable != null && vp.parent_relation_id == p.parent_relation_id) {
            parent_vars_specified = true;
          }
        }
      }
      if (!parent_vars_specified) {
        p.parent_variable = null;
      } else {
        StringJoiner args = new StringJoiner(",");
        for (VarInfo vi : bases) {
          boolean found = false;
          for (VarParent vp : vi.parents) {
            if (vp.parent_relation_id == p.parent_relation_id) {
              args.add(vp.parent_variable);
              found = true;
            }
          }
          if (!found) {
            args.add(vi.name());
          }
        }
        p.parent_variable = String.format("%s(%s)", name, args.toString());
      }
    }
  }

  /**
   * Setup information normally specified in the declaration record for derived variables where one
   * of the variables is the base of the derivation. In general this information is inferred from
   * the base variable of the derived variables. Note that parent_ppt is set if each VarInfo in the
   * derivation has the same parent, but parent_variable is not set. This has to be set based on the
   * particular derivation.
   */
  public void setup_derived_base(VarInfo base, @Nullable VarInfo... others) {

    // Copy variable info from the base
    ref_type = base.ref_type;
    var_kind = base.var_kind;
    var_flags = base.var_flags.clone();
    lang_flags = base.lang_flags.clone();
    enclosing_var = base.enclosing_var;
    arr_dims = base.arr_dims;
    function_args = base.function_args;

    // The parent ppt is the same as the base if each varinfo in the
    // derivation has the same parent
    for (VarParent bp : base.parents) {
      int parent_relation_id = bp.parent_relation_id;
      String parent_ppt = bp.parent_ppt;

      if (allHaveRelation(parent_relation_id, others)) {
        parents.add(new VarParent(parent_ppt, parent_relation_id, null));
      }
    }
  }

  /** Returns true if all variables have a parent relation with the specified id. */
  private static boolean allHaveRelation(int parent_relation_id, VarInfo... vs) {
    for (VarInfo vi : vs) {
      if (vi == null) {
        continue;
      }
      boolean hasRelId = false;
      for (VarParent vp : vi.parents) {
        if (parent_relation_id == vp.parent_relation_id) {
          hasRelId = true;
          break;
        }
      }
      if (!hasRelId) {
        return false;
      }
    }
    return true;
  }

  /**
   * Create the specified VarInfo. The resulting VarInfo does not have its ppt field set.
   *
   * @param name the variable name
   * @param type type as declared in the program point
   * @param file_rep_type type as written in the data trace file
   * @param comparability comparability info
   * @param is_static_constant true if the variable always has the same, known value
   * @param static_constant_value the static constant value, or null if not statically constant
   * @param aux auxiliary info
   */
  private VarInfo(
      VarInfoName name,
      ProglangType type,
      ProglangType file_rep_type,
      VarComparability comparability,
      boolean is_static_constant,
      @Nullable @Interned Object static_constant_value,
      VarInfoAux aux) {

    assert name != null;
    assert file_rep_type != null;
    assert legalFileRepType(file_rep_type)
        : "Unsupported representation type "
            + file_rep_type.format()
            + "/"
            + file_rep_type.getClass()
            + " "
            + ProglangType.HASHCODE.getClass()
            + " for variable "
            + name;
    assert type != null;
    assert comparability != null;
    // COMPARABILITY TEST
    // assert
    //   comparability.alwaysComparable() || ((VarComparabilityImplicit)comparability).dimensions ==
    // file_rep_type.dimensions()
    //   : "Types dimensions incompatibility: "
    //     + type
    //     + " vs. "
    //     + file_rep_type;
    assert aux != null;
    assert legalConstant(static_constant_value)
        : "unexpected constant class " + static_constant_value.getClass();

    // Possibly the call to intern() isn't necessary; but it's safest to
    // make the call to intern() rather than running the risk that a caller
    // didn't.
    this.var_info_name = name.intern(); // vin ok
    this.type = type;
    this.file_rep_type = file_rep_type;
    this.rep_type = file_rep_type.fileTypeToRepType();
    arr_dims = rep_type.isArray() ? 1 : 0;
    this.comparability = comparability;
    this.is_static_constant = is_static_constant;
    this.static_constant_value = static_constant_value;
    this.aux = aux;
    this.parents = new ArrayList<VarParent>();

    if (debug.isLoggable(Level.FINE)) {
      debug.fine("Var " + name + " aux: " + aux);
    }

    // Indicates that these haven't yet been set to reasonable values.
    value_index = -1;
    varinfo_index = -1;

    canBeMissing = false;
  }

  /** Create the specified VarInfo. */
  public VarInfo(
      String name,
      ProglangType type,
      ProglangType file_rep_type,
      VarComparability comparability,
      boolean is_static_constant,
      @Nullable @Interned Object static_constant_value,
      VarInfoAux aux) {
    this(
        VarInfoName.parse(name),
        type,
        file_rep_type,
        comparability,
        is_static_constant,
        static_constant_value,
        aux);
    assert name != null;
    this.str_name = name.intern();
  }

  /** Create the specified non-static VarInfo. */
  private VarInfo(
      VarInfoName name,
      ProglangType type,
      ProglangType file_rep_type,
      VarComparability comparability,
      VarInfoAux aux) {
    this(name, type, file_rep_type, comparability, false, null, aux);
  }

  /** Create the specified non-static VarInfo. */
  public VarInfo(
      String name,
      ProglangType type,
      ProglangType file_rep_type,
      VarComparability comparability,
      VarInfoAux aux) {
    this(name, type, file_rep_type, comparability, false, null, aux);
    assert name != null;
    this.str_name = name.intern();
  }

  /** Create a VarInfo with the same values as vi. */
  public VarInfo(VarInfo vi) {
    this(
        vi.name(),
        vi.type,
        vi.file_rep_type,
        vi.comparability,
        vi.is_static_constant,
        vi.static_constant_value,
        vi.aux);
    str_name = vi.str_name;
    canBeMissing = vi.canBeMissing;
    postState = vi.postState;
    equalitySet = vi.equalitySet;
    ref_type = vi.ref_type;
    var_kind = vi.var_kind;
    var_flags = vi.var_flags.clone();
    lang_flags = vi.lang_flags.clone();
    vardef = vi.vardef;
    enclosing_var = vi.enclosing_var;
    arr_dims = vi.arr_dims;
    function_args = vi.function_args;
    parents = new ArrayList<VarParent>();
    for (VarParent parent : vi.parents) {
      parents.add(
          new VarParent(parent.parent_ppt, parent.parent_relation_id, parent.parent_variable));
    }
    relative_name = vi.relative_name;
  }

  // /** Creates and returns a copy of this. */
  // // Default implementation to quiet Findbugs.
  // @SuppressWarnings("interning")  // temporary?
  // public VarInfo clone() throws CloneNotSupportedException {
  //   return (VarInfo) super.clone();
  // }

  /**
   * Create the prestate, or "orig()", version of the variable. Note that the returned value is not
   * completely initialized. The caller is still responsible for setting some fields of it, such as
   * enclosing_var.
   */
  public static VarInfo origVarInfo(VarInfo vi) {
    // At an exit point, parameters are uninteresting, but orig(param) is not.
    // So don't call orig(param) a parameter.
    // VIN (below should be removed)
    // VarInfoAux aux_nonparam =
    //   vi.aux.setValue(VarInfoAux.IS_PARAM, VarInfoAux.FALSE);

    VarInfo result;
    if (FileIO.new_decl_format) {

      // Build a Variable Definition from the poststate vardef
      VarDefinition result_vardef = vi.vardef.copy();
      result_vardef.name = vi.prestate_name();

      // The only hierarchy relation for orig variables is to the enter
      // ppt.  Remove any specified relations.
      result_vardef.clear_parent_relation();

      // Fix the VarDefinition enclosing variable, if any, to point to the
      // prestate version.  This code does not affect the VarInfo yet, but
      // the side-effected VarDefinition will be passed to "new VarInfo".
      if (result_vardef.enclosing_var_name != null) {
        assert vi.enclosing_var != null
            : "@AssumeAssertion(nullness): dependent: result_vardef was copied from vi and their"
                + " enclosing_var fields are the same";
        result_vardef.enclosing_var_name = vi.enclosing_var.prestate_name();
        assert result_vardef.enclosing_var_name != null : "" + result_vardef;
      }

      // Build the prestate VarInfo from the VarDefinition.
      result = new VarInfo(result_vardef);

      // Copy the missing flag from the original variable.  This is necessary
      // for combined exit points which are built after processing is
      // complete.  In most cases the missing flag will be set correctly
      // by merging the missing flag from the numbered exit points.  But
      // this will fail if the method terminates early each time the variable
      // is missing.  A better fix would be to instrument early exits and
      // merge them in as well, but this matches what we did previously.
      result.canBeMissing = vi.canBeMissing;

    } else {
      VarInfoName newname = vi.var_info_name.applyPrestate(); // vin ok
      result =
          new VarInfo(newname, vi.type, vi.file_rep_type, vi.comparability.makeAlias(), vi.aux);
      result.canBeMissing = vi.canBeMissing;
      result.postState = vi;
      result.equalitySet = vi.equalitySet;
      result.arr_dims = vi.arr_dims;
      result.str_name = vi.prestate_name();
    }

    // At an exit point, parameters are uninteresting, but orig(param) is not.
    // So don't call orig(param) a parameter.
    result.set_is_param(false);
    return result;
  }

  /**
   * Given an array of VarInfo objects, return an array of clones, where references to the originals
   * have been modified into references to the new ones (so that the new set is self-consistent).
   * The originals should not be modified by this operation.
   */
  public static VarInfo[] arrayclone_simple(VarInfo[] a_old) {
    int len = a_old.length;
    VarInfo[] a_new = new VarInfo[len];
    for (int i = 0; i < len; i++) {
      a_new[i] = new VarInfo(a_old[i]);
      if (a_old[i].derived != null) {
        assert a_new[i].derived != null;
      }
      a_new[i].varinfo_index = a_old[i].varinfo_index;
      a_new[i].value_index = a_old[i].value_index;
    }
    return a_new;
  }

  /** Trims the collections used by this VarInfo. */
  public void trimToSize() {
    // if (derivees != null) { derivees.trimToSize(); }
    // Derivation derived; probably can't be trimmed
  }

  /**
   * Returns the name of the variable.
   *
   * @see #name
   */
  @SideEffectFree
  @Override
  public String toString(@GuardSatisfied VarInfo this) {
    return name();
  }

  /**
   * Returns the argument or, if it is null, {@code "null"}.
   *
   * @param o a reference
   * @return the argument or, if it is null, {@code "null"}
   */
  private @PolySigned Object checkNull(
      @GuardSatisfied VarInfo this, @Nullable @PolySigned Object o) {
    return (o == null) ? (@PolySigned Object) "null" : o;
  }

  /**
   * Returns a complete string description of the variable.
   *
   * @return a complete string description of the variable
   */
  public String repr() {
    return "<VarInfo "
        + var_info_name // vin ok
        + ": "
        + "type="
        + type
        + ",file_rep_type="
        + file_rep_type
        + ",rep_type="
        + rep_type
        + ",comparability="
        + comparability
        + ",value_index="
        + value_index
        + ",varinfo_index="
        + varinfo_index
        + ",is_static_constant="
        + is_static_constant
        + ",static_constant_value="
        + static_constant_value
        + ",derived="
        + checkNull(derived)
        + ",derivees="
        + derivees()
        + ",ppt="
        // This method is only called for debugging, so let's
        // protect ourselves from a mistake somewhere else, such as ppt being null.
        // + ppt.name()
        + ppt
        + ",canBeMissing="
        + canBeMissing
        + (",equal_to=" + (equalitySet == null ? "null" : equalitySet.toString()))
        + ",PostState="
        + postState
        + ",isCanonical()="
        + isCanonical()
        + ">";
  }

  /** Returns whether or not this variable is a static constant. */
  @EnsuresNonNullIf(
      result = true,
      expression = {"constantValue()", "static_constant_value"})
  @Pure
  public boolean isStaticConstant() {
    return is_static_constant;
  }

  /**
   * Returns the static constant value of this variable. The variable must be a static constant. The
   * result is non-null.
   */
  public Object constantValue() {
    if (isStaticConstant()) {
      assert static_constant_value != null;
      return static_constant_value;
    } else {
      throw new Error("Variable " + name() + " is not constant");
    }
  }

  /** Returns true if this is an "orig()" variable. */
  @EnsuresNonNullIf(result = true, expression = "postState")
  @Pure
  public boolean isPrestate() {
    return postState != null;
  }

  /** Returns true if this variable is derived from prestate variables. */
  @SuppressWarnings("all:not.deterministic") // nondeterminism does not affect result
  @Pure
  public boolean isPrestateDerived() {
    if (postState != null) {
      return true;
    }
    if (isDerived()) {
      for (VarInfo vi : derived.getBases()) {
        if (!vi.isPrestate()) {
          return false;
        }
      }
      return true;
    } else {
      return isPrestate();
    }

    // return name.isAllPrestate();
  }

  /** Returns true if this variable is a derived variable. */
  @EnsuresNonNullIf(result = true, expression = "this.derived")
  @Pure
  public boolean isDerived() {
    return (derived != null);
  }

  /**
   * Returns the depth of derivation.
   *
   * @return the depth of derivation
   */
  public int derivedDepth() {
    if (derived == null) {
      return 0;
    } else {
      return derived.derivedDepth();
    }
  }

  /**
   * Return all derived variables that build off this one.
   *
   * @return all derived variables that build off this one
   */
  public List<Derivation> derivees() {
    ArrayList<Derivation> result = new ArrayList<>();
    // This method is only called from the debugging routine 'repr()'.
    // So let's protect ourselves from a mistake somewhere else.
    if (ppt == null) {
      return result;
    }
    VarInfo[] vis = ppt.var_infos;
    for (int i = 0; i < vis.length; i++) {
      VarInfo vi = vis[i];
      Derivation der = vi.derived;
      if (der == null) {
        continue;
      }
      if (ArraysPlume.indexOf(der.getBases(), this) >= 0) {
        result.add(der);
      }
    }
    return result;
  }

  /**
   * Returns a list of all of the basic (non-derived) variables that are used to make up this
   * variable. If this variable is not derived, it is just this variable. Otherwise it is all of the
   * the bases of this derivation.
   */
  public List<VarInfo> get_all_constituent_vars() {
    List<VarInfo> vars = new ArrayList<>();
    if (isDerived()) {
      for (VarInfo vi : derived.getBases()) {
        vars.addAll(vi.get_all_constituent_vars());
      }
    } else {
      vars.add(this);
    }
    return vars;
  }

  /**
   * Returns a list of all of the simple names that make up this variable. this includes each field
   * and function name in the variable. If this variable is derived it includes the simple names
   * from each of its bases. For example, 'this.item.a' would return a list with 'this', 'item', and
   * 'a' and 'this.theArray[i]' would return 'this', 'theArray' and 'i'.
   */
  public List<String> get_all_simple_names() {
    assert FileIO.new_decl_format;
    List<String> names = new ArrayList<>();
    if (isDerived()) {
      for (VarInfo vi : derived.getBases()) {
        names.addAll(vi.get_all_simple_names());
      }
    } else {
      VarInfo start = (isPrestate() ? postState : this);
      for (VarInfo vi = start; vi != null; vi = vi.enclosing_var) {
        if (relative_name == null) {
          names.add(vi.name());
        } else {
          names.add(vi.relative_name);
        }
      }
    }
    return names;
  }

  @Pure
  public boolean isClosure() {
    // This should eventually turn into
    //   return name.indexOf("closure(") != -1;
    // when I rename those variables to "closure(...)".
    return name().indexOf("~") != -1; // XXX
  }

  /** Cached value for getDerivedParam(). */
  public @Nullable VarInfo derivedParamCached = null;

  /** Cached value for isDerivedParam(). */
  // Boolean rather than boolean so we can use "null" to indicate "not yet set".
  public @MonotonicNonNull Boolean isDerivedParamCached = null;

  /**
   * Returns true if this is a param according to aux info, or this is a front end derivation such
   * that one of its bases is a param. To figure this out, what we do is get all the param variables
   * at this's program point. Then we search in this's name to see if the name contains any of the
   * variables. We have to do this because we only have name info, and we assume that x and x.a are
   * related from the names alone.
   *
   * <p>Effects: Sets isDerivedParamCached and derivedParamCached to values the first time this
   * method is called. Subsequent calls use these cached values.
   */
  @EnsuresNonNullIf(result = true, expression = "getDerivedParam()")
  @SuppressWarnings("all:purity") // created object is not returned
  @Pure
  public boolean isDerivedParam() {
    if (isDerivedParamCached != null) {
      // System.out.printf("var %s is-derived-param = %b%n", name(),
      //                   isDerivedParamCached);
      return isDerivedParamCached.booleanValue();
    }

    boolean result = false;
    if (isParam() && !isPrestate()) {
      result = true;
    }

    if (!FileIO.new_decl_format) {
      // Determine the result from VarInfoName
      Set<VarInfo> paramVars = ppt.getParamVars();
      Set<VarInfoName> param_names = new LinkedHashSet<>();
      for (VarInfo vi : paramVars) {
        param_names.add(vi.var_info_name); // vin ok
      }

      VarInfoName.Finder finder = new VarInfoName.Finder(param_names);
      Object baseMaybe = finder.getPart(var_info_name); // vin ok
      if (baseMaybe != null) {
        VarInfoName base = (VarInfoName) baseMaybe;
        derivedParamCached = this.ppt.find_var_by_name(base.name());
        if (Global.debugSuppressParam.isLoggable(Level.FINE)) {
          Global.debugSuppressParam.fine(name() + " is a derived param");
          Global.debugSuppressParam.fine("derived from " + base.name());
          Global.debugSuppressParam.fine(paramVars.toString());
        }
        result = true;
      }
    } else { // new format
      derivedParamCached = enclosing_param();
      if (derivedParamCached != null) {
        result = true;
      } else if (derived != null) {
        for (VarInfo vi : derived.getBases()) {
          derivedParamCached = vi.enclosing_param();
          if (derivedParamCached != null) {
            result = true;
            break;
          }
        }
      }
    }

    // System.out.printf("var %s is-derived-param = %b%n", name(), result);
    isDerivedParamCached = result ? Boolean.TRUE : Boolean.FALSE;
    return result;
  }

  /**
   * Returns the param variable that encloses this variable (if any). Returns null otherwise. Only
   * valid in the new decl format.
   */
  private @Nullable VarInfo enclosing_param() {
    // System.out.printf("Considering %s%n", this);
    assert FileIO.new_decl_format;
    if (isPrestate()) {
      return postState.enclosing_param();
    }
    for (VarInfo evi = this; evi != null; evi = evi.enclosing_var) {
      // System.out.printf("%s isParam=%b%n", evi, evi.isParam());
      if (evi.isParam()) {
        return evi;
      }
    }
    return null;
  }

  /**
   * Return a VarInfo that has two properties: this is a derivation of it, and it is a parameter
   * variable. If this is a parameter, then this is returned. For example, "this" is always a
   * parameter. The return value of getDerivedParam for "this.a" (which is not a parameter) is
   * "this".
   *
   * <p>Effects: Sets isDerivedParamCached and derivedParamCached to values the first time this
   * method is called. Subsequent calls use these cached values.
   *
   * @return null if the above condition doesn't hold
   */
  @Pure
  public @Nullable VarInfo getDerivedParam() {
    if (isDerivedParamCached == null) {
      // fill in the cache
      isDerivedParam();
    }
    return derivedParamCached;
  }

  private @MonotonicNonNull Boolean isDerivedParamAndUninterestingCached = null;

  /**
   * Returns true if a given VarInfo is a parameter or derived from one in such a way that changes
   * to it wouldn't be visible to the method's caller. There are 3 such cases:
   *
   * <ul>
   *   <li>The variable is a pass-by-value parameter "p".
   *   <li>The variable is of the form "p.prop" where "prop" is an immutable property of an object,
   *       like its type, or (for a Java array) its size.
   *   <li>The variable is of the form "p.prop", and "p" has been modified to point to a different
   *       object. We assume "p" has been modified if we don't have an invariant "orig(p) == p".
   * </ul>
   *
   * In any case, the variable must have a postState VarInfoName, and equality invariants need to
   * have already been computed.
   */
  @SuppressWarnings("all:purity") // set cache field
  @Pure
  public boolean isDerivedParamAndUninteresting() {
    if (isDerivedParamAndUninterestingCached != null) {
      return isDerivedParamAndUninterestingCached.booleanValue();
    } else {
      isDerivedParamAndUninterestingCached =
          _isDerivedParamAndUninteresting() ? Boolean.TRUE : Boolean.FALSE;
      return isDerivedParamAndUninterestingCached.booleanValue();
    }
  }

  /** Implementation of {@link #isDerivedParamAndUninteresting()}. */
  @Pure
  private boolean _isDerivedParamAndUninteresting() {
    if (PrintInvariants.debugFiltering.isLoggable(Level.FINE)) {
      PrintInvariants.debugFiltering.fine("isDPAU: name is " + name());
      PrintInvariants.debugFiltering.fine("  isPrestate is " + String.valueOf(isPrestate()));
    }

    // Orig variables are not considered parameters.  We only check the
    // first variable in a derivation because that is the sequence variable in
    // sequence-subscript or sequence-subsequence derivations and we don't
    // care if the index into the sequence is prestate or not.
    if (isPrestate() || (isDerived() && derived.getBase(0).isPrestate())) {
      return false;
    }

    if (isParam()) {
      PrintInvariants.debugFiltering.fine("  not interesting, IS_PARAM == true for " + name());
      return true;
    }
    if (Global.debugSuppressParam.isLoggable(Level.FINE)) {
      Global.debugSuppressParam.fine("Testing isDerivedParamAndUninteresting for: " + name());
      Global.debugSuppressParam.fine(aux.toString());
      Global.debugSuppressParam.fine("At ppt " + ppt.name());
    }
    if (isDerivedParam()) {
      // I am uninteresting if I'm a derived param from X and X's
      // type or X's size, because these things are boring if X
      // changes (the default for the rest of the code here), and
      // boring if X stays the same (because it's obviously true).
      if (!FileIO.new_decl_format) {
        if (var_info_name instanceof VarInfoName.TypeOf) { // vin ok
          VarInfoName base = ((VarInfoName.TypeOf) var_info_name).term; // vin ok
          VarInfo baseVar = ppt.find_var_by_name(base.name());
          if ((baseVar != null) && baseVar.isParam()) {
            Global.debugSuppressParam.fine("TypeOf returning true");
            PrintInvariants.debugFiltering.fine("  not interesting, first dpf case");
            return true;
          }
        }
        if (var_info_name instanceof VarInfoName.SizeOf) { // vin ok
          VarInfoName base = ((VarInfoName.SizeOf) var_info_name).get_term(); // vin ok
          VarInfo baseVar = ppt.find_var_by_name(base.name());
          if (baseVar != null && baseVar.isParam()) {
            Global.debugSuppressParam.fine("SizeOf returning true");
            PrintInvariants.debugFiltering.fine("  not interesting, second dpf case");
            return true;
          }
        }
      } else { // new decl format
        assert enclosing_var != null : this;
        assert enclosing_var != null : "@AssumeAssertion(nullness)";

        // The class of a parameter can't change in the caller
        if (var_flags.contains(VarFlags.CLASSNAME) && enclosing_var.isParam()) {
          return true;
        }

        // The size of a parameter can't change in the caller.  We shouldn't
        // have the shift==0 test, but need it to match the old code
        if (is_size() && enclosing_var.get_base_array_hashcode().isParam()) {
          if (((SequenceLength) derived).shift == 0) {
            return true;
          }
        }
      }

      VarInfo base = getDerivedParam();
      assert base != null : "can't find base for " + name();
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
      if (base.name().equals("this")) {
        return false;
      }
      Global.debugSuppressParam.fine("Base is " + base.name());
      VarInfo origBase = ppt.find_var_by_name(base.prestate_name());
      if (origBase == null) {
        Global.debugSuppressParam.fine("No orig variable for base, returning true ");
        PrintInvariants.debugFiltering.fine("  not interesting, no orig variable for base");
        return true; // There can't be an equal invariant without orig
      }
      if (base.isEqualTo(origBase)) {
        Global.debugSuppressParam.fine("Saw equality.  Derived worth printing.");
        return false;
      } else {
        Global.debugSuppressParam.fine("Didn't see equality in base, so uninteresting");
        PrintInvariants.debugFiltering.fine("  didn't see equality in base");
        return true;
      }

    } else {
      Global.debugSuppressParam.fine("  Not a derived param.");
    }
    return false;
  }

  /** Convenience methods that return information from the ValueTuple. */
  @Pure
  public int getModified(ValueTuple vt) {
    if (is_static_constant) {
      // return ValueTuple.STATIC_CONSTANT;
      return ValueTuple.MODIFIED;
    } else {
      return vt.getModified(value_index);
    }
  }

  @Pure
  public boolean isUnmodified(ValueTuple vt) {
    return ValueTuple.modIsUnmodified(getModified(vt));
  }

  @Pure
  public boolean isModified(ValueTuple vt) {
    return ValueTuple.modIsModified(getModified(vt));
  }

  @Pure
  public boolean isMissingNonsensical(ValueTuple vt) {
    return ValueTuple.modIsMissingNonsensical(getModified(vt));
  }

  @Pure
  public boolean isMissingFlow(ValueTuple vt) {
    return ValueTuple.modIsMissingFlow(getModified(vt));
  }

  @Pure
  public boolean isMissing(ValueTuple vt) {
    return isMissingNonsensical(vt) || isMissingFlow(vt);
  }

  /**
   * Get the value of this variable from a particular sample (ValueTuple).
   *
   * @param vt the ValueTuple from which to extract the value
   */
  public @Interned Object getValue(ValueTuple vt) {
    if (is_static_constant) {
      @SuppressWarnings("nullness") // derived: is_static_constant == true
      @NonNull Object result = static_constant_value;
      return result;
    } else {
      return vt.getValue(value_index);
    }
  }

  /** Use of this method is discouraged. */
  public @Nullable @Interned Object getValueOrNull(ValueTuple vt) {
    if (is_static_constant) {
      return static_constant_value;
    } else {
      return vt.getValueOrNull(value_index);
    }
  }

  /**
   * Returns the parent relation with the specified parent_relation_id; returns null if the relation
   * is not specified.
   */
  private @Nullable VarParent get_parent(int parent_relation_id) {
    for (VarParent vp : parents) {
      if (vp.parent_relation_id == parent_relation_id) {
        return vp;
      }
    }
    return null;
  }

  /** Returns the parent variable name for the specified parent_relation_id. */
  private String parent_var_name(int parent_relation_id) {
    VarParent parent = get_parent(parent_relation_id);
    if (parent == null) {
      throw new IllegalArgumentException(
          "invalid parent_relation_id " + parent_relation_id + " for variable " + name());
    }
    return parent.parent_variable != null ? parent.parent_variable : name();
  }

  /** Returns true iff the variable has a parent with the given parent_relation_id. */
  private boolean has_parent(int parent_relation_id) {
    return get_parent(parent_relation_id) != null;
  }

  private @Nullable String parent_var(int parent_relation_id) {
    VarParent parent = get_parent(parent_relation_id);
    if (parent == null) {
      throw new IllegalArgumentException(
          "invalid parent_relation_id " + parent_relation_id + " for variable " + name());
    }
    return parent.parent_variable;
  }

  /** Return the value of this long variable (as an integer) */
  public int getIndexValue(ValueTuple vt) {
    Object raw = getValue(vt);
    if (raw == null) {
      throw new Error(
          "getIndexValue: getValue returned null "
              + this.name()
              + " index="
              + this.varinfo_index
              + " vt="
              + vt);
    }
    return ((Long) raw).intValue();
  }

  /** Return the value of this long variable (as a long) */
  public long getIntValue(ValueTuple vt) {
    Object raw = getValue(vt);
    if (raw == null) {
      throw new Error(
          "getIntValue: getValue returned null "
              + this.name()
              + " index="
              + this.varinfo_index
              + " vt="
              + vt);
    }
    return ((Long) raw).longValue();
  }

  /** Return the value of an long[] variable. */
  public long[] getIntArrayValue(ValueTuple vt) {
    Object raw = getValue(vt);
    if (raw == null) {
      throw new Error(
          "getIntArrayValue: getValue returned null "
              + this.name()
              + " index="
              + this.varinfo_index
              + " vt="
              + vt);
    }
    return (long[]) raw;
  }

  /** Return the value of a double variable. */
  public double getDoubleValue(ValueTuple vt) {
    Object raw = getValue(vt);
    if (raw == null) {
      throw new Error(
          "getDoubleValue: getValue returned null "
              + this.name()
              + " index="
              + this.varinfo_index
              + " vt="
              + vt);
    }
    return ((Double) raw).doubleValue();
  }

  /** Return the value of a double[] variable. */
  public double[] getDoubleArrayValue(ValueTuple vt) {
    Object raw = getValue(vt);
    if (raw == null) {
      throw new Error(
          "getDoubleArrayValue: getValue returned null "
              + this.name()
              + " index="
              + this.varinfo_index
              + " vt="
              + vt);
    }
    return (double[]) raw;
  }

  /** Return the value of a String variable. */
  public String getStringValue(ValueTuple vt) {
    return (String) getValue(vt);
  }

  /** Reteurn the value of a String[] array variable. */
  public String[] getStringArrayValue(ValueTuple vt) {
    Object raw = getValue(vt);
    if (raw == null) {
      throw new Error(
          "getDoubleArrayValue: getValue returned null "
              + this.name()
              + " index="
              + this.varinfo_index
              + " vt="
              + vt);
    }
    return (String[]) raw;
  }

  /**
   * Whether this VarInfo is the leader of its equality set.
   *
   * @return true if this VarInfo is the leader of its equality set
   */
  @Pure
  public boolean isCanonical() {
    if (equalitySet == null) {
      return true;
    }
    return (equalitySet.leader() == this);
  }

  /** Canonical representative that's equal to this variable. */
  @Pure
  public VarInfo canonicalRep() {
    if (equalitySet == null) {
      System.out.println("equality sets = " + ppt.equality_sets_txt());
      assert equalitySet != null
          : "Variable " + name() + " in ppt " + ppt.name() + " index = " + varinfo_index;
    }
    return equalitySet.leader();
  }

  /** Return true if this is a pointer or reference to another object. */
  @Pure
  public boolean is_reference() {

    // This used to check to see if the item was a list and some other
    // odd things, but hashcode seems like the right check.
    return rep_type.isHashcode();
  }

  /**
   * Returns the VarInfo for the sequence from which this was derived, or null if this wasn't
   * derived from a sequence. Only works for scalars.
   *
   * @return the VarInfo for the sequence from which this was derived, or null
   */
  public @Nullable VarInfo isDerivedSequenceMember() {
    if (derived == null) {
      return null;
    }

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

  @Pure
  public boolean isDerivedSequenceMinMaxSum() {
    return ((derived != null)
        && ((derived instanceof SequenceMax)
            || (derived instanceof SequenceMin)
            || (derived instanceof SequenceSum)));
  }

  /**
   * Return the original sequence variable from which this derived sequence was derived. Only works
   * for sequences.
   *
   * @return the VarInfo for the original sequence from which this sequence was derived, or null
   */
  public @Nullable VarInfo isDerivedSubSequenceOf() {

    if (derived == null) {
      return null;
    }

    if (derived instanceof SequenceScalarSubsequence) {
      SequenceScalarSubsequence sss = (SequenceScalarSubsequence) derived;
      return sss.seqvar();
    } else if (derived instanceof SequenceScalarArbitrarySubsequence) {
      SequenceScalarArbitrarySubsequence ssas = (SequenceScalarArbitrarySubsequence) derived;
      return ssas.seqvar();
    } else {
      return null;
    }
  }

  /** Returns the variable (if any) that represents the size of this sequence. */
  public @Nullable VarInfo sequenceSize() {
    if (sequenceSize != null) {
      return sequenceSize;
    }
    assert rep_type.isArray();
    // we know the size follows the variable itself in the list
    VarInfo[] vis = ppt.var_infos;
    for (int i = varinfo_index + 1; i < vis.length; i++) {
      VarInfo vi = vis[i];
      if ((vi.derived instanceof SequenceLength) && (((SequenceLength) vi.derived).base == this)) {
        sequenceSize = vi;
        return sequenceSize;
      }
    }
    // It is possible that this VarInfo never had its size derived,
    // since it looked something like this.ary[].field.  In this case,
    // we should return size(this.ary[]), since it was derived and
    // must be the same values.
    if (FileIO.new_decl_format) {
      VarInfo base = get_base_array();
      VarInfo size = ppt.find_var_by_name("size(" + base.name() + ")");
      return size;
    } else {
      VarInfoName search = this.var_info_name; // vin ok
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
      VarInfo result = ppt.find_var_by_name(search.name());
      if (result != null) {
        return result;
        //        } else {
        //      System.out.println("Warning: Size variable " + search + " not found.");
        //      System.out.print("Variables: ");
        //      for (int i = 0; i<ppt.var_infos.length; i++) {
        //        VarInfo vi = ppt.var_infos[i];
        //        System.out.print(vi.name + " ");
        //      }
        //      System.out.println();
      }
    }
    //    throw new Error("Couldn't find size of " + name);
    return null;
  }

  /**
   * Returns true if the type in the original program is integer. Should perhaps check
   * Daikon.check_program_types and behave differently depending on that.
   */
  @Pure
  public boolean isIndex() {
    return (file_rep_type == ProglangType.INT) && type.isIndex();
  }

  /**
   * Returns true if this variable is an array.
   *
   * @return true if this variable is an array
   */
  @Pure
  public boolean is_array() {
    return arr_dims > 0;
  }

  /**
   * Returns false if this variable expression is not legal ESC syntax, except for any necessary
   * quantifications (subscripting). We err on the side of returning true, for now.
   *
   * @return false if this variable expression is not legal ESC syntax, except for any necessary
   *     quantifications (subscripting)
   */
  @Pure
  public boolean isValidEscExpression() {
    // "myList.length" is invalid
    if (derived instanceof SequenceLength) {
      SequenceLength sl = (SequenceLength) derived;
      if (!sl.base.type.isArray()) {
        // VarInfo base = sl.base;
        // System.out.printf("%s is not an array%n", base);
        // System.out.printf("type = %s%n", base.type);
        return false;
      }
    }

    // "myList[]" is invalid, as is myList[foo] (when myList is a list
    // of some sort and not an array)
    if (FileIO.new_decl_format) {
      for (VarInfo vi = this; vi != null; vi = vi.enclosing_var) {
        if (vi.file_rep_type.isArray() && !vi.type.isArray()) {
          return false;
        }
        if (vi.isDerived()) {
          VarInfo base = vi.derived.getBase(0);
          if (base.file_rep_type.isArray() && !base.type.isArray()) {
            return false;
          }
        }
      }
    } else {
      for (VarInfoName next : var_info_name.inOrderTraversal()) { // vin ok
        if (next instanceof VarInfoName.Elements) {
          VarInfoName.Elements elems = (VarInfoName.Elements) next;
          VarInfo seq = ppt.find_var_by_name(elems.term.name());
          if (!seq.type.isArray()) {
            return false;
          }
        }
      }
    }

    return true;
  }

  /**
   * Return true if invariants about this quantity are really properties of a pointer, but derived
   * variables can refer to properties of the thing pointed to. This distinction is important when
   * making logical statements about the object, because in the presence of side effects, the
   * pointed-to object can change even when the pointer doesn't. For instance, we might have "obj ==
   * orig(obj)", but "obj.color != orig(obj.color)". In such a case, isPointer() would be true of
   * obj, and for some forms of output we'd need to translate "obj == orig(obj)" into something like
   * "location(obj) == location(orig(obj))".
   */
  @Pure
  public boolean isPointer() {
    // This used to check whether the program type had a higher
    // dimension than the rep type, or if the rep type was integral
    // but the program type wasn't primitive. These rules worked
    // pretty well for Java, but not so well for C, where for instance
    // you might have rep_type = int and type = size_t.

    return file_rep_type.isPointerFileRep();
  }

  /**
   * A wrapper around VarInfoName.simplify_name() that also uses VarInfo information to guess
   * whether "obj" should logically be treated as just the hash code of "obj", rather than the whole
   * object.
   */
  public String simplifyFixup(String str) {
    if (isPointer()) {
      str = "(hash " + str + ")";
    }
    return str;
  }

  public String simplifyFixedupName() {
    return simplifyFixup(simplify_name());
  }

  // ///////////////////////////////////////////////////////////////////////////
  // Utility functions
  //

  // Where do these really belong?

  /**
   * Given two variables I and J, indicate whether it is necessarily the case that i&le;j or i&ge;j.
   * The variables also each have a shift, so the test can really be something like (i+1)&le;(j-1).
   * The test is one of:
   *
   * <ul>
   *   <li>i + i_shift &le; j + j_shift (if test_lessequal)
   *   <li>i + i_shift &ge; j + j_shift (if !test_lessequal)
   * </ul>
   *
   * This is a dynamic check, and so must not be called while Daikon is inferencing.
   */
  public static boolean compare_vars(
      VarInfo vari, int vari_shift, VarInfo varj, int varj_shift, boolean test_lessequal) {

    // System.out.printf("comparing variables %s and %s in ppt %s%n",
    //        vari.name(), varj.name(), vari.ppt.name());
    // Throwable stack = new Throwable("debug traceback");
    // stack.fillInStackTrace();
    // stack.printStackTrace();

    assert !Daikon.isInferencing;
    // System.out.println("compare_vars(" + vari.name + ", " + vari_shift + ", "+ varj.name + ", " +
    // varj_shift + ", " + (test_lessequal?"<=":">=") + ")");
    if (vari == varj) {
      // same variable
      return test_lessequal ? (vari_shift <= varj_shift) : (vari_shift >= varj_shift);
    }
    // different variables
    boolean samePpt = (vari.ppt == varj.ppt);
    assert samePpt;
    PptSlice indices_ppt = vari.ppt.findSlice_unordered(vari, varj);
    if (indices_ppt == null) {
      return false;
    }

    boolean vari_is_var1 = (vari == indices_ppt.var_infos[0]);
    LinearBinary lb = LinearBinary.find(indices_ppt);
    long index_vari_minus_seq = -2222; // valid only if lb != null
    if (lb != null) {
      if (!lb.enoughSamples()) {
        lb = null;
      } else if (lb.core.a != 1 || lb.core.b != -1) {
        // Do not attempt to deal with anything but y=x+b, aka x-y+b=0.
        lb = null;
      } else {
        // System.out.println("justified LinearBinary: " + lb.format());
        // lb.b is var2()-var1().

        // a is 1 or -1, and the values are integers, so c must be an integer
        long c_int = (long) lb.core.c;
        assert lb.core.c == c_int;
        index_vari_minus_seq = (vari_is_var1 ? -c_int : c_int);
        index_vari_minus_seq += vari_shift - varj_shift;
      }
    }

    boolean vari_lt = false;
    boolean vari_le = false;
    boolean vari_gt = false;
    boolean vari_ge = false;
    {
      IntLessEqual ile = IntLessEqual.find(indices_ppt);
      IntLessThan ilt = IntLessThan.find(indices_ppt);
      IntGreaterEqual ige = IntGreaterEqual.find(indices_ppt);
      IntGreaterThan igt = IntGreaterThan.find(indices_ppt);
      if (ile != null && !ile.enoughSamples()) {
        ile = null;
      }
      if (ilt != null && !ilt.enoughSamples()) {
        ilt = null;
      }
      if (ige != null && !ige.enoughSamples()) {
        ige = null;
      }
      if (igt != null && !igt.enoughSamples()) {
        igt = null;
      }

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
        return ((vari_le && (vari_shift <= varj_shift))
            || (vari_lt && (vari_shift - 1 <= varj_shift)));
      }
    } else {
      if (lb != null) {
        return (index_vari_minus_seq >= 0);
      } else {
        return ((vari_ge && (vari_shift >= varj_shift))
            || (vari_gt && (vari_shift + 1 >= varj_shift)));
      }
    }
  }

  // // takes an "orig()" var and gives a VarInfoName for a variable or
  // // expression in the post-state which is equal to this one.
  // public VarInfoName postStateEquivalent() {
  //   return otherStateEquivalent(true);
  // }

  // takes a non-"orig()" var and gives a VarInfoName for a variable
  // or expression in the pre-state which is equal to this one.
  public @Nullable VarInfoName preStateEquivalent() {
    return otherStateEquivalent(false);
  }

  /**
   * Return some variable in the other state (pre-state if this is post-state, or vice versa) that
   * equals this one, or null if no equal variable exists.
   */
  // This does *not* try the obvious thing of converting "foo" to
  // "orig(foo)"; it creates something new.  I need to clarify the
  // documentation.
  public @Nullable VarInfoName otherStateEquivalent(boolean post) {

    assert !FileIO.new_decl_format;

    // Below is equivalent to:
    // assert post == isPrestate();
    if (post != isPrestate()) {
      throw new Error(
          "Shouldn't happen (should it?): "
              + (post ? "post" : "pre")
              + "StateEquivalent("
              + name()
              + ")");
    }

    {
      List<LinearBinary> lbs = LinearBinary.findAll(this);
      for (LinearBinary lb : lbs) {
        if (this.equals(lb.var2()) && (post != lb.var1().isPrestate())) {

          // a * v1 + b * this + c = 0 or this == (-a/b) * v1 - c/b
          double a = lb.core.a, b = lb.core.b, c = lb.core.c;
          // if (a == 1) {  // match } for vim
          if (-a / b == 1) {
            // this = v1 - c/b
            // int add = (int) b;
            int add = (int) -c / (int) b;
            return lb.var1().var_info_name.applyAdd(add); // vin ok
          }
        }

        if (this.equals(lb.var1()) && (post != lb.var2().isPrestate())) {
          // v2 = a * this + b <-- not true anymore
          // a * this + b * v2 + c == 0 or v2 == (-a/b) * this - c/b
          double a = lb.core.a, b = lb.core.b, c = lb.core.c;
          // if (a == 1) {  // match } for vim
          if (-a / b == 1) {
            // this = v2 + c/b
            // int add = - ((int) b);
            int add = (int) c / (int) b;
            return lb.var2().var_info_name.applyAdd(add); // vin ok
          }
        }
      }

      // Should also try other exact invariants...
    }

    // Can't find post-state equivalent.
    return null;
  }

  /** Check if two VarInfos are truly (non guarded) equal to each other right now. */
  @Pure
  public boolean isEqualTo(VarInfo other) {
    assert equalitySet != null;
    return this.equalitySet == other.equalitySet;
  }

  /** Debug tracer. */
  private static final Logger debug = Logger.getLogger("daikon.VarInfo");

  /** Debug tracer for simplifying expressions. */
  private static final Logger debugSimplifyExpression =
      Logger.getLogger("daikon.VarInfo.simplifyExpression");

  /** Enable assertions that would otherwise reduce run time performance. */
  private static final Logger debugEnableAssertions =
      Logger.getLogger("daikon.VarInfo.enableAssertions");

  // This is problematic because it also enables some debugging output.
  // I need something that only enables assertions.
  // Slightly gross implementation, using a logger; but the command-line
  // options processing code already exists for it:
  // --dbg daikon.VarInfo
  public static boolean assertionsEnabled() {
    return debugEnableAssertions.isLoggable(Level.FINE);
  }

  /**
   * Change the name of this VarInfo by side effect into a more simplified form, which is easier to
   * read on display. Don't call this during processing, as I think the system assumes that names
   * don't change over time (?).
   */
  public void simplify_expression() {
    if (debugSimplifyExpression.isLoggable(Level.FINE)) {
      debugSimplifyExpression.fine("** Simplify: " + name());
    }

    if (!isDerived()) {
      if (debugSimplifyExpression.isLoggable(Level.FINE)) {
        debugSimplifyExpression.fine("** Punt because not derived variable");
      }
      return;
    }

    // find a ...post(...)... expression to simplify
    VarInfoName.Poststate postexpr = null;
    for (VarInfoName node : new VarInfoName.InorderFlattener(var_info_name).nodes()) { // vin ok
      if (node instanceof VarInfoName.Poststate) {
        // Remove temporary var when bug is fixed.
        VarInfoName.Poststate tempNode = (VarInfoName.Poststate) node;
        postexpr = tempNode;
        // old code; reinstate when bug is fixed
        // postexpr = (VarInfoName.Poststate) node;
        break;
      }
    }
    if (postexpr == null) {
      if (debugSimplifyExpression.isLoggable(Level.FINE)) {
        debugSimplifyExpression.fine("** Punt because no post()");
      }
      return;
    }

    // if we have post(...+k) rewrite as post(...)+k
    if (postexpr.term instanceof VarInfoName.Add) {
      VarInfoName.Add add = (VarInfoName.Add) postexpr.term;
      VarInfoName swapped = add.term.applyPoststate().applyAdd(add.amount);
      var_info_name =
          new VarInfoName.Replacer(postexpr, swapped)
              .replace(var_info_name)
              .intern(); // vin ok  // interning bugfix
      // start over
      simplify_expression();
      return;
    }

    // Stop now if we don't want to replace post vars with equivalent orig
    // vars
    if (!PrintInvariants.dkconfig_remove_post_vars) {
      return;
    }

    // [[ find the ppt context for the post() term ]] (I used to
    // search the expression for this, but upon further reflection,
    // there is only one EXIT point which could possibly be associated
    // with this VarInfo, so "this.ppt" must be correct.
    PptTopLevel post_context = this.ppt;

    // see if the contents of the post(...) have an equivalent orig()
    // expression.
    VarInfo postvar = post_context.find_var_by_name(postexpr.term.name());
    if (postvar == null) {
      if (debugSimplifyExpression.isLoggable(Level.FINE)) {
        debugSimplifyExpression.fine("** Punt because no VarInfo for postvar " + postexpr.term);
      }
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
      var_info_name =
          new VarInfoName.Replacer(postexpr, pre_expr)
              .replace(var_info_name)
              .intern(); // vin ok  // interning bugfix
      if (debugSimplifyExpression.isLoggable(Level.FINE)) {
        debugSimplifyExpression.fine("** Replaced with: " + var_info_name); // vin ok
      }
    }

    if (debugSimplifyExpression.isLoggable(Level.FINE)) {
      debugSimplifyExpression.fine("** Nothing to do (no state equlivalent)");
    }
  }

  /**
   * Two variables are "compatible" if their declared types are castable and their comparabilities
   * are comparable. This is a reflexive relationship, because it calls
   * ProglangType.comparableOrSuperclassEitherWay. However, it is not transitive because it might
   * not hold for two children of a superclass, even though it would for each child and the
   * superclass.
   */
  public boolean compatible(VarInfo var2) {
    VarInfo var1 = this;
    // Can only compare in the same ppt because otherwise
    // comparability info may not make sense.
    boolean samePpt = (var1.ppt == var2.ppt);
    assert samePpt;

    if (!comparableByType(var2)) {
      return false;
    }

    if (!Daikon.ignore_comparability && !VarComparability.comparable(var1, var2)) {
      return false;
    }

    return true;
  }

  /**
   * Return true if this sequence variable's element type is compatible with the scalar variable.
   */
  public boolean eltsCompatible(VarInfo sclvar) {
    VarInfo seqvar = this;
    if (Daikon.check_program_types) {
      ProglangType elttype = seqvar.type.elementType();
      if (!elttype.comparableOrSuperclassEitherWay(sclvar.type)) {
        // System.out.printf("eltsCompatible: bad program types; elttype(%s)=%s, scltype(%s)=%s%n",
        //                   seqvar, elttype, sclvar, sclvar.type);
        return false;
      }
    }
    if (!Daikon.ignore_comparability) {
      if (!VarComparability.comparable(seqvar.comparability.elementType(), sclvar.comparability)) {
        // System.out.printf("eltsCompatible: eltcomp(%s;%s)=%s, sclcomp(%s)=%s%n",
        //                   seqvar, seqvar.comparability.elementType(),
        //                   seqvar.comparability.elementType(), sclvar, sclvar.comparability);
        return false;
      }
    }
    return true;
  }

  /**
   * Without using comparability info, check that this is comparable to var2. This is a reflexive
   * relationship, because it calls ProglangType.comparableOrSuperclassEitherWay. However, it is not
   * transitive because it might not hold for two children of a superclass, even though it would for
   * each child and the superclass. Does not check comparabilities.
   */
  public boolean comparableByType(VarInfo var2) {
    VarInfo var1 = this;

    // System.out.printf("comparableByType(%s, %s)%n", var1, var2);

    // the check ensures that a scalar or string and elements of an array of the same type are
    // labelled as comparable
    if (Daikon.check_program_types
        && (var1.file_rep_type.isArray() && !var2.file_rep_type.isArray())) {

      // System.out.printf("comparableByType: case 1 %s%n", var1.eltsCompatible(var2));
      if (var1.eltsCompatible(var2)) {
        return true;
      }
    }

    // the check ensures that a scalar or string and elements of an array of the same type are
    // labelled as comparable
    if (Daikon.check_program_types
        && (!var1.file_rep_type.isArray() && var2.file_rep_type.isArray())) {

      // System.out.printf("comparableByType: case 2 %s%n", var2.eltsCompatible(var1));
      if (var2.eltsCompatible(var1)) {
        return true;
      }
    }

    if (Daikon.check_program_types && (var1.file_rep_type != var2.file_rep_type)) {
      // System.out.printf("comparableByType: case 4 return false%n");
      return false;
    }

    // If the file rep types match then the variables are comparable unless
    // their dimensions are different.
    if (!dkconfig_declared_type_comparability) {
      if (var1.type.dimensions() != var2.type.dimensions()) {
        // debug_print_once ("types %s and %s are not comparable",
        //                    var1.type, var2.type);
        return false;
      }
      return true;
    }

    if (Daikon.check_program_types && !var1.type.comparableOrSuperclassEitherWay(var2.type)) {
      // debug_print_once ("types %s and %s are not comparable",
      //                     var1.type, var2.type);
      return false;
    }
    // debug_print_once ("types %s and %s are comparable",
    //                  var1.type, var2.type);

    // System.out.printf("comparableByType: fallthough return true%n");
    return true;
  }

  /**
   * Without using comparability info, check that this is comparable to var2. This is a reflexive
   * and transitive relationship. Does not check comparabilities.
   *
   * @param var2 the variable to test comparability with
   * @return true if this is comparable to var2
   */
  public boolean comparableNWay(VarInfo var2) {
    VarInfo var1 = this;
    if (Daikon.check_program_types && !var1.type.comparableOrSuperclassOf(var2.type)) {
      return false;
    }
    if (Daikon.check_program_types && !var2.type.comparableOrSuperclassOf(var1.type)) {
      return false;
    }
    if (Daikon.check_program_types && (var1.file_rep_type != var2.file_rep_type)) {
      return false;
    }
    return true;
  }

  /** Return true if this sequence's first index type is compatible with the scalar variable. */
  public boolean indexCompatible(VarInfo sclvar) {
    VarInfo seqvar = this;
    if (Daikon.check_program_types) {
      if (!seqvar.is_array() || !sclvar.isIndex()) {
        return false;
      }
    }
    if (!Daikon.ignore_comparability) {
      if (!VarComparability.comparable(seqvar.comparability.indexType(0), sclvar.comparability)) {
        return false;
      }
    }
    return true;
  }

  // Interning is lost when an object is serialized and deserialized.
  // Manually re-intern any interned fields upon deserialization.
  private void readObject(ObjectInputStream in) throws IOException, ClassNotFoundException {
    in.defaultReadObject();
    var_info_name = var_info_name.intern(); // vin ok
    str_name = str_name.intern();

    for (VarParent parent : parents) {
      parent.parent_ppt = parent.parent_ppt.intern();
      if (parent.parent_variable != null) {
        parent.parent_variable = parent.parent_variable.intern();
      }
    }

    if (relative_name != null) {
      relative_name = relative_name.intern();
    }
  }

  // /**
  //  * It is <b>not</b> safe in general to compare based on VarInfoName
  //  * alone, because it is possible for two different program points to have
  //  * unrelated variables of the same name.
  //  */
  // public static class LexicalComparator implements Comparator<VarInfo> {
  //   @Pure
  //   public int compare(VarInfo vi1, VarInfo vi2) {
  //     VarInfoName name1 = vi1.name;
  //     VarInfoName name2 = vi2.name;
  //     return name1.compareTo(name2);
  //   }
  // }

  // Is this property always guaranteed to be true?  It's placed in a
  // slice, but then might it get printed or treated as true?
  /**
   * Create a guarding predicate for this VarInfo, that is, an invariant that ensures that this
   * object is available for access to variables that reference it, such as fields. (The invariant
   * is placed in the appropriate slice.) Returns null if no guarding is needed.
   */
  // Adding a test against null is not quite right for C programs, where *p
  // could be nonsensical (uninitialized or freed) even when p is non-null.
  // But this is a decent approximation to start with.
  public @Nullable Invariant createGuardingPredicate(boolean install) {
    // Later for the array, make sure index in bounds
    if (!(type.isArray() || type.isObject())) {
      String message =
          String.format("Unexpected guarding based on %s with type %s%n", name(), type);
      System.err.print(message);
      throw new Error(message);
    }

    // For now associating with the variable's PptSlice
    PptSlice slice = ppt.get_or_instantiate_slice(this);

    Invariant result = Invariant.find(NonZero.class, slice);

    // Check whether the predicate already exists
    if (result == null) {
      // If it doesn't, create a "fake" invariant, which should
      // never be printed.  Is it a good idea even to set
      // result.falsified to true?  We know it's true because
      // result's children were missing.  However, some forms of
      // filtering might remove it from slice.
      VarInfo[] vis = slice.var_infos;
      if (SingleScalar.valid_types_static(vis)) {
        result = NonZero.get_proto().instantiate(slice);
      } else if (SingleScalarSequence.valid_types_static(vis)) {
        result = EltNonZero.get_proto().instantiate(slice);
      } else {
        throw new Error("Bad VarInfos");
      }
      if (result == null) {
        // Return null if NonZero invariant is not applicable to this variable.
        return null;
      }
      result.isGuardingPredicate = true;
      // System.out.printf("Created a guarding predicate: %s at %s%n", result, slice);
      // new Error().printStackTrace(System.out);
      if (install) {
        slice.addInvariant(result);
      }
    }

    return result;
  }

  static Set<String> addVarMessages = new HashSet<>();

  /**
   * Finds a list of variables that must be guarded for this VarInfo to be guaranteed to not be
   * missing. This list never includes "this", as it can never be null. The variables are returned
   * in the order in which their guarding prefixes are supposed to print.
   *
   * <p>For example, if this VarInfo is "a.b.c", then the guarding list consists of the variables
   * "a" and "a.b". If "a" is null or "a.b" is null, then "a.b.c" is missing (does not exist).
   *
   * @return a list of varables that must be guarded
   */
  public List<VarInfo> getGuardingList() {

    // The list returned by this visitor always includes the argument itself (if it is testable
    // against null; for example, derived variables are not). If the caller does not want the
    // argument to be in the list, the caller must must remove the argument.

    // Inner class because it uses the "ppt" variable.
    // Basic structure of each visitor:
    //   If the argument should be guarded, recurse.
    //   If the argument is testable against null, add it to the result.
    // Recursing first arranges that the argument goes at the end,
    // after its subparts that need to be guarded.

    class GuardingVisitor implements Visitor<List<VarInfo>> {
      boolean inPre = false;

      private boolean shouldBeGuarded(VarInfoName viname) {
        // Not "shouldBeGuarded(ppt.findVar(viname))" because that
        // unnecessarily computes ppt.findVar(viname), if
        // dkconfig_guardNulls is "always".
        // System.out.printf("viname = %s, applyPreMaybe=%s, findvar=%s%n",
        //                   viname, applyPreMaybe(viname),
        //                   ppt.findVar(applyPreMaybe(viname)));
        if (Daikon.dkconfig_guardNulls == "always") { // interned
          return true;
        }
        if (Daikon.dkconfig_guardNulls == "missing") { // interned
          VarInfo vi = ppt.find_var_by_name(applyPreMaybe(viname).name());
          // Don't guard variables that don't exist.  This happends when
          // we incorrectly parse static variable package names as field names
          if (Invariant.debugGuarding.isLoggable(Level.FINE)) {
            Invariant.debugGuarding.fine(
                String.format(
                    "shouldBeGuarded(%s) [%s] %s %b",
                    viname, applyPreMaybe(viname), vi, ((vi == null) ? false : vi.canBeMissing)));
          }
          if (vi == null) {
            return false;
          }
          return vi.canBeMissing;
        }
        return false;
      }

      @Override
      public List<VarInfo> visitSimple(Simple o) {
        List<VarInfo> result = new ArrayList<>();
        // No recursion:  no children
        if (!o.name.equals("this")) {
          result = addVar(result, o);
        }
        if (Invariant.debugGuarding.isLoggable(Level.FINE)) {
          Invariant.debugGuarding.fine(String.format("visitSimple(%s) => %s", o.name(), result));
        }
        return result;
      }

      @Override
      public List<VarInfo> visitSizeOf(SizeOf o) {
        List<VarInfo> result = new ArrayList<>();
        if (shouldBeGuarded(o)) {
          result.addAll(o.sequence.accept(this));
        }
        // No call to addVar:  derived variable
        if (Invariant.debugGuarding.isLoggable(Level.FINE)) {
          Invariant.debugGuarding.fine(String.format("visitSizeOf(%s) => %s", o.name(), result));
        }
        return result;
      }

      @Override
      public List<VarInfo> visitFunctionOf(FunctionOf o) {
        List<VarInfo> result = new ArrayList<>();
        if (shouldBeGuarded(o)) {
          result.addAll(o.argument.accept(this));
        }
        result = addVar(result, o);
        if (Invariant.debugGuarding.isLoggable(Level.FINE)) {
          Invariant.debugGuarding.fine(
              String.format("visitFunctionOf(%s) => %s", o.name(), result));
        }
        return result;
      }

      @Override
      public List<VarInfo> visitFunctionOfN(FunctionOfN o) {
        List<VarInfo> result = new ArrayList<>();
        if (shouldBeGuarded(o)) {
          for (VarInfoName arg : o.args) {
            result.addAll(arg.accept(this));
          }
        }
        result = addVar(result, o);
        if (Invariant.debugGuarding.isLoggable(Level.FINE)) {
          Invariant.debugGuarding.fine(
              String.format("visitFunctionOfN(%s) => %s", o.name(), result));
        }
        return result;
      }

      @Override
      public List<VarInfo> visitField(Field o) {
        List<VarInfo> result = new ArrayList<>();
        if (Invariant.debugGuarding.isLoggable(Level.FINE)) {
          Invariant.debugGuarding.fine(
              String.format("visitField: shouldBeGuarded(%s) => %s", o.name(), shouldBeGuarded(o)));
        }
        if (shouldBeGuarded(o)) {
          result.addAll(o.term.accept(this));
        }
        result = addVar(result, o);
        if (Invariant.debugGuarding.isLoggable(Level.FINE)) {
          Invariant.debugGuarding.fine(String.format("visitField(%s) => %s", o.name(), result));
        }
        return result;
      }

      @Override
      public List<VarInfo> visitTypeOf(TypeOf o) {
        List<VarInfo> result = new ArrayList<>();
        if (shouldBeGuarded(o)) {
          result.addAll(o.term.accept(this));
        }
        // No call to addVar:  derived variable
        if (Invariant.debugGuarding.isLoggable(Level.FINE)) {
          Invariant.debugGuarding.fine(String.format("visitTypeOf(%s) => %s", o.name(), result));
        }
        return result;
      }

      @Override
      public List<VarInfo> visitPrestate(Prestate o) {
        assert inPre == false;
        inPre = true;
        List<VarInfo> result = o.term.accept(this);
        assert inPre == true;
        inPre = false;
        if (Invariant.debugGuarding.isLoggable(Level.FINE)) {
          Invariant.debugGuarding.fine(String.format("visitPrestate(%s) => %s", o.name(), result));
        }
        return result;
      }

      @Override
      public List<VarInfo> visitPoststate(Poststate o) {
        assert inPre == true;
        inPre = false;
        List<VarInfo> result = o.term.accept(this);
        assert inPre == false;
        inPre = true;
        if (Invariant.debugGuarding.isLoggable(Level.FINE)) {
          Invariant.debugGuarding.fine(String.format("visitPostState(%s) => %s", o.name(), result));
        }
        return result;
      }

      @Override
      public List<VarInfo> visitAdd(Add o) {
        List<VarInfo> result = new ArrayList<>();
        if (shouldBeGuarded(o)) {
          result.addAll(o.term.accept(this));
        }
        // No call to addVar:  derived variable
        if (Invariant.debugGuarding.isLoggable(Level.FINE)) {
          Invariant.debugGuarding.fine(String.format("visitAdd(%s) => %s", o.name(), result));
        }
        return result;
      }

      @Override
      public List<VarInfo> visitElements(Elements o) {
        List<VarInfo> result = new ArrayList<>();
        if (shouldBeGuarded(o)) {
          result.addAll(o.term.accept(this));
        }
        // No call to addVar:  derived variable
        if (Invariant.debugGuarding.isLoggable(Level.FINE)) {
          Invariant.debugGuarding.fine(String.format("visitElements(%s) => %s", o.name(), result));
        }
        return result;
      }

      @Override
      public List<VarInfo> visitSubscript(Subscript o) {
        List<VarInfo> result = new ArrayList<>();
        if (shouldBeGuarded(o)) {
          result.addAll(o.sequence.accept(this));
          result.addAll(o.index.accept(this));
        }
        result = addVar(result, o);
        if (Invariant.debugGuarding.isLoggable(Level.FINE)) {
          Invariant.debugGuarding.fine(String.format("visitSubscript(%s) => %s", o.name(), result));
        }
        return result;
      }

      @Override
      public List<VarInfo> visitSlice(Slice o) {
        List<VarInfo> result = new ArrayList<>();
        if (shouldBeGuarded(o)) {
          result.addAll(o.sequence.accept(this));
          if (o.i != null) {
            result.addAll(o.i.accept(this));
          }
          if (o.j != null) {
            result.addAll(o.j.accept(this));
          }
        }
        // No call to addVar:  derived variable
        if (Invariant.debugGuarding.isLoggable(Level.FINE)) {
          Invariant.debugGuarding.fine(String.format("visitSlice(%s) => %s", o.name(), result));
        }
        return result;
      }

      // Convert to prestate variable name if appropriate
      VarInfoName applyPreMaybe(VarInfoName vin) {
        if (inPre) {
          return vin.applyPrestate();
        } else {
          return vin;
        }
      }

      private List<VarInfo> addVar(List<VarInfo> result, VarInfoName vin) {
        VarInfo vi = ppt.find_var_by_name(applyPreMaybe(vin).name());
        // vi could be null because some variable's prefix is not a
        // variable.  Example: for static variable "Class.staticvar",
        // "Class" is not a varible, even though for variable "a.b.c",
        // typically "a" and "a.b" are also variables.
        if (vi == null) {
          // String message =
          //     String.format(
          //         "getGuardingList(%s, %s): did not find variable %s [inpre=%s]",
          //         name(), ppt.name(), vin.name(), inPre);
          // // Only print the error message at most once per variable.
          // if (addVarMessages.add(vin.name())) {
          //   // For now, don't print at all:  it's generally innocuous
          //   // (class prefix of a static variable).
          //   // System.err.println(message);
          // }
          // // System.out.println("vars: " + ppt.varNames());
          // // System.out.flush();
          // // throw new Error(String.format(message));
          return result;
        } else {
          return addVarInfo(result, vi);
        }
      }

      /**
       * Add the given variable to the result list. Does nothing if the variable is of primitive
       * type.
       */
      // Should this operate by side effect on a global variable?
      // (Then what is the type of the visitor; what does everything return?)
      private List<VarInfo> addVarInfo(List<VarInfo> result, VarInfo vi) {
        assert vi != null;
        assert !vi.isDerived() || vi.isDerived() : "addVar on derived variable: " + vi;
        // Don't guard primitives
        if ( // TODO: ***** make changes here *****
        // vi.file_rep_type.isScalar() &&
        !vi.type.isScalar()
        // (vi.type.isArray() || vi.type.isObject())
        ) {
          result.add(vi);
        } else {
          if (Invariant.debugGuarding.isLoggable(Level.FINE)) {
            Invariant.debugGuarding.fine(
                String.format(
                    "addVarInfo did not add %s: %s (%s) %s (%s)",
                    vi,
                    vi.file_rep_type.isScalar(),
                    vi.file_rep_type,
                    vi.type.isScalar(),
                    vi.type));
          }
        }
        if (Invariant.debugGuarding.isLoggable(Level.FINE)) {
          Invariant.debugGuarding.fine(String.format("addVarInfo(%s) => %s", vi, result));
        }
        return result;
      }
    } // end of class GuardingVisitor

    if (!FileIO.new_decl_format) {
      List<VarInfo> result = var_info_name.accept(new GuardingVisitor()); // vin ok
      result.remove(ppt.find_var_by_name(var_info_name.name())); // vin ok
      assert !ArraysPlume.anyNull(result);
      return result;
    } else { // new format
      List<VarInfo> result = new ArrayList<>();

      if (Daikon.dkconfig_guardNulls == "never") { // interned
        return result;
      }

      // If this is never missing, nothing to guard
      if ((Daikon.dkconfig_guardNulls == "missing") // interned
          && !canBeMissing) {
        return result;
      }

      // Create a list of variables to be guarded from the list of all
      // enclosing variables.
      for (VarInfo vi : get_all_enclosing_vars()) {
        // if (var_flags.contains(VarFlags.CLASSNAME)) {
        //   System.err.printf(
        //       "%s file_rep_type = %s, canbemissing = %b%n", vi, vi.file_rep_type,
        // vi.canBeMissing);
        // }
        if (!vi.file_rep_type.isHashcode()) {
          continue;
        }
        result.add(0, vi);
        if ((Daikon.dkconfig_guardNulls == "missing") // interned
            && !vi.canBeMissing) {
          break;
        }
      }
      return result;
    }
  }

  /**
   * Returns a list of all of the variables that enclose this one. If this is derived, this includes
   * all of the enclosing variables of all of the bases.
   */
  public List<VarInfo> get_all_enclosing_vars() {
    List<VarInfo> result = new ArrayList<>();
    if (isDerived()) {
      for (VarInfo base : derived.getBases()) {
        result.addAll(base.get_all_enclosing_vars());
      }
    } else { // not derived
      for (VarInfo vi = this.enclosing_var; vi != null; vi = vi.enclosing_var) {
        result.add(vi);
      }
    }
    return result;
  }

  /** Compare names by index. */
  public static final class IndexComparator implements Comparator<VarInfo>, Serializable {
    // This needs to be serializable because Equality invariants keep
    // a TreeSet of variables sorted by theInstance.

    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20050923L;

    private IndexComparator() {}

    @Pure
    @Override
    public int compare(VarInfo vi1, VarInfo vi2) {
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
   * Looks for an OBJECT ppt that corresponds to the type of this variable. Returns null if such a
   * point is not found.
   *
   * @param all_ppts map of all program points
   */
  public @Nullable PptTopLevel find_object_ppt(PptMap all_ppts) {

    // Arrays don't have types
    if (is_array()) {
      return null;
    }

    // build the name of the object ppt based on the variable type
    String type_str = type.base().replaceFirst("\\$", ".");
    PptName objname = new PptName(type_str, null, FileIO.object_suffix);
    return all_ppts.get(objname);
  }

  /**
   * Class used to contain a pair of VarInfos and their sample count. Currently used for equality
   * set merging as a way to store pairs of equal variables. The variable with the smaller index is
   * always stored first.
   *
   * <p>Pairs are equal if both of their VarInfos are identical. Note that the content of the
   * VarInfos are not compared, only their pointer values.
   */
  public static class Pair {

    public VarInfo v1;
    public VarInfo v2;
    public int samples;

    public Pair(VarInfo v1, VarInfo v2, int samples) {
      if (v1.varinfo_index < v2.varinfo_index) {
        this.v1 = v1;
        this.v2 = v2;
      } else {
        this.v1 = v2;
        this.v2 = v1;
      }
      this.samples = samples;
    }

    @EnsuresNonNullIf(result = true, expression = "#1")
    @Pure
    @Override
    public boolean equals(@GuardSatisfied Pair this, @GuardSatisfied @Nullable Object obj) {
      if (!(obj instanceof Pair)) {
        return false;
      }

      Pair o = (Pair) obj;
      return (o.v1 == v1) && (o.v2 == v2);
    }

    @Pure
    @Override
    public int hashCode(@GuardSatisfied Pair this) {
      return v1.hashCode() + v2.hashCode();
    }

    @SideEffectFree
    @Override
    public String toString(@GuardSatisfied Pair this) {
      return v1.name() + " = " + v2.name();
    }
  }

  /** Return the set of values that have been seen so far for this variable. */
  public ValueSet get_value_set() {

    // Static constants don't have value sets, so we must make one
    if (is_static_constant) {
      ValueSet vs = ValueSet.factory(this);
      assert static_constant_value != null
          : "@AssumeAssertion(nullness): dependent: is_static_constant";
      vs.add(static_constant_value);
      return vs;
    }

    return ppt.value_sets[value_index];
  }

  public String get_value_info() {
    return name() + "- " + get_value_set().repr_short();
  }

  /**
   * Returns the number of elements in the variable's equality set. Returns 1 if the equality
   * optimization is turned off.
   */
  public int get_equalitySet_size() {
    if (equalitySet == null) {
      return 1;
    } else {
      return equalitySet.size();
    }
  }

  /**
   * Returns the vars_info in the variable's equality set. Returns a set with just itself if the
   * equality optimization is turned off.
   */
  public Set<VarInfo> get_equalitySet_vars() {
    if (equalitySet == null) {
      HashSet<VarInfo> set = new HashSet<>();
      set.add(this);
      return set;
    } else {
      return equalitySet.getVars();
    }
  }

  /**
   * Returns the leader in the variable's equality set. Returns itself if the equality optimization
   * is turned off.
   */
  public VarInfo get_equalitySet_leader() {
    // if (equalitySet == null && VarInfo.use_equality_optimization == false) {  // match } for vim
    if (equalitySet == null) {
      return this;
    } else {
      return equalitySet.leader();
    }
  }

  private static Set<String> out_strings = new LinkedHashSet<>();

  /** If the message is new print it, otherwise discard it. */
  @FormatMethod
  static void debug_print_once(String format, @Nullable Object... args) {
    String msg = String.format(format, args);
    if (!out_strings.contains(msg)) {
      System.out.println(msg);
      out_strings.add(msg);
    }
  }

  /** Returns whether or not this variable is a parameter. */
  @Pure
  public boolean isParam() {
    if (FileIO.new_decl_format) {
      return var_flags.contains(VarFlags.IS_PARAM);
    } else {
      return aux.isParam(); // VIN
    }
  }

  /** Set this variable as a parameter. */
  public void set_is_param() {
    // System.out.printf("setting is_param for %s %n", name());
    if (FileIO.new_decl_format) {
      var_flags.add(VarFlags.IS_PARAM);
    }
    aux = aux.setValue(VarInfoAux.IS_PARAM, VarInfoAux.TRUE); // VIN
  }

  /** Set whether or not this variable is a parameter. */
  public void set_is_param(boolean set) {
    if (set) {
      set_is_param();
    } else {
      if (FileIO.new_decl_format) {
        var_flags.remove(VarFlags.IS_PARAM);
      }
      aux = aux.setValue(VarInfoAux.IS_PARAM, VarInfoAux.FALSE); // VIN
    }
  }

  /**
   * Adds a subscript (or sequence) to an array variable. This should really just just substitute
   * for '..', but the dots are currently removed for back compatability.
   */
  public String apply_subscript(String subscript) {
    if (FileIO.new_decl_format) {
      assert arr_dims == 1 : "Can't apply subscript to " + name();
      return name().replace("..", subscript);
    } else {
      assert name().contains("[]") : "Can't apply subscript to " + name();
      return apply_subscript(name(), subscript);
    }
  }

  /**
   * Adds a subscript (or subsequence) to an array name. This should really just substitute for
   * '..', but the dots are currently removed for back compatibility.
   */
  public static String apply_subscript(String sequence, String subscript) {
    if (FileIO.new_decl_format) {
      return sequence.replace("[..]", "[" + subscript + "]");
    } else {
      return sequence.replace("[]", "[" + subscript + "]");
    }
  }

  /**
   * For array variables, returns the variable that is a simple array. If this variable is a slice,
   * it returns the array variable that is being sliced. If this variable is a simple array itself,
   * returns this.
   */
  public VarInfo get_array_var() {
    assert file_rep_type.isArray();
    if (isDerived()) {
      return derived.get_array_var();
    } else {
      return this;
    }
  }

  /**
   * Returns the VarInfo that represents the base array of this array. For example, if the array is
   * a[].b.c, returns a[].
   */
  @Pure
  public VarInfo get_base_array() {
    assert file_rep_type.isArray() : this;
    if (FileIO.new_decl_format) {
      VarInfo var = this;
      while (var.var_kind != VarKind.ARRAY) {
        if (var.enclosing_var == null) {
          // error condition; print some debugging output before assertion failure
          for (VarInfo vi = this; vi != null; vi = vi.enclosing_var) {
            System.out.printf("%s %s%n", vi, vi.var_kind);
          }
          assert var.enclosing_var != null : this + " " + var;
        }
        assert var.enclosing_var != null : "@AssumeAssertion(nullness): just tested";
        var = var.enclosing_var;
      }
      return var;
    } else {
      Elements elems = new ElementsFinder(var_info_name).elems(); // vin ok
      return ppt.find_var_by_name(elems.name());
    }
  }

  /**
   * Returns the VarInfo that represents the hashcode of the base array of this array. For example,
   * if the array is a[].b.c, returns a. Returns null if there is no such variable.
   */
  @Pure
  public @Nullable VarInfo get_base_array_hashcode() {
    if (FileIO.new_decl_format) {
      return get_base_array().enclosing_var;
    } else {
      Elements elems = new ElementsFinder(var_info_name).elems(); // vin ok
      // System.out.printf("term.name() = %s%n", elems.term.name());
      return ppt.find_var_by_name(elems.term.name());
    }
  }

  /** Returns the lower bound of the array or slice. */
  public Quantify.Term get_lower_bound() {
    assert file_rep_type.isArray() : "var " + name() + " rep " + file_rep_type;
    if (isDerived()) {
      return derived.get_lower_bound();
    } else {
      return new Quantify.Constant(0);
    }
  }

  /** Returns the upper bound of the array or slice. */
  public Quantify.Term get_upper_bound() {
    assert file_rep_type.isArray();
    if (isDerived()) {
      return derived.get_upper_bound();
    } else {
      return new Quantify.Length(this, -1);
    }
  }

  /**
   * Returns the length of this array. The array can be an array or a list. It cannot be a slice.
   */
  public Quantify.Term get_length() {
    assert file_rep_type.isArray() && !isDerived() : this;
    return new Quantify.Length(this, 0);
  }

  /**
   * Updates any references to other variables that should be within this ppt by looking them up
   * within the ppt. Necessary if a variable is moved to a different program point or if cloned
   * variable is placed in a new program point (such as is done when combined exits are created).
   */
  public void update_after_moving_to_new_ppt() {
    if (enclosing_var != null) {
      // enclosing_var exists but is in the wrong ppt; update it
      enclosing_var = ppt.find_var_by_name(enclosing_var.name());
      assert enclosing_var != null;
    }
  }

  /**
   * Temporary to let things compile now that name is private. Eventually this should be removed.
   */
  public VarInfoName get_VarInfoName() {
    return var_info_name; // vin ok
  }

  private static boolean isStatic(String variable, String enclosing) {
    return !variable.startsWith(enclosing) || variable.charAt(enclosing.length()) != '.';
  }

  // Map java objects to C# objects.
  private static final Map<String, String> csharp_types = new HashMap<>();

  static {
    csharp_types.put("java.lang.String", "string");
    csharp_types.put("java.lang.String[]", "string[]");
    csharp_types.put("java.lang.Object", "object");
    csharp_types.put("java.lang.Object[]", "object[]");
    csharp_types.put("boolean", "bool");
  }

  /** Transforms a Daikon type representation into a valid C# type. */
  public static String fix_csharp_type_name(String type) {
    if (csharp_types.containsKey(type)) {
      return csharp_types.get(type);
    } else {
      return type;
    }
  }

  /**
   * If the variable is an array, returns a valid C# 'Select' statement representing the array. For
   * example, this.Array[].field would become this.Array.Select(x &rArr; x.field)
   *
   * <p>If the variable is not an array, csharp_name() is returned.
   */
  public String csharp_collection_string() {
    String[] split = csharp_array_split();
    if (split[1].equals("")) {
      return split[0];
    } else {
      return split[0] + ".Select(x => x" + split[1] + ")";
    }
  }

  /**
   * Splits an array variable into the array and field portions. For example, if the variable
   * this.Array[].field then
   *
   * <pre>
   * result[0] = this.Array[]
   * result[1] = field
   * </pre>
   *
   * If the variable is not an array then
   *
   * <pre>
   * result[0] = csharp_name()
   * result[1] = ""
   * </pre>
   *
   * (there is no splitting).
   */
  public String[] csharp_array_split() {
    String[] results = new String[2];

    if (!is_array()) {
      results[0] = csharp_name();
      results[1] = "";
      return results;
    }

    String fields = "";
    VarInfo v = this;
    // Go backwards from v until we reach the array portion.
    while (v.var_kind != VarInfo.VarKind.ARRAY && v.enclosing_var != null) {
      if (v.relative_name != null) {
        if (v.relative_name.equals("GetType()")) {
          fields = "." + v.relative_name;
        } else {
          fields = "." + v.relative_name + fields;
        }
      }
      v = v.enclosing_var;
    }

    results[0] = v.csharp_name();
    results[1] = fields;
    return results;
  }

  /** Returns the name of this variable in the specified format. */
  public String name_using(OutputFormat format) {
    if (format == OutputFormat.DAIKON) {
      return name();
    }
    if (format == OutputFormat.SIMPLIFY) {
      return simplify_name();
    }
    if (format == OutputFormat.ESCJAVA) {
      return esc_name();
    }
    if (format == OutputFormat.JAVA) {
      return java_name();
    }
    if (format == OutputFormat.JML) {
      return jml_name();
    }
    if (format == OutputFormat.DBCJAVA) {
      return dbc_name();
    }
    if (format == OutputFormat.CSHARPCONTRACT) {
      return csharp_name();
    }
    throw new UnsupportedOperationException("Unknown format requested: " + format);
  }

  /** Returns the name of this variable as a valid C# Code Contract. */
  @SideEffectFree
  public String csharp_name() {
    return csharp_name(null);
  }

  /**
   * Returns the name of this variable as a valid C# Code Contract.
   *
   * @param index an an array index. Must be null for a non-array variable.
   * @return the name of this variable as a valid C# Code Contract
   */
  @SideEffectFree
  public String csharp_name(@Nullable String index) {
    if (index != null) {
      assert file_rep_type.isArray();
    }

    if (postState != null) {
      return "Contract.OldValue(" + postState.csharp_name(index) + ")";
    }

    if (derived != null) {
      return derived.csharp_name(index);
    }

    switch (var_kind) {
      case FIELD:
        assert relative_name != null : this;

        if (enclosing_var != null) {
          if (isStatic(str_name, enclosing_var.name())) {
            return str_name;
          }
          return enclosing_var.csharp_name(index) + "." + relative_name;
        }

        return str_name;

      case FUNCTION:
        if (var_flags.contains(VarFlags.TO_STRING)) {
          return enclosing_var.csharp_name(index);
        }

        if (var_flags.contains(VarFlags.CLASSNAME)) {
          if (arr_dims > 0) {
            return csharp_collection_string();
          } else {
            return enclosing_var.csharp_name(index) + ".GetType()";
          }
        }

        if (enclosing_var != null) {

          if (isStatic(str_name, enclosing_var.name())) {
            String qualifiedName = str_name.substring(0, str_name.indexOf("("));
            return qualifiedName + "(" + enclosing_var.csharp_name(index) + ")";
          } else if (var_flags.contains(VarFlags.IS_PROPERTY)) {
            return enclosing_var.csharp_name(index) + "." + relative_name;
          } else {
            return enclosing_var.csharp_name(index) + "." + relative_name + "()";
          }
        } else {
          return str_name;
        }

      case ARRAY:
        if (index == null) {
          return enclosing_var.csharp_name(null);
        }
        return enclosing_var.csharp_name(null) + "[" + index + "]";

      case VARIABLE:
        assert enclosing_var == null;
        return str_name;

      case RETURN:
        return "Contract.Result<" + fix_csharp_type_name(type.toString()) + ">()";

      default:
        throw new Error("can't drop through switch statement.");
    }
  }

  /** Returns the name in Java format. This is the same as JML. */
  public String java_name() {
    if (!FileIO.new_decl_format) {
      return var_info_name.java_name(this); // vin ok
    }

    return jml_name();
  }

  /** Returns the name in DBC format. This is the same as JML. */
  public String dbc_name() {
    if (!FileIO.new_decl_format) {
      return var_info_name.dbc_name(this); // vin ok
    }

    return jml_name();
  }

  /** Returns the name of this variable in ESC format. */
  @SideEffectFree
  public String esc_name() {
    if (!FileIO.new_decl_format) {
      return var_info_name.esc_name(); // vin ok
    }

    return esc_name(null);
  }

  /**
   * Returns the name of this variable in ESC format. If an index is specified, it is used as an
   * array index. It is an error to specify an index on a non-array variable.
   */
  @SideEffectFree
  public String esc_name(@Nullable String index) {

    // System.out.printf("esc_name for %s, flags %s, enclosing-var %s "
    //                  + " poststate %s index %s rname %s ppt %s%n", str_name,
    //                    var_flags, enclosing_var, postState, index,
    //                    relative_name, ppt.name());
    if (index != null) {
      assert file_rep_type.isArray();
    }

    // If this is an orig variable, use the post version to generate the name
    if (postState != null) {
      return "\\old(" + postState.esc_name(index) + ")";
    }

    // If this is a derived variable, the derivations builds the name
    if (derived != null) {
      return derived.esc_name(index);
    }

    // Build the name by processing back through all of the enclosing variables
    switch (var_kind) {
      case FIELD:
        assert relative_name != null : this;
        if (enclosing_var != null) {
          return enclosing_var.esc_name(index) + "." + relative_name;
        }
        return str_name;
      case FUNCTION:
        // function_args      assert function_args == null : "function args not implemented";
        if (var_flags.contains(VarFlags.CLASSNAME)) {
          return ("\\typeof(" + enclosing_var.esc_name(index) + ")");
        }
        if (var_flags.contains(VarFlags.TO_STRING)) {
          return enclosing_var.esc_name(index) + ".toString";
        }
        if (enclosing_var != null) {
          return enclosing_var.esc_name(index) + "." + relative_name + "()";
        }
        return str_name;
      case ARRAY:
        if (index == null) {
          return enclosing_var.esc_name(null) + "[]";
        }
        return enclosing_var.esc_name(null) + "[" + index + "]";
      case VARIABLE:
        assert enclosing_var == null;
        return str_name;
      case RETURN:
        return "\\result";
      default:
        throw new Error("can't drop through switch statement");
    }
  }

  /** Returns the name of this variable in JML format. */
  @SideEffectFree
  public String jml_name() {
    if (!FileIO.new_decl_format) {
      return var_info_name.jml_name(this); // vin ok
    }

    return jml_name(null);
  }

  /**
   * Returns the name of this variable in JML format.
   *
   * @param index an array index. Must be null for a non-array variable.
   * @return the name of this variable in JML format
   */
  public String jml_name(@Nullable String index) {

    if (index != null) {
      assert file_rep_type.isArray();
    }

    // If this is an orig variable, use the post version to generate the name
    if (postState != null) {
      return "\\old(" + postState.jml_name(index) + ")";
    }

    // If this is a derived variable, the derivations builds the name
    if (derived != null) {
      return derived.jml_name(index);
    }

    // If this is an array of fields, collect the fields into a collection
    if ((arr_dims > 0) && (var_kind != VarKind.ARRAY) && !var_flags.contains(VarFlags.CLASSNAME)) {
      String field_name = relative_name;
      ;
      VarInfo vi = this.enclosing_var;
      for (; vi.var_kind != VarKind.ARRAY; vi = vi.enclosing_var) {
        field_name = vi.relative_name + "." + field_name;
      }
      return String.format("daikon.Quant.collectObject(%s, \"%s\")", vi.jml_name(), field_name);
    }

    // Build the name by processing back through all of the enclosing variables
    switch (var_kind) {
      case FIELD:
        assert relative_name != null : this;
        if (enclosing_var != null) {
          return enclosing_var.jml_name(index) + "." + relative_name;
        }
        return str_name;
      case FUNCTION:
        // function_args      assert function_args == null : "function args not implemented";
        if (var_flags.contains(VarFlags.CLASSNAME)) {
          if (arr_dims > 0) {
            return String.format("daikon.Quant.typeArray(%s)", enclosing_var.jml_name(index));
          } else {
            return enclosing_var.jml_name(index) + DaikonVariableInfo.class_suffix;
          }
        }
        if (var_flags.contains(VarFlags.TO_STRING)) {
          return enclosing_var.jml_name(index) + ".toString()";
        }
        if (enclosing_var != null) {
          return enclosing_var.jml_name(index) + "." + relative_name + "()";
        }
        return str_name;
      case ARRAY:
        if (index == null) {
          return enclosing_var.jml_name(null);
        }
        return enclosing_var.jml_name(null) + "[" + index + "]";
      case VARIABLE:
        assert enclosing_var == null;
        return str_name;
      case RETURN:
        return "\\result";
      default:
        throw new Error("can't drop through switch statement");
    }
  }

  /** Returns the name of this variable in simplify format. */
  @SideEffectFree
  public String simplify_name() {
    return simplify_name(null);
  }

  /**
   * Returns the name of this variable in simplify format. If an index is specified, it is used as
   * an array index. It is an error to specify an index on a non-array variable.
   */
  public String simplify_name(@Nullable String index) {
    if (!FileIO.new_decl_format) {
      return var_info_name.simplify_name(); // vin ok
    }

    assert (index == null) || file_rep_type.isArray() : index + " " + name();

    // If this is a derived variable, the derivations builds the name
    if (derived != null) {
      return derived.simplify_name();
    }

    // Build the name by processing back through all of the enclosing variables
    switch (var_kind) {
      case FIELD:
        assert relative_name != null : this;
        return String.format("(select |%s| %s)", relative_name, enclosing_var.simplify_name(index));
      case FUNCTION:
        // function_args      assert function_args == null : "function args not implemented";
        if (var_flags.contains(VarFlags.CLASSNAME)) {
          return ("(typeof " + enclosing_var.simplify_name(index) + ")");
        }
        if (var_flags.contains(VarFlags.TO_STRING)) {
          return String.format("(select |toString| %s)", enclosing_var.simplify_name(index));
        }
        if (enclosing_var != null) {
          return enclosing_var.simplify_name(index) + "." + relative_name + "()";
        }
        return str_name;
      case ARRAY:
        if (index == null) {
          return String.format("(select elems %s)", enclosing_var.simplify_name());
        }
        // if (index.equals("|0|")) {
        //   System.err.printf("index = %s%n", index);
        //   Throwable t = new Throwable();
        //   t.printStackTrace();
        // }
        return String.format("(select (select elems %s) %s)", enclosing_var.simplify_name(), index);
      case VARIABLE:
        if (dkconfig_constant_fields_simplify && str_name.contains(".")) {
          String sel;
          String[] fields;
          if (postState != null) {
            fields = postState.name().split("\\.");
            sel = String.format("(select |%s| |__orig__%s|)", fields[1], fields[0]);
          } else { // not orig variable
            fields = str_name.split("\\.");
            sel = String.format("(select |%s| |%s|)", fields[1], fields[0]);
          }
          for (int ii = 2; ii < fields.length; ii++) {
            sel = String.format("(select |%s| %s)", fields[ii], sel);
          }
          return sel;
        }

        assert enclosing_var == null;
        if (postState != null) {
          return "|__orig__" + postState.name() + "|";
        }
        return "|" + str_name + "|";
      case RETURN:
        return "|return|";
      default:
        throw new Error("can't drop through switch statement");
    }
  }

  /** Return the name of this variable in its prestate (orig). */
  @SideEffectFree
  public @Interned String prestate_name() {
    return ("orig(" + name() + ")").intern();
  }

  /**
   * Returns the name of the size variable that correponds to this array variable in simplify
   * format. Returns null if this variable is not an array or the size name can't be constructed for
   * other reasons. Note that isArray seems to distinguish between actual arrays and other sequences
   * (such as java.util.list). Simplify uses (it seems) the same length approach for both, so we
   * don't check isArray().
   */
  public @Nullable String get_simplify_size_name() {
    // Implement the method in two ways, to double-check results.

    @Interned String result;
    if (!file_rep_type.isArray() || isDerived()) {
      result = null;
    } else {
      // System.out.printf("Getting size name for %s [%s]%n", name(),
      //                    get_length());
      result = get_length().simplify_name().intern();
    }

    @Interned String old_result;
    if (!var_info_name.isApplySizeSafe()) { // vin ok
      old_result = null;
    } else {
      old_result = var_info_name.applySize().simplify_name().intern(); // vin ok
    }
    if (FileIO.new_decl_format && (old_result != result)) {
      throw new Error(
          String.format(
              "%s: '%s' '%s'%n basehashcode = %s%n",
              this, result, old_result, get_base_array_hashcode()));
    }

    return old_result;
  }

  /** Returns true if this variable contains a simple variable whose name is varname. */
  public boolean includes_simple_name(String varname) {
    if (!FileIO.new_decl_format) {
      return var_info_name.includesSimpleName(varname); // vin ok
    }

    if (isDerived()) {
      for (VarInfo base : derived.getBases()) {
        if (base.includes_simple_name(varname)) {
          return true;
        }
      }
    } else {
      for (VarInfo vi = this; vi != null; vi = vi.enclosing_var) {
        if ((vi.var_kind == VarKind.VARIABLE) && vi.name().equals(varname)) {
          return true;
        }
      }
    }
    return false;
  }

  /**
   * Quantifies over the specified array variables in ESC format. Returns an array with 2 more
   * elements than the argument. Element 0 is the quantification, Element 1 is the indexed form of
   * variable 1, Element 2 is the indexed form of variable 3, and Element 4 is syntax such as close
   * parentheses.
   */
  public static String[] esc_quantify(VarInfo... vars) {
    return esc_quantify(true, vars);
  }

  /**
   * Quantifies over the specified array variables in ESC format. Returns an array with 2 more
   * elements than the argument. Element 0 is the quantification, Element 1 is the indexed form of
   * variable 1, Element 2 is the indexed form of variable 3, and Element 4 is syntax such as close
   * parentheses.
   */
  public static String[] esc_quantify(boolean elementwise, VarInfo... vars) {

    if (FileIO.new_decl_format) {
      Quantify.ESCQuantification quant =
          new Quantify.ESCQuantification(Quantify.get_flags(elementwise), vars);
      if (vars.length == 1) {
        return new String[] {quant.get_quantification(), quant.get_arr_vars_indexed(0), ")"};
      } else if ((vars.length == 2) && vars[1].file_rep_type.isArray()) {
        return new String[] {
          quant.get_quantification(),
          quant.get_arr_vars_indexed(0),
          quant.get_arr_vars_indexed(1),
          ")"
        };
      } else {
        return new String[] {
          quant.get_quantification(), quant.get_arr_vars_indexed(0), vars[1].esc_name(), ")"
        };
      }
    } else {
      VarInfoName vin[] = new VarInfoName[vars.length];
      for (int ii = 0; ii < vars.length; ii++) {
        vin[ii] = vars[ii].var_info_name; // vin ok
      }
      return VarInfoName.QuantHelper.format_esc(vin, elementwise);
    }
  }

  /**
   * Returns a string array with 3 elements. The first element is the sequence, the second element
   * is the lower bound, and the third element is the upper bound. Returns null if this is not a
   * direct array or slice.
   */
  public String @Nullable [] simplifyNameAndBounds() {
    if (!FileIO.new_decl_format) {
      return VarInfoName.QuantHelper.simplifyNameAndBounds(var_info_name); // vin ok
    }

    String[] results = new String[3];
    if (is_direct_non_slice_array() || (derived instanceof SequenceSubsequence)) {
      results[0] = get_base_array_hashcode().simplify_name();
      results[1] = get_lower_bound().simplify_name();
      results[2] = get_upper_bound().simplify_name();
      return results;
    }

    return null;
  }

  /**
   * Returns the upper and lower bounds of the slice in simplify format. The implementation is
   * somewhat different that simplifyNameAndBounds (I don't know why).
   */
  public String @Nullable [] get_simplify_slice_bounds() {
    if (!FileIO.new_decl_format) {
      @Interned VarInfoName[] bounds = var_info_name.getSliceBounds(); // vin ok
      if (bounds == null) {
        return null;
      }
      String[] str_bounds = new String[2];
      str_bounds[0] = bounds[0].simplify_name();
      str_bounds[1] = bounds[1].simplify_name();
      return str_bounds;
    }

    String[] results;
    if (derived instanceof SequenceSubsequence) {
      results = new String[2];
      results[0] = get_lower_bound().simplify_name().intern();
      results[1] = get_upper_bound().simplify_name().intern();
    } else {
      results = null;
    }

    return results;
  }

  /**
   * Return a string in simplify format that will seclect the (index_base + index_off)-th element of
   * the sequence specified by this variable.
   *
   * @param simplify_index_name name of the index. If free is false, this must be a number or null
   *     (null implies an index of 0).
   * @param free true of simplify_index_name is variable name
   * @param index_off offset from the index
   */
  public String get_simplify_selectNth(String simplify_index_name, boolean free, int index_off) {

    // Remove the simplify bars if present from the index name
    if ((simplify_index_name != null)
        && simplify_index_name.startsWith("|")
        && simplify_index_name.endsWith("|"))
      simplify_index_name = simplify_index_name.substring(1, simplify_index_name.length() - 1);

    // Use VarInfoName to handle the old format
    if (!FileIO.new_decl_format) {
      VarInfoName select =
          VarInfoName.QuantHelper.selectNth(
              this.var_info_name, // vin ok
              simplify_index_name,
              free,
              index_off);
      // System.out.printf("sNth: index %s, free %b, off %d, result '%s'%n",
      //                     simplify_index_name, free, index_off,
      //                     select.simplify_name());
      return select.simplify_name();
    }

    // Calculate the index (including the offset if non-zero)
    String complete_index;
    if (!free) {
      int index = 0;
      if (simplify_index_name != null) {
        index = Integer.decode(simplify_index_name);
      }
      index += index_off;
      complete_index = String.format("%d", index);
    } else {
      if (index_off != 0) {
        complete_index = String.format("(+ |%s| %d)", simplify_index_name, index_off);
      } else {
        complete_index = String.format("|%s|", simplify_index_name);
      }
    }

    // Return the array properly indexed
    return simplify_name(complete_index);
  }

  /**
   * Return a string in simplify format that will seclect the index_off element in a sequence that
   * has a lower bound.
   *
   * @param index_off offset from the index
   */
  public String get_simplify_selectNth_lower(int index_off) {

    // Use VarInfoName to handle the old format
    if (!FileIO.new_decl_format) {
      @Interned VarInfoName[] bounds = var_info_name.getSliceBounds();
      VarInfoName lower = null;
      if (bounds != null) {
        lower = bounds[0];
      }
      VarInfoName select =
          VarInfoName.QuantHelper.selectNth(
              var_info_name, // vin ok
              lower,
              index_off);
      return select.simplify_name();
    }

    // Calculate the index (including the offset if non-zero)
    String complete_index;
    Quantify.Term lower = get_lower_bound();
    String lower_name = lower.simplify_name();
    if (!(lower instanceof Quantify.Constant)) {
      lower_name = String.format("|%s|", lower_name);
    }
    if (index_off != 0) {
      if (lower instanceof Quantify.Constant) {
        complete_index = String.format("%d", ((Quantify.Constant) lower).get_value() + index_off);
      } else {
        complete_index = String.format("(+ %s %d)", lower_name, index_off);
      }
    } else {
      complete_index = String.format("%s", lower_name);
    }

    // Return the array properly indexed
    // System.err.printf("lower bound type = %s [%s] %s%n", lower,
    //                   lower.getClass(), complete_index);
    return simplify_name(complete_index);
  }

  /** Get a fresh variable name that doesn't appear in the given variable in simplify format. */
  public static String get_simplify_free_index(VarInfo... vars) {
    if (!FileIO.new_decl_format) {
      VarInfoName[] vins = new VarInfoName[vars.length];
      for (int ii = 0; ii < vars.length; ii++) {
        vins[ii] = vars[ii].var_info_name; // vin ok
      }
      return VarInfoName.QuantHelper.getFreeIndex(vins).simplify_name();
    }

    // Get a free variable for each variable and return the first one
    QuantifyReturn[] qret = Quantify.quantify(vars);
    return qret[0].index.simplify_name();
  }

  /** Get a 2 fresh variable names that doesn't appear in the given variable in simplify format. */
  public static String[] get_simplify_free_indices(VarInfo... vars) {
    if (!FileIO.new_decl_format) {
      if (vars.length == 1) {
        VarInfoName index1_vin =
            VarInfoName.QuantHelper.getFreeIndex(vars[0].var_info_name); // vin ok
        String index2 =
            VarInfoName.QuantHelper.getFreeIndex(vars[0].var_info_name, index1_vin)
                .simplify_name(); // vin ok
        return new String[] {index1_vin.name(), index2};
      } else if (vars.length == 2) {
        VarInfoName index1_vin =
            VarInfoName.QuantHelper.getFreeIndex(
                vars[0].var_info_name, vars[1].var_info_name); // vin ok
        String index2 =
            VarInfoName.QuantHelper.getFreeIndex(
                    vars[0].var_info_name, vars[1].var_info_name, index1_vin) // vin ok
                .simplify_name();
        return new String[] {index1_vin.name(), index2};
      } else {
        throw new Error("unexpected length " + vars.length);
      }
    }

    // Get a free variable for each variable
    if (vars.length == 1) {
      vars = new VarInfo[] {vars[0], vars[0]};
    }
    QuantifyReturn qret[] = Quantify.quantify(vars);
    return new String[] {qret[0].index.simplify_name(), qret[1].index.simplify_name()};
  }

  /**
   * Quantifies over the specified array variables in Simplify format. Returns a string array that
   * contains the quantification, indexed form of each variable, optionally the index itself, and
   * the closer.
   *
   * <p>If elementwise is true, include the additional contraint that the indices (there must be
   * exactly two in this case) refer to corresponding positions. If adjacent is true, include the
   * additional constraint that the second index be one more than the first. If distinct is true,
   * include the constraint that the two indices are different. If includeIndex is true, return
   * additional strings, after the roots but before the closer, with the names of the index
   * variables.
   */
  public static String[] simplify_quantify(EnumSet<QuantFlags> flags, VarInfo... vars) {

    if (!FileIO.new_decl_format) {
      // Get the names for each variable.
      VarInfoName vin[] = new VarInfoName[vars.length];
      for (int ii = 0; ii < vars.length; ii++) {
        vin[ii] = vars[ii].var_info_name; // vin ok
      }

      return VarInfoName.QuantHelper.format_simplify(
          vin,
          flags.contains(QuantFlags.ELEMENT_WISE),
          flags.contains(QuantFlags.ADJACENT),
          flags.contains(QuantFlags.DISTINCT),
          flags.contains(QuantFlags.INCLUDE_INDEX));
    }

    Quantify.SimplifyQuantification quant = new Quantify.SimplifyQuantification(flags, vars);
    boolean include_index = flags.contains(QuantFlags.INCLUDE_INDEX);
    if ((vars.length == 1) && include_index) {
      return new String[] {
        quant.get_quantification(),
        quant.get_arr_vars_indexed(0),
        quant.get_index(0),
        quant.get_closer()
      };
    } else if (vars.length == 1) {
      return new String[] {
        quant.get_quantification(), quant.get_arr_vars_indexed(0), quant.get_closer()
      };
    } else if ((vars.length == 2) && include_index) {
      return new String[] {
        quant.get_quantification(),
        quant.get_arr_vars_indexed(0),
        quant.get_arr_vars_indexed(1),
        quant.get_index(0),
        quant.get_index(1),
        quant.get_closer()
      };
    } else { // must be length 2 and no index
      return new String[] {
        quant.get_quantification(),
        quant.get_arr_vars_indexed(0),
        quant.get_arr_vars_indexed(1),
        quant.get_closer()
      };
    }
  }

  /**
   * See {@link #simplify_quantify(EnumSet, VarInfo[])}.
   *
   * @see #simplify_quantify(EnumSet, VarInfo[])
   */
  public static String[] simplify_quantify(VarInfo... vars) {
    return simplify_quantify(EnumSet.noneOf(QuantFlags.class), vars);
  }

  /**
   * Returns a rough indication of the complexity of the variable. Higher numbers indicate more
   * complexity.
   */
  public int complexity() {
    if (!FileIO.new_decl_format) {
      // System.out.printf("%s - %s%n", this, var_info_name.repr());
      return var_info_name.inOrderTraversal().size(); // vin ok
    }

    int cnt = 0;
    if (isDerived()) {
      cnt += derived.complexity();
      VarInfo[] bases = derived.getBases();
      for (VarInfo vi : bases) {
        cnt += vi.complexity();
      }
      // Adjust for the complexity change when a prestate is nested in
      // another prestate.  This is just done to match the old version
      if ((bases.length == 2) && bases[0].isPrestate()) {
        if (bases[1].isPrestate()) {
          cnt--;
        } else {
          cnt++;
        }
      }
    } else {
      if (isPrestate()) {
        cnt++;
      }
      for (VarInfo vi = this; vi != null; vi = vi.enclosing_var) {
        cnt++;
      }
    }

    // int old_cnt = var_info_name.inOrderTraversal().size();
    // if (cnt != old_cnt)
    //   System.out.printf("var %s, new cnt = %d, old cnt = %d [%s]%n",
    //                 name(), cnt, old_cnt, var_info_name.inOrderTraversal());
    return cnt;
  }

  /**
   * Returns true if this variable can be assigned to. Currently this is presumed true of all
   * variable except the special variable for the type of a variable and the size of a sequence. It
   * should include pure functions as well.
   */
  @Pure
  public boolean is_assignable_var() {
    if (!FileIO.new_decl_format) {
      return !((var_info_name instanceof VarInfoName.TypeOf) // vin ok
          || (var_info_name instanceof VarInfoName.SizeOf)); // vin ok
    }

    return !(is_typeof() || is_size());
  }

  /**
   * Returns whether or not this variable represents the type of a variable (eg,
   * a.getClass().getName()). Note that this will miss prestate variables such as
   * 'orig(a.getClass().getName())'.
   */
  @Pure
  public boolean is_typeof() {
    if (!FileIO.new_decl_format) {
      return (var_info_name instanceof VarInfoName.TypeOf); // vin ok
    }

    // The isPrestate check doesn't seem necessary, but is required to
    // match old behavior.
    return !isPrestate() && var_flags.contains(VarFlags.CLASSNAME);
  }

  /**
   * Returns whether or not this variable represents the type of a variable (eg,
   * a.getClass().getName()). This version finds prestate variables such as
   * 'org(a.getClass().getName())'.
   */
  public boolean has_typeof() {
    if (!FileIO.new_decl_format) {
      return var_info_name.hasTypeOf(); // vin ok
    }

    if (isPrestate()) {
      return postState.has_typeof();
    }
    return is_typeof();
  }

  /** Returns whether or not this variable is the 'this' variable. */
  @Pure
  public boolean is_this() {
    return name().equals("this");
    // return get_VarInfoName().equals(VarInfoName.THIS);
  }

  /**
   * Returns whether or not this variable is the 'this' variable. True for both normal and prestate
   * versions of the variable.
   */
  @Pure
  public boolean isThis() {
    return var_info_name.isThis();
  }

  /** Returns whether this is a size of an array or a prestate thereof. */
  @Pure
  public boolean is_size() {
    return (derived instanceof SequenceLength);
  }

  /** Returns wehther or not this variable is a field. */
  @Pure
  public boolean is_field() {
    return (var_info_name instanceof VarInfoName.Field);
  }

  /** Returns whether or not this variable has an integer offset (eg, a+2) */
  @Pure
  public boolean is_add() {
    return (var_info_name instanceof VarInfoName.Add);
  }

  /**
   * Returns the integer offset if this variable is an addition such as a+2. Throws an exception of
   * this variable is not an addition.
   *
   * @see #is_add()
   */
  public int get_add_amount() {
    return ((VarInfoName.Add) var_info_name).amount;
  }

  /**
   * Returns whether or not this variable is an actual array as opposed to an array that is created
   * over fields/methods of an array. For example, 'a[]' is a direct array, but 'a[].b' is not.
   */
  @Pure
  public boolean is_direct_array() {
    // Must be an array to be a direct array
    if (!rep_type.isArray()) {
      return false;
    }

    // If $Field or $Type appears before $Elements, false.
    // System.out.printf("%s flatten %s%n", name(), name);
    for (VarInfoName node : new VarInfoName.InorderFlattener(var_info_name).nodes()) {
      if (node instanceof VarInfoName.Field) {
        return false;
      }
      if (node instanceof VarInfoName.TypeOf) {
        return false;
      }
      if (node instanceof VarInfoName.Elements) {
        break;
      }
    }

    return true;
  }

  /**
   * Returns whether or not this variable is an actual array as opposed to an array that is created
   * over fields/methods of an array or a slice. For example, 'a[]' is a direct array, but 'a[].b'
   * and 'a[i..]' are not.
   */
  @Pure
  public boolean is_direct_non_slice_array() {
    return (var_info_name instanceof VarInfoName.Elements);
  }

  /**
   * Returns whether or not two variables have the same enclosing variable. If either variable is
   * not a field, returns false.
   */
  public boolean has_same_parent(VarInfo other) {
    if (!is_field() || !other.is_field()) {
      return false;
    }

    VarInfoName.Field name1 = (VarInfoName.Field) var_info_name;
    VarInfoName.Field name2 = (VarInfoName.Field) other.var_info_name;

    return name1.term.equals(name2.term);
  }

  /**
   * Returns the variable that encloses this one. For example if this variable is 'x.a.b', the
   * enclosing variable is 'x.a'.
   */
  public @Nullable VarInfo get_enclosing_var() {
    if (FileIO.new_decl_format) {
      return enclosing_var;
    } else {
      List<VarInfoName> traversal = new VarInfoName.InorderFlattener(var_info_name).nodes();
      if (traversal.size() <= 1) {
        // System.out.printf("size <= 1, traversal = %s%n", traversal);
        return null;
      } else {
        VarInfo enclosing_vi = ppt.find_var_by_name(traversal.get(1).name());
        // if (enclosing_vi == null)
        //  System.out.printf("Can't find '%s' in %s%n",
        //                      traversal.get(1).name(), ppt.varNames());
        return enclosing_vi;
      }
    }
  }

  /**
   * Replaces all instances of 'this' in the variable with the name of arg. Used to match up
   * enter/exit variables with object variables.
   */
  public String replace_this(VarInfo arg) {
    VarInfoName parent_name = var_info_name.replaceAll(VarInfoName.THIS, arg.var_info_name);
    return parent_name.name();
  }

  /**
   * Creates a VarInfo that is a subsequence that begins at begin and ends at end with the specified
   * shifts. The begin or the end can be null, but a non-zero shift is only allowed with non-null
   * variables.
   */
  public static VarInfo make_subsequence(
      VarInfo seq, @Nullable VarInfo begin, int begin_shift, @Nullable VarInfo end, int end_shift) {

    String begin_str = inside_name(begin, seq.isPrestate(), begin_shift);
    if (begin_str.equals("")) { // interned if the null string, not interned otherwise
      begin_str = "0";
    }
    String end_str = inside_name(end, seq.isPrestate(), end_shift);

    VarInfoName begin_name;
    String parent_format = "%s..";
    if (begin == null) {
      begin_name = null;
    } else {
      begin_name = (begin != null) ? begin.var_info_name : null;
      if (begin_shift == -1) {
        begin_name = begin_name.applyDecrement();
        parent_format = "%s-1..";
      } else if (begin_shift == 1) {
        begin_name = begin_name.applyIncrement();
        parent_format = "%s+1..";
      } else {
        assert begin_shift == 0;
      }
    }

    VarInfoName end_name;
    if (end == null) {
      end_name = null;
      parent_format += "%s";
    } else {
      end_name = end.var_info_name;
      if (end_shift == -1) {
        end_name = end_name.applyDecrement();
        parent_format += "%s-1";
      } else if (end_shift == 1) {
        end_name = end_name.applyIncrement();
        parent_format += "%s+1";
      } else {
        assert end_shift == 0;
        parent_format += "%s";
      }
    }

    VarInfoName new_name = seq.var_info_name.applySlice(begin_name, end_name);

    VarInfo vi = new VarInfo(new_name, seq.type, seq.file_rep_type, seq.comparability, seq.aux);
    vi.setup_derived_base(seq, begin, end);
    vi.str_name =
        seq.apply_subscript(String.format("%s..%s", begin_str, end_str))
            .intern(); // interning bugfix

    // If there is a parent ppt (set in setup_derived_base), set the
    // parent variable accordingly.  If all of the original variables used
    // the default name, this can as well.  Otherwise, build the parent
    // name.
    for (VarParent parent : vi.parents) {
      int rid = parent.parent_relation_id;

      if ((seq.get_parent(rid) == null)
          && ((begin == null) || !begin.has_parent(rid) || (begin.parent_var(rid) == null))
          && ((end == null) || !end.has_parent(rid) || (end.parent_var(rid) == null))) {

        parent.parent_variable = null;
      } else {
        String begin_pname =
            (begin == null || !begin.has_parent(rid)) ? "0" : begin.parent_var_name(rid);
        String end_pname = (end == null || !end.has_parent(rid)) ? "" : end.parent_var_name(rid);
        @SuppressWarnings(
            "formatter") // format string is constructed above using make_subsequence's arguments
        String res =
            apply_subscript(
                seq.parent_var_name(rid), String.format(parent_format, begin_pname, end_pname));
        parent.parent_variable = res;
        // System.out.printf("-- set parent var from '%s' '%s' '%s' '%s'%n",
        //       seq.parent_var_name(), parent_format, begin_pname, end_pname);
      }
      // System.out.printf("Parent for %s:%s is %s:%s%n",
      //                ((seq.ppt != null)? seq.ppt.name() : "none"), vi.name(),
      //                  vi.parent_ppt, vi.parent_variable);
    }

    return vi;
  }

  /**
   * Returns the name to use for vi inside of a array reference. If the array reference is orig,
   * then orig is implied. This removes orig from orig variales and adds post to post variables.
   */
  private static String inside_name(@Nullable VarInfo vi, boolean in_orig, int shift) {
    if (vi == null) {
      return "";
    }

    String shift_str = "";
    if (shift != 0) {
      shift_str = String.format("%+d", shift);
    }

    if (in_orig) {
      if (vi.isPrestate()) {
        return vi.postState.name() + shift_str;
      } else {
        return String.format("post(%s)%s", vi.name(), shift_str);
      }
    } else {
      return vi.name() + shift_str;
    }
  }

  /**
   * Creates a VarInfo that is an index into a sequence. The type, file_rep_type, etc are taken from
   * the element type of the sequence.
   */
  public static VarInfo make_subscript(VarInfo seq, @Nullable VarInfo index, int index_shift) {

    String index_str = inside_name(index, seq.isPrestate(), index_shift);

    VarInfoName index_name;
    if (index == null) {
      index_name = VarInfoName.parse(String.valueOf(index_shift));
    } else {
      index_name = index.var_info_name;
      if (index_shift == -1) {
        index_name = index_name.applyDecrement();
      } else {
        assert index_shift == 0 : "bad shift " + index_shift + " for " + index;
      }
    }

    VarInfoName new_name = seq.var_info_name.applySubscript(index_name);
    VarInfo vi =
        new VarInfo(
            new_name,
            seq.type.elementType(),
            seq.file_rep_type.elementType(),
            seq.comparability.elementType(),
            VarInfoAux.getDefault());
    vi.setup_derived_base(seq, index);
    vi.var_kind = VarInfo.VarKind.FIELD;
    vi.str_name = seq.apply_subscript(index_str).intern(); // interning bugfix
    for (VarParent parent : vi.parents) {
      int rid = parent.parent_relation_id;

      if ((seq.parent_var(rid) == null)
          && ((index == null) || !index.has_parent(rid) || (index.parent_var(rid) == null))) {
        parent.parent_variable = null;
      } else { // one of the two bases has a different parent variable name
        String subscript_parent = String.valueOf(index_shift);
        if (index != null && index.has_parent(rid)) {
          subscript_parent = index.parent_var_name(rid);

          if (seq.isPrestate() && !index.isPrestate()) {
            // Wrap the index in POST if the sequence is original
            subscript_parent = VarInfoName.parse(subscript_parent).applyPoststate().name_impl();
          } else if (seq.isPrestate() && index.isPrestate()) {
            // Remove redundant ORIG
            subscript_parent = ((Prestate) VarInfoName.parse(subscript_parent)).term.name_impl();
          }

          if (index_shift == -1) {
            subscript_parent = subscript_parent + "-1";
          }
        }
        parent.parent_variable = apply_subscript(seq.parent_var_name(rid), subscript_parent);
      }
    }
    return vi;
  }

  /**
   * Create a VarInfo that is a function over one or more other variables. The type, rep_type, etc.
   * of the new function are taken from the first variable.
   */
  public static VarInfo make_function(String function_name, VarInfo... vars) {

    VarInfoName[] vin = new VarInfoName[vars.length];
    for (int ii = 0; ii < vars.length; ii++) {
      vin[ii] = vars[ii].var_info_name;
    }

    VarInfo vi =
        new VarInfo(
            VarInfoName.applyFunctionOfN(function_name, vin),
            vars[0].type,
            vars[0].file_rep_type,
            vars[0].comparability,
            vars[0].aux);
    vi.setup_derived_function(function_name, vars);
    return vi;
  }

  /*
   * Creates the derived variable func(seq) from seq.
   *
   * @param func_name name of the function
   * @param type return type of the function.  If null, the return type is
   *             the element type of the sequence.
   * @param seq sequence variable
   * @param shift value to add or subtract from the function.  Legal values
   *              are -1, 0, and 1.
   */
  public static VarInfo make_scalar_seq_func(
      String func_name, @Nullable ProglangType type, VarInfo seq, int shift) {

    VarInfoName viname = seq.var_info_name.applyFunction(func_name);
    if (func_name.equals("size")) {
      viname = seq.var_info_name.applySize();
    }
    String shift_name = "";
    if (shift == -1) {
      viname = viname.applyDecrement();
      shift_name = "_minus1";
    } else if (shift == 1) {
      viname = viname.applyIncrement();
      shift_name = "_plus1";
    } else {
      assert shift == 0;
    }

    @NonNull ProglangType ptype = type;
    @NonNull ProglangType frtype = type;
    VarComparability comp = seq.comparability.indexType(0);
    VarInfoAux aux = VarInfoAux.getDefault();
    if (type == null) {
      ptype = seq.type.elementType();
      frtype = seq.file_rep_type.elementType();
      comp = seq.comparability.elementType();
      aux = seq.aux;
    }
    VarInfo vi = new VarInfo(viname, ptype, frtype, comp, aux);
    vi.setup_derived_base(seq);
    vi.var_kind = VarInfo.VarKind.FUNCTION;
    vi.enclosing_var = seq;
    vi.arr_dims = 0;
    // null is initial value:  vi.function_args = null;
    vi.relative_name = func_name + shift_name;

    // Calculate the string to add for the shift.
    String shift_str = "";
    if (shift != 0) {
      shift_str = String.format("%+d", shift);
    }

    // Determine whether orig should be swapped with the function.
    // The original VarInfoName code did this only for the size
    // function (though it makes the same sense for all functions over
    // sequences).
    boolean swap_orig =
        func_name.equals("size") && seq.isPrestate() && !VarInfoName.dkconfig_direct_orig;

    // Force orig to the outside if specified.
    if (swap_orig) {
      vi.str_name =
          String.format("orig(%s(%s))%s", func_name, seq.postState.name(), shift_str)
              .intern(); // interning bugfix
    } else {
      vi.str_name =
          String.format("%s(%s)%s", func_name, seq.name(), shift_str).intern(); // interning bugfix
    }

    for (VarParent parent : vi.parents) {
      int rid = parent.parent_relation_id;
      if (!seq.has_parent(rid) || seq.parent_var(rid) == null) {
        parent.parent_variable = null;
      } else {
        if (func_name.equals("size")) {
          // Special handling for the case where the parent var name is orig(array[...]).
          // With swapping, the parent should be orig(size(array[..])), however it's stored
          // as a string so the swap can't be done textually.
          VarInfoName parentName = VarInfoName.parse(seq.parent_var_name(rid));

          // Can't use the more general applyFunction method here because it doesn't take into
          // account prestate values as the applySize method explicitly does
          parentName = parentName.applySize();

          parent.parent_variable = String.format("%s%s", parentName.name(), shift_str);
        } else {
          assert !swap_orig : "swap orig with parent " + vi;
          parent.parent_variable =
              String.format("%s(%s)%s", func_name, seq.parent_var_name(rid), shift_str);
        }
      }
    }
    return vi;
  }

  /*
   * Creates the derived variable func(str) from string.
   *
   * @param func_name name of the function
   * @param type return type of the function
   * @param str sequence variable
   */
  public static VarInfo make_scalar_str_func(String func_name, ProglangType type, VarInfo str) {

    VarInfoName viname = str.var_info_name.applyFunction(func_name);

    ProglangType ptype = type;
    ProglangType frtype = type;
    VarComparability comp = str.comparability.string_length_type();
    VarInfoAux aux = VarInfoAux.getDefault();
    VarInfo vi = new VarInfo(viname, ptype, frtype, comp, aux);
    vi.setup_derived_base(str);
    vi.var_kind = VarInfo.VarKind.FUNCTION;
    vi.enclosing_var = str;
    vi.arr_dims = 0;
    // null is initial value:  vi.function_args = null;
    vi.relative_name = func_name;

    vi.str_name = String.format("%s.%s()", str.name(), func_name).intern(); // interning bugfix

    for (VarParent parent : vi.parents) {
      int rid = parent.parent_relation_id;
      if (str.get_parent(rid).parent_variable == null) {
        parent.parent_variable = null;
      } else {
        parent.parent_variable =
            String.format("%s.%s()", str.get_parent(rid).parent_variable, func_name);
      }
    }
    return vi;
  }

  /**
   * Returns true if vi is the prestate version of this. If this is a derived variable, vi must be
   * the same derivation using prestate versions of each base variable.
   */
  @Pure
  public boolean is_prestate_version(VarInfo vi) {

    // If both variables are not derived
    if ((derived == null) && (vi.derived == null)) {

      // true if vi is the prestate version of this
      return !isPrestate() && vi.isPrestate() && name().equals(vi.postState.name());

      // else if both variables are derived
    } else if ((derived != null) && (vi.derived != null)) {

      return derived.is_prestate_version(vi.derived);

      // one is derived and the other isn't
    } else {
      return false;
    }
  }

  /** Returns true if this is an array or a slice. */
  @Pure
  public boolean isArray() {
    return type.isArray();
  }

  /** Returns true if this is a slice. */
  @Pure
  public boolean isSlice() {
    return isArray() && isDerived();
  }

  /** Converts a variable name or expression to the old style of names. */
  public static String old_var_names(String name) {
    if (PrintInvariants.dkconfig_old_array_names && FileIO.new_decl_format) {
      return name.replace("[..]", "[]");
    } else {
      return name;
    }
  }

  /** Returns the old style variable name for this name. */
  public String old_var_name() {
    return old_var_names(name());
  }

  /** Rough check to ensure that the variable name and derivation match up. */
  public void var_check() {

    if (false) {
      if (derived instanceof SequenceSubsequence) {
        if (name().contains("-1")) {
          SequenceSubsequence ss = (SequenceSubsequence) derived;
          // System.out.printf("checking %s [%s] with derived %s[%s]%n",
          //                   this, System.identityHashCode(this), derived,
          //                   System.identityHashCode(derived));
          assert ss.index_shift == -1
              : "bad var "
                  + this
                  + " derived "
                  + derived
                  + " shift "
                  + ss.index_shift
                  + " in ppt "
                  + ppt.name();
        }
      }
    }
  }
}
