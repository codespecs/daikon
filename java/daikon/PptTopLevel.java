package daikon;

import daikon.derive.*;
import daikon.derive.unary.*;
import daikon.derive.binary.*;
import daikon.inv.*;
import daikon.inv.Invariant.OutputFormat;
import daikon.inv.filter.*;
import daikon.inv.unary.scalar.*;
import daikon.inv.unary.string.*;
import daikon.inv.unary.sequence.*;
import daikon.inv.unary.stringsequence.*;
import daikon.inv.ternary.threeScalar.*;
import daikon.simplify.*;
import daikon.split.*;
import daikon.split.misc.*;
import daikon.suppress.*;
import utilMDE.Assert;
import daikon.inv.filter.InvariantFilters;

import java.io.*;
import java.util.*;

import org.apache.oro.text.regex.*;
import org.apache.log4j.Category;

import utilMDE.*;


/**
 * All information about a single program point.
 * A Ppt may also represent just part of the data: see PptConditional.
 * <p>
 * PptTopLevel doesn't do any direct computation, instead deferring that
 * to its views that are slices and that actually contain the invariants.
 * <p>
 * The data layout is as follows:
 * <ul>
 * <li>A PptMap is a collection of PptTopLevel objects.
 * <li>A PptTopLevel contains PptSlice objects, one for each set of
 * variables at the program point.  For instance, if a PptTopLevel has
 * variables a, b, and c, then it has three PptSlice1 objects (one for a;
 * one for b; and one for c), three PptSlice2 objects (one for a,b; one for
 * a,c; and one for b,c), and one PptSlice3 object (for a,b,c).
 * <li>A PptSlice object contains invariants.  When a sample (a tuple of
 * variable values) is fed to a PptTopLevel, it in turn feeds it to all the
 * slices, which feed it to all the invariants, which act on it
 * appropriately.
 * </ul>
 **/
public class PptTopLevel
  extends Ppt
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020731L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /**
   * Boolean.  If true, create implications for all pairwise
   * combinations of conditions, and all pairwise combinations of exit
   * points.  If false, create implications for only the first
   * two conditions, and create implications only if there are
   * exactly two exit points.
   **/
  public static boolean dkconfig_pairwise_implications = false;

  /** Main debug tracer **/
  public static final Category debug =
    Category.getInstance("daikon.PptTopLevel");

  /** Debug tracer for equalTo checks **/
  public static final Category debugEqualTo =
    Category.getInstance("daikon.PptTopLevel.equal");

  /** Debug tracer for addImplications. **/
  public static final Category debugAddImplications =
    Category.getInstance("daikon.PptTopLevel.addImplications");

  /** Debug tracer for data flow. **/
  public static final Category debugFlow =
    Category.getInstance ("daikon.flow.flow");

  /** Debug tracer for start of suppression. **/
  public static final Category debugSuppressInit =
    Category.getInstance ("daikon.suppress.init");

  /** Debug tracer for suppression. **/
  public static final Category debugSuppress =
    Category.getInstance ("daikon.suppress.suppress");

  /** Debug tracer for fillSuppressionTemplate. **/
  public static final Category debugSuppressFill =
    Category.getInstance ("daikon.suppress.fill");

  // Do we need both a num_tracevars for the number of variables in the
  // tracefile and a num_non_derived_vars for the number of variables
  // actually passed off to this Ppt?  The ppt wouldn't use num_tracevars,
  // but it would make sense to store it here anyway.

  // These values are -1 if not yet set (can that happen?). // No they're not
  // Invariant:  num_declvars == num_tracevars + num_orig_vars
  int num_declvars;             // number of variables in the decl file
  int num_tracevars;            // number of variables in the trace file
  int num_orig_vars;            // number of _orig vars
  int num_static_constant_vars; // these don't appear in the trace file

  // private transient VarValuesOrdered values; // [[INCR]]
  private int values_num_samples;
  // [INCR] private int values_num_mod_non_missing_samples;
  // [INCR] private int values_num_values;
  // [INCR] private String values_tuplemod_samples_summary;

  public int getSamplesSeen() {
    return values_num_samples;
  }

  /**
   * All the Views (that is, slices) on this are stored as values in
   * the HashMap
   * Provided so that this Ppt can notify them when significant events
   * occur, such as receiving a new value, deriving variables, or
   * discarding data.
   **/
  private HashMap/*[VarInfo[]->PptSlice]*/ views;

  // Temporarily have a separate collection for PptConditional views.
  // In the long run, I'm not sure whether the two collections will be
  // separate or not.
  // [I'm not sure whether the following is still true in version 3. -MDE]
  // Right now, these are created only after all the values have been seen,
  // so I don't have to get too tense about installing them correctly and
  // iterating over them.  That should be fixed later.  For now, maybe have
  // two methods that add:  one that puts all the values in, one that doesn't.
  public Vector views_cond;

  /**
   * Together, dataflow_ppts and dataflow_tranforms describe how
   * samples that are received at this program point flow to other
   * points.  If samples are not received at this point, both are
   * null.  If samples are received at this point, then both have
   * the same length and:
   *
   * <li>dataflow_ppts includes this (as its last element);
   *
   * <li>dataflow_ppts is ordered by the way samples will flow.  It is
   * a topological sort of the ancestors of this ppt, not just immediate
   * parents.
   *
   * <li>dataflow_transforms contains functions from this to
   * dataflow_ppts; each function is an int[] whose domain is
   * indices of var_infos in this, and whose range is indices of
   * var_infos in the corresponding element of dataflow_ppts;
   *
   * <li>dataflow_transforms describes the function from the var_infos
   * of this ppt to the same variable in dataflow_ppts, so its inner
   * length equals this.var_infos.length;
   *
   * <li>program points in dataflow_ppts may be repeated if a sample
   * at this point induces more than one sample at another point.
   * (For example, if a method has two arguments of type Foo, then a
   * sample for the method induces two different samples at
   * Foo:::OBJECT.)
   **/
  public PptTopLevel[] dataflow_ppts;
  /** @see dataflow_ppts */
  public int[][] dataflow_transforms;

  /**
   * Together, invflow_ppts and invflow_tranforms describe how
   * invariants that are changed or falsified at this program point
   * flow to other points.  They are never null, but may be
   * zero-length if there are no lower ppts.  They obey the following
   * invariants:
   *
   * <li>invflow_transforms contains functions from this to
   * invflow_ppts; each function is an int[] whose domain is
   * indices of var_infos in this, and whose range is indices of
   * var_infos in the corresponding element of invflow_ppts;
   *
   * <li>invflow_transforms describes the function from the var_infos
   * of this ppt to the same variable in invflow_ppts, so its inner
   * length equals this.var_infos.length;
   *
   * <li>program points in invflow_ppts may be repeated if a sample
   * at this point induces more than one sample another point.
   * (For example, if a method has two arguments of type Foo, then a
   * sample for the method induces two different samples at
   * Foo:::OBJECT.)
   **/
  public PptTopLevel[] invflow_ppts;
  /** @see invflow_ppts */
  public int[][] invflow_transforms;

  // [INCR] ...
  // Assumption: The "depends on" graph is acyclic
  // (the graph edges are: <this, (entry_ppt U controlling_ppts)>).
  // This is necessary because we search the graph in isWorthPrinting.
//    public PptTopLevel entry_ppt;             // null if this isn't an exit point
//    public Vector exit_ppts = new Vector(1); // elts are PptTopLevel objects;
//                                  // this is set for entry program points
//    public PptTopLevel combined_exit; // null if this isn't a line-numbered exit point
//    public int[] combined_exit_var_indices; // null if combined_exit == null
  // PptTopLevel has any number of 'controlling' ppts.  Any invariants
  // which exist in the controlling ppts are necessarily true in the
  // controlled ppts, and therefore may be suppressed in the output.
  // For example, public methods are controlled by object invariants,
  // and conditional points are controlled by the unconditional
  // parent point.  This set contains only the immediate controllers,
  // not the transitive closure of all controllers.
//    public Set controlling_ppts = new HashSet(); // elements are PptTopLevel objects
  // ... [INCR]

  // This was renamed to the joiner_view because it no longer just for
  // implications, but instead for any Invariants that represents a
  // "joining" of two others (such as and, or, etc)
  public PptSlice0 joiner_view = new PptSlice0(this);


  /**
   * Holds Equality invariants.  Never null after invariants are
   * instantiated.
   **/
  public PptSliceEquality equality_view;

  // The set of redundant_invs is filled in by the below method
  // mark_implied_via_simplify.  Contents are either Invariant
  // objects, or, in the case of Equality invariants, the canonical
  // VarInfo for the equality.
  public Set redundant_invs = new HashSet(0);

  public PptTopLevel(String name, VarInfo[] var_infos) {
    super(name);
    this.var_infos = var_infos;
    int val_idx = 0;
    num_static_constant_vars = 0;
    for (int i=0; i<var_infos.length; i++) {
      VarInfo vi = var_infos[i];
      vi.varinfo_index = i;
      if (vi.is_static_constant) {
        vi.value_index = -1;
        num_static_constant_vars++;
      } else {
        vi.value_index = val_idx;
        val_idx++;
      }
      vi.ppt = this;
    }
    for (int i=0; i<var_infos.length; i++) {
      VarInfo vi = var_infos[i];
      Assert.assertTrue((vi.value_index == -1) || (!vi.is_static_constant));
    }

    // values = new VarValuesOrdered(); // [[INCR]]
    views = new HashMap();
    views_cond = new Vector();

    num_declvars = var_infos.length;
    num_tracevars = val_idx;
    num_orig_vars = 0;
    Assert.assertTrue(num_static_constant_vars == num_declvars - num_tracevars);
    // System.out.println("Created PptTopLevel " + name + ": "
    //                    + "num_static_constant_vars=" + num_static_constant_vars
    //                    + ",num_declvars=" + num_declvars
    //                    + ",num_tracevars=" + num_tracevars);
  }


  ///////////////////////////////////////////////////////////////////////////
  /// Accessing data
  ///

  public int num_vars() {
    return var_infos.length;
  }

  // Returns true iff this is the only exit point or the combined exit point
  public boolean isMainExit() {
    // [INCR] return (ppt_name.isExitPoint() && (combined_exit == null));
    return ppt_name.isExitPoint();
  }

  // Appears to be used only in the memory monitor.
  public int num_array_vars() {
    int num_arrays=0;
    for (int i=0; i<var_infos.length; i++)
      if (var_infos[i].rep_type.isArray())
        num_arrays++;
    return num_arrays;
  }

  public Iterator var_info_iterator() {
    return Arrays.asList(var_infos).iterator();
  }

  // This method is added as somewhat of a hack for the TreeGUI.  In the
  // gui, PptTopLevel are stored as nodes in a tree.  Swing obtains the
  // string to display in the actual JTree by calling toString().
  public String toString() {
    if (ppt_name.isObjectInstanceSynthetic())   // display "MyClassName : OBJECT"
      return ppt_name.getFullClassName() + " : " + FileIO.object_suffix;
    else if (ppt_name.isClassStaticSynthetic()) // display "MyClassName : CLASS"
      return ppt_name.getFullClassName() + " : " + FileIO.class_static_suffix;
    else                                       // only display "EXIT184"
      return ppt_name.getPoint();
  }

  /** Trim the collections used in this PptTopLevel, in hopes of saving space. **/
  public void trimToSize() {
    super.trimToSize();
    if (views_cond != null) { views_cond.trimToSize(); }
  }

  /** The number of samples processed by this program point so far. **/
  public int num_samples() {
    return values_num_samples;
  }

  // Get the actual views from the HashMap
  Collection viewsAsCollection() {
    return views.values();
  }

  // Quick access to the number of views, since the views variable is private
  public int numViews() {
    return views.size();
  }

  ///////////////////////////////////////////////////////////////////////////
  /// Adding variables
  ///

  // Some of this should perhaps be moved up into Ppt.

  /**
   * Appends vi to the var_infos array of this ppt.  Also sets vi's
   * varinfo_index, value_index, and ppt fields.  Method is
   * non-private so that FileIO can access it; it should not be called
   * by other classes.
   * @param vi must not be a static constant VarInfo
   **/
  void addVarInfo(VarInfo vi) {
    VarInfo[] vis = new VarInfo[] { vi };
    addVarInfos(vis);
  }

  /**
   * Has the effect of performing addVarInfo(VarInfo) over all
   * elements in vis.  Method is not private so that FileIO can access
   * it; should not be called by other classes.
   * @param vis must not contain static constant VarInfos
   * @see addVarInfos(VarInfo)
   **/
  void addVarInfos(VarInfo[] vis) {
    if (vis.length == 0)
      return;
    int old_length = var_infos.length;
    VarInfo[] new_var_infos = new VarInfo[var_infos.length + vis.length];
    System.arraycopy(var_infos, 0, new_var_infos, 0, old_length);
    System.arraycopy(vis, 0, new_var_infos, old_length, vis.length);
    for (int i=old_length; i<new_var_infos.length; i++) {
      VarInfo vi = new_var_infos[i];
      vi.varinfo_index = i;
      vi.value_index = i - num_static_constant_vars;
      vi.ppt = this;
    }
    var_infos = new_var_infos;
  }


  ///////////////////////////////////////////////////////////////////////////
  /// Derived variables
  ///

  // Convenience function for PptConditional initializer (which can't
  // contain statements but can call a function).
  public VarInfo[] trace_and_orig_and_const_vars() {
    // Not ArraysMDE.subarray(var_infos, 0, num_tracevars + num_orig_vars)
    // because its result Object[] cannot be cast to VarInfo[].
    int total_vars = num_tracevars + num_orig_vars + num_static_constant_vars;
    VarInfo[] result = new VarInfo[total_vars];
    System.arraycopy(var_infos, 0, result, 0, total_vars);
    return result;
  }


  // This is here because I think it doesn't make sense to derive except
  // from a PptTopLevel (and possibly a PptConditional?).  Perhaps move it
  // to another class later.

  public static boolean worthDerivingFrom(VarInfo vi) {

    // This prevents derivation from ever occurring on
    // derived variables.  Ought to put this under the
    // control of the individual Derivation objects.

    // System.out.println("worthDerivingFrom(" + vi.name + "): "
    //                    + "derivedDepth=" + vi.derivedDepth()
    //                    + ", isCanonical=" + vi.isCanonical()
    //                    + ", canBeMissing=" + vi.canBeMissing);
    return ((vi.derivedDepth() < 2)
            // && (vi.isCanonical()) // [INCR]
            // && (!vi.canBeMissing) // [[INCR]]
            );

    // Should add this (back) in:
            // && !vi.always_missing()
            // && !vi.always_equal_to_null();

    // Testing for being canonical is going to be a touch tricky when we
    // integrate derivation and inference, because when something becomes
    // non-canonical we'll have to go back and derive from it, etc.  It's
    // almost as if that is a new variable appearing.  But it did appear in
    // the list until it was found to be equal to another and removed from
    // the list!  I need to decide whether the time savings of not
    // processing the non-canonical variables are worth the time and
    // complexity of making variables non-canonical and possibly canonical
    // again.

  }


  // final static int derivation_passes = 2; // [INCR]

  // To verify that these are all the factories of interest, do
  // cd ~/research/invariants/daikon/derive; search -i -n 'extends.*derivationfactory'

  transient UnaryDerivationFactory[] unaryDerivations
    = new UnaryDerivationFactory[] {
        new SequenceLengthFactory(),
        new SequenceInitialFactory(),
        new SequenceMinMaxSumFactory(),
        new SequenceInitialFactoryFloat(),
    };

  transient BinaryDerivationFactory[] binaryDerivations
    = new BinaryDerivationFactory[] {
        // subscript
        new SequenceScalarSubscriptFactory(),
        new SequenceFloatSubscriptFactory(),
        new SequenceStringSubscriptFactory(),
        // intersection
        new SequenceScalarIntersectionFactory(),
        new SequenceFloatIntersectionFactory(),
        new SequenceStringIntersectionFactory(),
        // union
        new SequenceScalarUnionFactory(),
        new SequenceFloatUnionFactory(),
        new SequenceStringUnionFactory(),
        // other
        new SequencesConcatFactory(),
        new SequencesJoinFactory(),
        new SequencesPredicateFactory(),
    };


  /* * [INCR] This is dead code now.
   *
   *
   * This does no inference; it just calls deriveVariablesOnePass once per pass.
   * It returns a Vector of Derivation objects.<p>
   *
   * If derivation_index == (a, b, c) and n = len(var_infos), then
   * the body of this loop:
   * <li>
   *   does pass1 introduction for b..a
   * <li>
   *   does pass2 introduction for c..b
   * <br>
   * and afterward, derivation_index == (n, a, b).
   * @return Vector of VarInfo
   * */
  /* [INCR] ... we longer need to do this in stages
  public Vector __derive() {
    Assert.assertTrue(ArraysMDE.sorted_descending(derivation_indices));

    Vector result = new Vector();
    for (int pass=1; pass<=derivation_passes; pass++) {
      int this_di = derivation_indices[pass];
      int last_di = derivation_indices[pass-1];
      if (Global.debugDerive.isDebugEnabled())
        Global.debugDerive.debug("pass=" + pass + ", range=" + this_di + ".." + last_di);
      if (this_di == last_di) {
        if (Global.debugDerive.isDebugEnabled()) {
          Global.debugDerive.debug("No pass " + pass + " derivation to do");
        }
        continue;
      }
      result.addAll(deriveVariablesOnePass(this_di, last_di,
                                           unaryDerivations[pass-1],
                                           binaryDerivations[pass-1]));
    }
    // shift values in derivation_indices:  convert [a,b,c] into [n,a,b]
    for (int i=derivation_passes; i>0; i--)
      derivation_indices[i] = derivation_indices[i-1];
    derivation_indices[0] = var_infos.length + result.size();

    if (Global.debugDerive.isDebugEnabled()) {
      Global.debugDerive.debug(name + ": derived " + result.size()
                               + " new variables; "
                               + "new derivation_indices: "
                               + ArraysMDE.toString(derivation_indices));
      // Alternately, and probably more usefully
      for (int i=0; i<result.size(); i++) {
        Global.debugDerive.debug
          ("  " + ((Derivation)result.get(i)).getVarInfo().name.name());
      }
    }
    return result;
  }
  */ // ... [INCR]

  /**
   * This routine creates derivations for one "pass"; that is, it adds
   * some set of derived variables.
   * All the results involve VarInfo objects at
   * indices i such that vi_index_min <= i < vi_index_limit (and
   * possibly also involve other VarInfos).
   * @return a Vector of VarInfo
   **/
  /* [INCR] let's rename this
  Vector deriveVariablesOnePass(int vi_index_min, int vi_index_limit, UnaryDerivationFactory[] unary, BinaryDerivationFactory[] binary) {
  */
  private Derivation[] derive(int vi_index_min,
                              int vi_index_limit)
  {
    UnaryDerivationFactory[] unary = unaryDerivations;
    BinaryDerivationFactory[] binary = binaryDerivations;

    if (Global.debugDerive.isDebugEnabled()) {
      Global.debugDerive.debug("Deriving one pass for ppt " + this.name);
      Global.debugDerive.debug("vi_index_min=" + vi_index_min
                               + ", vi_index_limit=" + vi_index_limit
                               + ", unary.length=" + unary.length
                               + ", binary.length=" + binary.length);
    }

    Collection result = new ArrayList();

    for (int i=vi_index_min; i<vi_index_limit; i++) {
      VarInfo vi = var_infos[i];
      if (Global.debugDerive.isDebugEnabled()) {
        Global.debugDerive.debug("Unary: trying to derive from " + vi.name.name());
      }
      if (!worthDerivingFrom(vi)) {
        if (Global.debugDerive.isDebugEnabled()) {
          Global.debugDerive.debug("Unary: not worth deriving from " + vi.name.name());
          // [INCR] Global.debugDerive.debug("Canonicality is: " + vi.isCanonical());
          // [INCR] Global.debugDerive.debug("Equal_to: " + vi.equal_to.name.name());
        }
        continue;
      }
      for (int di=0; di<unary.length; di++) {
        UnaryDerivationFactory udf = unary[di];
        UnaryDerivation[] uderivs = udf.instantiate(vi);
        if (uderivs != null) {
          for (int udi=0; udi<uderivs.length; udi++) {
            UnaryDerivation uderiv = uderivs[udi];
            if ((Daikon.var_omit_regexp != null)
                && Global.regexp_matcher.contains(uderiv.getVarInfo().name.name(), Daikon.var_omit_regexp)) {
              continue;
            }
            result.add(uderiv);
          }
        }
      }
    }

    // I want to get all pairs of variables such that at least one of the
    // variables is under consideration, but I want to generate each such
    // pair only once.  This probably isn't the most efficient technique,
    // but it's probably adequate and is not excessively complicated or
    // excessively slow.
    for (int i1=0; i1<var_infos.length; i1++) {
      VarInfo vi1 = var_infos[i1];
      if (!worthDerivingFrom(vi1)) {
        if (Global.debugDerive.isDebugEnabled()) {
          Global.debugDerive.debug("Binary first VarInfo: not worth deriving from " + vi1.name.name());
        }
        continue;
      }
      // This guarantees that at least one of the variables is under
      // consideration.
      // target1 indicates whether the first variable is under consideration.
      boolean target1 = (i1 >= vi_index_min) && (i1 < vi_index_limit);
      int i2_min, i2_limit;
      if (target1) {
        i2_min = i1+1;
        i2_limit = var_infos.length;
      } else {
        i2_min = Math.max(i1+1, vi_index_min);
        i2_limit = vi_index_limit;
      }
      // if (Global.debugDerive.isDebugEnabled())
      //   Global.debugDerive.debug("i1=" + i1
      //                      + ", i2_min=" + i2_min
      //                      + ", i2_limit=" + i2_limit);
      for (int i2=i2_min; i2<i2_limit; i2++) {
        VarInfo vi2 = var_infos[i2];
        if (!worthDerivingFrom(vi2)) {
          if (Global.debugDerive.isDebugEnabled()) {
            Global.debugDerive.debug("Binary: not worth deriving from ("
                               + vi1.name.name() + "," + vi2.name.name() + ")");
            /* [INCR]
            Global.debugDerive.debug("Canonicality is: " + vi2.isCanonical());
            Global.debugDerive.debug("Equal_to: " + vi2.equal_to.name.name());
            */ // [INCR]
          }
          continue;
        }
        for (int di=0; di<binary.length; di++) {
          BinaryDerivationFactory d = binary[di];
          BinaryDerivation[] bderivs = d.instantiate(vi1, vi2);
          if (bderivs != null) {
            for (int bdi=0; bdi<bderivs.length; bdi++) {
              BinaryDerivation bderiv = bderivs[bdi];
              if ((Daikon.var_omit_regexp != null)
                  && Global.regexp_matcher.contains(bderiv.getVarInfo().name.name(), Daikon.var_omit_regexp)) {
                continue;
              }
              result.add(bderiv);
            }
          }
        }
      }
    }

    if (Global.debugDerive.isDebugEnabled()) {
      Global.debugDerive.debug ("Number of derived variables at program point " + this.name + ": " + result.size());
      String derived_vars = "Derived:";
      for (Iterator itor = result.iterator(); itor.hasNext(); ) {
        derived_vars += " " + ((Derivation)itor.next()).getVarInfo().name.name();
      }
      Global.debugDerive.debug(derived_vars);
    }
    Derivation[] result_array =
      (Derivation[]) result.toArray(new Derivation[result.size()]);
    return result_array;
  }


  ///
  /// Adding derived variables
  ///

  // This doesn't compute what the derived variables should be, just adds
  // them after being computed.

  // derivs is a Vector of Derivation objects
  void __addDerivedVariables(Vector derivs) {
    Derivation[] derivs_array
      = (Derivation[]) derivs.toArray(new Derivation[0]);
    __addDerivedVariables(derivs_array);
  }

  void __addDerivedVariables(Derivation[] derivs) {

    VarInfo[] vis = new VarInfo[derivs.length];
    for (int i=0; i<derivs.length; i++) {
      vis[i] = derivs[i].getVarInfo();
    }
    addVarInfos(vis);

    // Since I am only modifying members, not making new objects, and since
    // I am using an Eq hash table, I don't need to rehash.
    // values.extend(derivs); // [[INCR]]
    // XXXXX
  }




  ///////////////////////////////////////////////////////////////////////////
  /// Manipulating values
  ///

  /**
   * Given a sample that was observed at this ppt, flow it up to
   * any higher ppts and lastly to this ppt.  Hit conditional
   * ppts along the way (via the add method).
   * @param vt the set of values for this and higher ppts to see
   * @param count the number of samples that vt represents
   *
   * Contract: since we hit higher ppts first and check this last,
   * invariants that have flown down from the higher ppt are also
   * checked by this vt.  If we hit this before parents, then the
   * flow wouldn't work.
   **/
  public void add_and_flow(ValueTuple vt, int count) {
    //     if (debugFlow.isDebugEnabled()) {
    //       debugFlow.debug ("add_and_flow for " + ppt_name);
    //     }

    // Doable, but commented out for efficiency
    // repCheck(); 

    // System.out.println("PptTopLevel " + name + ": add " + vt);
    Assert.assertTrue(vt.size() == var_infos.length - num_static_constant_vars);

    // The way adding samples works: We have precomputed program
    // points that have any VarInfos that are higher than this point's
    // VarInfos, and a transformation vector that maps the variable
    // index at this point to the variable index in the higher point.
    // Simply walk down that list, transforming value tuples according
    // to transormation vectors.  Then call add of the right program points.

    Assert.assertTrue(dataflow_ppts != null, name);
    Assert.assertTrue(dataflow_transforms != null, name);
    Assert.assertTrue(dataflow_ppts.length == dataflow_transforms.length, name);

    if (debugFlow.isDebugEnabled()) {
      debugFlow.debug ("<<< Doing add_and_flow() for " + name);
    }

    for (int i=0; i < dataflow_ppts.length; i++) {
      PptTopLevel ppt = dataflow_ppts[i];
      //       if (debugFlow.isDebugEnabled()) {
      //        debugFlow.debug ("add_and_flow: A parent is " + ppt.ppt_name);
      //       }

      int[] transform = dataflow_transforms[i];
      Assert.assertTrue(transform.length == var_infos.length);

      // Map vt into the transformed tuple
      int ppt_num_vals = ppt.var_infos.length - ppt.num_static_constant_vars;
      Object[] vals = new Object[ppt_num_vals];
      int[] mods = new int[ppt_num_vals];
      Arrays.fill(mods, ValueTuple.MISSING_FLOW);
      for (int j=0; j < transform.length; j++) {
        int tj = transform[j];
        if (tj == -1) continue;
        int this_value_index = var_infos[j].value_index;
        if (this_value_index == -1) continue; // is_static_constant
        int ppt_value_index = ppt.var_infos[tj].value_index;
        vals[ppt_value_index] = vt.vals[this_value_index];
        mods[ppt_value_index] = vt.mods[this_value_index];
      }
      ValueTuple ppt_vt = new ValueTuple(vals, mods);

      ppt.add(ppt_vt, count);
    }

  }

  /**
   * Add the sample to the invariants at this program point and any
   * child conditional program points, but do not flow the sample to
   * other related ppts.
   *
   * @param vt the set of values for this to see
   * @param count the number of samples that vt represents
   **/
  public List add(ValueTuple vt, int count) {
    // Doable, but commented out for efficiency
    // repCheck(); 

    // System.out.println("PptTopLevel " + name + ": add " + vt);
    Assert.assertTrue(vt.size() == var_infos.length - num_static_constant_vars, name);

    //     if (debugFlow.isDebugEnabled()) {
    //       debugFlow.debug ("Add for " + this.name);
    //     }

    if (debugSuppress.isDebugEnabled()) {
      debugSuppress.debug ("<<< Doing add for " + name);
      // debugSuppress.debug ("    with vt " + vt);
    }

    if (debugFlow.isDebugEnabled()) {
      debugFlow.debug ("<<< Doing add for " + name);
      debugFlow.debug ("    with vt " + vt);
    }

    if (values_num_samples == 0) {
      debugFlow.debug ("  Instantiating views for the first time");
      instantiate_views_and_invariants();

      if (Global.debugInfer.isDebugEnabled()) {
        Global.debugInfer.debug ("Instantiated views first time for " + this);
      }
    }

    if (!initiatedSuppression &&
        values_num_samples >= Daikon.suppress_samples_min) {
      initiateSuppression();
    }

    if (Daikon.use_equality_set) {
      equality_view.add (vt, count);
    }

    values_num_samples += count;

    Set viewsToCheck = new HashSet(viewsAsCollection());

    int checkCount = 0;
    Invariants invsFlowed = new Invariants();

    while (viewsToCheck.size() > 0) {
      checkCount++;

      if (debugSuppress.isDebugEnabled()) {
        debugSuppress.debug ("  Checkcount: "+ checkCount);
      }

      Set weakenedInvs = new HashSet();
      // Add to all the views
      for (Iterator itor = viewsToCheck.iterator() ; itor.hasNext() ; ) {
        PptSlice view = (PptSlice) itor.next();
        if (view.invs.size() == 0) {
          System.err.println("No invs for " + view.name);
          continue;
        }
        if (!view.no_invariants) {
          weakenedInvs.addAll (view.add(vt, count));
        }
      }

      viewsToCheck = new HashSet();
      // Checking of recently unsuppressed invariants done here.  For
      // every invariant that got unsuppressed: 1) Check if other
      // invariants still suppress it; 2) Check if the current vt
      // falsifies it (by placing it in viewsToCheck, which is checked
      // on the subsequent loop).  Remember that this.invariants can
      // suppress invariants in lower ppts, so we only do (2) for
      // unsuppressed invariants in this.invariants.

      for (Iterator itor = weakenedInvs.iterator(); itor.hasNext(); ) {
        Invariant inv = (Invariant) itor.next();
        // Why copy?  Because we want to keep unlink() as an atomic
        // operation that removes the SuppressionLink from the
        // suppressor's suppressed field.  Without copying, we get a
        // ConcurrentModifiecationException.
        Set suppressees = new HashSet(inv.getSuppressees());

        if (debugSuppress.isDebugEnabled() && suppressees.size() > 0) {
          debugSuppress.debug (" Inv " + inv.repr() +
                               " was falsified or weakened with suppressees");
        }
        // Try to resuppress the weakened inv
        // Why is this useful?  There may be isSameFormula comparisons
        // among suppressors such that the weakened form of the inv
        // qualifies (e.g. LowerBound)
        if (!inv.falsified && inv.getSuppressor() == null) {
          if (attemptSuppression (inv)) {
            if (debugSuppress.isDebugEnabled()) {
              debugSuppress.debug ("Suppressor res-suppressed");
            }
          }
        }

        for (Iterator iSuppressees = suppressees.iterator();
             iSuppressees.hasNext(); ) {
          SuppressionLink sl = (SuppressionLink) iSuppressees.next();
          Invariant invSuppressed = sl.getSuppressee();
          sl.unlink();
          Assert.assertTrue (invSuppressed.getSuppressor() == null);
          if (debugSuppress.isDebugEnabled()) {
            debugSuppress.debug ("  Attempting re-suppression of: " + invSuppressed.repr());
          }
          PptTopLevel suppressedPpt = invSuppressed.ppt.parent;
          if (attemptSuppression (invSuppressed)) {
            if (debugSuppress.isDebugEnabled()) {
              debugSuppress.debug ("  Re-suppressed by " + invSuppressed.getSuppressor());
            }
          } else if (suppressedPpt == this) {
            // If invSuppressed didn't get resuppressed, we have to check values
            debugSuppress.debug ("  Will re-check because in same ppt");
            viewsToCheck.add (invSuppressed.ppt);
          } else {
            // Do nothing because suppressedParent is a child of this,
            // and will be checked in good time.
          }
        }
      }
    }

    for (Iterator itor = views_iterator() ; itor.hasNext() ; ) {
      PptSlice view = (PptSlice) itor.next();
      if (view.invs.size() == 0) {
        itor.remove();
        if (Global.debugInfer.isDebugEnabled()) {
          Global.debugInfer.debug("add(ValueTulple,int): slice died: " + name + view.varNames());
        }
      }
    }

    // Add to all the conditional ppts
    for (Iterator itor = views_cond.iterator() ; itor.hasNext() ; ) {
      PptConditional pptcond = (PptConditional) itor.next();
      pptcond.add(vt, count);
      // TODO: Check for no more invariants on pptcond?
    }

    if (debugSuppress.isDebugEnabled()) {
      debugSuppress.debug (">>> End of add for " + name);
    }

    return new ArrayList();
  }

  /**
   * Create all the derived variables.
   **/
  public void create_derived_variables() {
    if (debug.isDebugEnabled())
      debug.debug("create_derived_variables for " + name);

    int first_new = var_infos.length;
    // Make ALL of the derived variables.  The loop terminates
    // because derive() stops creating derived variables after some
    // depth.  Within the loop, [lower..upper) need deriving from.
    int lower = 0;
    int upper = var_infos.length;
    while (lower < upper) {
      Derivation[] ders = derive(lower, upper);
      lower = upper;
      upper += ders.length;

      VarInfo[] vis = new VarInfo[ders.length];
      for (int i=0; i < ders.length; i++) {
        vis[i] = ders[i].getVarInfo();
      }
      if (Global.debugDerive.isDebugEnabled()) {
        for (int i=0; i < ders.length; i++) {
          Global.debugDerive.debug("Derived " + vis[i].name.name());
        }
      }

      // Using addDerivedVariables(derivations) would add data too
      addVarInfos(vis);
    }
    Assert.assertTrue(lower == upper);
    Assert.assertTrue(upper == var_infos.length);

    if (debug.isDebugEnabled())
      debug.debug("Done with create_derived_variables, " + var_infos.length + " vars");
  }

  /**
   * This function is called to jump-start processing; it creates all
   * the views (and thus candidate invariants), but does not check
   * those invariants.
   **/
  public void instantiate_views_and_invariants() {
    if (debug.isDebugEnabled()) 
      debug.debug("instantiate_views_and_invariants for " + name);

    // Now make all of the views (and thus candidate invariants)
    instantiate_views(0, var_infos.length);

    if (debug.isDebugEnabled())
      debug.debug("Done with instantiate_views_and_invariants");
  }


  ///////////////////////////////////////////////////////////////////////////
  /// Creating invariants
  ///

  // I can't decide which loop it's more efficient to make the inner loop:
  // the loop over samples or the loop over slices.

  // slices_vector is a Vector of PptSlice; this routine does not modify it.
  // Maybe this should return the rejected views.
  private void addViews(Vector slices_vector) {
    if (slices_vector.isEmpty())
      return;

    // Don't modify the actual parameter
    slices_vector = (Vector) slices_vector.clone();

    // This might be a brand-new Slice, and instantiate_invariants for this
    // pass might not have come up with any invariants.
    for (Iterator itor = slices_vector.iterator(); itor.hasNext(); ) {
      PptSlice slice = (PptSlice) itor.next();
      if (slice.invs.size() == 0) {
        // removes the element from slices_vector
        itor.remove();
        if (Global.debugInfer.isDebugEnabled()) {
          Global.debugInfer.debug("addViews: not adding " + slice + " due to no invariants");
        }
      }
    }

    addSlices(slices_vector);
  }

  /**
   * Add a collection of slices to the views of a PptTopLevel
   **/
  private void addSlices(Collection slices) {
    for (Iterator i=slices.iterator(); i.hasNext(); ) {
      addSlice((PptSlice)i.next());
    }
  }

  /**
   * Add a single slice to the views variable
   **/
  void addSlice(PptSlice slice) {
    views.put(Arrays.asList(slice.var_infos),slice);
  }

  /**
   * Remove a slice from this PptTopLevel.
   **/
  private void removeSlice (PptSlice slice) {
    Object o = views.remove(Arrays.asList(slice.var_infos));
    Assert.assertTrue (o != null);    
  }

  /**
   * Used to be a part of addViews, but for right now (Daikon V3) we
   * just want to set up all of the invariants, not actually feed them
   * data.
   **/
  private void __addViewsData(Vector slices_vector)
  {
    // use an array because iterating over it will be more efficient, I suspect.
    PptSlice[] slices = (PptSlice[])
      slices_vector.toArray(new PptSlice[slices_vector.size()]);
    int num_slices = slices.length;

    // System.out.println("Adding views for " + name);
    // for (int i=0; i<slices.length; i++) {
    //   System.out.println("  View: " + slices[i].name);
    // }
    // values.dump();

    // System.out.println("Number of samples for " + name + ": "
    //                    + values.num_samples()
    //                    + ", number of values: " + values.num_values());
    // If I recorded mod bits in value.ValueSet(), I could use it here instead.
//      for (Iterator vt_itor = values.sampleIterator(); vt_itor.hasNext(); ) {
//        VarValuesOrdered.ValueTupleCount entry = (VarValuesOrdered.ValueTupleCount) vt_itor.next();
//        ValueTuple vt = entry.value_tuple;
//        int count = entry.count;
//        for (int i=0; i<num_slices; i++) {
//          // System.out.println("" + slices[i] + " .add(" + vt + ", " + count + ")");
//          slices[i].add(vt, count);
//        }
//        if (views_to_remove_deferred.size() > 0) {
//          // Inefficient, but easy to code.
//          Assert.assertTrue(slices_vector.containsAll(views_to_remove_deferred));
//          slices_vector.removeAll(views_to_remove_deferred);
//          views_to_remove_deferred.clear();
//          if (slices_vector.size() == 0)
//            break;
//          slices = (PptSlice[]) slices_vector.toArray(new PptSlice[0]);
//          num_slices = slices.length;
//        }
//      }
  }

  public void removeView(Ppt slice) {
    // System.out.println("removeView " + slice.name + " " + slice);
    boolean removed = viewsAsCollection().remove(slice);
    Assert.assertTrue(removed);
  }

  // I've decided that views will contain only slices, which allows for
  // dramatic speedups in finding functions

  // The nouns "view" and "slice" are putatively different.  Slices
  // limit the variables but examine all data.  Views may ignore data,
  // etc.  In practive, getView always returns a slice anyway (see
  // comments on class daikon.Ppt).

  /**
   * Typically one should use the dynamic_constant or canBeMissing slots,
   * which cache the invariants of most interest, instead of this function.
   **/
  public PptSlice1 getView(VarInfo vi) {
    // This seems to do the same thing as findSlice(vi)

    return findSlice(vi);

    //      for (Iterator itor = views_iterator(); itor.hasNext(); ) {
    //        PptSlice slice = (PptSlice) itor.next();
    //        if ((slice.arity == 1) && slice.usesVar(vi))
    //          return (PptSlice1) slice;
    //      }
    //      return null;
  }

  /**
   * Typically one should use the equal_to slot, which caches the
   * invariants of most interest, instead of this function.
   **/
  public PptSlice2 getView(VarInfo vi1, VarInfo vi2) {
    // This seems to do the same thing as findSlice(vi1, vi2)...

    return findSlice(vi1, vi2);

    //      for (Iterator itor = views_iterator(); itor.hasNext(); ) {
    //        PptSlice slice = (PptSlice) itor.next();
    //        if ((slice.arity == 2) && slice.usesVar(vi1) && slice.usesVar(vi2))
    //          return (PptSlice2) slice;
    //      }
    //      return null;
  }

  // A slice is a specific kind of view, but we don't call this
  // findView because it doesn't find an arbitrary view.
  /**
   * findSlice can return null if the slice doesn't exist.  That can happen
   * if there are no true invariants over the set of variables -- when the
   * last invariant is removed, so is the slice.
   *
   * When one is looking for a particular invariant, typically one should
   * use the dynamic_constant or canBeMissing slots, which cache the
   * invariants of most interest, instead of calling function to get the
   * slice and then looking for the invariant in the slice.
   **/
  public PptSlice1 findSlice(VarInfo v) {
    return (PptSlice1)findSlice(new VarInfo [] {v});
    //      for (Iterator itor = views_iterator() ; itor.hasNext() ; ) {
    //        PptSlice view = (PptSlice) itor.next();
    //        if ((view.arity == 1) && (v == view.var_infos[0]))
    //          return (PptSlice1) view;
    //      }
    //      return null;
  }

  /**
   * findSlice can return null if the slice doesn't exist.  That can happen
   * if there are no true invariants over the set of variables -- when the
   * last invariant is removed, so is the slice.
   *
   * When one is looking for a particular invariant, typically one should
   * use the dynamic_constant or canBeMissing slots, which cache the
   * invariants of most interest, instead of calling function to get the
   * slice and then looking for the invariant in the slice.
   **/
  public PptSlice2 findSlice(VarInfo v1, VarInfo v2) {
    Assert.assertTrue(v1.varinfo_index < v2.varinfo_index);
    return (PptSlice2)findSlice(new VarInfo [] {v1, v2});
    //      for (Iterator itor = views_iterator() ; itor.hasNext() ; ) {
    //        PptSlice view = (PptSlice) itor.next();
    //        if ((view.arity == 2)
    //            && (v1 == view.var_infos[0])
    //            && (v2 == view.var_infos[1]))
    //          return (PptSlice2) view;
    //      }
    //      return null;
  }

  /**
   * Like findSlice, but it is not required that the variables be supplied
   * in order of varinfo_index.
   **/
  public PptSlice2 findSlice_unordered(VarInfo v1, VarInfo v2) {
    Assert.assertTrue(v1.varinfo_index != v2.varinfo_index);
    if (v1.varinfo_index < v2.varinfo_index) {
      return findSlice(v1, v2);
    } else {
      return findSlice(v2, v1);
    }
  }

  /**
   * findSlice can return null if the slice doesn't exist.  That can happen
   * if there are no true invariants over the set of variables -- when the
   * last invariant is removed, so is the slice.
   *
   * When one is looking for a particular invariant, typically one should
   * use the dynamic_constant or canBeMissing slots, which cache the
   * invariants of most interest, instead of calling function to get the
   * slice and then looking for the invariant in the slice.
   **/
  public PptSlice3 findSlice(VarInfo v1, VarInfo v2, VarInfo v3) {
    Assert.assertTrue(v1.varinfo_index < v2.varinfo_index);
    Assert.assertTrue(v2.varinfo_index < v3.varinfo_index);
    return (PptSlice3)findSlice(new VarInfo [] {v1, v2, v3});
    //      for (Iterator itor = views_iterator() ; itor.hasNext() ; ) {
    //        PptSlice view = (PptSlice) itor.next();
    //        if ((view.arity == 3)
    //            && (v1 == view.var_infos[0])
    //            && (v2 == view.var_infos[1])
    //            && (v3 == view.var_infos[2]))
    //          return (PptSlice3) view;
    //      }
    //      return null;
  }

  /**
   * Like findSlice, but it is not required that the variables be supplied
   * in order of varinfo_index.
   **/
  public PptSlice3 findSlice_unordered(VarInfo v1, VarInfo v2, VarInfo v3) {
    // bubble sort is easier than 3 levels of if-then-else
    VarInfo tmp;
    if (v1.varinfo_index > v2.varinfo_index) { tmp = v2; v2 = v1; v1 = tmp; }
    if (v2.varinfo_index > v3.varinfo_index) { tmp = v3; v3 = v2; v2 = tmp; }
    if (v1.varinfo_index > v2.varinfo_index) { tmp = v2; v2 = v1; v1 = tmp; }
    return (PptSlice3)findSlice(v1, v2, v3);
  }

  /**
   * Find a pptSlice without an assumed ordering.
   **/
  public PptSlice findSlice_unordered(VarInfo[] vis) {
    switch (vis.length) {
    case 1: return findSlice(vis[0]);
    case 2: return findSlice_unordered(vis[0], vis[1]);
    case 3: return findSlice_unordered(vis[0], vis[1], vis[2]);
    default:
      throw new RuntimeException("Bad length " + vis.length);
    }
  }

  /**
   * Find a pptSlice with an assumed ordering.
   **/
  public PptSlice findSlice(VarInfo[] vis) {
    if (vis.length > 3) {
      throw new RuntimeException("Bad length " + vis.length);
    }
    return (PptSlice)views.get(Arrays.asList(vis));
  }

  public int indexOf(String varname) {
    for (int i=0; i<var_infos.length; i++) {
      if (var_infos[i].name.name().equals(varname)) {
        return i;
      }
    }
    return -1;
  }


  // At present, this needs to occur after deriving variables, because
  // I haven't integrated derivation and inference yet.
  // (This function doesn't exactly belong in this part of the file.)

  // Should return a list of the views created, perhaps.


  /**
   * Install views and the invariants.  We create NO views over static
   * constant variables, but everything else is fair game.  We don't
   * create views over variables which have a higher (controlling)
   * view.  This function does NOT cause invariants over the new views
   * to be checked (but it does create invariants).  The installed
   * views and invariants will all have at least one element with
   * index i such that vi_index_min <= i < vi_index_limit.  (However,
   * we also assume that vi_index_limit == var_infos.length.)
   **/
  private void instantiate_views(int vi_index_min,
                                 int vi_index_limit)
  {
    if (Global.debugInfer.isDebugEnabled())
      Global.debugInfer.debug("instantiate_views: " + this.name
                           + ", vi_index_min=" + vi_index_min
                           + ", vi_index_limit=" + vi_index_limit
                           + ", var_infos.length=" + var_infos.length);

    // This test prevents instantiate views for variables one at a time.
    Assert.assertTrue(var_infos.length == vi_index_limit);

    if (vi_index_min == vi_index_limit)
      return;

    // used only for debugging
    int old_num_vars = var_infos.length;
    int old_num_views = views.size();

    /// 1. all unary views

    // Unary slices/invariants.
    Vector unary_views = new Vector(vi_index_limit-vi_index_min);
    for (int i=vi_index_min; i<vi_index_limit; i++) {      
      VarInfo vi = var_infos[i];

      // Debug
      if (!vi.isCanonical()) continue;

      // Eventually, add back in this test as "if constant and no
      // comparability info exists" then continue.
      // if (vi.isStaticConstant()) continue;
      PptSlice1 slice1 = new PptSlice1(this, vi);
      // Can't do this anymore, because we're using equality.  Use
      // SelfSuppressionFactories instead
      //       if (slice1.isControlled()) {
      //         // let invariant flow from controlling slice
      //         if (Global.debugInfer.isDebugEnabled())
      //           Global.debugInfer.debug("Skipping " + slice1.name + "; is controlled(1).");
      //         continue;
      //       }
      slice1.instantiate_invariants(false);
      unary_views.add(slice1);
    }
    addViews(unary_views);
    unary_views = null;

    /// 2. all binary views

    // Binary slices/invariants.
    Vector binary_views = new Vector();
    for (int i1=0; i1<vi_index_limit; i1++) {
      VarInfo var1 = var_infos[i1];
      if (!var1.isCanonical()) continue;

      // Eventually, add back in this test as "if constant and no
      // comparability info exists" then continue.
      // if (var1.isStaticConstant()) continue;
      boolean target1 = (i1 >= vi_index_min) && (i1 < vi_index_limit);
      int i2_min = (target1 ? i1+1 : Math.max(i1+1, vi_index_min));
      for (int i2=i2_min; i2<vi_index_limit; i2++) {
        VarInfo var2 = var_infos[i2];
        if (!var2.isCanonical()) continue;

        // Eventually, add back in this test as "if constant and no
        // comparability info exists" then continue.
        // if (var2.isStaticConstant()) continue;
        PptSlice2 slice2 = new PptSlice2(this, var1, var2);
        //         if (slice2.isControlled()) {
        //           // let invariant flow from controlling slice
        //           if (Global.debugInfer.isDebugEnabled())
        //             Global.debugInfer.debug("Skipping " + slice2.name + "; is controlled(2).");
        //           continue;
        //         }
        slice2.instantiate_invariants(false);
        binary_views.add(slice2);
      }
    }
    addViews(binary_views);
    binary_views = null;

    // 3. all ternary views
    if (! Daikon.disable_ternary_invariants) {
      if (Global.debugInfer.isDebugEnabled()) {
        Global.debugInfer.debug ("Trying ternary slices for " + this.ppt_name);
      }

      Vector ternary_views = new Vector();
      for (int i1=0; i1<vi_index_limit; i1++) {
        VarInfo var1 = var_infos[i1];
        if (!var1.isCanonical()) continue;
      
        // Eventually, add back in this test as "if constant and no
        // comparability info exists" then continue.
        // if (var1.isStaticConstant()) continue;
        // For now, only ternary invariants not involving any arrays
        if (var1.rep_type.isArray())
          continue;

        boolean target1 = (i1 >= vi_index_min) && (i1 < vi_index_limit);
        for (int i2=i1+1; i2<vi_index_limit; i2++) {
          VarInfo var2 = var_infos[i2];
          if (!var2.isCanonical()) continue;

          // Eventually, add back in this test as "if constant and no
          // comparability info exists" then continue.
          // if (var2.isStaticConstant()) continue;
          // For now, only ternary invariants not involving any arrays
          if (var2.rep_type.isArray())
            continue;

          boolean target2 = (i2 >= vi_index_min) && (i2 < vi_index_limit);
          int i3_min = ((target1 || target2) ? i2+1 : Math.max(i2+1, vi_index_min));
          for (int i3=i3_min; i3<vi_index_limit; i3++) {
            Assert.assertTrue(((i1 >= vi_index_min) && (i1 < vi_index_limit))
                          || ((i2 >= vi_index_min) && (i2 < vi_index_limit))
                          || ((i3 >= vi_index_min) && (i3 < vi_index_limit)));
            Assert.assertTrue((i1 < i2) && (i2 < i3));
            VarInfo var3 = var_infos[i3];
            if (!var3.isCanonical()) continue;

            // Eventually, add back in this test as "if constant and no
            // comparability info exists" then continue.
            // if (var3.isStaticConstant()) continue;
            // For now, only ternary invariants not involving any arrays
            if (var3.rep_type.isArray())
              continue;

            if ((! var1.compatible(var2)
                 || (! var1.compatible(var3)))) {
              continue;
            }

            PptSlice3 slice3 = new PptSlice3(this, var1, var2, var3);
            //             if (slice3.isControlled()) {
            //               // let invariant flow from controlling slice
            //               if (Global.debugInfer.isDebugEnabled())
            //                 Global.debugInfer.debug("Skipping " + slice3.name + "; is controlled(3).");
            //               continue;
            //             }
            slice3.instantiate_invariants(false);
            if (Global.debugInfer.isDebugEnabled()) {
              Global.debugInfer.debug("Instantiated for PptSlice3");
            }
            ternary_views.add(slice3);
          }
        }
      }
      addViews(ternary_views);
    }

    if (Global.debugInfer.isDebugEnabled()) {
      Global.debugInfer.debug(views.size() - old_num_views + " new views for " + name);
    }

    // This method didn't add any new variables.
    Assert.assertTrue(old_num_vars == var_infos.length);
    repCheck();
  }

  /**
   * Return a slice that contains the given VarInfos (creating if
   * needed).  It is incumbent on the caller that the slice be either
   * filled with one or more invariants, or else removed from the
   * views collection.
   **/
  public PptSlice get_or_instantiate_slice(VarInfo[] vis) {
    PptSlice result = findSlice_unordered(vis);
    if (result != null) return result;

    switch (vis.length) {
    case 1: {
      VarInfo vi = vis[0];
      // We may do inference over static constants
      // Assert.assertTrue(! vi.isStaticConstant());
      result = new PptSlice1(this, vi);
      break;
    }
    case 2: {
      VarInfo v1 = vis[0];
      VarInfo v2 = vis[1];
      // We may do inference over static constants
      // Assert.assertTrue(! v1.isStaticConstant());
      // Assert.assertTrue(! v2.isStaticConstant());
      VarInfo tmp;
      if (v1.varinfo_index > v2.varinfo_index) { tmp = v2; v2 = v1; v1 = tmp; }
      result = new PptSlice2(this, v1, v2);
      break;
    }
    case 3: {
      VarInfo v1 = vis[0];
      VarInfo v2 = vis[1];
      VarInfo v3 = vis[2];
      // We may do inference over static constants
      // Assert.assertTrue(! v1.isStaticConstant());
      // Assert.assertTrue(! v2.isStaticConstant());
      // Assert.assertTrue(! v3.isStaticConstant());
      VarInfo tmp;
      if (v1.varinfo_index > v2.varinfo_index) { tmp = v2; v2 = v1; v1 = tmp; }
      if (v2.varinfo_index > v3.varinfo_index) { tmp = v3; v3 = v2; v2 = tmp; }
      if (v1.varinfo_index > v2.varinfo_index) { tmp = v2; v2 = v1; v1 = tmp; }
      result = new PptSlice3(this, v1, v2, v3);
      break;
    }
    default:
      throw new IllegalArgumentException("bad length = " + vis.length);
    }

    addSlice(result);
    return result;
  }

  /* [INCR] ... We can't know this anymore
  // Set the dynamic_constant slots of all the new variables.
  void set_dynamic_constant_slots(Vector unary_views) {
    for (int i=0; i<unary_views.size(); i++) {
      PptSlice1 unary_view = (PptSlice1) unary_views.elementAt(i);
      // System.out.println("set_dynamic_constant_slots " + unary_view.name + " " + views.contains(unary_view));
      Assert.assertTrue(unary_view.arity == 1);
      // If this view has been installed in the views slot (ie, it has not
      // been eliminated already).
      if (views.contains(unary_view)) {
        // This is not true any longer.
        // // There is only one type of unary invariant in pass 1:
        // // OneOf{Scalar,Sequence}.  It must have been successful, or this
        // // view wouldn't have been installed.
        // Assert.assertTrue(unary_view.invs.size() == 1);
        // Invariant inv = (Invariant) unary_view.invs.elementAt(0);

        for (int j=0; j<unary_view.invs.size(); j++) {
          Invariant inv = (Invariant) unary_view.invs.elementAt(j);
          inv.finished = true;
          // unary_view.already_seen_all = true;
          OneOf one_of = (OneOf) inv;
          // System.out.println("num_elts: " + one_of.num_elts());
          if ((one_of.num_elts() == 1)
              && (! (inv instanceof EltOneOf))
              && (! (inv instanceof EltOneOfString))) {
            // System.out.println("Constant " + inv.ppt.name + " " + one_of.var().name + " because of " + inv.format() + "    " + inv.repr_prob() + "    " + inv.justified());
            // Should be Long, not Integer.
            Assert.assertTrue(! (one_of.elt() instanceof Integer));
            one_of.var().dynamic_constant = one_of.elt();
            one_of.var().is_dynamic_constant = true;
            // System.out.println("set dynamic_constant to " + one_of.elt());
          }
        }
      } else {
        unary_view.clear_cache();
      }
    }
  }
  */ // ... [INCR]

  /* [INCR] ... no longer makes sense
  // Set the equal_to slots of all the new variables.
  void set_equal_to_slots(Vector binary_views, int vi_index_min, int vi_index_limit) {
    for (int i=0; i<binary_views.size(); i++) {
      PptSlice2 binary_view = (PptSlice2) binary_views.elementAt(i);
      Assert.assertTrue(binary_view.arity == 2);

      if (binary_view.debugged) {
        System.out.println("Binary view " + binary_view.name + " has "
                           + (views.contains(binary_view) ? "not " : "") + "been eliminated.");
      }
      // If binary_view has been installed (hasn't yet been eliminated)
      if (views.contains(binary_view)) {

        // set_equal_to_slots runs after pass 1 of invariant introduction.
        // There is only one type of binary invariant in pass 1:
        // {Int,Seq,String}Comparison.  It must have been successful, or
        // this view wouldn't have been installed.
        Assert.assertTrue(binary_view.invs.size() == 1);

        Invariant inv = (Invariant) binary_view.invs.elementAt(0);
        inv.finished = true;
        // binary_view.already_seen_all = true;
        Assert.assertTrue(inv instanceof Comparison);
        // System.out.println("Is " + (IsEqualityComparison.it.accept(inv) ? "" : "not ")
        //                    + "equality: " + inv.format());
        if (! (IsEqualityComparison.it.accept(inv)
               && inv.enoughSamples())) {
          continue;
        }
        VarInfo var1 = binary_view.var_infos[0];
        VarInfo var2 = binary_view.var_infos[1];
        if (var1.canBeMissing || var2.canBeMissing) {
          // System.out.println("Not setting equal_to based on " + inv.format()
          //                    + "\n  because of canBeMissing: "
          //                    + var1.name.name() + "=" + var1.canBeMissing + " "
          //                    + var2.name.name() + "=" + var2.canBeMissing);
          continue;
        }
        Assert.assertTrue(var1.varinfo_index < var2.varinfo_index);
        // System.out.println("found equality: " + var1.name.name() + " = " + var2.name.name());
        // System.out.println("var1.equal_to="
        //                    + ((var1.equal_to == null) ? "null" : var1.equal_to.name.name())
        //                    + ", var2.equal_to="
        //                    + ((var2.equal_to == null) ? "null" : var2.equal_to.name.name()));
        // System.out.println(inv.repr());
        if ((var1.equal_to == null) && (var2.equal_to != null)) {
          var1.equal_to = var2.equal_to;
          if (debugEqualTo.isDebugEnabled()) {
            debugEqualTo.debug("Setting " + var1.name + ".equal_to = " + var1.equal_to.name);
          }
        } else if ((var1.equal_to != null) && (var2.equal_to == null)) {
          var2.equal_to = var1.equal_to;
          if (debugEqualTo.isDebugEnabled()) {
            debugEqualTo.debug("Setting " + var2.name + ".equal_to = " + var2.equal_to.name);
          }
        } else if ((var1.equal_to == null) && (var2.equal_to == null)) {
          // Can this cause the canonical version to not be the lowest-
          // numbered version?  I don't think so, because of the ordering
          // in which we are examining pairs.
          var1.equal_to = var1;
          var2.equal_to = var1;
          // System.out.println("Make " + var1.name + " canonical over " + var2.name + " at " + name);
        } else {
          Assert.assertTrue((var1.equal_to != null) && (var2.equal_to != null));
          if (var1.compatible(var2)
              && (var1.equal_to != var2.equal_to)) {

            // This can happen if we have canonical variables a and b,
            // then we introduce new variable c which equals both of
            // them.

            // This used to be an assert.  There is a real problem if
            // this arises, but I have commented it out to avoid
            // confusing users, and so we can concentrate on version 3.
            if (debugEqualTo.isDebugEnabled()) {
              // Stars because this message is more important than most.
              debugEqualTo.debug("*****");
              debugEqualTo.debug("Internal Daikon error: Variables not equal: " + var1.name.name() +
                                 " (= " + var1.equal_to.name.name() + "), " + var2.name.name() + " (= " +
                                 var2.equal_to.name.name() + ") [indices " + var1.varinfo_index +
                                 ", " + var1.equal_to.varinfo_index + ", " + var2.varinfo_index +
                                 ", " + var2.equal_to.varinfo_index + "] at " + name);
                debugEqualTo.debug("*****");
            }

            // If this reappears as a problem, I could fix the problem by
            // changing equal_to slots from canon2 to canon1.

          }
          Assert.assertTrue(var1.equal_to.varinfo_index <= var1.varinfo_index);
          Assert.assertTrue(var2.equal_to.varinfo_index <= var2.varinfo_index);
        }
      } else {
        binary_view.clear_cache();
      }
    }
    // Set equal_to for VarInfos that aren't equal to anything but themselves.
    for (int i=vi_index_min; i<vi_index_limit; i++) {
      VarInfo vi = var_infos[i];
      if (vi.equal_to == null) {
        if (debugEqualTo.isDebugEnabled()) {
          debugEqualTo.debug("Lonesome canonical var " + vi.varinfo_index + ": " + vi.name.name());
        }
        vi.equal_to = vi;
      }
    }

    // Now, remap the equal_to fields so that they point to
    // interesting VarInfos, if possible.  For this part,
    // "interesting" is when isDerivedParamAndUninteresting returns
    // false.  If a canonical VarInfo is interesting, then we do
    // nothing.  If it's uninteresting and all of the variables it
    // equals to are uninteresting, then also do nothing.  If it's
    // unintersting and one of its equals are intersting, change the
    // equal_to fields of all the variables in that equivalence set to
    // the interesting variable.  This may be repeated for derived
    // variables.  However, since isDerivedParamAndUninteresting is
    // cached in VarInfo, we should remember that param checking
    // should be "stable" when new VarInfos appear.  This doesn't seem
    // to be a problem.

    if (ppt_name.isExitPoint()) {
      Map equalMap = new HashMap();  // Map of canonicals to lists of non canonicals
      for (int i = 0; i < var_infos.length; i++) {
        VarInfo vi = var_infos[i];
        if (vi.isCanonical()) {
          if (equalMap.containsKey(vi)) {
            // Do nothing because case 4 handles this
          } else {
            // Case 2
            List newList = new ArrayList();
            newList.add(vi);
            equalMap.put (vi, newList);
          }
        } else {
          VarInfo eq = vi.equal_to;
          if (equalMap.containsKey(eq)) {
            // Case 3
            List oldList = (List) equalMap.get(eq);
            oldList.add (vi);
          } else {
            List newList = new ArrayList();
            // Case 4
            newList.add (eq);
            newList.add (vi);
            equalMap.put (eq, newList);
          }
        }

      }
      if (debugEqualTo.isDebugEnabled()) {
        debugEqualTo.debug ("Doing equality mapping for " + this.name);
        debugEqualTo.debug ("mapping: " + equalMap);

      }

      for (Iterator i = equalMap.keySet().iterator(); i.hasNext(); ) {
        VarInfo canonical = (VarInfo) i.next();
        if (canonical.isDerivedParamAndUninteresting()) {
          List equalTo = (List) equalMap.get(canonical);
          VarInfo viInteresting = null;
          for (Iterator iterEquals = equalTo.iterator(); iterEquals.hasNext(); ) {
            VarInfo vi = (VarInfo) iterEquals.next();
            if (!vi.isDerivedParamAndUninteresting()) {
              viInteresting = vi;
              break;
            }
          }
          if (viInteresting != null) {
            for (Iterator iterEquals = equalTo.iterator(); iterEquals.hasNext(); ) {
              VarInfo vi = (VarInfo) iterEquals.next();
              vi.equal_to = viInteresting;
            }
          }
        }
      }
    }


  }
  */ // ... [INCR]

  /* [INCR] ... don't think we still need this?
  // Compute exact_nonunary_invariants
  void set_exact_nonunary_invariants_slots(Vector nonunary_views) {
    for (int j=0; j<nonunary_views.size(); j++) {
      PptSlice nonunary_view = (PptSlice) nonunary_views.elementAt(j);
      for (int k=0; k<nonunary_view.invs.size(); k++) {
        Invariant inv = (Invariant) nonunary_view.invs.elementAt(k);
        if (inv.isExact() && inv.enoughSamples()) {
          nonunary_view.var_infos[0].exact_nonunary_invariants.add(inv);
        }
      }
      nonunary_view.clear_cache();
    }
  }
  */ // ... [INCR]


  ///////////////////////////////////////////////////////////////////////////
  /// Manipulating invariants and suppression
  ///

  private boolean initiatedSuppression = false;

  /**
   * Starts suppression by 1) unsuppressing all invariants. 2) running
   * full suppression check.  Called at the start of inferencing.  Can
   * be called repeatedly to refresh suppression, but this is an
   * expensive operation.  Requires that this and its parents have
   * already has instantiated invariants.
   *
   * @see daikon.suppress.SuppressionFactory
   * @pre Invariants already instantiated
   **/
  public void initiateSuppression() {
    if (Daikon.suppress_invariants && !initiatedSuppression) {
      initiatedSuppression = true;
      if (debugSuppressInit.isDebugEnabled()) {
        debugSuppressInit.debug ("Initiating suppression for: " + name);
      }
      List invs = getInvariants();
      for (Iterator i = invs.iterator(); i.hasNext(); ) {
        Invariant inv = (Invariant) i.next();
        if (inv.getSuppressor() == null) {
          attemptSuppression (inv);
        }
      }
      if (debugSuppressInit.isDebugEnabled()) {
        debugSuppressInit.debug ("  Suppressed invariants:");
        for (Iterator i = invs.iterator(); i.hasNext(); ) {
          Invariant inv = (Invariant) i.next();
          if (inv.getSuppressor() != null) {
            debugSuppressInit.debug (" +" + inv.repr());
          } else {
            debugSuppressInit.debug (" -" + inv.repr());
          }
        }
        debugSuppressInit.debug ("  end of suppressed invariants:");
      }

    }
  }


  /**
   * Try to suppress one invariant.  Links the invariant to a
   * SuppressionLink if suppression succeeds.
   * @param inv the Invariant to attempt suppression on, which has to
   * be a member of this.
   * @return true if invariant was suppressed
   * @see daikon.suppress.SuppressionFactory
   * @pre Invariants already instantiated.  inv not already suppressed.
   **/
  public boolean attemptSuppression (Invariant inv) {
    if (Daikon.suppress_invariants) {
      if (inv.getSuppressor() != null) {
        System.err.println ("Error: the invariant " + inv.format() +
                            " already has a suppressor");
        Assert.assertTrue (inv.getSuppressor() == null);
      }
      SuppressionFactory[] factories = inv.getSuppressionFactories();
      for (int i = 0; i < factories.length; i++) {
        SuppressionLink sl = factories[i].generateSuppressionLink (inv);
        if (sl != null) {
          sl.link();
          if (debugSuppress.isDebugEnabled()) {
            debugSuppress.debug ("Generated link with: " + sl + " at ppt " + name);
          }
          return true;
        }
      }
    }
    return false;
  }


  /**
   * Attempt to fill a given SuppressionTemplate with invariants.  If
   * successful, returns true.  Called by SuppressionFactory's.
   * @param template Template to fill.  Modified by this method.
   **/
  public boolean fillSuppressionTemplate (SuppressionTemplate template) {
    return fillSuppressionTemplate (template, true);
  }


  /**
   * Attempt to fill a given SuppressionTemplate with invariants.  If
   * successful, returns true.  Called by SuppressionFactory's.
   * @param template Template to fill.  Modified by this method.
   * @param checkSelf Whether to check in this ppt.  When false, skip
   * scanning this ppt.  This is useful for detecting identical
   * invariants (due to weakening) across ppts.
   **/
  public boolean fillSuppressionTemplate (SuppressionTemplate template,
                                          boolean checkSelf) {
    // We do two loops for performance: attempt to fill locally, then
    // attempt to fill using upper ppts.

    boolean firstLoopFilled = false;
    template.filled = false;
    template.results = new Invariant[template.invTypes.length];
    template.transforms = new VarInfo[template.invTypes.length][];
    Assert.assertTrue (template.invTypes.length == template.varInfos.length,
                       "Template varInfos and invariant slots must be equal");
    debugSuppressFill.debug ("Starting fill");

    if (checkSelf) {
      firstLoop:
      // debugSuppressFill.debug ("  Entering first loop");
      for (int iInvs = 0; iInvs < template.invTypes.length; iInvs++) {
        template.results[iInvs] = null;
        Class clazz = template.invTypes[iInvs];
        VarInfo[] varInfos = template.varInfos[iInvs];
        PptSlice slice = this.findSlice_unordered (varInfos);
        if (slice != null) {
          // Here's where we actually find the potential invariant.  There are
          // two choices here: suppressed invariants can do more suppression, or
          // they can be forbidden to suppress others.
          Invariant inv =
            Daikon.suppress_with_suppressed ?
            Invariant.find (clazz, slice) :
            Invariant.findUnsuppressed (clazz, slice);
          if (inv != null) {
            firstLoopFilled = true;
            template.results[iInvs] = inv;
            template.transforms[iInvs] = template.varInfos[iInvs];
          }
        }
      }
      // Formerly, we used to return null if the first loop didn't get
      // at least one invariant.  But there are some types of
      // suppression where this optimization would lower suppression
      // results.  For example, to show that
      // OBJECT:::NoDuplicates(this.array) implies
      // Method::NoDuplicates(this.array[0..i]), we need to search in
      // the OBJECT ppt, because the Method ppt won't have the
      // NoDuplicates(this.array) invariant, as it's in the OBJECT
      // ppt.
      // if (!firstLoopFilled) return false;
    }

    // debugSuppressFill.debug ("  Entering second loop: ");
    secondLoop:
    for (int iInvs = 0; iInvs < template.invTypes.length; iInvs++) {
      Class clazz = template.invTypes[iInvs];
      //       if (debugSuppressFill.isDebugEnabled()) {
      //         debugSuppressFill.debug ("  InvType: " + clazz);
      //       }
      if (dataflow_ppts == null) {
        // debugSuppressFill.debug ("  No dataflow_ppts");
        break;
      }
      if (template.results[iInvs] != null) {
        // debugSuppressFill.debug ("  Already filled");
        continue secondLoop;
      }

      VarInfo[] varInfos = template.varInfos[iInvs];

      forEachTransform:
      // Transform the VarInfos for each upper ppt
      // We go backwards so that we get the strongest invariants first.
      for (int iPpts = dataflow_ppts.length - (checkSelf ? 1 : 2);
           iPpts >= 0; iPpts--) {
        PptTopLevel dataflowPpt = dataflow_ppts[iPpts];
        //         if (debugSuppressFill.isDebugEnabled()) {
        //           debugSuppressFill.debug ("  Flow ppt: " + dataflowPpt.name);
        //         }
        int[] dataflowTransform = dataflow_transforms[iPpts];
        VarInfo[] newVarInfos = new VarInfo[varInfos.length];
        forEachVarInfo:
        for (int iVarInfos = 0; iVarInfos < varInfos.length; iVarInfos++) {
          int newIndex = dataflowTransform[varInfos[iVarInfos].varinfo_index];
          if (newIndex >= 0) {
            newVarInfos[iVarInfos] = dataflowPpt.var_infos[newIndex];
          } else {
            continue forEachTransform;
          }
        }

        PptSlice slice = dataflowPpt.findSlice_unordered (newVarInfos);
        if (slice != null) {
          Invariant inv =
            Daikon.suppress_with_suppressed ?
            Invariant.find (clazz, slice) :
            Invariant.findUnsuppressed (clazz, slice);
          if (inv != null) {
            template.results[iInvs] = inv;
            template.transforms[iInvs] = newVarInfos;
          }
        }
      }
    }

    // Only for checking if template got filled
    thirdLoop:
    for (int iInvs = 0; iInvs < template.invTypes.length; iInvs++) {
      if (template.results[iInvs] == null) {
        debugSuppressFill.debug ("  Unsuccessful fill");
        return false;
      }
    }

    template.filled = true;
    return true;
  }


  ///////////////////////////////////////////////////////////////////////////
  /// Creating conditioned views
  ///

  // This static region can't appear in PptConditional, lest it never get
  // called.  PptConditional isn't instantiated unless it needs to be, but
  // it doesn't need to be unless we run this static region!

  static {
    if (! Daikon.disable_splitting) {
      // new MiscSplitters();

      SplitterList.put(".*", new Splitter[] {
        new ReturnTrueSplitter(),
      });
    }
  }


  public void addConditions(Splitter[] splits) {
    // System.out.println("addConditions(" + splits.length + ") for " + name);

    int len = splits.length;
    if (len == 0) {
      if (Global.debugSplit.isDebugEnabled())
        Global.debugSplit.debug("No splits for " + name);
      return;
    }

    // for (int i=0; i<splits.length; i++) {
    //   Assert.assertTrue(splits[i].instantiated() == false);
    // }

    Vector pconds = new Vector(2 * len);
    for (int i=0; i<len; i++) {
      PptConditional cond1 = new PptConditional(this, splits[i], false);
      if (! cond1.splitter_valid()) {
        if (Global.debugSplit.isDebugEnabled())
          Global.debugSplit.debug("Splitter not valid: " + cond1.name);
        continue;
      }
      pconds.add(cond1);
      PptConditional cond2 = new PptConditional(this, splits[i], true);
      Assert.assertTrue(cond2.splitter_valid());
      // [INCR] pconds_vector.add(cond2);
    }
    /* [INCR] commented due to timidity; uncomment as soon as possible
    PptConditional[] pconds
      = (PptConditional[]) pconds_vector.toArray(new PptConditional[0]);
    int num_pconds = pconds.length;
    Assert.assertTrue(num_pconds % 2 == 0);
    int num_splits = num_pconds/2;

    for (int i=0; i<num_pconds; i+=2) {
      Assert.assertTrue(! pconds[i].splitter_inverse);
      Assert.assertTrue(pconds[i+1].splitter_inverse);
      Assert.assertTrue(pconds[i+1].splitter.condition().equals(pconds[i].splitter.condition()));
    }

    int trimlength = num_tracevars + num_orig_vars;

    int[][] cumulative_modbits = new int[num_pconds][trimlength];
    for (int i=0; i<num_pconds; i++) {
      Arrays.fill(cumulative_modbits[i], 1);
    }

    // Fill the new PptConditionals with values.
    for (Iterator vt_itor = values.sampleIterator(); vt_itor.hasNext(); ) {
      VarValuesOrdered.ValueTupleCount entry = (VarValuesOrdered.ValueTupleCount) vt_itor.next();
      ValueTuple vt = entry.value_tuple;
      int count = entry.count;
      // I do not want to use the same ValueTuple every time through the pconds
      // loop because the inserted ValueTuple will be modified in place.
      // It's OK to reuse its elements, though.
      ValueTuple vt_trimmed = vt.trim(trimlength);
      int[] trimmed_mods = vt_trimmed.mods;
      Object[] trimmed_vals = vt_trimmed.vals;
      for (int i=0; i<num_pconds; i+=2) {
        // I really only have to do one of these (depending on which way
        // the split goes), unless the splitter throws an error, in which
        // case I need to have done both.  Thus, do both, to be on the safe
        // side.
        ValueTuple.orModsInto(cumulative_modbits[i], trimmed_mods);
        ValueTuple.orModsInto(cumulative_modbits[i+1], trimmed_mods);
        boolean splitter_test;
        boolean split_exception = false;
        // System.out.println("Testing " + pconds[i].name);
        // This try block is tight so it doesn't accidentally catch
        // other errors.
        try {
          splitter_test = pconds[i].splitter.test(vt);
        } catch (Exception e) {
          // System.out.println("----------------");
          // System.out.println("splitter condition: " + pconds[i].splitter.condition() + " " + pconds[i].splitter.getClass().getName());
          // System.out.println("ppt: " + name);
          // System.out.print("vars:");
          // for (int j=0; j<var_infos.length; j++) {
          //   System.out.print(" " + var_infos[j].name.name());
          // }
          // System.out.println();
          // System.out.println("valuetuple: " + vt.toString());
          // Splitter splitter = pconds[i].splitter;
          // Class sclass = splitter.getClass();
          // String sclassname = sclass.getName();
          // // if (sclassname.equals("std_amatch_0")
          // //     || sclassname.equals("std_getccl_0")) {
          // //   try {
          // //     Method srepr = sclass.getMethod("repr", new Class[] {});
          // //     Object result = srepr.invoke(splitter, new Object[] {});
          // //     System.out.println("repr: " + result);
          // //     // Field f = sclass.getDeclaredField("pat_varinfo");
          // //     // Object pv = f.get(splitter);
          // //     // System.out.println(pv);
          // //   } catch (Exception e2) {
          // //     throw new Error(e2.toString());
          // //   }
          // // }
          // e.printStackTrace();

          // If an exception is thrown, don't put the data on either side
          // of the split.
          split_exception = true;
          splitter_test = false; // to pacify the Java compiler
        }
        if (! split_exception) {
          // System.out.println("Result = " + splitter_test);
          int index = (splitter_test ? i : i+1);
          // Do not reuse cum_mods!  It might itself be the
          // canonical version (returned by Intern.intern), and then
          // modifications would be bad.  Instead, create a new array.
          int[] cum_mods = cumulative_modbits[index];
          int[] new_mods = (int[]) trimmed_mods.clone();
          // This is somewhat like orModsInto, but not exactly.
          for (int mi=0; mi<trimlength; mi++) {
            if ((cum_mods[mi] == ValueTuple.MODIFIED)
                && (new_mods[mi] != ValueTuple.MISSING_NONSENSICAL)) {
              new_mods[mi] = ValueTuple.MODIFIED;
              cum_mods[mi] = ValueTuple.UNMODIFIED;
            }
          }
          // System.out.println("Adding (count " + count + ") to " + pconds[index].name);
          pconds[index].add_nocheck(ValueTuple.makeFromInterned(trimmed_vals,
                                                                Intern.intern(new_mods)),
                                    count);
          // I don't want to do "Arrays.fill(cum_mods, 0)" because where
          // the value was missing, we didn't use up the modification bit.
          // I've already fixed it up above, anyway.
        }
      }
    }


    // Install the new conditional ppts, if they are not trivial.
    int parent_num_samples = num_samples();
    Assert.assertTrue(num_pconds % 2 == 0);
    for (int i=0; i<num_pconds; i+=2) {
      // Don't bother with this pair of conditioned views if one of
      // them contains no samples.  This if different than one having
      // all samples, since some samples may not be added if the
      // splitter throws an exception.

      int cond0_num_samples = pconds[i].num_samples();
      int cond1_num_samples = pconds[i+1].num_samples();
      boolean discard_pair = (cond0_num_samples == 0) || (cond1_num_samples == 0);
      if (! discard_pair) {
        views_cond.add(pconds[i]);
        views_cond.add(pconds[i+1]);
      } else {
        if (Global.debugSplit.isDebugEnabled())
          Global.debugSplit.debug
            ("Omitting " + pconds[i].name + " (and its inverse): "
             + cond0_num_samples + "(" + cond1_num_samples + ")/"
             + parent_num_samples + " samples");
        // // Unconditional output, because it's too confusing otherwise.
        // if (this_num_samples == parent_num_samples) {
        //   System.out.println("Condition always satisfied: "
        //                      + pconds[i].name + " == " + this.name);
        // }
      }
    }

    // views_cond must have matching pairs
    Assert.assertTrue(views_cond.size() % 2 == 0);
    for (int i=0; i < views_cond.size(); i+=2) {
      PptConditional cond0 = (PptConditional) views_cond.get(i);
      PptConditional cond1 = (PptConditional) views_cond.get(i+1);
      Assert.assertTrue(! cond0.splitter_inverse);
      Assert.assertTrue(cond1.splitter_inverse);
      Assert.assertTrue(cond0.splitter.condition().equals(cond1.splitter.condition()));
    }

    if (Global.debugSplit.isDebugEnabled()) {
      Global.debugSplit.debug("" + views_cond.size() + " views on " + this.name);
      for (int i=0; i<views_cond.size(); i++) {
        PptConditional pcond = (PptConditional) views_cond.elementAt(i);
        debug.debug("    " + pcond.name);
      }
    }
    for (int i=0; i<views_cond.size(); i++) {
      PptConditional pcond = (PptConditional) views_cond.elementAt(i);
      pcond.initial_processing();
    }
    */ // [INCR]

    // Install the new conditional ppts
    views_cond.addAll(pconds);
  }

  /**
   * Given conditional program points (and invariants detected over them),
   * create implications.  Configuration variable "pairwise_implications"
   * controls whether all or only the first two conditional program points
   * are considered.
   **/
  public void addImplications() {
    int num_conds = views_cond.size();
    if (num_conds > 0) {
      // if dkconfig_pairwise_implications is not set, then only create
      // implications from the first two conditional program points.
      if (! dkconfig_pairwise_implications) {
        num_conds = Math.min(num_conds, 2);
      }
      // Take each conditional program point and its opposite and make
      // implications. We can't assume that the number or conditional
      // program points is even, because conditional program points
      // with no samples are discarded. Otherwise, a conditional
      // program point is usually next to its opposite pair in the
      // vector view_conds.
      for (int i = 0; i < num_conds; i++) { // note increment is NOT "i+=2".
        PptConditional cond1 = (PptConditional) views_cond.elementAt(i);
        if ( i+1 >= num_conds )
          continue;
        PptConditional cond2 = (PptConditional) views_cond.elementAt(i+1);
        if (cond1.splitter_inverse == cond2.splitter_inverse)
          continue;
        if (!cond1.splitter.condition().equals(cond2.splitter.condition()))
          continue;
        addImplications_internal(cond1, cond2, false);
        // skip cond2
        i++;
      }
    }

    /* [INCR] ...
    if (this.ppt_name.isCombinedExitPoint()) {
      Vector exits = this.entry_ppt.exit_ppts;
      if (exits.size() == 2) {
        // Eventually I ought to make this applicable when the number of
        // individual exits is not 2.

      // System.out.println("num exits = " + exits.size());
      // for (int i=0; i<exits.size(); i++) {
      //   System.out.println(((PptTopLevel)exits.elementAt(i)).name);
      // }

      // Assert.assertTrue(exits.size() == 2, "Bad number of exits: " + exits.size());
      for (int i = 0; i < num_exits; i++) {
        for (int j = i+1; j < num_exits; j++) {
          PptTopLevel ppt1 = (PptTopLevel) exits.elementAt(i);
          PptTopLevel ppt2 = (PptTopLevel) exits.elementAt(j);
          // No longer necessary to use add_implications, as we are now
          // adding combined program points early.
          // addImplications_internal(ppt1, ppt2, true);
          addImplications_internal(ppt1, ppt2, false);
        }
      }
    }
    */ // ... [INCR]
  }


  // Given a pair of conditional program points, form implications from the
  // invariants true at each one.  The algorithm divides the invariants
  // into three groups:  those that are true at both program points (the
  // "same" invariants), those that are true at one program point and whose
  // negation is true at the other program point (the "exclusive"
  // invariants), and all others (the "different" invariants).  At the
  // first program point, for each exclusive invariant and each different
  // invariant, create a conditional of the form "exclusive => different".
  // Do the same at the second program point.

  // This method is correct only if the two conditional program points
  // fully partition the input space (their conditions are negations of one
  // another).  For instance, suppose there is a three-way split with the
  // following invariants detected at each:
  //   {A,B}  {!A,!B}  {A,!B}
  // Examining just the first two would suggest that "A <=> B" is valid,
  // but in fact that is a false inference.
  private void addImplications_internal(PptTopLevel ppt1,
                                        PptTopLevel ppt2,
                                        boolean add_nonimplications)
  {
    // System.out.println("addImplications_internal: " + ppt1.name + ", " + ppt2.name);

    PptSlice[][] matched_views = match_views(ppt1, ppt2);
    if (debugAddImplications.isDebugEnabled()) {
      debugAddImplications.debug("Matched views=" + matched_views.length + " from " +
                                 ppt1.views.size() + ", " + ppt2.views.size());
    }

    Vector exclusive_conditions_vec = new Vector(); // elements are pairs of Invariants
    Vector same_invariants_vec = new Vector(); // elements are Invariants

    for (int i=0; i<matched_views.length; i++) {
      PptSlice slice1 = matched_views[i][0];
      PptSlice slice2 = matched_views[i][1];

      if ((slice1 == null) || (slice2 == null)) {
        if (debugAddImplications.isDebugEnabled()) {
          debugAddImplications.debug("addImplications: matched views skipped "
                                     + (slice1 == null ? "null" : slice1.name) + " "
                                     + (slice2 == null ? "null" : slice2.name));
        }
        continue;
      }

      // Do not eliminate invariants that are not worth printing at this
      // stage!  Perhaps x=y is not worth printing because it is true at a
      // controller, but x!=y is worth printing; now we can't determine
      // that there is an exclusive condition.  We'll eliminate those
      // not-worth-printing invariants later, when we actually make the
      // implication invariants.

      Invariants invs1 = new Invariants();
      for (int j=0; j<slice1.invs.size(); j++) {
        Invariant inv = (Invariant)slice1.invs.get(j);
        invs1.add(inv);
        if (debugAddImplications.isDebugEnabled()) {
          debugAddImplications.debug("invs1 " + inv.format());
        }
      }
      Invariants invs2 = new Invariants();
      for (int j=0; j<slice2.invs.size(); j++) {
        Invariant inv = (Invariant)slice2.invs.get(j);
        invs2.add(inv);
        if (debugAddImplications.isDebugEnabled()) {
          debugAddImplications.debug("invs2 " + inv.format());
        }
      }

      Vector this_excl = exclusive_conditions(invs1, invs2);
      if (debugAddImplications.isDebugEnabled()) {
        debugAddImplications.debug("addImplications: "
                                   + this_excl.size() + " exclusive conditions for "
                                   + slice1.name + " " + slice2.name);
      }
      exclusive_conditions_vec.addAll(this_excl);

      Vector this_same = same_invariants(invs1, invs2);
      same_invariants_vec.addAll(this_same);
    }

    if (add_nonimplications) {
      for (int i=0; i<same_invariants_vec.size(); i++) {
        Invariant same_inv = (Invariant)same_invariants_vec.elementAt(i);
        // This test doesn't seem to be productive.  (That comment may date
        // from the time that all not-worth-printing invariants were
        // already eliminated.)
        // if (! same_inv.isControlled()) // [INCR]
        joiner_view.addInvariant(same_inv);
      }
    }

    if (exclusive_conditions_vec.size() == 0) {
      if (debugAddImplications.isDebugEnabled()) {
        debugAddImplications.debug("addImplications: no exclusive conditions");
      }
      return;
    }

    // These two program points are mutually exclusive

    Invariant[][] exclusive_conditions
      = (Invariant[][])exclusive_conditions_vec.toArray(new Invariant[0][0]);
    Invariant[][] different_invariants
      = (Invariant[][])different_invariants(matched_views).toArray(new Invariant[0][0]);

    if (debugAddImplications.isDebugEnabled()) {
      debugAddImplications.debug("addImplications: "
                                 + exclusive_conditions.length + " exclusive conditions, "
                                 + different_invariants.length + " different invariants");
    }


    // Add an implication from each of a pair of mutually exclusive
    // invariants to everything that differs (at all) about the two

    // split into two in order to use indexOf
    Invariant[] excls1 = new Invariant[exclusive_conditions.length];
    Invariant[] excls2 = new Invariant[exclusive_conditions.length];
    for (int i=0; i<exclusive_conditions.length; i++) {
      excls1[i] = exclusive_conditions[i][0];
      excls2[i] = exclusive_conditions[i][1];
    }


    for (int i=0; i<exclusive_conditions.length; i++) {
      Assert.assertTrue(exclusive_conditions[i].length == 2);
      Invariant excl1 = exclusive_conditions[i][0];
      Invariant excl2 = exclusive_conditions[i][1];
      Assert.assertTrue(excl1 != null);
      Assert.assertTrue(excl2 != null);

      if (debugAddImplications.isDebugEnabled()) {
        debugAddImplications.debug("Adding implications with conditions "
                                   + excl1.format() + " and " + excl2.format());
      }

      for (int j=0; j<different_invariants.length; j++) {
        Assert.assertTrue(different_invariants[j].length == 2);
        Invariant diff1 = different_invariants[j][0];
        Invariant diff2 = different_invariants[j][1];

        Assert.assertTrue((diff1 == null) || (diff2 == null)
                      || (ArraysMDE.indexOf(excls1, diff1)
                          == ArraysMDE.indexOf(excls2, diff2)));

        if (debugAddImplications.isDebugEnabled()) {
          debugAddImplications.debug("different_invariants "
                                     + ((diff1 == null) ? "null" : diff1.format())
                                     + ", " + ((diff2 == null) ? "null" : diff2.format()));
        }

        // This adds an implication to itself; bad.
        // If one of the diffs implies the other, then should not add
        // an implication for the weaker one.
        if (diff1 != null) {
          int index1 = ArraysMDE.indexOf(excls1, diff1);
          if ((index1 == -1) || (index1 > i)) {
            boolean iff = (index1 != -1);
            Implication.makeImplication(this, excl1, diff1, iff);
          }
        }
        if (diff2 != null) {
          int index2 = ArraysMDE.indexOf(excls2, diff2);
          if ((index2 == -1) || (index2 > i)) {
            boolean iff = (index2 != -1);
            Implication.makeImplication(this, excl2, diff2, iff);
          }
        }
      }
    }

    HashMap canonical_inv = new HashMap(); // Invariant -> Invariant
    {
      HashMap inv_group = new HashMap(); // Invariant -> HashSet[Invariant]

      // Problem: I am not iterating through the invariants in any particular
      // order that will guarantee that I don't see A and B, then C and D,
      // and then A and C (which both already have different canonical versions).
      // System.out.println(name + " implication canonicalization");
      for (Iterator itor = joiner_view.invs.iterator(); itor.hasNext(); ) {
        Invariant inv = (Invariant) itor.next();
        if ((inv instanceof Implication) && ((Implication) inv).iff) {
          Implication impl = (Implication) inv;
          // System.out.println("Bi-implication: " + impl.format());
          Invariant canon1 = (Invariant) canonical_inv.get(impl.left);
          Invariant canon2 = (Invariant) canonical_inv.get(impl.right);
          if ((canon1 != null) && (canon2 != null) && (canon1 != canon2)) {
            // Move all the invariants for canon2 over to canon1
            HashSet hs1 = (HashSet) inv_group.get(canon1);
            HashSet hs2 = (HashSet) inv_group.get(canon2);
            inv_group.remove(canon2);
            for (Iterator itor2=hs2.iterator(); itor2.hasNext(); ) {
              Invariant inv2 = (Invariant) itor2.next();
              hs1.add(inv2);
              canonical_inv.put(inv2, canon1);
            }
            // System.out.print("Current set:");
            // for (Iterator itor2=hs1.iterator(); itor2.hasNext(); ) {
            //   Invariant inv2 = (Invariant) itor2.next();
            //   System.out.print("    " + inv2.format());
            // }
            // System.out.println();
          } else {
            Invariant canon = (canon1 != null) ? canon1 : (canon2 != null) ? canon2 : impl.left;
            // System.out.println("Canonical: " + canon.format());
            canonical_inv.put(impl.left, canon);
            canonical_inv.put(impl.right, canon);
            HashSet hs = (HashSet) inv_group.get(canon);
            if (hs == null) {
              hs = new HashSet();
              inv_group.put(canon, hs);
            }
            hs.add(impl.left);
            hs.add(impl.right);
            // System.out.print("Current set (2):");
            // for (Iterator itor2=hs.iterator(); itor2.hasNext(); ) {
            //   Invariant inv2 = (Invariant) itor2.next();
            //   System.out.print("    " + inv2.format());
            // }
            // System.out.println();
          }
        }
      }

      // Now adjust which of the invariants are canonical.
      // (That is why inv_group was computed above.)

      for (Iterator itor=inv_group.keySet().iterator(); itor.hasNext(); ) {
        Invariant canon_orig = (Invariant) itor.next();
        // System.out.println("Outer loop: " + canon_orig.format());
        HashSet hs = (HashSet) inv_group.get(canon_orig);
        if (hs.size() == 1) {
          continue;
        }
        Invariant canon_new = null;
        String canon_new_formatted = null;
        for (Iterator cand_itor=hs.iterator(); cand_itor.hasNext(); ) {
          Invariant candidate = (Invariant) cand_itor.next();
          String candidate_formatted = candidate.format();
          // System.out.println("Comparing:" + lineSep + "    " + candidate_formatted + lineSep + "    " + canon_new_formatted);
          // It is also desirable to be over the prestate;
          // but that is only true for variables that are modified.
          // A variable without "orig()" is fine if it's not modified.
          boolean canon_new_undesirable
            = ((canon_new == null) // avoid NullPointerException
               || (canon_new_formatted.indexOf("\"null\"") != -1)
               || (canon_new_formatted.indexOf("return") != -1));
          boolean candidate_undesirable
            = ((candidate_formatted.indexOf("\"null\"") != -1)
               || (candidate_formatted.indexOf("return") != -1));
          if ((canon_new == null)
              || canon_new_undesirable
              || ((! candidate_undesirable)
                  && (candidate_formatted.length() < canon_new_formatted.length()))) {
            canon_new = candidate;
            canon_new_formatted = candidate_formatted;
          }
        }
        if (canon_new != canon_orig) {
          // Don't set inv_group, lest I get a ConcurrentModificationException
          // inv_group.put(canon_new, hs);
          // inv_group.remove(canon_orig);
          for (Iterator inv_itor=hs.iterator(); inv_itor.hasNext(); ) {
            Invariant inv = (Invariant) inv_itor.next();
            Assert.assertTrue(canonical_inv.get(inv) == canon_orig);
            canonical_inv.put(inv, canon_new);
          }
        }
      }
      // inv_group is no longer up-to-date now.
      // I could have created an inv_group_2 during the above computation
      // and set inv_group to it if I liked.
    }

    // Prune out implications over non-canonical invariants

    Vector to_remove = new Vector();
    for (Iterator itor = joiner_view.invs.iterator(); itor.hasNext(); ) {
      Invariant inv = (Invariant) itor.next();
      if (inv instanceof Implication) {
        Implication impl = (Implication) inv;
        Invariant cpred = (Invariant) canonical_inv.get(impl.left);
        Invariant ccons = (Invariant) canonical_inv.get(impl.right);
        boolean pred_non_canon = ((cpred != null) && (impl.left != cpred));
        boolean cons_non_canon = ((ccons != null) && (impl.right != ccons));
        if ((! impl.iff)
            && (pred_non_canon || cons_non_canon)) {
          to_remove.add(inv);
        }
      }
    }

    // This line seems to cause non-determinism for implications
    // Run daikon on the dtrace files in:
    // ~mharder/research/reports/thesis/example-specdiff/gcd/delta/nondeterminism
    // Run on all, vs. all except 73.dtrace.  Nondeterministically, a
    // difference will appear and disappear
    joiner_view.invs.removeAll(to_remove);


    // System.out.println("Done adding no more than "
    //                    + (exclusive_conditions.length * different_invariants.length)
    //                    + " implications.");

  }


  // Match up the slices in the two program points.
  // Each element is a PptSlice[2].
  // (Perhaps I need to do something special in the case of differing canonical
  // variables; deal with that later.)
  public PptSlice[][] match_views(PptTopLevel ppt1, PptTopLevel ppt2) {
    Vector result = new Vector();

    // First, sort
    SortedSet ss1 = new TreeSet(arityVarnameComparator);
    ss1.addAll(ppt1.viewsAsCollection());
    SortedSet ss2 = new TreeSet(arityVarnameComparator);
    ss2.addAll(ppt2.viewsAsCollection());

    // Then, pair up elements from the sorted collections.
    for (OrderedPairIterator opi = new OrderedPairIterator(ss1.iterator(), ss2.iterator(), arityVarnameComparator); opi.hasNext(); ) {
      Pair pair = (Pair) opi.next();
      result.add(new PptSlice[] { (PptSlice) pair.a, (PptSlice) pair.b });
    }
    return (PptSlice[][])result.toArray(new PptSlice[0][0]);
  }


  // Determine which elements of invs1 are mutually exclusive with elements
  // of invs2.
  // Result elements are pairs of Invariants.
  Vector exclusive_conditions(Invariants invs1, Invariants invs2) {
    Vector result = new Vector();
    for (int i1=0; i1<invs1.size(); i1++) {
      for (int i2=0; i2<invs2.size(); i2++) {
        Invariant inv1 = (Invariant) invs1.get(i1);
        Invariant inv2 = (Invariant) invs2.get(i2);
        // This is a debugging tool, to make sure that various versions
        // of isExclusiveFormula remain coordinated.  (That's also one
        // reason we don't break out of the loop early:  also, there will
        // be few invariants in a slice, so breaking out is of minimal
        // benefit.)
        Assert.assertTrue(inv1.isExclusiveFormula(inv2)
                      == inv2.isExclusiveFormula(inv1),
                      "Bad exclusivity: " + inv1.isExclusiveFormula(inv2) + " " + inv2.isExclusiveFormula(inv1)
                       + "    " + inv1.format() + "    " + inv2.format());
        // System.out.println("isExclusiveFormula(" + inv1.format() + ", " + inv2.format() + ") = " + inv1.isExclusiveFormula(inv2));
        if (inv1.isExclusiveFormula(inv2)) {
          result.add(new Invariant[] { inv1, inv2 });
        }
      }
    }
    return result;
  }


  // Different_invariants and same_invariants should be merged.
  // They are used by the code that adds implications.


  // Determine which elements of invs1 differ from elements of invs2.
  // Result elements are pairs of Invariants (with one or the other
  // possibly null).
  Vector different_invariants(Invariants invs1, Invariants invs2) {
    SortedSet ss1 = new TreeSet(icfp);
    // ss1.addAll(invs1);
    for (int j=0; j<invs1.size(); j++) {
      Invariant inv = (Invariant)invs1.get(j);
      ss1.add(inv);
    }

    SortedSet ss2 = new TreeSet(icfp);
    for (int j=0; j<invs2.size(); j++) {
      Invariant inv = (Invariant)invs2.get(j);
      ss2.add(inv);
    }

    Vector result = new Vector();
    for (OrderedPairIterator opi = new OrderedPairIterator(ss1.iterator(), ss2.iterator(), icfp); opi.hasNext(); ) {
      Pair pair = (Pair) opi.next();
      if ((pair.a == null) || (pair.b == null) || (icfp.compare(pair.a, pair.b) != 0)) {
        result.add(new Invariant[] { (Invariant) pair.a, (Invariant) pair.b });
      }
    }
    return result;
  }


  // Determine which invariants at the program points differ.
  // Result elements are pairs of Invariants (with one or the other
  // possibly null.)
  Vector different_invariants(PptSlice[][] matched_views) {
    Vector result = new Vector();
    for (int i=0; i<matched_views.length; i++) {
      PptSlice cond1 = matched_views[i][0];
      PptSlice cond2 = matched_views[i][1];
      Invariants invs1 = (cond1 == null) ? new Invariants() : cond1.invs;
      Invariants invs2 = (cond2 == null) ? new Invariants() : cond2.invs;
      result.addAll(different_invariants(invs1, invs2));
    }
    return result;
  }


  // Determine which elements of invs1 are the same as elements of invs2.
  // Result elements are Invariants.
  Vector same_invariants(Invariants invs1, Invariants invs2) {
    SortedSet ss1 = new TreeSet(icfp);
    ss1.addAll(invs1);
    SortedSet ss2 = new TreeSet(icfp);
    ss2.addAll(invs2);
    Vector result = new Vector();
    for (OrderedPairIterator opi = new OrderedPairIterator(ss1.iterator(), ss2.iterator(), icfp); opi.hasNext(); ) {
      Pair pair = (Pair) opi.next();
      if (pair.a != null && pair.b != null) {
        Invariant inv1 = (Invariant) pair.a;
        Invariant inv2 = (Invariant) pair.b;
        if (inv1.enoughSamples() && inv2.enoughSamples()) {
          result.add(inv1);
        }
      }
    }
    return result;
  }



  ///////////////////////////////////////////////////////////////////////////
  /// Post processing after data trace files are read (but before printing)
  ///

  /**
   * Two things: a) convert Equality invariants into normal IntEqual
   * type for filtering, printing, etc. b) Pivot uninteresting
   * parameter VarInfos so that each equality set contains only the
   * interesting one.  a and b have to be done in the above order.
   **/
  public void postProcessEquality () {
    if (debugEqualTo.isDebugEnabled()) {
      debugEqualTo.debug ("PostProcessingEquality for: " + this.ppt_name);
    }
    Assert.assertTrue (equality_view != null);
    Invariants equalityInvs = equality_view.invs;

    // Pivot invariants to new equality leaders if needed, if old
    // leaders would prevent printing.  Then postprocess them to
    // create 2-way equality invariants.
    for (Iterator i = equalityInvs.iterator(); i.hasNext(); ) {
      Equality inv = (Equality) i.next();
      if (debugEqualTo.isDebugEnabled()) {
        debugEqualTo.debug ("  for: " + inv);
      }
      inv.pivot ();
      inv.postProcess ();
    }

    // Now pivot the other invariants
    Collection slices = viewsAsCollection();
    List pivoted = new LinkedList();

    // PptSlice newSlice = slice.cloneAndInvs(leader, newLeader);

    // Pivot each pptSlice so that each of its VarInfos map back to
    // their leaders.  Except for PptSlice2s, where the two VarInfos map
    // to the same leader.  Leave those alone since they're only there
    // for equality.

    if (debugEqualTo.isDebugEnabled()) {
      debugEqualTo.debug ("  Doing cloneAllPivots: ");
    }
    for (Iterator iSlices = slices.iterator();
         iSlices.hasNext(); ) {
      PptSlice slice = (PptSlice) iSlices.next();
      PptSlice newSlice = slice.cloneAllPivots();
      if (slice != newSlice) {
        pivoted.add (newSlice);
        iSlices.remove(); // Cuz the key is now wrong
      }
    }

    // Add in the removed slices
    for (Iterator iPivoted = pivoted.iterator(); iPivoted.hasNext(); ) {
      PptSlice oPivoted = (PptSlice) iPivoted.next();
      addSlice (oPivoted); // Make the key right again
      if (debugEqualTo.isDebugEnabled()) {
        debugEqualTo.debug ("  Readded: " + oPivoted);
      }
    }


  }


  ///////////////////////////////////////////////////////////////////////////
  /// Locating implied (same) invariants via the simplify theorem-prover
  ///

  // Created upon first use, then saved
  private static SessionManager prover = null;
  private static String prover_background = null;
  public static int prover_instantiate_count = 0;

  private static String prover_background() {
    if (prover_background == null) {
      try {
        StringBuffer result = new StringBuffer("(AND " + Global.lineSep);
        InputStream bg_stream = PptTopLevel.class.getResourceAsStream("simplify/daikon-background.txt");
        Assert.assertTrue(bg_stream != null, "Could not find simplify/daikon-background.txt");
        BufferedReader lines = new BufferedReader(new InputStreamReader(bg_stream));
        String line;
        while ((line = lines.readLine()) != null) {
          line = line.trim();
          if (line.length() == 0) continue;
          if (line.startsWith(";")) continue;
          result.append(" ");
          result.append(line);
          result.append(Global.lineSep);
        }
        result.append(")");
        prover_background = result.toString();
      } catch (IOException e) {
        throw new RuntimeException("Could not load prover background");
      }
    }
    return prover_background;
  }

  // Start up simplify, and send the universal backgound.
  // Is successful exactly when this.prover != null.
  private static void attempt_prover_startup()
  {
    // If already started, we are fine
    if (prover != null) {
      return;
    }

    // Limit ourselves to a few tries
    if (prover_instantiate_count > 5) {
      return;
    }

    // Start the prover
    try {
      prover_instantiate_count++;
      prover = new SessionManager();
      if (Daikon.no_text_output) {
        System.out.print("...");
      }
    } catch (SimplifyError e) {
      System.err.println("Could not utilize Simpilify: " + e);
      return;
    }

    try {
      prover.request(new CmdAssume(prover_background()));
    } catch (TimeoutException e) {
      throw new RuntimeException("Timeout on universal background " + e);
    }
  }

  /**
   * Interface used by mark_implied_via_simplify to determine what
   * invariants should be considered during the logical redundancy
   * tests.
   **/
  public static interface SimplifyInclusionTester {
    public boolean include(Invariant inv);
  }

  /**
   * Use the Simplify theorem prover to flag invariants that are
   * logically implied by others.  Considers only invariants that
   * pass isWorthPrinting.
   **/
  public void mark_implied_via_simplify(PptMap all_ppts) {
    mark_implied_via_simplify
      (all_ppts,
       new SimplifyInclusionTester() {
           public boolean include(Invariant inv) {
             return InvariantFilters.isWorthPrintingFilter().shouldKeep(inv);
           }
         });
  }

  /**
   * Use the Simplify theorem prover to flag invariants that are
   * logically implied by others.  Uses the provided test interface to
   * determine if an invariant is within the domain of inspection.
   **/
  public void mark_implied_via_simplify(PptMap all_ppts, SimplifyInclusionTester test) {
    SessionManager.debugln("Simplify checking " + ppt_name);

    // Create the list of invariants from this ppt which are
    // expressible in Simplify
    Invariant[] invs;
    {
      // Replace parwise equality with an equivalence set
      Collection all = InvariantFilters.addEqualityInvariants(getInvariants());
      Vector printing = new Vector(); // [Invariant]
      for (Iterator _invs = all.iterator(); _invs.hasNext(); ) {
        Invariant inv = (Invariant) _invs.next();
        if (test.include(inv)) { // think: inv.isWorthPrinting()
          String fmt = inv.format_using(OutputFormat.SIMPLIFY);
          if (fmt.indexOf("Simplify") < 0) {
          // If format_simplify is not defined for this invariant, don't
          // confuse Simplify with the error message
            printing.add(inv);
          }
        }
      }
      invs = (Invariant[]) printing.toArray(new Invariant[printing.size()]);
    }

    // For efficiency, bail if we don't have any invariants to mark as implied
    if (invs.length == 0) {
      return;
    }

    // Come up with a "desirability" ordering of the printing and
    // expressible invariants, so that we can remove the least
    // desirable first.  For now just use the ICFP.
    Arrays.sort(invs, icfp);

    // Debugging
    if (Global.debugSimplify.isDebugEnabled()) {
      Global.debugSimplify.debug("Sorted invs:");
      for (int i=0; i<invs.length; i++) {
        Global.debugSimplify.debug("    " + invs[i].format());
      }
      for (int i=0; i<invs.length-1; i++) {
        int cmp = icfp.compare(invs[i], invs[i+1]);
        Global.debugSimplify.debug("cmp(" + i + "," + (i+1) + ") = " + cmp);
        int rev_cmp = icfp.compare(invs[i+1], invs[i]);
        Global.debugSimplify.debug("cmp(" + (i+1) + "," + i + ") = " + rev_cmp);
        Assert.assertTrue(rev_cmp >= 0);
      }
    }

    // [INCR] The below two paragraphs of code (whose end result is to
    // compute "background") should be changed to use the VarInfo
    // partial ordering to determine background invariants, instead of
    // the (now deprecated) controlling_ppts relationship.

    // Form the closure of the controllers; each element is a Ppt
    Set closure = new HashSet();
    {
      // Set working = new HashSet(controlling_ppts); // [INCR]
      Set working = new HashSet();
      while (!working.isEmpty()) {
        PptTopLevel ppt = (PptTopLevel) working.iterator().next();
        working.remove(ppt);
        if (!closure.contains(ppt)) {
          closure.add(ppt);
          // working.addAll(ppt.controlling_ppts); // [INCR]
        }
      }
    }

    // Create the conjunction of the closures' invariants to form a
    // background environment for the prover.  Ignore implications,
    // since in the current scheme, implications came from controlled
    // program points, and we don't necessarily want to lose the
    // unconditional version of the invariant at the conditional ppt.
    StringBuffer all_cont = new StringBuffer();
    all_cont.append("(AND \n");
    for (Iterator ppts = closure.iterator(); ppts.hasNext(); ) {
      PptTopLevel ppt = (PptTopLevel) ppts.next();
      all_cont.append("\t(AND \n");
      Iterator _invs = InvariantFilters.addEqualityInvariants(ppt.getInvariants()).iterator();
      while (_invs.hasNext()) {
        Invariant inv = (Invariant) _invs.next();
        if (inv instanceof Implication) {
          continue;
        }
        if (!test.include(inv)) { // think: !inv.isWorthPrinting()
          continue;
        }
        String fmt = inv.format_using(OutputFormat.SIMPLIFY);
        if (fmt.indexOf("Simplify") >= 0) {
          // If format_simplify is not defined for this invariant, don't
          // confuse Simplify with the error message
          continue;
        }
        // We could also consider testing if the controlling invariant
        // was removed by Simplify, but what would the point be?  Also,
        // these "intermediate goals" might help out Simplify.
        all_cont.append("\t\t");
        all_cont.append(fmt);
        all_cont.append("\n");
        // If this is the :::OBJECT ppt, also restate all of them in
        // orig terms, since we the conditions also held upon entry.
        if (ppt.ppt_name.isObjectInstanceSynthetic()) {
          // XXX Side-effecting the invariant to change its ppt (and then
          // to change it back afterward) isn't such a hot thing to do, but
          // it isn't that hard, and seems to work.
          PptSlice saved = inv.ppt;
          PptSlice orig = new PptSlice0(saved.parent);
          orig.var_infos = new VarInfo[saved.var_infos.length];
          for (int i=0; i<orig.var_infos.length; i++) {
            orig.var_infos[i] = VarInfo.origVarInfo(saved.var_infos[i]);
          }
          inv.ppt = orig;
          all_cont.append("\t\t");
          all_cont.append(inv.format_using(OutputFormat.SIMPLIFY));
          all_cont.append("\n");
          inv.ppt = saved;
        }
      }
      all_cont.append(")");
    }

    // FIXME XXXXX:  Commented out by MDE, 6/26/2002, due to merging problems.
    // Should this be deleted?  Need to check CVS logs and/or think about this.
    // if (ppt_name.isEnterPoint() && controlling_ppts.size() == 1) {
    //   // Guess the OBJECT ppt; usually right
    //   PptTopLevel OBJ = (PptTopLevel) controlling_ppts.pptIterator().next();
    //   if (OBJ.ppt_name.isObjectInstanceSynthetic()) {
    //     // Find variables here of the same type as us
    //     String clsname = ppt_name.getFullClassName();
    //   }
    // }
    //
    // // Use type information to restate any OBJECT invariants over
    // // variable expressions such as arguments or fields whose types
    // // are instrumeted.
    // for (int i=0; i < var_infos.length; i++) {
    //   VarInfo vi = var_infos[i];
    //   ProglangType progtype = vi.type;
    //
    //   // Always skip "this" and "orig(this)" as necessary special cases.
    //   if (VarInfoName.THIS.equals(vi.name) || VarInfoName.ORIG_THIS.equals(vi.name)) {
    //     continue;
    //   }
    //
    //   // For now, we don't handle sequences.  We could use a GLB type
    //   // and state a forall, but it doesn't seem worth the work yet.
    //   if (progtype.isPseudoArray()) {
    //     continue;
    //   }
    //
    //   // Locate the OBJECT ppt
    //   PptName obj_name = new PptName(vi.type.base(), null, FileIO.object_suffix);
    //   PptTopLevel obj_ppt = all_ppts.get(obj_name);
    //   if (obj_ppt == null) {
    //     Global.debugSimplify.debug
    //       (ppt_name + ": no type-based invariants found for "
    //        + vi.name + " (" + obj_name + ")");
    //     continue;
    //   }
    //
    //   Global.debugSimplify.debug
    //       (ppt_name + ": using type-based invariants for "
    //        + vi.name + " (" + obj_name + ")");
    //
    //   // State the object invariant on the incoming argument
    //   all_cont.append("\t(AND \n");
    //   Iterator _invs = InvariantFilters.addEqualityInvariants(obj_ppt.getInvariants()).iterator();
    //   while (_invs.hasNext()) {
    //     Invariant inv = (Invariant) _invs.next();
    //     if (!test.include(inv)) { // think: !inv.isWorthPrinting()
    //       continue;
    //     }
    //     String fmt = inv.format_simplify();
    //     if (fmt.indexOf("format_simplify") >= 0) {
    //       continue;
    //     }
    //     // XXX This isn't such a hot thing to do, but it isn't that
    //     // hard, and seems to work.
    //     PptSlice saved = inv.ppt;
    //     PptSlice rewritten = new PptSlice0(saved.parent);
    //     rewritten.var_infos = new VarInfo[saved.var_infos.length];
    //     for (int x=0; x<rewritten.var_infos.length; x++) {
    //       VarInfo svi = saved.var_infos[x];
    //       rewritten.var_infos[x] =
    //         new VarInfo(svi.name.replace(VarInfoName.THIS, vi.name),
    //                     svi.type, svi.file_rep_type,
    //                     svi.comparability.makeAlias(svi.name), svi.aux);
    //     }
    //     inv.ppt = rewritten;
    //     all_cont.append("\t\t");
    //     all_cont.append(inv.format_simplify());
    //     all_cont.append("\n");
    //     inv.ppt = saved;
    //   }
    //   all_cont.append(")");
    // }

    all_cont.append(")");
    CmdAssume background = new CmdAssume(all_cont.toString());

    // Send the background to the prover
    try {
      attempt_prover_startup();
      if (prover == null) return;
      prover.request(background);
    } catch (TimeoutException e) {
      prover = null;
      return;
    }

    // Work from back to front, and flag things that are redundant
    boolean[] present = new boolean[invs.length];
    Arrays.fill(present, 0, present.length, true);
    for (int checking = invs.length-1; checking >= 0; checking--) {
      Invariant inv = invs[checking];
      StringBuffer bg = new StringBuffer("(AND ");
      for (int i=0; i < present.length; i++) {
        if (present[i] && (i != checking)) {
          bg.append(" ");
          // format_using(OutputFormat.SIMPLIFY) is guaranteed to return
          // a sensible result xfor invariants in invs[].
          bg.append(invs[i].format_using(OutputFormat.SIMPLIFY));
        }
      }
      bg.append(")");

      // Debugging
      if (Global.debugSimplify.isDebugEnabled()) {
      SessionManager.debugln("Background:");
      for (int i=0; i < present.length; i++) {
        if (present[i] && (i != checking)) {
          SessionManager.debugln("    " + invs[i].format());
        }
      }
      }

      try {
        // If the background is necessarily false, we are in big trouble
        CmdCheck bad = new CmdCheck("(NOT " + bg + ")");
        attempt_prover_startup();
        if (prover == null) return;
        prover.request(bad);
        if (bad.valid) {
          // BAD!!
          System.err.println("Warning: " + ppt_name + " invariants are contradictory; punting!");
          return;
        }

        // The background wasn't necessarily false; see if it implies
        // the invariant under test.
        String ask = "(IMPLIES " + bg + " " + inv.format_using(OutputFormat.SIMPLIFY) + ")";
        CmdCheck cc = new CmdCheck(ask); // result is initialized to false
        prover.request(cc);
        if (cc.valid) {
          // ick ick ick
          if (inv instanceof Equality) {
            // Equality is not represented with a permanent invariant
            // object, so store the canonical variable instead.

            // // Debugging
            // System.out.println("Adding redundant var " + ((Equality) inv).leader().name.name() + " due to " + inv.format());
            // System.out.println("Background = ");
            // for (int i=0; i < present.length; i++) {
            //   if (i == checking) {
            //     System.out.println("  <<this invariant not in its own background>>");
            //   }
            //   if (present[i] && (i != checking)) {
            //     System.out.println("  " + invs[i].format() + "\t" + invs[i].getClass().getName());
            //   }
            // }

            redundant_invs.add(((Equality) inv).leader());
          } else {
            redundant_invs.add(inv);
          }
          present[checking] = false;
        }
        SessionManager.debugln((present[checking] ? "UNIQUE" : "REDUNDANT") + " " + invs[checking].format());
      } catch (SimplifyError e) {
        prover = null;
        return;
      } catch (TimeoutException e) {
        // Reset the prover with the controlling invariant background
        prover = null;
        attempt_prover_startup();
        if (prover == null) return;
        try {
          prover.request(background);
        } catch (TimeoutException f) {
          prover = null;
          return;
        }
      }
    }

    // Remove the controlling invariant background
    try {
      prover.request(CmdUndoAssume.single);
    } catch (TimeoutException e) {
      prover = null;
    }
  }


  ///////////////////////////////////////////////////////////////////////////
  /// Parameter VarInfo processing
  ///

  /**
   * Cached VarInfoNames that are parameter variables.
   **/
  private Set paramVars = null;

  /**
   * Returns variables in this Ppt that are parameters.
   **/
  public Set getParamVars() {
    if (paramVars != null) return paramVars;

    paramVars = new HashSet();
    for (int i = 0; i < var_infos.length; i++) {
      VarInfo var = var_infos[i];
      if (var.aux.getFlag(VarInfoAux.IS_PARAM)) {
        paramVars.add(var.name);
      }
    } // We should cache paramedVars in PptToplevel
    return paramVars;
  }


  ///////////////////////////////////////////////////////////////////////////
  /// Printing invariants
  ///

  // This is a fairly inefficient method, as it does a lot of copying.
  // As of 1/9/2000, this is only used in print_invariants.
  /**
   * Return a List of all the invariants for the program point.
   * Also consider using views_iterator() instead.  You can't
   * modify the result of this.
   **/
  // Used to be known as invariants_vector, but changed to return a
  // List.
  public List getInvariants() {
    List result = new ArrayList();
    for (Iterator itor = new ViewsIteratorIterator(this); itor.hasNext(); ) {
      for (Iterator itor2 = (Iterator) itor.next(); itor2.hasNext(); ) {
        result.add(itor2.next());
      }
    }
    // Old implementation:  was slightly more efficient, but separate code
    // permitted drift between it an ViewsIteratorIterator.
    // for (Iterator views_itor = views.iterator(); views_itor.hasNext(); ) {
    //   PptSlice slice = (PptSlice) views_itor.next();
    //   result.addAll(slice.invs);
    // }
    // // System.out.println(implication_view.invs.size() + " implication invs for " + name + " at " + implication_view.name);
    // result.addAll(implication_view.invs);
    return Collections.unmodifiableList(result);
  }

  /**
   * @return the number of views
   **/
  public int views_size() {
    return viewsAsCollection().size();
  }

  /**
   * For some clients, this method may be more efficient than getInvariants.
   **/
  public Iterator views_iterator() {
    // assertion only true when guarding invariants
    // Assert.assertTrue(views.contains(joiner_view));
    return viewsAsCollection().iterator();
  }

  public Iterator invariants_iterator() {
    return new UtilMDE.MergedIterator(views_iterator_iterator());
  }

  private Iterator views_iterator_iterator() {
    return new ViewsIteratorIterator(this);
  }

  /** An iterator whose elements are themselves iterators that return invariants. **/
  public static final class ViewsIteratorIterator implements Iterator {
    Iterator vitor;
    Iterator implication_iterator;
    public ViewsIteratorIterator(PptTopLevel ppt) {
      vitor = ppt.views_iterator();
      implication_iterator = ppt.joiner_view.invs.iterator();
    }
    public boolean hasNext() {
      return (vitor.hasNext() || (implication_iterator != null));
    }
    public Object next() {
      if (vitor.hasNext())
        return ((PptSlice)vitor.next()).invs.iterator();
      else {
        Iterator tmp = implication_iterator;
        implication_iterator = null;
        return tmp;
      }
    }
    public void remove() {
      throw new UnsupportedOperationException();
    }
  }


  /**
   * Simplify the names of variables before printing them.  For
   * example, "orig(a[post(i)])" might change into "orig(a[i+1])".  We
   * might want to switch off this behavior, depending on various
   * heuristics.  We'll have to try it and see which output we like
   * best.  In any case, we have to do this for ESC output, since ESC
   * doesn't have anything like post().
   **/
  public void simplify_variable_names() {
    Iterator iter = Arrays.asList(var_infos).iterator();
    while (iter.hasNext()) {
      VarInfo vi = (VarInfo) iter.next();
      vi.simplify_expression();
    }
  }


  final public static Comparator icfp = new Invariant.InvariantComparatorForPrinting();

  static Comparator arityVarnameComparator = new PptSlice.ArityVarnameComparator();

  //////////////////////////////////////////////////////////////////////////////
  ///// Invariant guarding


  // This function guards all of the invariants in a PptTopLevel
  public void guardInvariants() {
    // To avoid concurrent modification exceptions using arrays
    Object viewArray[] = viewsAsCollection().toArray();
    for (int i=0; i < viewArray.length; i++) {
      PptSlice currentView = (PptSlice)viewArray[i];
      currentView.guardInvariants();
    }

    Object viewCondArray[] = views_cond.toArray();
    for (int i=0; i < viewCondArray.length; i++) {
      PptSlice currentCondView = (PptSlice)viewCondArray[i];
      currentCondView.guardInvariants();
    }

    // System.out.println("Ppt name: " + ppt_name);
    // System.out.println("Number of invs in joiner_view: " + joiner_view.invs.size());
  }

  /**
   * Check the rep invariants of this.  Throw an Error if not okay.
   **/
  public void repCheck() {
    // We could check a lot more than just that slices are okay.  For
    // example, we could ensure that flow graph is correct.
    for (Iterator i = viewsAsCollection().iterator(); i.hasNext(); ) {
      PptSlice slice = (PptSlice) i.next();
      slice.repCheck();
    }
    if (equality_view != null) equality_view.repCheck();
    if (dataflow_ppts != null) {
      Assert.assertTrue (dataflow_ppts[dataflow_ppts.length - 1] == this);
    }
  }

  /**
   * Debug method to display all slices
   **/
  public String debugSlices() {
    StringBuffer result = new StringBuffer();
    result.append ("Slices for: " + this.ppt_name);
    for (Iterator i = viewsAsCollection().iterator(); i.hasNext(); ) {
      PptSlice slice = (PptSlice) i.next();
      result.append ("\n" + slice.toString());
    }
    return result.toString();
  }
}
