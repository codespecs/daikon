package daikon;

import daikon.derive.*;
import daikon.derive.unary.*;
import daikon.derive.binary.*;
import daikon.derive.ternary.*;
import daikon.inv.*;
import daikon.inv.unary.*;
import daikon.inv.unary.scalar.*;
import daikon.inv.Invariant.OutputFormat;
import daikon.inv.filter.*;
import daikon.inv.unary.*;
import daikon.inv.binary.*;
import daikon.inv.ternary.*;
import daikon.inv.binary.twoScalar.*;
import daikon.inv.binary.twoString.*;
import daikon.inv.binary.twoSequence.*;
import daikon.simplify.*;
import daikon.split.*;
import daikon.split.misc.*;
import daikon.suppress.*;
import utilMDE.Assert;
import daikon.inv.filter.InvariantFilters;

import java.io.*;
import java.util.*;
import java.text.*;

import org.apache.oro.text.regex.*;
import java.util.logging.Logger;
import java.util.logging.Level;

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
  static final long serialVersionUID = 20030929L;

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

  /**
   * Remove invariants at lower program points when a matching invariant is
   * created at a higher program point. For experimental purposes only.
   */
  public static boolean dkconfig_remove_merged_invs = false;

  /** number of invariants after equality set processing for the last sample */
  public int instantiated_inv_cnt = 0;

  /** number of slices after equality set processing for the last sample */
  public int instantiated_slice_cnt = 0;

  /** Main debug tracer **/
  public static final Logger debug =
    Logger.getLogger("daikon.PptTopLevel");

  /** debug tracer for instantiated slices **/
  public static final Logger debugInstantiate =
    Logger.getLogger ("daikon.PptTopLevel.instantiate");

  /** Debug tracer for equalTo checks **/
  public static final Logger debugEqualTo =
    Logger.getLogger("daikon.PptTopLevel.equal");

  /** Debug tracer for addImplications. **/
  public static final Logger debugAddImplications =
    Logger.getLogger("daikon.PptTopLevel.addImplications");

  /** Debug tracer for adding and processing conditional ppts */
  public static final Logger debugConditional =
    Logger.getLogger("daikon.PptTopLevel.conditional");

  /** Debug tracer for data flow. **/
  public static final Logger debugFlow =
    Logger.getLogger ("daikon.flow.flow");

  /** Debug tracer for start of suppression. **/
  public static final Logger debugSuppressInit =
    Logger.getLogger ("daikon.suppress.init");

  /** Debug tracer for suppression. **/
  public static final Logger debugSuppress =
    Logger.getLogger ("daikon.suppress.suppress");

  /** Debug tracer for up-merging equality sets  **/
  public static final Logger debugMerge =
    Logger.getLogger ("daikon.PptTopLevel.merge");

  /** Debug tracer for global ppt **/
  public static final Logger debugGlobal =
    Logger.getLogger ("daikon.PptTopLevel.global");

  // These used to appear in Ppt, were moved down to PptToplevel
  public final String name;
  public final PptName ppt_name;

  public final String name() {
    return name;
  }

  /** Holds the falsified invariants under this PptTopLevel */
  public ArrayList falsified_invars = new ArrayList();

  /** list of constant variables */
  public DynamicConstants constants = null;

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
  // [INCR] private int values_num_mod_samples;
  // [INCR] private int values_num_values;

  ModBitTracker mbtracker;

  ValueSet[] value_sets;

  /**
   * All the Views (that is, slices) on this are stored as values in
   * the HashMap
   * Provided so that this Ppt can notify them when significant events
   * occur, such as receiving a new value, deriving variables, or
   * discarding data.
   * Indexed by a Arrays.asList array list of Integers holding
   * varinfo_index values
   **/
  private Map/*[Integer[]->PptSlice]*/ views;

  /** List of all of the splitters for this ppt. */
  public /*PptSplitter*/ ArrayList splitters = null;

  /**
   * Iterator for all of the conditional ppts.  Returns each PptConditional
   * from each entry in splitters
   */
  public class CondIterator implements java.util.Iterator {

    int splitter_index = 0;
    int ppts_index = 0;

    public boolean hasNext() {
      if (splitters == null)
        return (false);
      if (splitter_index >= splitters.size())
        return (false);
      return (true);
    }

    public Object next() {

      PptSplitter ppt_split = (PptSplitter) splitters.get(splitter_index);
      PptTopLevel ppt = ppt_split.ppts[ppts_index];

      if (ppts_index < (ppt_split.ppts.length - 1)) {
        ppts_index++;
      } else {
        splitter_index++;
        ppts_index = 0;
      }
      return (ppt);
    }

    public void remove() {
      throw new UnsupportedOperationException
                                ("Remove unsupported in CondIterator");
    }
  }

  public CondIterator cond_iterator() {
    return new CondIterator();
  }

  /** returns whether or not this ppt has any splitters */
  public boolean has_splitters() {
    return (splitters != null) && (splitters.size() > 0);
  }

  /** all children relations in the hierarchy */
  public List /* PptRelation */ children = new ArrayList();

  /** all parent relations in the hierarchy */
  public List /* PptRelation */ parents = new ArrayList();

  /**
   *  Flag that indicates whether or not invariants have been merged
   *  from all of this ppts children to form the invariants here.  Necessary
   *  because a ppt can have multiple parents and otherwise we'd needlessly
   *  merge multiple times
   */
  public boolean invariants_merged = false;

  /**
   * Flag that indicates whether or not invariants that are duplicated
   * at the parent have been removed
   */
  public boolean invariants_removed = false;

  /**
   * VarInfo index transform from this point to the ppt containing
   * all of the global variables.  The index into global transform is
   * the value_index in global ppt.  The result (global_transform[index])
   * is the value_index of the corresponding variable in this ppt
   *
   * Ther are two transforms for each exit point. Orig variables are
   * handled by global_transform_orig and post variables are handled
   * by global_transform_post.  This is necessary because bottom up
   * only processes numbered exit points, so both the orig and the post
   * value must be separately applied to the global ppt.
   */
  public int[] global_transform_orig = null;
  /** @see #global_transform_orig */
  public int[] global_transform_post = null;

  /** Global ppt (if any) **/
  public static PptTopLevel global = null;

  /** list of weakened invariants at the global ppt */
  public static List weakened_invs = new ArrayList();

  public static int weakened_start_index = 0;

  /**
   * Set of all PptTopLevels where the ordering provided by the
   * links is sorted from the smallest offset to the largest offset
   * This takes advantage of LinkedHashSet predictable ordering over
   * elements (insertion-order)
   */
  public static Set /* PptTopLevel */ weakened_offsets = new LinkedHashSet();

  /** offset of this ppt into the list of weakened invariants **/
  public int weakened_offset = 0;

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
  /** @see #dataflow_ppts */
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
  /** @see #invflow_ppts */
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
  public Set redundant_invs = new LinkedHashSet(0);

  public PptTopLevel(String name, VarInfo[] var_infos) {
    this.name = name;
    ppt_name = new PptName(name);
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
    views = new LinkedHashMap();

    num_declvars = var_infos.length;
    num_tracevars = val_idx;
    num_orig_vars = 0;
    Assert.assertTrue(num_static_constant_vars == num_declvars - num_tracevars);
    // System.out.println("Created PptTopLevel " + name() + ": "
    //                    + "num_static_constant_vars=" + num_static_constant_vars
    //                    + ",num_declvars=" + num_declvars
    //                    + ",num_tracevars=" + num_tracevars);

    Assert.assertTrue(num_tracevars == var_infos.length - num_static_constant_vars);
    mbtracker = new ModBitTracker(num_tracevars);
    value_sets = new ValueSet[num_tracevars];
    for (int i=0; i<var_infos.length; i++) {
      VarInfo vi = var_infos[i];
      int value_index = vi.value_index;
      if (value_index == -1) {
        continue;
      }
      Assert.assertTrue(value_sets[value_index] == null);
      value_sets[value_index] = ValueSet.factory(vi);
    }
    for (int i=0; i<num_tracevars; i++) {
      Assert.assertTrue(value_sets[i] != null);
    }
  }


  public static void init (PptMap all_ppts) {

    // Init the set of ppts used to track the index into the weakened invs
    // list.  The initial order is irrelevant since each needs to start
    // at the beginning of the weakened_invs list.
    for (Iterator i = all_ppts.ppt_all_iterator(); i.hasNext(); ) {
      PptTopLevel ppt = (PptTopLevel) i.next();
      if (ppt.ppt_name.isExitPoint() && !ppt.ppt_name.isCombinedExitPoint())
        weakened_offsets.add (ppt);
    }
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
    if (splitters != null) { splitters.trimToSize(); }
  }

  /** The number of samples processed by this program point so far. **/
  public int num_samples() {
    return values_num_samples;
  }

  public int num_samples(VarInfo vi1) {
    if (vi1.is_static_constant) {
      return mbtracker.num_samples();
    }
    BitSet b1 = mbtracker.get(vi1.value_index);
    int num_slice_samples = b1.cardinality();
    return num_slice_samples;
  }

  public int num_samples(VarInfo vi1, VarInfo vi2) {
    if (vi1.is_static_constant) {
      return num_samples(vi2);
    }
    if (vi2.is_static_constant) {
      return num_samples(vi1);
    }
    BitSet b1 = mbtracker.get(vi1.value_index);
    BitSet b2 = mbtracker.get(vi2.value_index);
    int num_slice_samples = UtilMDE.intersectionCardinality(b1, b2);
    return num_slice_samples;
  }

  public int num_samples(VarInfo vi1, VarInfo vi2, VarInfo vi3) {
    if (vi1.is_static_constant) {
      return num_samples(vi2, vi3);
    }
    if (vi2.is_static_constant) {
      return num_samples(vi1, vi3);
    }
    if (vi3.is_static_constant) {
      return num_samples(vi1, vi2);
    }
    BitSet b1 = mbtracker.get(vi1.value_index);
    BitSet b2 = mbtracker.get(vi2.value_index);
    BitSet b3 = mbtracker.get(vi3.value_index);
    int num_slice_samples = UtilMDE.intersectionCardinality(b1, b2, b3);
    return num_slice_samples;
  }

  /** The number of distinct values that have been seen. **/
  public int num_values(VarInfo vi1) {
    if (vi1.is_static_constant) {
      // This test is deeply wrong; I should always return 1.  But see what
      // effect this has.
      if (Daikon.dkconfig_df_bottom_up) {
        return 1;
      } else {
        return 0;
      }
    }
    ValueSet vs1 = value_sets[vi1.value_index];
    return vs1.size();
  }

  /** An upper bound on the number of distinct values that have been seen. **/
  public int num_values(VarInfo vi1, VarInfo vi2) {
    return num_values(vi1) * num_values(vi2);
  }

  /** An upper bound on the number of distinct values that have been seen. **/
  public int num_values(VarInfo vi1, VarInfo vi2, VarInfo vi3) {
    return num_values(vi1) * num_values(vi2) * num_values(vi3);
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
   * @see #addVarInfos(VarInfo)
   **/
  void addVarInfos(VarInfo[] vis) {
    if (vis.length == 0)
      return;
    int old_length = var_infos.length;
    VarInfo[] new_var_infos = new VarInfo[var_infos.length + vis.length];
    Assert.assertTrue(mbtracker.num_samples() == 0);
    mbtracker = new ModBitTracker(mbtracker.num_vars() + vis.length);
    System.arraycopy(var_infos, 0, new_var_infos, 0, old_length);
    System.arraycopy(vis, 0, new_var_infos, old_length, vis.length);
    for (int i=old_length; i<new_var_infos.length; i++) {
      VarInfo vi = new_var_infos[i];
      vi.varinfo_index = i;
      vi.value_index = i - num_static_constant_vars;
      vi.ppt = this;
    }
    var_infos = new_var_infos;
    int old_vs_length = value_sets.length;
    ValueSet[] new_value_sets = new ValueSet[old_vs_length + vis.length];
    System.arraycopy(value_sets, 0, new_value_sets, 0, old_vs_length);
    for (int i=0; i<vis.length; i++) {
      new_value_sets[old_vs_length+i] = ValueSet.factory(vis[i]);
    }
    value_sets = new_value_sets;
  }


  /* [INCR] ...
  ///////////////////////////////////////////////////////////////////////////
  /// Finding an object or class ppt for a given ppt
  ///

  void set_controlling_ppts(PptMap all_ppts)
  {
    // TODO: also require that this is a public method
    if (ppt_name.isEnterPoint() || ppt_name.isExitPoint()) {
      PptTopLevel object_ppt = (PptTopLevel) all_ppts.get(ppt_name.makeObject());
      if (object_ppt != null) {
        controlling_ppts.add(object_ppt);
      } else {
        // If we didn't find :::OBJECT, fall back to :::CLASS
        PptTopLevel class_ppt = (PptTopLevel) all_ppts.get(ppt_name.makeClassStatic());
        if (class_ppt != null) {
          controlling_ppts.add(class_ppt);
        }
      }
    } else if (ppt_name.isObjectInstanceSynthetic()) {
      PptTopLevel class_ppt = (PptTopLevel) all_ppts.get(ppt_name.makeClassStatic());
      if (class_ppt != null) {
        controlling_ppts.add(class_ppt);
      }
    }
  }


  ///////////////////////////////////////////////////////////////////////////
  /// Adding special variables
  ///

  // Given a program point, if it represents a function exit, then
  // return the corresponding function entry point.  The result is
  // cached in the entry_ppt slot, to prevent repeating this expensive
  // computation.

  void compute_entry_ppt(PptMap all_ppts) {
    if (ppt_name.isExitPoint() || ppt_name.isThrowsPoint()) {
      entry_ppt = (PptTopLevel) all_ppts.get(ppt_name.makeEnter());
      if (ppt_name.isExitPoint()) {
        if (entry_ppt == null) {
          throw new Error("Found no entry point for exit point " + this.name);
        }
        // System.out.println("Adding exit point " + this.name + " to " + entry_ppt.name());
        entry_ppt.exit_ppts.add(this);
      }
    }
  }

  // Add "_orig" (prestate) variables to the program point.
  // Derivation should not yet have occurred for the entry program point.
  void add_orig_vars(PptTopLevel entry_ppt) {
    VarInfo[] begin_vis = entry_ppt.var_infos;
    num_orig_vars = begin_vis.length - entry_ppt.num_static_constant_vars;
    Assert.assertTrue(num_orig_vars == entry_ppt.num_tracevars);
    // Don't bother to include the constants.
    VarInfo[] new_vis = new VarInfo[num_orig_vars];
    int new_vis_index = 0;
    for (int i=0; i<begin_vis.length; i++) {
      VarInfo vi = begin_vis[i];
      if (vi.isStaticConstant() || vi.isDerived())
        continue;
      VarInfo origvar = VarInfo.origVarInfo(vi);
      {
        VarInfo postvar = findVar(vi.name);
        if (postvar == null) {
          System.out.println("no postvar for origvar " + origvar.name.name());
        }
        origvar.comparability = postvar.comparability.makeAlias(origvar.name);
      }
      new_vis[new_vis_index] = origvar;
      new_vis_index++;
    }
    Assert.assertTrue(new_vis_index == num_orig_vars);
    addVarInfos(new_vis);
  }



  /// Possibly just blow this off; I'm not sure I care about it.
  /// In any event, leave it until later.
  //
  // void add_invocation_count_vars() {
  //
  //   // Add invocation counts
  //   if (compute_invocation_counts) {
  //     for ppt in fns_to_process {
  //       these_var_infos = fn_var_infos[ppt];
  //       for callee in fn_invocations.keys() {
  //    calls_var_name = "calls(%s)" % callee;
  //    these_var_infos.append(var_info(calls_var_name, "integral", "always", len(these_var_infos)));
  //    these_values.append(fn_invocations[callee]);
  //    current_var_index++;
  //       }
  //     }
  //   }
  //
  //       (ppt_sans_suffix, ppt_suffix) = (string.split(ppt, ":::", 1) + [""])[0:2]
  //       if ((ppt_suffix != "EXIT")
  //      and (ppt_suffix[0:4] != "EXIT")):
  //      continue
  //       these_var_infos = fn_var_infos[ppt]
  //       entry_ppt = ppt_sans_suffix + ":::ENTER"
  //       for vi in fn_var_infos[entry_ppt][0:fn_truevars[entry_ppt]]:
  //      these_var_infos.append(var_info("orig(" + vi.name + ")", vi.type, comparability_make_alias(vi.name, vi.comparability), len(these_var_infos)))
  //
  // }
  */ // ... [INCR]


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

  transient TernaryDerivationFactory[] ternaryDerivations
    = new TernaryDerivationFactory[] {
        new SequenceScalarArbitrarySubsequenceFactory(),
        new SequenceStringArbitrarySubsequenceFactory(),
        new SequenceFloatArbitrarySubsequenceFactory(),
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

  /* // [INCR] ... we longer need to do this in stages
  public Vector __derive() {
    Assert.assertTrue(ArraysMDE.sorted_descending(derivation_indices));

    Vector result = new Vector();
    for (int pass=1; pass<=derivation_passes; pass++) {
      int this_di = derivation_indices[pass];
      int last_di = derivation_indices[pass-1];
      if (Global.debugDerive.isLoggable(Level.FINE))
        Global.debugDerive.fine ("pass=" + pass + ", range=" + this_di + ".." + last_di);
      if (this_di == last_di) {
        if (Global.debugDerive.isLoggable(Level.FINE)) {
          Global.debugDerive.fine ("No pass " + pass + " derivation to do");
        }
        continue;
      }
      result.addAll(deriveVariablesOnePass(this_di, last_di,
                                           unaryDerivations[pass-1],
                                           binaryDerivations[pass-1],
                                           ternaryDerivations[pass-1]));
    }
    // shift values in derivation_indices:  convert [a,b,c] into [n,a,b]
    for (int i=derivation_passes; i>0; i--)
      derivation_indices[i] = derivation_indices[i-1];
    derivation_indices[0] = var_infos.length + result.size();

    if (Global.debugDerive.isLoggable(Level.FINE)) {
      Global.debugDerive.fine (name + ": derived " + result.size()
                               + " new variables; "
                               + "new derivation_indices: "
                               + ArraysMDE.toString(derivation_indices));
      // Alternately, and probably more usefully
      for (int i=0; i<result.size(); i++) {
        Global.debugDerive.fine
          ("  " + ((Derivation)result.get(i)).getVarInfo().name.name());
      }
    }
    return result;
  }
  */ // ... [INCR]

  /**
   * This routine creates derivations for one "pass"; that is, it adds
   * some set of derived variables, according to the functions that
   * are passed in.  All the results involve at least one VarInfo
   * object at an index i such that vi_index_min <= i < vi_index_limit
   * (and possibly other VarInfos outside that range).
   * @return a Vector of VarInfo
   **/

  // Formerly known as "deriveVariablesOnePass", but now there's only
  // ever one pass.
  private Derivation[] derive(int vi_index_min,
                              int vi_index_limit)
  {
    UnaryDerivationFactory[] unary = unaryDerivations;
    BinaryDerivationFactory[] binary = binaryDerivations;
    TernaryDerivationFactory[] ternary = ternaryDerivations;

    if (Global.debugDerive.isLoggable(Level.FINE)) {
      Global.debugDerive.fine ("Deriving one pass for ppt " + this.name);
      Global.debugDerive.fine ("vi_index_min=" + vi_index_min
                               + ", vi_index_limit=" + vi_index_limit
                               + ", unary.length=" + unary.length
                               + ", binary.length=" + binary.length
                               + ", ternary.length=" + ternary.length);
    }

    Collection result = new ArrayList();

    for (int i=vi_index_min; i<vi_index_limit; i++) {
      VarInfo vi = var_infos[i];
      if (Global.debugDerive.isLoggable(Level.FINE)) {
        Global.debugDerive.fine ("Unary: trying to derive from " + vi.name.name());
      }
      if (!worthDerivingFrom(vi)) {
        if (Global.debugDerive.isLoggable(Level.FINE)) {
          Global.debugDerive.fine ("Unary: not worth deriving from " + vi.name.name());
          // [INCR] Global.debugDerive.fine ("Canonicality is: " + vi.isCanonical());
          // [INCR] Global.debugDerive.fine ("Equal_to: " + vi.equal_to.name.name());
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
        if (Global.debugDerive.isLoggable(Level.FINE)) {
          Global.debugDerive.fine ("Binary first VarInfo: not worth deriving from " + vi1.name.name());
          // [INCR] Global.debugDerive.fine ("Canonicality is: " + vi1.isCanonical());
          // [INCR] Global.debugDerive.fine ("Equal_to: " + vi1.equal_to.name.name());
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
      // if (Global.debugDerive.isLoggable(Level.FINE))
      //   Global.debugDerive.fine ("i1=" + i1
      //                      + ", i2_min=" + i2_min
      //                      + ", i2_limit=" + i2_limit);
      for (int i2=i2_min; i2<i2_limit; i2++) {
        VarInfo vi2 = var_infos[i2];
        if (!worthDerivingFrom(vi2)) {
          if (Global.debugDerive.isLoggable(Level.FINE)) {
            Global.debugDerive.fine ("Binary: not worth deriving from ("
                               + vi1.name.name() + "," + vi2.name.name() + ")");
            /* [INCR]
            Global.debugDerive.fine ("Canonicality is: " + vi2.isCanonical());
            Global.debugDerive.fine ("Equal_to: " + vi2.equal_to.name.name());
            */ // [INCR]
          }
          continue;
        }
        for (int di=0; di<binary.length; di++) {
          BinaryDerivationFactory d = binary[di];
          if (Debug.logOn())
            Debug.log (d.getClass(), vi1.ppt, Debug.vis (vi1, vi2),
                           "Trying Binary Derivation ");
          BinaryDerivation[] bderivs = d.instantiate(vi1, vi2);
          if (bderivs != null) {
            for (int bdi=0; bdi<bderivs.length; bdi++) {
              BinaryDerivation bderiv = bderivs[bdi];
              if ((Daikon.var_omit_regexp != null)
                  && Global.regexp_matcher.contains(bderiv.getVarInfo().name.name(), Daikon.var_omit_regexp)) {
                continue;
              }
              result.add(bderiv);
              if (Debug.logOn())
                Debug.log (d.getClass(), vi1.ppt, Debug.vis (vi1, vi2),
                               "Created Binary Derivation "
                                + bderiv.getVarInfo().name.name());
            }
          }
        }
      }
    }

    // Ternary derivations follow the same pattern, one level deeper.
    for (int i1=0; i1<var_infos.length; i1++) {
      VarInfo vi1 = var_infos[i1];
      if (vi1.isDerived()) {
        if (Global.debugDerive.isLoggable(Level.FINE)) {
          Global.debugDerive.fine ("Ternary first VarInfo: not worth " +
                                   "deriving from " + vi1.name.name());
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
      // if (Global.debugDerive.isLoggable(Level.FINE))
      //   Global.debugDerive.fine ("i1=" + i1
      //                      + ", i2_min=" + i2_min
      //                      + ", i2_limit=" + i2_limit);
      for (int i2=i2_min; i2<i2_limit; i2++) {
        VarInfo vi2 = var_infos[i2];
        if (vi2.isDerived() ||
            !TernaryDerivationFactory.checkType(vi1,vi2) ||
            !TernaryDerivationFactory.checkComparability(vi1,vi2)){
          if (Global.debugDerive.isLoggable(Level.FINE)) {
            Global.debugDerive.fine ("Ternary 2nd: not worth deriving from ("
                                     + vi1.name.name() + ","
                                     + vi2.name.name() + ")");
            /* [INCR]
            Global.debugDerive.fine ("Canonicality is: " + vi2.isCanonical());
            Global.debugDerive.fine ("Equal_to: " + vi2.equal_to.name.name());
            */ // [INCR]
          }
          continue;
        }
        boolean target2 = (i2 >= vi_index_min) && (i2 < vi_index_limit);
        int i3_min, i3_limit;
        if (target1 || target2) {
          i3_min = i2+1;
          i3_limit = var_infos.length;
        } else {
          i3_min = Math.max(i2+1, vi_index_min);
          i3_limit = vi_index_limit;
        }
        for (int i3=i3_min; i3<i3_limit; i3++) {
          VarInfo vi3 = var_infos[i3];
          if (vi3.isDerived()) {
            if (Global.debugDerive.isLoggable(Level.FINE)) {
              Global.debugDerive.fine ("Ternary 3rd: not worth deriving from ("
                                       + vi1.name.name() + ","
                                       + vi2.name.name() + ")"
                                       + vi3.name.name() + ")");
              /* [INCR]
              Global.debugDerive.fine ("Canonicality is: "+ vi3.isCanonical());
              Global.debugDerive.fine ("Equal_to: "+ vi3.equal_to.name.name());
              */ // [INCR]
            }
            continue;
          }
          for (int di=0; di<ternary.length; di++) {
            TernaryDerivationFactory d = ternary[di];
            TernaryDerivation[] tderivs = d.instantiate(vi1, vi2, vi3);
            if (tderivs != null) {
              for (int tdi=0; tdi<tderivs.length; tdi++) {
                TernaryDerivation tderiv = tderivs[tdi];
                if ((Daikon.var_omit_regexp != null)
                    && Global.regexp_matcher.contains
                        (tderiv.getVarInfo().name.name(),
                         Daikon.var_omit_regexp)) {
                  continue;
                }
                result.add(tderiv);
              }
            } else {
              if (Global.debugDerive.isLoggable(Level.FINE)) {
                Global.debugDerive.fine ("Ternary instantiated but not used: "
                                         + vi1.name.name() + " "
                                         + vi2.name.name() + " "
                                         + vi3.name.name() + " "
                                         );
              }
            }
          }
        }
      }
    }


    if (Global.debugDerive.isLoggable(Level.FINE)) {
      Global.debugDerive.fine ("Number of derived variables at program point " + this.name + ": " + result.size());
      String derived_vars = "Derived:";
      for (Iterator itor = result.iterator(); itor.hasNext(); ) {
        derived_vars += " " + ((Derivation)itor.next()).getVarInfo().name.name();
      }
      Global.debugDerive.fine (derived_vars);
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
    //     if (debugFlow.isLoggable(Level.FINE)) {
    //       debugFlow.fine ("add_and_flow for " + name());
    //     }

    // Doable, but commented out for efficiency
    // repCheck();

    // System.out.println("PptTopLevel " + name() + ": add " + vt);
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

    if (debugFlow.isLoggable(Level.FINE)) {
      debugFlow.fine ("<<<< Doing add_and_flow() for " + name());
    }
    if (debugSuppress.isLoggable(Level.FINE)) {
      debugSuppress.fine ("<<<< Doing add_and_flow() for " + name());
    }

    for (int i=0; i < dataflow_ppts.length; i++) {
      PptTopLevel ppt = dataflow_ppts[i];
      //       if (debugFlow.isLoggable(Level.FINE)) {
      //        debugFlow.fine ("add_and_flow: A parent is " + ppt.name());
      //       }

      int[] transform = dataflow_transforms[i];
      Assert.assertTrue(transform.length == var_infos.length);

      // Map vt into the transformed tuple
      int ppt_num_vals = ppt.var_infos.length - ppt.num_static_constant_vars;
      Assert.assertTrue(ppt_num_vals == ppt.mbtracker.num_vars());
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

    // System.out.println("PptTopLevel " + name() + ": add " + vt);
    Assert.assertTrue(vt.size() == var_infos.length - num_static_constant_vars, name);

    //     if (debugFlow.isLoggable(Level.FINE)) {
    //       debugFlow.fine ("Add for " + this.name);
    //     }

    if (debugSuppress.isLoggable(Level.FINE)) {
      debugSuppress.fine ("<<< Doing add for " + name());
      // debugSuppress.fine ("    with vt " + vt);
    }

    if (debugFlow.isLoggable(Level.FINE)) {
      debugFlow.fine ("<<< Doing add for " + name());
      debugFlow.fine ("    with vt " + vt.toString(this.var_infos));
    }

    if (values_num_samples == 0) {
      debugFlow.fine ("  Instantiating views for the first time");
      instantiate_views_and_invariants();

      if (Global.debugInfer.isLoggable(Level.FINE)) {
        Global.debugInfer.fine ("Instantiated views first time for " + this);
      }
    }

    if (!initiatedSuppression &&
        values_num_samples >= Daikon.suppress_samples_min) {
      initiateSuppression();
    }

    if (Daikon.use_equality_optimization) {
      equality_view.add (vt, count);
    }
    instantiated_inv_cnt = invariant_cnt();
    instantiated_slice_cnt = views.size();

    values_num_samples += count;

    // For reasons I do not understand, in the "suppress" version of the
    // tests, calling mbtracker.add here causes (bad) diffs, but calling it
    // at the end of this routine is fine.  So for now, just call it at the
    // end of the routine.
    // // System.out.println("About to call ModBitTracker.add for " + name() + " <= " + vt.toString());
    // // mbtracker.add(vt, count);

    // System.out.println("About to call ValueSet.add for " + name() + " <= " + vt.toString());
    for (int i=0; i<vt.vals.length; i++) {
      if (! vt.isMissing(i)) {
        ValueSet vs = value_sets[i];
        vs.add(vt.vals[i]);
        // System.out.println("ValueSet(" + i + ") now has " + vs.size() + " elements");
      } else {
        ValueSet vs = value_sets[i];
        // System.out.println("ValueSet(" + i + ") not added to, still has " + vs.size() + " elements");
      }
    }

    Set viewsToCheck = new LinkedHashSet(viewsAsCollection());

    int checkCount = 0;
    Invariants invsFlowed = new Invariants();

    while (viewsToCheck.size() > 0) {
      checkCount++;

      if (debugSuppress.isLoggable(Level.FINE)) {
        debugSuppress.fine ("  Checkcount: " + checkCount);
      }

      Set weakenedInvs = new LinkedHashSet();
      // Add to all the views
      for (Iterator itor = viewsToCheck.iterator() ; itor.hasNext() ; ) {
        PptSlice view = (PptSlice) itor.next();
        if (view.invs.size() == 0) {
          continue;
        }
        weakenedInvs.addAll (view.add(vt, count));
      }

      viewsToCheck = new LinkedHashSet();
      // Checking of recently unsuppressed invariants done here.  For
      // every invariant that got unsuppressed: 1) Check if other
      // invariants still suppress it; 2) Check if the current vt
      // falsifies it (by placing it in viewsToCheck, which is checked
      // on the subsequent loop).  Remember that this.invariants can
      // suppress invariants in lower ppts, so we only do (2) for
      // unsuppressed invariants in this.invariants.

      for (Iterator itor = weakenedInvs.iterator(); itor.hasNext(); ) {
        Invariant inv = (Invariant) itor.next();

        if ((debugSuppress.isLoggable(Level.FINE) || inv.logOn()) && inv.numSuppressees() > 0) {
          debugSuppress.fine (" Inv " + inv.repr() +
                               " was falsified or weakened with suppressees");
          inv.log (" Inv " + inv.repr() +
                   " was falsified or weakened with suppressees");
        }
        // Try to resuppress the weakened inv
        // Why is this useful?  There may be isSameFormula comparisons
        // among suppressors such that the weakened form of the inv
        // qualifies (e.g. LowerBound)
        if (!inv.is_false() && inv.getSuppressor() == null) {
          if (attemptSuppression (inv, true)) {
            if (debugSuppress.isLoggable(Level.FINE)) {
              debugSuppress.fine ("Suppressor re-suppressed");
            }
            if (inv.logOn()) {
              inv.log ("Suppressor " + inv.format()
                       + " re-suppressed, sample count: "
                       + inv.ppt.num_samples());
            }
          }
        }

        if (inv.numSuppressees() > 0) {

          // Why copy?  Because we want to keep unlink() as an atomic
          // operation that removes the SuppressionLink from the
          // suppressor's suppressed field.  Without copying, we get a
          // ConcurrentModifiecationException.
          Set suppressees = new LinkedHashSet(inv.getSuppressees());

          for (Iterator iSuppressees = suppressees.iterator();
               iSuppressees.hasNext(); ) {
            SuppressionLink sl = (SuppressionLink) iSuppressees.next();
            Invariant invSuppressed = sl.getSuppressee();
            sl.unlink();
            Assert.assertTrue (invSuppressed.getSuppressor() == null);
            if (debugSuppress.isLoggable(Level.FINE) || invSuppressed.logOn()) {
              debugSuppress.fine ("  Attempting re-suppression of: " + invSuppressed.repr());
              invSuppressed.log ("  Attempting re-suppression of: " + invSuppressed.repr());
            }
            PptTopLevel suppressedPpt = invSuppressed.ppt.parent;
            if (attemptSuppression (invSuppressed, true)) {
              if (debugSuppress.isLoggable(Level.FINE) || invSuppressed.logOn()) {
                debugSuppress.fine ("  Re-suppressed by " + invSuppressed.getSuppressor());
                invSuppressed.log ("  Re-suppressed by " + invSuppressed.getSuppressor() +
                                   " samples: " + inv.ppt.num_samples());
              }
            } else if (suppressedPpt == this) {
              // If invSuppressed didn't get resuppressed, we have to check values
              debugSuppress.fine ("  Will re-check because in same ppt");
              if (invSuppressed.logOn()) {
                invSuppressed.log ("  Will re-check because in same ppt");
              }
              viewsToCheck.add (invSuppressed.ppt);
            } else {
              // Do nothing because suppressedParent is a child of this,
              // and will be checked in good time.
            }
          }
        }
      }
    }

    for (Iterator itor = views_iterator() ; itor.hasNext() ; ) {
      PptSlice view = (PptSlice) itor.next();
      if (view.invs.size() == 0) {
        itor.remove();
        if (Global.debugInfer.isLoggable(Level.FINE)) {
          Global.debugInfer.fine ("add(ValueTulple,int): slice died: " + name() + view.varNames());
        }
      }
    }

    // Add to all the conditional ppts (not implemented in top down)
    if (false) {
      for (Iterator itor = cond_iterator(); itor.hasNext() ; ) {
        PptConditional pptcond = (PptConditional) itor.next();
        pptcond.add(vt, count);
        // TODO: Check for no more invariants on pptcond?
      }
    }

    if (debugSuppress.isLoggable(Level.FINE)) {
      debugSuppress.fine (">>> End of add for " + name());
    }

    // System.out.println("About to call ModBitTracker.add for " + name() + " (current size " + mbtracker.num_samples() + ")" + " <= " + vt.toString());
    mbtracker.add(vt, count);

    return new ArrayList();
  }

  /**
   * Add the sample both to this point and to the global ppt (if
   * any).  Any invariants weakened at the global ppt are added to
   * the list of all weakened invariants.
   * @see #add_bottom_up
   **/
  public void add_global_bottom_up (ValueTuple vt, int count){

    // If there is a global ppt
    if (global != null) {

      // Create an orig version of the sample and apply it to the global ppt
      ValueTuple orig_vt = transform_sample (global, global_transform_orig,
                                             vt);
      weakened_invs.addAll (global.add_bottom_up (orig_vt, count));

      // Create a post version of the sample and apply it to the global ppt
      ValueTuple post_vt = transform_sample (global, global_transform_post,
                                             vt);
      weakened_invs.addAll (global.add_bottom_up (post_vt, count));
    }

    // Add any invariants that have weakened since the last time this ppt
    // was processed to this ppt.
    add_weakened_global_invs();

    // Add the sample to this ppt
    add_bottom_up (vt, count);

    // check_vs_global();

  }

  /**
   * Add the sample to the equality sets and invariants at this
   * program point.  This version is specific to the bottom up
   * processing mechanism.  Any invariants that were suppressed by
   * invariants that were weakened or falsified by this sample also
   * are presented the sample.
   *
   * This routine also instantiates slices/invariants on the first
   * call for the ppt and initiates suppression when enough samples
   * have been seen to warrant it.
   *
   * @param vt the set of values for this to see
   * @param count the number of samples that vt represents
   *
   * @return the set of all invariants weakened or falsified by this sample
   **/
  public Set /* Invariant */ add_bottom_up (ValueTuple vt, int count){
    // Doable, but commented out for efficiency
    // repCheck();

    // System.out.println ("Processing samples at " + name());

    Assert.assertTrue(vt.size() == var_infos.length - num_static_constant_vars,
                      name);

    if (debugSuppress.isLoggable(Level.FINE)) {
      debugSuppress.fine ("<<< Doing add for " + name());
      debugSuppress.fine ("    with vt " + vt);
    }
    if (debugFlow.isLoggable(Level.FINE)) {
      debugFlow.fine ("<<< Doing add for " + name());
      debugFlow.fine ("    with vt " + vt.toString(this.var_infos));
    }

    // If there are conditional program points, add the sample there instead
    if (has_splitters()) {
      for (Iterator ii = splitters.iterator(); ii.hasNext(); ) {
        PptSplitter ppt_split = (PptSplitter) ii.next();
        ppt_split.add_bottom_up (vt, count);
      }
      return (null);
    }

    // Set of invariants weakened by this sample
    Set weakened_invs = new LinkedHashSet();

    // Instantiate slices and invariants if this is the first sample
    if (values_num_samples == 0) {
      debugFlow.fine ("  Instantiating views for the first time");
      if (!Daikon.use_dynamic_constant_optimization)
        instantiate_views_and_invariants();
    }

    // Initiate suppression if we have seen enough samples to warrant it
    if (!initiatedSuppression &&
        values_num_samples >= Daikon.suppress_samples_min) {
      initiateSuppression();
    }

    // Add the samples to all of the equality sets, breaking sets as required
    if (Daikon.use_equality_optimization) {
      weakened_invs.addAll (equality_view.add (vt, count));
      for (Iterator i = weakened_invs.iterator(); i.hasNext(); )
        Assert.assertTrue (i.next() instanceof Invariant);
    }

    // Add samples to constants, adding new invariants as required
    if (Daikon.use_dynamic_constant_optimization) {
      if (constants == null)
        constants = new DynamicConstants (this);
      List non_missing = new ArrayList();
      List noncons = constants.add (vt, count, non_missing);
      constants.instantiate_new_views (noncons, non_missing);
    }

    instantiated_inv_cnt = invariant_cnt();
    instantiated_slice_cnt = views.size();

    if (debugInstantiate.isLoggable (Level.FINE) && values_num_samples == 0) {
      int slice1_cnt = 0;
      int slice2_cnt = 0;
      int slice3_cnt = 0;
      for (Iterator j = views_iterator(); j.hasNext(); ) {
        PptSlice slice = (PptSlice) j.next();
        if (slice instanceof PptSlice1)
          slice1_cnt++;
        else if (slice instanceof PptSlice2)
          slice2_cnt++;
        else if (slice instanceof PptSlice3)
          slice3_cnt++;
      }
      System.out.println ("ppt " + name());
      debugInstantiate.fine ("slice1 ("+ slice1_cnt + ") slices");
      for (Iterator j = views_iterator(); j.hasNext(); ) {
        PptSlice slice = (PptSlice) j.next();
        if (slice instanceof PptSlice1)
          debugInstantiate.fine (" : " + slice.var_infos[0].name.name()
                         + ": " + slice.var_infos[0].file_rep_type
                         + ": " + slice.var_infos[0].rep_type
                         + ": " + slice.var_infos[0].equalitySet.shortString());
        if (false) {
          for (int k = 0; k < slice.invs.size(); k++) {
            Invariant inv = (Invariant) slice.invs.get(k);
            debugInstantiate.fine ("-- invariant " + inv.format());
          }
        }
      }
      debugInstantiate.fine ("slice2 ("+ slice2_cnt + ") slices");
      for (Iterator j = views_iterator(); j.hasNext(); ) {
        PptSlice slice = (PptSlice) j.next();
        if (slice instanceof PptSlice2)
          debugInstantiate.fine (" : " + slice.var_infos[0].name.name()
                                + " : " + slice.var_infos[1].name.name());
      }
      debugInstantiate.fine ("slice3 ("+ slice3_cnt + ") slices");
      for (Iterator j = views_iterator(); j.hasNext(); ) {
        PptSlice slice = (PptSlice) j.next();
        if (slice instanceof PptSlice3)
          debugInstantiate.fine (" : " + slice.var_infos[0].name.name()
                                + " : " + slice.var_infos[1].name.name()
                                + " : " + slice.var_infos[2].name.name());
      }
    }

    values_num_samples += count;

    // System.out.println("About to call ModBitTracker.add for " + name() + " (current size " + mbtracker.num_samples() + ")" + " <= " + vt.toString());
    mbtracker.add(vt, count);

    // System.out.println("About to call ValueSet.add for " + name() + " <= " + vt.toString());
    for (int i=0; i<vt.vals.length; i++) {
      if (! vt.isMissing(i)) {
        ValueSet vs = value_sets[i];
        vs.add(vt.vals[i]);
        // System.out.println("ValueSet(" + i + ") now has " + vs.size() + " elements");
      } else {
        ValueSet vs = value_sets[i];
        // System.out.println("ValueSet(" + i + ") not added to, still has " + vs.size() + " elements");
      }
    }

    // Add the sample to each slice/invariant and keep track of the
    // list of weakened/destroyed invariants.  If the weakened
    // invariants suppressed any other invariants, each of the
    // suppressees must be checked.  First try and resuppress it.  If
    // that fails, add it to a list of unsuppressed invariants.  and
    // add the sample to those invariants.  Continue iteratively until
    // there are no more slices to check.

    // Add the sample to each slice and keep track of any weakened or
    // destroyed invariants
    Set viewsToCheck = new LinkedHashSet(viewsAsCollection());
    for (Iterator itor = viewsToCheck.iterator() ; itor.hasNext() ; ) {
      PptSlice view = (PptSlice) itor.next();
      if (view.invs.size() == 0)
        continue;
      weakened_invs.addAll (view.add(vt, count));
    }
    Set all_weakened_invs = new LinkedHashSet();

    // List of unsuppressed invariants
    List unsuppressed_invs = new ArrayList();

    // while new weakened invariants are left, process them
    while (weakened_invs.size() > 0) {

      // Keep track of all of weakened invariants
      all_weakened_invs.addAll (weakened_invs);

      // foreach weakened/destroyed invariant
      for (Iterator itor = weakened_invs.iterator(); itor.hasNext(); ) {

        // Get current invariant and its list of suppression links
        Invariant inv = (Invariant) itor.next();

        if ((debugSuppress.isLoggable(Level.FINE) || inv.logOn())
            && inv.numSuppressees() > 0) {
          inv.log (debugSuppress, " Inv " + inv.repr() +
                   " was falsified or weakened with suppressees");
        }

        // Try and suppress the weakened invariant (its new weakened
        // state might allow suppression, where its previous state did not)
        if (!inv.is_false() && inv.getSuppressor() == null) {
          if (attemptSuppression (inv, true)) {
            if (inv.logOn() || debugSuppress.isLoggable(Level.FINE))
              inv.log ("Weakened invariant suppressed");
          }
        }

        if (inv.numSuppressees() > 0) {
          Set suppressees = new LinkedHashSet(inv.getSuppressees());

          // Loop through each invariant suppressed by this one and attempt
          // to resuppress it clearing out the existing suppressions at the
          // same time.  If not resuppressed, add it to the list
          // of unsuppressed invariants.
          for (Iterator isup = suppressees.iterator(); isup.hasNext(); ) {
            SuppressionLink sl = (SuppressionLink) isup.next();
            Invariant sup_inv = sl.getSuppressee();
            sl.unlink();
            if (sup_inv.logOn() || debugSuppress.isLoggable(Level.FINE))
              sup_inv.log (debugSuppress, "Attempting resuppression");
            if (attemptSuppression (sup_inv, true)) {
              if (sup_inv.logOn() || debugSuppress.isLoggable(Level.FINE))
                sup_inv.log (debugSuppress, "Re-suppressed by "
                             + sup_inv.getSuppressor());
            } else {
              unsuppressed_invs.add (sup_inv);
            }
          }
        }
      }

      // Add the sample to each unsuppressed invariant and get back the list
      // of any that were weakened by the sample.
      weakened_invs.clear();
      weakened_invs.addAll (inv_add (unsuppressed_invs, vt, count));
    }

    // Remove slices from the list if all of their invariants have died
    for (Iterator itor = views_iterator() ; itor.hasNext() ; ) {
      PptSlice view = (PptSlice) itor.next();
      if (view.invs.size() == 0) {
        itor.remove();
        if (Global.debugInfer.isLoggable(Level.FINE))
          Global.debugInfer.fine ("add(ValueTulple,int): slice died: "
                                  + name() + view.varNames());
      }
    }

    // Add sample to all conditional ppts.  This is probably not fully
    // implemented in V3
    for (Iterator itor = cond_iterator(); itor.hasNext() ; ) {
      PptConditional pptcond = (PptConditional) itor.next();
      pptcond.add(vt, count);
      // TODO: Check for no more invariants on pptcond?
    }

    return (all_weakened_invs);
  }

  /*
    // OLD VERSION
    // Initially, viewsToCheck are all of the slices

    // While there are views to check
    while (viewsToCheck.size() > 0) {

      // Add the sample to each slice and keep track of any weakened or
      // destroyed invariants
      Set weakened_invs = new LinkedHashSet();
      for (Iterator itor = viewsToCheck.iterator() ; itor.hasNext() ; ) {
        PptSlice view = (PptSlice) itor.next();
        if (view.invs.size() == 0)
          continue;
        weakened_invs.addAll (view.add(vt, count));
      }

      // Initialize an empty slice set, the program points that include
      // invariants that are now unsuppressed are added below.
      viewsToCheck = new LinkedHashSet();

      // foreach weakened/destroyed invariant
      for (Iterator itor = weakened_invs.iterator(); itor.hasNext(); ) {

        // Get current invariant and its list of suppression links
        Invariant inv = (Invariant) itor.next();
        Set suppressees = new LinkedHashSet(inv.getSuppressees());
        if ((debugSuppress.isLoggable(Level.FINE) || inv.logOn())
          && suppressees.size() > 0)
          inv.log (debugSuppress, " Inv " + inv.repr() +
                   " was falsified or weakened with suppressees");

        // Try and suppress the weakened invariant (its new weakened
        // state might allow suppression, where its previous state did not)
        if (!inv.falsified && inv.getSuppressor() == null) {
          if (attemptSuppression (inv, true)) {
            if (inv.logOn() || debugSuppress.isLoggable(Level.FINE))
              inv.log ("Weakened invariant suppressed");
          }
        }

        // Loop through each invariant suppressed by this one and attempt
        // to resuppress it clearing out the existing suppressions at the
        // same time.  If not resuppressed, add its ppt to the
        // list of ppts to apply samples to.
        for (Iterator isup = suppressees.iterator(); isup.hasNext(); ) {
          SuppressionLink sl = (SuppressionLink) isup.next();
          Invariant sup_inv = sl.getSuppressee();
          sl.unlink();
          if (sup_inv.logOn() || debugSuppress.isLoggable(Level.FINE))
            sup_inv.log (debugSuppress, "Attempting resuppression");
          if (attemptSuppression (sup_inv, true)) {
            if (sup_inv.logOn() || debugSuppress.isLoggable(Level.FINE))
              sup_inv.log (debugSuppress, "Re-suppressed by "
                            + sup_inv.getSuppressor());
          } else {
            viewsToCheck.add (sup_inv.ppt);
          }
        }
      }
    }

  */

  /**
   * Adds a sample to each invariant in the list.  Returns the list of
   * weakened invariants.  This should only be called when the sample
   * has already been added to the slice containing each invariant.  Otherwise
   * the statistics kept in the slice will be incorrect.
   */
  public List /*Invariant */ inv_add (List /*Invariant*/ inv_list,
                                      ValueTuple vt, int count) {

    // Slices containing these invariants
    Set slices = new LinkedHashSet();

    // List of invariants weakened by this sample
    List weakened_invs = new ArrayList();

    // Loop through each invariant
    inv_loop:
    for (int i = 0; i < inv_list.size(); i++) {
      Invariant inv = (Invariant) inv_list.get(i);
      if (Debug.logDetail())
        inv.log ("Processing in inv_add");

      // Skip falsified invariants (shouldn't happen)
      if (inv.is_false())
        continue;

      // Skip any invariants with a missing variable
      for (int j = 0; j < inv.ppt.var_infos.length; j++) {
        if (inv.ppt.var_infos[j].isMissing(vt))
          continue inv_loop;
      }

      // Add the slice containing this invariant to the set of slices
      slices.add (inv.ppt);

      // Result of add
      InvariantStatus result = null;

      // Get the values and add them to the invariant.
      if (inv.ppt instanceof PptSlice1) {
        VarInfo v = inv.ppt.var_infos[0];
        UnaryInvariant unary_inv = (UnaryInvariant) inv;
        result = unary_inv.add (vt.getValue(v), vt.getModified(v), count);
      } else if (inv.ppt instanceof PptSlice2) {
        VarInfo v1 = inv.ppt.var_infos[0];
        VarInfo v2 = inv.ppt.var_infos[1];
        BinaryInvariant bin_inv = (BinaryInvariant) inv;
        if (v2.rep_type.isArray() && !v1.rep_type.isArray())
          result = bin_inv.add (vt.getValue (v2), vt.getValue(v1),
                                vt.getModified(v1), count);
        else
          result = bin_inv.add (vt.getValue (v1), vt.getValue(v2),
                                vt.getModified(v1), count);
        if (Debug.logDetail())
          bin_inv.log ("added sample " + Debug.toString(vt.getValue(v1)) + ", "
                       + Debug.toString(vt.getValue(v2)));
      } else /* must be ternary */ {
        VarInfo v1 = inv.ppt.var_infos[0];
        VarInfo v2 = inv.ppt.var_infos[1];
        VarInfo v3 = inv.ppt.var_infos[2];
        TernaryInvariant ternary_inv = (TernaryInvariant) inv;
        result = ternary_inv.add (vt.getValue(v1), vt.getValue(v2),
                                  vt.getValue(v3), vt.getModified(v1), count);
      }
      if (result == InvariantStatus.FALSIFIED) {
        inv.falsify();
        weakened_invs.add (inv);
      } else if (result == InvariantStatus.WEAKENED) {
        weakened_invs.add (inv);
      }
    }

    // Remove any falsified invariants
    for (Iterator i = slices.iterator(); i.hasNext(); ) {
      PptSlice slice = (PptSlice) i.next();
      slice.remove_falsified();
    }

    return (weakened_invs);
  }

  /**
   * Adds all global invariants that have been weakened since the last
   * time it was called to this ppt.  Resets the weakened index so
   * that we won't process the same invariants more than once
   */

  private void add_weakened_global_invs() {

    // Loop through each weakened invariant since the last time we were called
    for (int i = weakened_offset; i < weakened_invs.size(); i++) {
      Invariant global_inv = (Invariant) weakened_invs.get (i);

      // add via the orig transform
      add_weakened_global_inv (global_inv, global_transform_orig);

      // add via the post transform
      add_weakened_global_inv (global_inv, global_transform_post);
    }

    weakened_offset = weakened_invs.size();

    // Put this ppt at the end of the list of offsets
    weakened_offsets.remove (this);
    weakened_offsets.add (this);

    // If all of the ppts have processed the invariants at the beginning
    // of the weakened list, remove those invariants
    Iterator it = weakened_offsets.iterator();
    PptTopLevel first = (PptTopLevel) it.next();
    if (first.weakened_offset > weakened_start_index) {
      debugGlobal.fine ("Removing flowed invs " + weakened_start_index + " to "
                        + first.weakened_offset);
      debugGlobal.fine ("First ppt = " + first.name());
      int cnt = first.weakened_offset - weakened_start_index;
      for (int i = weakened_start_index; i < first.weakened_offset; i++)
        weakened_invs.set (i, null);
      weakened_start_index = first.weakened_offset;
    }
  }

  /**
   * Adds the specified global invariant to this ppt using the specified
   * transform for its variables.  If the slice for the invariant does not
   * currently exist, it is added.
   */
  private void add_weakened_global_inv (Invariant global_inv, int[] transform){

    // don't flow an invariant more than once (weakening)
    if (global_inv.flowed)
      return;
    global_inv.flowed = true;

    // Note that it is not necessary to check flowable here.  The invariant
    // will already exist at the lower ppt if it is unflowable.  The
    // only exception to that is LinearBinary and LinearTernary will
    // remove themselves from the lower point if their equations
    // match the global ppt when their equation becomes defined.  In that
    // case we SHOULD flow the invariant since it is still true at
    // the lower point.  In all other cases, there will be a local
    // version of the the invariant which will correctly stop the
    // flow.

    // Transform the invariants global variables to local ones.  If any
    // are not canonical, don't copy the invariant.  This occurs if the
    // equality sets at the global ppt are different from the local one.
    // If the local equality set splits, we'll get those invariants when
    // copying.
    PptSlice global_slice = global_inv.ppt;
    VarInfo vis[] = new VarInfo[global_slice.var_infos.length];
    for (int j = 0; j < vis.length; j++ ) {
      VarInfo v = global_slice.var_infos[j];
      vis[j] = var_infos[transform[v.varinfo_index]];
      if (!vis[j].isCanonical())
        return;
    }

    // We only need to flow invariants if this is a slice with all globals.
    // If there are any locals involved, we've already (or will) created
    // all of the invariants.  Locals can be involved because of equality
    // sets (the global is in an equality set with a local)
    if (!is_slice_global (vis))
      return;

    // Order the variables for this ppt
    VarInfo[] vis_sorted = (VarInfo[]) vis.clone();
    Arrays.sort (vis_sorted, VarInfo.IndexComparator.getInstance());

    // Look up the local slice.  If the slice doesn't already exist,
    // don't create it.  It must be over dynamic constants.  When the slice
    // is created, dynamic constants will create exactly those invariants
    // that  don't exist at the  global level (ie, exactly those that
    // have flowed
    PptSlice local_slice = findSlice (vis_sorted);
    if (local_slice == null)
      return;

    // build the global to local permute and use it to copy the invariant
    int[] permute = build_permute (vis, vis_sorted);
    Invariant local_inv = global_inv.clone_and_permute (permute);
    local_inv.clear_falsified();

    // Add the invariant to the local slice unless it is already there
    if (!local_slice.contains_inv (local_inv)) {
      local_inv.ppt = local_slice;
      local_slice.addInvariant (local_inv);
      local_inv.log ("Added inv '" + local_inv + "' from global inv"
                     + global_inv + " gfalse = " + global_inv.is_false());
    } else {
      Assert.assertTrue (local_slice.invs.size() > 0);
    }
  }

  /** returns the number of suppressed invariants at this ppt **/
  public int suppressed_invariant_cnt() {

    int suppress_cnt = 0;

    for (Iterator j = views_iterator(); j.hasNext(); ) {
      PptSlice slice = (PptSlice) j.next();
      for (int k = 0; k < slice.invs.size(); k++) {
        Invariant inv = (Invariant) slice.invs.get (k);
        if (inv.getSuppressor() != null)
          suppress_cnt++;
      }
    }
    return (suppress_cnt);
  }

  /** returns the number of true invariants at this ppt **/
  public int invariant_cnt() {

    int inv_cnt = 0;

    for (Iterator j = views_iterator(); j.hasNext(); ) {
      PptSlice slice = (PptSlice) j.next();
      inv_cnt += slice.invs.size();
    }
    return (inv_cnt);
  }

  /** returns the number of slices that contain one or more constants **/
  public int const_slice_cnt() {

    int const_cnt = 0;

    for (Iterator j = views_iterator(); j.hasNext(); ) {
      PptSlice slice = (PptSlice) j.next();
      for (int i = 0; i < slice.arity(); i++) {
        if ((constants != null) && constants.is_constant (slice.var_infos[i])){
          const_cnt++;
          break;
        }
      }
    }
    return (const_cnt);
  }

  /** returns the number of invariants that contain one or more constants **/
  public int const_inv_cnt() {

    int const_cnt = 0;

    for (Iterator j = views_iterator(); j.hasNext(); ) {
      PptSlice slice = (PptSlice) j.next();
      for (int i = 0; i < slice.arity(); i++) {
        if ((constants != null) && constants.is_constant (slice.var_infos[i])){
          const_cnt += slice.invs.size();
          break;
        }
      }
    }
    return (const_cnt);
  }

  static class Cnt {
    public int cnt = 0;
  }

  /**
   * Debug print to the specified logger information about each variable
   * in this ppt.  Currently only prints integer and float information
   * using the bound invariants
   */
  public void debug_unary_info (Logger debug) {

    for (Iterator j = views_iterator(); j.hasNext(); ) {
      PptSlice slice = (PptSlice) j.next();
      if (!(slice instanceof PptSlice1))
        continue;
      LowerBound lb = null;
      LowerBoundFloat lbf = null;
      UpperBound ub = null;
      UpperBoundFloat ubf = null;
      for (int k = 0; k < slice.invs.size(); k++) {
        Invariant inv = (Invariant) slice.invs.get (k);
        if (inv instanceof LowerBound)
          lb = (LowerBound) inv;
        else if (inv instanceof LowerBoundFloat)
          lbf = (LowerBoundFloat) inv;
        else if (inv instanceof UpperBound)
          ub = (UpperBound) inv;
        else if (inv instanceof UpperBoundFloat)
          ubf = (UpperBoundFloat) inv;
      }
      if (lb != null)
        debug.fine (lb.min() + " <= " + slice.var_infos[0].name.name()
                    + " <= " + ub.max());
      else if (lbf != null)
        debug.fine (lbf.min() + " <= " + slice.var_infos[0].name.name()
                    + " <= " + ubf.max());
    }
  }


  /**
   * Returns how many invariants there are of each invariant class.  The
   * map is from the invariant class to an integer cnt of the number of
   * that class
   */
  public Map invariant_cnt_by_class() {

    Map inv_map = new LinkedHashMap();

    for (Iterator j = views_iterator(); j.hasNext(); ) {
      PptSlice slice = (PptSlice) j.next();
      for (int k = 0; k < slice.invs.size(); k++) {
        Invariant inv = (Invariant) slice.invs.get (k);
        Cnt cnt = (Cnt) inv_map.get (inv.getClass());
        if (cnt == null) {
          cnt = new Cnt();
          inv_map.put (inv.getClass(), cnt);
        }
        cnt.cnt++;
      }
    }

    return (inv_map);
  }

  /** returns the number of slices at this ppt **/
  public int slice_cnt() {
    return (views.size());
  }

  /**
   * Create all the derived variables.
   **/
  public void create_derived_variables() {
    if (debug.isLoggable(Level.FINE))
      debug.fine ("create_derived_variables for " + name());

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
      if (Global.debugDerive.isLoggable(Level.FINE)) {
        for (int i=0; i < ders.length; i++) {
          Global.debugDerive.fine ("Derived " + vis[i].name.name());
        }
      }

      // Using addDerivedVariables(derivations) would add data too
      addVarInfos(vis);
    }
    Assert.assertTrue(lower == upper);
    Assert.assertTrue(upper == var_infos.length);

    if (debug.isLoggable(Level.FINE))
      debug.fine ("Done with create_derived_variables, " + var_infos.length + " vars");
  }

  /**
   * This function is called to jump-start processing; it creates all
   * the views (and thus candidate invariants), but does not check
   * those invariants.
   **/
  public void instantiate_views_and_invariants() {
    if (debug.isLoggable(Level.FINE))
      debug.fine ("instantiate_views_and_invariants for " + name());

    // Now make all of the views (and thus candidate invariants)
    instantiate_views(0, var_infos.length);

    if (debug.isLoggable(Level.FINE))
      debug.fine ("Done with instantiate_views_and_invariants");
  }


  ///////////////////////////////////////////////////////////////////////////
  /// Creating invariants
  ///

  // I can't decide which loop it's more efficient to make the inner loop:
  // the loop over samples or the loop over slices.

  // slices_vector is a Vector of PptSlice; this routine does not modify it.
  // Maybe this should return the rejected views.
  public void addViews(Vector slices_vector) {
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
        if (Global.debugInfer.isLoggable(Level.FINE)) {
          Global.debugInfer.fine ("addViews: not adding " + slice + " due to no invariants");
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

  // Given an array of VarInfos, return a List representing that array,
  // to be used as an index in the views hashtable.
  private List sliceIndex(VarInfo[] vis) {
    Integer[] a = new Integer[vis.length];
    for (int i = 0; i < vis.length; i++) {
      a[i] = new Integer(vis[i].varinfo_index);
    }
    return Arrays.asList(a);
  }

  /**
   * Add a single slice to the views variable
   **/
  public void addSlice(PptSlice slice) {

    // System.out.println ("Adding slice " + slice);
    // Throwable stack = new Throwable("debug traceback");
    // stack.fillInStackTrace();
    // stack.printStackTrace();

    // Make sure the slice doesn't already exist (should never happen)
    // Note that this can happen in top down due to flowing.  This is
    // probabably not correct, but not worth fixing
    PptSlice cslice = findSlice (slice.var_infos);
    if (Daikon.dkconfig_df_bottom_up && cslice != null) {
      System.out.println ("Trying to add slice " + slice);
      System.out.println ("but, slice " + cslice + " already exists");
      for (int i = 0; i < cslice.invs.size(); i++)
        System.out.println (" -- inv " + (Invariant) cslice.invs.get(i));
      Assert.assertTrue (cslice == null);
    }

    views.put(sliceIndex(slice.var_infos),slice);
    if (Debug.logOn())
      slice.log ("Adding slice");
  }

  /**
   * Remove a slice from this PptTopLevel.
   **/
  public void removeSlice (PptSlice slice) {
    Object o = views.remove(sliceIndex(slice.var_infos));
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

    // System.out.println("Adding views for " + name());
    // for (int i=0; i<slices.length; i++) {
    //   System.out.println("  View: " + slices[i].name);
    // }
    // values.dump();

    // System.out.println("Number of samples for " + name() + ": "
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
    // System.out.println("removeView " + slice.name() + " " + slice);
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
    //        if ((slice.arity() == 1) && slice.usesVar(vi))
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
    //        if ((slice.arity() == 2) && slice.usesVar(vi1) && slice.usesVar(vi2))
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
    //        if ((view.arity() == 1) && (v == view.var_infos[0]))
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
    Assert.assertTrue(v1.varinfo_index <= v2.varinfo_index);
    return (PptSlice2)findSlice(new VarInfo [] {v1, v2});
    //      for (Iterator itor = views_iterator() ; itor.hasNext() ; ) {
    //        PptSlice view = (PptSlice) itor.next();
    //        if ((view.arity() == 2)
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
    // Assert.assertTrue(v1.varinfo_index != v2.varinfo_index);
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
    Assert.assertTrue(v1.varinfo_index <= v2.varinfo_index);
    Assert.assertTrue(v2.varinfo_index <= v3.varinfo_index);
    return (PptSlice3)findSlice(new VarInfo [] {v1, v2, v3});
    //      for (Iterator itor = views_iterator() ; itor.hasNext() ; ) {
    //        PptSlice view = (PptSlice) itor.next();
    //        if ((view.arity() == 3)
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
    return (PptSlice)views.get(sliceIndex(vis));
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

  // Note that some slightly inefficient code has been added to aid
  // in debugging.  When creating binary and ternary views and debugging
  // is on, the outer loops will not terminate prematurely on innapropriate
  // (ie, non-canonical) variables.  This allows explicit debug statements
  // for each possible combination, simplifying determining why certain
  // slices were not created.

  private void instantiate_views(int vi_index_min,
                                 int vi_index_limit)
  {
    if (Global.debugInfer.isLoggable(Level.FINE))
      Global.debugInfer.fine ("instantiate_views: " + this.name
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
    boolean debug_on = debug.isLoggable(Level.FINE);

    /// 1. all unary views

    // Unary slices/invariants.
    Vector unary_views = new Vector(vi_index_limit-vi_index_min);
    for (int i=vi_index_min; i<vi_index_limit; i++) {
      VarInfo vi = var_infos[i];

      if (Debug.logOn())
        Debug.log (getClass(), this, Debug.vis (vi), " Instantiate Slice, ok="
                   + is_slice_ok (vi));
      //System.out.println (" Instantiate Slice " + name() + " var = "
      //                    + vi.name.name() + "ok=" + is_slice_ok (vi));
      if (!is_slice_ok (vi))
        continue;

      // Eventually, add back in this test as "if constant and no
      // comparability info exists" then continue.
      // if (vi.isStaticConstant()) continue;
      PptSlice1 slice1 = new PptSlice1(this, vi);
      slice1.instantiate_invariants();
      if (Debug.logOn() || debug_on)
        Debug.log (debug, getClass(), slice1, "Created unary slice");
      unary_views.add(slice1);
    }
    addViews(unary_views);
    unary_views = null;

    /// 2. all binary views

    // Binary slices/invariants.
    Vector binary_views = new Vector();
    for (int i1=0; i1<vi_index_limit; i1++) {
      VarInfo var1 = var_infos[i1];
      if (!var1.isCanonical() && !(Debug.logOn() || debug_on)) {
        continue;
      }

      // Eventually, add back in this test as "if constant and no
      // comparability info exists" then continue.
      // if (var1.isStaticConstant()) continue;
      boolean target1 = (i1 >= vi_index_min) && (i1 < vi_index_limit);
      int i2_min = (target1 ? i1 : Math.max(i1, vi_index_min));
      for (int i2=i2_min; i2<vi_index_limit; i2++) {
        VarInfo var2 = var_infos[i2];

        if (!var1.isCanonical()) {
          if (Debug.logOn() || debug_on)
            Debug.log (debug, getClass(), this, Debug.vis (var1, var2),
                       "Binary slice not created, var1 is not a leader");
          continue;
        }
        if (!var2.isCanonical()) {
          if (Debug.logOn() || debug_on)
            Debug.log (debug, getClass(), this, Debug.vis (var1, var2),
                       "Binary slice not created, var2 is not a leader");
          continue;
        }

        // This is commented out because if one var is an array and the
        // other is not, this will indicate that the two vars are not
        // compatible.  This causes us to miss seemingly valid elementwise
        // invariants
        // if (!var1.compatible(var2)) {
        //  if (Debug.logOn() || debug_on)
        //    Debug.log (debug, getClass(), this, new VarInfo[] {var1, var2},
        //               "Binary slice not created, vars not compatible");
        //  continue;
        //}

        // Eventually, add back in this test as "if constant and no
        // comparability info exists" then continue.
        // if (var2.isStaticConstant()) continue;
        if (!is_slice_ok (var1, var2)) {
          if (Debug.logOn() || debug_on)
            Debug.log (debug, getClass(), this, Debug.vis (var1, var2),
                       "Binary slice not created, is_slice_ok == false");
          continue;
        }
        PptSlice2 slice2 = new PptSlice2(this, var1, var2);
        if (Debug.logOn() || debug_on)
          Debug.log (debug, getClass(), slice2, "Creating binary slice");

        slice2.instantiate_invariants();
        binary_views.add(slice2);
      }
    }
    addViews(binary_views);
    binary_views = null;

    // 3. all ternary views
    if (! Daikon.disable_ternary_invariants) {
      if (Global.debugInfer.isLoggable(Level.FINE)) {
        Global.debugInfer.fine ("Trying ternary slices for " + this.name());
      }

      Vector ternary_views = new Vector();
      for (int i1=0; i1<vi_index_limit; i1++) {
        VarInfo var1 = var_infos[i1];
        if (!var1.isCanonical() && !(Debug.logOn() || debug_on))
          continue;

        // Eventually, add back in this test as "if constant and no
        // comparability info exists" then continue.
        // if (var1.isStaticConstant()) continue;
        // For now, only ternary invariants not involving any arrays
        if (var1.rep_type.isArray() && (!Debug.logOn() || debug_on))
          continue;

        boolean target1 = (i1 >= vi_index_min) && (i1 < vi_index_limit);
        for (int i2=i1; i2<vi_index_limit; i2++) {
          VarInfo var2 = var_infos[i2];
          if (!var2.isCanonical() && !(Debug.logOn() || debug_on))
            continue;

          // Eventually, add back in this test as "if constant and no
          // comparability info exists" then continue.
          // if (var2.isStaticConstant()) continue;
          // For now, only ternary invariants not involving any arrays
          if (var2.rep_type.isArray() && !(Debug.logOn() || debug_on))
            continue;

          boolean target2 = (i2 >= vi_index_min) && (i2 < vi_index_limit);
          int i3_min = ((target1 || target2) ? i2 : Math.max(i2, vi_index_min));
          for (int i3=i3_min; i3<vi_index_limit; i3++) {
            Assert.assertTrue(((i1 >= vi_index_min) && (i1 < vi_index_limit))
                          || ((i2 >= vi_index_min) && (i2 < vi_index_limit))
                          || ((i3 >= vi_index_min) && (i3 < vi_index_limit)));
            Assert.assertTrue((i1 <= i2) && (i2 <= i3));
            VarInfo var3 = var_infos[i3];

            if (!is_slice_ok (var1, var2, var3))
              continue;

            PptSlice3 slice3 = new PptSlice3(this, var1, var2, var3);
            slice3.instantiate_invariants();
            if (Debug.logOn() || debug_on)
              Debug.log (debug, getClass(), slice3, "Created Ternary Slice");
            ternary_views.add(slice3);
          }
        }
      }
      addViews(ternary_views);
    }


    if (debug.isLoggable(Level.FINE))
      debug.fine (views.size() - old_num_views + " new views for " + name());

    // This method didn't add any new variables.
    Assert.assertTrue(old_num_vars == var_infos.length);
    repCheck();
  }

  /**
   * Returns whether or not the specified slice should be created
   */
  public boolean is_slice_ok (VarInfo[] vis, int arity) {
    if (arity == 1)
      return (is_slice_ok (vis[0]));
    else if (arity == 2)
      return (is_slice_ok (vis[0], vis[1]));
    else
      return (is_slice_ok (vis[0], vis[1], vis[2]));
  }

  /**
   * Returns whether or not the specified unary slice should be
   * created.  The variable must be a leader, not a constant, and
   * not always missing.
   */
  public boolean is_slice_ok (VarInfo var1) {

    if (Daikon.use_dynamic_constant_optimization && constants == null)
      return (false);
    if ((constants != null) && (constants.is_constant (var1)))
      return (false);
    if ((constants != null) && constants.is_missing (var1))
      return (false);
    if (!var1.isCanonical())
      return (false);

    return (true);
  }

  /**
   * Returns whether or not the specified binary slice should be created.
   * Checks to sinsure that var1 and var2 are not both constants and
   * if they are in the same equality set, that there are at least 2
   * variables in the equality set.  Also makes sure that neither var1
   * or var2 is always missing.
   */
  public boolean is_slice_ok (VarInfo var1, VarInfo var2) {

    // Both vars must be leaders
    if (!var1.isCanonical() || !var2.isCanonical())
      return (false);

    // Check to see if the new slice would be over all constants
    if ((constants != null) && constants.is_constant (var1)
      && constants.is_constant (var2))
      return (false);

    // Each variable must not be always missing
    if (constants != null) {
      if (constants.is_missing (var1) || constants.is_missing (var2))
        return (false);
    }

    // Don't create a slice with the same variables if the equality
    // set only contains 1 variable
    // This is not turned on for now since suppressions need invariants
    // of the form a == a even when a is the only item in the set.
    if (false) {
      if ((var1 == var2) && (var1.equalitySet.size() == 1))
        return (false);
    }

    return (true);
  }

  /**
   * Returns whether or not the specified ternary slice should be created.
   * The slice should not be created if any of the following are true
   *    - Any var is always missing
   *    - Any var is not canonical
   *    - Any var is an array
   *    - Any of the vars are not comparable with the others
   *    - All of the vars are constants
   *    - Each var is the same and its equality set has only two variables
   *    - Two of the vars are the same and its equality has only one variable
   *      (this last one is currently disabled as x = func(x,y) might still
   *      be interesting even if x is the same.
   */
  public boolean is_slice_ok (VarInfo v1, VarInfo v2, VarInfo v3) {

    Debug dlog = null;
    if (Debug.logOn() || debug.isLoggable(Level.FINE))
      dlog = new Debug (getClass(), this, Debug.vis (v1, v2, v3));

    // Each variable must not be always missing
    if (constants != null) {
      if (constants.is_missing (v1) || constants.is_missing (v2)
          || constants.is_missing (v3))
        return (false);
    }

    // At least on variable must not be a constant
    if (constants != null) {
      if (constants.is_constant (v1) && constants.is_constant(v2)
          && constants.is_constant (v3))
        return false;
    }

    // Each variable must be canonical (leader)
    if (!v1.isCanonical()) {
      if (dlog != null)
        dlog.log (debug, "Ternary slice not created, var1 not lead");
      return (false);
    }
    if (!v2.isCanonical()) {
      if (dlog != null)
        dlog.log (debug, "Ternary slice not created, var2 not lead");
      return (false);
    }
    if (!v3.isCanonical()) {
      if (dlog != null)
        dlog.log (debug, "Ternary slice not created, var3 not lead");
      return (false);
    }

    // For now, each variable must also not be an array (ternary only)
    if (v1.rep_type.isArray()) {
      if (dlog != null)
        dlog.log (debug, "Ternary slice not created, var1 is an array");
      return (false);
    }
    if (v2.rep_type.isArray()) {
      if (dlog != null)
        dlog.log (debug, "Ternary slice not created, var2 is an array");
      return (false);
    }
    if (v3.rep_type.isArray()) {
      if (dlog != null)
        dlog.log (debug, "Ternary slice not created, var3 is an array");
      return (false);
    }

    // Vars must be compatible
    if (!v1.compatible(v2) || !v1.compatible(v3)) {
      if (dlog != null)
        dlog.log (debug, "Ternary slice not created, vars not compatible");
      return (false);
    }

    // Don't create a reflexive slice (all vars the same) if there are
    // only two vars in the equality set
    if ((v1 == v2) && (v2 == v3) && (v1.equalitySet.size() <= 2))
      return (false);

    // Don't create a partially reflexive slice (two vars the same) if there
    // is only one variable in its equality set
    if (false) {
      if ((v1 == v2) || (v1 == v3) && (v1.equalitySet.size() == 1))
        return (false);
      if ((v2 == v3) && (v2.equalitySet.size() == 1))
        return (false);
    }

    return (true);
  }

  /**
   * Returns whether or not the specified slice is made up of only
   * variables linked to those in the global ppt (ie, whether they are
   * globals)
   */
  public boolean is_slice_global (VarInfo[] vis) {
    if (vis.length == 1)
      return (is_slice_global (vis[0]));
    else if (vis.length == 2)
      return (is_slice_global (vis[0], vis[1]));
    else
      return (is_slice_global (vis[0], vis[1], vis[2]));
  }

  /**
   * Returns whether or not this slice is made up of only variables linked
   * to those in the global ppt (ie, whether they are globals)
   */
  public boolean is_slice_global (VarInfo vi) {

    return (vi.is_global());
  }

  /**
   * Returns whether or not this slice is made up of only variables linked
   * to those in the global ppt (ie, whether they are globals).  They must
   * also follow the same transform (global or orig)
   */
  public boolean is_slice_global (VarInfo vi1, VarInfo vi2) {

    if (vi1.is_orig_global() && vi2.is_orig_global())
      return (true);

    if (vi1.is_post_global() && vi2.is_post_global())
      return (true);

    return (false);
  }
  /**
   * Returns whether or not this slice is made up of only variables linked
   * to those in the global ppt (ie, whether they are globals).  They must
   * also follow the same transform (global or orig)
   */
  public boolean is_slice_global (VarInfo vi1, VarInfo vi2, VarInfo vi3) {

    if (vi1.is_orig_global() && vi2.is_orig_global() && vi3.is_orig_global())
      return (true);

    if (vi1.is_post_global() && vi2.is_post_global() && vi3.is_post_global())
      return (true);

    return (false);
  }

  /**
   * Return a slice that contains the given VarInfos (creating if
   * needed).  It is incumbent on the caller that the slice be either
   * filled with one or more invariants, or else removed from the
   * views collection.<p>
   *
   * When the arity of the slice is known, call one of the overloaded
   * definitions of get_or_instantiate_slice that takes (one or more)
   * VarInfo arguments; they are more efficient.
   *
   * @param vis array of VarInfo objects; is not used internally
   *      (so the same value can be passed in repeatedly).  Can be unsorted.
   **/
  public PptSlice get_or_instantiate_slice(VarInfo[] vis) {
    switch (vis.length) {
    case 1:
      return get_or_instantiate_slice(vis[0]);
    case 2:
      return get_or_instantiate_slice(vis[0], vis[1]);
    case 3:
      return get_or_instantiate_slice(vis[0], vis[1], vis[2]);
    default:
      throw new IllegalArgumentException("bad length = " + vis.length);
    }
  }


  /**
   * Return a slice that contains the given VarInfos (creating if
   * needed).  It is incumbent on the caller that the slice be either
   * filled with one or more invariants, or else removed from the
   * views collection.
   **/
  public PptSlice get_or_instantiate_slice(VarInfo vi) {
    PptSlice result = findSlice(vi);
    if (result != null) return result;

    // We may do inference over static constants
    // Assert.assertTrue(! vi.isStaticConstant());
    result = new PptSlice1(this, vi);

    addSlice(result);
    return result;
  }

  /**
   * Return a slice that contains the given VarInfos (creating if
   * needed).  It is incumbent on the caller that the slice be either
   * filled with one or more invariants, or else removed from the
   * views collection.
   **/
  public PptSlice get_or_instantiate_slice(VarInfo v1, VarInfo v2) {
    VarInfo tmp;
    if (v1.varinfo_index > v2.varinfo_index) { tmp = v2; v2 = v1; v1 = tmp; }

    PptSlice result = findSlice(v1, v2);
    if (result != null) return result;

    // We may do inference over static constants
    // Assert.assertTrue(! v1.isStaticConstant());
    // Assert.assertTrue(! v2.isStaticConstant());
    result = new PptSlice2(this, v1, v2);

    addSlice(result);
    return result;
  }

  /**
   * Return a slice that contains the given VarInfos (creating if
   * needed).  It is incumbent on the caller that the slice be either
   * filled with one or more invariants, or else removed from the
   * views collection.
   **/
  public PptSlice get_or_instantiate_slice(VarInfo v1, VarInfo v2, VarInfo v3) {
    VarInfo tmp;
    if (v1.varinfo_index > v2.varinfo_index) { tmp = v2; v2 = v1; v1 = tmp; }
    if (v2.varinfo_index > v3.varinfo_index) { tmp = v3; v3 = v2; v2 = tmp; }
    if (v1.varinfo_index > v2.varinfo_index) { tmp = v2; v2 = v1; v1 = tmp; }

    PptSlice result = findSlice(v1, v2, v3);
    if (result != null) return result;

    // We may do inference over static constants
    // Assert.assertTrue(! v1.isStaticConstant());
    // Assert.assertTrue(! v2.isStaticConstant());
    // Assert.assertTrue(! v3.isStaticConstant());
    result = new PptSlice3(this, v1, v2, v3);

    addSlice(result);
    return result;
  }

  /* [INCR] ... We can't know this anymore
  // Set the dynamic_constant slots of all the new variables.
  void set_dynamic_constant_slots(Vector unary_views) {
    for (int i=0; i<unary_views.size(); i++) {
      PptSlice1 unary_view = (PptSlice1) unary_views.elementAt(i);
      // System.out.println("set_dynamic_constant_slots " + unary_view.name + " " + views.contains(unary_view));
      Assert.assertTrue(unary_view.arity() == 1);
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
            // System.out.println("Constant " + inv.ppt.name() + " " + one_of.var().name + " because of " + inv.format() + "    " + inv.repr_prob() + "    " + inv.justified());
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
      Assert.assertTrue(binary_view.arity() == 2);

      if (binary_view.debugged) {
        System.out.println("Binary view " + binary_view.name() + " has "
                           + (views.contains(binary_view) ? "not " : "") + "been eliminated.");
      }
      // If binary_view has been installed (hasn't yet been eliminated)
      if (views.contains(binary_view)) {

        // set_equal_to_slots runs after pass 1 of invariant introduction.
        // There is only one type of binary invariant in pass 1:
        // {Int,Seq,String}Comparison.  It must have been successful, or
        // this view wouldn't have been installed.
        // Assert.assertTrue(binary_view.invs.size() == 1, "binary_view.invs.size()=="+binary_view.invs.size());

        Invariant inv = (Invariant) binary_view.invs.elementAt(0);
        inv.finished = true;
        // binary_view.already_seen_all = true;
        Assert.assertTrue(inv instanceof Comparison, inv.getClass().getName());
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
          if (debugEqualTo.isLoggable(Level.FINE)) {
            debugEqualTo.fine ("Setting " + var1.name + ".equal_to = " + var1.equal_to.name);
          }
        } else if ((var1.equal_to != null) && (var2.equal_to == null)) {
          var2.equal_to = var1.equal_to;
          if (debugEqualTo.isLoggable(Level.FINE)) {
            debugEqualTo.fine ("Setting " + var2.name + ".equal_to = " + var2.equal_to.name);
          }
        } else if ((var1.equal_to == null) && (var2.equal_to == null)) {
          // Can this cause the canonical version to not be the lowest-
          // numbered version?  I don't think so, because of the ordering
          // in which we are examining pairs.
          var1.equal_to = var1;
          var2.equal_to = var1;
          // System.out.println("Make " + var1.name + " canonical over " + var2.name + " at " + name());
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
            if (debugEqualTo.isLoggable(Level.FINE)) {
              // Stars because this message is more important than most.
              debugEqualTo.fine ("*****");
              debugEqualTo.fine ("Internal Daikon error: Variables not equal: " + var1.name.name() +
                                 " (= " + var1.equal_to.name.name() + "), " + var2.name.name() + " (= " +
                                 var2.equal_to.name.name() + ") [indices " + var1.varinfo_index +
                                 ", " + var1.equal_to.varinfo_index + ", " + var2.varinfo_index +
                                 ", " + var2.equal_to.varinfo_index + "] at " + name());
                debugEqualTo.fine ("*****");
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
        if (debugEqualTo.isLoggable(Level.FINE)) {
          debugEqualTo.fine ("Lonesome canonical var " + vi.varinfo_index + ": " + vi.name.name());
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
      // Map of canonicals to lists of non canonicals
      // This needs to be deterministic, since its order can affect
      // which variables are chosen as canonical.
      Map equalMap = new TreeMap(new VarInfo.LexicalComparator());
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
      if (debugEqualTo.isLoggable(Level.FINE)) {
        debugEqualTo.fine ("Doing equality mapping for " + this.name);
        debugEqualTo.fine ("mapping: " + equalMap);
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
   * Starts suppression by attempting to suppress all invariants.
   * Called at the start of inferencing.  Requires that this and its
   * parents have already has instantiated invariants.
   *
   * @see daikon.suppress.SuppressionFactory
   * @pre Invariants already instantiated
   **/
  public void initiateSuppression() {
    if (!initiatedSuppression) {
      suppressAll (true);
      initiatedSuppression = true;
    }
  }

  /**
   * Attempt to suppress all unsuppressed invariants.  Requires that
   * this and its parents have already has instantiated
   * invariants. Can be called repeatedly to refresh suppression, but
   * this is an expensive operation.
   *
   * @param in_process  should be set to true if samples are still being
   *                    processed. false otherwise (ie, at the end).
   *                    This is used to defer suppression on certain invariants
   *                    that lose important internal state information when
   *                    suppressed.
   *
   * @see daikon.suppress.SuppressionFactory
   * @pre Invariants already instantiated
   **/
  public void suppressAll (boolean in_process) {
    if (Daikon.use_suppression_optimization) {
      if (debugSuppressInit.isLoggable(Level.FINE)) {
        debugSuppressInit.fine ("SuppressAll for: " + name());
      }
      List invs = getInvariants();
      for (Iterator i = invs.iterator(); i.hasNext(); ) {
        Invariant inv = (Invariant) i.next();
        if (inv.getSuppressor() == null) {
          attemptSuppression (inv, in_process);
        }
      }
      if (debugSuppressInit.isLoggable(Level.FINE)) {
        debugSuppressInit.fine ("  Suppressed invariants:");
        for (Iterator i = invs.iterator(); i.hasNext(); ) {
          Invariant inv = (Invariant) i.next();
          if (inv.getSuppressor() != null) {
            debugSuppressInit.fine (" y " + inv.repr());
          } else {
            debugSuppressInit.fine (" n " + inv.repr());
          }
        }
        debugSuppressInit.fine ("  end of suppressed invariants:");
      }
    }
  }



  /**
   * Try to suppress one invariant.  Links the invariant to a
   * SuppressionLink if suppression succeeds.
   * @param inv         the Invariant to attempt suppression on, which has to
   *                    be a member of this.
   * @param in_process  should be set to true if samples are still being
   *                    processed. false otherwise (ie, at the end).
   *                    This is used to defer suppression on certain invariants
   *                    that lose important internal state information when
   *                    suppressed.
   * @return true if invariant was suppressed
   * @see daikon.suppress.SuppressionFactory
   * @pre Invariants already instantiated.  inv not already suppressed.
   **/
  public boolean attemptSuppression (Invariant inv, boolean in_process) {
    if (Daikon.use_suppression_optimization) {
      if (inv.getSuppressor() != null) {
        System.err.println ("Error: the invariant " + inv.format() +
                            " already has a suppressor");
        Assert.assertTrue (inv.getSuppressor() == null);
      }
      if (in_process && !inv.inProcessSuppressOk()) {
        inv.log ("No suppression search- inProcessSuppressOk is false");
        return (false);
      }
      SuppressionFactory[] factories = inv.getSuppressionFactories();
      for (int i = 0; i < factories.length; i++) {
        // Assert.assertTrue(factories[i] != null, "factories[" + i + "] == null for " + inv.format());
        SuppressionLink sl = factories[i].generateSuppressionLink (inv);
        if (sl != null) {
          sl.link();
          inv.log ("Generated link with: " + sl);
          return true;
        }
      }
    }
    return false;
  }


  ///////////////////////////////////////////////////////////////////////////
  /// Creating conditioned views
  ///

  // This static region can't appear in PptConditional, lest it never get
  // called.  PptConditional isn't instantiated unless it needs to be, but
  // it doesn't need to be unless we run this static region!

  static {
    if (! Daikon.dkconfig_disable_splitting) {
      // new MiscSplitters();

      SplitterList.put(".*", new Splitter[] {
        new ReturnTrueSplitter(),
      });
    }
  }

  public void addConditions(Splitter[] splits) {

    debugConditional.fine ("Applying splits to " + name());

    if ((splits == null) || (splits.length == 0)) {
        debugConditional.fine ("No splits for " + name());
      return;
    }

    // for (int i=0; i<splits.length; i++) {
    //   Assert.assertTrue(splits[i].instantiated() == false);
    // }

    // Create a Conditional ppt for each side of each splitter
    splitters = new ArrayList (splits.length);
    for (int ii = 0; ii < splits.length; ii++) {
      PptSplitter ppt_split = new PptSplitter (this, splits[ii]);
      if (!ppt_split.splitter_valid()) {
          debugConditional.fine ("Splitter (" + ppt_split.splitter.getClass()
                                  + ") not valid: " + ppt_split.ppts[0].name);
        continue;
      }
      splitters.add (ppt_split);
      if (debugConditional.isLoggable (Level.FINE))
        debugConditional.fine ("Added PptSplitter: " + ppt_split);
    }

  }


  /**
   * Given conditional program points (and invariants detected over them),
   * create implications.  Configuration variable "pairwise_implications"
   * controls whether all or only the first two conditional program points
   * are considered.
   **/
  public void addImplications() {

    if (Daikon.dkconfig_disable_splitting)
      return;

    // Add implications from each splitter
    if (splitters != null) {
      for (int i = 0; i < splitters.size(); i++) {
        PptSplitter ppt_split = (PptSplitter) splitters.get (i);
        ppt_split.add_implications();
      }
    }

    // If this is a combined exit point with two individual exits, create
    // implications from the two exit points
    if (ppt_name.isCombinedExitPoint()) {
      List exit_points = new ArrayList();
      for (Iterator ii = children.iterator(); ii.hasNext(); ) {
        PptRelation rel = (PptRelation) ii.next();
        if (rel.getRelationType() == PptRelation.EXIT_EXITNN)
          exit_points.add (rel.child);
      }
      if (exit_points.size() == 2) {
        PptTopLevel ppt1 = (PptTopLevel) exit_points.get(0);
        PptTopLevel ppt2 = (PptTopLevel) exit_points.get(1);
        PptSplitter ppt_split = new PptSplitter (this, ppt2, ppt1);
        ppt_split.add_implications();
      }
    }
  }


  ///////////////////////////////////////////////////////////////////////////
  /// Post processing after data trace files are read (but before printing)
  ///

  /**
   * Two things: a) convert Equality invariants into normal IntEqual
   * type for filtering, printing, etc. b) Pivot uninteresting
   * parameter VarInfos so that each equality set contains only the
   * interesting one.
   **/
  public void postProcessEquality () {
    if (debugEqualTo.isLoggable(Level.FINE)) {
      debugEqualTo.fine ("PostProcessingEquality for: " + this.name());
    }
    if (num_samples() == 0)
      return;
    Assert.assertTrue (equality_view != null, "ppt = " + ppt_name);
    Invariants equalityInvs = equality_view.invs;

    // Pivot invariants to new equality leaders if needed, if old
    // leaders would prevent printing.
    for (Iterator i = equalityInvs.iterator(); i.hasNext(); ) {
      Equality inv = (Equality) i.next();
      inv.pivot ();
    }

    // Now pivot the other invariants
    Collection slices = viewsAsCollection();
    List pivoted = new LinkedList();

    // PptSlice newSlice = slice.cloneAndInvs(leader, newLeader);

    // Pivot each pptSlice so that each of its VarInfos map back to
    // their leaders.
    if (debugEqualTo.isLoggable(Level.FINE)) {
      debugEqualTo.fine ("  Doing cloneAllPivots: ");
    }
    for (Iterator iSlices = slices.iterator();
         iSlices.hasNext(); ) {
      PptSlice slice = (PptSlice) iSlices.next();
      VarInfo[] newVis = new VarInfo[slice.arity()];
      boolean needPivoting = false;
      for (int i = 0; i < slice.arity(); i++) {
        if (slice.var_infos[i].canonicalRep() != slice.var_infos[i])
          needPivoting = true;
      }
      if (!needPivoting) continue;
      for (int i = 0; i < slice.arity(); i++) {
        newVis[i] = slice.var_infos[i].canonicalRep();
      }
      PptSlice newSlice = slice.cloneAndPivot(newVis);
      if (slice != newSlice) {
        pivoted.add (newSlice);
        iSlices.remove(); // Because the key is now wrong
      }
    }

    // Add in the removed slices
    for (Iterator iPivoted = pivoted.iterator(); iPivoted.hasNext(); ) {
      PptSlice oPivoted = (PptSlice) iPivoted.next();
      addSlice (oPivoted); // Make the key right again
      if (debugEqualTo.isLoggable(Level.FINE)) {
        debugEqualTo.fine ("  Readded: " + oPivoted);
      }
    }

    // Add specific equality invariants for each member of the
    // equality set
    for (Iterator i = equalityInvs.iterator(); i.hasNext(); ) {
      Equality inv = (Equality) i.next();
      inv.postProcess ();
    }

  }


  ///////////////////////////////////////////////////////////////////////////
  /// Locating implied (same) invariants via the simplify theorem-prover
  ///

  // Created upon first use, then saved
  private static LemmaStack proverStack = null;

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
    try {
      if (proverStack == null)
        proverStack = new LemmaStack();
      markImpliedViaSimplify_int
        (all_ppts,
         new SimplifyInclusionTester() {
           public boolean include(Invariant inv) {
             return InvariantFilters.isWorthPrintingFilter().shouldKeep(inv)
                      == null;
           }
         });
    } catch (SimplifyError e) {
      proverStack = null;
    }
  }

  /**
   * Returns true if there was a problem with Simplify formatting (such as
   * the invariant not having a Simplify representation).
   **/
  private static boolean format_simplify_problem(String s) {
    return ((s.indexOf("Simplify") >= 0)
            || (s.indexOf("format(OutputFormat:Simplify)") >= 0)
            || (s.indexOf("format_simplify") >= 0));
  }

  /**
   * Use the Simplify theorem prover to flag invariants that are
   * logically implied by others.  Uses the provided test interface to
   * determine if an invariant is within the domain of inspection.
   **/
  private void markImpliedViaSimplify_int(PptMap all_ppts,
                                          SimplifyInclusionTester test)
    throws SimplifyError
  {
    SessionManager.debugln("Simplify checking " + ppt_name);

    // Create the list of invariants from this ppt which are
    // expressible in Simplify
    Invariant[] invs;
    {
      // Replace parwise equality with an equivalence set
      Vector all_noeq = invariants_vector();
      Collections.sort(all_noeq, icfp);
      List all = InvariantFilters.addEqualityInvariants(all_noeq);
      Collections.sort(all, icfp);
      Vector printing = new Vector(); // [Invariant]
      for (Iterator _invs = all.iterator(); _invs.hasNext(); ) {
        Invariant inv = (Invariant) _invs.next();
        if (test.include(inv)) { // think: inv.isWorthPrinting()
          String fmt = inv.format_using(OutputFormat.SIMPLIFY);
          if (! format_simplify_problem(fmt)) {
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
    if (Global.debugSimplify.isLoggable(Level.FINE)) {
      Global.debugSimplify.fine ("Sorted invs:");
      for (int i=0; i<invs.length; i++) {
        Global.debugSimplify.fine ("    " + invs[i].format());
      }
      for (int i=0; i<invs.length-1; i++) {
        int cmp = icfp.compare(invs[i], invs[i+1]);
        Global.debugSimplify.fine ("cmp(" + i + "," + (i+1) + ") = " + cmp);
        int rev_cmp = icfp.compare(invs[i+1], invs[i]);
        Global.debugSimplify.fine ("cmp(" + (i+1) + "," + i + ") = " + rev_cmp);
        Assert.assertTrue(rev_cmp >= 0);
      }
    }

    // [INCR] The below two paragraphs of code (whose end result is to
    // compute "background") should be changed to use the VarInfo
    // partial ordering to determine background invariants, instead of
    // the (now deprecated) controlling_ppts relationship.

    // Form the closure of the controllers; each element is a Ppt
    Set closure = new LinkedHashSet();
    {
      // Set working = new LinkedHashSet(controlling_ppts); // [INCR]
      Set working = new LinkedHashSet();
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
    for (Iterator ppts = closure.iterator(); ppts.hasNext(); ) {
      PptTopLevel ppt = (PptTopLevel) ppts.next();
      Vector invs_vec = ppt.invariants_vector();
      Collections.sort(invs_vec, icfp);
      Iterator _invs
        = InvariantFilters.addEqualityInvariants(invs_vec).iterator();
      while (_invs.hasNext()) {
        Invariant inv = (Invariant) _invs.next();
        if (inv instanceof Implication) {
          continue;
        }
        if (!test.include(inv)) { // think: !inv.isWorthPrinting()
          continue;
        }
        String fmt = inv.format_using(OutputFormat.SIMPLIFY);
        if (format_simplify_problem(fmt)) {
          // If format_simplify is not defined for this invariant, don't
          // confuse Simplify with the error message
          continue;
        }
        // We could also consider testing if the controlling invariant
        // was removed by Simplify, but what would the point be?  Also,
        // these "intermediate goals" might help out Simplify.
        proverStack.pushLemma(new InvariantLemma(inv));

        // If this is the :::OBJECT ppt, also restate all of them in
        // orig terms, since the conditions also held upon entry.
        if (ppt.ppt_name.isObjectInstanceSynthetic())
          proverStack.pushLemma(InvariantLemma.makeLemmaAddOrig(inv));
      }
    }

    // FIXME XXXXX:  Commented out by MDE, 6/26/2002, due to merging problems.
    // Should this be deleted?  Need to check CVS logs and/or think about this.
    /*
    if (ppt_name.isEnterPoint() && controlling_ppts.size() == 1) {
      // Guess the OBJECT ppt; usually right
      PptTopLevel OBJ = (PptTopLevel) controlling_ppts.iterator().next();
      if (OBJ.ppt_name.isObjectInstanceSynthetic()) {
        // Find variables here of the same type as us
        String clsname = ppt_name.getFullClassName();
      }
    }

    // Use type information to restate any OBJECT invariants over
    // variable expressions such as arguments or fields whose types
    // are instrumeted.
    for (int i=0; i < var_infos.length; i++) {
      VarInfo vi = var_infos[i];
      ProglangType progtype = vi.type;

      // Always skip "this" and "orig(this)" as necessary special cases.
      if (VarInfoName.THIS.equals(vi.name) ||
          VarInfoName.ORIG_THIS.equals(vi.name)) {
        continue;
      }

      // For now, we don't handle sequences.  We could use a GLB type
      // and state a forall, but it doesn't seem worth the work yet.
      if (progtype.isPseudoArray()) {
        continue;
      }

      // Locate the OBJECT ppt
      PptName obj_name = new PptName(vi.type.base(), null,
                                     FileIO.object_suffix);
      PptTopLevel obj_ppt = all_ppts.get(obj_name);
      if (obj_ppt == null) {
        Global.debugSimplify.fine
          (ppt_name + ": no type-based invariants found for "
           + vi.name + " (" + obj_name + ")");
        continue;
      }

      Global.debugSimplify.fine
          (ppt_name + ": using type-based invariants for "
           + vi.name + " (" + obj_name + ")");

      // State the object invariant on the incoming argument
      Vector invs2 = obj_ppt.invariants_vector();
      Collections.sort(invs2, icfp);
      Iterator _invs
        = InvariantFilters.addEqualityInvariants(invs2).iterator();
      while (_invs.hasNext()) {
        Invariant inv = (Invariant) _invs.next();
        if (!test.include(inv)) { // think: !inv.isWorthPrinting()
          continue;
        }
        String fmt = inv.format_using(OutputFormat.SIMPLIFY);
        if (format_simplify_problem(fmt)) {
          continue;
        }
        Lemma replaced = InvariantLemma.makeLemmaReplaceThis(inv, vi.name);
        proverStack.pushLemma(replaced);
      }
    }
    */

    if (proverStack.checkForContradiction() == 'T') {
      if (LemmaStack.dkconfig_remove_contradictions) {
        System.err.println("Warning: " + ppt_name +
                           " background is contradictory, " +
                           "removing some parts");
        proverStack.removeContradiction();
      } else {
        System.err.println("Warning: " + ppt_name +
                           " background is contradictory, giving up");
        return;
      }
    }

    int backgroundMark = proverStack.markLevel();

    InvariantLemma[] lemmas = new InvariantLemma[invs.length];
    for (int i = 0; i < invs.length; i++)
      lemmas[i] = new InvariantLemma(invs[i]);
    boolean[] present = new boolean[lemmas.length];
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
      if (Global.debugSimplify.isLoggable(Level.FINE)) {
        SessionManager.debugln("Background:");
        for (int i=0; i < present.length; i++) {
          if (present[i] && (i != checking)) {
            SessionManager.debugln("    " + invs[i].format());
          }
        }
      }
    }

    for (int i = 0; i < invs.length; i++)
      proverStack.pushLemma(lemmas[i]);

    // If the background is necessarily false, we are in big trouble
    if (proverStack.checkForContradiction() == 'T') {
      // Uncomment to punt on contradictions
      if (!LemmaStack.dkconfig_remove_contradictions) {
        System.err.println("Warning: " + ppt_name +
                           " invariants are contradictory, giving up");
        if (LemmaStack.dkconfig_print_contradictions) {
          LemmaStack.printLemmas(System.err,
                                 proverStack.minimizeContradiction());
        }
      }
      System.err.println("Warning: " + ppt_name +
                         " invariants are contradictory, axing some");
      Map demerits = new TreeMap();
      int worstWheel = 0;
      do {
        // But try to recover anyway
        Vector problems = proverStack.minimizeContradiction();
        if (LemmaStack.dkconfig_print_contradictions) {
          System.err.println("Minimal set:");
          LemmaStack.printLemmas(System.err,
                                 proverStack.minimizeContradiction());
          System.err.println();
        }
        if (problems.size() == 0) {
          System.err.println("Warning: removal failed, punting");
          return;
        }
        for (int j = 0; j < problems.size(); j++) {
          Lemma problem = (Lemma)problems.elementAt(j);
          if (demerits.containsKey(problem))
            demerits.put(problem,
                         new Integer(((Integer)demerits.get(problem))
                                     .intValue() + 1));
          else
            demerits.put(problem, new Integer(1));
        }
        int max_demerits = -1;
        Vector worst = new Vector();
        Iterator it = demerits.entrySet().iterator();
        while (it.hasNext()) {
          Map.Entry ent = (Map.Entry)it.next();
          int value = ((Integer)ent.getValue()).intValue();
          if (value == max_demerits) {
            worst.add(ent.getKey());
          } else if (value > max_demerits) {
            max_demerits = value;
            worst = new Vector();
            worst.add(ent.getKey());
          }
        }
        int offsetFromEnd = worstWheel % worst.size();
        worstWheel = (3*worstWheel + 1) % 10000019;
        int index = worst.size() - 1 - offsetFromEnd;
        Lemma bad = (Lemma)worst.elementAt(index);
        demerits.remove(bad);
        proverStack.popToMark(backgroundMark);
        boolean isInvariant = false;
        for (int i = 0; i < lemmas.length; i++) {
          if (lemmas[i] == bad) {
            present[i] = false;
            isInvariant = true;
          } else if (present[i]) {
            proverStack.pushLemma(lemmas[i]);
          }
        }
        if (!isInvariant)
          proverStack.removeLemma(bad);
        if (LemmaStack.dkconfig_print_contradictions) {
          System.err.println("Removing " + bad.summarize());
        } else if (Daikon.no_text_output && Daikon.show_progress) {
          System.err.print("x");
        }
      } while (proverStack.checkForContradiction() == 'T');
    }

    proverStack.popToMark(backgroundMark);

    flagRedundantRecursive(lemmas, present, 0, lemmas.length - 1);

    proverStack.clear();
  }

  /** Go though an array of invariants, marking those that can be
   * proved as consequences of others as redundant. */
  private void flagRedundantRecursive(InvariantLemma[] lemmas,
                                      boolean[] present, int start, int end)
    throws SimplifyError
  {
    Assert.assertTrue(start <= end);
    if (start == end) {
      // Base case: check a single invariant
      int checking = start;
      if (proverStack.checkLemma(lemmas[checking]) == 'T') {
//         System.err.println("-------------------------");
//         System.err.println(lemmas[checking].summarize() +
//                            " is redundant because of");
//         LemmaStack.printLemmas(System.err,
//                                proverStack.minimizeProof(lemmas[checking]));
        flagRedundant(lemmas[checking].invariant);
        present[checking] = false;
      }
      SessionManager.debugln((present[checking] ? "UNIQUE" : "REDUNDANT")
                             + " " + lemmas[checking].summarize());
    } else {
      // Recursive case: divide and conquer
      int first_half_end = (start + end) / 2;
      int second_half_start = first_half_end + 1;
      int mark = proverStack.markLevel();
      // First, assume the first half and check the second half
      for (int i = start; i <= first_half_end; i++) {
        if (present[i])
          proverStack.pushLemma(lemmas[i]);
      }
      flagRedundantRecursive(lemmas, present, second_half_start, end);
      proverStack.popToMark(mark);
      // Now, assume what's left of the second half, and check the
      // first half.
      for (int i = second_half_start; i <= end; i++) {
        if (present[i])
          proverStack.pushLemma(lemmas[i]);
      }
      flagRedundantRecursive(lemmas, present, start, first_half_end);
      proverStack.popToMark(mark);
    }
  }

  /** Mark an invariant as redundant */
  private void flagRedundant(Invariant inv) {
    if (inv instanceof Equality) {
      // ick ick ick
      // Equality is not represented with a permanent invariant
      // object, so store the canonical variable instead.
      redundant_invs.add(((Equality) inv).leader());
    } else {
      redundant_invs.add(inv);
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

    paramVars = new LinkedHashSet();
    for (int i = 0; i < var_infos.length; i++) {
      VarInfo var = var_infos[i];
      if (var.aux.getFlag(VarInfoAux.IS_PARAM) && !var.isPrestate()) {
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
    // // System.out.println(implication_view.invs.size() + " implication invs for " + name() + " at " + implication_view.name);
    // result.addAll(implication_view.invs);
    return Collections.unmodifiableList(result);
  }

  // restored to ease merge between V2 and V3.  Its unclear why the above
  // was changed in any event...
  public Vector invariants_vector() {
    return new Vector (getInvariants());
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

    // Commented this code out because conditional views are not slices.
    // It is not clear what this is trying to accomplish
//     Object viewCondArray[] = views_cond.toArray();
//     for (int i=0; i < viewCondArray.length; i++) {
//       PptSlice currentCondView = (PptSlice)viewCondArray[i];
//       currentCondView.guardInvariants();
//     }
    // This is a version changed to use the new conditional ppt iterator.
    // But the elements are still not slices!
//     for (Iterator i = cond_iterator(); i.hasNext(); ) {
//       PptSlice currentCondView = (PptSlice) i.next();
//       currentCondView.guardInvariants();
//     }


    // System.out.println("Ppt name: " + name());
    // System.out.println("Number of invs in joiner_view: " + joiner_view.invs.size());
  }

  public void processOmissions(boolean[] omitTypes) {
    // Avoid concurrent modification exceptions using arrays
    Object viewArray[] = viewsAsCollection().toArray();
    for (int i=0; i < viewArray.length; i++) {
      PptSlice currentView = (PptSlice)viewArray[i];
      currentView.processOmissions(omitTypes);
    }
    for (Iterator i = cond_iterator(); i.hasNext(); ) {
      PptConditional currentCondView = (PptConditional) i.next();
      currentCondView.processOmissions(omitTypes);
    }
  }

  /**
   * Check the rep invariants of this.  Throw an Error if not okay.
   **/
  public void repCheck() {
    // Check that the hashing of 'views' is working correctly. This
    // should really be beneath the abstraction layer of the hash
    // table, but it isn't because Java can't enforce the immutability
    // of the keys. In particular, we got into trouble in the past
    // when the keys had pointers to VarInfos which themselves
    // indirectly pointed back to us. If the serializer found the
    // VarInfo before it found us, the VarInfo would be in-progress at
    // the time the HashMap was serialized. At the point when When the
    // PptTopLevel was unserialized, the VarInfo pointers in the keys
    // would be null, causing them to have a different hashCode than
    // they should. When the VarInfo was fully unserialized, the key's
    // hashCode then changed to the correct one, messing up the
    // indexing in a hard-to-debug way. -SMcC
    Iterator view_keys_it = views.keySet().iterator();
    while (view_keys_it.hasNext()) {
      List this_key = (List)view_keys_it.next();
      Assert.assertTrue(views.containsKey(this_key));
    }
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

  /**
   * Debug method to print children (in the partial order) recursively
   */
  public void debug_print_tree (Logger l, int indent, PptRelation parent_rel) {

    // Calculate the indentation
    String indent_str = "";
    for (int i = 0; i < indent; i++)
      indent_str += "--  ";

    // Get the type of the parent relation
    String rel_type = "";
    if (parent_rel != null)
      rel_type = parent_rel.getRelationType();

    // Calculate the variable relationships
    String var_rel = "[]";
    if (parent_rel != null)
      var_rel = "[" + parent_rel.parent_to_child_var_string() +"]";

    // Put out this item
    l.fine (indent_str + ppt_name + ": " + rel_type + ": " + var_rel);

    // Put out children if this is the primary relationship.  Limiting
    // this to primary relations simplifies the tree for viewing while
    // not leaving anything out.
    if ((parent_rel == null) || parent_rel.is_primary()
         || (Daikon.ppt_regexp != null)) {
      for (Iterator i = children.iterator(); i.hasNext(); )
        ((PptRelation)(i.next())).debug_print_tree (l, indent+1);
    }
  }

  /**
   * Returns a string version of all of the equality sets for this ppt.
   * The string is of the form [a,b], [c,d] where a,b and c,d are
   * each in an equality set.  Should be used only for debugging.
   */
  public String equality_sets_txt () {

    if (equality_view == null)
      return ("null");

    String out = "";
    for (int i = 0; i < equality_view.invs.size(); i++) {
      Equality e = (Equality) equality_view.invs.get (i);
      Set vars = e.getVars();
      String set_str = "";
      for (Iterator j = vars.iterator(); j.hasNext(); ) {
        VarInfo v = (VarInfo) j.next();
        if (set_str != "")      // interned
          set_str += ",";
        set_str += v.name.name();
        if (v.missingOutOfBounds())
          set_str += "{MOB}";
      }
      if (out != "")            // interned
        out += ", ";
      out += "[" + set_str + "]";
    }

    return (out);
  }

  /**
   * Returns whether or not the specified variable in this ppt has any
   * parents.
   */
  public boolean has_parent (VarInfo v) {

    for (Iterator i = parents.iterator(); i.hasNext(); ) {
      PptRelation rel = (PptRelation) i.next();
      if (rel.parentVar (v) != null)
        return (true);
    }

    return (false);
  }

  /**
   * Recursively merge invariants from children to create an invariant
   * list at this ppt.
   *
   * First, equality sets are created for this ppt.  These are the
   * intersection of the equality sets from each child.  Then create
   * unary, binary, and ternary slices for each combination of equality
   * sets and build the invariants for each slice.
   */

  public void mergeInvs() {

    // If we don't have any children, there is nothing to do.
    if (children.size() == 0)
      return;

    // If this has already been done (because this ppt has multiple parents)
    // there is nothing to do.
    if (invariants_merged)
      return;

    // First do this for any children.
    for (Iterator i = children.iterator(); i.hasNext(); ) {
      PptRelation rel = (PptRelation) i.next();
        rel.child.mergeInvs();
    }

    if (debugMerge.isLoggable(Level.FINE))
      debugMerge.fine ("Processing ppt " + name());

    // Number of samples here is the sum of all of the child samples, presuming
    // there are some variable relationships with the child (note that
    // some ppt relationships such as constructor ENTER ppts to their
    // object ppts do not have any variable relationships)
    for (int i = 0; i < children.size(); i++) {
      PptRelation rel = (PptRelation) children.get(i);
      if (rel.size() > 0)
        values_num_samples += rel.child.values_num_samples;
    }

    // Merge any always missing variables from the children
    if (Daikon.use_dynamic_constant_optimization) {
      Assert.assertTrue (constants == null);
      constants = new DynamicConstants (this);
      constants.merge();
    }

    // Merge the ModBitTracker.
    // We'll reuse one dummy ValueTuple throughout, side-effecting its mods
    // array.
    int num_tracevars = mbtracker.num_vars();
    Object[] vals = new Object[num_tracevars];
    int[] mods = new int[num_tracevars];
    ValueTuple vt = ValueTuple.makeUninterned(vals, mods);
    for (int childno = 0; childno < children.size(); childno++) {
      PptRelation rel = (PptRelation) children.get(childno);
      ModBitTracker child_mbtracker = rel.child.mbtracker;
      int child_mbsize = child_mbtracker.num_samples();
      // System.out.println("mergeInvs child #" + childno + "=" + rel.child.name() + " has size " + child_mbsize + " for " + name());
      for (int sampno=0; sampno<child_mbsize; sampno++) {
        Arrays.fill(mods, ValueTuple.MISSING_FLOW);
        for (int j = 0; j < var_infos.length; j++) {
          VarInfo parent_vi = var_infos[j];
          VarInfo child_vi = rel.childVar(parent_vi);
          if ((child_vi != null) && (child_vi.value_index != -1)) {
            if (child_mbtracker.get(child_vi.value_index, sampno)) {
              mods[parent_vi.value_index] = ValueTuple.MODIFIED;
            }
          }
        }
        mbtracker.add(vt, 1);
      }
    }

    // Merge the ValueSets.
    for (int childno = 0; childno < children.size(); childno++) {
      PptRelation rel = (PptRelation) children.get(childno);

      for (int j = 0; j < var_infos.length; j++) {
        VarInfo parent_vi = var_infos[j];
        VarInfo child_vi = rel.childVar(parent_vi);
        if (child_vi != null) {
          Assert.assertTrue(parent_vi.ppt == this);
          if (parent_vi.value_index == -1) {
            continue;
          }
          ValueSet parent_vs = value_sets[parent_vi.value_index];
          ValueSet child_vs = rel.child.value_sets[child_vi.value_index];
          parent_vs.add(child_vs);
        }
      }
    }

    // Merge information stored in the VarInfo objects themselves.
    // Currently just the "canBeMissing" field, which is needed by
    // guarding, and the flag that marks missing-out-of-bounds
    // derived variables
    for (int i = 0; i < children.size(); i++) {
      PptRelation rel = (PptRelation) children.get(i);
      // This approach doesn't work correctly for the OBJECT_USER
      // relation case, because obj.field could be missing in a user PPT
      // when obj is null, but shouldn't be missing in the OBJECT PPT,
      // since "this" is always present for object invariants.
      // For the moment, just punt on this case, to match the previous
      // behavior.
      if (rel.getRelationType() == PptRelation.OBJECT_USER)
        continue;
      for (int j = 0; j < var_infos.length; j++) {
        VarInfo parent_vi = var_infos[j];
        VarInfo child_vi = rel.childVar(parent_vi);
        if (child_vi != null) {
          parent_vi.canBeMissing |= child_vi.canBeMissing;
          if (parent_vi.derived != null && child_vi.derived != null)
            parent_vi.derived.missing_array_bounds |=
              child_vi.derived.missing_array_bounds;
        }
      }
    }

    // Create the (empty) equality view for this ppt
    Assert.assertTrue (equality_view == null);
    equality_view = new PptSliceEquality (this);

    // Get all of the binary relationships from the first child's
    // equality sets.
    Map emap = null;
    int first_child = 0;
    for (first_child = 0; first_child  < children.size(); first_child++) {
      PptRelation c1 = (PptRelation) children.get(first_child);
      debugMerge.fine ("looking at " + c1.child.name() + " "
                       + c1.child.num_samples());
      if (c1.child.num_samples() > 0) {
        emap = c1.get_child_equalities_as_parent();
        debugMerge.fine ("child " + c1.child.name() + " equality = " + emap);
        break;
      }
    }
    if (emap == null) {
      equality_view.instantiate_invariants();
      invariants_merged = true;
      return;
    }

    // Loop through the remaining children, intersecting the equal
    // variables and incrementing the sample count as we go
    for (int i = first_child+1; i < children.size(); i++) {
      PptRelation rel = (PptRelation) children.get(i);
      if (rel.child.num_samples() == 0)
        continue;
      Map eq_new = rel.get_child_equalities_as_parent();
      for (Iterator j = emap.keySet().iterator(); j.hasNext(); ) {
        VarInfo.Pair curpair = (VarInfo.Pair) j.next();
        VarInfo.Pair newpair = (VarInfo.Pair) eq_new.get (curpair);
        if (newpair == null)
          j.remove();
        else
          curpair.samples += newpair.samples;
      }
    }
    if (debugMerge.isLoggable (Level.FINE)) {
      debugMerge.fine ("Found equality pairs ");
      for (Iterator i = emap.keySet().iterator(); i.hasNext(); )
        debugMerge.fine ("-- " + (VarInfo.Pair) i.next());
    }

    // Build actual equality sets that match the pairs we found
    equality_view.instantiate_from_pairs (emap.keySet());
    if (debugMerge.isLoggable (Level.FINE)) {
      debugMerge.fine ("Built equality sets ");
      for (int i = 0; i < equality_view.invs.size(); i++) {
        Equality e = (Equality) equality_view.invs.get (i);
        debugMerge.fine ("-- " + e.shortString());
      }
    }

    // There shouldn't be any slices when we start
    Assert.assertTrue (views.size() == 0);

    // Create an array of leaders to build slices over
    VarInfo[] leaders = new VarInfo[equality_view.invs.size()];
    for (int i = 0; i < equality_view.invs.size(); i++)
      leaders[i] = ((Equality) equality_view.invs.get(i)).leader();

    // Create unary views and related invariants
    List unary_slices = new ArrayList();
    for (int i = 0; i < leaders.length; i++) {
      PptSlice1 slice1 = new PptSlice1 (this, leaders[i]);
      slice1.merge_invariants();
      unary_slices.add (slice1);
    }
    addSlices (unary_slices);
    if (debugMerge.isLoggable(Level.FINE))
      debug_print_slice_info (debugMerge, "unary", unary_slices);

    // Create binary views and related invariants
    List binary_slices = new ArrayList();
    for (int i = 0; i < leaders.length; i++) {
      for (int j = i; j < leaders.length; j++) {
        PptSlice2 slice2 = new PptSlice2 (this, leaders[i], leaders[j]);
        slice2.merge_invariants();
        if (slice2.invs.size() > 0)
          binary_slices.add (slice2);
      }
    }
    addSlices (binary_slices);
    if (debugMerge.isLoggable(Level.FINE))
      debug_print_slice_info (debugMerge, "binary", binary_slices);


    // Create ternary views and related invariants.  Since there
    // are no ternary array invariants, those slices don't need to
    // be created.
    List ternary_slices = new ArrayList();
    for (int i = 0; i < leaders.length; i++) {
      if (leaders[i].rep_type.isArray())
        continue;
      for (int j = i; j < leaders.length; j++) {
        if (leaders[j].rep_type.isArray())
          continue;
        if (!leaders[i].compatible(leaders[j]))
          continue;
        for (int k = j; k < leaders.length; k++) {
          if (leaders[k].rep_type.isArray())
            continue;
          if (!leaders[i].compatible(leaders[k]))
            continue;
          PptSlice3 slice3 = new PptSlice3 (this, leaders[i], leaders[j],
                                            leaders[k]);
          slice3.merge_invariants();
          if (slice3.invs.size() > 0)
            ternary_slices.add (slice3);
        }
      }
    }
    addSlices (ternary_slices);
    if (debugMerge.isLoggable(Level.FINE))
      debug_print_slice_info (debugMerge, "ternary", ternary_slices);

    // Merge the conditionals
    merge_conditionals();

    // Remove any child invariants that now exist here
    if (dkconfig_remove_merged_invs) {
      for (Iterator i = children.iterator(); i.hasNext(); ) {
        PptRelation rel = (PptRelation) i.next();
        rel.child.remove_child_invs (rel);
      }
    }

    // Mark this ppt as merged, so we don't process it multiple times
    invariants_merged = true;
  }

  // used by merge_conditionals below
  private static class SplitChild {
    public PptRelation rel;
    public PptSplitter ppt_split;
    public SplitChild (PptRelation rel, PptSplitter ppt_split) {
      this.rel = rel;
      this.ppt_split = ppt_split;
    }
  }

  /**
   * Merges the conditionals from the children of this ppt to this ppt.
   * Only conditionals that exist at each child and whose splitting condition
   * is valid here can be merged.
   */
  public void merge_conditionals() {

    debugConditional.fine ("attempting merge conditional for " + name());

    // If there are no children, there is nothing to do
    if (children.size() == 0)
      return;

    // If the children are only ppt => ppt_cond, there is nothing to do
    for (Iterator ii = children.iterator(); ii.hasNext(); ) {
      PptRelation rel = (PptRelation) ii.next();
      if (rel.getRelationType() == PptRelation.PPT_PPTCOND)
        return;
    }

    // If there are no splitters there is nothing to do
    if (!has_splitters())
      return;

    if (debugConditional.isLoggable (Level.FINE)) {
      debugConditional.fine ("Merge conditional for " + name());
      for (int ii = 0; ii < children.size(); ii++ ) {
        PptRelation rel = (PptRelation) children.get(ii);
        debugConditional.fine ("child: " + rel);
      }
    }

    // Build a splitters list at this point that contains all of the
    // splitters that exist at each child.  This should also insure that
    // the splitter is over the same variables both here and at each
    // child (but it doesn't).  The shortcut is to accept the splitter if
    // it is valid at this point.

    // Loop over each splitter
    splitter_loop:
    for (Iterator ii = splitters.iterator(); ii.hasNext(); ) {
      PptSplitter ppt_split = (PptSplitter) ii.next();

      // list of children that match this splitter
      List /*SplitChild*/ split_children = new ArrayList();

      // Create a list of children for this splitter
      child_loop: for (int jj = 0; jj < children.size(); jj++ ) {
        PptRelation rel = (PptRelation) children.get(jj);
        if (!rel.child.has_splitters())
          break;
        for (Iterator kk = rel.child.splitters.iterator(); kk.hasNext(); ) {
          PptSplitter csplit = (PptSplitter) kk.next();
          if (ppt_split.splitter == csplit.splitter) {
            split_children.add (new SplitChild (rel, csplit));
            continue child_loop;
          }
        }
        break;
      }


      // If we didn't find a matching splitter at each child, can't merge
      // this point.  Just remove it from the list of splitters
      if (split_children.size() != children.size()) {
        ii.remove();
        continue;
      }

      // Build the PptRelations for each child.  The PptRelation from
      // the conditional point is of the same type as the original
      // relation from parent to child
      for (Iterator jj = split_children.iterator(); jj.hasNext(); ) {
        SplitChild sc = (SplitChild) jj.next();
        ppt_split.add_relation (sc.rel, sc.ppt_split);
      }
    }

    // Merge the conditional points
    for (Iterator ii = cond_iterator(); ii.hasNext(); ) {
      PptConditional ppt_cond = (PptConditional) ii.next();
      if (debugConditional.isLoggable (Level.FINE)) {
        debugConditional.fine ("Merge invariants for conditional "
                                + ppt_cond.name());
        for (int jj = 0; jj < ppt_cond.children.size(); jj++ ) {
          PptRelation rel = (PptRelation) ppt_cond.children.get(jj);
          debugConditional.fine ("child relation: " + rel);
          debugConditional.fine ("child equality set = "
                                  + rel.child.equality_sets_txt());
        }
      }
      ppt_cond.mergeInvs();
      debugConditional.fine ("After merge, equality set = "
                              + ppt_cond.equality_sets_txt());
    }
  }

  /**
   * Cleans up the ppt so that its invariants can be merged from other
   * ppts.  Not normally necessary unless the merge is taking place over
   * multiple ppts maps based on different data.  This allows a ppt to
   * have its invariants recalculated.
   */
  public void clean_for_merge() {
    equality_view = null;
    for (int i = 0; i < var_infos.length; i++)
      var_infos[i].equalitySet = null;
    views = new HashMap();
    // parents = new ArrayList();
    // children = new ArrayList();
    invariants_merged = false;
  }

  /**
   * Removes any invariant in this ppt which has a matching invariant in the
   * parent (as specified in the relation).  Done to save space.  Only safe
   * when all processing of this child is complete (ie, all of the parents
   * of this child must have been merged)
   *
   * Another interesting problem arises with this code.  As currently
   * setup, it won't process combined exit points (which often have two
   * parents), but it will process enter points.  Once the enter point
   * is removed, it can no longer parent filter the combined exit point.
   *
   * Also, the dynamic obvious code doesn't work anymore (because it is
   * missing the appropriate invariants).  This could be fixed by changing
   * dynamic obvious to search up the tree (blecho!).  Fix this by
   * only doing this for ppts whose parent only has one child
   */
  public void remove_child_invs (PptRelation rel) {

    // For now, only do this for ppts with only one parent
    if (parents.size() != 1)
      return;

    // Only do this for ppts whose children have also been removed
    for (Iterator i = children.iterator(); i.hasNext(); ) {
      PptRelation crel = (PptRelation) i.next();
      if (!crel.child.invariants_removed) {
        // System.out.println ("Rel " + rel + "has not had its child invs rm");
        return;
      }
    }

    // Only do this for ppts whose parent only has one child
    if (rel.parent.children.size() != 1)
      return;

    System.out.println ("Removing child invariants at " + name());

    // Do suppressions first, so that we don't lose any suppressions
    // when eliminating invariants
    suppressAll (false);

    List /*PptSlice*/ slices_to_remove = new ArrayList();

    // Loop through each slice
    slice_loop: for (Iterator i = views_iterator(); i.hasNext(); ) {
      PptSlice slice = (PptSlice) i.next();

      // Build the varlist for the parent.  If any variables are not present in
      // the parent, skip this slice
      VarInfo pvis[] = new VarInfo[slice.var_infos.length];
      VarInfo pvis_sorted[] = new VarInfo[slice.var_infos.length];
      for (int j = 0; j < slice.var_infos.length; j++) {
        VarInfo pv = rel.parentVar (slice.var_infos[j]);
        if (pv == null)
          continue slice_loop;
        pvis[j] = pv.canonicalRep();
        pvis_sorted[j] = pvis[j];
      }
      Arrays.sort (pvis_sorted, VarInfo.IndexComparator.getInstance());

      // Find the parent slice.  If it doesn't exist, there is nothing to do
      PptSlice pslice = rel.parent.findSlice (pvis_sorted);
      if (pslice == null)
        continue;

      // Build the permute from child to parent
      int[] permute = build_permute (pvis_sorted, pvis);

      // Remove any invariant that is also present in the parent
      for (Iterator j = slice.invs.iterator(); j.hasNext(); ) {
        Invariant orig_inv = (Invariant) j.next();
        Invariant inv = orig_inv.clone_and_permute (permute);
        Invariant pinv = pslice.find_inv_exact (inv);
        if (pinv != null) {
          j.remove();
          //System.out.println ("Removed " + orig_inv + " @" + name()
          //                    + " parent inv " + pinv + " @" + rel.parent);
        }
      }

      // If all of the invariants in a slice were removed, note it for removal
      if (slice.invs.size() == 0)
        slices_to_remove.add (slice);
    }

    // Remove all of the slices with 0 invariants
    System.out.println ("  Removed " + slices_to_remove.size() + " slices");
    for (Iterator i = slices_to_remove.iterator(); i.hasNext(); ) {
      PptSlice slice = (PptSlice) i.next();
      // System.out.println ("Removing Slice " + slice);
      removeSlice (slice);
    }

    invariants_removed = true;

  }
  /**
   * Builds a permutation from vis1 to vis2. The result is
   * vis1[i] = vis2[permute[i]]
   */
  public static int[] build_permute (VarInfo[] vis1, VarInfo[] vis2) {

    int[] permute = new int[vis1.length];
    boolean[] matched = new boolean[vis1.length];
    Arrays.fill (matched, false);

    for (int j = 0; j < vis1.length; j++) {
      for (int k = 0; k < vis2.length; k++) {
        if ((vis1[j] == vis2[k]) && (!matched[k])) {
          permute[j] = k;
          matched[k] = true;  // don't match the same one twice
          break;
        }
      }
    }

    // Check results
    for (int j = 0; j < vis1.length; j++)
      Assert.assertTrue (vis1[j] == vis2[permute[j]]);

    return (permute);
  }


  public void debug_print_slice_info (Logger debug, String descr,
                                      List /*PptSlice*/ slices) {

    int inv_cnt = 0;
    for (int i = 0; i < slices.size(); i++)
      inv_cnt += ((PptSlice) slices.get(i)).invs.size();
    debug.fine (slices.size() + descr + " slices with " + inv_cnt
                + " invariants");

  }

  /** prints out any suppressed invariants at the ppt and their suppressors **/
  public void print_suppressed_invs (Logger debug) {

    for (Iterator j = views_iterator(); j.hasNext(); ) {
      PptSlice slice = (PptSlice) j.next();
      for (int k = 0; k < slice.invs.size(); k++ ) {
        Invariant inv = (Invariant) slice.invs.get(k);
        if (inv.getSuppressor() != null)
          debug.fine (" : " + inv.format() + " suppressed by : "
                      + inv.getSuppressor());
      }
    }
  }

  /**
   * Insures that there are no invariants at this level that are duplicated
   * at the global level
   */
  public boolean check_vs_global () {

    if (global == null)
      return (true);

    boolean ok = true;
    for (Iterator j = views_iterator(); j.hasNext(); ) {
      PptSlice slice = (PptSlice) j.next();
      PptSlice gslice = slice.find_global_slice (slice.var_infos);
      if (gslice == null)
        continue;
      for (int k = 0; k < slice.invs.size(); k++) {
        Invariant inv = (Invariant) slice.invs.get(k);
        Invariant ginv = gslice.find_inv_exact (inv);
        if ((ginv != null) && inv.isActive() && ginv.isActive()) {
          String cname = inv.getClass().getName();
          if ((cname.indexOf ("Bound") == -1)
            && (cname.indexOf("OneOf") == -1)) {
            System.out.println ("inv " + inv + " in slice " + name() +
                              " also appears at the global slice as " + ginv);
            ok = false;
          }
        }
      }
    }
    return (ok);
  }

  /**
   * Create the transforms between this point and the global ppt.
   * These transforms allow samples to be quickly created for the
   * global point and also allow invariants to be easily permuted
   * between the two points
   */

  public void init_global_transforms (PptTopLevel global) {

    // We use this during post processing at all points, so the
    // following check is commented out.
    // Make sure this is a numbered exit point.
    //Assert.assertTrue (ppt_name.isExitPoint()
    //                   && !ppt_name.isCombinedExitPoint());

    // Look for matching names for each global variable in the child
    global_transform_post = new int[global.var_infos.length];
    Arrays.fill (global_transform_post, -1);
    for (int i = 0; i < global.var_infos.length; i++) {
      VarInfo vp = global.var_infos[i];
      for (int j = 0; j < var_infos.length; j++) {
        VarInfo vc = var_infos[j];
        if (vp.name == vc.name) {
          Assert.assertTrue (vp.varinfo_index == vp.value_index);
          Assert.assertTrue (vc.varinfo_index == vc.value_index);
          global_transform_post[vp.varinfo_index] = vc.varinfo_index;
          vc.global_index = (short) vp.varinfo_index;
          break;
        }
      }
    }

    // Look for orig versions of each non-derived global variable in the child
    global_transform_orig = new int[global.var_infos.length];
    Arrays.fill (global_transform_orig, -1);
    for (int i = 0; i < global.var_infos.length; i++) {
      VarInfo vp = global.var_infos[i];
      if (vp.derived != null)
        continue;
      VarInfoName orig_name = vp.name.applyPrestate().intern();
      for (int j = 0; j < var_infos.length; j++) {
        VarInfo vc = var_infos[j];
        if (orig_name == vc.name) {
          Assert.assertTrue (vp.varinfo_index == vp.value_index);
          Assert.assertTrue (vc.varinfo_index == vc.value_index);
          global_transform_orig[vp.varinfo_index] = vc.varinfo_index;
          vc.global_index = (short) vp.varinfo_index;
          break;
        }
      }
    }

    // Look for orig versions of derived variables in the child.  This is
    // done by finding the base of each derived variable and looking for
    // a child variable with the same bases and the same equation.  This
    // is necessary because derivations are done AFTER orig variables so
    // applying the prestate name (as done above) won't work (the resulting
    // variable is really the same but the name is constructed differently)

    // TODO

    // Debug print the transforms
    if (debugGlobal.isLoggable(Level.FINE)) {
      debugGlobal.fine ("orig transform at " + name());
      for (int i = 0; i < global_transform_orig.length; i++) {
        if (global_transform_orig[i] == -1)
          debugGlobal.fine ("-- " + global.var_infos[i].name.name()
                            + " : NO MATCH");
        else
          debugGlobal.fine ("-- " + global.var_infos[i].name.name() + " : " +
                            var_infos[global_transform_orig[i]].name.name());
      }
      debugGlobal.fine ("post transform at " + name());
      for (int i = 0; i < global_transform_post.length; i++) {
        if (global_transform_post[i] == -1)
          debugGlobal.fine ("-- " + global.var_infos[i].name.name()
                            + " : NO MATCH");
        else
          debugGlobal.fine ("-- " + global.var_infos[i].name.name() + " : " +
                            var_infos[global_transform_post[i]].name.name());
      }
    }
  }

  /**
   * Transform a sample to the global ppt using the specified transform.
   * transform[i] returns the value_index in this ppt that corresponds to
   * index i in the global ppt
   **/
  public ValueTuple transform_sample (PptTopLevel global, int[] transform,
                                      ValueTuple vt) {

    Object[] vals = new Object[global.var_infos.length];
    int[] mods = new int [global.var_infos.length];
    Arrays.fill (mods, ValueTuple.MISSING_FLOW);
    for (int i = 0; i < transform.length; i++) {
      if (transform[i] != -1) {
        vals[i] = vt.vals[transform[i]];
        mods[i] = vt.mods[transform[i]];
      }
    }
    return (new ValueTuple (vals, mods));
  }

  /**
   * Returns the local variable that corresponds to the specified global
   * variable via the post transform
   */
  public VarInfo local_postvar (VarInfo global) {

    return var_infos[global_transform_post[global.varinfo_index]];
  }

  /**
   * Returns the local variable that corresponds to the specified global
   * variable via the orig transform
   */
  public VarInfo local_origvar (VarInfo global) {

    return var_infos[global_transform_orig[global.varinfo_index]];
  }

  public PptSlice create_equality_inv (VarInfo v1, VarInfo v2, int samples) {

    ProglangType rep = v1.rep_type;
    boolean rep_is_scalar = rep.isScalar();
    boolean rep_is_float = rep.isFloat();

    // Assert.assertTrue (findSlice_unordered (v1, v2) == null);
    PptSlice newSlice = get_or_instantiate_slice (v1, v2);

    // Copy over the number of samples from this to the new slice,
    // so that all invariants on the slice report the right number
    // of samples.
    newSlice.set_samples (samples);

    Invariant invEquals = null;

    // This is almost directly copied from PptSlice2's instantiation
    // of factories
    if (rep_is_scalar) {
      invEquals = IntEqual.instantiate (newSlice);
    } else if ((rep == ProglangType.STRING)) {
      // invEquals = StringEqual.instantiate (newSlice);
      invEquals = StringComparison.instantiate (newSlice, true);
      ((StringComparison) invEquals).core.can_be_eq = true;
    } else if ((rep == ProglangType.INT_ARRAY)) {
      invEquals = SeqSeqIntEqual.instantiate (newSlice, true);
    } else if ((rep == ProglangType.STRING_ARRAY)) {
      // JHP commented out to see what diffs are coming from here (5/3/3)
//         invEquals = SeqComparisonString.instantiate (newSlice, true);
//         if (invEquals != null) {
//           ((SeqComparisonString) invEquals).can_be_eq = true;
//         }
//         debugPostProcess.fine ("  seqStringEqual");
    } else if (Daikon.dkconfig_enable_floats) {
      if (rep_is_float) {
        invEquals = FloatEqual.instantiate (newSlice);
      } else if (rep == ProglangType.DOUBLE_ARRAY) {
        invEquals = SeqSeqFloatEqual.instantiate (newSlice, true);
      }
    } else {
      throw new Error ("No known Comparison invariant to convert equality into");
    }


    if (invEquals != null) {
      SuppressionLink sl = SelfSuppressionFactory.getInstance().generateSuppressionLink (invEquals);
      if (sl != null) {
        // System.out.println (invEquals + "suppressed by " + sl);
        ;
      } else {
        newSlice.addInvariant (invEquals);
        // System.out.println ("created equality " + invEquals.format());
      }
    } else {
      // System.out.println ("Can't create equality for " + v1.name.name() + ", "
      //                      + v2.name.name());
      if (newSlice.invs.size() == 0)
        newSlice.parent.removeSlice (newSlice);
    }
    return (newSlice);
  }



  public static void count_unique_slices (Logger debug, PptMap all_ppts) {

    Map slices = new LinkedHashMap (10000);

    int slice_cnt = 0;
    int ppt_cnt = 0;

    // Loop through each ppt
    for (Iterator ii = all_ppts.pptIterator() ; ii.hasNext() ; ) {
      PptTopLevel ppt = (PptTopLevel) ii.next();
      if (ppt == all_ppts.getGlobal())
        continue;
      if (!ppt.ppt_name.isExitPoint())
        continue;
      if (ppt.ppt_name.isCombinedExitPoint())
        continue;
      ppt_cnt++;

      // Loop through each ternary slice
      slice_loop:
      for (Iterator jj = ppt.views_iterator(); jj.hasNext(); ) {
        PptSlice slice = (PptSlice) jj.next();
        if (slice.arity() != 3)
          continue;
        for (int kk = 0; kk < slice.var_infos.length; kk++) {
          if (!slice.var_infos[kk].is_global()) {
            continue slice_loop;
          }
        }

        slice_cnt++;
        SliceMatch sm_new = new SliceMatch (slice);
        SliceMatch sm_old = (SliceMatch) slices.get (sm_new);
        if (sm_old != null)
          sm_old.all_slices.add (slice);
        else
          slices.put (sm_new, sm_new);
      }
    }

    int max_out = 1000;
    System.out.println (slice_cnt + " slices considered over " + ppt_cnt
            + " ppts, " + slices.size() + " unique slices in map");
    for (Iterator ii = slices.values().iterator(); ii.hasNext(); ) {
      SliceMatch sm = (SliceMatch) ii.next();
      debug.fine ("Slice occurs " + sm.all_slices.size() + " times");
      for (int jj = 0; jj < sm.all_slices.size(); jj++) {
        PptSlice slice = (PptSlice) sm.all_slices.get(jj);
        debug.fine (": " + slice);
        for (int kk = 0; kk < slice.invs.size(); kk++) {
          Invariant inv = (Invariant) slice.invs.get(kk);
          debug.fine (": : " + inv.format());
        }
      }
      if (--max_out <= 0)
        break;
    }
  }

  public static void count_unique_inv_lists (Logger debug, PptMap all_ppts) {

    Map slices = new LinkedHashMap (10000);

    int inv_list_cnt = 0;
    int ppt_cnt = 0;

    // Loop through each ppt
    for (Iterator ii = all_ppts.pptIterator() ; ii.hasNext() ; ) {
      PptTopLevel ppt = (PptTopLevel) ii.next();
      if (ppt == all_ppts.getGlobal())
        continue;
      if (!ppt.ppt_name.isExitPoint())
        continue;
      if (ppt.ppt_name.isCombinedExitPoint())
        continue;
      ppt_cnt++;

      // Loop through each ternary slice
      for (Iterator jj = ppt.views_iterator(); jj.hasNext(); ) {
        PptSlice slice = (PptSlice) jj.next();
        if (slice.arity() != 3)
          continue;

        inv_list_cnt++;
        InvListMatch invs_new = new InvListMatch (slice.invs);
        InvListMatch invs_old = (InvListMatch) slices.get (invs_new);
        if (invs_old != null)
          invs_old.all_invs.add (slice.invs);
        else
          slices.put (invs_new, invs_new);
      }
    }

    int max_out = 100;
    System.out.println (inv_list_cnt + " slices considered over " + ppt_cnt
            + " ppts, " + slices.size() + " unique slices in map");
    if (true)
      return;
    for (Iterator ii = slices.values().iterator(); ii.hasNext(); ) {
      InvListMatch ilm = (InvListMatch) ii.next();
      if (ilm.all_invs.size() > 1)
        continue;
      debug.fine ("Slice occurs " + ilm.all_invs.size() + " times");
      for (int jj = 0; jj < ilm.all_invs.size(); jj++) {
        Invariants invs = (Invariants) ilm.all_invs.get(jj);
        PptSlice slice = ((Invariant) invs.get(0)).ppt;
        debug.fine (": " + slice);
        for (int kk = 0; kk < invs.size(); kk++) {
          Invariant inv = (Invariant) invs.get(kk);
          debug.fine (": : " + inv.format());
        }
      }
      if (--max_out <= 0)
        break;
    }
  }

  /**
   * Provides hashcode/equal functions for slices over global variables
   * regardless of what ppt they are in.
   */
  public static class SliceMatch {

    PptSlice slice = null;
    public List all_slices = new ArrayList();

    public SliceMatch (PptSlice slice) {
      this.slice = slice;
      all_slices.add (slice);
    }

    public boolean equals (Object obj) {
      if (!(obj instanceof SliceMatch))
        return (false);

      SliceMatch sm = (SliceMatch) obj;
      PptSlice s1 = slice;
      PptSlice s2 = sm.slice;

      // Make sure they both refer to the same gobal variables via the same
      // transform
      if (s1.var_infos.length != s2.var_infos.length)
        return (false);
      for (int ii = 0; ii < s1.var_infos.length; ii++) {
        VarInfo v1 = s1.var_infos[ii];
        VarInfo v2 = s2.var_infos[ii];
        if (!v1.is_global() || !v2.is_global())
          return (false);
        if (v1.global_var() != v2.global_var())
          return (false);
        if (v1.is_post_global() != v2.is_post_global())
          return (false);
      }

      // Make sure that each invariant matches exactly
      if (s1.invs.size() != s2.invs.size())
        return (false);
      for (int ii = 0; ii < s1.invs.size(); ii++) {
        Invariant inv1 = (Invariant) s1.invs.get (ii);
        Invariant inv2 = (Invariant) s2.invs.get (ii);
        if (inv1.getClass() != inv2.getClass())
          return (false);
        if (!inv1.isSameFormula (inv2))
          return (false);
      }

      return (true);
    }

    public int hashCode() {

      int code = 0;

      // include a hash over the equivalent global vars
      for (int ii = 0; ii < slice.var_infos.length; ii++) {
        VarInfo gvar = slice.var_infos[ii].global_var();
        if (gvar == null)
          code += slice.var_infos[ii].hashCode();
        else
          code += gvar.hashCode();
      }

      // hash over the invariants.  This should be better for function
      // binary invariants
      for (int ii = 0; ii < slice.invs.size(); ii++) {
        code += slice.invs.get(ii).getClass().hashCode();
      }

      return (code);
    }
  }

  /**
   * Provides hashcode/equal functions for invariant lists regardless
   * of their ppt.
   */
  public static class InvListMatch {

    Invariants invs = null;
    public List all_invs = new ArrayList();

    public InvListMatch (Invariants invs) {
      this.invs = invs;
      all_invs.add (invs);
    }

    public boolean equals (Object obj) {
      if (!(obj instanceof InvListMatch))
        return (false);

      InvListMatch ilm = (InvListMatch) obj;
      Invariants invs1 = invs;
      Invariants invs2 = ilm.invs;

      // Make sure that each invariant matches exactly
      if (invs1.size() != invs2.size())
        return (false);
      for (int ii = 0; ii < invs1.size(); ii++) {
        Invariant inv1 = (Invariant) invs1.get (ii);
        Invariant inv2 = (Invariant) invs2.get (ii);
        if (inv1.getClass() != inv2.getClass())
          return (false);
        if (!inv1.isSameFormula (inv2))
          return (false);
      }

      return (true);
    }

    public int hashCode() {

      int code = 0;

      // hash over the invariants.  This should be better for function
      // binary invariants
      for (int ii = 0; ii < invs.size(); ii++) {
        code += invs.get(ii).getClass().hashCode();
      }

      return (code);
    }
  }

  /**
   * Stores various statistics about a ppt
   */
  public static class Stats {

    /** sample count **/
    public int sample_cnt = 0;

    /** number of equality sets **/
    public int set_cnt = 0;

    /** total number of variables in all equality sets **/
    public int var_cnt = 0;

    /** time (milliseconds) to process this sample **/
    public int time = 0;

    /** additional memory (bytes) allocated to processing this sample **/
    public long memory = 0;

    /** number of invariants **/
    public int inv_cnt = 0;

    /** number of slices **/
    public int slice_cnt = 0;

    /** number of instantiated invariants before the sample is applied **/
    public int instantiated_inv_cnt = 0;

    /** number of instantiated slices **/
    public int instantiated_slice_cnt = 0;

    /** number of suppressed invariants **/
    public int suppress_cnt = 0;

    /** total number of global invariants that have flowed **/
    public int tot_invs_flowed = 0;

    /** global invariants still in the list to flow **/
    public int invs_to_flow = 0;

    /** program point of the stat **/
    public PptTopLevel ppt;

    int const_slice_cnt = 0;
    int const_inv_cnt = 0;
    int constant_leader_cnt = 0;
    public static boolean cnt_inv_classes = false;
    Map inv_map = null;
    public static boolean show_invs = false;
    public static boolean show_tern_slices = false;

    /**
     * Sets each of the stats from the current info in ppt and the specified
     * time (msecs) and memory (bytes).
     */
    void set (PptTopLevel ppt, int time, int memory) {
      set_cnt = 0;
      var_cnt = 0;
      if (ppt.equality_view != null) {
        for (int j = 0; j < ppt.equality_view.invs.size(); j++) {
          set_cnt++;
          Equality e = (Equality) ppt.equality_view.invs.get(j);
          Collection vars = e.getVars();
          var_cnt += vars.size();
        }
      }
      this.ppt = ppt;
      sample_cnt = ppt.num_samples();
      slice_cnt = ppt.slice_cnt();
      const_slice_cnt = ppt.const_slice_cnt();
      const_inv_cnt = ppt.const_inv_cnt();
      inv_cnt = ppt.invariant_cnt();
      instantiated_slice_cnt = ppt.instantiated_slice_cnt;
      instantiated_inv_cnt = ppt.instantiated_inv_cnt;
      suppress_cnt = ppt.suppressed_invariant_cnt();
      tot_invs_flowed = weakened_invs.size();
      invs_to_flow = weakened_invs.size() - weakened_start_index;
      if (ppt.constants != null)
        constant_leader_cnt = ppt.constants.constant_leader_cnt();
      this.time = time;
      this.memory = memory;

      if (cnt_inv_classes)
        inv_map = ppt.invariant_cnt_by_class();
    }

    static void dump_header (Logger debug) {

      debug.fine ("Program Point : Sample Cnt: Equality Cnt : Var Cnt : "
                + " Vars/Equality : Const Slice Cnt :  "
                + " Slice /  Inv Cnt : Instan Slice / Inv Cnt "
                + ": Suppressed : Memory (bytes) : Time (msecs) ");
    }

    void dump (Logger debug) {

      DecimalFormat dfmt = new DecimalFormat();
      dfmt.setMaximumFractionDigits (2);
      dfmt.setGroupingSize (3);
      dfmt.setGroupingUsed (true);

      double vars_per_eq = 0;
      if (set_cnt > 0)
        vars_per_eq = (double) var_cnt / set_cnt;

      debug.fine (ppt.name() + " : "
                        + sample_cnt + " : "
                        + set_cnt + " (" + constant_leader_cnt + " con) : "
                        + var_cnt + " : "
                        + dfmt.format (vars_per_eq) + " : "
                        + const_slice_cnt + "/" + const_inv_cnt + " : "
                        + slice_cnt + "/"
                        + inv_cnt + " : "
                        + instantiated_slice_cnt + "/"
                        + instantiated_inv_cnt + " : "
                        + suppress_cnt + " : "
                        + tot_invs_flowed + "/"
                        + invs_to_flow + ": "
                        + memory + ": "
                        + time);
      if (cnt_inv_classes) {
        for (Iterator i = inv_map.keySet().iterator(); i.hasNext(); ) {
          Class inv_class = (Class) i.next();
          Cnt cnt = (Cnt) inv_map.get (inv_class);
          debug.fine (" : " + inv_class + ": " + cnt.cnt);
        }
      }

      if (show_invs) {
        for (Iterator j = ppt.views_iterator(); j.hasNext(); ) {
          PptSlice slice = (PptSlice) j.next();
          for (int k = 0; k < slice.invs.size(); k++) {
            Invariant inv = (Invariant) slice.invs.get (k);
            String suppress = "";
            if (inv.getSuppressor() != null)
              suppress = "(suppressed) ";
            String falsify = "";
            if (inv.is_false())
              falsify = "(falsified) ";
            debug.fine (" : " + suppress + falsify + inv.format());
          }
        }
      }

      if (show_tern_slices) {
        for (Iterator j = ppt.views_iterator(); j.hasNext(); ) {
          PptSlice slice = (PptSlice) j.next();
          StringBuffer sb = new StringBuffer();
          for (int k = 0; k < slice.arity(); k++) {
            VarInfo v = slice.var_infos[k];
            sb.append (v.name.name() + "/" + v.equalitySet.getVars().size()
                      + "/" + v.file_rep_type + " ");
          }
          debug.fine (": " + sb.toString() + ": " + slice.invs.size());
        }
      }

    }
  }

  /**
   * print statistics concerning equality sets over the entire set of
   * ppts to the specified logger
   */
  public static void print_equality_stats (Logger debug, PptMap all_ppts) {

    if (!debug.isLoggable (Level.FINE))
      return;
    boolean show_details = true;


    NumberFormat dfmt = NumberFormat.getInstance();
    dfmt.setMaximumFractionDigits (2);
    double equality_set_cnt = 0;
    double vars_cnt = 0;
    double total_sample_cnt = 0;
    Map stats_map = Global.stats_map;

    Stats.dump_header (debug);
    for (Iterator i = all_ppts.pptIterator(); i.hasNext(); ) {
      PptTopLevel ppt = (PptTopLevel) i.next();
      List slist = (List) stats_map.get (ppt);
      if (slist == null)
        continue;
      int sample_cnt = 0;
      int time = 0;
      double avg_equality_cnt = 0;
      double avg_var_cnt = 0;
      double avg_vars_per_equality = 0;
      double avg_inv_cnt = 0;
      int instantiated_inv_cnt = 0;
      int slice_cnt = 0;
      int instantiated_slice_cnt = 0;
      int suppress_cnt = 0;
      long memory= 0;
      if (slist != null) {
        sample_cnt = slist.size();
        total_sample_cnt += sample_cnt;
        for (int j = 0; j < slist.size(); j++) {
          Stats stats = (Stats) slist.get (j);
          avg_equality_cnt += stats.set_cnt;
          avg_var_cnt += stats.var_cnt;
          equality_set_cnt += stats.set_cnt;
          vars_cnt += stats.var_cnt;
          time += stats.time;
          avg_inv_cnt += stats.inv_cnt;
          slice_cnt += stats.slice_cnt;
          instantiated_inv_cnt += stats.instantiated_inv_cnt;
          instantiated_slice_cnt += stats.instantiated_slice_cnt;
          suppress_cnt += stats.suppress_cnt;
          memory += stats.memory;
        }
        avg_equality_cnt = avg_equality_cnt / sample_cnt;
        avg_var_cnt = avg_var_cnt / sample_cnt;
      }
      if (avg_equality_cnt > 0)
        avg_vars_per_equality = avg_var_cnt / avg_equality_cnt;
      debug.fine (ppt.name() + " : " + sample_cnt + " : "
                  + dfmt.format (avg_equality_cnt) + " : "
                  + dfmt.format (avg_var_cnt) + " : "
                  + dfmt.format (avg_vars_per_equality) + " : "
                  + dfmt.format ((double) slice_cnt / sample_cnt) + "/"
                  + dfmt.format ((double) avg_inv_cnt / sample_cnt) + " : "
                  + dfmt.format ((double) instantiated_slice_cnt/sample_cnt) +"/"
                  + dfmt.format ((double) instantiated_inv_cnt/sample_cnt) + ": "
                  + dfmt.format ((double) suppress_cnt/sample_cnt) + ": "
                  + dfmt.format ((double) memory / sample_cnt) + ": "
                  + dfmt.format ((double) time / sample_cnt));
      if (show_details) {
        double avg_time = (double) time / sample_cnt;
        for (int j = 0; j < slist.size(); j++) {
          Stats stats = (Stats) slist.get (j);
          double vars_per_eq = 0;
          if (stats.set_cnt > 0)
            vars_per_eq = (double) stats.var_cnt / stats.set_cnt;
          if ((j == (slist.size() - 1)) || (stats.time > (2 * avg_time)))
            debug.fine (" : " + j + " : "
                        + stats.set_cnt + " : "
                        + stats.var_cnt + " : "
                        + dfmt.format (vars_per_eq) + " : "
                        + stats.slice_cnt + "/"
                        + stats.inv_cnt + " : "
                        + stats.instantiated_slice_cnt + "/"
                        + stats.instantiated_inv_cnt + " : "
                        + stats.suppress_cnt + " : "
                        + stats.memory + ": "
                        + stats.time);
        }
      }
    }
  }

}
