package daikon;

import java.io.*;
import java.util.*;
import utilMDE.*;
import java.util.logging.Logger;
import java.util.logging.Level;

/**
 * A collection of code that configures and assists the dataflow
 * system in Daikon.  See Chapter 7 of Jeremy Nimmer's Masters Thesis
 * for the higher-level design ideas.
 *
 * <p> Almost all of the methods of this class are used only while
 * program point declarations ("decls files") are being processed.
 * The only exception is init_pptslice_po(), which selects a portion
 * of some pre-computed data to store in the PptSlice.
 **/
public final class Dataflow {
  // Nobody should be instantiating a Dataflow.
  private Dataflow() { throw new Error("do not instantiate"); }

  public static final Logger debug = Logger.getLogger("daikon.flow");


  /** Debug tracer for ppt initialization.   **/
  public static final Logger debugInit = Logger.getLogger("daikon.flow.init");

  /** Debug tracer for ppt invariant merging. **/
  public static final Logger debugMerge = Logger.getLogger
                                                ("daikon.flow.merge");

  /**
   * Indicate progress for FileIOProgress.
   **/
  public static String progress = "";

  // Temporary routine, for debugging
  // Will eventually move into daikon.test.DataflowTest
  //
  // java daikon.Dataflow `find /scratch/$USER/tests/daikon-tests/StackAr/ -name '*.decls'`
  //
  public static void main(String[] args)
    throws Exception
  {
    debug.setLevel(LogHelper.FINE);

    String outf = "Dataflow_testing.txt";
    File[] files = new File[args.length];
    for (int i=0; i < args.length; i++) {
      files[i] = new File(args[i]);
    }
    PptMap map = FileIO.read_declaration_files(Arrays.asList(files));
    init_partial_order(map);

    OutputStream out = new FileOutputStream(new File(outf));
    dump_ppts(out, map);
    dump_flow(out, map);
    out.close();
  }


  /**
   * Process all ppts by adding orig and derived variables, relating
   * variables to other program points, and fixing all necessary
   * pre-computed flows.  (Calls init_partial_order(PptTopLevel,
   * PptMap) for each ppt in turn.)
   **/
  public static void init_partial_order(PptMap all_ppts) {

    // If we are doing bottom up as opposed to top down
    if (Daikon.dkconfig_df_bottom_up) {

      // Create combined exit points
      progress = "Creating combined exit points";
      create_combined_exits (all_ppts);

      // Setup splitters.  This must be done after creating the
      // combined exit points and before adding derived variables
      Daikon.setup_splitters(all_ppts);

      // Setup orig variables
      for (Iterator i = all_ppts.ppt_all_iterator(); i.hasNext(); ) {
        PptTopLevel ppt = (PptTopLevel) i.next();
        progress = "Creating orig variables for: " +ppt.ppt_name.toString();
        create_and_relate_orig_vars (ppt, all_ppts);
      }

      // Set up derived variables
      for (Iterator i = all_ppts.ppt_all_iterator(); i.hasNext(); ) {
        PptTopLevel ppt = (PptTopLevel) i.next();
        progress = "Creating derived variables for: " +ppt.ppt_name.toString();
        ppt.create_derived_variables();
      }

      // Relate the global ppt (if any) to each numbered exit
      PptTopLevel global = all_ppts.getGlobal();
      PptTopLevel.global = global;
      if (global != null) {
        for (Iterator i = all_ppts.pptIterator(); i.hasNext(); ) {
          PptTopLevel ppt = (PptTopLevel) i.next();
          progress = "Relating to GLOBAL ppt: " +ppt.ppt_name.toString();
          if (ppt != global)
            ppt.init_global_transforms (global);
        }
      }

      return;
    }

    if (Daikon.use_dataflow_hierarchy) {
      // Create combined exit points
      create_combined_exits (all_ppts);

      // Connect VarInfos and program points via dataflow ordering
      for (Iterator i = all_ppts.pptIterator(); i.hasNext(); ) {
        PptTopLevel ppt = (PptTopLevel) i.next();
        progress = "Initializing partial order for: " + ppt.name();
        //System.out.println ("processing " + ppt.ppt_name + " point = "
        //                    + ppt.ppt_name.getPoint());
        if (ppt.ppt_name.isGlobalPoint())
          continue;
        init_partial_order(ppt, all_ppts);
      }

      // Create or modify the data flow and invariant flow vectors.  We
      // *must* recompute all of them, rather than just the new ones.
      for (Iterator i = all_ppts.pptIterator(); i.hasNext(); ) {
        PptTopLevel item = (PptTopLevel) i.next();
        progress = "Initializing data flow order for: " + item.name();
        create_ppt_dataflow(item);
        create_ppt_invflow(item);
      }
    } else {
      for (Iterator i = all_ppts.pptIterator(); i.hasNext(); ) {
        PptTopLevel ppt = (PptTopLevel) i.next();
        progress = "Initializing simple partial order for: " + ppt.name();
        create_simple_pptflow(ppt);
      }
    }
    progress = null;
  }

  /**
   * Process a set of appts by adding orig and derived variables,
   * relating variables to other program points, and fixing all
   * necessary pre-computed flows.  (Calls
   * init_partial_order(PptTopLevel, PptMap) for each ppt in turn.)
   * @param ppts a Collection of PptTopLevels to init
   **/
  public static void init_partial_order(Collection ppts, PptMap all_ppts) {
    for (Iterator i = ppts.iterator(); i.hasNext(); ) {
      PptTopLevel ppt = (PptTopLevel) i.next();
      init_partial_order(ppt, all_ppts);
    }

    // Create or modify the data flow and invariant flow vectors.  We
    // *must* recompute all of them, rather than just the new ones.
    for (Iterator i = all_ppts.pptIterator(); i.hasNext(); ) {
      PptTopLevel item = (PptTopLevel) i.next();
      create_ppt_dataflow(item);
      create_ppt_invflow(item);
    }
  }

  // Why is the following private?  Because it no longer does
  // create_ppt_dataflow and create_ppt_invflow, which are necessary
  // after the initi process.  Why?  Because it's more efficient to do
  // these after every batch of ppts, since they *must* be redone over
  // all the ppts.
  /**
   * Process the ppt by adding orig and derived variables, relating
   * variables to other program points, and fixing all necessary
   * pre-computed flows.  When relating EXITnn program points, we only
   * relate EXITnn to EXIT, because the transitive closure handles the
   * rest.
   **/
  private static void init_partial_order(PptTopLevel ppt, PptMap all_ppts) {
    if (debugInit.isLoggable(Level.FINE)) {
      debugInit.fine ("Initializing partial order for ppt " + ppt.name());
    }

    // Set up OBJECT and CLASS relationships
    relate_object_procedure_ppts(ppt, all_ppts);
    // Set up orig() declarations and relationships
    create_and_relate_orig_vars(ppt, all_ppts);
    // Set up EXITnn and EXIT relationships
    relate_combined_exits(ppt, all_ppts);
    // Set up OBJECT on arguments relationships
    relate_types_to_object_ppts(ppt, all_ppts);


    if (debugInit.isLoggable(Level.FINE)) {
      for (int i=0; i< ppt.var_infos.length; i++) {
        VarInfo vi = ppt.var_infos[i];
        debugInit.fine ("  Parents for " + vi.name.name() + ":" +
                         vi.po_higher());
      }
    }


    // Set up derived variables
    create_derived_variables(ppt, all_ppts);

  }

  /**
   * For every variable that has the same name in higher and lower,
   * add a link in the po relating them.  See the definitions of lower
   * and higher in VarInfo for their semantics.
   * @see VarInfo.po_higher
   * @see VarInfo.po_lower
   **/
  private static void setup_po_same_name(VarInfo[] lower,
                                         VarInfo[] higher) {
    setup_po_same_name(lower, VarInfoName.IDENTITY_TRANSFORMER,
                       higher, VarInfoName.IDENTITY_TRANSFORMER);
  }

  /**
   * For every variable that has the same name in higher and lower (after transformation),
   * add a link in the po relating them.  See the definitions of lower
   * and higher in VarInfo for their semantics.
   * @see VarInfo.po_higher
   * @see VarInfo.po_lower
   **/
  private static void setup_po_same_name(VarInfo[] lower,
                                         VarInfoName.Transformer lower_xform,
                                         VarInfo[] higher,
                                         VarInfoName.Transformer higher_xform) {
    debugInit.fine ("Setup_po_same_name");
    for (int i=0; i<higher.length; i++) {
      VarInfo higher_vi = higher[i];
      VarInfoName higher_vi_name = higher_xform.transform(higher_vi.name);
      for (int j=0; j<lower.length; j++) {
        VarInfo lower_vi = lower[j];
        VarInfoName lower_vi_name = lower_xform.transform(lower_vi.name);
        if (higher_vi_name == lower_vi_name) { // VarInfoNames are interned
          if (debugInit.isLoggable(Level.FINE)) {
            debugInit.fine ("Lower and higher: " + lower_vi_name.name()
                + " " + higher_vi_name.name());
            debugInit.fine ("Lower and higher ppt: " + lower_vi.ppt.name()
                + " " + higher_vi.ppt.name());

          }

          // Why is this guard needed?  Because there could be
          // transforms where Foo.x gets set up with Foo.x

          if (lower_vi != higher_vi) {
            lower_vi.addHigherPO(higher_vi, static_po_group_nonce);
          }
        }
      }
    }
    static_po_group_nonce++;
  }

  // Used by setup_po_same_name and related to group variables that flow together
  private static int static_po_group_nonce = 0;


  /**
   * Create EXIT program points as needed for EXITnn program points.
   **/
  public static void create_combined_exits(PptMap ppts) {
    List newPpts = new LinkedList();

    for (Iterator i = ppts.pptIterator(); i.hasNext(); ) {
      PptTopLevel ppt = (PptTopLevel) i.next();
    }

    for (Iterator i = ppts.pptIterator(); i.hasNext(); ) {
      PptTopLevel ppt = (PptTopLevel) i.next();
      // skip unless its an EXITnn
      if (! (ppt.ppt_name.isExitPoint() && !ppt.ppt_name.isCombinedExitPoint())) {
        continue;
      }

      PptTopLevel exitnn_ppt = ppt;
      PptName exitnn_name = exitnn_ppt.ppt_name;
      PptName exit_name = ppt.ppt_name.makeExit();
      PptTopLevel exit_ppt = ppts.get(exit_name);

      if (debugInit.isLoggable(Level.FINE))
        debugInit.fine ("create_combined_exits: encounted exit "
                        + exitnn_ppt.name());

      // Create the exit, if necessary
      if (exit_ppt == null) {
        VarInfo[] exit_vars = VarInfo.arrayclone_simple(ppt.var_infos);
        exit_ppt = new PptTopLevel(exit_name.getName(), exit_vars);
        newPpts.add(exit_ppt);
        if (debugInit.isLoggable(Level.FINE))
          debugInit.fine ("create_combined_exits: created exit "
                          + exit_name);
      }
    }

    ppts.addAll (newPpts);
  }


  /**
   * Connect EXITnn program points to EXIT program points.
   * @param ppt The Exitnn program point to relate to its EXIT program point
   * @param ppts The standard program point map
   **/
  public static void relate_combined_exits(PptTopLevel ppt, PptMap ppts) {
    // return unless its an EXITnn
    if (! (ppt.ppt_name.isExitPoint() && !ppt.ppt_name.isCombinedExitPoint()))
      return;

    PptTopLevel exitnn_ppt = ppt;
    PptName exitnn_name = exitnn_ppt.ppt_name;
    PptName exit_name = ppt.ppt_name.makeExit();
    PptTopLevel exit_ppt = ppts.get(exit_name);

    Assert.assertTrue (exit_ppt != null);

    // Set up the PO between EXIT and EXITnn
    setup_po_same_name(exitnn_ppt.var_infos, exit_ppt.var_infos);
  }

  /**
   * Set up the "controlling program point" partial ordering on
   * VarInfos using the OBJECT, CLASS, ENTER-EXIT program point
   * naming.
   **/
  private static void relate_object_procedure_ppts(PptTopLevel ppt, PptMap ppts) {
    if (debugInit.isLoggable(Level.FINE)) {
      debugInit.fine ("Relating object and procedure ppts for " + ppt.name());
    }
    PptName ppt_name = ppt.ppt_name;
    // Find the ppt that controls this one.
    // CLASS controls OBJECT, and OBJECT controls ENTER/EXIT.
    PptTopLevel controlling_ppt = null;
    if (ppt_name.isObjectInstanceSynthetic()) {
      debugInit.fine ("This is an OBJECT ppt");
      controlling_ppt = ppts.get(ppt_name.makeClassStatic());
    } else {
      boolean enter = ppt_name.isEnterPoint();
      boolean ctor = ppt_name.isConstructor();
      boolean exit = ppt_name.isCombinedExitPoint();
      if ((enter && !ctor) || exit) {
        // TODO: also require that this is a public method (?)
        controlling_ppt = ppts.get(ppt_name.makeObject());
        if (controlling_ppt == null) {
          // If we didn't find :::OBJECT, fall back to :::CLASS
          controlling_ppt = ppts.get(ppt_name.makeClassStatic());
        }
      }
    }
    // Create VarInfo relations with the controller when names match.


    if (controlling_ppt != null) {
      if (debugInit.isLoggable(Level.FINE)) {
        debugInit.fine ("Controlling ppt is " + controlling_ppt.name());
      }
      setup_po_same_name(ppt.var_infos, controlling_ppt.var_infos);
    } else {
    if (debugInit.isLoggable(Level.FINE)) {
      debugInit.fine ("Controlling ppt is null");
    }
    }
  }

  /**
   * Add orig() variables to the given EXIT/EXITnn point, and relate
   * them to the corresponding ENTER point.  Does nothing if exit_ppt
   * is not an EXIT/EXITnn.  Does not relate if point is EXITnn.
   **/
  private static void create_and_relate_orig_vars(PptTopLevel exit_ppt, PptMap ppts) {
    if (! exit_ppt.ppt_name.isExitPoint()) {
      return;
    }

    if (debugInit.isLoggable(Level.FINE)) {
      debugInit.fine ("Doing create and relate orig vars for: " + exit_ppt.name());
    }

    PptTopLevel entry_ppt = ppts.get(exit_ppt.ppt_name.makeEnter());
    Assert.assertTrue(entry_ppt != null, exit_ppt.name());

    // comb_exit_ppt may be same as exit_ppt if exit_ppt is EXIT
    PptTopLevel comb_exit_ppt = ppts.get(exit_ppt.ppt_name.makeExit());
    // Add "orig(...)" (prestate) variables to the program point.
    // Don't bother to include the constants.  Walk through
    // entry_ppt's vars.  For each non-constant, put it on the
    // new_vis worklist after fixing its comparability information.
    exit_ppt.num_orig_vars = entry_ppt.num_tracevars;
    VarInfo[] new_vis = new VarInfo[exit_ppt.num_orig_vars];
    {
      VarInfo[] entry_ppt_vis = entry_ppt.var_infos;
      int new_vis_index = 0;
      for (int k = 0; k < entry_ppt.num_declvars; k++) {
        VarInfo vi = entry_ppt_vis[k];
        Assert.assertTrue(!vi.isDerived(), "Derived when making orig(): " + vi.name);
        if (vi.isStaticConstant())
          continue;
        VarInfo origvar = VarInfo.origVarInfo(vi);
        // Fix comparability
        VarInfo postvar = exit_ppt.findVar(vi.name);
        Assert.assertTrue(postvar != null, "Exit not superset of entry: "  + vi.name);
        origvar.comparability = postvar.comparability.makeAlias(origvar.name);
        // Setup PO; relate orig(...) on EXIT to ... on ENTER But only
        // if it's a combined exit point, not for EXITnn, because for
        // EXITnn we let transitive closure handle the relation.
        if (!Daikon.dkconfig_df_bottom_up &&
            exit_ppt.ppt_name.isCombinedExitPoint()) {
          origvar.addHigherPO(vi, static_po_group_nonce);
        }
        // Add to new_vis
        new_vis[new_vis_index] = origvar;
        new_vis_index++;
      }
      Assert.assertTrue(new_vis_index == exit_ppt.num_orig_vars);
      static_po_group_nonce++; // advance once per (EXIT#) program point
    }
    exit_ppt.addVarInfos(new_vis);
  }

  /**
   * For all variables that have no parent, and whose type we have an
   * OBJECT ppt for, add a partial order relation due to types.
   * Because of the "have no parent" constraint, this step must be the
   * last one performed on a program point being related.
   **/
  private static void relate_types_to_object_ppts(PptTopLevel ppt, PptMap ppts) {
    if (debugInit.isLoggable(Level.FINE)) {
      debugInit.fine ("Doing relate types to objects: " + ppt.name());
    }


    // All expresions with no parent
    List orphans = new ArrayList(); // [VarInfo]
    // Subset of orphans which we have an OBJECT ppt for
    Map known = new HashMap(); // [VarInfo -> PptTopLevel]
    // Search and fill the "orphans" and "known" lists
    VarInfo[] vis = ppt.var_infos;
    for (int i=0; i<vis.length; i++) {
      VarInfo vi = vis[i];
      if (debugInit.isLoggable(Level.FINE)) {
        debugInit.fine ("Processing VarInfo: " + vi.name.name());
      }

      if (vi.name.equals(VarInfoName.THIS)) {
        debugInit.fine ("Skipping because name is 'this': " + vi.name.name());
        continue;
      }

      // Arguments are the things with no controller yet
      if (vi.po_higher().size() == 0) {
        debugInit.fine ("  which is an orphan");
        orphans.add(vi);
        if (! vi.type.isPseudoArray()) {
          PptName objname = new PptName(vi.type.base(), // class
                                        null, // method
                                        FileIO.object_suffix // point
                                        );
          Assert.assertTrue(objname.isObjectInstanceSynthetic());
          debugInit.fine ("Looking for object ppt: " + objname);
          PptTopLevel object_ppt = ppts.get(objname);
          if (object_ppt != null) {
            debugInit.fine ("  whose type is known (success)");
            known.put(vi, object_ppt);
          } else {
            debugInit.fine ("  but whose type is not known");
            // TODO: Note that orphan has no relatives, for later hook?
          }
        } else {
          debugInit.fine ("  but type won't be known because it's a pseudoarray");
        }
      }
    }
    // For each known-type variable, substitute its name for
    // 'this' in the OBJECT ppt and see if we get any expression
    // matches with other orphaned variables.  Note that only the
    // fields of the object (eg, this.x, this.y) and not the object
    // itself (eg, this) are substituted in this fashion.
    //
    // While it could be argued that a pointer to an object of type T
    // and the this pointer in an object of type T are analogous, they are
    // really not the same.  The pointer is a reference to the object while
    // 'this' is really the object itself.  The relationship is also not
    // intuitive when looking at the invariants.  For example, assume that
    // every reference to T at all ppts was not null.  This invariant would
    // print as 'this != null.'  The invariant is both confusing (since
    // in a normal context 'this' can never be null) and it is not obvious
    // that it implies that all references to the object are not NULL.
    //
    // So for example, if we have a and a.x in a lower ppt B, where a
    // is of type A, we relate a.x to this.x in the ppt for A.  We do
    // this by using a transformer that replaces "this" with a and
    // checks for matches between A:::a and B:::a.  This transformer
    // explicitly rejects variables whose name is exactly 'this'

    if (debugInit.isLoggable(Level.FINE)) {
      String orphan_str = "";
      for (Iterator ii = orphans.iterator(); ii.hasNext(); )
        orphan_str += ((VarInfo)ii.next()).name.name() + " ";
      String known_str = "";
      for (Iterator it = known.keySet().iterator(); it.hasNext(); )
        known_str += ((VarInfo) it.next()).name.name() + " ";
      debugInit.fine ("Entering second stage, orphans: " + orphan_str
                        + " known = " + known_str);
      }

    VarInfo[] orphans_array = (VarInfo[]) orphans.toArray(new VarInfo[orphans.size()]);
    for (Iterator it = known.keySet().iterator(); it.hasNext(); ) {
      final VarInfo known_vi = (VarInfo) it.next();
      if (debugInit.isLoggable(Level.FINE)) {
        debugInit.fine ("  known vi is: " + known_vi.name.name());
      }

      PptTopLevel object_ppt = (PptTopLevel) known.get(known_vi);
      setup_po_same_name(orphans_array, // lower
                         VarInfoName.IDENTITY_TRANSFORMER,
                         object_ppt.var_infos, // higher
                         // but with known_vi.name in for "this"
                         new VarInfoName.Transformer() {
                             public VarInfoName transform(VarInfoName v) {
                               if (v == VarInfoName.THIS)
                                 return (v);
                               else
                                 return v.replaceAll(VarInfoName.THIS,
                                                     known_vi.name);
                             }
                           }
                         );
    }

  }

  /**
   * Create all of the derived variables for all program points.
   **/
  private static void create_derived_variables(PptTopLevel ppt, PptMap all_ppts) {
    int first_new = ppt.var_infos.length;
    if (debugInit.isLoggable(Level.FINE)) {
      debugInit.fine ("  Creating derived vars for " + ppt.name());
    }

    ppt.create_derived_variables();
    if (debugInit.isLoggable(Level.FINE)) {
      debugInit.fine ("  Created derived vars for " + ppt.name());
    }

    relate_derived_variables(ppt, first_new, ppt.var_infos.length);

    if (debugInit.isLoggable(Level.FINE)) {
      debugInit.fine ("  Related derived vars for " + ppt.name());
    }
  }

  /**
   * Modify ppt.var_infos in the range [lower..upper) to have the
   * proper partial ordering (drawn from the partial ordering of their
   * base(s)).
   **/
  public static void relate_derived_variables(PptTopLevel ppt,
                                              int lower,
                                              int upper) {
    debug.fine ("relate_derived_variables on " + ppt.name());

    // For all immediately higher groups of variables
    PptsAndInts flow = compute_ppt_flow(ppt,
                                        false, // one step only
                                        true   // higher
                                        );
    int size = flow.ppts.length - 1; // -1 because don't want self
    debug.fine ("size = " + size);
    for (int i = 0; i < size; i++) {
      PptTopLevel flow_ppt = flow.ppts[i];
      int[] flow_ints = flow.ints[i];

      // Determine the first nonce along the path
      int nonce = -1;
    nonce_search:
      for (int search = 0; search < flow_ints.length; search++) {
        if (flow_ints[search] != -1) {
          VarInfo search_vi = ppt.var_infos[search];
          int count = 0;
          for (Iterator j = search_vi.po_higher().iterator(); j.hasNext(); ) {
            VarInfo up_vi = (VarInfo) j.next();
            if (up_vi.ppt == flow_ppt) {
              nonce = search_vi.po_higher_nonce()[count];
              break nonce_search;
            }
            count++;
          }
          throw new IllegalStateException("Path was not 1-length");
        }
      }
      if (nonce == -1) throw new IllegalStateException("Mapless path");

      debug.fine (" nonce = " + nonce);

      // For all derived variables to relate
    forall_derived_vars:
      for (int j = lower; j < upper; j++) {
        VarInfo vi = ppt.var_infos[j];
        Assert.assertTrue(vi.isDerived());
        debug.fine ("  vi = " + vi.name);
        // Obtain the bases of derived varable
        VarInfo[] bases = vi.derived.getBases();
        // See where this path maps them
        VarInfo[] basemap = new VarInfo[bases.length];
        for (int k = 0; k < bases.length; k++) {
          VarInfo base = bases[k];
          int newindex = flow_ints[base.varinfo_index];
          if (newindex == -1) {
            debug.fine ("  vars not mapped in path: vi = " + vi.name
                        + "; bases = " + Ppt.varNames(bases));
            continue forall_derived_vars;
          }
          basemap[k] = flow_ppt.var_infos[newindex];
        }
        // Find the derived varaible of the same class over the related bases
        VarInfo vi_higher = null;
        for (int k = 0; k < flow_ppt.var_infos.length; k++) {
          VarInfo maybe = flow_ppt.var_infos[k];
          if (maybe.derived == null)
            continue;
          if (vi.derived.isSameFormula(maybe.derived)) {
            VarInfo[] maybe_bases = maybe.derived.getBases();
            if (Arrays.equals(basemap, maybe_bases)) {
              vi_higher = maybe;
              break;
            }
          }
        }
        if (vi_higher == null) {
          debug.fine ("  No match found for " + vi.derived.getClass()
                      + " using " + Ppt.varNames(basemap));
          continue forall_derived_vars;
        }
        // Create the new relation
        vi.addHigherPO(vi_higher, nonce);
      }
    }
  }

  /** Our own record type for programming ease. */
  static final class VarAndSource {
    public final VarInfo var; // variable at the head of a search path
    public final int source;  // var's source index in var_infos from the ppt
                              // where the flow computation originated.
    public VarAndSource(VarInfo _var, int _source) {
      var = _var;
      source = _source;
    }
    public String toString() {
      return (var.name.name() + " [" + source + "/" + var.varinfo_index + "]");
    }
  }

  /**
   * Record type for parallel arrays of PptTopLevel and int[].
   **/
  public static final class PptsAndInts {
    public final PptTopLevel[] ppts;
    public final int[][] ints;
    public PptsAndInts(PptTopLevel[] ppts, int[][] ints) {
      Assert.assertTrue(ppts.length == ints.length);
      this.ppts = ppts;
      this.ints = ints;
    }
    /**
     * @return new records containing the first length pairs in this
     * (elements of the result are aliased with elements of the arguments!)
     **/
    public PptsAndInts makeSubArray(int length) {
      PptTopLevel[] _ppts = new PptTopLevel[length];
      int[][] _ints = new int[length][];
      System.arraycopy(ppts, 0, _ppts, 0, length);
      System.arraycopy(ints, 0, _ints, 0, length);
      return new PptsAndInts(_ppts, _ints);
    }
  }

  /**
   * Create the dataflow "injection vectors" for PptTopLevels that
   * receive samples.  An "injection vector" is a list of all program
   * points that a samlpe will flow to, and the mapping between the
   * sample's variables and each flowed-to program point's variables.
   * Must be done after VarInfo partial ordering relations have been
   * set up.
   **/
  private static void create_ppt_dataflow(PptTopLevel ppt) {
    // Points that receive samples (currently just EXIT/EXITnn)
    boolean receives_samples =
      ppt.ppt_name.isExitPoint() || ppt.ppt_name.isEnterPoint();

    if (receives_samples) {
      PptsAndInts rec = compute_ppt_flow(ppt,
                                         true, // full paths
                                         true  // higher
                                         );
      // Store result into ppt
      ppt.dataflow_ppts = rec.ppts;
      ppt.dataflow_transforms = rec.ints;
    }
  }

  /**
   * Create the invariant flow injection vectors for PptTopLevels.  An
   * "injection vector" is a list of all program points that
   * invariants from <tt>ppt</tt> might flow to, and the mapping
   * between the <tt>ppt</tt>'s variables and each flowed-to program
   * point's variables.  Must be done after VarInfo partial ordering
   * relations have been set up.
   **/
  private static void create_ppt_invflow(PptTopLevel ppt) {
    {
      PptsAndInts rec = compute_ppt_flow(ppt,
                                         false, // one-step paths
                                         false  // lower
                                         );
      // Remove last element (don't flow invariant to same ppt)
      rec = rec.makeSubArray(rec.ppts.length - 1);
      // Store result into ppt
      ppt.invflow_ppts = rec.ppts;
      ppt.invflow_transforms = rec.ints;
    }
  }

  /**
   * Create the simple data and invariant flow that represents each
   * program point only flowing to itself.  Used for flat hierarchies
   * when Daikon.use_dataflow_hierarchy is set to false.
   **/
  private static void create_simple_pptflow(PptTopLevel ppt) {
    Assert.assertTrue (!Daikon.use_dataflow_hierarchy);

    ppt.dataflow_ppts = new PptTopLevel[] {ppt};
    // Identity transform
    int[] transform = new int[ppt.var_infos.length];
    for (int i = 0; i < transform.length; i++) {
      transform[i] = i;
    }
    ppt.dataflow_transforms = new int[][] {transform};

    ppt.invflow_ppts = new PptTopLevel[0];
    ppt.invflow_transforms = new int[0][];
  }

  /**
   * Compute a flow path for one ppt.  Can go higher or lower, and
   * either just one step up or all paths to completion.
   **/
  public static PptsAndInts compute_ppt_flow(PptTopLevel ppt,
                                             boolean all_steps,
                                             boolean higher) {
    // Create the worklist 'first' to contain all variables in ppt.
    // That way, the result will be the flow from the whole program
    // point.
    int nvis = ppt.var_infos.length;
    List first = new ArrayList(nvis);
    for (int i = 0; i < nvis; i++) {
      first.add(new VarAndSource(ppt.var_infos[i], i));
    }

    return compute_ppt_flow(ppt, first, all_steps, higher);
  }


  /**
   * Compute a flow path for a portion of one ppt.  Can go higher or
   * lower, and either just one step up or all paths to completion.
   *
   * @param start subset of ppt's VarInfos to use as the starting
   * point for the flow computation.
   **/
  public static PptsAndInts compute_ppt_flow(PptTopLevel ppt,
                                             VarInfo[] start,
                                             boolean all_steps,
                                             boolean higher) {
    // We could assert that start's VarInfos are from ppt.

    // Construct the worklist 'first' from the argument 'start'.
    List first = new ArrayList(start.length);
    for (int i = 0; i < start.length; i++) {
      first.add(new VarAndSource(start[i], start[i].varinfo_index));
    }

    return compute_ppt_flow(ppt, first, all_steps, higher);
  }

  /**
   * Compute a flow path for a portion of one ppt.  Can go higher or
   * lower, and either just one step up or all paths to completion.
   *
   * @param start subset of ppt's VarInfos to use as the starting
   * point for the flow computation.
   **/
  private static PptsAndInts compute_ppt_flow(PptTopLevel ppt,
                                              List start, // [VarAndSource]
                                              boolean all_steps,
                                              boolean higher) {
    if (debugInit.isLoggable(Level.FINE)) {
      debugInit.fine ("compute_ppt_flow for " + ppt.name() + " all_steps = "
                       + all_steps + " higher = " + higher);
      String vars = "";
      for (Iterator ii = start.iterator(); ii.hasNext(); ) {
        VarAndSource vs = (VarAndSource) ii.next();
        vars += vs.var.name.name() + "[" + vs.source + "] ";
      }
      debugInit.fine ("vars = " + vars);
    }

    // We could assert that start's VarInfos are from ppt, and that
    // the VarAndSources have right the varinfo_index for them.

    if (start.size() == 0) {
      // Return the trivial result: one self-step with an empty
      // projection.
      return new PptsAndInts
        (new PptTopLevel[] { ppt },
         new int[][] { new int[0] } );
    }

    // These two lists collect the result that will be returned
    List dataflow_ppts = new ArrayList(); // of type PptTopLevel
    List dataflow_transforms = new ArrayList(); // of type int[nvis]
    int nvis = ppt.var_infos.length;

    {
      // Our worklist is the heads of paths flowing up from "ppt".
      // (The worklist elements are actually VarAndSource objects.)
      // We store the vars which specify the head and the original
      // indices in ppt that flow up to the head.
      LinkedList worklist = new LinkedList(); // element type is List[VarAndSource]
      worklist.add(start);

      // While worklist is non-empty, process the first element
      while (! worklist.isEmpty()) {
        List head = (List) worklist.removeFirst();

        // Use null element to signal a gap, and thus completion.
        // A null element appears only if all_steps is false.
        if (head == null) break;

        // Add a flow from ppt to head
        Assert.assertTrue(head.size() >= 1);
        PptTopLevel flow_ppt = ((VarAndSource) head.get(0)).var.ppt;
        int[] flow_transform = new int[nvis];
        Arrays.fill(flow_transform, -1);
        for (Iterator i = head.iterator(); i.hasNext(); ) {
          VarAndSource vs = (VarAndSource) i.next();
          Assert.assertTrue(vs.var.ppt == flow_ppt); // all flow to the same ppt
          Assert.assertTrue(flow_transform[vs.source] == -1); // with no overlap
          flow_transform[vs.source] = vs.var.varinfo_index;
        }
        dataflow_ppts.add(flow_ppt);
        dataflow_transforms.add(flow_transform);

        // Debug print flow_ppt, flow_ppt vars, transforms, and head list
        if (debugInit.isLoggable(Level.FINE)) {
          debugInit.fine ("  Add flow ppt: " + flow_ppt.name());
          String vars = "";
          for (int ii = 0; ii < flow_ppt.var_infos.length; ii++)
            vars += flow_ppt.var_infos[ii].name.name() + " ";
          debugInit.fine ("  With vars: " + vars);
          debugInit.fine ("  Add transforms: "
                            + ArraysMDE.toString (flow_transform));
          vars = "";
          debugInit.fine ("  head var list: " + head);
        }

        // Extend head using all higher (or lower) nonces
        Map nonce_to_vars = new HashMap(); // [Integer -> List[VarAndSource]]
        for (Iterator i = head.iterator(); i.hasNext(); ) {
          VarAndSource vs = (VarAndSource) i.next();
          List higher_vis = higher ? vs.var.po_higher() : vs.var.po_lower();
          int[] higher_nonces = higher ? vs.var.po_higher_nonce() : vs.var.po_lower_nonce();
          for (int nonce_idx = 0; nonce_idx < higher_vis.size(); nonce_idx++) {
            VarInfo higher_vi = (VarInfo) higher_vis.get(nonce_idx);
            Integer higher_nonce = new Integer(higher_nonces[nonce_idx]);
            if (debugInit.isLoggable(Level.FINE)) {
              debugInit.fine ("  var " + vs + " po " + higher_vi.name.name()
                               + "[nonce = " + higher_nonce + "]");
            }
            // newpath has type List[VarAndSource].
            List newpath = (List) nonce_to_vars.get(higher_nonce);
            if (newpath == null) {
              newpath = new ArrayList();
              nonce_to_vars.put(higher_nonce, newpath);
            }
            newpath.add(new VarAndSource(higher_vi, vs.source));
          }
        }
        for (Iterator i = nonce_to_vars.keySet().iterator(); i.hasNext(); ) {
          Integer nonce = (Integer) i.next();
          List newpath = (List) nonce_to_vars.get(nonce);
          Assert.assertTrue(newpath != null);
          worklist.add(newpath);
          if (debugInit.isLoggable(Level.FINE))
            debugInit.fine ("  Added new worklist: " + newpath);
        }

        // Put in a null to signal that all of the fields of the original
        // worklist element have been added to the worklist.
        if (! all_steps) {
          worklist.add(null);
        }
      }

      // We want to flow from the highest point to the lowest
      Collections.reverse(dataflow_ppts);
      Collections.reverse(dataflow_transforms);
    }

    // Convert results to arrays
    PptTopLevel[] ppts_array = (PptTopLevel[])
      dataflow_ppts.toArray(new PptTopLevel[dataflow_ppts.size()]);
    int[][] transforms_array = (int[][])
      dataflow_transforms.toArray(new int[dataflow_ppts.size()][]);
    // Intern to save memory (I hope?)
    for (int j=0; j < transforms_array.length; j++) {
      transforms_array[j] = Intern.intern(transforms_array[j]);
    }

    return new PptsAndInts(ppts_array, transforms_array);
  }

  /**
   * Use slice.var_infos[].po_lower to initialize slice.po_lower.  The
   * result is slices H such that:
   * <ul>
   * <li> H.arity == this.arity
   * <li> exist i,j s.t. this.var_infos[i] :[ H.var_infos[j]  (lower in po)
   * <li> Not exist h1 in H, h2 in H s.t. path to h1 is prefix of path to h2  (minimality)
   * <li> Nonces are respected.  That is, for each slice in H that
   * results from this initialization, the path from here to there
   * follows the same nonces on each step.
   * </ul>
   *
   * <p> This method is called whenever a PptSlice is instantiated.
   * It doesn't actually compute the result directly, but instead
   * selects a portion of the pre-computed PptTopLevel.invflow_ppts
   * field to store in the PptSlice.
   **/
  public static void init_pptslice_po(PptSlice slice) {
    // Don't setup top-down data structures if we are doing bottom up
    if (Daikon.dkconfig_df_bottom_up)
      return;

    // For all immediately lower groups of variables
    PptTopLevel ppt = slice.parent;
    int size = ppt.invflow_ppts.length;
  for_all_paths:
    for (int i = 0; i < size; i++) {
      PptTopLevel adj_ppt = ppt.invflow_ppts[i];
      int[] flow_ints = ppt.invflow_transforms[i];

      // Add to slice's partial order
      VarInfo[] vis_adj = new VarInfo[slice.arity()];
      for (int j = 0; j < slice.arity(); j++) {
        int slice_index = slice.var_infos[j].varinfo_index;
        int adj_index = flow_ints[slice_index];
        if (adj_index == -1) {
          // slice doesn't flow here
          continue for_all_paths;
        }
        vis_adj[j] = adj_ppt.var_infos[adj_index];
        Assert.assertTrue (vis_adj[j].compatible(adj_ppt.var_infos[adj_index]));
      }
      slice.addToOnePO(adj_ppt, vis_adj);
    }

    // Handle the PptConditionals // XXX untested code
    if (false) {
      for (Iterator i = slice.parent.cond_iterator(); i.hasNext(); ) {
        PptConditional adj_ppt = (PptConditional) i.next();
        VarInfo[] vis_adj = new VarInfo[slice.arity()];
        for (int j = 0; j < slice.arity(); j++) {
          int slice_index = slice.var_infos[j].varinfo_index;
          int adj_index = slice_index;
          vis_adj[j] = adj_ppt.var_infos[adj_index];
        }
      }
    }
  }

  private static final String HR =
    "===========================================================================";

  /**
   * @param outstream the stream to send output to
   * @param ppts the program points to dump
   *
   * Writes a textual (debugging) form of the program point hierarchy
   * and relationships to the given stream.
   **/
  public static void dump_ppts(OutputStream outstream,
                               PptMap ppts) {
    Map po_lower_stats = new HashMap();
    Map po_higher_stats = new HashMap();

    PrintStream out = new PrintStream(outstream);
    out.println(HR);

    for (Iterator iter = ppts.pptIterator(); iter.hasNext(); ) {
      PptTopLevel ppt = (PptTopLevel) iter.next();

      out.println(ppt.name());
      for (int i=0; i < ppt.var_infos.length; i++) {
        VarInfo vi = ppt.var_infos[i];
        out.println(vi.name.name());
        out.println("  Declared type: " + vi.type.format() );
        out.println("  File rep type: " + vi.file_rep_type.format() );
        out.println("  Internal type: " + vi.rep_type.format() );
        out.println("  Comparability: " + vi.comparability );
        out.println("  PO higher:");
        int count = 0;
        for (Iterator vs = vi.po_higher().iterator(); vs.hasNext(); ) {
          VarInfo v = (VarInfo) vs.next();
          int nonce = vi.po_higher_nonce()[count++];
          out.println("    " + nonce + ": " + v.name.name() + " in " + v.ppt.name());
        }
        { // stats
          Integer _count = new Integer(count);
          Integer _n = (Integer) po_higher_stats.get(_count);
          int n;
          if (_n == null) {
            n = 0;
          } else {
            n = _n.intValue();
          }
          po_higher_stats.put(_count, new Integer(n+1));
        }
        out.println("  PO lower:");
        count = 0;
        for (Iterator vs = vi.po_lower().iterator(); vs.hasNext(); ) {
          VarInfo v = (VarInfo) vs.next();
          int nonce = vi.po_lower_nonce()[count++];
          out.println("    " + nonce + ": " + v.name.name() + " in " + v.ppt.name());
        }
        { // stats
          Integer _count = new Integer(count);
          Integer _n = (Integer) po_lower_stats.get(_count);
          int n;
          if (_n == null) {
            n = 0;
          } else {
            n = _n.intValue();
          }
          po_lower_stats.put(_count, new Integer(n+1));
        }
      }
      out.println();

    }

    out.println(HR);
    out.println("Statistics:");
    out.println("  PO higher frequencies:");
    for (int i = 0; i < 1000; i++) {
      Integer count = (Integer) po_higher_stats.get(new Integer(i));
      if (count != null) {
        out.println("    " + i + " : " + count);
      }
    }
    out.println("  PO lower frequencies:");
    for (int i = 0; i < 1000; i++) {
      Integer count = (Integer) po_lower_stats.get(new Integer(i));
      if (count != null) {
        out.println("    " + i + " : " + count);
      }
    }
    out.println();
    out.println(HR);
  }

  /**
   * @param outstream the stream to send output to
   * @param ppts the program points to dump
   *
   * Writes a textual (debugging) form of the dataflow
   **/
  public static void dump_flow(OutputStream outstream,
                               PptMap ppts) {
    PrintStream out = new PrintStream(outstream);

    for (Iterator iter = ppts.pptIterator(); iter.hasNext(); ) {
      PptTopLevel ppt = (PptTopLevel) iter.next();

      if (ppt.dataflow_ppts != null) {
        out.println(ppt.name());
        for (int j = 0; j < ppt.dataflow_ppts.length; j++) {
          PptTopLevel df_ppt = ppt.dataflow_ppts[j];
          int[] df_ints = ppt.dataflow_transforms[j];
          out.println("    To " + df_ppt.name() + ":");
          for (int k = 0; k < ppt.var_infos.length; k++) {
            int map = df_ints[k];
            if (map != -1) {
              out.println("      " + ppt.var_infos[k].name.name()
                          + " -> " + df_ppt.var_infos[map].name.name());
            }
          }
        }
        out.println();
      }

    }
  }

  /**
   * Creates upper program points by merging together the invariants
   * from all of the lower points.
   */
  public static void createUpperPpts (PptMap all_ppts) {

    // Process each ppt that doesn't have a parent
    for (Iterator i = all_ppts.pptIterator(); i.hasNext(); ) {
      PptTopLevel ppt = (PptTopLevel) i.next();
      if (ppt.parents.size() == 0) {
        ppt.mergeInvs();
      }
    }
  }

}
