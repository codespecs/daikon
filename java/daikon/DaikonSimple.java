package daikon;
import daikon.*;
import daikon.inv.*;
import daikon.inv.binary.twoScalar.*;
import daikon.inv.ternary.threeScalar.*;
import daikon.suppress.*;
import utilMDE.*;
import java.util.*;
import java.io.*;
import gnu.getopt.*;
import java.util.logging.Logger;
import java.util.logging.Level;

/**
 * DaikonSimple reads a declaration file and trace file and outputs a list
 * of likely invariants using the simple incremental algorithm.
 */
public class DaikonSimple {

  //printing usage
  public static final String lineSep = Global.lineSep;

  //logging information
  public static final Logger debug = Logger.getLogger("daikon.DaikonSimple");
  public static final Logger debug_detail =
    Logger.getLogger("daikon.DaikonSimple.Detail");

  //inv file for storing the invariants in serialized form
  public static File inv_file = null;

  private static String usage =
    UtilMDE.join(
      new String[] {
        "",
        "Usage: java daikon.PrintInvariants [OPTION]... <decls_file> "
          + "<dtrace_file>",
        "  -h, --" + Daikon.help_SWITCH,
        "      Display this usage message",
        "  -o, <inv_file> ",
        "      Writes output to <inv_file>",
        "  --" + Daikon.debugAll_SWITCH,
        "      Turns on all debug flags (voluminous output)",
        "  --" + Daikon.debug_SWITCH + " logger",
        "      Turns on the specified debug logger",
        "  --" + Daikon.track_SWITCH + " class<var1,var2,var3>@ppt",
        "      Print debug info on the specified invariant class, vars, and ppt",
        },
      lineSep);


  public static void main(final String[] args)
    throws IOException, FileNotFoundException {
    try {
      mainHelper(args);
    } catch (Daikon.TerminationMessage e) {
      System.err.println(e.getMessage());
      System.exit(1);
    }
    // Any exception other than Daikon.TerminationMessage gets propagated.
    // This simplifies debugging by showing the stack trace.
  }

  /**
   * This does the work of main, but it never calls System.exit, so it
   * is appropriate to be called progrmmatically.
   * Termination of the program with a message to the user is indicated by
   * throwing Daikon.TerminationMessage.
   * @see #main(String[])
   * @see daikon.Daikon.TerminationMessage
   **/
  public static void mainHelper(final String[] args)
    throws IOException, FileNotFoundException {

    //set up logs
    daikon.LogHelper.setupLogs(daikon.LogHelper.INFO);

    //no optimizations used in the simple incremental algorithm
    Daikon.use_equality_optimization = false;
    Daikon.dkconfig_use_dynamic_constant_optimization = false;
    Daikon.suppress_implied_controlled_invariants = false;
    NIS.dkconfig_enabled = false;

    // Create the list of all invariant types
    Daikon.setup_proto_invs();

    // Read command line options
    Set[] files = read_options(args);
    Assert.assertTrue(files.length == 4);
    Set decls_files = files[0]; // [File]
    Set dtrace_files = files[1]; // [File]
    Set spinfo_files = files[2]; // [File]
    Set map_files = files[3]; // [File]
    if ((decls_files.size() == 0) && (dtrace_files.size() == 0)) {
      throw new Daikon.TerminationMessage("No .decls or .dtrace files specified");
    }

    // Load declarations

    PptMap all_ppts = FileIO.read_declaration_files(decls_files);

    //  adding orig and derived variables
    init_partial_order(all_ppts);
    if (debug.isLoggable(Level.FINE)) {
      debug.fine("Partial order initialized");
    }

    // Read and process the data trace files
    SimpleProcessor processor = new SimpleProcessor();
    FileIO.read_data_trace_files(dtrace_files, all_ppts, processor);

    Iterator t = all_ppts.pptIterator();

    //System.exit(0);
    //    print out the invariants for each program point (sort first)
    t = all_ppts.pptIterator();

    while (t.hasNext()) {
      PptTopLevel ppt = (PptTopLevel) t.next();

      if (ppt.num_samples() != 0) {
        List invs =
          PrintInvariants.sort_invariant_list(ppt.invariants_vector());
        Iterator i = invs.iterator();

        if (Daikon.dkconfig_quiet) {
          System.out.println(
            "====================================================");
          System.out.println(ppt.name());
        } else {
          System.out.println(
            "===================================================+");
          System.out.println(ppt.name() + " +");
        }
        System.out.println(ppt.num_samples());
        while (i.hasNext()) {
          Invariant x = (Invariant) i.next();
          VarInfo[] vars = x.ppt.var_infos;

          //this is the most non-intrusive way to filter out the invs
          //filter out reflexive invariants in the binary invs
          if (!((x.ppt instanceof PptSlice2) && vars[0] == vars[1])) {

            //filter out the reflexive and partially reflexive invs in the
            //ternary slices
            if (!((x.ppt instanceof PptSlice3)
              && (vars[0] == vars[1]
                || vars[1] == vars[2]
                || vars[0] == vars[2]))) {
              if (x.ppt.num_values() != 0) {
                if (x.isActive()) {

                  System.out.println(x.getClass());
                  System.out.println(x);
                }
              }
            }
          }
        }
      }
    }

    //
    // Write serialized output
    // Unresolved problem: the current implementation of PrintInvariants, taking
    //in an inv file will guard the invariants before printing them out but this problem
    //can be resolved by turning the configuration on (Daikon.dkconfig_noInvariantGuarding)
    //however, this does not solve the problem that PrintInvariants uses invariant filtering
    //so the output of DaikonSimple and PrinvInvariants(inv file) will still not be the same
    //is it useful to add another configuration?

    if (inv_file != null) {
      try {
        FileIO.write_serialized_pptmap(all_ppts, inv_file);
      } catch (IOException e) {
        throw new RuntimeException(
          "Error while writing .inv file '" + inv_file + "': " + e.toString());
      }
    }

    // finished; return (and end the program)

    //using print_invariants will add invariant filtering
    //  PrintInvariants.print_invariants(all_ppts);
  }

  ///////////////////////////////////////////////////////////////////////////
  // Read in the command line options
  // Return an array of {decls, dtrace, spinfo, map} files; each array
  // element is a set.
  private static Set[] read_options(String args[])
    throws FileNotFoundException {
    Set decl_files = new HashSet();
    Set dtrace_files = new HashSet();
    Set spinfo_files = new HashSet();
    Set map_files = new HashSet();
    LongOpt[] longopts =
      new LongOpt[] {
        new LongOpt(
          Daikon.config_option_SWITCH,
          LongOpt.REQUIRED_ARGUMENT,
          null,
          0),
        new LongOpt(Daikon.debugAll_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
        new LongOpt(Daikon.debug_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
        new LongOpt(
          Daikon.ppt_regexp_SWITCH,
          LongOpt.REQUIRED_ARGUMENT,
          null,
          0),
        new LongOpt(Daikon.track_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
        };
    Getopt g = new Getopt("daikon.PrintInvariants", args, "ho:", longopts);
    int c;
    while ((c = g.getopt()) != -1) {
      switch (c) {
        case 0 :
          // got a long option
          String option_name = longopts[g.getLongind()].getName();
          if (Daikon.help_SWITCH.equals(option_name)) {
            System.out.println(usage);
            throw new Daikon.TerminationMessage();
          } else if (Daikon.config_option_SWITCH.equals(option_name)) {
            String item = g.getOptarg();
            daikon.config.Configuration.getInstance().apply(item);
            break;
          } else if (Daikon.debugAll_SWITCH.equals(option_name)) {
            Global.debugAll = true;
          } else if (Daikon.debug_SWITCH.equals(option_name)) {
            LogHelper.setLevel(g.getOptarg(), LogHelper.FINE);
          } else if (Daikon.track_SWITCH.equals(option_name)) {
            LogHelper.setLevel("daikon.Debug", LogHelper.FINE);
            String error = Debug.add_track(g.getOptarg());
            if (error != null) {
              throw new Daikon.TerminationMessage(
                "Error parsing track argument '"
                  + g.getOptarg()
                  + "' - "
                  + error);
            }
          } else {
            throw new RuntimeException(
              "Unknown long option received: " + option_name);
          }
          break;
        case 'o' :
          if (inv_file != null)
            throw new Error("multiple serialization output files supplied on command line");
          String inv_filename = g.getOptarg();
          inv_file = new File(inv_filename);
          if (!UtilMDE.canCreateAndWrite(inv_file)) {
            throw new Error("Cannot write to file " + inv_file);
          }
          break;
        case 'h' :
          System.out.println(usage);
          throw new Daikon.TerminationMessage();
        case '?' :
          break; // getopt() already printed an error
        default :
          System.out.println("getopt() returned " + c);
          break;
      }
    }

    // Loop through each filename specified
    for (int i = g.getOptind(); i < args.length; i++) {
      // Get the file and make sure it exists
      File file = new File(args[i]);
      if (!file.exists()) {
        throw new Error("File " + file + " not found.");
      }
      // These aren't "endsWith()" because there might be a suffix on the end
      // (eg, a date).
      String filename = file.toString();
      if (filename.indexOf(".decls") != -1) {
        decl_files.add(file);
      } else if (filename.indexOf(".dtrace") != -1) {
        dtrace_files.add(file);
      } else {
        throw new Error("Unrecognized argument: " + file);
      }
    }
    return new Set[] { decl_files, dtrace_files, spinfo_files, map_files };
  }

  /**
    * Process all ppts by adding orig and derived variables, relating
    * variables to other program points, and fixing all necessary
    * pre-computed flows.  (Calls init_partial_order(PptTopLevel,
    * PptMap) for each ppt in turn.)
    **/
  public static void init_partial_order(PptMap all_ppts) {

    // Create combined exit points
    Daikon.create_combined_exits(all_ppts);

    // Setup orig variables
    for (Iterator i = all_ppts.ppt_all_iterator(); i.hasNext();) {
      PptTopLevel ppt = (PptTopLevel) i.next();
      create_and_relate_orig_vars(ppt, all_ppts);
    }
    // Set up derived variables
    for (Iterator i = all_ppts.ppt_all_iterator(); i.hasNext();) {
      PptTopLevel ppt = (PptTopLevel) i.next();
      ppt.create_derived_variables();
    }
    return;
  }

  /**
     * Add orig() variables to the given EXIT/EXITnn point, and relate
     * them to the corresponding ENTER point.  Does nothing if exit_ppt
     * is not an EXIT/EXITnn.  Does not relate if point is EXITnn.
     **/
  private static void create_and_relate_orig_vars(
    PptTopLevel exit_ppt,
    PptMap ppts) {
    if (!exit_ppt.ppt_name.isExitPoint()) {
      return;
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
        Assert.assertTrue(
          !vi.isDerived(),
          "Derived when making orig(): " + vi.name);
        if (vi.isStaticConstant())
          continue;
        VarInfo origvar = VarInfo.origVarInfo(vi);
        // Fix comparability
        VarInfo postvar = exit_ppt.findVar(vi.name);
        Assert.assertTrue(
          postvar != null,
          "Exit not superset of entry: " + vi.name);
        origvar.comparability = postvar.comparability.makeAlias(origvar.name);

        // Add to new_vis
        new_vis[new_vis_index] = origvar;
        new_vis_index++;
      }
      Assert.assertTrue(new_vis_index == exit_ppt.num_orig_vars);

    }
    exit_ppt.addVarInfos(new_vis);
  }

  /**
     * This function is called to jump-start processing; it creates all
     * the views (and thus candidate invariants), but does not check
     * those invariants.
     **/
  public static void instantiate_views_and_invariants(PptTopLevel ppt) {
    // Now make all of the views (and thus candidate invariants)
    instantiate_views(0, ppt.var_infos.length, ppt);

  }

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

  public static void instantiate_views(
    int vi_index_min,
    int vi_index_limit,
    PptTopLevel ppt) {

    // This test prevents instantiate views for variables one at a time.
    Assert.assertTrue(ppt.var_infos.length == vi_index_limit);

    if (vi_index_min == vi_index_limit)
      return;

    // used only for debugging
    int old_num_vars = ppt.var_infos.length;
    int old_num_views = ppt.numViews();
    boolean debug_on = debug.isLoggable(Level.FINE);

    /// 1. all unary views

    // Unary slices/invariants.
    Vector unary_views = new Vector(vi_index_limit - vi_index_min);
    for (int i = vi_index_min; i < vi_index_limit; i++) {
      VarInfo vi = ppt.var_infos[i];

      if (!is_slice_ok(vi, ppt)) {
        continue;
      }
      // Eventually, add back in this test as "if constant and no
      // comparability info exists" then continue.
      // if (vi.isStaticConstant()) continue;
      PptSlice1 slice1 = new PptSlice1(ppt, vi);
      slice1.instantiate_invariants();

      unary_views.add(slice1);
    }
    ppt.addViews(unary_views);
    unary_views = null;

    /// 2. all binary views

    // Binary slices/invariants.
    Vector binary_views = new Vector();
    for (int i1 = 0; i1 < vi_index_limit; i1++) {
      VarInfo var1 = ppt.var_infos[i1];
      if (!var1.isCanonical() && !(Debug.logOn() || debug_on)) {
        continue;
      }

      // Eventually, add back in this test as "if constant and no
      // comparability info exists" then continue.
      // if (var1.isStaticConstant()) continue;
      boolean target1 = (i1 >= vi_index_min) && (i1 < vi_index_limit);
      int i2_min = (target1 ? i1 : Math.max(i1, vi_index_min));
      for (int i2 = i2_min; i2 < vi_index_limit; i2++) {
        VarInfo var2 = ppt.var_infos[i2];

        if (!var1.isCanonical()) {
          continue;
        }
        if (!var2.isCanonical()) {
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
        if (!is_slice_ok(var1, var2, ppt)) {
          continue;
        }

        PptSlice2 slice2 = new PptSlice2(ppt, var1, var2);
        slice2.instantiate_invariants();
        binary_views.add(slice2);
      }
    }
    ppt.addViews(binary_views);

    binary_views = null;

    // 3. all ternary views

    Vector ternary_views = new Vector();
    for (int i1 = 0; i1 < vi_index_limit; i1++) {
      VarInfo var1 = ppt.var_infos[i1];
      //     System.out.println("var1: " + var1);
      //     if (!var1.isCanonical() && !(Debug.logOn() || debug_on))
      //       continue;

      // Eventually, add back in this test as "if constant and no
      // comparability info exists" then continue.
      // if (var1.isStaticConstant()) continue;
      // For now, only ternary invariants not involving any arrays
      //     if (var1.rep_type.isArray() && (!Debug.logOn() || debug_on))
      //       continue;

      boolean target1 = (i1 >= vi_index_min) && (i1 < vi_index_limit);
      for (int i2 = i1; i2 < vi_index_limit; i2++) {
        VarInfo var2 = ppt.var_infos[i2];
        //       System.out.println("var2:" + var2);
        //       if (!var2.isCanonical() && !(Debug.logOn() || debug_on))
        //       continue;

        // Eventually, add back in this test as "if constant and no
        // comparability info exists" then continue.
        // if (var2.isStaticConstant()) continue;
        // For now, only ternary invariants not involving any arrays
        //       if (var2.rep_type.isArray() && !(Debug.logOn() || debug_on))
        //       continue;

        boolean target2 = (i2 >= vi_index_min) && (i2 < vi_index_limit);
        int i3_min = ((target1 || target2) ? i2 : Math.max(i2, vi_index_min));
        for (int i3 = i3_min; i3 < vi_index_limit; i3++) {
          Assert.assertTrue(
            ((i1 >= vi_index_min) && (i1 < vi_index_limit))
              || ((i2 >= vi_index_min) && (i2 < vi_index_limit))
              || ((i3 >= vi_index_min) && (i3 < vi_index_limit)));
          Assert.assertTrue((i1 <= i2) && (i2 <= i3));
          VarInfo var3 = ppt.var_infos[i3];
          //      System.out.println("var3: " + var3);

          //  System.out.println(ppt.name);
          //  System.out.println(var1.name.name() + " " + var2.name.name() + " " + var3.name.name());

          if (!is_slice_ok(var1, var2, var3, ppt)) {
            //System.out.println("slice not ok");

            continue;
          } else {

            //System.out.println("slice ok");
          }
          PptSlice3 slice3 = new PptSlice3(ppt, var1, var2, var3);
          slice3.instantiate_invariants();
          ternary_views.add(slice3);
        }
      }
    }

    ppt.addViews(ternary_views);

    // This method didn't add any new variables.
    Assert.assertTrue(old_num_vars == ppt.var_infos.length);
    ppt.repCheck();

  }

  /**
   * Returns whether or not the specified unary slice should be
   * created.  The variable must be a leader, not a constant, and
   * not always missing.
   */
  public static boolean is_slice_ok(VarInfo var1, PptTopLevel ppt) {

    //     if (Daikon.dkconfig_use_dynamic_constant_optimization && constants == null)
    //     return (false);
    //     if (ppt.is_constant (var1))
    //     return (false);
    //     if (ppt.is_missing (var1))
    //     return (false);
    //     if (!var1.isCanonical())
    //     return (false);

    return (true);
  }

  /**
   * Returns whether or not the specified binary slice should be created.
   * Checks to sinsure that var1 and var2 are not both constants and
   * if they are in the same equality set, that there are at least 2
   * variables in the equality set.  Also makes sure that neither var1
   * or var2 is always missing.
   */
  public static boolean is_slice_ok(
    VarInfo var1,
    VarInfo var2,
    PptTopLevel ppt) {

    //     // Both vars must be leaders
    //     if (!var1.isCanonical() || !var2.isCanonical())
    //     return (false);
    //
    //     // Check to see if the new slice would be over all constants
    //     if (ppt.is_constant (var1) && ppt.is_constant (var2))
    //     return (false);
    //
    //     // Each variable must not be always missing
    //     if (ppt.is_missing (var1) || ppt.is_missing (var2))
    //     return (false);
    //
    //     // Don't create a slice with the same variables if the equality
    //     // set only contains 1 variable
    //     // This is not turned on for now since suppressions need invariants
    //     // of the form a == a even when a is the only item in the set.
    //     if (false) {
    //           if ((var1 == var2) && (var1.get_equalitySet_size() == 1))
    //             return (false);
    //     }

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
   *    - Any var is not (integral or float)
   *    - Each var is the same and its equality set has only two variables
   *    - Two of the vars are the same and its equality has only one variable
   *      (this last one is currently disabled as x = func(x,y) might still
   *      be interesting even if x is the same.
   */
  public static boolean is_slice_ok(
    VarInfo v1,
    VarInfo v2,
    VarInfo v3,
    PptTopLevel ppt) {

    Debug dlog = null;

    // Each variable must not be always missing
    //     if (ppt.is_missing (v1) || ppt.is_missing (v2) || ppt.is_missing (v3))
    //     return (false);
    //
    //     // At least one variable must not be a constant
    //     if (ppt.is_constant (v1) && ppt.is_constant(v2) && ppt.is_constant (v3))
    //     return false;
    //
    //     // Each variable must be canonical (leader)
    //     if (!v1.isCanonical()) {
    //
    //     return (false);
    //     }
    //     if (!v2.isCanonical()) {
    //
    //     return (false);
    //     }
    //     if (!v3.isCanonical()) {
    //
    //     return (false);
    //     }

    // For now, each variable must also not be an array (ternary only)
    if (v1.rep_type.isArray()) {

      return (false);
    }
    if (v2.rep_type.isArray()) {

      return (false);
    }
    if (v3.rep_type.isArray()) {

      return (false);
    }

    // Vars must be compatible
    if (!v1.compatible(v2) || !v1.compatible(v3)) {

      return (false);
    }

    // For now, each variable must be integral or float.  We only need
    // to check the first variable since comparability will handle the
    // others
    if (!v1.file_rep_type.isIntegral() && !v1.file_rep_type.isFloat()) {
      if (dlog != null)
        dlog.log(
          debug,
          "Ternary slice not created, vars are neither " + "integral or float");
      return (false);
    }
    Assert.assertTrue(
      v2.file_rep_type.isIntegral() || v2.file_rep_type.isFloat());
    Assert.assertTrue(
      v3.file_rep_type.isIntegral() || v3.file_rep_type.isFloat());

    // Don't create a reflexive slice (all vars the same) if there are
    // only two vars in the equality set
    //         if ((v1 == v2) && (v2 == v3))
    //         return (false);

    // Don't create a partially reflexive slice (two vars the same) if there
    // is only one variable in its equality set
    //     if (false) {
    //         if ((v1 == v2) || (v1 == v3) && (v1.get_equalitySet_size() == 1))
    //           return (false);
    //         if ((v2 == v3) && (v2.get_equalitySet_size() == 1))
    //           return (false);
    //     }

    return (true);
  }

  /** Class to track matching ppt and its values. */
  static final class EnterCall {

    public PptTopLevel ppt;
    public ValueTuple vt;

    public EnterCall(PptTopLevel ppt, ValueTuple vt) {

      this.ppt = ppt;
      this.vt = vt;
    }
  }

  private static class SimpleProcessor extends FileIO.Processor {
    PptMap all_ppts = null;

    /** nonce -> EnterCall **/
    Map call_map = new LinkedHashMap();

    /**
     * process the sample by checking it against each existing invariant
     * and removing the invariant from the list of possibles if any invariant is falsified.
     */
    public void process_sample(
      PptMap all_ppts,
      PptTopLevel ppt,
      ValueTuple vt,
      Integer nonce) {
      this.all_ppts = all_ppts;

      // Add samples to orig and derived variables
      FileIO.add_orig_variables(ppt, vt.vals, vt.mods, nonce);
      FileIO.add_derived_variables(ppt, vt.vals, vt.mods);

      // Intern the sample
      vt = new ValueTuple(vt.vals, vt.mods);

      // If this is an enter point, just remember it for later
      if (ppt.ppt_name.isEnterPoint()) {
        Assert.assertTrue(nonce != null);
        Assert.assertTrue(call_map.get(nonce) == null);
        call_map.put(nonce, new EnterCall(ppt, vt));
        return;
      }

      // If this is an exit point, process the saved enter point
      if (ppt.ppt_name.isExitPoint()) {
        Assert.assertTrue(nonce != null);
        EnterCall ec = (EnterCall) call_map.get(nonce);
        call_map.remove(nonce);
        add(ec.ppt, ec.vt);
      }

      add(ppt, vt);
    }

    private void add(PptTopLevel ppt, ValueTuple vt) {
      // if this is a numbered exit, apply to the combined exit as well
      //  if (!(ppt instanceof PptConditional)
      //    && ppt.ppt_name.isNumberedExitPoint()) {
      if (ppt.ppt_name.isNumberedExitPoint()) {
        PptTopLevel parent = all_ppts.get(ppt.ppt_name.makeExit());
        if (parent != null) {
          parent.get_missingOutOfBounds(ppt, vt);
          add(parent, vt);
        }
      }

      // If the point has no variables, skip it
      if (ppt.var_infos.length == 0)
        return;

      //Instantiate slices and invariants if this is the first sample
      if (ppt.num_samples() == 0) {
        instantiate_views_and_invariants(ppt);
        //ppt.instantiate_views_and_invariants();
      }

      //manually inc the sample number because i don't use any of ppt's add methods
      ppt.incSampleNumber();

      // Loop through each slice
      Iterator i = ppt.views_iterator();
      while (i.hasNext()) {
        PptSlice slice = (PptSlice) i.next();
        List to_remove = new ArrayList();
        List result = new ArrayList();
        Iterator k = slice.invs.iterator();
        boolean missing = false;

        for (int j = 0; j < slice.var_infos.length; j++) {
          VarInfo v = slice.var_infos[j];
          // If any var has encountered out of array bounds values,
          // stop all invariants in this slice.  The presumption here is that
          // an index out of bounds implies that the derived variable (eg a[i])
          // doesn't really make any sense (essentially that i is not a valid
          // index for a).  Invariants on the derived variable are thus not
          // relevant
          //  If any variables are out of bounds, remove the invariants
          if (v.missingOutOfBounds()) {

            while (k.hasNext()) {
              Invariant inv = (Invariant) k.next();
              k.remove();
            }
            missing = true;
            break;
          }

          //  If any variables are missing, skip this slice
          if (v.isMissing(vt)) {
            missing = true;
            break;
          }
        }

        //keep a list of the falsified invariants
        if (!missing) {
          while (k.hasNext()) {
            Invariant inv = (Invariant) k.next();

            InvariantStatus status = inv.add_sample(vt, 1);

            if (status == InvariantStatus.FALSIFIED) {
              //to_remove.add(inv);
              k.remove();

            }
          }
        }

        //update num_samples and num_values of a slice
        for (int j = 0; j < vt.vals.length; j++) {
          if (!vt.isMissing(j)) {
            ValueSet vs = ppt.value_sets[j];
            vs.add(vt.vals[j]);
            // System.out.println("ValueSet(" + i + ") now has " + vs.size() + " elements");
          } else {
            ValueSet vs = ppt.value_sets[j];
            // System.out.println("ValueSet(" + i + ") not added to, still has " + vs.size() + " elements");
          }
        }
        ppt.mbtracker.add(vt, 1);

      }
    }
  }
}
