// Main routine for Daikon invariant detector
// For documentation, see file doc/daikon.html in the distribution.

package daikon;

import daikon.split.*;
import daikon.suppress.*;
import daikon.inv.*;
import daikon.inv.Invariant.OutputFormat;
import daikon.config.Configuration;

import java.util.*;
import java.io.*;
import java.text.DateFormat;
import java.text.NumberFormat;
import java.text.DecimalFormat;
import java.lang.Thread;

import org.apache.oro.text.regex.*;
import java.util.logging.Logger;
import java.util.logging.Level;
import gnu.getopt.*;
import utilMDE.*;

public final class Daikon {

  public final static String release_version = "3.1.0";
  public final static String release_date = "June 1, 2004";
  public final static String release_string
    = "Daikon version " + release_version
    + ", released " + release_date
    + "; http://pag.csail.mit.edu/daikon.";

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /**
   * Boolean.  Controls whether conditional program points (see Daikon
   * manual) are displayed.
   **/
  public static boolean dkconfig_output_conditionals = true;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /**
   * Boolean.  Controls whether invariants are reported over floating-point
   * values.
   **/
  public static boolean dkconfig_enable_floats = true;

  /**
   * Boolean.  Just print the total number of possible invariants
   * and exit.
   */
  public static boolean dkconfig_calc_possible_invs;

  /**
   * Integer. Percentage of ppts to process.  All ppts are sorted and
   * the first dkconfig_ppt_perc ppts are included.  A percentage of
   * 100 (default) matches all ppts.
   */
  public static int dkconfig_ppt_perc = 100;

  /**
   * Boolean.  Controls whether or not total samples read and processed
   * are printed at the end of processing.
   */
  public static boolean dkconfig_print_sample_totals = false;

  // All these variables really need to be organized better.

  public final static String lineSep = Global.lineSep;

  /**
   * Boolean.  Controls whether or not splitting based on the built-in
   * splitting rules is disabled.  The built-in rules look for implications
   * based on boolean return values and also when there are exactly two
   * exit points from a method.
   **/
  public static boolean dkconfig_disable_splitting = false;

  // Change this at your peril; high costs in time and space for "false",
  // because so many more invariants get instantiated.
  public final static boolean check_program_types = true;

  // Problem with setting this to true:
  //  get no invariants over any value that can ever be missing
  // Problem with setting this to false:
  //  due to differrent number of samples, IsEqualityComparison is
  //  non-transitive (that is specially handled in the code)
  public final static boolean invariants_check_canBeMissing = false;

  // Specialized version for array elements; only examined if
  // invariants_check_canBeMissing is false
  public final static boolean invariants_check_canBeMissing_arrayelt = true;

  public final static boolean disable_modbit_check_message = false;
  // Not a good idea to set this to true, as it is too easy to ignore the
  // warnings and the modbit problem can cause an error later.
  public final static boolean disable_modbit_check_error = false;

  // When true, don't print textual output.
  public static boolean no_text_output = false;

  // When true, show how much time each program point took.
  // Has no effect unless no_text_output is true.
  public static boolean show_progress = false;

  /**
   * Whether to use the "new" equality set mechanism for handling
   * equality, using canonicals to have instantiation of invariants
   * only over equality sets.
   **/
  public static boolean use_equality_optimization = true;

  /**
   * Whether to use the dynamic constants optimization.  This
   * optimization doesn't instantiate invariants over constant
   * variables (ie, that that have only seen one value).  When the
   * variable receives a second value, invariants are instantiated and
   * are given the sample representing the previous constant value.
   **/
  public static boolean dkconfig_use_dynamic_constant_optimization = true;

  /**
   * If true no invariants will be guarded, guarding meaning that
   * if a variable "can be missing" in a dtrace file that predicates
   * are attached to invariants ensuring their values can be gather
   * (for instance, if a.b "can be missing", and a.b == 5 is an
   * invariant, then it is more properly declared as (a != null) ==>
   * (a.b == 5))
   **/
  public static boolean dkconfig_noInvariantGuarding = false;

  /**
   * When true compilation errors during splitter file generation
   * will not be reported to the user.
   */
  public static boolean dkconfig_suppressSplitterErrors = false;

  /**
   * When true, invariants that are implied by other invariants are
   * suppressed.
   **/
  public static boolean dkconfig_use_suppression_optimization = true;

  /**
   * Whether suppressed invariants can suppress others.  Eventually
   * this will be obsolete, as we will be able to do cycle detection
   * on invariant dependencies.
   **/
  public static boolean suppress_with_suppressed = true;

  /**
   * How many samples to process per ppt before suppression is
   * initiated.  When zero, initiate suppression immediately before
   * seeing samples.
   **/
  public static int suppress_samples_min = 10;

  /**
   * Whether to associate the program points in a dataflow hierarchy,
   * as via Nimmer's thesis.  Deactivate only for languages and
   * analyses where flow relation is nonsensical.
   **/
  public static boolean use_dataflow_hierarchy = true;

  /**
   * Whether to use the bottom up implementation of the dataflow
   * hierarchy.  This mechanism builds invariants initially
   * only at the leaves of the partial order.  Upper points are
   * calculated by joining the invariants from each of their children
   * points.
   **/
  public static boolean dkconfig_df_bottom_up = true;

  // When true, don't print invariants when their controlling ppt
  // already has them.  For example, this is the case for invariants
  // in public methods which are already given as part of the object
  // invariant for that class.
  public static boolean suppress_implied_controlled_invariants = true;

  // When true, don't print EXIT invariants over strictly orig()
  // variables when the corresponding entry ppt already has the
  // invariant.
  public static boolean suppress_implied_postcondition_over_prestate_invariants = false;

  // When true, use the Simplify theorem prover (not part of Daikon)
  // to locate logically redundant invariants, and flag them as
  // redundant, so that they are removed from the printed output.
  public static boolean suppress_redundant_invariants_with_simplify = false;

  // Set what output style to use.  DAIKON is the default; ESC style
  // is based on JML; SIMPLIFY style uses first order logical
  // expressions with lots of parens
  public static OutputFormat output_style = OutputFormat.DAIKON;
  // public static OutputFormat output_style = OutputFormat.ESCJAVA;
  // public static OutputFormat output_style = OutputFormat.DBCJAVA;
  // public static OutputFormat output_style = OutputFormat.SIMPLIFY;

  // When true, output numbers of values and samples (also names of variables)
  public static boolean output_num_samples = false;

  public static boolean ignore_comparability = false;

  // Controls which program points/variables are used/ignored.
  public static Pattern ppt_regexp;
  public static Pattern ppt_omit_regexp;
  public static Pattern var_omit_regexp;

  /**
   * When true perform detailed internal checking (essentially additional,
   * possibly costly assert statements
   */
  public static boolean dkconfig_internal_check = false;

  /**
   * If set, only ppts less than ppt_max_name are included.  Used by the
   * configuration option dkconfig_ppt_percent to only work on a specified
   * percent of the ppts.
   */
  public static String ppt_max_name = null;

  // The invariants detected will be serialized and written to this
  // file.
  public static File inv_file;

  // Whether we want the memory monitor activated
  private static boolean use_mem_monitor = false;

  /**
   * Whether Daikon should print its version number and date
   **/
  public static boolean noversion_output = false;

  /**
   * Whether Daikon is in its inferncing loop.  Used only for
   * assertion checks.
   **/
  public static boolean isInferencing = false;

  /**
   * When true, omit certain invariants from the output .inv
   * file. Generally these are invariants that wouldn't be printed in
   * any case; but by default, they're retained in the .inv file in
   * case they would be useful for later processing. (For instance, we
   * might at some point in the future support resuming processing
   * with more data from an .inv file). These invariants can increase
   * the size of the .inv file, though, so when only limited further
   * processing is needed, it can save space to omit them.
   **/
  public static boolean omit_from_output = false;

  /**
   * An array of flags, indexed by characters, in which a true entry
   * means that invariants of that sort should be omitted from the
   * output .inv file.
   **/
  public static boolean[] omit_types = new boolean[256];

  // Public so other programs can reuse the same command-line options
  public static final String help_SWITCH = "help";
  public static final String ppt_regexp_SWITCH = "ppt";
  public static final String ppt_omit_regexp_SWITCH = "ppt_omit";
  public static final String list_type_SWITCH = "list_type";
  public static final String var_omit_regexp_SWITCH = "var_omit";
  public static final String no_text_output_SWITCH = "no_text_output";
  public static final String show_progress_SWITCH = "show_progress";
  public static final String no_show_progress_SWITCH = "no_show_progress";
  public static final String no_dataflow_hierarchy_SWITCH = "nohierarchy";
  public static final String suppress_redundant_SWITCH = "suppress_redundant";
  public static final String conf_limit_SWITCH = "conf_limit";
  public static final String esc_output_SWITCH = "esc_output";
  public static final String ioa_output_SWITCH = "ioa_output";
  public static final String test_ioa_output_SWITCH = "test_ioa_output";
  public static final String java_output_SWITCH = "java_output";
  public static final String jml_output_SWITCH = "jml_output";
  public static final String dbc_output_SWITCH = "dbc_output";
  public static final String mem_stat_SWITCH = "mem_stat";
  public static final String simplify_output_SWITCH = "simplify_output";
  public static final String output_num_samples_SWITCH = "output_num_samples";
  public static final String config_SWITCH = "config";
  public static final String config_option_SWITCH = "config_option";
  public static final String debugAll_SWITCH = "debug";
  public static final String debug_SWITCH = "dbg";
  public static final String files_from_SWITCH = "files_from";
  public static final String noversion_SWITCH = "noversion";
  public static final String disc_reason_SWITCH = "disc_reason";
  public static final String track_SWITCH = "track";
  public static final String omit_from_output_SWITCH = "omit_from_output";

  // A pptMap which contains all the Program Points
  // This isn't used anymore; instead, methods have parameters or
  // local variables of the same name, and all_ppts is always null. Is
  // there a reason it shouldn't just go away? -SMcC 4/24/03
  // JHP 11/20/03, it is used now to avoid passing it in a few spots.
  public static PptMap all_ppts;

  /** Debug tracer **/
  public static final Logger debugTrace = Logger.getLogger("daikon.Daikon");

  public static final Logger debugProgress = Logger.getLogger("daikon.Progress");

  public static final Logger debugEquality = Logger.getLogger("daikon.Equality");

  /** prints out statistics concerning equality sets, suppressions, etc **/
  public static final Logger debugStats = Logger.getLogger ("daikon.stats");

  // Avoid problems if daikon.Runtime is loaded at analysis (rather than
  // test-run) time.  This might have to change when JTrace is used.
  static { daikon.Runtime.no_dtrace = true; }

  private static Stopwatch stopwatch = new Stopwatch();

  static String usage =
    UtilMDE.join(new String[] {
      release_string,
      "Daikon invariant detector, copyright 1998-2004",
      // " by Michael Ernst <mernst@csail.mit.edu>",
      "Uses the Java port of GNU getopt, copyright (c) 1998 Aaron M. Renn",
      "For licensing information, see the License section of the manual.",
      "Usage:",
      "    java daikon.Daikon [flags...] files...",
      "  Each file is a declaration file or a data trace file; the file type",
      "  is determined by the file name (containing \".decls\" or \".dtrace\").",
      "  For a list of flags, see the Daikon manual, which appears in the ",
      "  Daikon distribution and also at http://pag.csail.mit.edu/daikon/."},
                 lineSep);

  /**
   * The arguments to daikon.Daikon are file names; declaration file names end
   * in ".decls" and data trace file names end in ".dtrace".
   **/
  public static void main(String[] args)
  {
    // Read command line options
    Set[] files = read_options(args);
    Assert.assertTrue(files.length == 4);
    Set decls_files = files[0];  // [File]
    Set dtrace_files = files[1]; // [File]
    Set spinfo_files = files[2]; // [File]
    Set map_files = files[3];    // [File]
    if ((decls_files.size() == 0) && (dtrace_files.size() == 0)) {
      System.out.println("No .decls or .dtrace files specified");
      System.exit(1);
    }

    // Set up debug traces; note this comes after reading command line options.
    LogHelper.setupLogs(Global.debugAll ? LogHelper.FINE : LogHelper.INFO);

    if (! noversion_output) {
      System.out.println(release_string);
    }

    if (PrintInvariants.print_discarded_invariants) {
      DiscReasonMap.initialize();
    }

    fileio_progress.start();

    // Load declarations and splitters
    all_ppts = load_decls_files(decls_files);
    load_spinfo_files(all_ppts, spinfo_files);
    load_map_files(all_ppts, map_files);

    Dataflow.init_partial_order(all_ppts);
    if (debugTrace.isLoggable(Level.FINE)) {
      debugTrace.fine ("Partial order initialized");
    }
    PptTopLevel.init (all_ppts);
    all_ppts.trimToSize();


    // If requested, just calculate the total number of invariants possible
    if (dkconfig_calc_possible_invs) {
      fileio_progress.shouldStop = true;
      setupEquality (all_ppts);
      int total_invs = 0;
      for (Iterator itor = all_ppts.ppt_all_iterator() ; itor.hasNext() ; ) {
        PptTopLevel ppt = (PptTopLevel) itor.next();
        System.out.println ("Processing " + ppt.name() + " with "
                            + ppt.var_infos.length + " variables");
        int inv_cnt = 0;
        if (ppt.var_infos.length > 1600)
          System.out.println ("Skipping, too many variables!");
        else {
          ppt.instantiate_views_and_invariants();
          inv_cnt = ppt.invariant_cnt();
          ppt.clean_for_merge();
          System.out.println (inv_cnt + " invariants in " + ppt.name());
          total_invs += inv_cnt;
        }
      }
      System.out.println (total_invs + "invariants total");
      System.exit(0);
    }

    // Only for assertion checks
    isInferencing = true;

    // Infer invariants
    process_data(all_ppts, dtrace_files);
    isInferencing = false;
    if (Debug.logOn())
      Debug.check (all_ppts, "After process data");

    if (suppress_redundant_invariants_with_simplify) {
      suppressWithSimplify(all_ppts);
    }

    // Check that PptMap created was correct
    all_ppts.repCheck();

    // Remove undesired invariants, if requested
    if (omit_from_output) {
      processOmissions(all_ppts);
    }

    // Write serialized output - must be done before guarding invariants
    if (inv_file != null) {
      try {
        FileIO.write_serialized_pptmap(all_ppts, inv_file);
      } catch (IOException e) {
        throw new RuntimeException("Error while writing .inv file "
                                   + "'" + inv_file + "': " + e.toString());
      }
    }

    // Guard invariants
    if ((Daikon.output_style == OutputFormat.JML ||
         Daikon.output_style == OutputFormat.ESCJAVA) &&
        !dkconfig_noInvariantGuarding)
      guardInvariants(all_ppts);

    // Display invariants
    if (output_num_samples) {
      System.out.println("The --output_num_samples debugging flag is on.");
      System.out.println("Some of the debugging output may only make sense to Daikon programmers.");
    }

    // If they want to see discarded invariants, they probably don't
    // want to see the true ones.
    if (!PrintInvariants.print_discarded_invariants) {
      PrintInvariants.print_invariants(all_ppts);
    } else {
      PrintInvariants.print_reasons(all_ppts);
    }

    if (output_num_samples) {
      Global.output_statistics();
    }
    if (dkconfig_print_sample_totals)
      System.out.println (FileIO.samples_processed + " samples processed of " +
                          FileIO.samples_considered + " total samples");

    // print statistics concerning what invariants are printed
    if (debugStats.isLoggable (Level.FINE)) {
      for (Iterator itor = all_ppts.ppt_all_iterator() ; itor.hasNext() ; ) {
        PptTopLevel ppt = (PptTopLevel) itor.next();
        PrintInvariants.print_filter_stats (debugStats, ppt, all_ppts);
      }
    }

    // Done
    System.out.println("Exiting");
  }

  ///////////////////////////////////////////////////////////////////////////
  // Read in the command line options
  // Return an array of {decls, dtrace, spinfo, map} files; each array
  // element is a set.
  private static Set[] read_options(String args[])
  {
    if (args.length == 0) {
      System.out.println("Daikon error: no files supplied on command line.");
      System.out.println(usage);
      System.exit(1);
    }

    Set decl_files = new HashSet();
    Set dtrace_files = new HashSet();
    Set spinfo_files = new HashSet();
    Set map_files = new HashSet();

    LongOpt[] longopts = new LongOpt[] {
      new LongOpt(help_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(ppt_regexp_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
      new LongOpt(ppt_omit_regexp_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
      new LongOpt(list_type_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
      new LongOpt(var_omit_regexp_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
      new LongOpt(no_text_output_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(show_progress_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(no_show_progress_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(no_dataflow_hierarchy_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(suppress_redundant_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(conf_limit_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
      new LongOpt(esc_output_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(simplify_output_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(dbc_output_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(ioa_output_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(test_ioa_output_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(java_output_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(jml_output_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(mem_stat_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(output_num_samples_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(config_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
      new LongOpt(config_option_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
      new LongOpt(debugAll_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(debug_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
      new LongOpt(files_from_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
      new LongOpt(noversion_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(disc_reason_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
      new LongOpt(track_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
      new LongOpt(omit_from_output_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
    };
    Getopt g = new Getopt("daikon.Daikon", args, "ho:", longopts);
    int c;

    while ((c = g.getopt()) != -1) {
      switch(c) {
      case 0:
        // got a long option
        String option_name = longopts[g.getLongind()].getName();
        if (help_SWITCH.equals(option_name)) {
          System.out.println(usage);
          System.exit(1);
        } else if (disc_reason_SWITCH.equals(option_name)) {
          try { PrintInvariants.discReasonSetup(g.getOptarg()); }
          catch (IllegalArgumentException e) {
            System.out.print(e.getMessage());
            System.exit(1);
          }
        } else if (track_SWITCH.equals (option_name)) {
          LogHelper.setLevel("daikon.Debug", LogHelper.FINE);
          String error = Debug.add_track (g.getOptarg());
          if (error != null) {
            System.out.println ("Error parsing track argument '"
                                + g.getOptarg() + "' - " + error);
            System.exit(1);
          }
        } else if (ppt_regexp_SWITCH.equals(option_name)) {
          if (ppt_regexp != null)
            throw new Error("multiple --" + ppt_regexp_SWITCH
                            + " regular expressions supplied on command line");
          try {
            String regexp_string = g.getOptarg();
            // System.out.println("Regexp = " + regexp_string);
            ppt_regexp = Global.regexp_compiler.compile(regexp_string);
          } catch (Exception e) {
            throw new Error(e.toString());
          }
          break;
        } else if (ppt_omit_regexp_SWITCH.equals(option_name)) {
          if (ppt_omit_regexp != null)
            throw new Error("multiple --" + ppt_omit_regexp_SWITCH
                            + " regular expressions supplied on command line");
          try {
            String regexp_string = g.getOptarg();
            // System.out.println("Regexp = " + regexp_string);
            ppt_omit_regexp = Global.regexp_compiler.compile(regexp_string);
          } catch (Exception e) {
            throw new Error(e.toString());
          }
          break;
        } else if (list_type_SWITCH.equals(option_name)) {
          try {
            String list_type_string = g.getOptarg();
            ProglangType.list_implementors.add(list_type_string);
          } catch (Exception e) {
            throw new Error(e.toString());
          }
          break;
        } else if (var_omit_regexp_SWITCH.equals(option_name)) {
          if (var_omit_regexp != null)
            throw new Error("multiple --" + var_omit_regexp_SWITCH
                            + " regular expressions supplied on command line");
          try {
            String regexp_string = g.getOptarg();
            // System.out.println("Regexp = " + regexp_string);
            var_omit_regexp = Global.regexp_compiler.compile(regexp_string);
          } catch (Exception e) {
            throw new Error(e.toString());
          }
          break;
        } else if (debugAll_SWITCH.equals(option_name)) {
          Global.debugAll = true;
        } else if (debug_SWITCH.equals(option_name)) {
          LogHelper.setLevel(g.getOptarg(), LogHelper.FINE);
        } else if (no_text_output_SWITCH.equals(option_name)) {
          no_text_output = true;
        } else if (show_progress_SWITCH.equals(option_name)) {
          show_progress = true;
          LogHelper.setLevel ("daikon.Progress", LogHelper.FINE);
        } else if (no_show_progress_SWITCH.equals(option_name)) {
          show_progress = false;
        } else if (no_dataflow_hierarchy_SWITCH.equals(option_name)) {
          use_dataflow_hierarchy = false;
        } else if (suppress_redundant_SWITCH.equals(option_name)) {
          suppress_redundant_invariants_with_simplify = true;
        } else if (conf_limit_SWITCH.equals(option_name)) {
          double limit = Double.parseDouble(g.getOptarg());
          if ((limit < 0.0) || (limit > 1.0)) {
            throw new Error(conf_limit_SWITCH + " must be between [0..1]");
          }
          Configuration.getInstance().apply
            ("daikon.inv.Invariant.confidence_limit", String.valueOf(limit));
        } else if (esc_output_SWITCH.equals(option_name)) {
          output_style = OutputFormat.ESCJAVA;
        } else if (simplify_output_SWITCH.equals(option_name)) {
          output_style = OutputFormat.SIMPLIFY;
        } else if (ioa_output_SWITCH.equals(option_name)) {
          output_style = OutputFormat.IOA;
        } else if (test_ioa_output_SWITCH.equals(option_name)) {
          output_style = OutputFormat.IOA;
          PrintInvariants.test_output = true;
        } else if (java_output_SWITCH.equals(option_name)) {
          output_style = OutputFormat.JAVA;
        } else if (jml_output_SWITCH.equals(option_name)) {
          output_style = OutputFormat.JML;
        } else if (dbc_output_SWITCH.equals(option_name)) {
          output_style = OutputFormat.DBCJAVA;
        } else if (mem_stat_SWITCH.equals(option_name)) {
          use_mem_monitor = true;
        } else if (output_num_samples_SWITCH.equals(option_name)) {
          output_num_samples = true;
        } else if (config_SWITCH.equals(option_name)) {
          String config_file = g.getOptarg();
          try {
            InputStream stream = new FileInputStream(config_file);
            Configuration.getInstance().apply(stream);
          } catch (IOException e) {
            throw new RuntimeException("Could not open config file " + config_file);
          }
          break;
        } else if (config_option_SWITCH.equals(option_name)) {
          String item = g.getOptarg();
          Configuration.getInstance().apply(item);
          break;
        } else if (files_from_SWITCH.equals(option_name)) {
          try {
            BufferedReader files_from = UtilMDE.BufferedFileReader(g.getOptarg());
            String filename;
            while ((filename = files_from.readLine()) != null) {
              // Ignore blank lines in file.
              if (filename.equals("")) {
                continue;
              }
              // This code is duplicated below outside the options loop.
              // These aren't "endsWith()" because there might be a suffix
              // on the end (eg, a date).
              File file = new File(filename);
              if (! file.exists()) {
                throw new Error("File " + filename + " not found.");
              }
              if (filename.indexOf(".decls") != -1) {
                decl_files.add(file);
              } else if (filename.indexOf(".dtrace") != -1) {
                dtrace_files.add(file);
              } else if (filename.indexOf(".spinfo") != -1) {
                spinfo_files.add(file);
              } else if (filename.indexOf(".map") != -1) {
                map_files.add(file);
              } else {
                throw new Error("Unrecognized file extension: " + filename);
              }
            }
          } catch (IOException e) {
            throw new RuntimeException("Error reading --files_from file");
          }
          break;
        } else if (noversion_SWITCH.equals(option_name)) {
          noversion_output = true;
        } else if (omit_from_output_SWITCH.equals(option_name)) {
          String f = g.getOptarg();
          for (int i = 0; i < f.length(); i++) {
            if ("0rs".indexOf(f.charAt(i)) == -1)
              throw new RuntimeException("omit_from_output flag letter '"
                                         + f.charAt(i) + "' is unknown");
            omit_types[f.charAt(i)] = true;
          }
          omit_from_output = true;
        } else {
          throw new RuntimeException("Unknown long option received: " + option_name);
        }
        break;
      case 'h':
        System.out.println(usage);
        System.exit(1);
        break;
      case 'o':
        if (inv_file != null)
          throw new Error("multiple serialization output files supplied on command line");

        String inv_filename = g.getOptarg();
        inv_file = new File(inv_filename);

        if (! UtilMDE.canCreateAndWrite(inv_file)) {
          throw new Error("Cannot write to file " + inv_file);
        }
        break;
        //
      case '?':
        break; // getopt() already printed an error
        //
      default:
        System.out.print("getopt() returned " + c + lineSep);
        break;
      }
    }

    // This code is duplicated above within the switch processing
    // First check that all the file names are OK, so we don't do lots of
    // processing only to bail out at the end.
    for (int i=g.getOptind(); i<args.length; i++) {
      File file = new File(args[i]);
      // These aren't "endsWith()" because there might be a suffix on the end
      // (eg, a date).
      if (! file.exists()) {
        throw new Error("File " + file + " not found.");
      }
      String filename = file.toString();
      if (filename.indexOf(".decls") != -1) {
        decl_files.add(file);
      } else if (filename.indexOf(".dtrace") != -1) {
        dtrace_files.add(file);
      } else if (filename.indexOf(".spinfo") != -1) {
        spinfo_files.add(file);
      } else if (filename.indexOf(".map") != -1) {
        map_files.add(file);
      } else {
        throw new Error("Unrecognized argument: " + file);
      }
    }

    // Set the fuzzy float comparison ratio.  This needs to be done after
    // any configuration options (which may set the ratio) are processed.
    Global.fuzzy.set_rel_diff (Invariant.dkconfig_fuzzy_ratio);

    // Enable dynamic constants for bottom up only
    if (!dkconfig_df_bottom_up)
      dkconfig_use_dynamic_constant_optimization = false;

    // Setup ppt_max_name based on the specified percentage of ppts to process
    if (dkconfig_ppt_perc != 100) {
      ppt_max_name = setup_ppt_perc (decl_files, dkconfig_ppt_perc);
      System.out.println ("Max ppt name = " + ppt_max_name);
    }

    return new Set[] {
      decl_files,
      dtrace_files,
      spinfo_files,
      map_files,
    };
  }


  ///////////////////////////////////////////////////////////////////////////
  // Read decls, dtrace, etc. files

  private static PptMap load_decls_files(Set decl_files)
  {
    stopwatch.reset();
    try {
      System.out.print("Reading declaration files ");
      PptMap all_ppts = FileIO.read_declaration_files(decl_files);
      if (debugTrace.isLoggable(Level.FINE)) {
        debugTrace.fine ("Initializing partial order");
      }
      fileio_progress.clear();
      System.out.print(" (read ");
      System.out.print(UtilMDE.nplural(decl_files.size(), "decls file"));
      System.out.println(")");
      return all_ppts;
    } catch (IOException e) {
      System.out.println();
      e.printStackTrace();
      throw new Error(e.toString());
    } finally {
      debugProgress.fine ("Time spent on read_declaration_files: " + stopwatch.format());
    }
  }

  private static void load_spinfo_files(PptMap all_ppts,
                                        Set spinfo_files // [File]
                                        )
  {
    stopwatch.reset();
    if (!dkconfig_disable_splitting && spinfo_files.size() > 0) {
      try {
        System.out.print("Reading splitter info files ");
        create_splitters(all_ppts, spinfo_files);
        System.out.print(" (read ");
        System.out.print(UtilMDE.nplural(spinfo_files.size(), "spinfo file"));
        System.out.println(")");
      } catch (IOException e) {
        System.out.println();
        e.printStackTrace();
        throw new Error(e.toString());
      } finally {
        debugProgress.fine ("Time spent on load_spinfo_files: " + stopwatch.format());
      }
    }
  }

  private static void load_map_files(PptMap all_ppts,
                                     Set map_files // [File]
                                     )
  {
    stopwatch.reset();
    if (!dkconfig_disable_splitting && map_files.size() > 0) {
      System.out.print("Reading map (context) files ");
      ContextSplitterFactory.load_mapfiles_into_splitterlist
        (map_files, ContextSplitterFactory.dkconfig_granularity);
      System.out.print(" (read ");
      System.out.print(UtilMDE.nplural(map_files.size(), "map (context) file"));
      System.out.println(")");
      debugProgress.fine ("Time spent on load_map_files: " + stopwatch.format());
    }
  }

  public static void setup_splitters(PptMap all_ppts)
  {
    if (dkconfig_disable_splitting) {
      return;
    }

    for (Iterator itor = all_ppts.pptIterator() ; itor.hasNext() ; ) {
      PptTopLevel ppt = (PptTopLevel) itor.next();

      Splitter[] pconds = null;
      if (SplitterList.dkconfig_all_splitters) {
        pconds = SplitterList.get_all();
      } else {
        pconds = SplitterList.get(ppt.name());
      }
      if (pconds != null) {
        if (Global.debugSplit.isLoggable(Level.FINE)) {
          Global.debugSplit.fine ("Got "
                                  + UtilMDE.nplural(pconds.length, "splitter")
                                  + " for " + ppt.name());
        }
        ppt.addConditions(pconds);
      }
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  // Infer invariants over the trace data

  /**
   * The amount of time to wait between updates of the progress
   * display, measured in milliseconds. A value of -1 means not to
   * print the progress display at all.
   **/
  public static int dkconfig_progress_delay = 1000;

  /**
   * The number of columns of progress information to display. In many
   * Unix shells, this can be set to an appropriate value by
   * --config_option daikon.Daikon.progress_display_width=$COLUMNS
   * .
   **/
  public static int dkconfig_progress_display_width = 80;

  /** A way to output FileIO progress information easily */
  private final static FileIOProgress fileio_progress = new FileIOProgress();
  public static class FileIOProgress extends Thread {
    public FileIOProgress() {
      setDaemon(true);
      pctFmt = NumberFormat.getPercentInstance();
      pctFmt.setMinimumFractionDigits(2);
      pctFmt.setMaximumFractionDigits(2);
      df = DateFormat.getTimeInstance(/*DateFormat.LONG*/);
    }
    /**
     * Clients should set this variable instead of calling Thread.stop(),
     * which is deprecated.  Typically a client calls "display()" before
     * setting this.
     **/
    public boolean shouldStop = false;
    private static NumberFormat pctFmt;
    private DateFormat df;
    public void run() {
      if (dkconfig_progress_delay == -1)
        return;
      while (true) {
        if (shouldStop) {
          clear();
          return;
        }
        display();
        try {
          sleep(dkconfig_progress_delay);
        } catch (InterruptedException e) {
          // hmm
        }
      }
    }
    /** Clear the display; good to do before printing to System.out. **/
    public void clear() {
      if (dkconfig_progress_delay == -1)
        return;
      // "display("");" is wrong becuase it leaves the timestamp and writes
      // spaces across the screen.
      String status = "";
      while (status.length() < dkconfig_progress_display_width - 1)
        status += " ";
      System.out.print("\r" + status);
      System.out.print("\r");   // return to beginning of line
      System.out.flush();
    }
    /**
     * Displays the current status.
     * Call this if you don't want to wait until the next automatic display.
     **/
    public void display() {
      if (dkconfig_progress_delay == -1)
        return;
      display(message());
    }
    /** Displays the given message. **/
    public void display(String message) {
      if (dkconfig_progress_delay == -1)
        return;
      String status = "[" + (df.format(new Date())) + "]: " + message;
      if (status.length() > dkconfig_progress_display_width - 1)
        status = status.substring(0, dkconfig_progress_display_width - 1);
      while (status.length() < dkconfig_progress_display_width - 1)
        status += " ";
      System.out.print("\r" + status);
      System.out.flush();
      // System.out.println (status);

      if (debugTrace.isLoggable(Level.FINE)) {
        debugTrace.fine ("Free memory: " + java.lang.Runtime.getRuntime().freeMemory());
        debugTrace.fine ("Used memory: " +
                         (java.lang.Runtime.getRuntime().totalMemory()
                          - java.lang.Runtime.getRuntime().freeMemory()));
        debugTrace.fine ("Active slices: " + FileIO.data_num_slices);
      }
    }
    private String message() {
      File file = FileIO.data_trace_filename;
      if (file == null) {
        if (Dataflow.progress == null) {
          return "[no status]";
        } else {
          return Dataflow.progress;
        }
      }
      LineNumberReader lnr = FileIO.data_trace_reader;
      String line;
      if (lnr == null) {
        line = "?";
      } else {
        long lineNum = lnr.getLineNumber();
        line = String.valueOf(lineNum);
        if (FileIO.data_trace_total_lines > 0) {
          double frac = lineNum / (double)FileIO.data_trace_total_lines;
          String percent = pctFmt.format(frac);
          line = line + ", " + percent;
        }
      }
      return "Reading " + file.getName() + " (line " + line + ") ...";
    }
  }

  /**
   * The main data-processing routine of the daikon engine.  At this
   * point, the decls and spinfo files have been loaded, all of the
   * program points have been setup, and candidate invariants have
   * been instantiated.  This routine processes data to falsify the
   * candidate invariants.
   **/
  private static void process_data(PptMap all_ppts,
                                   Set dtrace_files)
  {
    MemMonitor monitor = null;
    if (use_mem_monitor) {
      monitor = new MemMonitor("stat.out");
      new Thread((Runnable) monitor).start();
    }

    stopwatch.reset();

    // Preprocessing
    setupEquality (all_ppts);
    setup_NISuppression();

    // Processing (actually using dtrace files)
    try {
      fileio_progress.clear();
      System.out.println("Processing trace data; reading "
                         + UtilMDE.nplural(dtrace_files.size(), "dtrace file")
                         + ":");
      FileIO.read_data_trace_files(dtrace_files, all_ppts);
      fileio_progress.shouldStop = true;
      // Final update, so "100%", not "99.70%", is the last thing printed.
      fileio_progress.display();
      System.out.println();
      // System.out.print("Creating implications "); // XXX untested code
      // for (Iterator itor = all_ppts.pptIterator() ; itor.hasNext() ; ) {
      //   PptTopLevel ppt = (PptTopLevel) itor.next();
      //   System.out.print('.');
      //   ppt.addImplications();
      // }
      // System.out.println();
    } catch (IOException e) {
      System.out.println();
      e.printStackTrace();
      throw new Error(e.toString());
    } finally {
      debugProgress.fine ("Time spent on read_data_trace_files: " + stopwatch.format());
    }

    if (monitor != null) {
      monitor.stop();
    }

    if (FileIO.dkconfig_read_samples_only) {
      Fmt.pf ("Finished reading %s samples", "" + FileIO.samples_processed);
      System.exit (0);
    }

//     if (debugStats.isLoggable (Level.FINE)) {
//       PptSliceEquality.print_equality_stats (debugStats, all_ppts);
//       if (false) {
//         for (Iterator i = all_ppts.pptIterator(); i.hasNext(); ) {
//           PptTopLevel ppt = (PptTopLevel) i.next();
//           if (ppt.name().indexOf ("EXIT42") >= 0) {
//             System.out.println (ppt.name() + " After processing data");
//             ppt.print_suppressed_invs (debugStats);
//           }
//         }
//       }
//     }

    // Print equality set info
//     for (Iterator i = all_ppts.pptIterator(); i.hasNext(); ) {
//       PptTopLevel ppt = (PptTopLevel) i.next();
//       Fmt.pf ("ppt: %s", ppt.name);
//       if ((ppt.equality_view == null) || (ppt.equality_view.invs == null))
//         continue;
//       for (int j = 0; j < ppt.equality_view.invs.size(); j++) {
//         Equality e = (Equality) ppt.equality_view.invs.get(j);
//         Fmt.pf ("    equality set = %s", e);
//       }
//     }

    // Fmt.pf ("printing ternary invariants");
    // PrintInvariants.print_all_ternary_invs (all_ppts);
    // System.exit(0);

    // Postprocessing

    stopwatch.reset();

    // Post process dynamic constants
    if (dkconfig_use_dynamic_constant_optimization) {
      for (Iterator itor = all_ppts.ppt_all_iterator() ; itor.hasNext() ; ) {
        PptTopLevel ppt = (PptTopLevel) itor.next();
        if (ppt.constants != null)
          ppt.constants.post_process();
      }
    }

    // If we are processing dataflow bottom up
    if (dkconfig_df_bottom_up) {

      // Initialize the partial order hierarchy
      debugProgress.fine ("Init Hierarchy ... ");
      PptRelation.init_hierarchy (all_ppts);
      debugProgress.fine ("Init Hierarchy ... done");

      // Calculate invariants at all non-leaf ppts
      if (use_dataflow_hierarchy) {
        debugProgress.fine ("createUpperPpts");
        Dataflow.createUpperPpts (all_ppts);
        debugProgress.fine ("createUpperPpts ... done");
      }
    }

    // Equality data for each PptTopLevel.
    if (Daikon.use_equality_optimization) {
      debugProgress.fine ("Equality Post Process ... ");
      for (Iterator itor = all_ppts.ppt_all_iterator() ; itor.hasNext() ; ) {
        PptTopLevel ppt = (PptTopLevel) itor.next();
        ppt.postProcessEquality();
      }
      debugProgress.fine ("Equality Post Process ... done");
    }

    if (debugEquality.isLoggable (Level.FINE)) {
      for (Iterator itor = all_ppts.ppt_all_iterator() ; itor.hasNext() ; ) {
        PptTopLevel ppt = (PptTopLevel) itor.next();
        debugEquality.fine (ppt.name() +": " + ppt.equality_sets_txt());
      }
    }

    // One more round of suppression for printing
    debugProgress.fine ("Suppress for printing ... ");
    for (Iterator itor = all_ppts.ppt_all_iterator() ; itor.hasNext() ; ) {
      PptTopLevel ppt = (PptTopLevel) itor.next();
      ppt.suppressAll (false);
    }
    debugProgress.fine ("Suppress for printing ... done");

    debugProgress.fine ("Time spent on non-implication postprocessing: " + stopwatch.format());

    // Add implications
    stopwatch.reset();
    fileio_progress.clear();
    System.out.println("Creating implications ");
    debugProgress.fine ("Adding Implications ... ");
    for (Iterator itor = all_ppts.pptIterator() ; itor.hasNext() ; ) {
      PptTopLevel ppt = (PptTopLevel) itor.next();
      // debugProgress.fine ("  Adding Implications for " + ppt.name);
      ppt.addImplications();
    }
    debugProgress.fine ("Time spent adding implications: " + stopwatch.format());

    // debug print suppressed invariants
    if (false && debugStats.isLoggable (Level.FINE)) {
      for (Iterator i = all_ppts.ppt_all_iterator(); i.hasNext(); ) {
        PptTopLevel ppt = (PptTopLevel) i.next();
        if (ppt.name().indexOf ("EXIT42") >= 0) {
          System.out.println (ppt.name() + " After final suppression");
          ppt.print_suppressed_invs (debugStats);
        }
      }
    }
  }

  private static void suppressWithSimplify(PptMap all_ppts) {
    System.out.print("Invoking Simplify to identify redundant invariants");
    System.out.flush();
    stopwatch.reset();
    for (Iterator itor = all_ppts.ppt_all_iterator() ; itor.hasNext() ; ) {
      PptTopLevel ppt = (PptTopLevel) itor.next();
      ppt.mark_implied_via_simplify(all_ppts);
      System.out.print(".");
      System.out.flush();
    }
    System.out.println(stopwatch.format());
  }

  public static void setup_NISuppression() {

    NIS.init_ni_suppression();
  }

  public static void setupEquality (PptMap allPpts) {

    // PptSliceEquality does all the necessary instantiations
    if (Daikon.use_equality_optimization) {

      // Foreach program point
      for (Iterator i = allPpts.pptIterator(); i.hasNext(); ) {
        PptTopLevel ppt = (PptTopLevel) i.next();

        if (dkconfig_df_bottom_up) {
          // Skip points that are not leaves
          if (use_dataflow_hierarchy) {
            if (!ppt.ppt_name.isGlobalPoint()
                && !ppt.ppt_name.isNumberedExitPoint())
              continue;
          }

          // setup equality on the splitters of a point with splitters
          if (ppt.has_splitters()) {
            for (Iterator ii = ppt.cond_iterator(); ii.hasNext(); ) {
              PptConditional ppt_cond = (PptConditional) ii.next();
              ppt_cond.equality_view = new PptSliceEquality(ppt_cond);
              ppt_cond.equality_view.instantiate_invariants();
            }
            if (use_dataflow_hierarchy)
              continue;
          }
        }

        // Create the initial equality sets
        ppt.equality_view = new PptSliceEquality(ppt);
        ppt.equality_view.instantiate_invariants();
      }
    }

  }

  // Not yet used in version 3 (?).
  /* INCR
  public static void add_combined_exits(PptMap ppts) {
    // For each collection of related :::EXITnn ppts, add a new ppt (which
    // will only contain implication invariants).

    Vector new_ppts = new Vector();
    for (Iterator itor = ppts.pptIterator() ; itor.hasNext() ; ) {
      PptTopLevel enter_ppt = (PptTopLevel) itor.next();
      Vector exits = enter_ppt.exit_ppts;
      if (exits.size() > 1) {
        Assert.assertTrue(enter_ppt.ppt_name.isEnterPoint());
        String exit_name = enter_ppt.ppt_name.makeExit().getName();
        Assert.assertTrue(ppts.get(exit_name) == null);
        VarInfo[] comb_vars = VarInfo.arrayclone_simple(Ppt.common_vars(exits));
        PptTopLevel exit_ppt = new PptTopLevel(exit_name, comb_vars);
        // {
        //   System.out.println("Adding " + exit_ppt.name() + " because of multiple expt_ppts for " + enter_ppt.name() + ":");
        //   for (int i=0; i<exits.size(); i++) {
        //     Ppt exit = (Ppt) exits.elementAt(i);
        //     System.out.print(" " + exit.name);
        //   }
        //   System.out.println();
        // }
        new_ppts.add(exit_ppt);
        exit_ppt.entry_ppt = enter_ppt;
        exit_ppt.set_controlling_ppts(ppts);
        for (Iterator exit_itor=exits.iterator() ; exit_itor.hasNext() ; ) {
          PptTopLevel line_exit_ppt = (PptTopLevel) exit_itor.next();
          line_exit_ppt.controlling_ppts.add(exit_ppt);
          VarInfo[] line_vars = line_exit_ppt.var_infos;
          line_exit_ppt.combined_exit = exit_ppt;
          {
            // Indices will be used to extract values from a ValueTuple.
            // ValueTuples do not include static constant values.
            // comb_index doesn't include those, either.

            int[] indices = new int[comb_vars.length];
            int new_len = indices.length;
            int new_index = 0;
            int comb_index = 0;

            // comb_vars never contains static finals but line_vars can:
            // Assert.assertTrue(line_vars.length == comb_vars.length,
            //                   "\nIncorrect number of variables (line=" +
            //                   line_vars.length + ", comb=" + comb_vars.length +
            //                   ") at exit points: " + enter_ppt.name() );
            for (int lv_index=0; lv_index<line_vars.length; lv_index++) {
              if (line_vars[lv_index].isStaticConstant()) {
                continue;
              }
              if (line_vars[lv_index].name == comb_vars[comb_index].name) {
                indices[new_index] = comb_index;
                new_index++;
              }
              comb_index++;
            }
            line_exit_ppt.combined_exit_var_indices = indices;
            Assert.assertTrue(new_index == new_len);

            // System.out.println("combined_exit_var_indices " + line_exit_ppt.name());
            // for (int i=0; i<indices.length; i++) {
            //   // System.out.print(" " + indices[i]);
            //   System.out.print(" " + indices[i] + " " + comb_vars[indices[i]].name);
            // }
            // System.out.println();
          }
        }

        // exit_ppt.num_samples = enter_ppt.num_samples;
        // exit_ppt.num_values = enter_ppt.num_values;
      }
    }

    // System.out.println("add_combined_exits: " + new_ppts.size() + " " + new_ppts);

    // Avoid ConcurrentModificationException by adding after the above loop
    for (int i=0; i<new_ppts.size(); i++) {
      ppts.add((PptTopLevel) new_ppts.elementAt(i));
    }

  }
  */ // [INCR]


  ///////////////////////////////////////////////////////////////////////////
  //

  static public void create_splitters(PptMap all_ppts, Set spinfo_files)
    throws IOException
  {
    for (Iterator i = spinfo_files.iterator(); i.hasNext(); ) {
      File filename = (File) i.next();
      SplitterObject[][] splitterObjectArrays
        = SplitterFactory.read_spinfofile(filename, all_ppts);
      for (int j = 0; j < splitterObjectArrays.length; j++) {
        int numsplitters = splitterObjectArrays[j].length;
        if (numsplitters == 0)
          continue; // Why do we have this entry in the array, anyway? -smcc
        String pptname = splitterObjectArrays[j][0].getPptName();
        Vector splitters = new Vector();
        for (int k = 0; k < numsplitters; k++) {
          if (splitterObjectArrays[j][k].splitterExists()) {
            splitters.addElement(splitterObjectArrays[j][k].getSplitter());
          } else {
            System.out.println(splitterObjectArrays[j][k].getError());
          }
        }

        if (splitters.size() >= 1) {
          // If the pptname is ALL, associate it with all program points.
          if (pptname.equals("ALL")) {
            SplitterList.put(".*", (Splitter[]) splitters.toArray(new Splitter[0]));
          } else {
            SplitterList.put( pptname, (Splitter[])splitters.toArray(new Splitter[0]));
          }
        }
      }
    }
  }

  // Guard the invariants at all PptTopLevels. Note that this changes
  // the contents of the PptTopLevels, and the changes made should
  // probably not be written out to an inv file (save the file before
  // this is called).
  public static void guardInvariants(PptMap allPpts) {
    for (Iterator i=allPpts.asCollection().iterator(); i.hasNext(); ) {
      PptTopLevel ppt = (PptTopLevel)i.next();
      if (ppt.num_samples() == 0)
        continue;
      // Make sure isDerivedParam is set before guarding.  Otherwise
      // we'll never get it correct.
      for (int iVarInfo = 0; iVarInfo < ppt.var_infos.length; iVarInfo++) {
        boolean temp = ppt.var_infos[iVarInfo].isDerivedParamAndUninteresting();
      }

      ppt.guardInvariants();
    }
  }

  private static void processOmissions(PptMap allPpts) {
    if (omit_types['0'])
      allPpts.removeUnsampled();
    for (Iterator i = allPpts.asCollection().iterator(); i.hasNext(); ) {
      PptTopLevel ppt = (PptTopLevel)i.next();
      ppt.processOmissions(omit_types);
    }
  }

  /**
   * Returns the max ppt that corresponds the specified percentage
   * of ppts (presuming that only those ppts <= max_ppt will be
   * processed)
   */
  private static String setup_ppt_perc (Collection decl_files, int ppt_perc) {

    // Make sure the percentage is valid
    if ((ppt_perc < 1) || (ppt_perc > 100))
      throw new Error ("ppt_perc of " + ppt_perc + " is out of range 1..100");
    if (ppt_perc == 100)
      return null;

    // Keep track of all of the ppts in a set ordered by the ppt name
    Set ppts = new TreeSet();

    // Read all of the ppt names out of the decl files
    try {
      for (Iterator i = decl_files.iterator(); i.hasNext(); ) {
        File file = (File) i.next();

        // Open the file
        LineNumberReader fp = UtilMDE.LineNumberFileReader(file);

        // Read each ppt name from the file
        for (String line = fp.readLine(); line != null; line = fp.readLine()) {
          if (line.equals("") || FileIO.isComment(line))
            continue;
          if (!line.equals ("DECLARE"))
            continue;
          String ppt_name = fp.readLine();
          ppts.add (ppt_name);
        }

        fp.close();
      }
    } catch (IOException e) {
      e.printStackTrace();
      throw new Error (e);
    }

    // Determine the ppt_name that matches the specified percentage.  Always
    // return the last exit point from the method (so we don't get half the
    // exits from a method or enters without exits, etc)
    int ppt_cnt = (ppts.size() * ppt_perc) / 100;
    if (ppt_cnt == 0)
      throw new Error ("ppt_perc of " + ppt_perc + " over " + ppts.size()
                       + " results in 0 ppts to process");
    for (Iterator i = ppts.iterator(); i.hasNext(); ) {
      String ppt_name = (String) i.next();
      if (--ppt_cnt <= 0) {
        String last_ppt_name = ppt_name;
        while (i.hasNext()) {
          ppt_name = (String) i.next();
          if ((last_ppt_name.indexOf ("EXIT") != -1)
              && (ppt_name.indexOf ("EXIT") == -1))
            return (last_ppt_name);
          last_ppt_name = ppt_name;
        }
        return (ppt_name);
      }
    }
    throw new Error ("ppt_cnt " + ppt_cnt + " ppts.size " + ppts.size());
  }
}
