// Main routine for Daikon invariant detector
// For documentation, see file doc/daikon.html in the distribution.

package daikon;

import daikon.split.*;
import daikon.split.misc.*;
import daikon.inv.Invariant;
import daikon.config.Configuration;

import java.util.*;
import java.io.*;
import java.lang.Thread;

import org.apache.oro.text.regex.*;
import org.apache.log4j.Category;
import gnu.getopt.*;
import utilMDE.*;

public final class Daikon {
  public static final String lineSep = Global.lineSep;

  public static boolean dkconfig_output_conditionals = true;

  // All these variables really need to be organized better.

  public final static boolean disable_splitting = false;

  public static boolean disable_ternary_invariants = false;

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
  // Has no effect unless no_text_output is "false".
  public static boolean show_progress = false;

  // When true, don't print invariants when their controlling ppt
  // already has them.  For example, this is the case for invariants
  // in public methods which are already given as part of the object
  // invariant for that class.
  public static boolean suppress_implied_controlled_invariants = false;

  // When true, don't print EXIT invariants over strictly orig()
  // variables when the corresponding entry ppt already has the
  // invariant.
  public static boolean suppress_implied_postcondition_over_prestate_invariants = false;

  // When true, use the Simplify theorem prover (not part of Daikon)
  // to locate logically redundant invariants, and flag them as
  // redundant, so that they are removed from the printed output.
  public static boolean suppress_redundant_invariants_with_simplify = false;

  // Set what output style to use.  NORMAL is the default; ESC style
  // is based on JML; SIMPLIFY style uses first order logical
  // expressions with lots of parens
  public static final int OUTPUT_STYLE_NORMAL = 0;
  public static final int OUTPUT_STYLE_ESC = 1;
  public static final int OUTPUT_STYLE_SIMPLIFY = 2;
  public static final int OUTPUT_STYLE_IOA = 3;
  public static int output_style = OUTPUT_STYLE_NORMAL;
  // public static int output_style = OUTPUT_STYLE_ESC;
  // public static int output_style = OUTPUT_STYLE_SIMPLIFY;

  // When true, output numbers of values and samples (also names of variables)
  public static boolean output_num_samples = false;

  public static boolean ignore_comparability = false;

  // Controls which program points/variables are used/ignored.
  public static Pattern ppt_regexp;
  public static Pattern ppt_omit_regexp;
  public static Pattern var_omit_regexp;

  // The invariants detected will be serialized and written to this
  // file
  public static File inv_file;

  // Whether we want the memory monitor activated
  private static boolean use_mem_monitor = false;

  // Public so other programs can reuse the same command-line options
  public static final String help_SWITCH = "help";
  public static final String ppt_regexp_SWITCH = "ppt";
  public static final String ppt_omit_regexp_SWITCH = "ppt_omit";
  public static final String var_omit_regexp_SWITCH = "var_omit";
  public static final String no_text_output_SWITCH = "no_text_output";
  public static final String show_progress_SWITCH = "show_progress";
  public static final String suppress_cont_SWITCH = "suppress_cont";
  public static final String suppress_post_SWITCH = "suppress_post";
  public static final String suppress_redundant_SWITCH = "suppress_redundant";
  public static final String prob_limit_SWITCH = "prob_limit";
  public static final String esc_output_SWITCH = "esc_output";
  public static final String ioa_output_SWITCH = "ioa_output";
  public static final String mem_stat_SWITCH = "mem_stat";
  public static final String simplify_output_SWITCH = "simplify_output";
  public static final String output_num_samples_SWITCH = "output_num_samples";
  public static final String noternary_SWITCH = "noternary";
  public static final String config_SWITCH = "config";
  public static final String debugAll_SWITCH = "debug";
  public static final String debug_SWITCH = "dbg";


  // A pptMap which contains all the Program Points
  public static PptMap all_ppts;

  public static final Category debugTrace = Category.getInstance (Daikon.class.getName());

  static String usage =
    UtilMDE.join(new String[] {
      "Daikon invariant detector.",
      "Copyright 1998-2001 by Michael Ernst <mernst@lcs.mit.edu>",
      "Usage:",
      "    java daikon.Daikon [flags...] files...",
      "  Each file is a declaration file or a data trace file; the file type",
      "  is determined by the file name (containing \".decls\" or \".dtrace\").",
      "  For a list of flags, see the Daikon manual, which appears in the ",
      "  Daikon distribution and also at http://pag.lcs.mit.edu/daikon/."},
                 lineSep);

  /**
   * The arguments to daikon.Daikon are file names; declaration file names end
   * in ".decls" and data trace file names end in ".dtrace".
   **/
  public static void main(String[] args)
  {
    {
      System.err.
	print("**************************************************\n" +
	      "*                     WARNING                    *\n" +
	      "**************************************************\n" +
	      "* You are using the REDESIGN version (V3) of the *\n" +
	      "* Daikon engine. Make sure this is what you want.*\n" +
	      "**************************************************\n");
    }

    // Read command line options
    Set[] files = read_options(args);
    Assert.assert(files.length == 3);

    // Set up debug traces
    Logger.setupLogs (Global.debugAll ? Logger.DEBUG : Logger.INFO);


    // Load all data
    PptMap all_ppts = load_files(files[0], files[1], files[2]);

    // Infer invariants
    do_inference(all_ppts);

    // Display invariants
    if (output_num_samples) {
      System.out.println("The --output_num_samples debugging flag is on.");
      System.out.println("Some of the debugging output may only make sense to Daikon programmers.");
    }
    print_invariants(all_ppts);
    if (output_num_samples) {
      Global.output_statistics();
    }

    // Write serialized output
    if (inv_file != null) {
      try {
	FileIO.write_serialized_pptmap(all_ppts, inv_file);
      } catch (IOException e) {
	throw new RuntimeException("Error while writing .inv file "
				   + "'" + inv_file + "': " + e.toString());
      }
    }

    // Done
    System.out.println("Exiting");
  }


  ///////////////////////////////////////////////////////////////////////////
  // Read in the command line options
  // Return an array of {decls, dtrace, spinfo} filenames
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

    LongOpt[] longopts = new LongOpt[] {
      new LongOpt(help_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(ppt_regexp_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
      new LongOpt(ppt_omit_regexp_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
      new LongOpt(var_omit_regexp_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
      new LongOpt(no_text_output_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(show_progress_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(suppress_cont_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(suppress_post_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(suppress_redundant_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(prob_limit_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
      new LongOpt(esc_output_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(simplify_output_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(ioa_output_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(mem_stat_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(output_num_samples_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(noternary_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(config_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
      new LongOpt(debugAll_SWITCH, LongOpt.NO_ARGUMENT, null, 0),
      new LongOpt(debug_SWITCH, LongOpt.REQUIRED_ARGUMENT, null, 0),
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
	  Logger.setPriority (g.getOptarg(), Logger.DEBUG);
	} else if (no_text_output_SWITCH.equals(option_name)) {
	  no_text_output = true;
	} else if (show_progress_SWITCH.equals(option_name)) {
	  show_progress = true;
	} else if (suppress_cont_SWITCH.equals(option_name)) {
	  suppress_implied_controlled_invariants = true;
	} else if (suppress_post_SWITCH.equals(option_name)) {
	  suppress_implied_postcondition_over_prestate_invariants = true;
	} else if (suppress_redundant_SWITCH.equals(option_name)) {
	  suppress_redundant_invariants_with_simplify = true;
	} else if (prob_limit_SWITCH.equals(option_name)) {
	  Invariant.probability_limit = 0.01 * Double.parseDouble(g.getOptarg());
	} else if (esc_output_SWITCH.equals(option_name)) {
	  output_style = OUTPUT_STYLE_ESC;
	} else if (simplify_output_SWITCH.equals(option_name)) {
	  output_style = OUTPUT_STYLE_SIMPLIFY;
	} else if (ioa_output_SWITCH.equals(option_name)) {
	  output_style = OUTPUT_STYLE_IOA;
	} else if (mem_stat_SWITCH.equals(option_name)) {
	  use_mem_monitor = true;
	} else if (output_num_samples_SWITCH.equals(option_name)) {
	  output_num_samples = true;
	} else if (noternary_SWITCH.equals(option_name)) {
	  disable_ternary_invariants = true;
        } else if (config_SWITCH.equals(option_name)) {
	  String config_file = g.getOptarg();
	  try {
	    InputStream stream = new FileInputStream(config_file);
	    Configuration.getInstance().apply(stream);
	  } catch (IOException e) {
	    throw new RuntimeException("Could not open config file " + config_file);
	  }
          break;
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
        System.out.println("Inv filename = " + inv_filename);
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

    // First check that all the file names are OK, so we don't do lots of
    // processing only to bail out at the end.
    for (int i=g.getOptind(); i<args.length; i++) {
      String arg = args[i];
      // These aren't "endsWith()" because there might be a suffix on the end
      // (eg, a date).
      if (! new File(arg).exists()) {
        throw new Error("File " + arg + " not found.");
      }
      if (arg.indexOf(".decls") != -1) {
        decl_files.add(arg);
      } else if (arg.indexOf(".dtrace") != -1) {
        dtrace_files.add(arg);
      } else if (arg.indexOf(".spinfo") != -1) {
	spinfo_files.add(arg);
      } else {
        throw new Error("Unrecognized argument: " + arg);
      }
    }

    return new Set[] {
      decl_files,
      dtrace_files,
      spinfo_files,
    };
  }


  ///////////////////////////////////////////////////////////////////////////
  // Read decls, dtrace, etc. files
  private static PptMap load_files(Set decl_files,
				   Set dtrace_files,
				   Set spinfo_files)
  {
    PptMap all_ppts = new PptMap();

    int num_decl_files = decl_files.size();
    int num_dtrace_files = dtrace_files.size();
    int num_spinfo_files = spinfo_files.size();

    try {
      System.out.print("Reading declaration files ");
      FileIO.read_declaration_files(decl_files, all_ppts);
      System.out.println();
      add_combined_exits(all_ppts);

      if (!disable_splitting && num_spinfo_files > 0) {
	System.out.println("Reading Splitter Info files ");
	create_splitters(all_ppts, spinfo_files);
      }

      System.out.print("Reading data trace files ");
      FileIO.read_data_trace_files(dtrace_files, all_ppts);
      System.out.println();
    } catch (IOException e) {
      System.out.println();
      e.printStackTrace();
      throw new Error(e.toString());
    }
    // Jikes complains when I put this all in one big string.
    System.out.print("Read " + UtilMDE.nplural(num_decl_files, "declaration file"));
    System.out.print(", " + UtilMDE.nplural(num_spinfo_files, "spinfo file"));
    System.out.println(", " + UtilMDE.nplural(num_dtrace_files, "dtrace file"));

    // Old location; but we want to add these before reading trace files.
    // add_combined_exits(all_ppts);

    return all_ppts;
  }


  ///////////////////////////////////////////////////////////////////////////
  // Infer invariants over the trace data
  private static void do_inference(PptMap all_ppts)
  {
    // Retrieve Ppt objects in sorted order.
    // Use a custom comparator for a specific ordering
    Comparator comparator = new Ppt.NameComparator();
    TreeSet all_ppts_sorted = new TreeSet(comparator);
    all_ppts_sorted.addAll(all_ppts.asCollection());

    MemMonitor monitor=null;
    if (use_mem_monitor) {
      monitor = new MemMonitor("stat.out");
      new Thread((Runnable) monitor).start();
    }

    for (Iterator itor = all_ppts_sorted.iterator() ; itor.hasNext() ; ) {
      PptTopLevel ppt = (PptTopLevel) itor.next();
      if (ppt.has_samples()) {
	int num_samples = ppt.num_samples();
	int num_array_vars = ppt.num_array_vars();
	int num_scalar_vars = ppt.num_vars() - num_array_vars;
	int num_static_vars = ppt.num_static_constant_vars;
	int num_orig_vars = ppt.num_orig_vars;

        // System.out.println(ppt.name + ": " + ppt.num_samples() + " samples, "
        //                    + ppt.num_values() + " values, "
        //                    + "ratio = " + ((double)ppt.num_samples()) / ((double)ppt.num_values()));
        // System.out.println("start dump-----------------------------------------------------------------");
        // System.out.println(ppt.name);
        // ppt.values.dump();
        // System.out.println("end dump-------------------------------------------------------------------");
	long ppt_start_time = System.currentTimeMillis();
	if (no_text_output && show_progress) {
	  System.out.print(ppt.name + "...");
	  System.out.flush();
	}
        ppt.initial_processing();

	int num_derived_array_vars = ppt.num_array_vars() - num_array_vars;
	int num_derived_scalar_vars = ppt.num_vars() - ppt.num_array_vars() - num_scalar_vars;


	if (no_text_output && show_progress) {
	  System.out.print("...");
	  System.out.flush();
	}
        if (! disable_splitting) {
	  Splitter[] pconds = null;
	  if (SplitterList.dkconfig_all_splitters) {
	    pconds = SplitterList.get_all();
	  } else {
	    pconds = SplitterList.get(ppt.name);
	  }
          if (Global.debugSplit.isDebugEnabled())
            Global.debugSplit.debug("Got " + ((pconds == null)
					   ? "no"
					   : Integer.toString(pconds.length))
				 + " splitters for " + ppt.name);
          if (pconds != null)
            ppt.addConditions(pconds);
        }
        ppt.addImplications();

        {
          // Clear memory
          ppt.set_values_null();
          ppt.clear_view_caches();
          for (int i=0; i<ppt.views_cond.size(); i++) {
            PptConditional pcond = (PptConditional) ppt.views_cond.elementAt(i);
            pcond.set_values_null();
            pcond.clear_view_caches();
          }
        }

	if (monitor!=null) {
	  monitor.end_of_iteration(ppt.name, num_samples, num_static_vars, num_orig_vars, num_scalar_vars, num_array_vars, num_derived_scalar_vars, num_derived_array_vars);
	}
	long ppt_end_time = System.currentTimeMillis();
	if (no_text_output && show_progress) {
	  double elapsed = (ppt_end_time - ppt_start_time) / 1000.0;
	  System.out.println((new java.text.DecimalFormat("#.#")).format(elapsed) + "s");
	}
      }
    }

    if (monitor!=null) {
      monitor.stop();
    }

    if (suppress_redundant_invariants_with_simplify) {
      System.out.print("Invoking Simplify to identify redundant invariants...");
      System.out.flush();
      long start = System.currentTimeMillis();
      for (Iterator itor = all_ppts_sorted.iterator() ; itor.hasNext() ; ) {
	PptTopLevel ppt = (PptTopLevel) itor.next();
	ppt.mark_implied_via_simplify();
      }
      long end = System.currentTimeMillis();
      double elapsed = (end - start) / 1000.0;
      System.out.println((new java.text.DecimalFormat("#.#")).format(elapsed) + "s");
    }

  }


  ///////////////////////////////////////////////////////////////////////////
  //
  public static void print_invariants(PptMap ppts) {
    // Retrieve Ppt objects in sorted order.
    // Use a custom comparator for a specific ordering
    Comparator comparator = new Ppt.NameComparator();
    TreeSet ppts_sorted = new TreeSet(comparator);
    ppts_sorted.addAll(ppts.asCollection());

    for (Iterator itor = ppts_sorted.iterator() ; itor.hasNext() ; ) {
      PptTopLevel ppt = (PptTopLevel) itor.next();
      if (ppt.has_samples() && ! no_text_output) {
        ppt.print_invariants_maybe(System.out, ppts);
      }
    }
  }


  ///////////////////////////////////////////////////////////////////////////
  //
  public static void add_combined_exits(PptMap ppts) {
    // For each collection of related :::EXITnn ppts, add a new ppt (which
    // will only contain implication invariants).

    Vector new_ppts = new Vector();
    for (Iterator itor = ppts.iterator() ; itor.hasNext() ; ) {
      PptTopLevel enter_ppt = (PptTopLevel) itor.next();
      Vector exits = enter_ppt.exit_ppts;
      if (exits.size() > 1) {
        Assert.assert(enter_ppt.ppt_name.isEnterPoint());
        String exit_name = enter_ppt.ppt_name.makeExit().getName();
        Assert.assert(ppts.get(exit_name) == null);
        VarInfo[] comb_vars = VarInfo.arrayclone_simple(Ppt.common_vars(exits));
        PptTopLevel exit_ppt = new PptTopLevel(exit_name, comb_vars);
        // {
        //   System.out.println("Adding " + exit_ppt.name + " because of multiple expt_ppts for " + enter_ppt.name + ":");
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
            /* Assert.assert(line_vars.length == comb_vars.length,
                          "\nIncorrect number of variables (line=" + 
			  line_vars.length + ", comb=" + comb_vars.length + 
			  ") at exit points: " + enter_ppt.name ); */
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
            Assert.assert(new_index == new_len);

            // System.out.println("combined_exit_var_indices " + line_exit_ppt.name);
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

  ///////////////////////////////////////////////////////////////////////////
  //
  static public void create_splitters(PptMap all_ppts, Set spinfo_files)
    throws IOException
  {
    Vector sps = new Vector();
    for (Iterator i = spinfo_files.iterator(); i.hasNext(); ) {
      sps = SplitterFactory.read_spinfofile((String)i.next(), all_ppts);
    }
    int siz = sps.size();
    Assert.assert(java.lang.Math.IEEEremainder(siz, 2) == 0);
    for (int j = 0; j < siz; j+=2) {
      SplitterList.put( (String )sps.elementAt(j), (Splitter[]) sps.elementAt(j+1));
    }
  }
}
