package daikon;
import daikon.*;
import daikon.inv.*;
import utilMDE.*;
import java.util.*;
import java.io.*;
import gnu.getopt.*;
import java.util.logging.Logger;
import java.util.logging.Level;
/**
 * DaikonSimple reads a declaration file and trace file and outputs a list of likely invariants
 * using the simple incremental algorithm.  
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

	public static void main(String[] args)
		throws IOException, FileNotFoundException {

		//set up logs
		daikon.LogHelper.setupLogs(daikon.LogHelper.INFO);
		
		//no optimizations used in the simple incremental algorithm
		Daikon.use_equality_optimization = false;
		Daikon.dkconfig_use_dynamic_constant_optimization = false;
		Daikon.dkconfig_use_suppression_optimization = false;
		Daikon.suppress_implied_controlled_invariants = false;
		
		// Read command line options
		Set[] files = read_options(args);
		Assert.assertTrue(files.length == 4);
		Set decls_files = files[0]; // [File]
		Set dtrace_files = files[1]; // [File]
		Set spinfo_files = files[2]; // [File]
		Set map_files = files[3]; // [File]
		if ((decls_files.size() == 0) && (dtrace_files.size() == 0)) {
			System.out.println("No .decls or .dtrace files specified");
			System.exit(1);
		}
		
		// Load declarations
		PptMap all_ppts = FileIO.read_declaration_files(decls_files);
		System.out.println(lineSep);
		
		//	adding orig and derived variables                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
		init_partial_order(all_ppts);
		if (debug.isLoggable(Level.FINE)) {
			debug.fine("Partial order initialized");
		}
					  
		// Read and process the data trace files
		SimpleProcessor processor = new SimpleProcessor();
		FileIO.read_data_trace_files(dtrace_files, all_ppts, processor);
		
		//	Write serialized output 
		if (inv_file != null) {
			try {
				FileIO.write_serialized_pptmap(all_ppts, inv_file);
			} catch (IOException e) {
				throw new RuntimeException(
					"Error while writing .inv file '"
						+ inv_file
						+ "': "
						+ e.toString());
			}
		}
		
		//print out the invariants for each program point
		Iterator it = all_ppts.ppt_all_iterator();
		while (it.hasNext()) {
			PptTopLevel ppt = (PptTopLevel) it.next();
			System.out.println(
				"====================================================");
			System.out.println(ppt.name());
			Iterator i = ppt.invariants_iterator();
			while (i.hasNext()) {
				System.out.println(i.next());
			}
		}

			//using print_invariants will add invariant filtering
		//	PrintInvariants.print_invariants(all_ppts);
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
				new LongOpt(
					Daikon.debugAll_SWITCH,
					LongOpt.NO_ARGUMENT,
					null,
					0),
				new LongOpt(
					Daikon.debug_SWITCH,
					LongOpt.REQUIRED_ARGUMENT,
					null,
					0),
				new LongOpt(
					Daikon.ppt_regexp_SWITCH,
					LongOpt.REQUIRED_ARGUMENT,
					null,
					0),
				new LongOpt(
					Daikon.track_SWITCH,
					LongOpt.REQUIRED_ARGUMENT,
					null,
					0),
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
						System.exit(1);
					} else if (
						Daikon.config_option_SWITCH.equals(option_name)) {
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
							System.out.println(
								"Error parsing track argument '"
									+ g.getOptarg()
									+ "' - "
									+ error);
							System.exit(1);
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
					System.exit(1);
					break;
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
		// If we are doing bottom up as opposed to top down
		if (Daikon.dkconfig_df_bottom_up) {

			// Create combined exit points
			Dataflow.create_combined_exits(all_ppts);

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
				origvar.comparability =
					postvar.comparability.makeAlias(origvar.name);

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
	   if (debug.isLoggable(Level.FINE))
		 debug.fine ("instantiate_views_and_invariants for " + ppt.name());

	   // Now make all of the views (and thus candidate invariants)
	   instantiate_views(0, ppt.var_infos.length, ppt);

	   if (debug.isLoggable(Level.FINE))
		 debug.fine ("Done with instantiate_views_and_invariants");
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

	  private static void instantiate_views(int vi_index_min,
									 int vi_index_limit, PptTopLevel ppt) {
		if (Global.debugInfer.isLoggable(Level.FINE))
		  Global.debugInfer.fine ("instantiate_views: " + ppt.name
							   + ", vi_index_min=" + vi_index_min
							   + ", vi_index_limit=" + vi_index_limit
							   + ", var_infos.length=" + ppt.var_infos.length);

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
		//no restrictions on unary invariants in the simple algorithm
		Vector unary_views = new Vector(vi_index_limit-vi_index_min);
		for (int i=vi_index_min; i<vi_index_limit; i++) {
		  VarInfo vi = ppt.var_infos[i];
		  PptSlice1 slice1 = new PptSlice1(ppt, vi);
		  slice1.instantiate_invariants();
		  unary_views.add(slice1);
		}
		ppt.addViews(unary_views);
		unary_views = null;


		/// 2. all binary views

		// Binary slices/invariants.
		Vector binary_views = new Vector();
		for (int i1=0; i1<vi_index_limit; i1++) {
		  VarInfo var1 = ppt.var_infos[i1];
		 
		  // Eventually, add back in this test as "if constant and no
		  // comparability info exists" then continue.
		  // if (var1.isStaticConstant()) continue;
		  boolean target1 = (i1 >= vi_index_min) && (i1 < vi_index_limit);
		  int i2_min = (target1 ? i1 : Math.max(i1, vi_index_min));
		  for (int i2=i2_min; i2<vi_index_limit; i2++) {
			VarInfo var2 = ppt.var_infos[i2];
			if (!(Debug.logOn() || debug_on)) {
				   continue;
				 }
			// Eventually, add back in this test as "if constant and no
			// comparability info exists" then continue.
			// if (var2.isStaticConstant()) continue;
			if (var1 == var2) {
			  if (Debug.logOn() || debug_on)
				Debug.log (debug, ppt.getClass(), ppt, Debug.vis (var1, var2),
						   "Binary slice not created, is_slice_ok == false");
			  continue;
			}
			PptSlice2 slice2 = new PptSlice2(ppt, var1, var2);
			if (Debug.logOn() || debug_on)
			  Debug.log (debug, ppt.getClass(), slice2, "Creating binary slice");

			slice2.instantiate_invariants();
			binary_views.add(slice2);
		  }
		}
		ppt.addViews(binary_views);
		binary_views = null;

		// 3. all ternary views
		if (Global.debugInfer.isLoggable(Level.FINE)) {
		  Global.debugInfer.fine ("Trying ternary slices for " + ppt.name());
		}

		Vector ternary_views = new Vector();
		for (int i1=0; i1<vi_index_limit; i1++) {
		  VarInfo var1 = ppt.var_infos[i1];
		  if (!(Debug.logOn() || debug_on))
			continue;

		  // Eventually, add back in this test as "if constant and no
		  // comparability info exists" then continue.
		  // if (var1.isStaticConstant()) continue;
		  // For now, only ternary invariants not involving any arrays
		  if (var1.rep_type.isArray() && (!Debug.logOn() || debug_on))
			continue;

		  boolean target1 = (i1 >= vi_index_min) && (i1 < vi_index_limit);
		  for (int i2=i1; i2<vi_index_limit; i2++) {
			VarInfo var2 = ppt.var_infos[i2];
			if (!(Debug.logOn() || debug_on))
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
			  VarInfo var3 = ppt.var_infos[i3];

			  if (!is_slice_ok (var1, var2, var3))
				continue;

			  PptSlice3 slice3 = new PptSlice3(ppt, var1, var2, var3);
			  slice3.instantiate_invariants();
			  if (Debug.logOn() || debug_on)
				Debug.log (debug, ppt.getClass(), slice3, "Created Ternary Slice");
			  ternary_views.add(slice3);
			}
		  }
		}
		ppt.addViews(ternary_views);

		if (debug.isLoggable(Level.FINE))
		  debug.fine (ppt.numViews() - old_num_views + " new views for " + ppt.name());

		// This method didn't add any new variables.
		Assert.assertTrue(old_num_vars == ppt.var_infos.length);
		ppt.repCheck();
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
	 public static boolean is_slice_ok (VarInfo v1, VarInfo v2, VarInfo v3) {

	
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
		 return (false);
	   }
	   Assert.assertTrue
		 (v2.file_rep_type.isIntegral() || v2.file_rep_type.isFloat());
	   Assert.assertTrue
		 (v3.file_rep_type.isIntegral() || v3.file_rep_type.isFloat());

	   // Don't create a reflexive slice (all vars the same) if there are
	   // only two vars in the equality set
		if ((v1 == v2) && (v2 == v3))
		 return (false);

	   // Don't create a partially reflexive slice (two vars the same) if there
	   // is only one variable in its equality set
		if ((v1 == v2) || (v1 == v3))
		   return (false);
		if (v2 == v3)
		   return (false);
	 
	   return (true);
	 }	  
	private static class SimpleProcessor extends FileIO.Processor {
		PptMap all_ppts = null;

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
			add(ppt, vt);
		}
		
		private void add(PptTopLevel ppt, ValueTuple vt) {
			// if this is a numbered exit, apply to the combined exit as well
			if (!(ppt instanceof PptConditional)
				&& ppt.ppt_name.isNumberedExitPoint()) {
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
			}
			
			//manually inc the sample number because i don't use of ppt's add methods
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
						for (int a = 0; a < slice.invs.size(); a++) {
							Invariant inv = (Invariant) slice.invs.get(a);
							result.add(inv);
						}
						missing = true;
						Iterator y = result.iterator();
						while (y.hasNext()) {
							Invariant inv = (Invariant) y.next();
							slice.removeInvariant(inv);
						}
						break;
					}
				
					//	If any variables are missing, skip this slice
					if (v.isMissing(vt)) {
						missing = true;
						break;
					}
				}
				
				//keep a list of the falsified invariants
				if (!missing) {
					while (k.hasNext()) {
						Invariant inv = (Invariant) k.next();
						//	Invariant pre_inv = (Invariant) inv.clone();
						InvariantStatus status = inv.add_sample(vt, 1);
						if (status == InvariantStatus.FALSIFIED) {
							to_remove.add(inv);
						}
					}
					
					//remove the falsified invariants from the slice
					//must do this separate from the searching to avoid concurrentModification exception
					Iterator y = to_remove.iterator();
					while (y.hasNext()) {
						slice.removeInvariant((Invariant) y.next());
					}
				}
			}
		}
	}
}
