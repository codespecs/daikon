package daikon;

import daikon.derive.*;

import utilMDE.*;
import org.apache.oro.text.regex.*;

import java.io.*;
import java.util.*;
import java.util.zip.GZIPInputStream;

public final class FileIO {

// We get all the declarations before reading any traces because at each
// program point, we need to know the names of all the available variables,
// which includes all global variables (of which these are examples).
//  * Alternative:  in each trace file, declare all the functions before
//    any data are seen
//     + more reasonable for on-line information (can't know when a
//       new declaration would be seen)
//     + obviates the need for this function and extra pass over the data
//     - forces declaration of all functions ahead of time
//     * perhaps not reasonable (at the least, complicated) for dynamic
//       loading of code; but I should know what I've instrumented and can
//       specify all those declaration files; it isn't possible to instrument
//       as part of dynamic loading
//  * Alternative:  go back and update info to insert new "missing" or
//    "zero" values everywhere that the variables weren't yet known about
//     - tricky for online operation



/// Constants

  // If true, prints the unmatched procedure entries verbosely
  private static final boolean VERBOSE_UNMATCHED_PROCEDURE_ENTRIES = false;

  final static String comment_prefix = "//";
  final static String declaration_header = "DECLARE";


  // Program point name tags
  public final static String ppt_tag_separator = ":::";
  public final static String enter_suffix = "ENTER";
  public final static String enter_tag = ppt_tag_separator + enter_suffix;
  // This does not necessarily appear at the end of the program point name;
  // a number may follow it.
  public final static String exit_suffix = "EXIT";
  public final static String exit_tag = ppt_tag_separator + exit_suffix;
  public final static String object_suffix = "OBJECT";
  public final static String object_tag = ppt_tag_separator + object_suffix;
  public final static String class_static_suffix = "CLASS";
  public final static String class_static_tag = ppt_tag_separator + class_static_suffix;


  /// Settings

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.  When false, set modbits
  // to 1 iff the printed representation has changed.  When true, set
  // modbits to 1 if the printed representation has changed; leave
  // other modbits as is.
  public static boolean dkconfig_add_changed = true;


  /// Variables

  // I'm not going to make this static because then it doesn't get restored
  // when reading from a file; and it won't be *that* expensive to add yet
  // one more slot to the Ppt object.
  // static HashMap entry_ppts = new HashMap(); // maps from Ppt to Ppt

  // This hashmap maps every program point to an array, which contains the
  // old values of all variables in scope the last time the program point
  // was executed. This enables us to determine whether the values have been
  // modified since this program point was last executed.
    static HashMap ppt_to_value_reps = new HashMap();

  //for debugging purposes: printing out a modified trace file with changed modbits
  static boolean to_write_nonce = false;
  static String nonce_value, nonce_string;

///////////////////////////////////////////////////////////////////////////
/// Declaration files
///


  /** This is intended to be used only interactively, while debugging. */
  static void reset_declarations() {
    throw new Error("to implement");
  }

  /**
   * Calls @link{read_declaration_file(String)} for each element
   * of files.  See the definition of that function.
   **/
  static void read_declaration_files(Collection files, PptMap all_ppts) {
    for (Iterator i = files.iterator(); i.hasNext(); ) {
      String file = (String) i.next();
      System.out.print(".");  // show progress
      try {
	read_declaration_file(file, all_ppts);
      } catch (IOException e) {
	e.printStackTrace();
	throw new Error(e.toString());
      }
    }
  }


  /**
   * @param filename
   * @return a vector of PptTopLevel objects
   **/
  static Vector read_declaration_file(String filename, PptMap all_ppts) throws IOException {

    if (Global.debugRead)
      System.out.println("read_declaration_file " + filename
                         + ((Daikon.ppt_regexp != null) ? " " + Daikon.ppt_regexp.getPattern() : "")
                         + ((Daikon.ppt_omit_regexp != null) ? " " + Daikon.ppt_omit_regexp.getPattern() : ""));

    Vector new_ppts = new Vector();

    int varcomp_format = VarComparability.NONE;

    LineNumberReader reader = UtilMDE.LineNumberFileReader(filename);

    String line = reader.readLine();

    // line == null when we hit end of file
    for ( ; line != null; line = reader.readLine()) {
      if (Global.debugRead)
	System.out.println("read_declaration_file line: " + line);
      if (line.equals("") || line.startsWith("//") || line.startsWith("#"))
	continue;
      if (line.equals(declaration_header)) {
	PptTopLevel ppt = read_declaration(reader, all_ppts, varcomp_format, filename);
	// ppt can be null if this declaration was skipped because of --ppt or --ppt_omit.
        if (ppt != null) {
          new_ppts.add(ppt);
        }
	continue;
      }
      if (line.equals("VarComparability")) {
        line = reader.readLine();
        if (line.equals("none")) {
          varcomp_format = VarComparability.NONE;
        } else if (line.equals("implicit")) {
          varcomp_format = VarComparability.IMPLICIT;
        } else if (line.equals("explicit")) {
          varcomp_format = VarComparability.EXPLICIT;
        } else {
          throw new Error("Unrecognized VarComparability: " + line);
        }
        continue;
      }

      // Not a declaration.
      // Read the rest of this entry (until we find a blank line).
      if (Global.debugRead)
	System.out.println("Skipping paragraph starting at line " + reader.getLineNumber() + " of file " + filename + ": " + line);
      while ((line != null) && (!line.equals("")) && !line.startsWith("//")) {
	System.out.println("Unrecognized paragraph contains line = `" + line + "'");
	System.out.println("" + (line != null) + " " + (line.equals("")) + " " + !line.startsWith("//"));
	if (line == null)
	  throw new IllegalStateException();
	line = reader.readLine();
      }
      continue;
    }
    for (int i=0; i<new_ppts.size(); i++) {
      PptTopLevel ppt = (PptTopLevel) new_ppts.elementAt(i);
      all_ppts.add(ppt);
    }
    after_processing_file_declarations(new_ppts, all_ppts);
    return new_ppts;
  }


  // The "DECLARE" line has alredy been read.
  static PptTopLevel read_declaration(LineNumberReader file, PptMap all_ppts, int varcomp_format, String filename) throws IOException {
    // We have just read the "DECLARE" line.
    String ppt_name = file.readLine().intern();

    // This program point name has already been encountered.
    if (all_ppts.containsName(ppt_name)) {
      throw new Error("Duplicate declaration of program point " + ppt_name
                      + " found at file " + filename
                      + " line " + file.getLineNumber());
    }

    if (!Global.allSplitters) {
      //if all the splitters are to be tried at all program points,
      //then we need to create all the program points because the
      //creation of splitters requires information from the program
      //points
      if (((Daikon.ppt_omit_regexp != null)
	   && Global.regexp_matcher.contains(ppt_name, Daikon.ppt_omit_regexp))
	  || ((Daikon.ppt_regexp != null)
	      && ! Global.regexp_matcher.contains(ppt_name, Daikon.ppt_regexp))) {
	// Discard this declaration
	// System.out.println("Discarding non-matching program point declaration " + ppt_name);
	String line = file.readLine();
	// This fails if some lines of a declaration (e.g., the comparability
	// field) are empty.
	while ((line != null) && !line.equals("")) {
	  line = file.readLine();
	}
	return null;
      }
    }


    // if (ppt_name.endsWith(":::ENTER"))
    //   functions.add(ppt_name.substring(0, ppt_name.length() - 8));

    Vector var_infos = new Vector();

    // Each iteration reads a variable name, type, and comparability.
    // Possibly abstract this out into a separate function??
    VarInfo vi;
    while ((vi = read_VarInfo(file, varcomp_format, filename, ppt_name)) != null) {
      for (int i=0; i<var_infos.size(); i++) {
        if (vi.name == ((VarInfo)var_infos.elementAt(i)).name) {
          throw new Error("Duplicate variable name " + vi.name
                          + " found at file " + filename
                          + " line " + file.getLineNumber());
        }
      }
      // Can't do this test in read_VarInfo, it seems, because of the test
      // against null above.
      if ((Daikon.var_omit_regexp != null)
          && Global.regexp_matcher.contains(vi.name.name(), Daikon.var_omit_regexp)) {
        continue;
      }
      var_infos.add(vi);
    }

    VarInfo[] vi_array = (VarInfo[]) var_infos.toArray(new VarInfo[0]);
    return new PptTopLevel(ppt_name, vi_array);
  }


  /**
   * Read a variable name, type, and comparability; construct a VarInfo.
   * Return null after reading the last variable in this program point
   * declaration.
   **/
  static VarInfo read_VarInfo(LineNumberReader file, int varcomp_format, String filename, String ppt_name) throws IOException {
    String line = file.readLine();
    if ((line == null) || (line.equals("")))
      return null;
    String varname = line;
    String proglang_type_string = file.readLine();
    String file_rep_type_string = file.readLine();
    String comparability_string = file.readLine();
    if ((varname == null) || (proglang_type_string == null) || (file_rep_type_string == null) || (comparability_string == null))
      throw new Error("End of file " + filename + " while reading variable " + varname + " in declaration of program point " + ppt_name);
    int equals_index = file_rep_type_string.indexOf(" = ");
    String static_constant_value_string = null;
    Object static_constant_value = null;
    boolean is_static_constant = false;
    if (equals_index != -1) {
      is_static_constant = true;
      static_constant_value_string = file_rep_type_string.substring(equals_index+3);
      file_rep_type_string = file_rep_type_string.substring(0, equals_index);
    }
    ProglangType prog_type = ProglangType.parse(proglang_type_string);
    ProglangType file_rep_type = ProglangType.rep_parse(file_rep_type_string);
    ProglangType rep_type = file_rep_type.fileTypeToRepType();
    if (static_constant_value_string != null) {
      static_constant_value = rep_type.parse_value(static_constant_value_string);
      // Why can't the value be null?
      Assert.assert(static_constant_value != null);
    }
    VarComparability comparability
      = VarComparability.parse(varcomp_format, comparability_string, prog_type);
    // Not a call to assert.assert in order to avoid doing the (expensive)
    // string concatenations.
    if (! VarInfo.legalFileRepType(file_rep_type)) {
      throw new Error("Unsupported representation type " + file_rep_type.format() + " for variable " + varname + " at line " + file.getLineNumber() + " of file " + filename);
    }

    return new VarInfo(VarInfoName.parse(varname), prog_type, file_rep_type, comparability, is_static_constant, static_constant_value);
  }

  static final class Invocation {
    String fn_name;		// not interned:  not worth the bother

    // Rather than a valuetuple, place its elements here.
    // ValueTuple value;
    Object[] vals;
    int[] mods;

    Invocation(String fn_name, Object[] vals, int[] mods) {
      Assert.assert(!fn_name.equals(""));
      this.fn_name = fn_name;
      this.vals = vals;
      this.mods = mods;
    }

    // Print the Invocation on two lines, indented by two spaces
    String format() {
      StringWriter sw = new StringWriter();
      PrintWriter pw = new PrintWriter(sw);

      pw.println("  " + fn_name);
      pw.print("    ");
      for (int j=0; j<vals.length; j++) {
        if (j != 0)
          pw.print(", ");
        Object val = vals[j];
        if (val instanceof int[])
          pw.print(ArraysMDE.toString((int[]) val));
        else if (val instanceof String)
          pw.print((String)val);
        else
          pw.print(val);
      }
      pw.println();

      return sw.toString();
    }

    // Return true if the invocations print the same
    public boolean equals(Object other) {
      if (other instanceof FileIO.Invocation)
        return this.format().equals(((FileIO.Invocation) other).format());
      else
        return false;
    }

    public int hashCode() {
      return this.format().hashCode();
    }
  }

  // call_hashmap is for functions with a nonce to indicate which returns
  // are associated with which entries.
  // call_stack is for functions without nonces.

  // Reading a data trace file first initializes this.  I could save some
  // Object overhead by using two parallel stacks instead of Invocation
  // objects; but that's not worth it.
  static Stack call_stack;		// stack of Invocation objects

  static HashMap call_hashmap; 	// map from Integer to Invocation


  // Only processes the ppts in the given Vector.
  static void after_processing_file_declarations(Vector ppts, PptMap all_ppts) {

    /// Add _orig vars.

    for (Iterator itor = ppts.iterator() ; itor.hasNext() ; ) {
      PptTopLevel ppt = (PptTopLevel) itor.next();
      ppt.set_controlling_ppts(all_ppts);
      ppt.compute_entry_ppt(all_ppts);
      if (ppt.entry_ppt != null) {
        ppt.add_orig_vars(ppt.entry_ppt);
      }
    }
  }


// This should call the new version; but as of 1/9/2000, it isn't called at all.
// /// Old version.
// /// This should probably call the new version.
// // I ought to be able to call this individually for each program point,
// // after processing its declarations, right?  Yes for adding _orig
// // variables (but only after reading the corresponding entry program
// // point!), but, not for adding invocation counts, unless I have a way to
// // add new variables to an existing program point.
//
// // I probably *do* want to have original values even for global variables,
// // so I probably want to add original values after globals.
// // What about original vs. final invocation counts?  That could give info
// // about the dynamic call graph.  For now I'll do it, to avoid potential
// // problems with number of orig vars not equaling number of final vars.
//   static void after_processing_file_declarations(PptMap all_ppts) {
//
//     //// Take care not to call this multiple times.  If fn_truevars is set,
//     //// it has been called.  But we need to set all the fn_truevars (for use
//     //// adding _orig) before doing the rest of the work.
//
//     /// Add _orig vars.
//
//     for (Iterator itor = all_ppts.values().iterator() ; itor.hasNext() ; ) {
//       PptTopLevel ppt = (PptTopLevel) itor.next();
//       PptTopLevel entry_ppt = ppt.entry_ppt(all_ppts);
//       if (entry_ppt != null)
// 	entry_ppts.put(ppt, entry_ppt);
//     }
//     for (Iterator itor = entry_ppts.entrySet().iterator() ; itor.hasNext() ; ) {
//       Map.Entry entry = (Map.Entry) itor.next();
//       PptTopLevel exit_ppt = (PptTopLevel) entry.getKey();
//       PptTopLevel entry_ppt = (PptTopLevel) entry.getValue();
//       exit_ppt.add_orig_vars(entry_ppt);
//     }
//
//     // // Add function invocation counts.
//     // Vector fn_names = new HashSet();
//     // for (int i=0; i<all_ppts.size(); i++) {
//     //   String fn_name = ppt.fn_name();
//     //   if (fn_name != null)
//     //     fn_names.add(fn_name);
//     // }
//     // for (int i=0; i<all_ppts.size(); i++) {
//     //   Ppt ppt = all_ppts[i];
//     //   ppt.add_fn_invocation_counts();
//     // }
//
// //     for (Iterator itor = all_ppts.values().iterator() ; itor.hasNext() ; ) {
// //       PptTopLevel ppt = (PptTopLevel) itor.next();
// //       ppt.initial_processing();
// //     }
//
//
//   }


///////////////////////////////////////////////////////////////////////////
/// Data trace files
///


// This is going to manage the orig variables, augmenting variable lists
// before the rest of the system ever sees them.

// This is important, but it's largely user interface,
// so I can get around to it later.
// /**
//  * Read data traces from FILES.
//  * See read_decls_and_traces for more documentation.
//  */
//   void read_data_traces(String[] files, int clear, Pattern fn_regexp, Integer num_files, Long random_seed) {
//     if (clear) {
//       clear_trace_variables();
//     }
//     fn_regexp = util.re_compile_maybe(fn_regexp, re.IGNORECASE);
//
//     files_orig = files;
//     if type(files) == types.StringType {
//       files = util.expand_file_name(files);
//       files = glob.glob(files);
//     }
//     if files == [] {
//       raise "No files specified";
//     }
//     if (num_files != null) {
//       int num_files_int = num_files.intValue();
//       if num_files_int == 0 {
// 	raise "Requested 0 of %d files" % len(files);
//       }
//       total_files = len(files);
//       if (num_files_int > total_files) {
// 	raise "Requested %d files, but only %d supplied" % (num_files, total_files);
//       }
//       if (random_seed != null)
// 	Global.random.setSeed(random_seed.longValue());
//       files = util.random_subset(files, num_files);
//       //// Skip this, too wordy.  We can always determine them later.
//       // print num_files, "files randomly chosen:"
//       // for file in files:
//       //     print " ", file
//       assert num_files == None or num_files == len(files);
//     }
//
//     for file in files {
//       read_merge_data_trace_file(file, fn_regexp);
//     }
//
//     if (__debug__) {                       // for loop is outside assert, yuck
//       for fname in fn_var_values.keys() {
// 	assert ((len(fn_var_values[fname].keys()) == 0)
// 		or (len(fn_var_infos[fname]) == len(fn_var_values[fname].keys()[0])));
//       }
//     }
//   }




// An instrumented program produces a .dtrace file containing information about
// run-time values of expressions and variables.  The invariant detector tries
// to find patterns in the values recorded in one or more trace files.
// To detect invariants in a particular program, it is enough to insert code
// in the application which creates a trace file.  In Lisp, the
// `write-to-data-trace' macro and `instrument' function perform this task.
// For documentation of the data trace file format, see dtrace-format.txt.




// All this appears in the Ppt now, and none of this is called as of 1/9/2000.
// It should probably all be deleted.
// // Since I want Ppts to be dumb, fat, and happy, they won't contain any
// // info about number of nonconstant variables, etc.  That all appears here.
//
//   // Additional information useful when reading from a data trace file.
//   // I doubt this will be able to be garbage-collected, unfortunately.
//   static final class PptReadingInfo {
//     int trace_vars;		// number of vars actually in the trace file
//     int[] vi_index;		// map from position in trace file (and thus in
// 				//   ValueTuple) to VarInfo index
//
//     PptReadingInfo(PptTopLevel ppt) {
//       trace_vars = 0;
//       for (int i=0; i<ppt.var_infos.length; i++) {
// 	VarInfo vi = ppt.var_infos[i];
// 	if (! (vi.isStaticConstant() || vi.isDerived())) {
// 	  trace_vars++;
// 	}
//       }
//       vi_index = new int[trace_vars];
//       int trace_var = 0;
//       for (int i=0; i<ppt.var_infos.length; i++) {
// 	VarInfo vi = ppt.var_infos[i];
// 	if (! (vi.isStaticConstant() || vi.isDerived())) {
// 	  vi_index[trace_var] = i;
// 	  trace_var++;
// 	}
//       }
//       Assert.assert(trace_var == trace_vars);
//     }
//   }
//
//   // returns a map from program point name to PptReadingInfo
//   static HashMap getPptReadingInfoMap(PptMap all_ppts) {
//     HashMap result = new HashMap(all_ppts.size());
//
//     for (Iterator itor = all_ppts.entrySet().iterator() ; itor.hasNext() ; ) {
//       Map.Entry entry = (Map.Entry) itor.next();
//       result.put(entry.getKey(), new PptReadingInfo((PptTopLevel) entry.getValue()));
//     }
//     return result;
//   }


  // Eventually add these arguments to this function; they were in the Python
  // version.
  //  *     NUM_FILES indicates how many (randomly chosen) files are to be read;
  //  *       it defaults to all of the files.
  //  *     RANDOM_SEED is a triple of numbers, each in range(0,256), used to
  //  *       initialize the random number generator.

  /**
   * Read data from .dtrace files.
   * Calls @link{read_data_trace_file(String,PptMap,Pattern)} for each
   * element of filenames.
   **/
  static void read_data_trace_files(Collection files, PptMap all_ppts) throws IOException {

    init_call_stack_and_hashmap();

    for (Iterator i = files.iterator(); i.hasNext(); ) {
      System.out.print(".");
      String file = (String) i.next();
      read_data_trace_file(file, all_ppts);
    }

    process_unmatched_procedure_entries();
  }


  // for debugging only.  We stash values here to be examined/printed later.
  static public LineNumberReader data_trace_reader;
  static public String data_trace_filename;

  static void init_call_stack_and_hashmap() {
    call_stack = new Stack();
    call_hashmap = new HashMap();
  }

  /**
   * Read data from .dtrace file.
   **/
  static void read_data_trace_file(String filename, PptMap all_ppts) throws IOException {

    if (Global.debugRead) {
      System.out.println("read_data_trace_file " + filename
                         + ((Daikon.ppt_regexp != null) ? " " + Daikon.ppt_regexp.getPattern() : "")
                         + ((Daikon.ppt_omit_regexp != null) ? " " + Daikon.ppt_omit_regexp.getPattern() : ""));
    }

    LineNumberReader reader = UtilMDE.LineNumberFileReader(filename);
    data_trace_reader = reader;
    data_trace_filename = filename;

    //used for debugging: write new data trace file
    if (Global.debugPrintDtrace) {
      Global.dtraceWriter = new PrintWriter(new FileWriter(new File(filename + ".debug")));
    }
    // init_ftn_call_ct();          // initialize function call counts to 0

    // Maps from a function to the cumulative modification bits seen for
    // the entry since the time other elements were seen.  There is one tag
    // for each exit point associated with this entry.
    HashMap cumulative_modbits = new HashMap();
    for (Iterator itor = all_ppts.iterator() ; itor.hasNext() ; ) {
      PptTopLevel ppt = (PptTopLevel) itor.next();
      PptTopLevel entry_ppt = ppt.entry_ppt;
      if (entry_ppt != null) {
        int num_vars = entry_ppt.num_vars() - entry_ppt.num_static_constant_vars;
        int[] mods = new int[num_vars];
        Arrays.fill(mods, 0);
        HashMap subhash = (HashMap) cumulative_modbits.get(entry_ppt);
        if (subhash == null) {
          subhash = new HashMap();
          cumulative_modbits.put(entry_ppt, subhash);
        }
        subhash.put(ppt, mods);
      }
    }


    // try {
      // "line_" is uninterned, "line" is interned
      for (String line_ = reader.readLine(); line_ != null; line_ = reader.readLine()) {
        if (line_.equals("") || (line_.startsWith(comment_prefix))) {
          continue;
        }

        String line = line_.intern();

        if ((line == declaration_header)
            || ((Daikon.ppt_omit_regexp != null)
                && Global.regexp_matcher.contains(line, Daikon.ppt_omit_regexp))
            || ((Daikon.ppt_regexp != null)
                && ! Global.regexp_matcher.contains(line, Daikon.ppt_regexp))) {
          // Discard this entire program point information
          // System.out.println("Discarding non-matching dtrace program point " + line);
          while ((line != null) && !line.equals(""))
            line = reader.readLine();
          continue;
        }

        String ppt_name = line; // already interned

        PptTopLevel ppt = (PptTopLevel) all_ppts.get(ppt_name);
        Assert.assert(ppt != null, "Program point " + ppt_name + " appears in dtrace file but not in any decl file");

        VarInfo[] vis = ppt.var_infos;

        // not vis.length, as that includes constants, derived variables, etc.
        // Actually, we do want to leave space for _orig vars.
        // And for the time being (and possibly forever), for derived variables.
        int num_tracevars = ppt.num_tracevars;
        int vals_array_size = ppt.var_infos.length - ppt.num_static_constant_vars;
        Assert.assert(vals_array_size == num_tracevars + ppt.num_orig_vars);

        Object[] vals = new Object[vals_array_size];
        int[] mods = new int[vals_array_size];

	// Read an invocation nonce if one exists
        Integer nonce = null;
        {
          // arbitrary number, hopefully big enough; catch exceptins
          reader.mark(100);
          String nonce_name_maybe;
          try {
            nonce_name_maybe = reader.readLine();
          } catch (Exception e) {
            nonce_name_maybe = null;
          }
          reader.reset();
          if ("this_invocation_nonce".equals(nonce_name_maybe)) {

	      String nonce_name = reader.readLine();
	      Assert.assert(nonce_name.equals("this_invocation_nonce"));
	      nonce = new Integer(reader.readLine());

	      if (Global.debugPrintDtrace) {
		to_write_nonce = true;
		nonce_value = nonce.toString();
		nonce_string = nonce_name_maybe;
	      }
          }
        }

        // Fills up vals and mods arrays by side effect.
        read_vals_and_mods_from_data_trace_file(reader, ppt, vals, mods);

        // Now add some additional variable values that don't appear directly
        // in the data trace file but aren't traditional derived variables.

        add_orig_variables(ppt, cumulative_modbits, vals, mods, nonce);

        // // Add invocation counts
        // if not no_invocation_counts {
        //   for ftn_ppt_name in fn_invocations.keys() {
        //     calls_var_name = "calls(%s)" % ftn_ppt_name;
        //     assert calls_var_name == these_var_infos[current_var_index].name;
        //     these_values.append((fn_invocations[ftn_ppt_name],1))e;
        //     current_var_index++;
        //   }
        // }

        // Add derived variables
        add_derived_variables(ppt, vals, mods);


        vals = Intern.intern(vals);
        Assert.assert(Intern.isInterned(vals));

        // Done adding additional variable values that don't appear directly
        // in the data trace file.

        ValueTuple vt = new ValueTuple(vals, mods);

        if (Global.debugRead) {
          System.out.println("Adding ValueTuple to " + ppt.name);
        }
        ppt.add(vt, 1);

        PptTopLevel exit_ppt = (PptTopLevel) ppt.combined_exit;
        if (exit_ppt != null) {
          VarInfo[] exit_vis = exit_ppt.var_infos;
          // System.out.println("ppt = " + ppt.name);
          // System.out.println(" comb_indices = " + utilMDE.ArraysMDE.toString(ppt.combined_exit_var_indices));
          // System.out.println(" vt = " + vt.toString());
          ValueTuple exit_vt = vt.slice(ppt.combined_exit_var_indices);
          exit_ppt.add(exit_vt, 1);
        }

      }
    // }
    // // This catch clause is a bit of a pain.  On the plus side, it gives
    // // line number information in the error message.  On the minus side, it
    // // prevents the debugger from getting to the right frame.  As a
    // // compromise, perhaps eliminate this but have callers use the public
    // // "data_trace_reader" and "data_trace_filename" members.
    // catch (RuntimeException e) {
    //   System.out.println(lineSep + "At " + filename + " line " + reader.getLineNumber() + ":");
    //   e.printStackTrace();
    //   throw e;
    // } catch (Error e) {
    //   System.out.println(lineSep + "At " + filename + " line " + reader.getLineNumber() + ":");
    //   e.printStackTrace();
    //   throw e;
    // }

    if (Global.debugPrintDtrace) {
      Global.dtraceWriter.close();
    }

  }


  static void process_unmatched_procedure_entries() {
    if ((!call_stack.empty()) || (!call_hashmap.isEmpty())) {
      System.out.println();
      System.out.println("Detected abnormal termination of "
			 + (call_stack.size() + call_hashmap.size())
                         + " functions.");
      if (!call_hashmap.isEmpty()) {
        System.out.println("Unterminated calls:");
        print_invocations(call_hashmap.values());
      }

      if (!call_stack.empty()) {
        System.out.println("Remaining call stack:");
        print_invocations(call_stack);
      }
      System.out.println("End of abnormal termination report");
    }
  }

  static void print_invocations(Collection invocations) {
    if (VERBOSE_UNMATCHED_PROCEDURE_ENTRIES)
      print_invocations_verbose(invocations);
    else
      print_invocations_grouped(invocations);
  }

  static void print_invocations_verbose(Collection invocations) {
    for (Iterator i = invocations.iterator(); i.hasNext(); ) {
      Invocation invok = (Invocation) i.next();
      System.out.println(invok.format());
    }
  }

  static void print_invocations_grouped(Collection invocations) {
    // Maps an Invocation to its frequency
    Map counter = new HashMap();

    for (Iterator i = invocations.iterator(); i.hasNext(); ) {
      Invocation invok = (Invocation) i.next();
      if (counter.containsKey(invok)) {
        Integer oldCount = (Integer) counter.get(invok);
        Integer newCount = new Integer(oldCount.intValue() + 1);
        counter.put(invok, newCount);
      } else {
        counter.put(invok, new Integer(1));
      }
    }

    for (Iterator i = counter.keySet().iterator(); i.hasNext(); ) {
      Invocation invok = (Invocation) i.next();
      Integer count = (Integer) counter.get(invok);
      System.out.println(count + " instance" + ((count.intValue() == 1) ? "" : "s") + " of:");
      System.out.println(invok.format());
    }
  }

  // This procedure fills up vals and mods by side effect.
  static void read_vals_and_mods_from_data_trace_file(LineNumberReader reader, PptTopLevel ppt, Object[] vals, int[] mods) throws IOException {
    VarInfo[] vis = ppt.var_infos;
    int num_tracevars = ppt.num_tracevars;


    String[] oldvalue_reps;
    if ( (oldvalue_reps = (String[]) ppt_to_value_reps.get(ppt)) == null) {
      // We've not encountered this program point before.  The nulls in
      // this array will compare non-equal to whatever is in the trace
      // file, which is the desired behavior.
      oldvalue_reps = new String [num_tracevars];
    }

    if (Global.debugPrintDtrace) {
      Global.dtraceWriter.println(ppt.name);

      if (to_write_nonce) {
	Global.dtraceWriter.println(nonce_string);
	Global.dtraceWriter.println(nonce_value);
	to_write_nonce = false;
      }
    }

    for (int vi_index=0, val_index=0; val_index<num_tracevars; vi_index++) {
      Assert.assert(vi_index < vis.length
                    // , "Got to vi_index " + vi_index + " after " + val_index + " of " + num_tracevars + " values"
                    );
      VarInfo vi = vis[vi_index];
      Assert.assert((! vi.is_static_constant)
                    || (vi.value_index == -1)
                    // , "Bad value_index " + vi.value_index + " when static_constant_value = " + vi.static_constant_value + " for " + vi.repr() + " at " + ppt_name
                    );
      if (vi.is_static_constant)
        continue;
      Assert.assert(val_index == vi.value_index
                    // , "Differing val_index = " + val_index
                    // + " and vi.value_index = " + vi.value_index
                    // + " for " + vi.name + lineSep + vi.repr()
                    );

      // In errors, say "for program point", not "at program point" as the
      // latter confuses Emacs goto-error.

      String line = reader.readLine();
      if (line == null) {
        throw new Error("Unexpected end of file at " + data_trace_filename + " line " + reader.getLineNumber()
                        + "\n  Expected variable " + vi.name + ", got " + line
                        + " for program point " + ppt.name);
      }

      while ((Daikon.var_omit_regexp != null)
             && (line != null)
             && Global.regexp_matcher.contains(line, Daikon.var_omit_regexp)) {
        line = reader.readLine(); // value
        line = reader.readLine(); // modbit
        line = reader.readLine(); // next variable name
      }

      if (!VarInfoName.parse(line).equals(vi.name)) {
        throw new Error("Expected variable " + vi.name + ", got " + line
                        + " for program point " + ppt.name
                        + " at " + data_trace_filename + " line " + reader.getLineNumber()
                        // + "\n  VarInfo detail: " + vi.repr()
                        );
      }
      line = reader.readLine();
      if (line == null) {
        throw new Error("Unexpected end of file at " + data_trace_filename + " line " + reader.getLineNumber()
                        + "\n  Expected value for variable " + vi.name + ", got " + line
                        + " for program point " + ppt.name);
      }
      String value_rep = line;
      line = reader.readLine();
      if (line == null) {
        throw new Error("Unexpected end of file at " + data_trace_filename + " line " + reader.getLineNumber()
                        + "\n  Expected modbit for variable " + vi.name + ", got " + line
                        + " for program point " + ppt.name);
      }
      if (!((line.equals("0") || line.equals("1") || line.equals("2")))) {
        throw new Error("Bad modbit"
                        + " at " + data_trace_filename + " line " + reader.getLineNumber()
                        + ": " + line);
      }
      String mod_string = line;
      int mod = ValueTuple.parseModified(line);

      // System.out.println("Mod is " + mod + " at " + data_trace_filename + " line " + reader.getLineNumber()
      //                   + "\n  for variable " + vi.name
      //                   + " for program point " + ppt.name);

      if (mod != ValueTuple.MISSING) {
        // Set the modbit now, depending on whether the value of the variable
        // has been changed or not.
        if (value_rep.equals(oldvalue_reps[val_index])) {
          if (!dkconfig_add_changed) {
            mod = ValueTuple.UNMODIFIED;
          }
        } else {
          mod = ValueTuple.MODIFIED;
        }
      }

      mods[val_index] = mod;
      oldvalue_reps[val_index] = value_rep;

      if (Global.debugPrintDtrace) {
	Global.dtraceWriter.println(vi.name);
	Global.dtraceWriter.println(value_rep);
	Global.dtraceWriter.println(mod);
      }

      if (ValueTuple.modIsMissing(mod)) {
        if (!(value_rep.equals("missing") || value_rep.equals("uninit"))) {
          System.out.println("\nModbit indicates missing value for variable " + vi.name + " with value \"" + value_rep + "\";\n  text of value should be \"missing\" or \"uninit\" at " + data_trace_filename + " line " + reader.getLineNumber());
          System.exit(1);
        }
        vals[val_index] = null;
        vis[val_index].canBeMissing = true;
      } else {
        // System.out.println("Mod is " + mod + " (missing=" + ValueTuple.MISSING + "), rep=" + value_rep + " (modIsMissing=" + ValueTuple.modIsMissing(mod) + ")");
        vals[val_index] = vi.rep_type.parse_value(value_rep);
        // Testing, to catch a particular value once upon a time.
        // Assert.assert(! vals[val_index].equals("null"));
      }
      val_index++;

    }

    ppt_to_value_reps.put(ppt, oldvalue_reps);

    if (Global.debugPrintDtrace) {
      Global.dtraceWriter.println();
    }

    String blank_line = reader.readLine();
    // Expecting the end of a block of values.
    Assert.assert((blank_line == null) || (blank_line.equals("")), blank_line);
  }


  static void add_orig_variables(PptTopLevel ppt, HashMap cumulative_modbits, Object[] vals, int[] mods, Integer nonce) throws IOException {

    VarInfo[] vis = ppt.var_infos;
    String fn_name = ppt.fn_name();
    String ppt_name = ppt.name;
    if (ppt_name.endsWith(enter_tag)) {
      Invocation invok = new Invocation(fn_name, vals, mods);
      if (nonce == null) {
        call_stack.push(invok);
      } else {
        call_hashmap.put(nonce, invok);
      }
      HashMap subhash = (HashMap) cumulative_modbits.get(ppt);
      // If subhash is null, then there must have been no exit program
      // point that mapped back to this entry.  That could happen if the
      // body is "while (true) { }"; Jikes/dfej adds no synthetic "return"
      // statement in that case.
      // if (subhash == null) {
      //   System.out.println("Entry " + ppt_name + " has no cumulative_modbits");
      // }
      if (subhash != null) {
        // System.out.println("Entry " + ppt_name + " has " + subhash.size() + " exits");
        for (Iterator itor = subhash.values().iterator(); itor.hasNext(); ) {
          int[] exitmods = (int[]) itor.next();
          // System.out.println("lengths: " + exitmods.length + " " + mods.length);
          ValueTuple.orModsInto(exitmods, mods);
        }
      }
    } else {
      PptTopLevel entry_ppt = (PptTopLevel) ppt.entry_ppt;
      if (entry_ppt != null) {
        Invocation invoc;
        // Set invoc
        if (nonce == null) {
          if (call_stack.empty()) {
            throw new Error("Function exit without corresponding entry: "
                            + ppt.name);
          }
          invoc = (Invocation) call_stack.pop();
          while (invoc.fn_name != fn_name) {
            // Should also mark as a function that made an exceptional exit
            // at runtime.
            System.err.println("Exceptional exit from function " + fn_name
                               + ", expected to first exit from " + invoc.fn_name
                               + "; at " + data_trace_filename + " line " + data_trace_reader.getLineNumber());
            invoc = (Invocation) call_stack.pop();
          }
        } else {
          if (! call_hashmap.containsKey(nonce)) {
            throw new Error("Didn't find call to " + ppt.name + " with nonce " + nonce);
          }
          invoc = (Invocation) call_hashmap.get(nonce);
          call_hashmap.remove(nonce);
        }
        Assert.assert(ppt.num_orig_vars == entry_ppt.num_tracevars
                      // , ppt.name + " has " + ppt.num_orig_vars + " orig_vars, but " + entry_ppt.name + " has " + entry_ppt.num_tracevars + " tracevars"
                      );
        int[] entrymods = (int[]) ((HashMap)cumulative_modbits.get(entry_ppt)).get(ppt);
        for (int i=0; i<ppt.num_orig_vars; i++) {
          vals[ppt.num_tracevars+i] = invoc.vals[i];
          int mod = invoc.mods[i];
          if ((mod == ValueTuple.UNMODIFIED)
              && (entrymods[i] == ValueTuple.MODIFIED)) {
            // System.out.println("Entrymods made a difference.");
            mod = ValueTuple.MODIFIED;
          }
          mods[ppt.num_tracevars+i] = mod;
          // Possibly more efficient to set this all at once, late in
          // the game; but this gets it done.
          if (ValueTuple.modIsMissing(mods[ppt.num_tracevars+i])) {
            vis[ppt.num_tracevars+i].canBeMissing = true;
            Assert.assert(vals[ppt.num_tracevars+i] == null);
          }
        }
        Arrays.fill(entrymods, 0);

      }
    }

  }

  // Add derived variables
  static void add_derived_variables(PptTopLevel ppt, Object[] vals, int[] mods) throws IOException {
    // This ValueTuple is temporary:  we're temporarily suppressing interning,
    // which we will do after we have all the values available.
    ValueTuple partial_vt = ValueTuple.makeUninterned(vals, mods);
    int filled_slots = ppt.num_orig_vars+ppt.num_tracevars+ppt.num_static_constant_vars;
    for (int i=0; i<filled_slots; i++) {
      Assert.assert(!ppt.var_infos[i].isDerived());
    }
    for (int i=filled_slots; i<ppt.var_infos.length; i++) {
      Assert.assert(ppt.var_infos[i].isDerived(),
                    "variable not derived: " + ppt.var_infos[i].repr());
    }
    for (int i=filled_slots; i<ppt.var_infos.length; i++) {
      // Add this derived variable's value
      ValueAndModified vm = ppt.var_infos[i].derived.computeValueAndModified(partial_vt);
      vals[i] = vm.value;
      mods[i] = vm.modified;
    }
  }


  public static PptMap read_invariant_file(String filename) throws
  ClassNotFoundException, FileNotFoundException, IOException,
  OptionalDataException, StreamCorruptedException {
    InputStream istream = new FileInputStream(filename);
    if (filename.endsWith(".gz")) {
      istream = new GZIPInputStream(istream);
    }
    ObjectInputStream oistream = new ObjectInputStream(istream);
    try {
      return (PptMap) oistream.readObject();
    } catch (Exception e) {
      throw new Error("Trouble reading from .inv file " + filename + ": " + e.toString());
    }
  }

}
