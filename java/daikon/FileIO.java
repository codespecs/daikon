

package daikon;

import java.io.*;
import java.util.*;
import com.oroinc.text.regex.*;
import utilMDE.*;

class FileIO {

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


// /**
//  * Read declarations from the FILES, then read the traces in the FILES.
//  * When declarations and data traces are in separate files, it is more
//  * efficient to call read_declarations and then read_data_traces.
//  *     NUM_FILES indicates how many (randomly chosen) files are to be read;
//  *       it defaults to all of the files.
//  *     RANDOM_SEED is a triple of numbers, each in range(0,256), used to
//  *       initialize the random number generator.
//  */
// def read_decls_and_traces(files, clear=0, fn_regexp=None, num_files=None, random_seed=None):
//     read_declarations(files, clear, fn_regexp)
//     read_data_traces(files, clear, fn_regexp, num_files, random_seed)


/// Constants

  final static boolean debug_read = false;
  // final static boolean debug_read = true;

  final static String comment_prefix = "//";
  final static String declaration_header = "DECLARE";


  // Program point name tags
  final static String ppt_tag_separator = ":::";
  final static String enter_tag = ppt_tag_separator + "ENTER";
  // This does not necessarily appear at the end of the program point name;
  // a number may follow it.
  final static String exit_tag = ppt_tag_separator + "EXIT";


/// Variables
  static HashMap entry_ppts = new HashMap(); // maps from Ppt to Ppt


///////////////////////////////////////////////////////////////////////////
/// Declaration files
///


  static void reset_declarations() {
    throw new Error("to implement");
  }

  /**
   * Calls @link{read_declaration_file(String, fn_regexp)} for each element
   * of files.  See the definition of that function.
   */
  static void read_declaration_files(String[] files, PptMap all_ppts, Pattern fn_regexp) {
    for (int i=0; i<files.length; i++)
      try {
	read_declaration_file(files[i], all_ppts, fn_regexp);
      } catch (IOException e) {
	e.printStackTrace();
	throw new Error(e.toString());
      }
    after_processing_all_declarations(all_ppts);
  }

  /**
   * Calls @link{read_declaration_file(String, fn_regexp)} for each element
   * of files.  See the definition of that function.
   */
  static void read_declaration_files(Vector files, PptMap all_ppts, Pattern fn_regexp) {
    read_declaration_files((String[])files.toArray(new String[] {}), all_ppts, fn_regexp);
  }


  // Maybe the caller should deal with converting this into a regular
  // expression.
  /**
   * @param file
   * @param fn_regexp may be a String or a regular expression.
   *        If a string, it is converted into a case-insensitive regular
   *        expression, and only program points matching it are considered.
   */
  static void read_declaration_file(String filename, PptMap all_ppts, Pattern fn_regexp) throws IOException {

    // Get all function names to add checks on ftn invocation counts

    if (debug_read)
      System.out.println("read_declaration_file " + filename + " " + ((fn_regexp != null) ? fn_regexp.getPattern() : ""));

    BufferedReader reader = UtilMDE.BufferedFileReader(filename);
    String line = reader.readLine();

    // line == null when we hit end of file
    for ( ; line != null; line = reader.readLine()) {
      if (debug_read)
	System.out.println("read_declaration_file line: " + line);
      if (line.equals("") || line.startsWith("//"))
	continue;
      if (line.equals(declaration_header)) {
	Ppt ppt = read_declaration(reader, all_ppts, fn_regexp);
	all_ppts.put(ppt.name, ppt);
	continue;
      }
      // Not a declaration.
      // Read the rest of this entry (until we find a blank line).
      if (debug_read)
	System.out.println("Found odd line, skipping paragraph: " + line);
      while ((line != null) && line.equals("") && !line.startsWith("//")) {
	System.out.println("Offending line = `" + line + "'");
	System.out.println("" + (line != null) + " " + (line.equals("")) + " " + !line.startsWith("//"));
	if (line == null)
	  throw new Error("Can't happen");
	line = reader.readLine();
      }
      continue;
    }
  }


  // The "DECLARE" line has alredy been read.
  // This has certain parallels with read_data_trace file; but I think
  // separating the implementations is clearer, even if there's a bit of
  // duplication.
  static Ppt read_declaration(BufferedReader file, PptMap all_ppts, Object fn_regexp) throws IOException {
    // We have just read the "DECLARE" line.
    String ppt_name = file.readLine().intern();
    // if (fn_regexp and not fn_regexp.search(ppt_name)):
    //     // Discard this declaration
    //     while (!line.equals("\n")) and (!line.equals("")):
    //         line = file.readLine()
    //     return

    // This program point name has already been encountered.
    if (all_ppts.containsKey(ppt_name)) {
      throw new Error("Already contains program point " + ppt_name);
    }

    // if (ppt_name.endsWith(":::ENTER"))
    //   functions.add(ppt_name.substring(0, ppt_name.length() - 8));

    Vector var_infos = new Vector();

    String line = file.readLine();
    // Each iteration reads a variable name, type, and comparability.
    // Possibly abstract this out into a separate function??
    while ((line != null) && (!line.equals(""))) {
      String varname = line;
      String proglang_type_string = file.readLine();
      String rep_type_string = file.readLine();
      String comparability_string = file.readLine();
      if ((proglang_type_string == null) || (rep_type_string == null) || (comparability_string == null))
	throw new Error("End of file while reading variable " + varname + " in declaration of program point " + ppt_name);
      int equals_index = proglang_type_string.indexOf(" = ");
      // constant_value is a future enhancement
      String constant_value_string = null;
      Object constant_value = null;
      if (equals_index != -1) {
	constant_value_string = proglang_type_string.substring(equals_index+3);
	proglang_type_string = proglang_type_string.substring(0, equals_index);
      }
      ProglangType prog_type = ProglangType.parse(proglang_type_string);
      ProglangType rep_type = ProglangType.parse(rep_type_string);
      if (constant_value != null) {
	constant_value = ProglangType.parse(constant_value_string);
      }
      ExplicitVarComparability comparability
	= ExplicitVarComparability.parse(comparability_string, prog_type);
      var_infos.add(new VarInfo(varname, prog_type, rep_type, comparability, constant_value));
      line = file.readLine();
    }
    VarInfo[] vi_array = (VarInfo[]) var_infos.toArray(new VarInfo[0]);
    return new PptTopLevel(ppt_name, vi_array);
  }


  static class Invocation {
    String fn_name;		// not interned:  not worth the bother

    // Rather than a valuetuple, place its elements here.
    // ValueTuple value;
    Object[] vals;
    int[] mods;

    Invocation(String fn_name_, Object[] vals_, int[] mods_) {
      Assert.assert(!fn_name_.equals(""));
      fn_name = fn_name_;
      vals = vals_;
      mods = mods_;
    }
  }

  // Reading a data trace file should first initialize this.
  // No huge loss if it doesn't, though.  Well, except that the
  // exceptional-return information is slightly wrong.
  // I could save some space by using two parallel stacks instead
  // of this scheme.
  static Stack call_stack;		// stack of Invocation objects





// I ought to be able to call this individually for each program point,
// after processing its declarations, right?  Yes for adding _orig
// variables (but only after reading the corresponding entry program
// point!), but, not for adding invocation counts, unless I have a way to
// add new variables to an existing program point.

// I probably *do* want to have original values even for global variables,
// so I probably want to add original values after globals.
// What about original vs. final invocation counts?  That could give info
// about the dynamic call graph.  For now I'll do it, to avoid potential
// problems with number of orig vars not equaling number of final vars.
  static void after_processing_all_declarations(PptMap all_ppts) {

    //// Take care not to call this multiple times.  If fn_truevars is set,
    //// it has been called.  But we need to set all the fn_truevars (for use
    //// adding _orig) before doing the rest of the work.

    /// Add _orig vars.

    for (Iterator itor = all_ppts.values().iterator() ; itor.hasNext() ; ) {
      PptTopLevel ppt = (PptTopLevel) itor.next();
      PptTopLevel entry_ppt = ppt.entry_ppt(all_ppts);
      if (entry_ppt != null)
	entry_ppts.put(ppt, entry_ppt);
    }
    for (Iterator itor = entry_ppts.entrySet().iterator() ; itor.hasNext() ; ) {
      Map.Entry entry = (Map.Entry) itor.next();
      PptTopLevel exit_ppt = (PptTopLevel) entry.getKey();
      PptTopLevel entry_ppt = (PptTopLevel) entry.getValue();
      exit_ppt.add_orig_vars(entry_ppt);
    }

    // // Add function invocation counts.
    // Vector fn_names = new HashSet();
    // for (int i=0; i<all_ppts.size(); i++) {
    //   String fn_name = ppt.fn_name();
    //   if (fn_name != null)
    //     fn_names.add(fn_name);
    // }
    // for (int i=0; i<all_ppts.size(); i++) {
    //   Ppt ppt = all_ppts[i];
    //   ppt.add_fn_invocation_counts();
    // }




  }


///////////////////////////////////////////////////////////////////////////
/// Data trace files
///


// This is going to manage the orig variables, augmenting variable lists
// before the rest of the system ever sees them.

// This is important, but it's largely user interface, so I can get around
// to it later.
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
// For documentation of the data trace file format, see daikon.py.doc.




// Since I want Ppts to be dumb, fat, and happy, they won't contain any
// info about number of nonconstant variables, etc.  That all appears here.

  // Additional iformation useful when reading from a data trace file.
  // I doubt this will be able to be garbage-collected, unfortunately.
  static class PptReadingInfo {
    int trace_vars;		// number of vars actually in the trace file
    int[] vi_index;		// map from position in trace file (and thus in
				//   ValueTuple) to VarInfo index

    PptReadingInfo(PptTopLevel ppt) {
      trace_vars = 0;
      for (int i=0; i<ppt.var_infos.length; i++) {
	VarInfo vi = ppt.var_infos[i];
	if (! (vi.isConstant() || vi.isDerived())) {
	  trace_vars++;
	}
      }
      vi_index = new int[trace_vars];
      int trace_var = 0;
      for (int i=0; i<ppt.var_infos.length; i++) {
	VarInfo vi = ppt.var_infos[i];
	if (! (vi.isConstant() || vi.isDerived())) {
	  vi_index[trace_var] = i;
	  trace_var++;
	}
      }
      Assert.assert(trace_var == trace_vars);
    }
  }

  // returns a map from program point name to PptReadingInfo
  static HashMap getPptReadingInfoMap(PptMap all_ppts) {
    HashMap result = new HashMap(all_ppts.size());

    for (Iterator itor = all_ppts.entrySet().iterator() ; itor.hasNext() ; ) {
      Map.Entry entry = (Map.Entry) itor.next();
      result.put(entry.getKey(), new PptReadingInfo((PptTopLevel) entry.getValue()));
    }
    return result;
  }


  /**
   * Read data from .dtrace files.
   * Calls @link{read_data_trace_file(String,PptMap,Pattern)} for each
   * element of filenames.
   */
  // Maybe this should be a member function of PptMap?
  static void read_data_trace_files(String[] filenames, PptMap all_ppts, Pattern fn_regexp) {
    for (int i=0; i<filenames.length; i++)
      try {
	read_data_trace_file(filenames[i], all_ppts, fn_regexp);
      } catch (IOException e) {
	e.printStackTrace();
	throw new Error(e.toString());
      }
    System.out.println("Read " + filenames.length + " data trace files");
  }

  /**
   * Read data from .dtrace files.
   * Calls @link{read_data_trace_file(String,PptMap,Pattern)} for each
   * element of filenames.
   */
  // Maybe this should be a member function of PptMap?
  static void read_data_trace_files(Vector filenames, PptMap all_ppts, Pattern fn_regexp) {
    read_data_trace_files((String[])filenames.toArray(new String[]{}), all_ppts, fn_regexp);
  }

  /**
   * Read data from .dtrace file.
   */
  // Maybe this should be a member function of PptMap?
  static void read_data_trace_file(String filename, PptMap all_ppts, Pattern fn_regexp) throws IOException {

    // fn_regexp = util.re_compile_maybe(fn_regexp, re.IGNORECASE)

    if (debug_read) {
      System.out.println("read_data_trace_file " + filename + " " + fn_regexp);
    }

    BufferedReader reader = UtilMDE.BufferedFileReader(filename);

    // init_ftn_call_ct();          // initialize function call counts to 0
    call_stack = new Stack();


    for (String line_ = reader.readLine(); line_ != null; line_ = reader.readLine()) {
      if (line_.equals("") || (line_.startsWith(comment_prefix))) {
	continue;
      }

      String line = line_.intern();

      if (line == declaration_header) {
	// Discard this entire declaration
	while (!line.equals(""))
	  line = reader.readLine();
	continue;
      }

      String ppt_name = line;

      if ((fn_regexp != null) && !Global.regexp_matcher.contains(ppt_name, fn_regexp)) {
	// Discard this entire program point information
	while (!line.equals(""))
	  line = reader.readLine();
	continue;
      }

      ppt_name = ppt_name.intern();
      PptTopLevel ppt = (PptTopLevel) all_ppts.get(ppt_name);
      Assert.assert(ppt != null, "Didn't find program point " + ppt_name);

      VarInfo[] vis = ppt.var_infos;

      // not vis.length, as that includes constants, derived variables, etc.
      // Actually, we do want to leave space for _orig vars.
      int num_vals = ppt.num_tracevars;
      int vals_array_size = num_vals + ppt.num_orig_vars;
      Object[] vals = new Object[vals_array_size];
      int[] mods = new int[vals_array_size];

      for (int vi_index=0, val_index=0; val_index<num_vals; vi_index++) {
	VarInfo vi = vis[vi_index];
	if (vi.constant_value != null)
	  continue;
	Assert.assert(val_index == vi.value_index);
	line = reader.readLine();
	Assert.assert(line.equals(vi.name));
	line = reader.readLine();
	String value_rep = line;
	line = reader.readLine();
	Assert.assert(line.equals("1") || line.equals("0"));
	String mod_string = line;
	int mod = ValueTuple.parseModified(line);
	mods[val_index] = mod;
	vals[val_index] = (ValueTuple.modIsMissing(mod) ? null
		                    : vi.rep_type.parse_value(value_rep));
	val_index++;
      }

      String blank_line = reader.readLine();
      // Expecting the end of a block of values.
      Assert.assert((blank_line == null) || (blank_line.equals("")));

      // Now add some additional variable values that don't appear directly
      // in the data trace file but aren't traditional derived variables.

      String fn_name = ppt.fn_name();
      if (ppt_name.endsWith(enter_tag)) {
	call_stack.push(new Invocation(fn_name, vals, mods));
      } else {
	PptTopLevel entry_ppt = (PptTopLevel) entry_ppts.get(ppt);
	if (entry_ppt != null) {
	  Invocation invoc = (Invocation) call_stack.pop();
	  if (invoc.fn_name != fn_name)
	    // Should actually just mark as a function that made an
	    // exceptional exit at runtime and keep looking down the stack
	    // for the proper matching function entry.
	    throw new Error("Unexpected function name " + invoc.fn_name
			    + ", expected " + fn_name);
	  for (int i=0; i<entry_ppt.num_tracevars; i++) {
	    vals[i+ppt.num_tracevars] = invoc.vals[i];
	    mods[i+ppt.num_tracevars] = invoc.mods[i];
	  }
	}
      }


      // // Add invocation counts
      // if not no_invocation_counts {
      //   for ftn_ppt_name in fn_invocations.keys() {
      //     calls_var_name = "calls(%s)" % ftn_ppt_name;
      //     assert calls_var_name == these_var_infos[current_var_index].name;
      //     these_values.append((fn_invocations[ftn_ppt_name],1))e;
      //     current_var_index++;
      //   }
      // }

      // Done adding additional variable values that don't appear directly
      // in the data trace file.

      ValueTuple vt = new ValueTuple(vals, mods);

      if (debug_read) {
	System.out.println("Adding ValueTuple to " + ppt.name);
      }
      ppt.add(vt, 1);
    }

    if (!call_stack.empty()) {
      System.out.println("Detected abnormal termination of "
			 + call_stack.size() + " functions.");
      System.out.println("Remaining call stack:");
      int size = call_stack.size();
      for (int i=0; i<size; i++) {
	Invocation invok = (Invocation)call_stack.elementAt(i);
	System.out.println(invok.fn_name);
	System.out.println(UtilMDE.join(invok.vals, ", "));
      }
    }

  }

}
