package daikon;

import daikon.derive.ValueAndModified;
import daikon.config.Configuration;
import daikon.diff.InvMap;
import daikon.inv.Invariant;

import utilMDE.*;
import java.util.logging.Logger;
import java.util.logging.Level;

import java.io.*;
import java.util.*;

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

  /** Nobody should ever instantiate a FileIO. **/
  private FileIO() {
    throw new Error();
  }

  /// Constants

  static final String declaration_header = "DECLARE";

  // Program point name tags
  public static final String ppt_tag_separator = ":::";
  public static final String enter_suffix = "ENTER";
  public static final String enter_tag = ppt_tag_separator + enter_suffix;
  // EXIT does not necessarily appear at the end of the program point name;
  // a number may follow it.
  public static final String exit_suffix = "EXIT";
  public static final String exit_tag = ppt_tag_separator + exit_suffix;
  public static final String throws_suffix = "THROWS";
  public static final String throws_tag = ppt_tag_separator + throws_suffix;
  public static final String object_suffix = "OBJECT";
  public static final String object_tag = ppt_tag_separator + object_suffix;
  public static final String class_static_suffix = "CLASS";
  public static final String class_static_tag =
    ppt_tag_separator + class_static_suffix;
  public static final String global_suffix = "GLOBAL";

  /** Total number of samples read in. */
  public static int samples_considered = 0;
  /**
   * Total number of samples passed to process_sample().  This should
   * exactly match the number processed except for dataflow optimizations
   * in process_sample() (which have the end result of processing the
   * sample without the work).
   */
  public static int samples_processed = 0;

  /// Settings

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.

  /**
   * Boolean.  If true, prints the unmatched procedure entries
   * verbosely.
   **/
  public static boolean dkconfig_verbose_unmatched_procedure_entries = false;

  /**
   * Boolean.  When false, set modbits to 1 iff the printed
   * representation has changed.  When true, set modbits to 1 if the
   * printed representation has changed; leave other modbits as is.
   **/
  public static boolean dkconfig_add_changed = true;

  /**
   * Integer.  Maximum number of lines to read from the dtrace file.  If
   * 0, reads the entire file.
   */
  public static int dkconfig_max_line_number = 0;

  /**
   * Boolean. When false, don't count the number of lines in the dtrace file
   * before reading.  This will disable the percentage progress printout
   */
  public static boolean dkconfig_count_lines = true;

  /**
   * Boolean.  When true, only read the samples, but don't process them.
   * used to gather timing information
   */
  public static boolean dkconfig_read_samples_only = false;

  /** Boolean.  When true, don't print unmatched procedure entries. **/
  public static boolean dkconfig_unmatched_procedure_entries_quiet = false;

  /**
   * Boolean.  When true, suppress Exceptions related to file reading.
   * This permits Daikon to continue even if there is a malformed trace
   * file.  Use this with care:  in general, it is better to fix the
   * problem that caused a bad trace file, rather than to suppress the
   * exception.
   **/
  public static boolean dkconfig_continue_after_file_exception = false;

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

  // For debugging purposes: printing out a modified trace file with
  // changed modbits.
  private static boolean to_write_nonce = false;
  private static String nonce_value, nonce_string;

  // Logging Categories

  /** Debug tracer for reading. **/
  public static final Logger debugRead = Logger.getLogger("daikon.FileIO.read");

  /** Debug tracer for printing. **/
  public static final Logger debugPrint =
    Logger.getLogger("daikon.FileIO.printDtrace");

  /** Debug tracer for printing variable values. **/
  public static final Logger debugVars = Logger.getLogger("daikon.FileIO.vars");

  // Utilities
  // The Daikon manual states that "#" is the comment starter, but
  // some code assumes "//", so permit both (at least temporarily).
  // static final String comment_prefix = "//";
  public static final boolean isComment(String s) {
    return s.startsWith("//") || s.startsWith("#");
  }

  ///////////////////////////////////////////////////////////////////////////
  /// Declaration files
  ///

  /** This is intended to be used only interactively, while debugging. */
  static void reset_declarations() {
    throw new Error("to implement");
  }

  /**
   * @param files files to be read (java.io.File)
   * @return a new PptMap containing declarations read from the files
   * listed in the argument; connection information (controlling
   * variables and entry ppts) is set correctly upon return.
   **/
  public static PptMap read_declaration_files(Collection files // [File]
  ) throws IOException {
    PptMap all_ppts = new PptMap();
    // Read all decls, creating PptTopLevels and VarInfos
    for (Iterator i = files.iterator(); i.hasNext();) {
      File file = (File) i.next();
      if (!Daikon.dkconfig_quiet) {
        System.out.print("."); // show progress
      }
      read_declaration_file(file, all_ppts);
    }
    return all_ppts;
  }

  /** Read one decls file; add it to all_ppts. **/
  private static void read_declaration_file(File filename, PptMap all_ppts)
    throws IOException {
    if (debugRead.isLoggable(Level.FINE)) {
      debugRead.fine(
        "read_declaration_file "
          + filename
          + ((Daikon.ppt_regexp != null)
            ? " " + Daikon.ppt_regexp.getPattern()
            : "")
          + ((Daikon.ppt_omit_regexp != null)
            ? " " + Daikon.ppt_omit_regexp.getPattern()
            : ""));
    }

    // Default VarComparability
    int varcomp_format = VarComparability.IMPLICIT;

    LineNumberReader reader = UtilMDE.LineNumberFileReader(filename.toString());

    String line = reader.readLine();

    // line == null when we hit end of file
    for (; line != null; line = reader.readLine()) {
      if (debugRead.isLoggable(Level.FINE))
        debugRead.fine("read_declaration_file line: " + line);
      if (line.equals("") || isComment(line))
        continue;
      if (line.equals(declaration_header)) {
        PptTopLevel ppt =
          read_declaration(reader, all_ppts, varcomp_format, filename);
        // ppt can be null if this declaration was skipped because of --ppt or --ppt_omit.
        if (ppt != null) {
          all_ppts.add(ppt);
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
          throw new FileIOException(
            "Unrecognized VarComparability",
            reader,
            filename);
        }
        continue;
      }
      if (line.equals("ListImplementors")) {
        // Each line following is the name (in JVM form) of a class
        // that implemnts java.util.List.
        for (;;) {
          line = reader.readLine();
          if (line == null || line.equals(""))
            break;
          if (isComment(line))
            continue;
          ProglangType.list_implementors.add(line.intern());
        }
        continue;
      }

      // Not a declaration.
      // Read the rest of this entry (until we find a blank line).
      if (debugRead.isLoggable(Level.FINE))
        debugRead.fine(
          "Skipping paragraph starting at line "
            + reader.getLineNumber()
            + " of file "
            + filename
            + ": "
            + line);
      while ((line != null) && (!line.equals("")) && (!isComment(line))) {
        System.out.println(
          "Unrecognized paragraph contains line = `" + line + "'");
        System.out.println(
          ""
            + (line != null)
            + " "
            + (line.equals(""))
            + " "
            + (isComment(line)));
        if (line == null)
          throw new IllegalStateException();
        line = reader.readLine();
      }
      continue;
    }
  }

  // The "DECLARE" line has alredy been read.
  private static PptTopLevel read_declaration(
    LineNumberReader file,
    PptMap all_ppts,
    int varcomp_format,
    File filename)
    throws IOException {
    // We have just read the "DECLARE" line.
    String ppt_name = file.readLine().intern();

    // This program point name has already been encountered.
    if (all_ppts.containsName(ppt_name)) {
      throw new FileIOException(
        "Duplicate declaration of program point",
        file,
        filename);
    }

    // Despite the comments below, its not clear to me why we have to
    // create all ppts if we the user has explicitly asked them to
    // be excluded.
    //if (!daikon.split.SplitterList.dkconfig_all_splitters) {
    // If all the splitters are to be tried at all program points,
    // then we need to create all the program points because the
    // creation of splitters requires information from the program
    // points.

    // XXX Re-examine the below rant
    // JWN: The above is crazy!  Program points are now EXPENSIVE --
    // they create all derived variables and possible invariants.
    // I am turning off all_splitters by default.

    if (!ppt_included(ppt_name)) {
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
    // }

    // The var_infos that will populate the new program point
    List var_infos = new ArrayList();

    // Enable this code when Daikon supports handling of multiple exit
    // points via a variable rather than via separate ppts
    if (false) {
      //     Rename EXITnn to EXIT
      {
        PptName parsed_name = new PptName(ppt_name);
        if (parsed_name.isExitPoint()) {
          PptName new_name = parsed_name.makeExit();
          // Punt if we already read a different EXITnn
          if (all_ppts.get(new_name) != null) {
            String line = file.readLine();
            while ((line != null) && !line.equals("")) {
              // This fails if some lines of a declaration (e.g., the
              // comparability field) are empty.
              line = file.readLine();
            }
            return null;
          }
          // Override what was read from file
          ppt_name = new_name.name().intern();
          // Add the pseudo-variable $return_line
          if (false) {
            // Skip this for now; we're not sure how to make it work
            ProglangType prog_type = ProglangType.INT;
            // ?? new special type like HASHCODE
            ProglangType file_rep_type = ProglangType.INT;
            VarComparability comparability = VarComparabilityNone.it;
            // ?? comparable to nothing -- explicit?
            VarInfo line =
              new VarInfo(
                VarInfoName.parse("$return_line"),
                prog_type,
                file_rep_type,
                comparability,
                VarInfoAux.getDefault());
            var_infos.add(line);
          }
        }
      }
    }

    // Each iteration reads a variable name, type, and comparability.
    // Possibly abstract this out into a separate function??
    VarInfo vi;
    while ((vi = read_VarInfo(file, varcomp_format, filename, ppt_name))
      != null) {
      for (int i = 0; i < var_infos.size(); i++) {
        if (vi.name == ((VarInfo) var_infos.get(i)).name) {
          throw new FileIOException("Duplicate variable name", file, filename);
        }
      }
      // Can't do this test in read_VarInfo, it seems, because of the test
      // against null above.
      if ((Daikon.var_omit_regexp != null)
        && Global.regexp_matcher.contains(
          vi.name.name(),
          Daikon.var_omit_regexp)) {
        continue;
      }
      var_infos.add(vi);
    }

    VarInfo[] vi_array =
      (VarInfo[]) var_infos.toArray(new VarInfo[var_infos.size()]);
    return new PptTopLevel(ppt_name, vi_array);
  }

  /**
   * Read a variable name, type, and comparability; construct a VarInfo.
   * Return null after reading the last variable in this program point
   * declaration.
   **/
  private static VarInfo read_VarInfo(
    LineNumberReader file,
    int varcomp_format,
    File filename,
    String ppt_name)
    throws IOException {
    String line = file.readLine();
    if ((line == null) || (line.equals("")))
      return null;
    String varname = line;
    String proglang_type_string_and_aux = file.readLine();
    String file_rep_type_string = file.readLine();
    String comparability_string = file.readLine();
    if ((varname == null)
      || (proglang_type_string_and_aux == null)
      || (file_rep_type_string == null)
      || (comparability_string == null))
      throw new Error(
        "End of file "
          + filename
          + " while reading variable "
          + varname
          + " in declaration of program point "
          + ppt_name);
    int equals_index = file_rep_type_string.indexOf(" = ");
    String static_constant_value_string = null;
    Object static_constant_value = null;
    boolean is_static_constant = false;
    if (equals_index != -1) {
      is_static_constant = true;
      static_constant_value_string =
        file_rep_type_string.substring(equals_index + 3);
      file_rep_type_string = file_rep_type_string.substring(0, equals_index);
    }
    // XXX temporary, for compatibility with older .dtrace files.  12/20/2001
    if ("String".equals(file_rep_type_string)) {
      file_rep_type_string = "java.lang.String";
    }
    /// XXX

    int hash_position = proglang_type_string_and_aux.indexOf('#');
    String aux_string = "";
    if (hash_position == -1) {
      hash_position = proglang_type_string_and_aux.length();
    } else {
      aux_string =
        proglang_type_string_and_aux.substring(
          hash_position + 1,
          proglang_type_string_and_aux.length());
    }

    String proglang_type_string =
      proglang_type_string_and_aux.substring(0, hash_position).trim();

    ProglangType prog_type;
    ProglangType file_rep_type;
    ProglangType rep_type;
    VarInfoAux aux;
    try {
      prog_type = ProglangType.parse(proglang_type_string);
      file_rep_type = ProglangType.rep_parse(file_rep_type_string);
      rep_type = file_rep_type.fileTypeToRepType();
      aux = VarInfoAux.parse(aux_string);
    } catch (IOException e) {
      throw new FileIOException(e.getMessage(), file, filename);
    }

    if (static_constant_value_string != null) {
      static_constant_value =
        rep_type.parse_value(static_constant_value_string);
      // Why can't the value be null?
      Assert.assertTrue(static_constant_value != null);
    }
    VarComparability comparability =
      VarComparability.parse(varcomp_format, comparability_string, prog_type);
    // Not a call to Assert.assert in order to avoid doing the (expensive)
    // string concatenations.
    if (!VarInfo.legalFileRepType(file_rep_type)) {
      throw new FileIOException(
        "Unsupported (file) representation type "
          + file_rep_type.format()
          + " (parsed as "
          + rep_type
          + ")"
          + " for variable "
          + varname,
        file,
        filename);
    }
    if (!VarInfo.legalRepType(rep_type)) {
      throw new FileIOException(
        "Unsupported (converted) representation type "
          + file_rep_type.format()
          + " for variable "
          + varname,
        file,
        filename);
    }

    return new VarInfo(
      VarInfoName.parse(varname),
      prog_type,
      file_rep_type,
      comparability,
      is_static_constant,
      static_constant_value,
      aux);
  }

  ///////////////////////////////////////////////////////////////////////////
  /// invocation tracking for dtrace files entry/exit grouping
  ///

  static final class Invocation {
    PptTopLevel ppt; // used in printing and in suppressing duplicates
    // Rather than a valuetuple, place its elements here.
    Object[] vals;
    int[] mods;

    static Object canonical_hashcode = new Object();

    Invocation(PptTopLevel ppt, Object[] vals, int[] mods) {
      this.ppt = ppt;
      this.vals = vals;
      this.mods = mods;
    }

    // Print the Invocation on two lines, indented by two spaces
    String format() {
      StringWriter sw = new StringWriter();
      PrintWriter pw = new PrintWriter(sw);

      pw.println("  " + ppt.ppt_name.getNameWithoutPoint());
      pw.print("    ");

      // [adonovan] is this sound? Let me know if not (sorry).
      //Assert.assertTrue(ppt.var_infos.length == vals.length);

      for (int j = 0; j < vals.length; j++) {
        if (j != 0)
          pw.print(", ");

        pw.print(ppt.var_infos[j].name + "=");

        Object val = vals[j];
        if (val == canonical_hashcode)
          pw.print("<hashcode>");
        else if (val instanceof int[])
          pw.print(ArraysMDE.toString((int[]) val));
        else if (val instanceof String)
          pw.print(val == null ? "null" : UtilMDE.escapeNonASCII((String) val));
        else
          pw.print(val);
      }
      pw.println();

      return sw.toString();
    }

    /** Change uses of hashcodes to canonical_hashcode. **/
    public Invocation canonicalize() {
      Object[] new_vals = new Object[vals.length];
      System.arraycopy(vals, 0, new_vals, 0, vals.length);
      VarInfo[] vis = ppt.var_infos;
      // Warning: abstraction violation!
      for (int i = 0; i < vis.length; i++) {
        VarInfo vi = vis[i];
        if ((vi.value_index != -1)
          && (vi.file_rep_type == ProglangType.HASHCODE)) {
          new_vals[vi.value_index] = canonical_hashcode;
        }
      }
      return new Invocation(ppt, new_vals, mods);
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

  // call_hashmap is for procedures with a (global, not per-procedure)
  // nonce that indicates which returns are associated with which entries.
  // call_stack is for functions without nonces.

  // I could save some Object overhead by using two parallel stacks
  // instead of Invocation objects; but that's not worth it.
  static Stack call_stack = new Stack(); // stack of Invocation objects
  static HashMap call_hashmap = new HashMap();
  // map from Integer to Invocation

  ///////////////////////////////////////////////////////////////////////////
  /// Data trace files
  ///

  // Eventually add these arguments to this function; they were in the Python
  // version.
  //  *     NUM_FILES indicates how many (randomly chosen) files are to be read;
  //  *       it defaults to all of the files.
  //  *     RANDOM_SEED is a triple of numbers, each in range(0,256), used to
  //  *       initialize the random number generator.

  /** Reads data trace files using the default sample processor. **/
  public static void read_data_trace_files(Collection /*File*/
  files, PptMap all_ppts) throws IOException {

    Processor processor = new Processor();
    read_data_trace_files(files, all_ppts, processor);
  }

  /**
   * Read data from .dtrace files.
   * Calls @link{read_data_trace_file(File,PptMap,Pattern)} for each
   * element of filenames.
   **/
  public static void read_data_trace_files(Collection /*File*/
  files, PptMap all_ppts, Processor processor) throws IOException {
    for (Iterator i = files.iterator(); i.hasNext();) {
      File file = (File) i.next();
      try {
        read_data_trace_file(file, all_ppts, processor);
      } catch (IOException e) {
        if (e.getMessage().equals("Corrupt GZIP trailer")) {
          System.out.print(
            file.getName()
              + " has a corrupt gzip trailer.  "
              + "All possible data was recovered.\n");
        } else {
          throw e;
        }
      }
    }

    process_unmatched_procedure_entries();
  }

  private static long count_lines(File filename) throws IOException {
    LineNumberReader reader = UtilMDE.LineNumberFileReader(filename.toString());
    long count = 0;
    while (reader.readLine() != null)
      count++;
    return count;
  }

  // We stash values here to be examined/printed later.  Used to be
  // for debugging only, but now also used for Daikon progress output.
  public static LineNumberReader data_trace_reader;
  public static File data_trace_filename;
  public static long data_trace_total_lines;
  public static int data_num_slices = 0;

  /**
   * Class used to specify the processor to use for sample data.  By
   * default, the internal process_sample routine will be called.
   */
  public static class Processor {
    public void process_sample(
      PptMap all_ppts,
      PptTopLevel ppt,
      ValueTuple vt,
      Integer nonce) {
      FileIO.process_sample(all_ppts, ppt, vt, nonce);
    }
  }

  /** Read data from .dtrace file using standard data processor. **/
  static void read_data_trace_file(File filename, PptMap all_ppts)
    throws IOException {
    Processor processor = new Processor();
    read_data_trace_file(filename, all_ppts, processor);
  }

  /** Read data from .dtrace file. **/
  static void read_data_trace_file(
    File filename,
    PptMap all_ppts,
    Processor processor)
    throws IOException {
    int pptcount = 1;

    if (debugRead.isLoggable(Level.FINE)) {
      debugRead.fine(
        "read_data_trace_file "
          + filename
          + ((Daikon.ppt_regexp != null)
            ? " " + Daikon.ppt_regexp.getPattern()
            : "")
          + ((Daikon.ppt_omit_regexp != null)
            ? " " + Daikon.ppt_omit_regexp.getPattern()
            : ""));
    }

    LineNumberReader reader = UtilMDE.LineNumberFileReader(filename.toString());
    data_trace_reader = reader;
    data_trace_filename = filename;
    if ((Daikon.dkconfig_progress_delay != -1) && dkconfig_count_lines) {
      // avoid divide-by-zero for display while lines are being counted
      data_trace_total_lines = 1;
      data_trace_total_lines = count_lines(filename);
    }

    // Used for debugging: write new data trace file.
    if (Global.debugPrintDtrace) {
      Global.dtraceWriter =
        new PrintWriter(new FileWriter(new File(filename + ".debug")));
    }
    // init_ftn_call_ct();          // initialize function call counts to 0

    // try {
    // "line_" is uninterned, "line" is interned
    for (String line_ = reader.readLine();
      line_ != null;
      line_ = reader.readLine()) {
      if (line_.equals("") || isComment(line_)) {
        continue;
      }

      // stop at a specified point in the file
      if ((dkconfig_max_line_number > 0)
        && (reader.getLineNumber() > dkconfig_max_line_number))
        break;

      // Keep track of the total number of samples we have seen.
      samples_considered++;

      String line = line_.intern();

      if ((line == declaration_header) || !ppt_included(line)) {
        // Discard this entire program point information
        // System.out.println("Discarding non-matching dtrace program point " + line);
        while ((line != null) && !line.equals(""))
          line = reader.readLine();
        continue;
      }

      String ppt_name = line; // already interned
      {
        try {
          PptName parsed = new PptName(ppt_name);
          // Enable the code below when Daikon stops using different
          // ppts for different exits
          if (false) {
            // Rename EXITnn to EXIT
            if (parsed.isExitPoint()) {
              ppt_name = parsed.makeExit().name().intern();
            }
          }
        } catch (Error e) {
          throw new Error(
            "Illegal program point name \""
              + ppt_name
              + "\""
              + " at "
              + data_trace_filename
              + " line "
              + reader.getLineNumber());
        }
      }

      // if (pptcount++ % 10000 == 0)
      //    System.out.print(":");

      if (Daikon.debugTrace.isLoggable(Level.FINE)) {
        data_num_slices = all_ppts.countSlices();
      }

      PptTopLevel ppt = (PptTopLevel) all_ppts.get(ppt_name);
      Assert.assertTrue(
        ppt != null,
        "Program point "
          + ppt_name
          + " appears in dtrace file but not in any decl file");

      VarInfo[] vis = ppt.var_infos;

      // not vis.length, as that includes constants, derived variables, etc.
      // Actually, we do want to leave space for _orig vars.
      // And for the time being (and possibly forever), for derived variables.
      int num_tracevars = ppt.num_tracevars;
      int vals_array_size = ppt.var_infos.length - ppt.num_static_constant_vars;
      // This is no longer true; we now derive variables before reading dtrace!
      // Assert.assertTrue(vals_array_size == num_tracevars + ppt.num_orig_vars);

      // Read an invocation nonce if one exists
      Integer nonce = null;
      {
        // arbitrary number, hopefully big enough; catch exceptions
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
          Assert.assertTrue(nonce_name.equals("this_invocation_nonce"));
          nonce = new Integer(reader.readLine());

          if (Global.debugPrintDtrace) {
            to_write_nonce = true;
            nonce_value = nonce.toString();
            nonce_string = nonce_name_maybe;
          }
        }
      }

      Object[] vals = new Object[vals_array_size];
      int[] mods = new int[vals_array_size];

      // Read a single record from the trace file;
      // fills up vals and mods arrays by side effect.
      try {
        read_vals_and_mods_from_trace_file(
          reader,
          filename.toString(),
          ppt,
          vals,
          mods);
      } catch (IOException e) {
        String nextLine = reader.readLine();
        if ((e instanceof EOFException) || (nextLine == null)) {
          System.out.println();
          System.out.println(
            "WARNING: Unexpected EOF while processing "
              + "trace file - last record of trace file ignored");
          break;
        } else if (dkconfig_continue_after_file_exception) {
          System.out.println();
          System.out.println(
            "WARNING: IOException while processing "
              + "trace file - record ignored");
          System.out.print("Ignored backtrace:");
          e.printStackTrace(System.out);
          System.out.println();
          while (nextLine != null && !nextLine.equals("")) {
            // System.out.println("Discarded line " + reader.getLineNumber() + ": " + nextLine);
            nextLine = reader.readLine();
          }
          continue;
        } else {
          throw e;
        }
      }

      ValueTuple vt = ValueTuple.makeUninterned(vals, mods);

      // If we are only reading the sample, don't process them
      //        if (dkconfig_read_samples_only) {
      //  samples_processed++;
      //  continue;
      // }

      // Add orig and derived variables; pass to inference (add_and_flow)
      try {
        processor.process_sample(all_ppts, ppt, vt, nonce);
      } catch (Error e) {
        if (!dkconfig_continue_after_file_exception) {
          throw e;
        } else {
          System.out.println();
          System.out.println(
            "WARNING: Error while processing " + "trace file - record ignored");
          System.out.print("Ignored backtrace:");
          e.printStackTrace(System.out);
          System.out.println();
        }
      }
      // Debug.check (all_ppts, " ppt = " + ppt.name()
      //             + " " + Debug.related_vars (ppt, vt));
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

    data_trace_filename = null;
    data_trace_reader = null;
  }

  static java.lang.Runtime runtime = java.lang.Runtime.getRuntime();
  static PptTopLevel.Stats stats = new PptTopLevel.Stats();
  static PptTopLevel.Stats gstats = new PptTopLevel.Stats();
  static boolean store_stats = false;

  /**
   * Add orig() and derived variables to vt (by side effect), then
   * supply it to the program point for flowing.
   * @param vt trace data only; modified by side effect to add derived vars
   **/
  public static void process_sample(
    PptMap all_ppts,
    PptTopLevel ppt,
    ValueTuple vt,
    Integer nonce) {

    // We consider a sample processed even if we don't do any actual
    // processing below becaue of the dataflow optimizations.
    samples_processed++;

    { // For now, keep indentation the same
      {

        // Add orig variables.  This must be above the check below because
        // it saves away the orig values from enter points for later use
        // by exit points.
        add_orig_variables(ppt, vt.vals, vt.mods, nonce);

        // Only process the leaves of the ppt tree
        if (Daikon.use_dataflow_hierarchy) {
          if (!ppt.ppt_name.isExitPoint())
            return;
          Assert.assertTrue(!ppt.ppt_name.isCombinedExitPoint());
        }

        // Add derived variables
        add_derived_variables(ppt, vt.vals, vt.mods);

        // Causes interning
        vt = new ValueTuple(vt.vals, vt.mods);

        if (debugRead.isLoggable(Level.FINE)) {
          debugRead.fine("Adding ValueTuple to " + ppt.name());
          debugRead.fine("  length is " + vt.vals.length);
        }

        // If we are only reading the sample, don't process them
        if (dkconfig_read_samples_only) {
          samples_processed++;
          return;
        }

        long start = 0;
        long start_mem = 0;
        if (Daikon.debugStats.isLoggable(Level.FINE)) {
          // runtime.gc();
          start_mem = runtime.freeMemory();
          start = System.currentTimeMillis();
        }

        if (Daikon.dkconfig_df_bottom_up)
          ppt.add_global_bottom_up(vt, 1);
        else
          ppt.add_and_flow(vt, 1);

        if (debugVars.isLoggable(Level.FINE))
          debugVars.fine(ppt.name() + " vars: " + Debug.int_vars(ppt, vt));

        // Keep track of statistics
        if (Daikon.debugStats.isLoggable(Level.FINE)) {
          // runtime.gc();
          if (store_stats)
            stats = new PptTopLevel.Stats();
          stats.set(
            ppt,
            (int) (System.currentTimeMillis() - start),
            (int) (start_mem - runtime.freeMemory()));
          if (PptTopLevel.global != null)
            gstats.set(
              PptTopLevel.global,
              (int) (System.currentTimeMillis() - start),
              (int) (start_mem - runtime.freeMemory()));
          if (store_stats) {
            List slist = (List) Global.stats_map.get(ppt);
            if (slist == null) {
              slist = new ArrayList();
              Global.stats_map.put(ppt, slist);
            }
            slist.add(stats);
          } else {
            //if ((ppt.num_samples() < 10) || (ppt.num_samples() % 100) == 0) {
            if (!debugVars.isLoggable(Level.FINE))
              Daikon.debugStats.fine("vars: " + Debug.related_vars(ppt, vt));
            if (PptTopLevel.global != null)
              gstats.dump(Daikon.debugStats);
            stats.dump(Daikon.debugStats);
            //if ((ppt.num_samples() % 10) == 0)
            //PptTopLevel.count_unique_inv_lists (Daikon.debugStats, all_ppts);
            //}
          }
        }

      }
    }
    if (Global.debugPrintDtrace) {
      Global.dtraceWriter.close();
    }

  }

  public static void process_unmatched_procedure_entries() {

    if (dkconfig_unmatched_procedure_entries_quiet)
      return;

    if ((!call_stack.empty()) || (!call_hashmap.isEmpty())) {
      System.out.println();
      System.out.println(
        "No return from procedure observed "
          + UtilMDE.nplural((call_stack.size() + call_hashmap.size()), "time")
          + ".");
      if (!call_hashmap.isEmpty()) {
        System.out.println("Unterminated calls:");
        if (dkconfig_verbose_unmatched_procedure_entries) {
          // Print the invocations in sorted order.
          TreeSet keys = new TreeSet(call_hashmap.keySet());
          ArrayList invocations = new ArrayList();
          for (Iterator itor = keys.iterator(); itor.hasNext();) {
            invocations.add(call_hashmap.get(itor.next()));
          }
          print_invocations_verbose(invocations);
        } else {
          print_invocations_grouped(call_hashmap.values());
        }
      }

      if (!call_stack.empty()) {
        System.out.println("Remaining call stack:");
        if (dkconfig_verbose_unmatched_procedure_entries) {
          print_invocations_verbose(call_stack);
        } else {
          print_invocations_grouped(call_stack);
        }
      }
      System.out.println("End of report for procedures not returned from.");
    }
  }

  /** Print all the invocations in the collection, in order. **/
  static void print_invocations_verbose(Collection invocations) {
    for (Iterator i = invocations.iterator(); i.hasNext();) {
      Invocation invok = (Invocation) i.next();
      System.out.println(invok.format());
    }
  }

  /**
   * Print the invocations in the collection, in no particular order, but
   * suppressing duplicates.
   **/
  static void print_invocations_grouped(Collection invocations) {
    // Maps an Invocation to its frequency
    Map counter = new HashMap();

    for (Iterator i = invocations.iterator(); i.hasNext();) {
      Invocation invok = (Invocation) i.next();
      invok = invok.canonicalize();
      if (counter.containsKey(invok)) {
        Integer oldCount = (Integer) counter.get(invok);
        Integer newCount = new Integer(oldCount.intValue() + 1);
        counter.put(invok, newCount);
      } else {
        counter.put(invok, new Integer(1));
      }
    }

    for (Iterator i = counter.keySet().iterator(); i.hasNext();) {
      Invocation invok = (Invocation) i.next();
      Integer count = (Integer) counter.get(invok);
      System.out.println(
        UtilMDE.nplural(count.intValue(), "instance") + " of:");
      System.out.println(invok.format());
    }
  }

  // This procedure reads a single record from a trace file and
  // fills up vals and mods by side effect.
  private static void read_vals_and_mods_from_trace_file(
    LineNumberReader reader,
    String filename,
    PptTopLevel ppt,
    Object[] vals,
    int[] mods)
    throws IOException {
    VarInfo[] vis = ppt.var_infos;
    int num_tracevars = ppt.num_tracevars;

    String[] oldvalue_reps;
    if ((oldvalue_reps = (String[]) ppt_to_value_reps.get(ppt)) == null) {
      // We've not encountered this program point before.  The nulls in
      // this array will compare non-equal to whatever is in the trace
      // file, which is the desired behavior.
      oldvalue_reps = new String[num_tracevars];
    }

    if (Global.debugPrintDtrace) {
      Global.dtraceWriter.println(ppt.name());

      if (to_write_nonce) {
        Global.dtraceWriter.println(nonce_string);
        Global.dtraceWriter.println(nonce_value);
        to_write_nonce = false;
      }
    }

    for (int vi_index = 0, val_index = 0;
      val_index < num_tracevars;
      vi_index++) {
      Assert.assertTrue(vi_index < vis.length
      // , "Got to vi_index " + vi_index + " after " + val_index + " of " + num_tracevars + " values"
      );
      VarInfo vi = vis[vi_index];
      Assert.assertTrue((!vi.is_static_constant) || (vi.value_index == -1)
      // , "Bad value_index " + vi.value_index + " when static_constant_value = " + vi.static_constant_value + " for " + vi.repr() + " at " + ppt_name
      );
      if (vi.is_static_constant)
        continue;
      Assert.assertTrue(val_index == vi.value_index
      // , "Differing val_index = " + val_index
      // + " and vi.value_index = " + vi.value_index
      // + " for " + vi.name + lineSep + vi.repr()
      );

      // In errors, say "for program point", not "at program point" as the
      // latter confuses Emacs goto-error.

      String line = reader.readLine();
      if (line == null) {
        throw new EOFException(
          "Unexpected end of file at "
            + data_trace_filename
            + " line "
            + reader.getLineNumber()
            + "\n  Expected variable "
            + vi.name.name()
            + ", got "
            + line
            + " for program point "
            + ppt.name());
      }

      while ((Daikon.var_omit_regexp != null)
        && (line != null)
        && Global.regexp_matcher.contains(line, Daikon.var_omit_regexp)) {
        line = reader.readLine(); // value
        line = reader.readLine(); // modbit
        if (!((line.equals("0") || line.equals("1") || line.equals("2")))) {
          throw new FileIOException("Bad modbit", reader, data_trace_filename);
        }
        line = reader.readLine(); // next variable name
      }

      if (!VarInfoName.parse(line).equals(vi.name)) {
        throw new FileIOException(
          "Mismatch between .dtrace file and .decls file.  Expected variable "
            + vi.name.name()
            + ", got "
            + line
            + " for program point "
            + ppt.name(),
          reader,
          data_trace_filename);
      }
      line = reader.readLine();
      if (line == null) {
        throw new EOFException(
          "Unexpected end of file at "
            + data_trace_filename
            + " line "
            + reader.getLineNumber()
            + "\n  Expected value for variable "
            + vi.name.name()
            + ", got "
            + line
            + " for program point "
            + ppt.name());
      }
      String value_rep = line;
      line = reader.readLine();
      if (line == null) {
        throw new EOFException(
          "Unexpected end of file at "
            + data_trace_filename
            + " line "
            + reader.getLineNumber()
            + "\n  Expected modbit for variable "
            + vi.name.name()
            + ", got "
            + line
            + " for program point "
            + ppt.name());
      }
      if (!((line.equals("0") || line.equals("1") || line.equals("2")))) {
        throw new FileIOException("Bad modbit", reader, data_trace_filename);
      }
      int mod = ValueTuple.parseModified(line);

      // System.out.println("Mod is " + mod + " at " + data_trace_filename + " line " + reader.getLineNumber()
      //                   + "\n  for variable " + vi.name.name()
      //                   + " for program point " + ppt.name());

      // MISSING_FLOW is only found during flow algorithm
      Assert.assertTrue(
        mod != ValueTuple.MISSING_FLOW,
        "Data trace value can't be missing due to flow");

      if (mod != ValueTuple.MISSING_NONSENSICAL) {
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
        Global.dtraceWriter.println(vi.name.name());
        Global.dtraceWriter.println(value_rep);
        Global.dtraceWriter.println(mod);
      }
      Debug dbg = Debug.newDebug(FileIO.class, ppt, Debug.vis(vi));
      if (dbg != null)
        dbg.log(
          "Var " + vi.name.name() + " has value " + value_rep + " mod " + mod);

      // Both uninit and nonsensical mean missing modbit 2, because
      // it doesn't make sense to look at x.y when x is uninitialized.
      if (ValueTuple.modIsMissingNonsensical(mod)) {
        if (!(value_rep.equals("nonsensical")
          || value_rep.equals("uninit") // backward compatibility (9/27/2002)
          || value_rep.equals("missing"))) {
          System.out.println(
            "\nModbit indicates missing value for variable "
              + vi.name.name()
              + " with value \""
              + value_rep
              + "\";\n  text of value should be \"nonsensical\""
              + " or \"uninit\" at "
              + data_trace_filename
              + " line "
              + reader.getLineNumber());
          System.exit(1);
        } else {
          // Keep track of variables that can be missing
          vi.canBeMissing = true;
        }
        vals[val_index] = null;
      } else {
        // System.out.println("Mod is " + mod + " (missing=" +
        // ValueTuple.MISSING + "), rep=" + value_rep +
        // "(modIsMissing=" + ValueTuple.modIsMissing(mod) + ")");

        try {
          vals[val_index] = vi.rep_type.parse_value(value_rep);
          if (vals[val_index] == null) {
            mods[val_index] = ValueTuple.MISSING_NONSENSICAL;
            vi.canBeMissing = true;
            if (false)
              Fmt.pf(
                "Var %s in ppt %s at line %s is null and not missing",
                vi.name.name(),
                ppt.name(),
                "" + FileIO.data_trace_reader.getLineNumber());
          }
        } catch (Exception e) {
          throw new FileIOException(
            "Error while parsing value "
              + value_rep
              + " for variable "
              + vi.name.name()
              + " of type "
              + vi.rep_type
              + ": "
              + e.toString(),
            reader,
            filename);
        }
      }
      val_index++;

    }

    ppt_to_value_reps.put(ppt, oldvalue_reps);

    if (Global.debugPrintDtrace) {
      Global.dtraceWriter.println();
    }

    // Expecting the end of a block of values.
    String line = reader.readLine();
    // First, we might get some variables that ought to be omitted.
    while ((Daikon.var_omit_regexp != null)
      && (line != null)
      && Global.regexp_matcher.contains(line, Daikon.var_omit_regexp)) {
      line = reader.readLine(); // value
      line = reader.readLine(); // modbit
      line = reader.readLine(); // next variable name
    }
    Assert.assertTrue(
      (line == null) || (line.equals("")),
      "Expected blank line at line " + reader.getLineNumber() + ": " + line);
  }

  public static void add_orig_variables(PptTopLevel ppt,
  // HashMap cumulative_modbits,
  Object[] vals, int[] mods, Integer nonce) {
    VarInfo[] vis = ppt.var_infos;
    String fn_name = ppt.ppt_name.getNameWithoutPoint();
    String ppt_name = ppt.name();
    if (ppt_name.endsWith(enter_tag)) {
      Invocation invok = new Invocation(ppt, vals, mods);
      if (nonce == null) {
        call_stack.push(invok);
      } else {
        call_hashmap.put(nonce, invok);
      }
      return;
    }

    if (ppt.ppt_name.isExitPoint() || ppt.ppt_name.isThrowsPoint()) {
      Invocation invoc;
      // Set invoc
      {
        if (nonce == null) {
          if (call_stack.empty()) {
            throw new Error(
              "Function exit without corresponding entry: " + ppt.name());
          }
          invoc = (Invocation) call_stack.pop();
          while (invoc.ppt.ppt_name.getNameWithoutPoint() != fn_name) {
            // Should also mark as a function that made an exceptional exit
            // at runtime.
            System.err.println(
              "Exceptional exit from function "
                + fn_name
                + ", expected to first exit from "
                + invoc.ppt.ppt_name.getNameWithoutPoint()
                + ((data_trace_filename == null)
                  ? ""
                  : "; at "
                    + data_trace_filename
                    + " line "
                    + data_trace_reader.getLineNumber()));
            invoc = (Invocation) call_stack.pop();
          }
        } else {
          // nonce != null
          invoc = (Invocation) call_hashmap.get(nonce);
          if (invoc == null) {
            throw new Error(
              "Didn't find call with nonce "
                + nonce
                + " to match "
                + ppt.name()
                + " ending at "
                + data_trace_filename
                + " line "
                + data_trace_reader.getLineNumber());
          }
          invoc = (Invocation) call_hashmap.get(nonce);
          call_hashmap.remove(nonce);
        }
      }
      Assert.assertTrue(invoc != null);
      for (int i = 0; i < ppt.num_orig_vars; i++) {
        vals[ppt.num_tracevars + i] = invoc.vals[i];
        int mod = invoc.mods[i];
        mods[ppt.num_tracevars + i] = mod;
        // Possibly more efficient to set this all at once, late in
        // the game; but this gets it done.
        // It was once moved to PptTopLevel.add(ValueTuple,int), but it
        // disappeared from there, so now it's back here.
        // In the long run canBeMissing should perhaps go away
        if (ValueTuple.modIsMissingNonsensical(mods[ppt.num_tracevars + i])) {
          vis[ppt.num_tracevars + i].canBeMissing = true;
          Assert.assertTrue(vals[ppt.num_tracevars + i] == null);
        }
      }
    }
  }

  // Add derived variables
  public static void add_derived_variables(
    PptTopLevel ppt,
    Object[] vals,
    int[] mods) {
    // This ValueTuple is temporary:  we're temporarily suppressing interning,
    // which we will do after we have all the values available.
    ValueTuple partial_vt = ValueTuple.makeUninterned(vals, mods);
    int filled_slots =
      ppt.num_orig_vars + ppt.num_tracevars + ppt.num_static_constant_vars;
    for (int i = 0; i < filled_slots; i++) {
      Assert.assertTrue(!ppt.var_infos[i].isDerived());
    }
    for (int i = filled_slots; i < ppt.var_infos.length; i++) {
      if (!ppt.var_infos[i].isDerived()) {
        // Check first because repr() can be slow
        Assert.assertTrue(
          ppt.var_infos[i].isDerived(),
          "variable not derived: " + ppt.var_infos[i].repr());
      }
    }
    int num_const = ppt.num_static_constant_vars;
    for (int i = filled_slots; i < ppt.var_infos.length; i++) {
      // Add this derived variable's value
      ValueAndModified vm =
        ppt.var_infos[i].derived.computeValueAndModified(partial_vt);
      vals[i - num_const] = vm.value;
      mods[i - num_const] = vm.modified;
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  /// Serialized PptMap files
  ///

  /**
   * Use a special record type.  Saving as one object allows for
   * reference-sharing, easier saves and loads, and potential for
   * later overriding of SerialFormat.readObject if the save format
   * changes (ick).
   **/
  static final class SerialFormat implements Serializable {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20020122L;

    public SerialFormat(PptMap map, Configuration config) {
      this.map = map;
      this.config = config;
    }
    public PptMap map;
    public Configuration config;
  }

  public static void write_serialized_pptmap(PptMap map, File file)
    throws IOException {
    SerialFormat record = new SerialFormat(map, Configuration.getInstance());
    UtilMDE.writeObject(record, file);
  }

  /**
   * Read either a serialized PptMap or a InvMap and return a
   * PptMap.  If an InvMap is specified, it is converted to a PptMap
   */
  public static PptMap read_serialized_pptmap(
    File file,
    boolean use_saved_config)
    throws IOException {

    try {
      Object obj = UtilMDE.readObject(file);
      if (obj instanceof FileIO.SerialFormat) {
        SerialFormat record = (SerialFormat) obj;
        if (use_saved_config) {
          Configuration.getInstance().overlap(record.config);
        }
        return (record.map);
      } else if (obj instanceof InvMap) {
        InvMap invs = (InvMap) obj;
        PptMap ppts = new PptMap();
        for (Iterator i = invs.pptIterator(); i.hasNext();) {
          PptTopLevel ppt = (PptTopLevel) i.next();
          PptTopLevel nppt = new PptTopLevel(ppt.name, ppt.var_infos);
          nppt.set_sample_number(ppt.num_samples());
          ppts.add(nppt);
          List /*Invariants*/
          inv_list = invs.get(ppt);
          for (Iterator j = inv_list.iterator(); j.hasNext();) {
            Invariant inv = (Invariant) j.next();
            PptSlice slice = nppt.get_or_instantiate_slice(inv.ppt.var_infos);
            inv.ppt = slice;
            slice.addInvariant(inv);
          }
        }
        return (ppts);
      } else {
        throw new IOException(
          "Unexpected serialized file type: " + obj.getClass());
      }
    } catch (ClassNotFoundException e) {
      throw new IOException("Error while loading inv file: " + e);
    } catch (InvalidClassException e) {
      throw new IOException(
        "It is likely that the .inv file format has changed, because a Daikon data structure has been modified, so your old .inv file is no longer readable by Daikon.  Please regenerate your .inv file.\n"
          + e.toString());
    }
    // } catch (StreamCorruptedException e) { // already extends IOException
    // } catch (OptionalDataException e) {    // already extends IOException
  }

  /**
   * Returns whether or not the specified ppt name should be included
   * in processing.  Ppts can be excluded because they match the omit_regexp,
   * don't match ppt_regexp, or are greater than ppt_max_name.
   */
  public static boolean ppt_included(String ppt_name) {

    // System.out.println ("ppt_name = '" + ppt_name + "' max name = '"
    //                     + Daikon.ppt_max_name + "'");
    if (((Daikon.ppt_omit_regexp != null)
      && Global.regexp_matcher.contains(ppt_name, Daikon.ppt_omit_regexp))
      || ((Daikon.ppt_regexp != null)
        && !Global.regexp_matcher.contains(ppt_name, Daikon.ppt_regexp))
      || ((Daikon.ppt_max_name != null)
        && ((Daikon.ppt_max_name.compareTo(ppt_name) < 0)
          && (ppt_name.indexOf(global_suffix) == -1))))
      return (false);
    else
      return (true);
  }
}
