package daikon;

import daikon.derive.ValueAndModified;
import daikon.config.Configuration;
import daikon.diff.InvMap;
import daikon.inv.Invariant;

import utilMDE.*;
import java.util.logging.Logger;
import java.util.logging.Level;

import java.io.*;
import java.net.*;
import java.util.*;

public final class FileIO {

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
  public static final String class_static_tag = ppt_tag_separator
                                                        + class_static_suffix;
  public static final String global_suffix = "GLOBAL";

  private static final String lineSep = Global.lineSep;


  /// Settings

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.

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
   * before reading.  This will disable the percentage progress printout.
   */
  public static boolean dkconfig_count_lines = true;

  /**
   * Boolean.  When true, only read the samples, but don't process them.
   * Used to gather timing information.
   */
  public static boolean dkconfig_read_samples_only = false;

  /** Boolean.  When true, don't print a warning about unmatched procedure
   * entries, which are ignored by Daikon (unless the --nohierarchy switch
   * is provided).
   **/
  public static boolean dkconfig_unmatched_procedure_entries_quiet = false;

  /**
   * Boolean.  If true, prints the unmatched procedure entries
   * verbosely.
   **/
  public static boolean dkconfig_verbose_unmatched_procedure_entries = false;

  /**
   * Boolean.  When true, suppress exceptions related to file reading.
   * This permits Daikon to continue even if there is a malformed trace
   * file.  Use this with care:  in general, it is better to fix the
   * problem that caused a bad trace file, rather than to suppress the
   * exception.
   **/
  public static boolean dkconfig_continue_after_file_exception = false;

  /**
   * Long integer. If non-zero, this value will be used as the number
   * of lines in (each) dtrace file input for the purposes of the
   * progress display, and the counting of the lines in the file will
   * be suppressed.
   */
  public static long dkconfig_dtrace_line_count = 0;

  /// Variables

  // This hashmap maps every program point to an array, which contains the
  // old values of all variables in scope the last time the program point
  // was executed. This enables us to determine whether the values have been
  // modified since this program point was last executed.
  static HashMap<PptTopLevel,String[]> ppt_to_value_reps = new HashMap<PptTopLevel,String[]>();

  // For debugging purposes: printing out a modified trace file with
  // changed modbits.
  private static boolean to_write_nonce = false;
  private static String nonce_value, nonce_string;

  // (This implementation as a public static variable is a bit unclean.)
  // Number of ignored declarations.
  public static int omitted_declarations = 0;

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

  /**
   * @param files files to be read (java.io.File)
   * @return a new PptMap containing declarations read from the files
   * listed in the argument; connection information (controlling
   * variables and entry ppts) is set correctly upon return.
   **/
  public static PptMap read_declaration_files(Collection<File> files) throws IOException {
    PptMap all_ppts = new PptMap();
    // Read all decls, creating PptTopLevels and VarInfos
    for (File file : files) {
      Daikon.progress = "Reading " + file;
      if (!Daikon.dkconfig_quiet) {
        System.out.print("."); // show progress
      }
      read_declaration_file(file, all_ppts);
    }
    return all_ppts;
  }

  /** Read one decls file; add it to all_ppts. **/
  public static void read_declaration_file(File filename, PptMap all_ppts)
    throws IOException {
    if (Daikon.using_DaikonSimple) {
      Processor processor = new DaikonSimple.SimpleProcessor();
      read_data_trace_file(filename.toString(), all_ppts, processor, true,
                           false);
    } else {
      Processor processor = new Processor();
      read_data_trace_file(filename.toString(), all_ppts, processor, true,
                           true);
    }

  }


  // The "DECLARE" line has already been read.
  private static PptTopLevel read_declaration(ParseState state)
    throws IOException {

    // We have just read the "DECLARE" line.
    String ppt_name = state.reader.readLine();
    if (ppt_name == null) {
      throw new FileIOException(
        "File ends with \"DECLARE\" with no following program point name",
        state.reader, state.filename);
    }
    ppt_name = ppt_name.intern();

    // This program point name has already been encountered.
    if (state.all_ppts.containsName(ppt_name)) {
      if (state.ppts_are_new)
        throw new FileIOException("Duplicate declaration of program point \""
                            + ppt_name + "\"", state.reader, state.filename);
      else { // ppts are already in the map
        skip_decl (state.reader);
        return state.all_ppts.get (ppt_name);
      }
    }

    // If we are excluding this ppt, just read the data and throw it away
    if (!ppt_included (ppt_name)) {
      skip_decl (state.reader);
      omitted_declarations++;
      return null;
    }

    // The var_infos that will populate the new program point
    List<VarInfo> var_infos = new ArrayList<VarInfo>();

    // Each iteration reads a variable name, type, and comparability.
    // Possibly abstract this out into a separate function??
    VarInfo vi;
    while ((vi = read_VarInfo(state.reader, state.varcomp_format,
                              state.file, ppt_name)) != null) {
      for (int i=0; i<var_infos.size(); i++) {
        if (vi.name == var_infos.get(i).name) {
          throw new FileIOException("Duplicate variable name", state.reader,
                                    state.filename);
        }
      }
      // Can't do this test in read_VarInfo, it seems, because of the test
      // against null above.
      if (!var_included (vi.name.name())) {
        continue;
      }
      var_infos.add(vi);
    }

    VarInfo[] vi_array = (VarInfo[])
                            var_infos.toArray(new VarInfo[var_infos.size()]);

    // taking care of visibility information
    // the information is needed in the variable hierarchy because private methods
    // should not be linked under the object program point
    // the ppt name is truncated before putting it in the pptMap because the visibility
    // information is only present in the decls file and not the dtrace file

    //    if (ppt_name.startsWith("public")) {
    //      int position = ppt_name.indexOf("public");
    //      ppt_name = ppt_name.substring(7);
    //      PptTopLevel newppt = new PptTopLevel(ppt_name, vi_array);
    //      newppt.ppt_name.setVisibility("public");
    //      return newppt;
    //    }
    //    if (ppt_name.startsWith("private")) {
    //      int position = ppt_name.indexOf("private");
    //      ppt_name = ppt_name.substring(8);
    //      PptTopLevel newppt = new PptTopLevel(ppt_name, vi_array);
    //      newppt.ppt_name.setVisibility("private");
    //      return newppt;
    //    }
    //    if (ppt_name.startsWith("protected")) {
    //      int position = ppt_name.indexOf("protected");
    //      ppt_name = ppt_name.substring(10);
    //      PptTopLevel newppt = new PptTopLevel(ppt_name, vi_array);
    //      newppt.ppt_name.setVisibility("protected");
    //      return newppt;
    //    }

    //TODO: add a new config variable to turn this accessibility flag processing on?
    PptTopLevel newppt = new PptTopLevel(ppt_name, vi_array);
    // newppt.ppt_name.setVisibility("package-protected");
    return newppt;
    // return new PptTopLevel(ppt_name, vi_array);
  }

  // So that warning message below is only printed once
  private static boolean seen_string_rep_type = false;

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
    if ( // (varname == null) || // just cheeck varname above
        (proglang_type_string_and_aux == null)
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
      if (!seen_string_rep_type) {
        seen_string_rep_type = true;
        System.err.println("Warning: Malformed trace file.  Representation type 'String' should be "+
                           "'java.lang.String' instead on line " +
                           (file.getLineNumber()-1) + " of " + filename);
      }
    }
    // This is for people who were confused by the above temporary
    // workaround when it didn't have a warning. But this has never
    // worked, so it's fatal.
    else if ("String[]".equals(file_rep_type_string)) {
      throw new FileIOException("Representation type 'String[]' should be " +
                                "'java.lang.String[]' instead for variable " +
                                varname, file, filename);
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
      throw new FileIOException(file, filename, e);
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
    // COMPARABILITY TEST
    // if (!(comparability.alwaysComparable()
    //       || ((VarComparabilityImplicit)comparability).dimensions == file_rep_type.dimensions())) {
    //   throw new FileIOException(
    //     "Rep type " + file_rep_type.format() + " has " + file_rep_type.dimensions() + " dimensions"
    //       + " but comparability " + comparability + " has " + ((VarComparabilityImplicit)comparability).dimensions + " dimensions"
    //       + " for variable "
    //       + varname,
    //     file,
    //     filename);
    // }

    return new VarInfo(
      VarInfoName.parse(varname),
      prog_type,
      file_rep_type,
      comparability,
      is_static_constant,
      static_constant_value,
      aux);
  }

  private static int read_var_comparability (LineNumberReader reader,
					     File filename)
    throws IOException {
    int varcomp_format;
    String line = reader.readLine();
    if (line == null) {
      throw new FileIOException("Found end of file, expected comparability",
                                reader,
                                filename);
    }
    if (line.equals("none")) {
      varcomp_format = VarComparability.NONE;
    } else if (line.equals("implicit")) {
      varcomp_format = VarComparability.IMPLICIT;
    } else {
      throw new FileIOException("Unrecognized VarComparability",
				reader,
				filename);
    }
    return varcomp_format;
  }

  private static void read_list_implementors (LineNumberReader reader,
					      File filename)
    throws IOException {
    // Each line following is the name (in JVM form) of a class
    // that implements java.util.List.
    for (;;) {
      String line = reader.readLine();
      if (line == null || line.equals(""))
	break;
      if (isComment(line))
	continue;
      ProglangType.list_implementors.add(line.intern());
    }
  }






  ///////////////////////////////////////////////////////////////////////////
  /// invocation tracking for dtrace files entry/exit grouping
  ///

  static final class Invocation implements Comparable<Invocation> {
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
      return format(true);
    }

    // Print the Invocation on one or two lines, indented by two spaces
    String format(boolean show_values) {
      if (! show_values) {
        return "  " + ppt.ppt_name.getNameWithoutPoint();
      }

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

    public int compareTo(Invocation other) {
      return ppt.name().compareTo(other.ppt.name());
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
  static Stack<Invocation> call_stack = new Stack<Invocation>();
  static HashMap<Integer,Invocation> call_hashmap = new HashMap<Integer,Invocation>();

  /** Reads data trace files using the default sample processor. **/
  public static void read_data_trace_files(Collection<String> files,
                                           PptMap all_ppts) throws IOException {

    Processor processor = new Processor();
    read_data_trace_files(files, all_ppts, processor, true);
  }

  /**
   * Read data from .dtrace files.
   * Calls @link{read_data_trace_file(File,PptMap,Pattern,false)} for each
   * element of filenames.
   *
   * @param ppts_are_new - true if declarations of ppts read from the data
   *                       trace file are new (and thus are not in all_ppts)
   *                       false if the ppts may already be there.
   **/
  public static void read_data_trace_files(Collection<String> files,
                PptMap all_ppts, Processor processor, boolean ppts_are_new)
                throws IOException {

    for (String filename : files) {
      try {
        read_data_trace_file(filename, all_ppts, processor, false,
                             ppts_are_new);
      } catch (IOException e) {
        if (e.getMessage().equals("Corrupt GZIP trailer")) {
          System.out.println(
            filename
              + " has a corrupt gzip trailer.  "
              + "All possible data was recovered.");
        } else {
          throw e;
        }
      }
    }

    process_unmatched_procedure_entries();
  }

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
  static void read_data_trace_file(String filename, PptMap all_ppts)
    throws IOException {
    Processor processor = new Processor();
    read_data_trace_file(filename, all_ppts, processor, false, true);
  }

  /**
   * Class used to encapsulate state information while parsing
   * decl/dtrace files.
   */

  public enum ParseStatus {
    NULL,		// haven't read anything yet
    DECL,		// got a decl
    SAMPLE,		// got a sample
    COMPARABILITY,	// got a VarComparability declaration
    LIST,		// got a ListImplementors declaration
    EOF,		// found EOF
    ERROR,		// continuable error; fatal errors thrown as exceptions
    TRUNCATED		// dkconfig_max_line_number reached
  };

  public static class ParseState {
    public String filename;
    public boolean is_decl_file;
    public boolean ppts_are_new;
    public PptMap all_ppts;
    public LineNumberReader reader;
    public File file;
    public long total_lines;
    public int varcomp_format;
    public ParseStatus status;
    public PptTopLevel ppt;	// returned when state=DECL or SAMPLE
    public Integer nonce;	// returned when state=SAMPLE
    public ValueTuple vt;	// returned when state=SAMPLE
    public long lineNum;

    public ParseState (String raw_filename, boolean decl_file_p,
                       boolean ppts_are_new, PptMap ppts) throws IOException {
      // Pretty up raw_filename for use in messages
      file = new File(raw_filename);
      if (raw_filename.equals("-")) {
        filename = "standard input";
      }
      else if (raw_filename.equals("+")) {
        filename = "chicory socket";
      }
      else {
        // Remove directory parts, to make it shorter
        filename = file.getName();
      }

      is_decl_file = decl_file_p;
      this.ppts_are_new = ppts_are_new;
      all_ppts = ppts;

      // Do we need to count the lines in the file?
      total_lines = 0;
      boolean count_lines = dkconfig_count_lines;
      if (is_decl_file) {
        count_lines = false;
      } else if (dkconfig_dtrace_line_count != 0) {
        total_lines = dkconfig_dtrace_line_count;
        count_lines = false;
      } else if (filename.equals("-")) {
        count_lines = false;
      } else if (Daikon.dkconfig_progress_delay == -1) {
        count_lines = false;
      } else if ((new File(filename)).length() == 0) {
	// Either it's actually empty, or it's something like a pipe.
        count_lines = false;
      }

      if (count_lines) {
        Daikon.progress = "Checking size of " + filename;
        total_lines = UtilMDE.count_lines(raw_filename);
      }

      // Open the reader stream
      if (raw_filename.equals("-")) {
	// "-" means read from the standard input stream
        Reader file_reader = new InputStreamReader(System.in, "ISO-8859-1");
        reader = new LineNumberReader(file_reader);
      }
      else if (raw_filename.equals("+")) { //socket comm with Chicory
        InputStream chicoryInput = connectToChicory();
        InputStreamReader chicReader = new InputStreamReader(chicoryInput);
        reader = new LineNumberReader(chicReader);
      } else {
        reader = UtilMDE.lineNumberFileReader(raw_filename);
      }

      varcomp_format = VarComparability.IMPLICIT;
      status = ParseStatus.NULL;
      ppt = null;
    }
  }

  private static InputStream connectToChicory()
    {


        ServerSocket daikonServer = null;
        try
        {
            daikonServer = new ServerSocket(0); //bind to any free port

            //tell Chicory what port we have!
            System.out.println("DaikonChicoryOnlinePort=" + daikonServer.getLocalPort());

            daikonServer.setReceiveBufferSize(64000);

        }
        catch (IOException e)
        {
            throw new RuntimeException("Unable to create server", e);
        }

        Socket chicSocket = null;
        try
        {
            daikonServer.setSoTimeout(5000);

            //System.out.println("waiting for chicory connection on port " + daikonServer.getLocalPort());
            chicSocket = daikonServer.accept();
        }
        catch (IOException e)
        {
            throw new RuntimeException("Unable to connect to Chicory", e);
        }


        try
        {
            return chicSocket.getInputStream();
        }
        catch (IOException e)
        {
            throw new RuntimeException("Unable to get Chicory's input stream", e);
        }

    }


  /** Stash state here to be examined/printed by other parts of Daikon. */
  public static ParseState data_trace_state = null;

  /**
   * Total number of samples passed to process_sample().
   * Not part of data_trace_state because it's global over all files
   * processed by Daikon.
   */
  public static int samples_processed = 0;


  /** Read data from .dtrace file. **/
  static void read_data_trace_file(String filename, PptMap all_ppts,
                                   Processor processor,
                                   boolean is_decl_file, boolean ppts_are_new)
    throws IOException {

    if (debugRead.isLoggable(Level.FINE)) {
      debugRead.fine ("read_data_trace_file " + filename
                      + ((Daikon.ppt_regexp != null)
                         ? " " + Daikon.ppt_regexp.pattern() : "")
                      + ((Daikon.ppt_omit_regexp != null)
                         ? " " + Daikon.ppt_omit_regexp.pattern() : ""));
    }

    data_trace_state = new ParseState(filename, is_decl_file, ppts_are_new,
                                      all_ppts);

    // Used for debugging: write new data trace file.
    if (Global.debugPrintDtrace) {
      Global.dtraceWriter
             = new PrintWriter(new FileWriter(new File(filename + ".debug")));
    }

    while (true) {
      read_data_trace_record (data_trace_state);
      if (data_trace_state.status == ParseStatus.SAMPLE) {
	// Keep track of the total number of samples we have seen.
	samples_processed++;
	// Add orig and derived variables; pass to inference (add_and_flow)
	try {
	  processor.process_sample (data_trace_state.all_ppts,
				    data_trace_state.ppt,
				    data_trace_state.vt,
				    data_trace_state.nonce);
	} catch (Error e) {
	  if (! dkconfig_continue_after_file_exception) {
	    throw e;
	  } else {
	    System.out.println ();
	    System.out.println ("WARNING: Error while processing "
				+ "trace file - record ignored");
	    System.out.print ("Ignored backtrace:");
	    e.printStackTrace(System.out);
	    System.out.println ();
	  }
	}
      }
      else if ((data_trace_state.status == ParseStatus.EOF)
	       || (data_trace_state.status == ParseStatus.TRUNCATED)) {
	break;
      }
      else
	;  // don't need to do anything explicit for other records found
    }

    if (Global.debugPrintDtrace) {
      Global.dtraceWriter.close();
    }

    Daikon.progress = "Finished reading " + data_trace_state.filename;
    data_trace_state = null;
  }


  // read a single record (declaration or sample) from a dtrace file.
  public static void read_data_trace_record (ParseState state)
    throws IOException {

    LineNumberReader reader = state.reader;

    // "line_" is uninterned, "line" is interned
    for (String line_ = reader.readLine(); line_ != null;
	 line_ = reader.readLine()) {
      if (line_.equals("") || isComment(line_)) {
        continue;
      }
      state.lineNum = reader.getLineNumber();

      // stop at a specified point in the file
      if ((dkconfig_max_line_number > 0)
          && (state.lineNum > dkconfig_max_line_number))
	{
	  state.status = ParseStatus.TRUNCATED;
	  return;
	}

      String line = line_.intern();

      // First look for declarations in the dtrace stream
      if (line == declaration_header) {
        state.ppt = read_declaration(state);
        // ppt can be null if this declaration was skipped because of
        // --ppt-select-pattern or --ppt-omit-pattern.
        if (state.ppt != null) {
          if (!state.all_ppts.containsName (state.ppt.name())) {
            state.all_ppts.add(state.ppt);
            Daikon.init_ppt(state.ppt, state.all_ppts);
          }
	}
	state.status = ParseStatus.DECL;
	return;
      }
      if (line.equals("VarComparability")) {
	state.varcomp_format = read_var_comparability (reader, state.file);
        state.status = ParseStatus.COMPARABILITY;
	return;
      }
      if (line.equals("ListImplementors")) {
	read_list_implementors (reader, state.file);
	state.status = ParseStatus.LIST;
	return;
      }
      if (!ppt_included (line)) {
        // System.out.printf ("skipping ppt %s\n", line);
        while ((line != null) && !line.equals(""))
          line = reader.readLine();
        continue;
      }
      // System.out.printf ("Not skipping ppt  %s\n", line);

      // If we got here, we're looking at a sample and not a declaration.
      // For compatibility with previous implementation, if this is a
      // declaration file, skip over samples.
      if (state.is_decl_file) {
	if (debugRead.isLoggable(Level.FINE))
	  debugRead.fine("Skipping paragraph starting at line "
			 + reader.getLineNumber()
			 + " of file "
			 + state.filename
			 + ": "
			 + line);
	while ((line != null) && (!line.equals("")) && (!isComment(line))) {
	  System.out.println("Unrecognized paragraph contains line = `"
			     + line
			     + "'");
	  System.out.println(" line: null="
			     + false // (line != null)
			     + " empty="
			     + (line.equals(""))
			     + " comment="
			     + (isComment(line)));
	  line = reader.readLine();
	}
	continue;
      }


      // Parse the ppt name
      String ppt_name = line; // already interned
      try {
        new PptName(ppt_name);
      } catch (Error e) {
        throw new Error("Illegal program point name \"" + ppt_name + "\""
                        + " at " + state.filename
                        + " line " + reader.getLineNumber());
      }

      PptTopLevel ppt = state.all_ppts.get(ppt_name);
      if (ppt == null) {
        throw new Error("Program point " + ppt_name
                        + " appears in dtrace file " + state.filename
                        + " at line " + reader.getLineNumber()
                        + " but not in any decl file");
      }

      VarInfo[] vis = ppt.var_infos;

      // not vis.length, as that includes constants, derived variables, etc.
      // Actually, we do want to leave space for _orig vars.
      // And for the time being (and possibly forever), for derived variables.
      int num_tracevars = ppt.num_tracevars;
      int vals_array_size = ppt.var_infos.length - ppt.num_static_constant_vars;

      // Read an invocation nonce if one exists
      Integer nonce = null;

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
          Assert.assertTrue(nonce_name != null && nonce_name.equals("this_invocation_nonce"));
          String nonce_number = reader.readLine();
          if (nonce_number == null) {
            throw new FileIOException("File ended while trying to read nonce",
                                      reader,
                                      state.file);
          }
          nonce = new Integer(nonce_number);

          if (Global.debugPrintDtrace) {
            to_write_nonce = true;
            nonce_value = nonce.toString();
            nonce_string = nonce_name_maybe;
          }
      }

      Object[] vals = new Object[vals_array_size];
      int[] mods = new int[vals_array_size];

      // Read a single record from the trace file;
      // fills up vals and mods arrays by side effect.
      try {
        read_vals_and_mods_from_trace_file (reader, state.filename,
                                            ppt, vals, mods);
      } catch (IOException e) {
        String nextLine = reader.readLine();
        if ((e instanceof EOFException) || (nextLine == null)) {
          System.out.println ();
          System.out.println ("WARNING: Unexpected EOF while processing "
                        + "trace file - last record of trace file ignored");
	  state.status = ParseStatus.EOF;
	  return;
        } else if (dkconfig_continue_after_file_exception) {
          System.out.println ();
          System.out.println ("WARNING: IOException while processing "
			      + "trace file - record ignored");
          System.out.print ("Ignored backtrace:");
          e.printStackTrace(System.out);
          System.out.println ();
          while (nextLine != null && ! nextLine.equals("")) {
            // System.out.println("Discarded line " + reader.getLineNumber()
            //                     + ": " + nextLine);
            nextLine = reader.readLine();
          }
          continue;
        } else {
          throw e;
        }
      }

      state.ppt = ppt;
      state.nonce = nonce;
      state.vt = ValueTuple.makeUninterned(vals, mods);
      state.status = ParseStatus.SAMPLE;
      return;
    }

    state.status = ParseStatus.EOF;
    return;
  }


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

    // Add orig variables.  This must be above the check below because
    // it saves away the orig values from enter points for later use
    // by exit points.
    add_orig_variables(ppt, vt.vals, vt.mods, nonce);

    // Only process the leaves of the ppt tree.
    // This test assumes that all leaves are numbered exit program points
    // -- that is, points of the form foo:::EXIT22 for which isExitPoint()
    // is true and isCombinedExitPoint() is false.  "Combined" exit points
    // of the form foo:::EXIT are not processed -- they are assumed to be
    // non-leaves.
    if (Daikon.use_dataflow_hierarchy) {
      if (!ppt.ppt_name.isExitPoint())
        return;
      Assert.assertTrue (!ppt.ppt_name.isCombinedExitPoint());
    }

    // Add derived variables
    add_derived_variables(ppt, vt.vals, vt.mods);

    // Causes interning
    vt = new ValueTuple(vt.vals, vt.mods);

    if (debugRead.isLoggable(Level.FINE)) {
      debugRead.fine ("Adding ValueTuple to " + ppt.name());
      debugRead.fine ("  length is " + vt.vals.length);
    }

    // If we are only reading the sample, don't process them
    if (dkconfig_read_samples_only) {
      return;
    }

    ppt.add_bottom_up (vt, 1);

    if (debugVars.isLoggable (Level.FINE))
      debugVars.fine (ppt.name() + " vars: " + Debug.int_vars (ppt, vt));

    if (Global.debugPrintDtrace) {
      Global.dtraceWriter.close();
    }

  }

  /** Returns non-null if this procedure has an unmatched entry. **/
  static boolean has_unmatched_procedure_entry(PptTopLevel ppt) {
    for (Invocation invok : call_hashmap.values()) {
      if (invok.ppt == ppt) {
        return true;
      }
    }
    for (Invocation invok : call_stack) {
      if (invok.ppt == ppt) {
        return true;
      }
    }
    return false;
  }


  /**
   * Print each call that does not have a matching exit
   */
  public static void process_unmatched_procedure_entries() {

    if (dkconfig_unmatched_procedure_entries_quiet)
      return;

    int unmatched_count = call_stack.size() + call_hashmap.size();

    if ((!call_stack.empty()) || (!call_hashmap.isEmpty())) {
      System.out.println();
      System.out.print(
        "No return from procedure observed "
          + UtilMDE.nplural(unmatched_count, "time") + ".");
      if (Daikon.use_dataflow_hierarchy) {
        System.out.print("  Unmatched entries are ignored!");
      }
      System.out.println();
      if (!call_hashmap.isEmpty()) {
        System.out.println("Unterminated calls:");
        if (dkconfig_verbose_unmatched_procedure_entries) {
          // Print the invocations in sorted order.
          // (Does this work?  The keys are integers. -MDE 7/1/2005.)
          TreeSet<Integer> keys = new TreeSet<Integer>(call_hashmap.keySet());
          ArrayList<Invocation> invocations = new ArrayList<Invocation>();
          for (Integer i : keys) {
            invocations.add(call_hashmap.get(i));
          }
          print_invocations_verbose(invocations);
        } else {
          print_invocations_grouped(call_hashmap.values());
        }
      }

      if (!call_stack.empty()) {
        if (dkconfig_verbose_unmatched_procedure_entries) {
          System.out.println("Remaining " +
                             UtilMDE.nplural(unmatched_count, "stack")
                             + " call summarized below.");
          print_invocations_verbose(call_stack);
        } else {
          print_invocations_grouped(call_stack);
        }
      }
      System.out.print("End of report for procedures not returned from.");
      if (Daikon.use_dataflow_hierarchy) {
        System.out.print("  Unmatched entries are ignored!");
      }
      System.out.println();
    }
  }

  /** Print all the invocations in the collection, in order. **/
  static void print_invocations_verbose(Collection<Invocation> invocations) {
    for (Invocation invok : invocations) {
      System.out.println(invok.format());
    }
  }

  /**
   * Print the invocations in the collection, in order, and
   * suppressing duplicates.
   **/
  static void print_invocations_grouped(Collection<Invocation> invocations) {
    Map<Invocation,Integer> counter = new HashMap<Invocation,Integer>();

    for (Invocation invok : invocations) {
      invok = invok.canonicalize();
      if (counter.containsKey(invok)) {
        Integer oldCount = counter.get(invok);
        Integer newCount = new Integer(oldCount.intValue() + 1);
        counter.put(invok, newCount);
      } else {
        counter.put(invok, new Integer(1));
      }
    }

    // Print the invocations in sorted order.
    TreeSet<Invocation> keys = new TreeSet<Invocation>(counter.keySet());
    for (Invocation invok : keys) {
      Integer count = counter.get(invok);
      System.out.println(invok.format(false) + " : "
                         + UtilMDE.nplural(count.intValue(), "invocation"));
    }
  }

  // This procedure reads a single record from a trace file and
  // fills up vals and mods by side effect.  The ppt name and
  // invocation nonce (if any) have already been read.
  private static void read_vals_and_mods_from_trace_file
                        (LineNumberReader reader, String filename,
                         PptTopLevel ppt, Object[] vals, int[] mods)
    throws IOException
  {
    VarInfo[] vis = ppt.var_infos;
    int num_tracevars = ppt.num_tracevars;

    String[] oldvalue_reps = ppt_to_value_reps.get(ppt);
    if (oldvalue_reps == null) {
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
            + data_trace_state.filename
            + " line "
            + reader.getLineNumber() + lineSep
            + "  Expected variable "
            + vi.name.name()
            + ", got "
            + "null" // line
            + " for program point "
            + ppt.name());
      }

      while ((line != null)
             && !line.equals("")
             && !var_included(line)) {
        line = reader.readLine(); // value (discard it)
        line = reader.readLine(); // modbit
        if (line == null
            || !((line.equals("0") || line.equals("1") || line.equals("2")))) {
          throw new FileIOException("Bad modbit", reader,
				    data_trace_state.filename);
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
          data_trace_state.filename);
      }
      line = reader.readLine();
      if (line == null) {
        throw new EOFException(
          "Unexpected end of file at "
            + data_trace_state.filename
            + " line "
            + reader.getLineNumber() + lineSep
            + "  Expected value for variable "
            + vi.name.name()
            + ", got "
            + "null" // line
            + " for program point "
            + ppt.name());
      }
      String value_rep = line;
      line = reader.readLine();
      if (line == null) {
        throw new EOFException(
          "Unexpected end of file at "
            + data_trace_state.filename
            + " line "
            + reader.getLineNumber() + lineSep
            + "  Expected modbit for variable "
            + vi.name.name()
            + ", got "
            + "null" // line
            + " for program point "
            + ppt.name());
      }
      if (!((line.equals("0") || line.equals("1") || line.equals("2")))) {
        throw new FileIOException("Bad modbit `" + line + "'",
                                  reader, data_trace_state.filename);
      }
      int mod = ValueTuple.parseModified(line);

      // System.out.println("Mod is " + mod + " at " + data_trace_state.filename + " line " + reader.getLineNumber());
      // System.out.pringln("  for variable " + vi.name.name()
      //                   + " for program point " + ppt.name());

      // MISSING_FLOW is only found during flow algorithm
      Assert.assertTrue (mod != ValueTuple.MISSING_FLOW,
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
          throw new Daikon.TerminationMessage(
            "Modbit indicates missing value for variable "
              + vi.name.name() + " with value \"" + value_rep + "\";" + lineSep
            + "  text of value should be \"nonsensical\" or \"uninit\" at "
              + data_trace_state.filename + " line " + reader.getLineNumber());
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
                "" + FileIO.data_trace_state.reader.getLineNumber());
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
    while ((line != null)
           && !line.equals("")
           && !var_included(line)) {
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
                + ((data_trace_state.filename == null)
                  ? ""
                  : "; at "
                    + data_trace_state.filename
                    + " line "
                    + data_trace_state.reader.getLineNumber()));
            invoc = (Invocation) call_stack.pop();
          }
        } else {
          // nonce != null
          invoc = call_hashmap.get(nonce);
          if (invoc == null) {
            throw new Error(
              "Didn't find call with nonce "
                + nonce
                + " to match "
                + ppt.name()
                + " ending at "
                + data_trace_state.filename
                + " line "
                + data_trace_state.reader.getLineNumber());
          }
          invoc = call_hashmap.get(nonce);
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

  /** Add derived variables **/
  public static void add_derived_variables(PptTopLevel ppt,
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
        for (Iterator<PptTopLevel> i = invs.pptIterator(); i.hasNext();) {
          PptTopLevel ppt = i.next();
          PptTopLevel nppt = new PptTopLevel(ppt.name, ppt.var_infos);
          nppt.set_sample_number(ppt.num_samples());
          ppts.add(nppt);
          List<Invariant> inv_list = invs.get(ppt);
          for (Invariant inv : inv_list) {
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
        "It is likely that the .inv file format has changed, because a Daikon data structure has been modified, so your old .inv file is no longer readable by Daikon.  Please regenerate your .inv file." + lineSep
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
	 && Daikon.ppt_omit_regexp.matcher(ppt_name).find())
        || ((Daikon.ppt_regexp != null)
            && !Daikon.ppt_regexp.matcher(ppt_name).find())
        || ((Daikon.ppt_max_name != null)
            && ((Daikon.ppt_max_name.compareTo(ppt_name) < 0)
                && (ppt_name.indexOf(global_suffix) == -1)))) {
      return (false);
    } else {
      return (true);
    }
  }

  public static boolean var_included(String var_name) {
    assert ! var_name.equals("");
    if (((Daikon.var_omit_regexp != null)
         && Daikon.var_omit_regexp.matcher(var_name).find())
        || ((Daikon.var_regexp != null)
            && !Daikon.var_regexp.matcher(var_name).find())) {
      return (false);
    } else {
      return true;
    }
  }

  /**
   * Skips over a decl.  Essentially reads in everything up to and including
   * the next blank line.
   */
  private static void skip_decl (LineNumberReader reader) throws IOException {
    String line = reader.readLine();
    // This fails if some lines of a declaration (e.g., the comparability
    // field) are empty.
    while ((line != null) && !line.equals("")) {
      line = reader.readLine();
    }
  }

}
