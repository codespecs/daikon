package daikon;

import static daikon.PptRelation.PptRelationType;
import static daikon.PptTopLevel.PptFlags;
import static daikon.PptTopLevel.PptType;
import static daikon.VarInfo.LangFlags;
import static daikon.VarInfo.RefType;
import static daikon.VarInfo.VarFlags;
import static daikon.VarInfo.VarKind;
import static daikon.tools.nullness.NullnessUtil.castNonNullDeep;
import static java.nio.charset.StandardCharsets.UTF_8;

import daikon.config.Configuration;
import daikon.derive.ValueAndModified;
import daikon.diff.InvMap;
import daikon.inv.Invariant;
import java.io.BufferedReader;
import java.io.EOFException;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.InvalidClassException;
import java.io.LineNumberReader;
import java.io.ObjectInputStream;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.Serializable;
import java.io.StringWriter;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.URL;
import java.nio.file.Files;
import java.text.NumberFormat;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Deque;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.StringJoiner;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.zip.GZIPInputStream;
import org.checkerframework.checker.interning.qual.Interned;
import org.checkerframework.checker.interning.qual.UsesObjectEquals;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.EnsuresNonNull;
import org.checkerframework.checker.nullness.qual.EnsuresNonNullIf;
import org.checkerframework.checker.nullness.qual.KeyFor;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.nullness.qual.RequiresNonNull;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;
import org.plumelib.util.CollectionsPlume;
import org.plumelib.util.UtilPlume;

public final class FileIO {

  /** Nobody should ever instantiate a FileIO. */
  private FileIO() {
    throw new Error();
  }

  /// Constants

  static final String declaration_header = "DECLARE";

  // Program point name tags
  /** String used to append a ppt type to a ppt name. */
  public static final String ppt_tag_separator = ":::";
  /** String used to identify entry ppt names. */
  public static final String enter_suffix = "ENTER";
  /** String used to mark entry ppt names. */
  public static final String enter_tag = ppt_tag_separator + enter_suffix;
  // EXIT does not necessarily appear at the end of the program point name;
  // a number may follow it.
  /** String used to identify exit ppt names. */
  public static final String exit_suffix = "EXIT";
  /** String used to mark exit ppt names. */
  public static final String exit_tag = ppt_tag_separator + exit_suffix;
  /** To be deleted. */
  public static final String throws_suffix = "THROWS";
  /** To be deleted. */
  public static final String throws_tag = ppt_tag_separator + throws_suffix;

  public static final String object_suffix = "OBJECT";
  /** String used to mark object ppt names. */
  public static final String object_tag = ppt_tag_separator + object_suffix;
  /** String used to identify class ppt names. */
  public static final String class_static_suffix = "CLASS";
  /** String used to mark class ppt names. */
  public static final String class_static_tag = ppt_tag_separator + class_static_suffix;
  /** String used to identify global ppt names. */
  public static final String global_suffix = "GLOBAL";

  private static final String lineSep = Global.lineSep;

  /// Settings

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.

  /**
   * When true, just ignore exit ppts that don't have a matching enter ppt rather than exiting with
   * an error. Unmatched exits can occur if only a portion of a dtrace file is processed.
   */
  public static boolean dkconfig_ignore_missing_enter = false;

  /**
   * Boolean. When false, set modbits to 1 iff the printed representation has changed. When true,
   * set modbits to 1 if the printed representation has changed; leave other modbits as is.
   */
  public static boolean dkconfig_add_changed = true;

  /** Integer. Maximum number of lines to read from the dtrace file. If 0, reads the entire file. */
  public static int dkconfig_max_line_number = 0;

  /**
   * Boolean. When false, don't count the number of lines in the dtrace file before reading. This
   * will disable the percentage progress printout.
   */
  public static boolean dkconfig_count_lines = true;

  /**
   * Boolean. When true, only read the samples, but don't process them. Used to gather timing
   * information.
   */
  public static boolean dkconfig_read_samples_only = false;

  /**
   * Boolean. When true, don't print a warning about unmatched procedure entries, which are ignored
   * by Daikon (unless the {@code --nohierarchy} command-line argument is provided).
   */
  public static boolean dkconfig_unmatched_procedure_entries_quiet = false;

  /** Boolean. If true, prints the unmatched procedure entries verbosely. */
  public static boolean dkconfig_verbose_unmatched_procedure_entries = false;

  /**
   * Boolean. When true, suppress exceptions related to file reading. This permits Daikon to
   * continue even if there is a malformed trace file. Use this with care: in general, it is better
   * to fix the problem that caused a bad trace file, rather than to suppress the exception.
   */
  public static boolean dkconfig_continue_after_file_exception = false;

  /**
   * Long integer. If non-zero, this value will be used as the number of lines in (each) dtrace file
   * input for the purposes of the progress display, and the counting of the lines in the file will
   * be suppressed.
   */
  public static long dkconfig_dtrace_line_count = 0;

  /** True if declaration records are in the new format -- that is, decl-version 2.0. */
  // Set by read_decl_version; by read_data_trace_record if the file is non-empty;
  // by read_serialized_pptmap; and by InvMap.readObject.
  public static @MonotonicNonNull Boolean new_decl_format = null;

  /**
   * Do not use this routine unless you know what you are doing. This routine breaks the
   * representation invariant that new_decl_format, once set, is never reset to null. This routine
   * should be used only if you can guarantee that new_decl_format will be once again set to a
   * non-null value before any code runs that depends on the fact that new_decl_format is non-null.
   */
  @SuppressWarnings("nullness") // reinitialization
  public static void resetNewDeclFormat() {
    FileIO.new_decl_format = null;
  }

  /**
   * If true, modified all ppt names to remove duplicate routine names within the ppt name. This is
   * used when a stack trace (of active methods) is used as the ppt name. The routine names must be
   * separated by vertical bars (|).
   */
  public static boolean dkconfig_rm_stack_dups = false;

  /// Variables

  // This hashmap maps every program point to an array, which contains the
  // old values of all variables in scope the last time the program point
  // was executed. This enables us to determine whether the values have been
  // modified since this program point was last executed.
  static HashMap<PptTopLevel, String[]> ppt_to_value_reps = new HashMap<>();

  // For debugging purposes: printing out a modified trace file with
  // changed modbits.
  private static boolean to_write_nonce = false;
  private static final String NONCE_HEADER = "this_invocation_nonce";
  private static String nonce_value = "no nonce (yet)";

  // (This implementation as a public static variable is a bit unclean.)
  // Number of ignored declarations.
  public static int omitted_declarations = 0;

  // Logging Categories

  /**
   * If true, then print the variable name each time the variable's value is first
   * missing/nonsensical.
   */
  public static boolean debug_missing = false;

  /** Debug tracer for reading. */
  public static final Logger debugRead = Logger.getLogger("daikon.FileIO.read");
  /** Debug tracer for printing. */
  public static final Logger debugPrint = Logger.getLogger("daikon.FileIO.printDtrace");

  /** Debug tracer for printing variable values. */
  public static final Logger debugVars = Logger.getLogger("daikon.FileIO.vars");

  // public static final SimpleLog debug_decl = new SimpleLog(false);

  /** Parents in the ppt/variable hierarchy for a particular program point. */
  public static final class ParentRelation implements java.io.Serializable {
    static final long serialVersionUID = 20060622L;
    public PptRelationType rel_type;
    public @Interned String parent_ppt_name;
    public int id;

    public ParentRelation(PptRelationType rel_type, @Interned String parent_ppt_name, int id) {
      this.rel_type = rel_type;
      this.parent_ppt_name = parent_ppt_name;
      this.id = id;
    }

    @SideEffectFree
    @Override
    public String toString(@GuardSatisfied ParentRelation this) {
      return parent_ppt_name + "[" + id + "] " + rel_type;
    };

    private void readObject(ObjectInputStream in) throws IOException, ClassNotFoundException {
      in.defaultReadObject();
      if (parent_ppt_name != null) {
        parent_ppt_name = parent_ppt_name.intern();
      }
    }
  }

  // Utilities
  @EnsuresNonNullIf(result = true, expression = "#1")
  @Pure
  public static final boolean isComment(@Nullable String s) {
    return s != null && (s.startsWith("//") || s.startsWith("#"));
  }

  // Nullness-checking of read_data_trace_record(ParseState) works even
  // without these two lines, since StringJoiner accepts null values.
  @SuppressWarnings(
      "nullness:contracts.conditional.postcondition.not.satisfied") // readLine() assertion is
  // ensured by call to reset()
  @EnsuresNonNullIf(result = true, expression = "#1.readLine()")
  public static final boolean nextLineIsComment(BufferedReader reader) {
    boolean result = false;
    try {
      reader.mark(10000);
      String nextline = reader.readLine();
      result = isComment(nextline);
    } catch (IOException e) {
      result = false;
    }
    try {
      reader.reset();
    } catch (IOException e) {
      throw new Error(e);
    }
    return result;
  }

  ///////////////////////////////////////////////////////////////////////////
  /// Declaration files
  ///

  /**
   * @param files files to be read (java.io.File)
   * @return a new PptMap containing declarations read from the files listed in the argument;
   *     connection information (controlling variables and entry ppts) is set correctly upon return
   */
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

  /** Read one decls file; add it to all_ppts. */
  public static void read_declaration_file(File filename, PptMap all_ppts) throws IOException {
    if (Daikon.using_DaikonSimple) {
      Processor processor = new DaikonSimple.SimpleProcessor();
      read_data_trace_file(filename.toString(), all_ppts, processor, true, false);
    } else {
      Processor processor = new Processor();
      read_data_trace_file(filename.toString(), all_ppts, processor, true, true);
    }
  }

  // Read a declaration in the Version 2 format.  For Version 1, see
  // read_declaration.
  /**
   * Reads one ppt declaration. The next line should be the ppt record. After completion, the file
   * pointer will be pointing at the next record (ie, the blank line at the end of the ppt
   * declaration will have been read in). Returns null if the ppt is excluded/omitted from this
   * execution of Daikon.
   */
  private static @Nullable PptTopLevel read_ppt_decl(ParseState state, String top_line)
      throws IOException {

    // process the ppt record
    String line = top_line;
    Scanner scanner = new Scanner(line);
    @Interned String record_name = need(state, scanner, "'ppt'");
    if (record_name != "ppt") { // interned
      decl_error(state, "found '%s' where 'ppt' expected", record_name);
    }
    String ppt_name = need(state, scanner, "ppt name");
    ppt_name = user_mod_ppt_name(ppt_name);

    /** Information that will populate the new program point. */
    Map<String, VarDefinition> varmap = new LinkedHashMap<>();
    /** The VarDefinition we are in the middle of reading, or null if we are not. */
    VarDefinition vardef = null;
    List<ParentRelation> ppt_parents = new ArrayList<>();
    EnumSet<PptFlags> ppt_flags = EnumSet.noneOf(PptFlags.class);
    PptType ppt_type = PptType.POINT;

    try {
      // Read the records that define this program point
      while ((line = state.reader.readLine()) != null) {
        // debug_decl.log("read line %s%n", line);
        line = line.trim();
        if (line.length() == 0) {
          break;
        }

        scanner = new Scanner(line);
        @Interned String record = scanner.next().intern();
        if (vardef == null) {
          if (record == "parent") { // interned
            ppt_parents.add(parse_ppt_parent(state, scanner));
          } else if (record == "flags") { // interned
            parse_ppt_flags(state, scanner, ppt_flags);
          } else if (record == "variable") { // interned
            vardef = new VarDefinition(state, scanner);
            // There is no need to check "varmap.containsKey(vardef.name)"
            // because this is the first variable.
            assert varmap.isEmpty();
            if (var_included(vardef.name)) varmap.put(vardef.name, vardef);
          } else if (record == "ppt-type") { // interned
            ppt_type = parse_ppt_type(state, scanner);
          } else {
            decl_error(state, "record '%s' found where %s expected", record, "'parent', 'flags'");
          }
        } else { // there must be a current variable
          if (record == "var-kind") { // interned
            vardef.parse_var_kind(scanner);
          } else if (record == "enclosing-var") { // interned
            vardef.parse_enclosing_var_name(scanner);
          } else if (record == "reference-type") { // interned
            vardef.parse_reference_type(scanner);
          } else if (record == "array") { // interned
            vardef.parse_array(scanner);
          } else if (record == "function-args") { // interned
            vardef.parse_function_args(scanner);
          } else if (record == "rep-type") { // interned
            vardef.parse_rep_type(scanner);
          } else if (record == "dec-type") { // interned
            vardef.parse_dec_type(scanner);
          } else if (record == "flags") { // interned
            vardef.parse_flags(scanner);
          } else if (record == "lang-flags") { // interned
            vardef.parse_lang_flags(scanner);
          } else if (record == "parent") { // interned
            vardef.parse_parent(scanner, ppt_parents);
          } else if (record == "comparability") { // interned
            vardef.parse_comparability(scanner);
          } else if (record == "constant") { // interned
            vardef.parse_constant(scanner);
          } else if (record == "variable") { // interned
            try {
              vardef.checkRep(); // make sure the previous variable is ok
            } catch (AssertionError e) {
              decl_error(state, e);
            }
            vardef = new VarDefinition(state, scanner);
            if (varmap.containsKey(vardef.name)) {
              decl_error(state, "var %s declared twice", vardef.name);
            }
            if (var_included(vardef.name)) varmap.put(vardef.name, vardef);
          } else if (record == "min-value") { // interned
            vardef.parse_min_value(scanner);
          } else if (record == "max-value") { // interned
            vardef.parse_max_value(scanner);
          } else if (record == "min-length") { // interned
            vardef.parse_min_length(scanner);
          } else if (record == "max-length") { // interned
            vardef.parse_max_length(scanner);
          } else if (record == "valid-values") { // interned
            vardef.parse_valid_values(scanner);
          } else {
            decl_error(state, "Unexpected variable item '%s' found", record);
          }
        }
      }
    } catch (Daikon.ParseError pe) {
      decl_error(state, "%s", pe.getMessage());
      throw new Error(); // this can't happen
    }
    if (vardef != null) {
      try {
        vardef.checkRep();
      } catch (AssertionError e) {
        decl_error(state, e);
      }
    }

    // If we are excluding this ppt, just read the data and throw it away
    if (!ppt_included(ppt_name)) {
      omitted_declarations++;
      return null;
    }

    // Build the var infos from the var definitions.
    List<VarInfo> vi_list = new ArrayList<>(varmap.size());
    for (VarDefinition vd : varmap.values()) {
      @SuppressWarnings("interning") // about to be used in a new program point
      @Interned VarInfo vi = new VarInfo(vd);
      vi_list.add(vi);
    }
    VarInfo[] vi_array = vi_list.toArray(new VarInfo[vi_list.size()]);

    // Check to see if the program point is new
    if (state.all_ppts.containsName(ppt_name)) {
      @NonNull PptTopLevel existing_ppt = state.all_ppts.get(ppt_name);
      assert existing_ppt != null : "state.all_ppts.containsName(" + ppt_name + ")";
      if (state.ppts_may_be_new) {
        check_decl_match(state, existing_ppt, vi_array);
      } else { // ppts are already in the map
        if (VarInfo.assertionsEnabled()) {
          for (VarInfo vi : vi_array) {
            vi.checkRep();
          }
        }
        return existing_ppt;
      }
    }

    // Build the program point
    PptTopLevel newppt = new PptTopLevel(ppt_name, ppt_type, ppt_parents, ppt_flags, vi_array);

    return newppt;
  }

  /** Parses a ppt parent hierarchy record and returns it. */
  private static ParentRelation parse_ppt_parent(ParseState state, Scanner scanner) {

    PptRelationType rel_type =
        parse_enum_val(state, scanner, PptRelationType.class, "relation type");
    String parent_ppt_name = need(state, scanner, "ppt name");
    int id = Integer.parseInt(need(state, scanner, "relation id"));
    ParentRelation pr = new ParentRelation(rel_type, parent_ppt_name, id);

    need_eol(state, scanner);
    return pr;
  }

  /** Parses a program point flag record. Adds any specified flags to to flags. */
  private static void parse_ppt_flags(ParseState state, Scanner scanner, EnumSet<PptFlags> flags) {

    flags.add(parse_enum_val(state, scanner, PptFlags.class, "ppt flags"));
    while (scanner.hasNext())
      flags.add(parse_enum_val(state, scanner, PptFlags.class, "ppt flags"));
  }

  /** Parses a ppt-type record and returns the type. */
  private static PptType parse_ppt_type(ParseState state, Scanner scanner) {

    PptType ppt_type = parse_enum_val(state, scanner, PptType.class, "ppt type");
    need_eol(state, scanner);
    return ppt_type;
  }

  // Read a declaration in the Version 1 format.  For version 2, see
  // read_ppt_decl.
  // The "DECLARE" line has already been read.
  private static @Nullable PptTopLevel read_declaration(ParseState state) throws IOException {

    // We have just read the "DECLARE" line.
    String ppt_name = state.reader.readLine();
    if (ppt_name == null) {
      throw new Daikon.UserError(
          "File ends with \"DECLARE\" with no following program point name", state);
    }
    ppt_name = user_mod_ppt_name(ppt_name);
    ppt_name = ppt_name.intern();
    VarInfo[] vi_array = read_VarInfos(state, ppt_name);

    // System.out.printf("Ppt %s with %d variables%n", ppt_name,
    //                   vi_array.length);

    // This program point name has already been encountered.
    if (state.all_ppts.containsName(ppt_name)) {
      @NonNull PptTopLevel existing_ppt = state.all_ppts.get(ppt_name);
      assert existing_ppt != null : "state.all_ppts.containsName(" + ppt_name + ")";
      if (state.ppts_may_be_new) {
        check_decl_match(state, existing_ppt, vi_array);
      } else { // ppts are already in the map
        return existing_ppt;
      }
    }

    // If we are excluding this ppt, just throw it away
    if (!ppt_included(ppt_name)) {
      omitted_declarations++;
      return null;
    }

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

    // TODO: add a new config variable to turn this accessibility flag processing on?
    PptTopLevel newppt = new PptTopLevel(ppt_name, vi_array);
    // newppt.ppt_name.setVisibility("package-protected");
    return newppt;
    // return new PptTopLevel(ppt_name, vi_array);
  }

  private static VarInfo[] read_VarInfos(ParseState state, String ppt_name) throws IOException {

    // The var_infos that will populate the new program point
    List<VarInfo> var_infos = new ArrayList<>();

    // Each iteration reads a variable name, type, and comparability.
    // Possibly abstract this out into a separate function??
    VarInfo vi;
    while ((vi = read_VarInfo(state, ppt_name)) != null) {
      for (VarInfo vi2 : var_infos) {
        if (vi.name() == vi2.name()) {
          throw new Daikon.UserError("Duplicate variable name " + vi.name(), state);
        }
      }
      // Can't do this test in read_VarInfo, it seems, because of the test
      // against null above.
      if (!var_included(vi.name())) {
        continue;
      }
      var_infos.add(vi);
    }

    VarInfo[] result = var_infos.toArray(new VarInfo[var_infos.size()]);
    return result;
  }

  // So that warning message below is only printed once
  private static boolean seen_string_rep_type = false;

  /**
   * Read a variable name, type, and comparability; construct a VarInfo. Return null after reading
   * the last variable in this program point declaration.
   */
  private static @Nullable VarInfo read_VarInfo(ParseState state, String ppt_name)
      throws IOException {
    LineNumberReader file = state.reader;
    int varcomp_format = state.varcomp_format;
    String filename = state.filename;

    String line = file.readLine();
    if ((line == null) || line.equals("")) {
      return null;
    }
    String varname = line;
    String proglang_type_string_and_aux = file.readLine();
    String file_rep_type_string = file.readLine();
    String comparability_string = file.readLine();
    if ( // (varname == null) || // already returned null if varname==null
    (proglang_type_string_and_aux == null)
        || (file_rep_type_string == null)
        || (comparability_string == null))
      throw new Daikon.UserError(
          "End of file "
              + filename
              + " while reading variable "
              + varname
              + " in declaration of program point "
              + ppt_name);
    int equals_index = file_rep_type_string.indexOf(" = ");
    String static_constant_value_string = null;
    @Interned Object static_constant_value = null;
    boolean is_static_constant = false;
    if (equals_index != -1) {
      is_static_constant = true;
      static_constant_value_string = file_rep_type_string.substring(equals_index + 3);
      file_rep_type_string = file_rep_type_string.substring(0, equals_index);
    }
    // XXX temporary, for compatibility with older .dtrace files.  12/20/2001
    if ("String".equals(file_rep_type_string)) {
      file_rep_type_string = "java.lang.String";
      if (!seen_string_rep_type) {
        seen_string_rep_type = true;
        System.err.println(
            "Warning: Malformed trace file.  Representation type 'String' should be "
                + "'java.lang.String' instead on line "
                + (file.getLineNumber() - 1)
                + " of "
                + filename);
      }
    }
    // This is for people who were confused by the above temporary
    // workaround when it didn't have a warning. But this has never
    // worked, so it's fatal.
    else if ("String[]".equals(file_rep_type_string)) {
      throw new Daikon.UserError(
          "Representation type 'String[]' should be "
              + "'java.lang.String[]' instead for variable "
              + varname,
          file,
          filename);
    }
    /// XXX

    int hash_position = proglang_type_string_and_aux.indexOf('#');
    String aux_string = "";
    if (hash_position == -1) {
      hash_position = proglang_type_string_and_aux.length();
    } else {
      aux_string =
          proglang_type_string_and_aux.substring(
              hash_position + 1, proglang_type_string_and_aux.length());
    }

    String proglang_type_string = proglang_type_string_and_aux.substring(0, hash_position).trim();

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
      throw new Daikon.UserError(e, file, filename);
    }

    if (static_constant_value_string != null) {
      static_constant_value = rep_type.parse_value(static_constant_value_string, file, filename);
      // Why can't the value be null?
      assert static_constant_value != null;
    }
    VarComparability comparability = null;
    try {
      comparability = VarComparability.parse(varcomp_format, comparability_string, prog_type);
    } catch (Exception e) {
      throw new Daikon.UserError(
          String.format(
              "Error parsing comparability (%s) at line %d in file %s",
              e, file.getLineNumber(), filename));
    }
    if (!VarInfo.legalFileRepType(file_rep_type)) {
      throw new Daikon.UserError(
          "Unsupported representation type "
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
      throw new Daikon.UserError(
          "Unsupported (converted) representation type "
              + file_rep_type.format()
              + " for variable "
              + varname,
          file,
          filename);
    }
    // COMPARABILITY TEST
    if (!(comparability.alwaysComparable()
        || ((VarComparabilityImplicit) comparability).dimensions == file_rep_type.dimensions())) {
      System.err.println();
      throw new Daikon.UserError(
          "Rep type "
              + file_rep_type.format()
              + " has "
              + file_rep_type.dimensions()
              + " dimensions,"
              + " but comparability "
              + comparability
              + " has "
              + ((VarComparabilityImplicit) comparability).dimensions
              + " dimensions,"
              + " for variable "
              + varname,
          file,
          filename);
    }

    @SuppressWarnings("interning")
    @Interned VarInfo result =
        new VarInfo(
            varname,
            prog_type,
            file_rep_type,
            comparability,
            is_static_constant,
            static_constant_value,
            aux);
    return result;
  }

  @RequiresNonNull("FileIO.new_decl_format")
  private static int read_var_comparability(ParseState state, String line) throws IOException {

    // System.out.printf("read_var_comparability, line = '%s' %b%n", line,
    //                   new_decl_format);
    String comp_str;
    if (new_decl_format) {
      Scanner scanner = new Scanner(line);
      scanner.next();
      comp_str = need(state, scanner, "comparability");
      need_eol(state, scanner);
    } else { // old format
      comp_str = state.reader.readLine();
      if (comp_str == null) {
        throw new Daikon.UserError("Found end of file, expected comparability", state);
      }
    }

    if (comp_str.equals("none")) {
      return VarComparability.NONE;
    } else if (comp_str.equals("implicit")) {
      return VarComparability.IMPLICIT;
    } else {
      throw new Daikon.UserError("Unrecognized VarComparability '" + comp_str + "'", state);
    }
  }

  private static @Interned String read_input_language(ParseState state, String line)
      throws IOException {

    Scanner scanner = new Scanner(line);
    scanner.next();
    @Interned String input_lang = need(state, scanner, "input language");
    need_eol(state, scanner);
    return input_lang;
  }

  @EnsuresNonNull("FileIO.new_decl_format")
  private static void read_decl_version(ParseState state, String line) throws IOException {
    Scanner scanner = new Scanner(line);
    scanner.next();
    @Interned String version = need(state, scanner, "declaration version number");
    need_eol(state, scanner);
    boolean new_df;
    if (version == "2.0") // interned
    new_df = true;
    else if (version == "1.0") // interned
    new_df = false;
    else {
      decl_error(state, "'%s' found where 1.0 or 2.0 expected", version);
      throw new Error("Can't get here"); // help out definite assignment analysis
    }

    // Make sure that if a format was specified previously, it is the same
    if ((new_decl_format != null) && (new_df != new_decl_format.booleanValue())) {
      decl_error(state, "decl format '%s' does not match previous setting", version);
    }

    // System.out.println("setting new_decl_format = " + new_df);
    new_decl_format = Boolean.valueOf(new_df);
  }

  // Each line following is the name (in JVM form) of a class that
  // implements java.util.List.  All those lines (including interspersed
  // comments) are returned.
  private static String read_list_implementors(LineNumberReader reader) throws IOException {
    StringJoiner result = new StringJoiner(lineSep);
    for (; ; ) {
      String line = reader.readLine();
      if (line == null || line.equals("")) {
        break;
      }
      result.add(line);
      if (isComment(line)) {
        continue;
      }
      ProglangType.list_implementors.add(line.intern());
    }
    return result.toString();
  }

  ///////////////////////////////////////////////////////////////////////////
  /// invocation tracking for dtrace files entry/exit grouping
  ///

  static final class Invocation implements Comparable<Invocation> {
    PptTopLevel ppt; // used in printing and in suppressing duplicates
    // Rather than a valuetuple, place its elements here.
    @Nullable Object[] vals;
    int[] mods;

    static Object canonical_hashcode = new Object();

    Invocation(PptTopLevel ppt, @Nullable Object[] vals, int[] mods) {
      this.ppt = ppt;
      this.vals = vals;
      this.mods = mods;
    }

    // Print the Invocation on two lines, indented by two spaces
    // The receiver Invocation may be canonicalized or not.
    String format(@GuardSatisfied Invocation this) {
      return format(true);
    }

    // Print the Invocation on one or two lines, indented by two spaces.
    // The receiver Invocation may be canonicalized or not.
    String format(@GuardSatisfied Invocation this, boolean show_values) {
      if (!show_values) {
        return "  " + ppt.ppt_name.getNameWithoutPoint();
      }

      StringWriter sw = new StringWriter();
      PrintWriter pw = new PrintWriter(sw);

      pw.println("  " + ppt.ppt_name.getNameWithoutPoint());
      pw.print("    ");

      // [adonovan] is this sound? Let me know if not (sorry).
      // assert ppt.var_infos.length == vals.length;

      for (int j = 0; j < vals.length; j++) {
        if (j != 0) pw.print(", ");

        pw.print(ppt.var_infos[j].name() + "=");

        Object val = vals[j];
        if (canonical_hashcode.equals(
            val)) // succeeds only for canonicalized Invocations.  Can be an == test, but there is
          // little point.  val can be null, so it cannot be the receiver.
          pw.print("<hashcode>");
        else if (val instanceof int[]) pw.print(Arrays.toString((int[]) val));
        else if (val instanceof String) pw.print(UtilPlume.escapeNonASCII((String) val));
        else {
          pw.print(val);
        }
      }
      pw.println();

      return sw.toString();
    }

    /** Change uses of hashcodes to canonical_hashcode. */
    public @Interned Invocation canonicalize() {
      @Nullable Object[] new_vals = new @Nullable Object[vals.length];
      System.arraycopy(vals, 0, new_vals, 0, vals.length);
      VarInfo[] vis = ppt.var_infos;
      // Warning: abstraction violation!
      for (VarInfo vi : vis) {
        if ((vi.value_index != -1) && (vi.file_rep_type == ProglangType.HASHCODE)) {
          new_vals[vi.value_index] = canonical_hashcode;
        }
      }
      @SuppressWarnings("interning:cast.unsafe.constructor.invocation")
      @Interned Invocation result = new @Interned Invocation(ppt, new_vals, mods);
      return result;
    }

    // Return true if the invocations print the same
    @EnsuresNonNullIf(result = true, expression = "#1")
    @Pure
    @Override
    public boolean equals(@GuardSatisfied Invocation this, @GuardSatisfied @Nullable Object other) {
      if (other instanceof FileIO.Invocation) {
        return this.format().equals(((FileIO.Invocation) other).format());
      } else {
        return false;
      }
    }

    @Pure
    @Override
    public int compareTo(@GuardSatisfied Invocation this, Invocation other) {
      return ppt.name().compareTo(other.ppt.name());
    }

    @Pure
    @Override
    public int hashCode(@GuardSatisfied Invocation this) {
      return this.format().hashCode();
    }
  }

  // I could save some Object overhead by using two parallel stacks
  // instead of Invocation objects; but that's not worth it.

  // Map key is a (global, not per-procedure) nonce.
  // The nonce indicates which returns are associated with which entries.
  static HashMap<Integer, Invocation> call_hashmap = new HashMap<>();
  // call_stack is for procedures without nonces.
  static Deque<Invocation> call_stack = new ArrayDeque<Invocation>();

  /**
   * Reads data from {@code .dtrace} files. For each record in the files, calls the appropriate
   * callback in the processor.
   *
   * @see #read_data_trace_files(Collection,PptMap,Processor,boolean)
   * @see #read_data_trace_file(String,PptMap,Processor,boolean,boolean)
   */
  public static void read_data_trace_files(Collection<String> files, PptMap all_ppts)
      throws IOException {

    Processor processor = new Processor();
    read_data_trace_files(files, all_ppts, processor, true);
  }

  /**
   * Reads data from {@code .dtrace} files. Calls {@link
   * #read_data_trace_file(String,PptMap,Processor,boolean,boolean)} for each element of filenames.
   *
   * @param ppts_may_be_new true if declarations of ppts read from the data trace file are new (and
   *     thus are not in all_ppts). false if the ppts may already be there.
   * @see #read_data_trace_file(String,PptMap,Processor,boolean,boolean)
   */
  public static void read_data_trace_files(
      Collection<String> files, PptMap all_ppts, Processor processor, boolean ppts_may_be_new)
      throws IOException {

    for (String filename : files) {
      // System.out.printf("processing filename %s%n", filename);
      try {
        read_data_trace_file(filename, all_ppts, processor, false, ppts_may_be_new);
      } catch (Daikon.NormalTermination e) {
        throw e;
      } catch (Throwable e) {
        if (dkconfig_continue_after_file_exception) {
          System.out.println();
          System.out.println(
              "WARNING: Error while processing trace file; remaining records ignored.");
          System.out.print("Ignored backtrace:");
          e.printStackTrace(System.out);
          System.out.println();
        } else {
          throw e;
        }
      }
    }
    if (Daikon.server_dir != null) {
      // Yoav: server mode
      while (true) {
        @SuppressWarnings(
            "nullness") // server_dir is a directory; this was checked when the variable was set
        String @NonNull [] dir_files = Daikon.server_dir.list();
        Arrays.sort(dir_files);
        boolean hasEnd = false;
        for (String f : dir_files) {
          if (f.endsWith(".end")) {
            hasEnd = true;
          }
          if (f.endsWith(".end") || f.endsWith(".start")) {
            continue;
          }
          if (files.contains(f)) {
            continue;
          }
          files.add(f);
          System.out.println("Reading " + f);
          read_data_trace_file(
              new File(Daikon.server_dir, f).toString(),
              all_ppts,
              processor,
              false,
              ppts_may_be_new);
        }
        if (hasEnd) {
          break;
        }
        try {
          Thread.sleep(1000);
        } catch (java.lang.InterruptedException e) {
        }
      }
    }

    process_unmatched_procedure_entries();

    warn_if_hierarchy_mismatch(all_ppts);
  }

  // Determine if dataflow hierarchy should have been used, and print
  // warning if this does not match Daikon.use_dataflow_hierarchy.
  // Dataflow hierarchy should be used only when all program points
  // correspond to points normally found in traces from a
  // programming languages.
  private static void warn_if_hierarchy_mismatch(PptMap all_ppts) {

    boolean some_program_points = false;
    boolean all_program_points = true;

    // Go through each top level ppt, and make all_program_points
    // false if at least one of them is not a program point normally
    // found in traces from programming languages.
    for (PptTopLevel ppt_top_level : all_ppts.ppt_all_iterable()) {
      boolean is_program_point =
          (ppt_top_level.ppt_name.isExitPoint()
              || ppt_top_level.ppt_name.isEnterPoint()
              || ppt_top_level.ppt_name.isThrowsPoint()
              || ppt_top_level.ppt_name.isObjectInstanceSynthetic()
              || ppt_top_level.ppt_name.isClassStaticSynthetic()
              || ppt_top_level.ppt_name.isGlobalPoint());

      all_program_points = all_program_points && is_program_point;
      some_program_points = some_program_points || is_program_point;
    }

    // If all program points correspond to a programming language,
    // but the dataflow hierarchy has been turned off, then
    // suggest not using the --nohierarchy flag.
    //    if (all_program_points && (!Daikon.use_dataflow_hierarchy)) {
    //      System.out.println("Warning: data trace appears to be over" +
    //                         " a program execution, but dataflow" +
    //                         " hierarchy has been turned off," +
    //                         " consider running Daikon without the" +
    //                         " --nohierarchy flag");
    //    }

    // if some of the program points do not correspond to a
    // points from a programming language, and the dataflow
    // hierarchy is being used, suggest using the --nohierarchy flag.
    if (Daikon.use_dataflow_hierarchy && !all_program_points && some_program_points) {
      System.out.println(
          "Warning: Daikon is using a dataflow"
              + " hierarchy analysis on a data trace"
              + " that does not appear to be over a"
              + " program execution, consider running"
              + " Daikon with the --nohierarchy flag.");
    }
  }

  private static InputStream connectToChicory() {

    ServerSocket daikonServer;
    try {
      daikonServer = new ServerSocket(0); // bind to any free port

      // tell Chicory what port we have!
      System.out.println("DaikonChicoryOnlinePort=" + daikonServer.getLocalPort());

      daikonServer.setReceiveBufferSize(64000);

    } catch (IOException e) {
      throw new RuntimeException("Unable to create server", e);
    }

    Socket chicSocket;
    try {
      daikonServer.setSoTimeout(5000);

      // System.out.println("waiting for chicory connection on port " +
      // daikonServer.getLocalPort());
      chicSocket = daikonServer.accept();
    } catch (IOException e) {
      throw new RuntimeException("Unable to connect to Chicory", e);
    }

    try {
      return chicSocket.getInputStream();
    } catch (IOException e) {
      throw new RuntimeException("Unable to get Chicory's input stream", e);
    }
  }

  /**
   * A Processor is used to read a dtrace file. A Processor defines callbacks for each record type
   * in a dtrace file. As each record is read from a dtrace file, the corresponding callback is
   * called.
   *
   * <p>to use a Processor, pass it to {@link #read_data_trace_files(Collection, PptMap,
   * FileIO.Processor, boolean)}. {@code read_data_trace_files} will call {@link
   * #process_sample(PptMap,PptTopLevel,ValueTuple,Integer)} once for every sample in the dtrace
   * file, and will call other callbacks for other records in the dtrace file.
   *
   * <p>For an example of how to create and use a Processor, see {@link daikon.tools.ReadTrace}.
   *
   * @see #read_data_trace_files(Collection, PptMap, FileIO.Processor, boolean)
   * @see daikon.tools.ReadTrace
   */
  public static class Processor {
    /**
     * Process a data sample record. This default implementation calls {@link
     * FileIO#process_sample(PptMap, PptTopLevel, ValueTuple, Integer)}.
     *
     * @see FileIO#process_sample(PptMap, PptTopLevel, ValueTuple, Integer)
     */
    @RequiresNonNull("FileIO.data_trace_state")
    public void process_sample(
        PptMap all_ppts, PptTopLevel ppt, ValueTuple vt, @Nullable Integer nonce) {
      FileIO.process_sample(all_ppts, ppt, vt, nonce);
    }

    /** Process a program point declaration record. */
    public void process_decl(PptMap all_ppts, PptTopLevel ppt) {}

    /** Process a ppt decl format record. */
    public void process_decl_version(String format) {}

    /** Process a VarComparability declaration. */
    public void process_comparability(String comparability) {}

    /** Process a ListImplementors declaration. */
    public void process_list_implementors(String implementors) {}

    /** Process an input-language declaration. */
    public void process_input_language(String language) {}

    /** Process a null record (haven't read anything yet). */
    public void process_null() {}

    /** Process a comment. */
    public void process_comment(String comment) {}

    /** Process indication of end of file. */
    public void process_eof() {}

    /** Process indication of exceeding file size limit. */
    public void process_truncated() {}

    /** Process continuable error. */
    public void process_error() {}
  }

  /**
   * Total number of samples passed to process_sample(). Not part of ParseState because it's global
   * over all files processed by Daikon.
   */
  public static int samples_processed = 0;

  /** The type of the record that was most recently read. */
  public enum RecordType {
    SAMPLE, // got a sample

    DECL, // got a ppt decl
    DECL_VERSION, // got an indication of the ppt decl format
    COMPARABILITY, // got a VarComparability declaration
    LIST_IMPLEMENTORS, // got a ListImplementors declaration
    INPUT_LANGUAGE, // got an input-language declaration

    NULL, // haven't read anything yet
    COMMENT, // got a comment
    EOF, // reached end of file
    TRUNCATED, // dkconfig_max_line_number reached (without error)
    ERROR, // continuable error; fatal errors thrown as exceptions
  };

  /**
   * ParseState indicates:
   *
   * <ol>
   *   <li>Some global information about the state of the parser while reading a decl or dtrace
   *       file.
   *   <li>The record that was most recently read; thus, ParseState is essentially a discriminated
   *       union whose tag is a RecordType. (TODO: These are poor names that should probably be
   *       swapped!) ParseState is what is returned (actually, side-effected) by method
   *       read_data_trace_record when it reads a record.
   * </ol>
   */
  @UsesObjectEquals
  public static class ParseState {

    //
    // This is the global information about the state of the parser.
    //

    /** Name of input file. */
    public String filename;

    /** True if the current file is a declaration file. */
    public boolean is_decl_file;

    /**
     * True if ppts may be new. If a duplicate is seen, it must match a previous point exactly. If
     * false, the previous ppt is used without checking for a match.
     */
    public boolean ppts_may_be_new;

    /** All of the ppts seen so far. */
    public PptMap all_ppts;

    /** Input stream. */
    public LineNumberReader reader;

    /** Total number of lines in the input file. */
    public long total_lines;

    /** Comparability format, either VarComparability.IMPLICIT or VarComparability.NONE. */
    public int varcomp_format;

    //
    // This is the discriminated-union part of the ParseState.
    // (Presumably this design was chosen for efficiency, to avoid creating
    // & garbage-collecting these values many times.)
    //

    public RecordType rtype;

    /**
     * Current ppt. Used when status=DECL or SAMPLE. Can be null if this declaration was skipped
     * because of --ppt-select-pattern or --ppt-omit-pattern.
     */
    public @Nullable PptTopLevel ppt;

    /** The current nonce. Used when status=SAMPLE. */
    public @Nullable Integer nonce;

    /** The current set of values. Used when status=SAMPLE. */
    public @Nullable ValueTuple vt;

    /** Miscellaneous text in the parsed item. */
    public @Nullable Object payload; // used when status=COMMENT

    /** Start parsing the given file. */
    public ParseState(
        String raw_filename, boolean decl_file_p, boolean ppts_may_be_new, PptMap ppts)
        throws IOException {
      // Pretty up raw_filename for use in messages
      if (raw_filename.equals("-")) {
        filename = "standard input";
      } else if (raw_filename.equals("+")) {
        filename = "chicory socket";
      } else {
        // Remove directory parts, to make it shorter
        filename = raw_filename;
      }

      is_decl_file = decl_file_p;
      this.ppts_may_be_new = ppts_may_be_new;
      all_ppts = ppts;

      boolean is_url = raw_filename.startsWith("file:") || raw_filename.startsWith("jar:");

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
      } else if (is_url) {
        count_lines = false;
      } else if (Daikon.dkconfig_progress_delay == -1) {
        count_lines = false;
      } else if (new File(raw_filename).length() == 0) {
        // Either it's actually empty, or it's something like a pipe.
        count_lines = false;
      }

      if (count_lines) {
        Daikon.progress = "Checking size of " + filename;
        total_lines = UtilPlume.countLines(raw_filename);
      } else {
        // System.out.printf("no count %b %d %s %d %d%n", is_decl_file,
        //                    dkconfig_dtrace_line_count, filename,
        //  Daikon.dkconfig_progress_delay, (new File(raw_filename)).length());
      }

      // Open the reader stream
      if (raw_filename.equals("-")) {
        // "-" means read from the standard input stream
        Reader file_reader = new InputStreamReader(System.in, "ISO-8859-1");
        reader = new LineNumberReader(file_reader);
      } else if (raw_filename.equals("+")) { // socket comm with Chicory
        InputStream chicoryInput = connectToChicory();
        InputStreamReader chicReader = new InputStreamReader(chicoryInput, UTF_8);
        reader = new LineNumberReader(chicReader);
      } else if (is_url) {
        URL url = new URL(raw_filename);
        InputStream stream = url.openStream();
        if (raw_filename.endsWith(".gz")) {
          GZIPInputStream gzip_stream = new GZIPInputStream(stream);
          reader = new LineNumberReader(new InputStreamReader(gzip_stream, UTF_8));
        } else {
          reader = new LineNumberReader(new InputStreamReader(stream, UTF_8));
        }
      } else {
        reader = UtilPlume.lineNumberFileReader(raw_filename);
      }

      varcomp_format = VarComparability.IMPLICIT;
      rtype = RecordType.NULL;
      ppt = null;
    }

    /** Returns the current line number in the input file, or -1 if not available. */
    public int get_linenum() {
      return reader.getLineNumber();
    }

    private static NumberFormat pctFmt;

    static {
      pctFmt = NumberFormat.getPercentInstance();
      pctFmt.setMinimumFractionDigits(2);
      pctFmt.setMaximumFractionDigits(2);
    }

    public String reading_message() {
      String line;
      if (reader == null) {
        line = "?";
      } else {
        long lineNum = reader.getLineNumber();
        line = String.valueOf(lineNum);
        if (total_lines > 0) {
          double frac = lineNum / (double) total_lines;
          String percent = pctFmt.format(frac);
          line = line + ", " + percent;
        }
      }
      return "Reading " + filename + " (line " + line + ") ...";
    }

    public String line_file_message() {
      return String.format(" at line %d in file %s", reader.getLineNumber(), filename);
    }
  }

  /** Returns the current line number in the input file, or -1 if not available. */
  public static int get_linenum() {
    if (FileIO.data_trace_state == null) {
      return -1;
    } else {
      return FileIO.data_trace_state.get_linenum();
    }
  }

  /**
   * Logically, this is a local variable in static method read_data_trace_file. It is used for
   * status output, and to give the line number at which a problem was detected.
   */
  // The @MonotonicNonNull property is not true globally, but within every
  // method it's true, so it is a useful annotation.
  public static @MonotonicNonNull ParseState data_trace_state = null;
  // The variable is only ever cleared at the end of a routine that set it.
  @SuppressWarnings("nullness") // reinitialization
  private static void clear_data_trace_state() {
    FileIO.data_trace_state = null;
  }

  /**
   * Read only samples from {@code .dtrace} file. Uses the standard data processor which calls
   * {@link FileIO#process_sample(PptMap, PptTopLevel, ValueTuple, Integer)} on each record, and
   * ignores records other than samples.
   */
  public static void read_data_trace_file(String filename, PptMap all_ppts) throws IOException {
    Processor processor = new Processor();
    read_data_trace_file(filename, all_ppts, processor, false, true);
  }

  /**
   * Read declarations AND samples (not just sample data as the name might imply) from {@code
   * .dtrace} file. For each record read from the file, passes the record to a method of the
   * processor.
   */
  public static void read_data_trace_file(
      String filename,
      PptMap all_ppts,
      Processor processor,
      boolean is_decl_file,
      boolean ppts_may_be_new)
      throws IOException {

    if (debugRead.isLoggable(Level.FINE)) {
      debugRead.fine(
          "read_data_trace_file "
              + filename
              + ((Daikon.ppt_regexp != null) ? " " + Daikon.ppt_regexp.pattern() : "")
              + ((Daikon.ppt_omit_regexp != null) ? " " + Daikon.ppt_omit_regexp.pattern() : ""));
    }

    ParseState data_trace_state = new ParseState(filename, is_decl_file, ppts_may_be_new, all_ppts);
    FileIO.data_trace_state = data_trace_state;

    // Used for debugging: write new data trace file.
    if (Global.debugPrintDtrace) {
      Global.dtraceWriter =
          new PrintWriter(Files.newBufferedWriter(new File(filename + ".debug").toPath(), UTF_8));
    }

    while (true) {
      read_data_trace_record(data_trace_state);

      if (data_trace_state.rtype == RecordType.SAMPLE) {
        assert data_trace_state.ppt != null
            : "@AssumeAssertion(nullness): dependent: RecordType.SAMPLE";
        assert data_trace_state.vt != null
            : "@AssumeAssertion(nullness): dependent: RecordType.SAMPLE";
        // Nonce may be null
        samples_processed++;
        // Add orig and derived variables; pass to inference (add_and_flow)
        try {
          processor.process_sample(
              data_trace_state.all_ppts,
              data_trace_state.ppt,
              data_trace_state.vt,
              data_trace_state.nonce);
        } catch (Error e) {
          // e.printStackTrace();
          if (!dkconfig_continue_after_file_exception) {
            throw new Daikon.UserError(e, data_trace_state);
          } else {
            System.out.println();
            System.out.println(
                "WARNING: Error while processing trace file; subsequent records ignored.");
            System.out.print("Ignored backtrace:");
            e.printStackTrace(System.out);
            System.out.println();
          }
        }
      } else if ((data_trace_state.rtype == RecordType.EOF)
          || (data_trace_state.rtype == RecordType.TRUNCATED)) {
        break;
      } else {
        // don't need to do anything explicit for other records found
      }
    }

    if (Global.debugPrintDtrace) {
      assert Global.dtraceWriter != null
          : "@AssumeAssertion(nullness): dependent: set if debugPrintDtrace is true";
      Global.dtraceWriter.close();
    }

    Daikon.progress = "Finished reading " + data_trace_state.filename;

    clear_data_trace_state();
  }

  /**
   * Like read_data_trace_record, but sets global FileIO.data_trace_state for the duration of the
   * call then clears it before returning. Intended for most external callers.
   */
  public static void read_data_trace_record_setstate(ParseState state) throws IOException {

    FileIO.data_trace_state = state;
    read_data_trace_record(state);
    clear_data_trace_state();
  }

  /**
   * Read a single record of ANY type (sample, declaration, comparability, etc.) from a dtrace file.
   * If the record is anything but a sample, also processes it. The record is stored by side effect
   * into the state argument.
   */
  // TODO:  For clarity, this should perhaps return its side-effected argument.
  @RequiresNonNull("FileIO.data_trace_state")
  // not guaranteed: File might be empty  EnsuresNonNull("FileIO.new_decl_format")
  public static void read_data_trace_record(ParseState state) throws IOException {

    // Abstract out the test result into a variable because Java doesn't
    // permit suppressing warnings on a statement.  Yuck.
    boolean stateOK = (state == FileIO.data_trace_state);
    assert stateOK;

    LineNumberReader reader = state.reader;

    for (String line = reader.readLine(); line != null; line = reader.readLine()) {
      if (line.equals("")) {
        continue;
      }

      // This cleverness would not be necessary if every comment was followed by
      // a blank line.  We can't depend on that, though.
      if (isComment(line)) {
        StringJoiner commentLines = new StringJoiner(lineSep);
        commentLines.add(line);
        while (nextLineIsComment(reader)) {
          commentLines.add(reader.readLine());
        }
        state.payload = commentLines.toString();
        state.rtype = RecordType.COMMENT;
        return;
      }

      // stop at a specified point in the file
      if ((dkconfig_max_line_number > 0) && (reader.getLineNumber() > dkconfig_max_line_number)) {
        state.rtype = RecordType.TRUNCATED;
        return;
      }

      // interning bugfix:  no need to intern "line" (after code change to is_declaration_header)

      // Check for the file format
      if (line.startsWith("decl-version")) {
        read_decl_version(state, line);
        state.payload = (new_decl_format ? "2.0" : "1.0");
        state.payload = (FileIO.new_decl_format ? "2.0" : "1.0");
        state.rtype = RecordType.DECL_VERSION;
        return;
      }

      // Check for the input language
      if (line.startsWith("input-language")) {
        String input_language = read_input_language(state, line);
        state.payload = input_language;
        state.rtype = RecordType.INPUT_LANGUAGE;
        return;
      }

      // If we have gotten to here and new_decl_format is not set, presume
      // it is the old format
      if (new_decl_format == null) {
        // System.out.printf("setting new_decl_format to false%n");
        new_decl_format = Boolean.FALSE;
      }

      // First look for declarations in the dtrace stream
      if (is_declaration_header(line)) {
        if (new_decl_format) {
          state.ppt = read_ppt_decl(state, line);
        } else {
          state.ppt = read_declaration(state);
        }
        // ppt can be null if this declaration was skipped because of
        // --ppt-select-pattern or --ppt-omit-pattern.
        if (state.ppt != null) {
          if (!state.all_ppts.containsName(state.ppt.name())) {
            state.all_ppts.add(state.ppt);
            assert state.ppt != null; // for nullness checker
            try {
              Daikon.init_ppt(state.ppt, state.all_ppts);
            } catch (Exception e) {
              decl_error(state, e);
            }
          }
        }
        state.rtype = RecordType.DECL;
        return;
      }
      if (line.equals("VarComparability") || line.startsWith("var-comparability")) {
        state.varcomp_format = read_var_comparability(state, line);
        state.rtype = RecordType.COMPARABILITY;
        return;
      }
      if (line.equals("ListImplementors")) {
        state.payload = read_list_implementors(reader);
        state.rtype = RecordType.LIST_IMPLEMENTORS;
        return;
      }
      String ppt_name = line;
      if (new_decl_format) ppt_name = unescape_decl(line); // interning bugfix: no need to intern
      ppt_name = user_mod_ppt_name(ppt_name);
      if (!ppt_included(ppt_name)) {
        // System.out.printf("skipping ppt %s%n", line);
        while ((line != null) && !line.equals("")) line = reader.readLine();
        continue;
      }
      // System.out.printf("Not skipping ppt  %s%n", line);

      if (state.is_decl_file) {
        if (!new_decl_format && line.startsWith("ppt ")) {
          throw new Daikon.UserError(
              String.format(
                  "Declaration file %s is not version 2.0, but line %d looks like a version 2.0 declaration: %s%nPerhaps the file is missing a \"decl-version 2.0\" record at the beginning",
                  state.filename, state.reader.getLineNumber(), line));
        }
        throw new Daikon.UserError(
            String.format(
                "Declaration files should not contain samples, but file %s does at line %d: %s",
                state.filename, state.reader.getLineNumber(), line));
      }

      // Parse the ppt name
      try {
        new PptName(ppt_name);
      } catch (Throwable t) {
        @SuppressWarnings("nullness") // thrown exception always has a detail message
        @NonNull String message = t.getMessage();
        // Augment the message with line number information.
        if (!(t instanceof Daikon.UserError)) {
          message = String.format("Illegal program point name '%s' (%s)", ppt_name, message);
        }
        throw new Daikon.UserError(message, reader, state.filename);
      }

      if (state.all_ppts.size() == 0) {
        throw new Daikon.UserError(
            "No declarations were provided before the first sample.  Perhaps you did not supply the proper .decls file to Daikon.  (Or, there could be a bug in the front end that created the .dtrace file "
                + state.filename
                + ".)");
      }

      PptTopLevel ppt = state.all_ppts.get(ppt_name);
      if (ppt == null) {
        throw new Daikon.UserError(
            "No declaration was provided for program point " + ppt_name, state);
      }

      // not vis.length, as that includes constants, derived variables, etc.
      // Actually, we do want to leave space for _orig vars.
      // And for the time being (and possibly forever), for derived variables.
      int vals_array_size = ppt.var_infos.length - ppt.num_static_constant_vars;

      // Read an invocation nonce if one exists
      Integer nonce;

      boolean nonce_exists;
      {
        String nonce_header_peekahead;
        // arbitrary number, hopefully big enough; catch exceptions
        reader.mark(1000);
        try {
          nonce_header_peekahead = reader.readLine();
        } catch (Exception e) {
          nonce_header_peekahead = null;
        }
        reader.reset();
        nonce_exists = NONCE_HEADER.equals(nonce_header_peekahead);
      }
      if (!nonce_exists) {
        nonce = null;
      } else {
        @SuppressWarnings("nullness") // nonce_exists is true, so readLine() returns non-null
        @NonNull String nonce_header = reader.readLine(); // read & discard header
        assert NONCE_HEADER.equals(nonce_header);
        String nonce_number = reader.readLine();
        if (nonce_number == null) {
          throw new Daikon.UserError("File ended while trying to read nonce", state);
        }
        nonce = Integer.valueOf(nonce_number);

        if (Global.debugPrintDtrace) {
          to_write_nonce = true;
          nonce_value = nonce.toString();
        }
      }

      @Nullable Object[] vals = new @Nullable Object[vals_array_size];
      int[] mods = new int[vals_array_size];

      // Read a single record from the trace file;
      // fills up vals and mods arrays by side effect.
      try {
        read_vals_and_mods_from_trace_file(reader, state.filename, ppt, vals, mods);
      } catch (IOException e) {
        String nextLine = reader.readLine();
        if ((e instanceof EOFException) || (nextLine == null)) {
          System.out.println();
          System.out.println(
              "WARNING: Unexpected EOF while processing "
                  + "trace file - last record of trace file ignored");
          state.rtype = RecordType.EOF;
          return;
        } else if (dkconfig_continue_after_file_exception) {
          System.out.println();
          System.out.println("WARNING: IOException while processing trace file - record ignored");
          System.out.print("Ignored backtrace:");
          e.printStackTrace(System.out);
          System.out.println();
          while (nextLine != null && !nextLine.equals("")) {
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
      state.rtype = RecordType.SAMPLE;
      return;
    }

    state.rtype = RecordType.EOF;
    return;
  }

  /**
   * Add orig() and derived variables to vt (by side effect), then supply it to the program point
   * for flowing.
   *
   * @param vt trace data only; modified by side effect to add derived vars
   */
  @RequiresNonNull("FileIO.data_trace_state")
  public static void process_sample(
      PptMap all_ppts, PptTopLevel ppt, ValueTuple vt, @Nullable Integer nonce) {

    // Add orig variables.  This must be above the check below because
    // it saves away the orig values from enter points for later use
    // by exit points.
    boolean ignore = compute_orig_variables(ppt, vt.vals, vt.mods, nonce);
    if (ignore) {
      return;
    }

    // Only process the leaves of the ppt tree.
    // This test assumes that all leaves are numbered exit program points
    // -- that is, points of the form foo:::EXIT22 for which isExitPoint()
    // is true and isCombinedExitPoint() is false.  "Combined" exit points
    // of the form foo:::EXIT are not processed -- they are assumed to be
    // non-leaves.
    if (Daikon.use_dataflow_hierarchy) {

      // Rather than defining leaves as :::EXIT54 (numbered exit)
      // program points define them as everything except
      // ::EXIT (combined), :::ENTER, :::THROWS, :::OBJECT, ::GLOBAL
      //  and :::CLASS program points.  This scheme ensures that arbitrarly
      //  named program points such as :::POINT (used by convertcsv.pl)
      //  will be treated as leaves.

      if (ppt.ppt_name.isEnterPoint()
          || ppt.ppt_name.isThrowsPoint()
          || ppt.ppt_name.isObjectInstanceSynthetic()
          || ppt.ppt_name.isClassStaticSynthetic()
          || ppt.ppt_name.isGlobalPoint()) {
        return;
      }

      if (ppt.ppt_name.isExitPoint() && ppt.ppt_name.isCombinedExitPoint()) {
        // not Daikon.UserError; caller has more info (e.g., filename)
        throw new RuntimeException(
            "Bad program point name " + ppt.name + " is a combined exit point name");
      }
    }

    // Add derived variables
    compute_derived_variables(ppt, vt.vals, vt.mods);

    // Causes interning
    vt = new ValueTuple(vt.vals, vt.mods);

    if (debugRead.isLoggable(Level.FINE)) {
      debugRead.fine("Adding ValueTuple to " + ppt.name());
      debugRead.fine("  length is " + vt.vals.length);
    }

    // If we are only reading the sample, don't process them
    if (dkconfig_read_samples_only) {
      return;
    }

    @SuppressWarnings({
      "UnusedVariable",
      "nullness:flowexpr.parse.error"
    }) // https://tinyurl.com/cfissue/862
    Object dummy = ppt.add_bottom_up(vt, 1);

    if (debugVars.isLoggable(Level.FINE)) {
      debugVars.fine(ppt.name() + " vars: " + Debug.int_vars(ppt, vt));
    }

    if (Global.debugPrintDtrace) {
      assert Global.dtraceWriter != null
          : "@AssumeAssertion(nullness): dependent: set if debugPrintDtrace is true";
      Global.dtraceWriter.close();
    }
  }

  /** Returns true if this procedure has an unmatched entry. */
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

  /** Print each call that does not have a matching exit. */
  public static void process_unmatched_procedure_entries() {

    if (dkconfig_unmatched_procedure_entries_quiet) {
      return;
    }

    int unmatched_count = call_stack.size() + call_hashmap.size();

    if (!call_stack.isEmpty() || !call_hashmap.isEmpty()) {
      System.out.println();
      System.out.print(
          "No return from procedure observed " + UtilPlume.nplural(unmatched_count, "time") + ".");
      if (Daikon.use_dataflow_hierarchy) {
        System.out.print("  Unmatched entries are ignored!");
      }
      System.out.println();
      if (!call_hashmap.isEmpty()) {
        // Put the invocations in sorted order for printing.
        ArrayList<Invocation> invocations = new ArrayList<>();
        for (@KeyFor("call_hashmap") Integer i : CollectionsPlume.sortedKeySet(call_hashmap)) {
          Invocation invok = call_hashmap.get(i);
          assert invok != null;
          invocations.add(invok);
        }
        System.out.println("Unterminated calls:");
        if (dkconfig_verbose_unmatched_procedure_entries) {
          print_invocations_verbose(invocations);
        } else {
          print_invocations_grouped(invocations);
        }
      }

      if (!call_stack.isEmpty()) {
        if (dkconfig_verbose_unmatched_procedure_entries) {
          System.out.println(
              "Remaining "
                  + UtilPlume.nplural(unmatched_count, "stack")
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

  /** Print all the invocations in the collection, in order. */
  static void print_invocations_verbose(Collection<Invocation> invocations) {
    for (Invocation invok : invocations) {
      System.out.println(invok.format());
    }
  }

  /** Print the invocations in the collection, in order, and coalescing duplicates. */
  static void print_invocations_grouped(Collection<Invocation> invocations) {
    Map<@Interned String, Integer> counter = new LinkedHashMap<>();

    for (Invocation invok_noncanonical : invocations) {
      @Interned Invocation invok = invok_noncanonical.canonicalize();
      String invokString = invok.format(false).intern();
      if (counter.containsKey(invokString)) {
        Integer oldCount = counter.get(invokString);
        Integer newCount = oldCount.intValue() + 1;
        counter.put(invokString, newCount);
      } else {
        counter.put(invokString, 1);
      }
    }

    // Print the invocations in sorted order.
    for (Map.Entry<@Interned String, Integer> invokEntry : counter.entrySet()) {
      System.out.println(
          invokEntry.getKey() + " : " + UtilPlume.nplural(invokEntry.getValue(), "invocation"));
    }
  }

  // This procedure reads a single record from a trace file and
  // fills up vals and mods by side effect.  The ppt name and
  // invocation nonce (if any) have already been read.
  @RequiresNonNull("FileIO.data_trace_state")
  private static void read_vals_and_mods_from_trace_file(
      LineNumberReader reader,
      String filename,
      PptTopLevel ppt,
      @Nullable Object[] vals,
      int[] mods)
      throws IOException {
    VarInfo[] vis = ppt.var_infos;
    int num_tracevars = ppt.num_tracevars;

    /*NNC:@Nullable*/ String[] oldvalue_reps = ppt_to_value_reps.get(ppt);
    if (oldvalue_reps == null) {
      // We've not encountered this program point before.  The nulls in
      // this array will compare non-equal to whatever is in the trace
      // file, which is the desired behavior.
      oldvalue_reps = new /*NNC:@Nullable*/ String[num_tracevars];
    }

    if (Global.debugPrintDtrace) {
      assert Global.dtraceWriter != null
          : "@AssumeAssertion(nullness): dependent: set if debugPrintDtrace is true";
      Global.dtraceWriter.println(ppt.name());

      if (to_write_nonce) {
        Global.dtraceWriter.println(NONCE_HEADER);
        Global.dtraceWriter.println(nonce_value);
        to_write_nonce = false;
      }
    }

    for (int vi_index = 0, val_index = 0; val_index < num_tracevars; vi_index++) {
      assert vi_index < vis.length
          : "Got to vi_index "
              + vi_index
              + " after "
              + val_index
              + " of "
              + num_tracevars
              + " values";
      VarInfo vi = vis[vi_index];
      assert !vi.is_static_constant || (vi.value_index == -1)
      // : "Bad value_index " + vi.value_index + " when static_constant_value = " +
      // vi.static_constant_value + " for " + vi.repr() + " at " + ppt_name
      ;
      if (vi.is_static_constant) {
        continue;
      }
      assert val_index == vi.value_index
      // : "Differing val_index = " + val_index
      // + " and vi.value_index = " + vi.value_index
      // + " for " + vi.name + lineSep + vi.repr()
      ;

      // In errors, say "for program point", not "at program point" as the
      // latter confuses Emacs goto-error.

      String line = reader.readLine();
      if (line == null) {
        throw new Daikon.UserError(
            "Unexpected end of file at "
                + data_trace_state.filename
                + " line "
                + reader.getLineNumber()
                + lineSep
                + "  Expected variable "
                + vi.name()
                + ", got "
                + "null" // line
                + " for program point "
                + ppt.name());
      }

      // Read lines until an included variable is found
      while ((line != null) && !line.equals("") && !var_included(line)) {
        line = reader.readLine(); // value (discard it)
        line = reader.readLine(); // modbit
        if (line == null || !(line.equals("0") || line.equals("1") || line.equals("2"))) {
          throw new Daikon.UserError("Bad modbit '" + line + "'", data_trace_state);
        }
        line = reader.readLine(); // next variable name
      }
      if (line == null) {
        throw new Daikon.UserError(
            "Unexpected end of file at "
                + data_trace_state.filename
                + " line "
                + reader.getLineNumber()
                + lineSep
                + "  Expected to find variable name"
                + " for program point "
                + ppt.name());
      }

      if (!unescape_decl(line.trim()).equals(vi.str_name())) {
        throw new Daikon.UserError(
            "Mismatch between declaration and trace.  Expected variable "
                + vi.name()
                + ", got "
                + line
                + " for program point "
                + ppt.name(),
            data_trace_state);
      }
      line = reader.readLine();
      if (line == null) {
        throw new Daikon.UserError(
            "Unexpected end of file at "
                + data_trace_state.filename
                + " line "
                + reader.getLineNumber()
                + lineSep
                + "  Expected value for variable "
                + vi.name()
                + ", got "
                + "null" // line
                + " for program point "
                + ppt.name());
      }
      String value_rep = line;
      line = reader.readLine();
      if (line == null) {
        throw new Daikon.UserError(
            "Unexpected end of file at "
                + data_trace_state.filename
                + " line "
                + reader.getLineNumber()
                + lineSep
                + "  Expected modbit for variable "
                + vi.name()
                + ", got "
                + "null" // line
                + " for program point "
                + ppt.name());
      }
      if (!(line.equals("0") || line.equals("1") || line.equals("2"))) {
        throw new Daikon.UserError("Bad modbit `" + line + "'", data_trace_state);
      }
      int mod = ValueTuple.parseModified(line);

      // System.out.println("Mod is " + mod + " at " + data_trace_state.filename + " line " +
      // reader.getLineNumber());
      // System.out.pringln("  for variable " + vi.name()
      //                   + " for program point " + ppt.name());

      // MISSING_FLOW is only found during flow algorithm
      assert mod != ValueTuple.MISSING_FLOW : "Data trace value can't be missing due to flow";

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
        assert Global.dtraceWriter != null
            : "@AssumeAssertion(nullness): dependent: set if debugPrintDtrace is true";
        Global.dtraceWriter.println(vi.name());
        Global.dtraceWriter.println(value_rep);
        Global.dtraceWriter.println(mod);
      }
      Debug dbg = Debug.newDebug(FileIO.class, ppt, Debug.vis(vi));
      if (dbg != null) dbg.log("Var " + vi.name() + " has value " + value_rep + " mod " + mod);

      // Both uninit and nonsensical mean missing modbit 2, because
      // it doesn't make sense to look at x.y when x is uninitialized.
      if (ValueTuple.modIsMissingNonsensical(mod)) {
        if (!(value_rep.equals("nonsensical")
            // Kvasir still uses "uninit" (it distinguishes between
            // uninit and nonsensical), though the Daikon manual does not
            // officially permit "uninit" as a value and has not since at
            // least 2002.  This is fixed in the Kvasir repository as of
            // 5/2009, so the following two lines should be removed at
            // some point not too long after that.  Then Daikon should
            // print a warning (or even terminate execution) about uses
            // of "uninit".
            || value_rep.equals("uninit")
            || value_rep.equals("missing"))) {
          throw new Daikon.UserError(
              "Modbit indicates nonsensical value for variable "
                  + vi.name()
                  + " with value \""
                  + value_rep
                  + "\";"
                  + lineSep
                  + "  text of value should be \"nonsensical\"",
              data_trace_state);
        } else {
          if (debug_missing && !vi.canBeMissing) {
            System.out.printf(
                "Var %s ppt %s at line %d missing%n", vi, ppt.name(), FileIO.get_linenum());
            System.out.printf("val_index = %d, mods[val_index] = %d%n", val_index, mods[val_index]);
          }
          vi.canBeMissing = true;
        }
        vals[val_index] = null;
      } else {
        // mod is not MISSING_NONSENSICAL

        // System.out.println("Mod is " + mod + " (missing=" +
        // ValueTuple.MISSING + "), rep=" + value_rep +
        // "(modIsMissing=" + ValueTuple.modIsMissing(mod) + ")");

        try {
          vals[val_index] = vi.rep_type.parse_value(value_rep, reader, filename);
          if (vals[val_index] == null) {
            if (debug_missing && !vi.canBeMissing) {
              System.out.printf(
                  "Var %s ppt %s at line %d is null, and modbit is not missing%n",
                  vi, ppt.name(), FileIO.get_linenum());
            }
            // The value in the trace was null even though the modbit was not
            // MISSING_NONSENSICAL.  Set the modbit to MISSING_NONSENSICAL.
            // This can happen for a value like [1 nonsensical 2], because
            // if any array value is nonsensical, the whole array is
            // treated as nonsensical.
            mods[val_index] = ValueTuple.MISSING_NONSENSICAL;
            vi.canBeMissing = true;
          }
        } catch (Daikon.UserError e) {
          throw e;
        } catch (Throwable e) {
          // e.printStackTrace(System.err); // for debugging
          throw new Daikon.UserError(
              e,
              "Error while parsing value "
                  + value_rep
                  + " for variable "
                  + vi.name()
                  + " of type "
                  + vi.rep_type
                  + ": "
                  + e.getLocalizedMessage(),
              reader,
              filename);
        }
      }
      val_index++;
    }

    // Does oldvalue_reps now have no null elements???
    oldvalue_reps = castNonNullDeep(oldvalue_reps); // https://tinyurl.com/cfissue/986
    ppt_to_value_reps.put(ppt, oldvalue_reps);

    if (Global.debugPrintDtrace) {
      assert Global.dtraceWriter != null
          : "@AssumeAssertion(nullness): dependent: set if debugPrintDtrace is true";
      Global.dtraceWriter.println();
    }

    // Expecting the end of a block of values.
    String line = reader.readLine();
    // First, we might get some variables that ought to be omitted.
    while ((line != null) && !line.equals("") && !var_included(line)) {
      line = reader.readLine(); // value
      line = reader.readLine(); // modbit
      line = reader.readLine(); // next variable name
    }
    assert (line == null) || line.equals("")
        : "Expected blank line in "
            + data_trace_state.filename
            + " at line "
            + reader.getLineNumber()
            + ": "
            + line;
  }

  /**
   * If this is a function entry ppt, stores the values of all of the variables away for use at the
   * exit. If this is an exit, finds the values at enter and adds them as the values of the orig
   * variables. Normally returns false. Returns true if this is an exit without a matching enter.
   * See dkconfig_ignore_missing_enter for more info. If true is returned, this ppt should be
   * ignored by the caller.
   */
  @RequiresNonNull("FileIO.data_trace_state")
  public static boolean compute_orig_variables(
      PptTopLevel ppt,
      // HashMap cumulative_modbits,
      @Nullable Object[] vals,
      int[] mods,
      @Nullable Integer nonce) {
    assert data_trace_state != null;

    VarInfo[] vis = ppt.var_infos;
    @Interned String fn_name = ppt.ppt_name.getNameWithoutPoint();
    String ppt_name = ppt.name();
    if (ppt_name.endsWith(enter_tag)) {
      Invocation invok = new Invocation(ppt, vals, mods);
      if (nonce == null) {
        call_stack.push(invok);
      } else {
        call_hashmap.put(nonce, invok);
      }
      return false;
    }

    if (ppt.ppt_name.isExitPoint() || ppt.ppt_name.isThrowsPoint()) {
      Invocation invoc;
      // Set invoc
      {
        if (nonce == null) {
          if (call_stack.isEmpty()) {
            // Not Daikon.UserError:  caller knows context such as
            // file name and line number.
            throw new Error("Function exit without corresponding entry: " + ppt.name());
          }
          invoc = call_stack.pop();
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
            invoc = call_stack.pop();
          }
        } else {
          // nonce != null
          if (!call_hashmap.containsKey(nonce)) {
            if (dkconfig_ignore_missing_enter) {
              // System.out.printf("Didn't find call with nonce %d to match %s" +
              //                   " ending at %s line %d\n", nonce, ppt.name(),
              //                   data_trace_state.filename,
              //                   data_trace_state.reader.getLineNumber());
              return true;
            } else {
              // Not Daikon.UserError:  caller knows context such as
              // file name and line number.
              throw new Error(
                  String.format(
                      "Didn't find call with nonce %s to match %s ending at %s line %d",
                      nonce,
                      ppt.name(),
                      data_trace_state.filename,
                      data_trace_state.reader.getLineNumber()));
            }
          }
          invoc = call_hashmap.get(nonce);
          call_hashmap.remove(nonce);
        }
      }

      // Loop through each orig variable and get its value/mod bits from
      // the ENTER point.  vi_index is the index into var_infos at the
      // ENTER point.  val_index is the index into vals[] and mods[] at
      // ENTER point.  Note that vis[] includes static constants but
      // vals[] and mods[] do not.  Also that we don't create orig versions
      // of static constants
      int vi_index = 0;
      for (int val_index = 0; val_index < ppt.num_orig_vars; val_index++) {
        VarInfo vi = vis[ppt.num_tracevars + ppt.num_static_constant_vars + val_index];
        assert (!vi.is_static_constant) : "orig constant " + vi;

        // Skip over constants in the entry point
        while (invoc.ppt.var_infos[vi_index].is_static_constant) vi_index++;

        // Copy the vals and mod bits from entry to exit
        vals[ppt.num_tracevars + val_index] = invoc.vals[val_index];
        int mod = invoc.mods[val_index];
        mods[ppt.num_tracevars + val_index] = mod;

        // If the value was missing, mark this variable as can be missing.
        // Carefully check that we have orig version of the variable from
        // the ENTER point.
        if (ValueTuple.modIsMissingNonsensical(mod)) {
          if (debug_missing && !vi.canBeMissing) {
            System.out.printf("add_orig: var %s missing[%d/%d]%n", vi, val_index, vi_index);
          }
          vi.canBeMissing = true;
          assert invoc.vals[val_index] == null;
          assert vi.name() == invoc.ppt.var_infos[vi_index].prestate_name()
              : vi.name() + " != " + invoc.ppt.var_infos[vi_index];
          assert invoc.ppt.var_infos[vi_index].canBeMissing : invoc.ppt.var_infos[vi_index];
        }
        vi_index++;
      }
    }
    return false;
  }

  /** Computes values of derived variables. */
  public static void compute_derived_variables(
      PptTopLevel ppt, @Nullable Object[] vals, int[] mods) {
    // This ValueTuple is temporary:  we're temporarily suppressing interning,
    // which we will do after we have all the values available.
    ValueTuple partial_vt = ValueTuple.makeUninterned(vals, mods);
    int filled_slots = ppt.num_orig_vars + ppt.num_tracevars + ppt.num_static_constant_vars;
    for (int i = 0; i < filled_slots; i++) {
      assert !ppt.var_infos[i].isDerived();
    }
    int num_const = ppt.num_static_constant_vars;
    for (int i = filled_slots; i < ppt.var_infos.length; i++) {
      assert ppt.var_infos[i].derived != null : "variable not derived: " + ppt.var_infos[i].repr();
      assert ppt.var_infos[i].derived != null : "@AssumeAssertion(nullness): application invariant";
      // Add this derived variable's value
      ValueAndModified vm = ppt.var_infos[i].derived.computeValueAndModified(partial_vt);
      vals[i - num_const] = vm.value;
      mods[i - num_const] = vm.modified;
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  /// Serialized PptMap files
  ///

  /**
   * Use a special record type. Saving as one object allows for reference-sharing, easier saves and
   * loads, and potential for later overriding of SerialFormat.readObject if the save format changes
   * (ick).
   */
  static final class SerialFormat implements Serializable {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20060905L;

    @RequiresNonNull("FileIO.new_decl_format")
    public SerialFormat(PptMap map, Configuration config) {
      this.map = map;
      this.config = config;
      this.new_decl_format = FileIO.new_decl_format;
    }

    public PptMap map;
    public Configuration config;
    public boolean new_decl_format = false;
  }

  public static void write_serialized_pptmap(PptMap map, File file) throws IOException {
    SerialFormat record = new SerialFormat(map, Configuration.getInstance());
    UtilPlume.writeObject(record, file);
  }

  /**
   * Read either a serialized PptMap or a InvMap and return a PptMap. If an InvMap is specified, it
   * is converted to a PptMap.
   */
  @EnsuresNonNull("FileIO.new_decl_format")
  public static PptMap read_serialized_pptmap(File file, boolean use_saved_config)
      throws IOException {

    try {
      Object obj = UtilPlume.readObject(file);
      if (obj instanceof FileIO.SerialFormat) {
        SerialFormat record = (SerialFormat) obj;
        if (use_saved_config) {
          Configuration.getInstance().overlap(record.config);
        }
        FileIO.new_decl_format = record.new_decl_format;
        // System.err.printf("Setting FileIO.new_decl_format to %b%n",
        //                   FileIO.new_decl_format);
        return record.map;
      } else if (obj instanceof InvMap) {
        // System.err.printf("Restoring an InvMap%n");
        InvMap invs = (InvMap) obj;
        PptMap ppts = new PptMap();
        for (PptTopLevel ppt : invs.pptIterable()) {
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
        assert FileIO.new_decl_format != null
            : "@AssumeAssertion(nullness): InvMap.readObject() sets FileIO.new_decl_format";
        return ppts;
      } else {
        throw new IOException("Unexpected serialized file type: " + obj.getClass());
      }
    } catch (ClassNotFoundException e) {
      throw (IOException) new IOException("Error while loading inv file").initCause(e);
    } catch (InvalidClassException e) {
      throw new IOException(
          "It is likely that the .inv file format has changed, because a Daikon data structure has been modified, so your old .inv file is no longer readable by Daikon.  Please regenerate your .inv file."
          // + lineSep + e.toString()
          );
    }
    // } catch (StreamCorruptedException e) { // already extends IOException
    // } catch (OptionalDataException e) {    // already extends IOException
  }

  /**
   * Returns whether or not the specified ppt name should be included in processing. Ppts can be
   * excluded because they match the omit_regexp, don't match ppt_regexp, or are greater than
   * ppt_max_name.
   */
  public static boolean ppt_included(String ppt_name) {

    // System.out.println ("ppt_name = '" + ppt_name + "' max name = '"
    //                     + Daikon.ppt_max_name + "'");
    if (((Daikon.ppt_omit_regexp != null) && Daikon.ppt_omit_regexp.matcher(ppt_name).find())
        || ((Daikon.ppt_regexp != null) && !Daikon.ppt_regexp.matcher(ppt_name).find())
        || ((Daikon.ppt_max_name != null)
            && ((Daikon.ppt_max_name.compareTo(ppt_name) < 0)
                && (ppt_name.indexOf(global_suffix) == -1)))) {
      return false;
    } else {
      return true;
    }
  }

  /**
   * Returns true if the given variable is included, according to Daikon's {@code
   * --var-select-pattern} and {@code --var-omit-pattern} flags.
   */
  public static boolean var_included(String var_name) {
    assert !var_name.equals("");
    if (((Daikon.var_omit_regexp != null) && Daikon.var_omit_regexp.matcher(var_name).find())
        || ((Daikon.var_regexp != null) && !Daikon.var_regexp.matcher(var_name).find())) {
      return false;
    } else {
      return true;
    }
  }

  /**
   * Checks the specified array of variables to see if it matches exactly the variables in the
   * existing ppt. Throws an error if there are any differences. Used to ensure that a new ppt with
   * the same name as an existing ppt is exactly the same.
   */
  static void check_decl_match(ParseState state, PptTopLevel existing_ppt, VarInfo[] vi_array) {

    VarInfo[] existing_vars = existing_ppt.var_infos;
    if (existing_ppt.num_declvars != vi_array.length) {
      throw new Daikon.UserError(
          "Duplicate declaration of program point \""
              + existing_ppt.name()
              + "\" with a different number of VarInfo objects: "
              + "old VarInfo number="
              + existing_ppt.num_declvars
              + ", new VarInfo number="
              + vi_array.length,
          state);
    }

    for (int i = 0; i < vi_array.length; i++) {
      String oldName = existing_vars[i].str_name();
      String newName = vi_array[i].str_name();
      if (!oldName.equals(newName)) {
        throw new Daikon.UserError(
            "Duplicate declaration of program point \""
                + existing_ppt.name()
                + "\" with two different VarInfo: old VarInfo="
                + oldName
                + ", new VarInfo="
                + newName,
            state);
      }
    }
  }

  /** Skips over a decl. Essentially reads in everything up to and including the next blank line. */
  private static void skip_decl(LineNumberReader reader) throws IOException {
    String line = reader.readLine();
    // This fails if some lines of a declaration (e.g., the comparability
    // field) are empty.
    while ((line != null) && !line.equals("")) {
      line = reader.readLine();
    }
  }

  /**
   * Converts the declaration record version of a name into its correct version. In the declaration
   * record, blanks are encoded as \_ and backslashes as \\.
   */
  private static String unescape_decl(String orig) {
    StringBuilder sb = new StringBuilder(orig.length());
    // The previous escape character was seen just before this position.
    int post_esc = 0;
    int this_esc = orig.indexOf('\\');
    while (this_esc != -1) {
      if (this_esc == orig.length() - 1) {
        sb.append(orig.substring(post_esc, this_esc + 1));
        post_esc = this_esc + 1;
        break;
      }
      switch (orig.charAt(this_esc + 1)) {
        case 'n':
          sb.append(orig.substring(post_esc, this_esc));
          sb.append('\n'); // not lineSep
          post_esc = this_esc + 2;
          break;
        case 'r':
          sb.append(orig.substring(post_esc, this_esc));
          sb.append('\r');
          post_esc = this_esc + 2;
          break;
        case '_':
          sb.append(orig.substring(post_esc, this_esc));
          sb.append(' ');
          post_esc = this_esc + 2;
          break;
        case '\\':
          // This is not in the default case because the search would find
          // the quoted backslash.  Here we incluce the first backslash in
          // the output, but not the first.
          sb.append(orig.substring(post_esc, this_esc + 1));
          post_esc = this_esc + 2;
          break;

        default:
          // In the default case, retain the character following the
          // backslash, but discard the backslash itself.  "\*" is just
          // a one-character string.
          sb.append(orig.substring(post_esc, this_esc));
          post_esc = this_esc + 1;
          break;
      }
      this_esc = orig.indexOf('\\', post_esc);
    }
    if (post_esc == 0) {
      return orig;
    }
    sb.append(orig.substring(post_esc));
    return sb.toString();
  }

  // The reverse of unescape_decl.  Test them together.
  /**
   * Converts a name into its declaration record version. In the declaration record, blanks are
   * encoded as \_ and backslashes as \\.
   */
  private static String escape_decl(String orig) {
    return orig.replace("\\", "\\\\").replace(" ", "\\_").replace("\n", "\\n").replace("\r", "\\r");
  }

  /**
   * Class that holds information from the declaration record (in the file). Once collected, this
   * information is used to create a VarInfo. This class is necessary because a VarInfo cannot be
   * created until much of this information is present: the constructor requires all the information
   * at the time of construction, and some of the fields are final.
   *
   * <p>In general, each field has a one-to-one relation with the corresponding entry in the
   * variable definition block in the trace file. More detailed information about each of the fields
   * can be found in the 'Variable declarations' section of the 'File Formats' appendix of the
   * Daikon developers manual. Specifics can also be found in the 'parse_[field]' methods of the
   * class (eg, parse_var_kind, parse_enclosing_var_name, etc).
   */
  @SuppressWarnings(
      "nullness") // undocumented class needs documentation before annotating with nullness
  public static class VarDefinition implements java.io.Serializable, Cloneable {
    static final long serialVersionUID = 20060524L;
    /** Current information about input file and previously parsed values. */
    transient ParseState state;
    /** Name of the variable (required) */
    public String name;
    /** Type of the variable (required) */
    public VarKind kind = null;
    /** Name of variable that contains this variable (optional) */
    // seems non-null for arrays/sequences
    public @Nullable String enclosing_var_name;
    /** the simple (not fully specified) name of this variable (optional) */
    public @Nullable String relative_name = null;
    /** Type of reference for structure/class variables. */
    public RefType ref_type = RefType.POINTER;
    /** Number of array dimensions (0 or 1) */
    public int arr_dims = 0;
    /**
     * Non-null iff (vardef.kind == VarKind.FUNCTION). The arguments that were used to create this
     * function application.
     */
    public @Nullable List<String> function_args = null;
    /** The type of the variable as stored in the dtrace file (required) */
    public ProglangType rep_type = null;
    /** Declared type of the variable as an arbitrary string (required) */
    public ProglangType declared_type = null;
    /** Variable flags (optional) */
    public EnumSet<VarFlags> flags = EnumSet.noneOf(VarFlags.class);
    /** Language specific variable flags (optional) */
    public EnumSet<LangFlags> lang_flags = EnumSet.noneOf(LangFlags.class);
    /** Comparability of this variable (required. */
    public VarComparability comparability = null;
    /** Parent program points in ppt hierarchy (optional) */
    public List<VarParent> parents;
    /** Non-null if this 'variable' always has the same value (optional) */
    public @Nullable @Interned Object static_constant_value = null;
    /**
     * Non-null if it is statically known that the value of the variable will be always greater than
     * or equal to this value.
     */
    public @Nullable String min_value = null;
    /**
     * Non-null if it is statically known that the value of the variable will be always less than or
     * equal to this value.
     */
    public @Nullable String max_value = null;
    /** Non-null if it is statically known that the array will have at least this many elements. */
    public @Nullable Integer min_length = null;
    /** Non-null if it is statically known that the array will have up to this many elements. */
    public @Nullable Integer max_length = null;
    /** Non-null if the set of valid values for the variable is statically known. */
    public @Nullable String valid_values = null;

    /** Check representation invariants. */
    public void checkRep() {

      // Basic checking for sensible input
      assert name != null;
      if (kind == null) {
        throw new AssertionError("missing var-kind information for variable " + name);
      }
      assert (arr_dims == 0) || (arr_dims == 1)
          : String.format(
              "array dimensions==%s, should be 0 or 1, for variable %s", arr_dims, name);
      assert !rep_type.isArray() || arr_dims == 1
          : String.format("array dimensions is 0, should be 1, for variable %s", name);
      if (rep_type == null) {
        throw new AssertionError("missing rep-type information for variable " + name);
      }
      if (declared_type == null) {
        throw new AssertionError("missing dec-type information for variable " + name);
      }
      if (comparability == null) {
        throw new AssertionError("missing comparability information for variable " + name);
      }
      assert ((kind == VarKind.FUNCTION) || (function_args == null))
          : String.format(
              "incompatible kind=%s and function_args=%s for VarDefinition %s",
              kind, function_args, name);
      if ((kind == VarKind.FIELD || kind == VarKind.ARRAY) && enclosing_var_name == null) {
        throw new AssertionError("enclosing-var not specified for variable " + name);
      }
    }

    /** Initialize from the 'variable <em>name</em>' record. Scanner should be pointing at name. */
    public VarDefinition(ParseState state, Scanner scanner) {
      this.state = state;
      this.parents = new ArrayList<VarParent>();
      name = need(scanner, "name");
      need_eol(scanner);
      if (state.varcomp_format == VarComparability.IMPLICIT) {
        comparability = VarComparabilityImplicit.unknown;
      } else {
        comparability = VarComparabilityNone.it;
      }
    }

    public VarDefinition(String name, VarKind kind, ProglangType type) {
      this.state = null;
      this.parents = new ArrayList<VarParent>();
      this.name = name;
      this.kind = kind;
      this.rep_type = type;
      this.declared_type = type;
      comparability = VarComparabilityNone.it;
    }

    @SideEffectFree
    @Override
    public VarDefinition clone(@GuardSatisfied VarDefinition this) {
      try {
        return (VarDefinition) super.clone();
      } catch (CloneNotSupportedException e) {
        throw new Error("This can't happen: ", e);
      }
    }

    public VarDefinition copy() {
      try {
        VarDefinition copy = this.clone();
        copy.flags = flags.clone();
        copy.lang_flags = lang_flags.clone();
        return copy;
      } catch (Throwable t) {
        throw new RuntimeException(t);
      }
    }

    /** Restore interned strings. */
    private void readObject(ObjectInputStream in) throws IOException, ClassNotFoundException {
      in.defaultReadObject();
      name = name.intern();
      if (enclosing_var_name != null) {
        enclosing_var_name = enclosing_var_name.intern();
      }
      if (relative_name != null) {
        relative_name = relative_name.intern();
      }
      for (VarParent parent : parents) {
        parent.parent_ppt = parent.parent_ppt.intern();
        if (parent.parent_variable != null) {
          parent.parent_variable = parent.parent_variable.intern();
        }
      }
    }

    /** Clears the parent relations, if any existed. */
    public void clear_parent_relation() {
      parents.clear();
    }

    /** Parse a var-kind record. Scanner should be pointing at the variable kind. */
    public void parse_var_kind(Scanner scanner) {
      VarKind kind_local = parse_enum_val(scanner, VarKind.class, "variable kind");
      kind = kind_local;

      if ((kind == VarKind.FIELD) || (kind == VarKind.FUNCTION)) {
        relative_name = need(scanner, "relative name");
      }
      need_eol(scanner);
    }

    /** Parses the enclosing-var record. */
    public void parse_enclosing_var_name(Scanner scanner) {
      enclosing_var_name = need(scanner, "enclosing variable name");
      need_eol(scanner);
    }

    /** Parses the reference-type record. */
    public void parse_reference_type(Scanner scanner) {
      RefType ref_type_local = parse_enum_val(scanner, RefType.class, "reference type");
      ref_type = ref_type_local;
      need_eol(scanner);
    }

    /** Parses the array record. */
    public void parse_array(Scanner scanner) {
      @Interned String arr_str = need(scanner, "array dimensions");
      if (arr_str == "0") { // interned
        arr_dims = 0;
      } else if (arr_str == "1") { // interned
        arr_dims = 1;
      } else {
        decl_error(state, "%s found where 0 or 1 expected", arr_str);
      }
    }

    /** Parses the function-args record. */
    public void parse_function_args(Scanner scanner) {

      function_args = new ArrayList<String>();
      while (scanner.hasNext()) {
        function_args.add(unescape_decl(scanner.next()).intern());
      }
    }

    public void parse_rep_type(Scanner scanner) {
      @Interned String rep_type_str = need(scanner, "rep type");
      need_eol(scanner);
      rep_type = ProglangType.rep_parse(rep_type_str);
    }

    public void parse_dec_type(Scanner scanner) {
      @Interned String declared_type_str = need(scanner, "declaration type");
      need_eol(scanner);
      declared_type = ProglangType.parse(declared_type_str);
    }

    /** Parse the flags record. Multiple flags can be specified. */
    public void parse_flags(Scanner scanner) {

      flags.add(parse_enum_val(scanner, VarFlags.class, "Flag"));
      while (scanner.hasNext()) flags.add(parse_enum_val(scanner, VarFlags.class, "Flag"));
      // System.out.printf("flags for %s are %s%n", name, flags);
    }

    /** Parse the langauge specific flags record. Multiple flags can be specified. */
    public void parse_lang_flags(Scanner scanner) {

      lang_flags.add(parse_enum_val(scanner, LangFlags.class, "Language Specific Flag"));
      while (scanner.hasNext())
        lang_flags.add(parse_enum_val(scanner, LangFlags.class, "Language Specific Flag"));
    }

    /** Parses a comparability record. */
    public void parse_comparability(Scanner scanner) {
      @Interned String comparability_str = need(scanner, "comparability");
      need_eol(scanner);
      comparability =
          VarComparability.parse(state.varcomp_format, comparability_str, declared_type);
    }

    /** Parse a parent ppt record. */
    public void parse_parent(Scanner scanner, List<ParentRelation> ppt_parents)
        throws Daikon.ParseError {

      String parent_ppt = need(scanner, "parent ppt");
      String parent_relation_id_string = need(scanner, "parent id");
      int parent_relation_id;
      try {
        parent_relation_id = Integer.parseInt(parent_relation_id_string);
      } catch (NumberFormatException nfe) {
        throw new Daikon.ParseError("Expected a number, found: " + parent_relation_id_string);
      }
      String parent_variable = null;

      boolean found = false;
      for (ParentRelation pr : ppt_parents) {
        if ((pr.parent_ppt_name == parent_ppt) && (pr.id == parent_relation_id)) {
          found = true;
          break;
        }
      }
      if (!found) {
        decl_error(
            state,
            "specified parent ppt '%s[%d]' for variable '%s' is not a parent to this ppt",
            parent_ppt,
            parent_relation_id,
            name);
      }
      if (scanner.hasNext()) {
        parent_variable = need(scanner, "parent variable");
      }

      parents.add(new VarParent(parent_ppt, parent_relation_id, parent_variable));

      need_eol(scanner);
    }

    /** Parse a constant record. */
    public void parse_constant(Scanner scanner) {
      @Interned String constant_str = need(scanner, "constant value");
      need_eol(scanner);
      try {
        static_constant_value = rep_type.parse_value(constant_str, null, "parse_constant");
      } catch (Error e) {
        decl_error(state, e);
      }
    }

    /** Parse a minimum value record. */
    public void parse_min_value(Scanner scanner) {
      this.min_value = need(scanner, "minimum value");
      need_eol(scanner);
    }

    /** Parse a maximum value record. */
    public void parse_max_value(Scanner scanner) {
      this.max_value = need(scanner, "maximum value");
      need_eol(scanner);
    }

    /** Parse a minimum length record. */
    public void parse_min_length(Scanner scanner) {
      this.min_length = Integer.parseInt(need(scanner, "minimum length"));
      need_eol(scanner);
    }

    /** Parse a maximum length record. */
    public void parse_max_length(Scanner scanner) {
      this.max_length = Integer.parseInt(need(scanner, "maximum length"));
      need_eol(scanner);
    }

    /** Parse a valid values record. */
    public void parse_valid_values(Scanner scanner) {
      this.valid_values = scanner.nextLine();
    }

    /**
     * Helper function, returns the next string token unescaped and interned. Throw Daikon.UserError
     * if there is no next token.
     */
    public @Interned String need(Scanner scanner, String description) {
      return (FileIO.need(state, scanner, description));
    }

    /** Throws Daikon.UserError if the scanner is not at end of line */
    public void need_eol(Scanner scanner) {
      FileIO.need_eol(state, scanner);
    }

    /**
     * Looks up the next token as a member of enum_class. Throws Daikon.UserError if there is no
     * token or if it is not valid member of the class. Enums are presumed to be in in upper case.
     */
    public <E extends Enum<E>> E parse_enum_val(
        Scanner scanner, Class<E> enum_class, String descr) {
      return FileIO.parse_enum_val(state, scanner, enum_class, descr);
    }
  }

  /**
   * Helper function, returns the next string token unescaped and interned. Throws Daikon.UserError
   * if there is no next token.
   */
  public static @Interned String need(ParseState state, Scanner scanner, String description) {
    if (!scanner.hasNext()) {
      decl_error(state, "end-of-line found where %s expected", description);
    }
    return unescape_decl(scanner.next()).intern();
  }

  /** Throws a Daikon.UserError if the scanner is not at end of line */
  public static void need_eol(ParseState state, Scanner scanner) {
    if (scanner.hasNext()) {
      decl_error(state, "'%s' found where end-of-line expected", scanner.next());
    }
  }

  /**
   * Looks up the next token as a member of enum_class. Throws Daikon.UserError if there is no token
   * or if it is not valid member of the class. Enums are presumed to be in in upper case.
   */
  public static <E extends Enum<E>> E parse_enum_val(
      ParseState state, Scanner scanner, Class<E> enum_class, String descr) {

    @Interned String str = need(state, scanner, descr);
    try {
      E e = Enum.valueOf(enum_class, str.toUpperCase());
      return e;
    } catch (Exception exception) {
      @SuppressWarnings(
          "nullness") // getEnumConstants returns non-null because enum_class is an enum class
      E @NonNull [] all = enum_class.getEnumConstants();
      StringJoiner msg = new StringJoiner(", ");
      for (E e : all) {
        msg.add(String.format("'%s'", e.name().toLowerCase()));
      }
      decl_error(state, "'%s' found where %s expected", str, msg);
      throw new Error("execution cannot get to here, previous line threw an error");
    }
  }

  /** Call this to indicate a malformed declaration. */
  private static void decl_error(ParseState state, String format, @Nullable Object... args) {
    @SuppressWarnings("formatter:format.string.invalid") // https://tinyurl.com/cfissue/2584
    String msg = String.format(format, args) + state.line_file_message();
    throw new Daikon.UserError(msg);
  }

  /** Call this to indicate a malformed declaration. */
  private static void decl_error(
      ParseState state, Throwable cause, String format, @Nullable Object... args) {
    @SuppressWarnings("formatter:format.string.invalid") // https://tinyurl.com/cfissue/2584
    String msg = String.format(format, args) + state.line_file_message();
    throw new Daikon.UserError(cause, msg);
  }

  /** Call this to indicate a malformed declaration. */
  private static void decl_error(ParseState state, Throwable cause) {
    String msg = cause.getMessage() + state.line_file_message();
    if (msg.startsWith("null at")) {
      msg = msg.substring(5);
    }
    throw new Daikon.UserError(cause, msg);
  }

  /** Returns whether the line is the start of a ppt declaration. */
  @RequiresNonNull("FileIO.new_decl_format")
  @Pure
  private static boolean is_declaration_header(String line) {
    if (new_decl_format) {
      return (line.startsWith("ppt "));
    } else {
      return (line.equals(declaration_header));
    }
  }

  /**
   * Handle any possible modifications to the ppt name. For now, just support the Applications
   * Communities specific modification to remove duplicate stack entries. But a more generic
   * technique could be implemented in the future.
   */
  public static String user_mod_ppt_name(String ppt_name) {

    if (!dkconfig_rm_stack_dups) {
      return ppt_name;
    }

    // System.out.printf("removing stack dups (%b)in fileio%n",
    //                    dkconfig_rm_stack_dups);

    String[] stack = ppt_name.split("[|]");
    List<String> nd_stack = new ArrayList<>();
    for (String si : stack) {
      if (nd_stack.contains(si)) {
        continue;
      }
      nd_stack.add(si);
    }
    return String.join("|", nd_stack).intern();
  }
}
