package daikon.chicory;

import daikon.Daikon;
import daikon.plumelib.util.EntryReader;
import daikon.plumelib.util.EntryReader.CommentFormat;
import daikon.plumelib.util.EntryReader.EntryFormat;
import daikon.plumelib.util.FilesPlume;
import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Scanner;
import java.util.Set;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.SideEffectFree;
import org.checkerframework.dataflow.qual.TerminatesExecution;

/**
 * Reads declaration files and provides methods to access the information within them. A declaration
 * file consists of header records, such as "decl-version 2.0", followed by a number of program
 * points and the variables for each program point. Only version 2.0 declaration files are
 * supported.
 *
 * <p>DeclReader parses only the records that its clients need. A DeclReader that is created for
 * rewriting also retains the text of every declaration, so a client can write a declaration with
 * some of its records changed.
 */
public class DeclReader {

  /**
   * If true, this reader retains the text of every declaration, and it rejects input that cannot be
   * faithfully rewritten: a file that is not a version 2.0 declaration file, records (such as
   * sample records in a .dtrace file) that are neither header records nor declarations, a header
   * record that appears more than once, a program point that is declared more than once, a variable
   * that is declared more than once in a program point, and a variable with more than one
   * comparability record.
   *
   * <p>If false, this reader skips everything other than program point declarations, and a later
   * declaration of a program point, of a variable, or of a comparability replaces an earlier one.
   */
  private final boolean forRewriting;

  /** Map from ppt name to corresponding DeclPpt, in the order the ppts appear in the input. */
  public HashMap<String, DeclPpt> ppts = new LinkedHashMap<>();

  /**
   * The header records, such as "decl-version 2.0", in the order they appear in the input. In each
   * record, the tokens are separated by a single space; comments and blank lines are omitted.
   */
  public List<String> header = new ArrayList<>();

  /** The keywords of the header records. */
  private static final Set<String> HEADER_KEYWORDS =
      new HashSet<>(Arrays.asList("decl-version", "var-comparability", "input-language"));

  /**
   * The keywords of the records that may appear in a program point declaration before its first
   * variable. This must be kept in sync with {@code FileIO.read_ppt_decl}.
   */
  private static final Set<String> PPT_KEYWORDS =
      new HashSet<>(Arrays.asList("parent", "flags", "ppt-type"));

  /**
   * The keywords of the records that may appear in a variable declaration after its "variable"
   * record. This must be kept in sync with {@code FileIO.read_ppt_decl}.
   */
  private static final Set<String> VAR_KEYWORDS =
      new HashSet<>(
          Arrays.asList(
              "var-kind",
              "enclosing-var",
              "reference-type",
              "array",
              "function-args",
              "rep-type",
              "dec-type",
              "flags",
              "lang-flags",
              "parent",
              "comparability",
              "constant",
              "min-value",
              "max-value",
              "min-length",
              "max-length",
              "valid-values"));

  /** Information about variables within a program point. */
  public static class DeclVarInfo {
    public String name;
    public String type;
    public String rep_type;

    /**
     * The comparability, such as "3" or "3[4]"; null if there is no comparability record, which
     * means that the variable is comparable to every other variable.
     */
    public @Nullable String comparability;

    public int index;

    /**
     * The lines of the declaration, starting with the "variable" line; empty if the DeclReader does
     * not retain the text of declarations.
     */
    public List<String> lines;

    /** The index in {@link #lines} of the comparability record, or -1 if there is none. */
    public int comparabilityLine;

    /**
     * Creates a new DeclVarInfo.
     *
     * @param name the variable name
     * @param type the declared type
     * @param rep_type the representation type
     * @param comparability the comparability, or null
     * @param index the index of the variable within its program point
     * @param lines the lines of the declaration
     * @param comparabilityLine the index in {@code lines} of the comparability record, or -1
     */
    public DeclVarInfo(
        String name,
        String type,
        String rep_type,
        @Nullable String comparability,
        int index,
        List<String> lines,
        int comparabilityLine) {
      this.name = name;
      this.type = type;
      this.rep_type = rep_type;
      this.comparability = comparability;
      this.index = index;
      this.lines = lines;
      this.comparabilityLine = comparabilityLine;
    }

    /**
     * Returns the variable's name.
     *
     * @return the variable's name
     */
    public String get_name() {
      return name;
    }

    /**
     * Returns the comparability value from the decl file.
     *
     * @return the comparability value, or null if there is none
     */
    public @Nullable String get_comparability() {
      return comparability;
    }

    @SideEffectFree
    @Override
    public String toString(@GuardSatisfied DeclVarInfo this) {
      return String.format("%s [%s] %s", type, rep_type, name);
    }
  }

  /**
   * Information about the program point that is contained in the decl file. This consists of the
   * ppt name and a list of the declared variables.
   */
  public static class DeclPpt {
    /** Program point name. */
    public String name;

    /** The file in which this declaration appears. */
    public String filename;

    /**
     * The lines of the declaration that precede the first variable, starting with the "ppt" line;
     * empty if the DeclReader does not retain the text of declarations.
     */
    public List<String> declHeaderLines = new ArrayList<>();

    /** Map from variable name to corresponding DeclVarInfo, in declaration order. */
    public HashMap<String, DeclVarInfo> vars = new LinkedHashMap<>();

    /**
     * DeclPpt constructor.
     *
     * @param name program point name
     * @param filename the file in which this declaration appears
     */
    public DeclPpt(String name, String filename) {
      this.name = name;
      this.filename = filename;
    }

    /**
     * Read a single variable declaration from decl_file. The file must be positioned immediately
     * before the variable name.
     *
     * @param decl_file where to read data from
     * @param forRewriting if true, retain the text of the declaration and reject input that cannot
     *     be faithfully rewritten; see {@link DeclReader#forRewriting}
     * @return DeclVarInfo for the program point variable
     * @throws IOException if there is trouble reading the file
     */
    public DeclVarInfo read_var(EntryReader decl_file, boolean forRewriting) throws IOException {

      String firstLine = decl_file.readLine();
      if (firstLine == null) {
        reportFileError(decl_file, "Expected \"variable <VARNAME>\", found end of file");
      }
      Scanner scanner = new Scanner(firstLine);
      if (!(scanner.hasNext() && scanner.next().equals("variable") && scanner.hasNext())) {
        reportFileError(decl_file, "Expected \"variable <VARNAME>\", found \"" + firstLine + "\"");
      }
      String varName = scanner.next();
      if (forRewriting && vars.containsKey(varName)) {
        reportFileError(decl_file, "Variable " + varName + " declared twice in ppt " + name);
      }

      // Avoid allocating a list per variable when the lines are not retained.
      List<String> lines = forRewriting ? new ArrayList<>() : Collections.emptyList();
      if (forRewriting) {
        lines.add(firstLine);
      }
      String type = null;
      String rep_type = null;
      String comparability = null;
      int comparabilityLine = -1;

      // read variable data records until next variable or blank line
      String record = decl_file.readLine();
      while ((record != null) && !record.trim().isEmpty()) {
        String[] tokens = tokenize(record);
        String keyword = tokens[0];
        if (keyword.equals("variable")) {
          break;
        }
        if (!VAR_KEYWORDS.contains(keyword)) {
          reportFileError(
              decl_file, "Unexpected record \"" + record.trim() + "\" in variable " + varName);
        }
        if (forRewriting) {
          lines.add(record);
        }
        // The record without its keyword, with each run of whitespace replaced by a single space.
        String value = String.join(" ", Arrays.asList(tokens).subList(1, tokens.length));
        if (keyword.equals("dec-type")) {
          if (value.isEmpty()) {
            reportFileError(decl_file, "\"dec-type\" not followed by a type");
          }
          type = value;
        } else if (keyword.equals("rep-type")) {
          if (value.isEmpty()) {
            reportFileError(decl_file, "\"rep-type\" not followed by a type");
          }
          rep_type = value;
        } else if (keyword.equals("comparability")) {
          if (forRewriting && comparability != null) {
            reportFileError(decl_file, "Multiple comparability records for variable " + varName);
          }
          if (tokens.length != 2) {
            reportFileError(decl_file, "Malformed comparability record \"" + record.trim() + "\"");
          }
          comparability = tokens[1];
          if (forRewriting) {
            comparabilityLine = lines.size() - 1;
          }
        }
        // All other record types (such as flags and enclosing-var) are not parsed, because no
        // client needs them.
        record = decl_file.readLine();
      }
      // push back the variable or blank line record
      if (record != null) {
        decl_file.putback(record);
      }

      if (type == null) {
        reportFileError(decl_file, "No type for variable " + varName);
      }
      if (rep_type == null) {
        reportFileError(decl_file, "No rep-type for variable " + varName);
      }

      // I don't see the point of this interning.  No code seems to take
      // advantage of it.  Is it just for space?  -MDE
      DeclVarInfo var =
          new DeclVarInfo(
              varName.intern(),
              type.intern(),
              rep_type.intern(),
              (comparability == null) ? null : comparability.intern(),
              vars.size(),
              lines,
              comparabilityLine);
      vars.put(varName, var);
      return var;
    }

    /**
     * Returns the DeclVarInfo named var_name or null if it doesn't exist.
     *
     * @param var_name a variable name
     * @return DeclVarInfo for the given variable
     */
    public @Nullable DeclVarInfo find_var(String var_name) {
      return vars.get(var_name);
    }

    /**
     * Returns the ppt name.
     *
     * @return the program point name
     */
    public String get_name() {
      return name;
    }

    /**
     * Returns the name without the :::EXIT, :::ENTER, etc.
     *
     * @return the program point name
     */
    public String get_short_name() {
      return name.replaceFirst(":::.*", "");
    }

    @SideEffectFree
    @Override
    public String toString(@GuardSatisfied DeclPpt this) {
      return name;
    }
  }

  /**
   * Create a new DeclReader that does not retain the text of declarations and that skips everything
   * other than program point declarations.
   */
  public DeclReader() {
    this(false);
  }

  /**
   * Create a new DeclReader.
   *
   * @param forRewriting if true, retain the text of every declaration and reject input that cannot
   *     be faithfully rewritten; see {@link #forRewriting}
   */
  public DeclReader(boolean forRewriting) {
    this.forRewriting = forRewriting;
  }

  /**
   * Splits a record into whitespace-separated tokens.
   *
   * @param record a record, possibly with leading and trailing whitespace
   * @return the tokens of the record; a single empty string if the record is blank
   */
  public static String[] tokenize(String record) {
    return tokenize(record, 0);
  }

  /**
   * Splits a record into at most {@code limit} whitespace-separated tokens. If the record has more
   * tokens, the last token is the rest of the record, including any whitespace within it.
   *
   * @param record a record, possibly with leading and trailing whitespace
   * @param limit the maximum number of tokens, or 0 for no limit
   * @return the tokens of the record; a single empty string if the record is blank
   */
  public static String[] tokenize(String record, int limit) {
    return record.trim().split("\\s+", limit);
  }

  /**
   * Read declarations from the specified pathname.
   *
   * @param pathname a File for reading data
   * @throws IOException if there is trouble reading the file
   */
  public void read(File pathname) throws IOException {
    read(FilesPlume.newFileReader(pathname), pathname.toString());
  }

  /**
   * Read declarations from the specified reader, which is closed afterward.
   *
   * @param reader where to read data from
   * @param filename the name of the file being read, used in error messages
   * @throws IOException if there is trouble reading the file
   */
  public void read(Reader reader, String filename) throws IOException {
    try (EntryReader decl_file =
        new EntryReader(
            reader, filename, EntryFormat.DEFAULT, new CommentFormat("^(//|#).*"), null)) {
      boolean seenVersion2 = false;
      // The keywords of the header records read so far.
      Set<String> seenHeaderKeywords = new HashSet<>();
      for (String line = decl_file.readLine(); line != null; line = decl_file.readLine()) {
        String[] tokens = tokenize(line);
        String keyword = tokens[0];
        if (keyword.isEmpty()) {
          continue;
        }
        // In a .dtrace file, a line that is exactly "ppt" is the name of a variable in a sample
        // record.  A rewriting reader rejects it in read_decl.
        if (keyword.equals("ppt") && (tokens.length > 1 || forRewriting)) {
          if (forRewriting && !seenVersion2) {
            reportFileError(decl_file, "Program point declaration precedes \"decl-version 2.0\"");
          }
          decl_file.putback(line);
          read_decl(decl_file);
          continue;
        }
        String record = String.join(" ", tokens);
        if (HEADER_KEYWORDS.contains(keyword)) {
          if (forRewriting && !seenHeaderKeywords.add(keyword)) {
            reportFileError(decl_file, "Multiple \"" + keyword + "\" records");
          }
          if (keyword.equals("decl-version")) {
            if (forRewriting && !record.equals("decl-version 2.0")) {
              reportFileError(decl_file, "Unsupported \"" + record + "\"");
            }
            seenVersion2 = true;
          }
          header.add(record);
        } else if (forRewriting) {
          if (record.equals("DECLARE")) {
            reportFileError(decl_file, "Only version 2.0 declaration files are supported");
          }
          if (!seenVersion2) {
            reportFileError(decl_file, "Expected \"decl-version 2.0\", found \"" + record + "\"");
          }
          // For example, a sample record in a .dtrace file.
          reportFileError(decl_file, "Expected a declaration, found \"" + record + "\"");
        }
        // Otherwise, skip the record.  For example, it is a sample record in a .dtrace file.
      }
      if (forRewriting && !seenVersion2) {
        // For example, the file is empty.
        reportFileError(decl_file, "No \"decl-version 2.0\" record");
      }
    }
  }

  /**
   * Reads a single program point declaration from decl_file.
   *
   * @param decl_file EntryReader for reading data
   * @return the program point declaration
   * @throws IOException if there is trouble reading the file
   */
  protected DeclPpt read_decl(EntryReader decl_file) throws IOException {

    // Read the name of the program point
    String firstLine = decl_file.readLine();
    if (firstLine == null) {
      reportFileError(decl_file, "File ends prematurely, expected \"ppt ...\"");
    }
    String[] tokens = tokenize(firstLine, 2);
    if (tokens.length != 2 || !tokens[0].equals("ppt")) {
      reportFileError(decl_file, "Expected \"ppt <PPTNAME>\", found \"" + firstLine + "\"");
    }
    String pptname = tokens[1];
    if (!pptname.contains(":::")) {
      reportFileError(decl_file, "Program point name \"" + pptname + "\" does not contain \":::\"");
    }
    if (forRewriting && ppts.containsKey(pptname)) {
      reportFileError(decl_file, "Program point " + pptname + " declared twice");
    }
    DeclPpt ppt = new DeclPpt(pptname, decl_file.getFileName());
    ppts.put(pptname, ppt);
    if (forRewriting) {
      ppt.declHeaderLines.add(firstLine);
    }

    // Read the records, such as ppt-type, that precede the first variable.
    String line = decl_file.readLine();
    while (line != null) {
      String keyword = tokenize(line)[0];
      if (keyword.isEmpty() || keyword.equals("variable")) {
        break;
      }
      if (!PPT_KEYWORDS.contains(keyword)) {
        // For example, a "ppt" record because the blank line that ends a declaration is missing.
        reportFileError(
            decl_file,
            "Expected \"variable <VARNAME>\" or a blank line, found \"" + line.trim() + "\"");
      }
      if (forRewriting) {
        ppt.declHeaderLines.add(line);
      }
      line = decl_file.readLine();
    }

    // Read each of the variables in this program point.  The variables
    // are terminated by a blank line.
    while ((line != null) && !line.trim().isEmpty()) {
      decl_file.putback(line);
      ppt.read_var(decl_file, forRewriting);
      line = decl_file.readLine();
    }

    return ppt;
  }

  /**
   * Fetches program point declaration for the ppt_name argument.
   *
   * <p>This can return null. Example: when DynComp is run to compute comparability information, it
   * produces no information (not even a declaration) for program points that are never executed.
   * But, Chicory outputs a declaration for every program point, and this lookup can fail when using
   * the --comparability-file=... command-line argument with a file produced by DynComp.
   *
   * @param ppt_name name of Ppt to fetch
   * @return the program point declaration
   */
  public @Nullable DeclPpt find_ppt(String ppt_name) {
    return ppts.get(ppt_name);
  }

  /**
   * Report an error while reading from an EntryReader, with file name and line number.
   *
   * @param er an EntryReader, from which file name and line number are obtained
   * @param message the error message
   */
  @TerminatesExecution
  private static void reportFileError(EntryReader er, String message) {
    throw new Daikon.UserError(message + " at " + er.getFileName() + " line " + er.getLineNumber());
  }
}
