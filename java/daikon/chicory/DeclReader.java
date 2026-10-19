package daikon.chicory;

import daikon.Daikon;
import daikon.plumelib.util.EntryReader;
import daikon.plumelib.util.EntryReader.CommentFormat;
import daikon.plumelib.util.EntryReader.EntryFormat;
import daikon.plumelib.util.FilesPlume;
import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.io.UncheckedIOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Set;
import java.util.regex.Pattern;
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
 * <p>DeclReader skips everything other than program point declarations, so it can also read a
 * .dtrace file that contains declarations. Within a declaration, it parses only the "ppt",
 * "variable", "dec-type", "rep-type", and "comparability" records, which are the ones that its
 * clients need, and it skips the others. It ignores extra tokens at the end of a "variable" or
 * "comparability" record. A later declaration of a program point, of a variable, or of a
 * comparability replaces an earlier one. If the header record "var-comparability none" precedes a
 * declaration, then the comparabilities in that declaration are ignored.
 */
public class DeclReader {

  /** Map from ppt name to corresponding DeclPpt, in the order the ppts appear in the input. */
  public HashMap<String, DeclPpt> ppts = new LinkedHashMap<>();

  /** The keywords of the header records. */
  private static final Set<String> HEADER_KEYWORDS =
      new HashSet<>(
          Arrays.asList("decl-version", "var-comparability", "input-language", "ListImplementors"));

  /**
   * The keywords of the records that are permitted only within a variable. The keywords "parent"
   * and "flags" are omitted, because they are also permitted before the first variable.
   */
  private static final Set<String> VARIABLE_KEYWORDS =
      new HashSet<>(
          Arrays.asList(
              "var-kind",
              "enclosing-var",
              "reference-type",
              "array",
              "function-args",
              "rep-type",
              "dec-type",
              "lang-flags",
              "comparability",
              "constant",
              "min-value",
              "max-value",
              "min-length",
              "max-length",
              "valid-values"));

  /**
   * True if the most recent "var-comparability" header record in the file being read is
   * "var-comparability none".
   */
  private boolean comparabilityNone = false;

  /** Matches a run of whitespace. */
  private static final Pattern WHITESPACE = Pattern.compile("\\s+");

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
     * Creates a new DeclVarInfo.
     *
     * @param name the variable name
     * @param type the declared type
     * @param rep_type the representation type
     * @param comparability the comparability, or null
     * @param index the index of the variable within its program point
     */
    public DeclVarInfo(
        String name, String type, String rep_type, @Nullable String comparability, int index) {
      this.name = name;
      this.type = type;
      this.rep_type = rep_type;
      this.comparability = comparability;
      this.index = index;
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

    /** Map from variable name to corresponding DeclVarInfo, in declaration order. */
    public HashMap<String, DeclVarInfo> vars = new LinkedHashMap<>();

    /**
     * DeclPpt constructor.
     *
     * @param name program point name
     */
    public DeclPpt(String name) {
      this.name = name;
    }

    /**
     * Read a single variable declaration from decl_file. The file must be positioned immediately
     * before the variable name.
     *
     * @param decl_file where to read data from
     * @return DeclVarInfo for the program point variable
     * @throws IOException if there is trouble reading the file
     */
    public DeclVarInfo read_var(EntryReader decl_file) throws IOException {

      String firstLine = decl_file.readLine();
      if (firstLine == null) {
        reportFileError(decl_file, "Expected \"variable <VARNAME>\", found end of file");
      }
      String[] firstTokens = tokenize(firstLine);
      if (!(firstTokens[0].equals("variable") && firstTokens.length >= 2)) {
        reportFileError(decl_file, "Expected \"variable <VARNAME>\", found \"" + firstLine + "\"");
      }
      String varName = firstTokens[1];
      DeclVarInfo previous = vars.get(varName);
      // The location of the "variable" record.  The EntryReader cannot report a location after the
      // end of the input, where a missing record is detected.
      String varLocation = decl_file.getFileName() + " line " + decl_file.getLineNumber();

      String type = null;
      String rep_type = null;
      String comparability = null;

      // read variable data records until next variable or blank line
      String record = decl_file.readLine();
      while ((record != null) && !record.trim().isEmpty()) {
        String[] tokens = tokenize(record);
        String keyword = tokens[0];
        if (keyword.equals("variable")) {
          break;
        }
        if (startsBlock(keyword)) {
          reportFileError(
              decl_file, "Unexpected record \"" + record.trim() + "\" in variable " + varName);
        }
        if (keyword.equals("dec-type")) {
          if (tokens.length < 2) {
            reportFileError(decl_file, "\"dec-type\" not followed by a type");
          }
          type = recordValue(tokens);
        } else if (keyword.equals("rep-type")) {
          if (tokens.length < 2) {
            reportFileError(decl_file, "\"rep-type\" not followed by a type");
          }
          rep_type = recordValue(tokens);
        } else if (keyword.equals("comparability")) {
          if (tokens.length < 2) {
            reportFileError(decl_file, "Malformed comparability record \"" + record.trim() + "\"");
          }
          comparability = tokens[1];
        }
        // All other record types (such as flags and enclosing-var) are skipped, because no client
        // needs them.
        record = decl_file.readLine();
      }
      // push back the variable or blank line record
      if (record != null) {
        decl_file.putback(record);
      }

      if (type == null) {
        throw new Daikon.UserError("No dec-type for variable " + varName + " at " + varLocation);
      }
      if (rep_type == null) {
        throw new Daikon.UserError("No rep-type for variable " + varName + " at " + varLocation);
      }

      // I don't see the point of this interning.  No code seems to take
      // advantage of it.  Is it just for space?  -MDE
      DeclVarInfo var =
          new DeclVarInfo(
              varName.intern(),
              type.intern(),
              rep_type.intern(),
              (comparability == null) ? null : comparability.intern(),
              // A later declaration of a variable replaces an earlier one, in the same position.
              (previous == null) ? vars.size() : previous.index);
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

    /**
     * Returns the value of a record: its tokens other than the keyword, separated by single spaces.
     *
     * @param tokens the tokens of a record, as returned by {@link DeclReader#tokenize}
     * @return the value of the record
     */
    private static String recordValue(String[] tokens) {
      return String.join(" ", Arrays.asList(tokens).subList(1, tokens.length));
    }

    @SideEffectFree
    @Override
    public String toString(@GuardSatisfied DeclPpt this) {
      return name;
    }
  }

  /** Create a new DeclReader. */
  public DeclReader() {
    // This constructor is intentionally empty.
  }

  /**
   * Splits a record into whitespace-separated tokens.
   *
   * @param record a record, possibly with leading and trailing whitespace
   * @return the tokens of the record; a single empty string if the record is blank
   */
  private static String[] tokenize(String record) {
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
  private static String[] tokenize(String record, int limit) {
    return WHITESPACE.split(record.trim(), limit);
  }

  /**
   * Returns true if a record with the given keyword starts a declaration or a header record. Within
   * a declaration, such a record means that the blank line that ends the declaration is missing.
   *
   * @param keyword the first token of a record
   * @return true if a record with the given keyword starts a declaration or a header record
   */
  private static boolean startsBlock(String keyword) {
    return keyword.equals("ppt") || HEADER_KEYWORDS.contains(keyword);
  }

  /**
   * Returns the first whitespace-separated token of a record. This is cheaper than {@link
   * #tokenize}.
   *
   * @param record a record, possibly with leading and trailing whitespace
   * @return the first token of the record; the empty string if the record is blank
   */
  private static String firstToken(String record) {
    int length = record.length();
    int start = 0;
    while (start < length && Character.isWhitespace(record.charAt(start))) {
      start++;
    }
    int end = start;
    while (end < length && !Character.isWhitespace(record.charAt(end))) {
      end++;
    }
    return record.substring(start, end);
  }

  /**
   * Read declarations from the specified pathname.
   *
   * @param pathname a File for reading data
   * @throws IOException if there is trouble reading the file
   */
  public void read(File pathname) throws IOException {
    try (Reader reader = FilesPlume.newFileReader(pathname)) {
      read(reader, pathname.toString());
    }
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
      comparabilityNone = false;
      for (String line = decl_file.readLine(); line != null; line = decl_file.readLine()) {
        // Most lines of a .dtrace file are skipped, so avoid tokenizing each line.  In a .dtrace
        // file, a line that is exactly "ppt" is the name of a variable in a sample record.
        String keyword = firstToken(line);
        if (keyword.equals("ppt") && !line.trim().equals("ppt")) {
          decl_file.putback(line);
          read_decl(decl_file);
        } else if (keyword.equals("var-comparability")) {
          String[] tokens = tokenize(line);
          comparabilityNone = tokens.length >= 2 && tokens[1].equals("none");
        }
        // Skip any other record, such as a header record or a sample record in a .dtrace file.
      }
    } catch (UncheckedIOException e) {
      throw e.getCause();
    } catch (Daikon.UserError e) {
      throw e;
    } catch (RuntimeException e) {
      throw new Error("Error reading declaration file " + filename, e);
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
    DeclPpt ppt = new DeclPpt(pptname);
    ppts.put(pptname, ppt);

    // Skip the records, such as ppt-type, that precede the first variable.  No client needs them.
    String line = decl_file.readLine();
    while (line != null) {
      String keyword = firstToken(line);
      if (keyword.isEmpty() || keyword.equals("variable")) {
        break;
      }
      if (startsBlock(keyword) || VARIABLE_KEYWORDS.contains(keyword)) {
        reportFileError(
            decl_file,
            "Expected \"variable <VARNAME>\" or a blank line, found \"" + line.trim() + "\"");
      }
      line = decl_file.readLine();
    }

    // Read each of the variables in this program point.  The variables
    // are terminated by a blank line.
    while ((line != null) && !line.trim().isEmpty()) {
      decl_file.putback(line);
      DeclVarInfo var = ppt.read_var(decl_file);
      if (comparabilityNone) {
        var.comparability = null;
      }
      line = decl_file.readLine();
    }

    return ppt;
  }

  /**
   * Fetches program point declaration for the ppt_name argument.
   *
   * <p>This can return null. Example: when DynComp is run to compute comparability information, it
   * produces no information (not even a declaration) for the program points of classes that are
   * never loaded. But, Chicory outputs a declaration for every program point, and this lookup can
   * fail when using the --comparability-file=... command-line argument with a file produced by
   * DynComp.
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
