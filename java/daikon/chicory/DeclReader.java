package daikon.chicory;

import daikon.plumelib.util.EntryReader;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Scanner;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.SideEffectFree;
import org.checkerframework.dataflow.qual.TerminatesExecution;

/**
 * Reads declaration files and provides methods to access the information within them. A declaration
 * file consists of a number of program points and the variables for each program point.
 */
public class DeclReader {

  /** Map from ppt name to corresponding DeclPpt. */
  public HashMap<String, DeclPpt> ppts = new LinkedHashMap<>();

  /** Information about variables within a program point. */
  public static class DeclVarInfo {
    public String name;
    public String type;
    public String rep_type;
    public String comparability;
    public int index;

    public DeclVarInfo(String name, String type, String rep_type, String comparability, int index) {
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
     * @return the comparability value
     */
    public String get_comparability() {
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

    /** Map from variable name to corresponding DeclVarInfo. */
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
    public DeclVarInfo read_var(EntryReader decl_file) throws java.io.IOException {

      String firstLine = decl_file.readLine();
      if (firstLine == null) {
        reportFileError(decl_file, "Expected \"variable <VARNAME>\", found end of file");
      }
      Scanner scanner = new Scanner(firstLine);
      if (!(scanner.hasNext() && scanner.next().equals("variable") && scanner.hasNext())) {
        reportFileError(decl_file, "Expected \"variable <VARNAME>\", found \"" + firstLine + "\"");
      }
      String varName = scanner.next();

      String type = null;
      String rep_type = null;
      String comparability = null;

      // read variable data records until next variable or blank line
      String record = decl_file.readLine();
      while ((record != null) && (record.length() != 0)) {
        scanner = new Scanner(record);
        String token = scanner.next();
        if (token.equals("variable")) {
          break;
        } else if (token.equals("dec-type")) {
          if (!scanner.hasNext()) {
            reportFileError(decl_file, "\"dec-type\" not followed by a type");
          }
          type = scanner.next();
        } else if (token.equals("rep-type")) {
          if (!scanner.hasNext()) {
            reportFileError(decl_file, "\"rep-type\" not followed by a type");
          }
          rep_type = scanner.next();
        } else if (token.equals("comparability")) {
          if (!scanner.hasNext()) {
            reportFileError(decl_file, "\"comparability\" not followed by a comparability-type");
          }
          comparability = scanner.next();
        }
        // Chicory ignores all other record types (such as flags and enclosing-var) as they are not
        // needed to calculate comparability values.
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
      if (comparability == null) {
        reportFileError(decl_file, "No comparability for variable " + varName);
      }

      // I don't see the point of this interning.  No code seems to take
      // advantage of it.  Is it just for space?  -MDE
      DeclVarInfo var =
          new DeclVarInfo(
              varName.intern(),
              type.intern(),
              rep_type.intern(),
              comparability.intern(),
              vars.size());
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

  /** Create a new DeclReader. */
  public DeclReader() {}

  /**
   * Read declarations from the specified pathname.
   *
   * @param pathname a File for reading data
   * @throws IOException if there is trouble reading the file
   */
  public void read(File pathname) throws IOException {

    // have caller deal with FileNotFound

    try (EntryReader decl_file = new EntryReader(pathname, "^(//|#).*", null)) {
      for (String line = decl_file.readLine(); line != null; line = decl_file.readLine()) {
        // Skip all input until we find a ppt.
        if (!line.startsWith("ppt ")) {
          continue;
        }
        decl_file.putback(line);

        // Read the program point declaration.
        read_decl(decl_file);
      }
    } catch (Exception e) {
      throw new Error("Error reading comparability decl file " + pathname, e);
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
    if (!firstLine.startsWith("ppt ")) {
      reportFileError(decl_file, "Expected \"ppt ...\", found \"" + firstLine + "\"");
    }
    String pptname = firstLine.substring(4); // skip "ppt "
    assert pptname.contains(":::");
    DeclPpt ppt = new DeclPpt(pptname);
    ppts.put(pptname, ppt);

    // Chicory skips the ppt-type record as it is not needed to calculate comparability values.
    String ppt_type = decl_file.readLine();
    if (ppt_type == null) {
      reportFileError(decl_file, "File terminated prematurely while reading decl for " + ppt);
    }

    // Read each of the variables in this program point.  The variables
    // are terminated by a blank line.
    String line = decl_file.readLine();
    while ((line != null) && (line.length() != 0)) {
      if (!line.startsWith("variable ")) {
        reportFileError(decl_file, "Expected \"variable ...\", found \"" + line + "\"");
      }
      decl_file.putback(line);
      ppt.read_var(decl_file);
      line = decl_file.readLine();
    }

    return ppt;
  }

  // This can return null.  Example:  when DynComp is run to compute
  // comparability information, it produces no information (not even a
  // declaration) for program points that are never executed.  But, Chicory
  // outputs a declaration for every program point, and this lookup can
  // fail when using the --comparability-file=... command-line argument
  // with a file produced by DynComp.
  public @Nullable DeclPpt find_ppt(String ppt_name) {
    DeclPpt result = ppts.get(ppt_name);
    return result;
  }

  /**
   * Report an error while reading from an EntryReader, with file name and line number.
   *
   * @param er an EntryReader, from which file name and line number are obtained
   * @param message the error message
   */
  @TerminatesExecution
  private static void reportFileError(EntryReader er, String message) {
    throw new Error(message + " at " + er.getFileName() + " line " + er.getLineNumber());
  }
}
