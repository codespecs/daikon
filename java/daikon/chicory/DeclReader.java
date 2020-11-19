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

/**
 * Reads declaration files and provides methods to access the information within them. A declaration
 * file consists of a number of program points and the variables for each program point.
 */
public class DeclReader {

  /** map from ppt name to corresponding DeclPpt. */
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
     * @return string containing variable name
     */
    public String get_name() {
      return name;
    }

    /**
     * Returns the comparability string from the decl file.
     *
     * @return string containing comparability value
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
    /** program point name. */
    public String name;
    /** map from variable name to corresponding DeclVarInfo. */
    public HashMap<String, DeclVarInfo> vars = new LinkedHashMap<>();

    /** DeclPpt constructor. */
    public DeclPpt(String name) {
      this.name = name;
    }

    /**
     * Read a single variable declaration from decl_file. The file must be positioned immediately
     * before the variable name.
     *
     * @param decl_file EntryReader for reading data
     * @return DeclVarInfo for the program point variable
     * @throws IOException if there is trouble reading the file
     */
    public DeclVarInfo read_var(EntryReader decl_file) throws java.io.IOException {

      String name = null;
      String type = null;
      String rep_type = null;
      String comparability = null;
      Scanner scanner = new Scanner(decl_file.readLine());
      scanner.next(); // skip the "variable "
      name = scanner.next();

      // read variable data records until next variable or blank line
      String record = decl_file.readLine();
      while ((record != null) && (record.length() != 0)) {
        scanner = new Scanner(record);
        String token = scanner.next();
        if (token.equals("variable")) break;
        if (token.equals("dec-type")) {
          type = scanner.next();
        } else if (token.equals("rep-type")) {
          rep_type = scanner.next();
        } else if (token.equals("comparability")) {
          comparability = scanner.next();
        }
        // we ignore all other record types
        record = decl_file.readLine();
      }
      // push back the variable or blank line record
      decl_file.putback(record);

      if ((name == null) || (type == null) || (rep_type == null) || (comparability == null)) {
        throw new Error("File " + decl_file.getFileName() + " is invalid");
      }

      // I don't see the point of this interning.  No code seems to take
      // advantage of it.  Is it just for space?  -MDE
      DeclVarInfo var =
          new DeclVarInfo(
              name.intern(), type.intern(), rep_type.intern(), comparability.intern(), vars.size());
      vars.put(name, var);
      return var;
    }

    /**
     * Returns the DeclVarInfo named var_name or null if it doesn't exist.
     *
     * @param var_name the variable name
     * @return DeclVarInfo for the program point variable
     */
    public @Nullable DeclVarInfo find_var(String var_name) {
      return vars.get(var_name);
    }

    /**
     * Returns the ppt name.
     *
     * @return String containing the program point name
     */
    public String get_name() {
      return name;
    }

    /**
     * Returns the name without the :::EXIT, :::ENTER, etc.
     *
     * @return String containing the program point name
     */
    public String get_short_name() {
      return name.replaceFirst(":::.*", "");
    }

    /**
     * Returns the ppt name.
     *
     * @return String containing the program point name
     */
    @SideEffectFree
    @Override
    public String toString(@GuardSatisfied DeclPpt this) {
      return name;
    }
  }

  /** DeclReader constructor. */
  public DeclReader() {}

  /**
   * Read declarations from the specified pathname.
   *
   * @param pathname File for reading data
   * @throws IOException if there is trouble reading the file
   */
  public void read(File pathname) throws IOException {

    // have caller deal with FileNotFound
    EntryReader decl_file = new EntryReader(pathname, "^(//|#).*", null);

    try {
      for (String line = decl_file.readLine(); line != null; line = decl_file.readLine()) {
        // Skip all input until we find a ppt.
        if (!line.startsWith("ppt ")) {
          continue;
        }
        // Read the program point declaration.
        read_decl(decl_file, line);
      }
    } catch (Exception e) {
      throw new Error("Error reading comparability decl file", e);
    }
  }

  /**
   * Reads a single program point declaration from decl_file.
   *
   * @param decl_file EntryReader for reading data
   * @param record String containing the "ppt" record
   * @return DeclPpt for the program point
   * @throws IOException if there is trouble reading the file
   */
  protected DeclPpt read_decl(EntryReader decl_file, String record) throws IOException {

    // Read the name of the program point
    String pptname = record.substring(4); // skip "ppt "
    assert pptname.contains(":::");
    DeclPpt ppt = new DeclPpt(pptname);
    ppts.put(pptname, ppt);

    // Skip the ppt-type record.
    decl_file.readLine();

    // Read each of the variables in this program point.  The variables
    // are terminated by a blank line.
    String line = decl_file.readLine();

    while ((line != null) && (line.length() != 0)) {
      if (!line.startsWith("variable ")) {
        throw new Error("file " + decl_file.getFileName() + " is invalid");
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
}
