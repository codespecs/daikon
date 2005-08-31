package daikon.chicory;

import java.util.*;
import java.io.*;

/**
 * Reads declaration files and provides methods to access the information
 * within them.  A declaration file consists of a number of program points
 * and the variables for each program point
 */
public class DeclReader {

  HashMap<String,DeclPpt> ppts = new LinkedHashMap<String,DeclPpt>();

  /**
   * Information about variables within a program point
   */
  public static class VarInfo {
    String name;
    String type;
    String rep_type;
    String comparability;

    public VarInfo (String name, String type, String rep_type,
                    String comparability) {
      this.name = name;
      this.type = type;
      this.rep_type = rep_type;
      this.comparability = comparability;
    }

    /** Returns the variables name */
    public String get_name() {
      return name;
    }

    /** Returns the variables declared type as specified in the decl file **/
    public String get_type() {
      return type;
    }

    /**
     * Returns the representation type of the variable as specified in
     * the decl file
     */
    public String get_rep_type() {
      return rep_type;
    }

    /** Returns the comparability string from the decl file **/
    public String get_comparability() {
      return comparability;
    }
  }

  /**
   * Information about the program point that is contained in the decl
   * file.  This consists of the ppt name and a list of the declared
   * variables
   */
  public static class DeclPpt {
    String name;
    HashMap<String,VarInfo> vars = new LinkedHashMap<String,VarInfo>();

    public DeclPpt (String name) {
      this.name = name;
    }

    /**
     * Read a single variable declaration from decl_file.  The file
     * must be positioned immediately before the variable name
     */
    public VarInfo read_var (BufferedReader decl_file)
      throws java.io.IOException{

      String name = decl_file.readLine();
      String type = decl_file.readLine();
      String rep_type = decl_file.readLine();
      String comparability = decl_file.readLine();

      VarInfo var = new VarInfo (name, type, rep_type, comparability);
      vars.put (name, var);
      return (var);
    }

    /**
     * Returns the VarInfo named var_name or null if it doesn't exist
     */
    public VarInfo find_var (String var_name) {
      return vars.get (var_name);
    }

    /** Returns the ppt name **/
    public String get_name() {
      return name;
    }
  }

  public DeclReader() {
  }

  /**
   * Read declarations from the specified pathname
   */
  public void read (File pathname) {
    try {

      BufferedReader decl_file = new BufferedReader(new FileReader(pathname));

      for (String line = decl_file.readLine(); line != null;
           line = decl_file.readLine()) {
        if (!line.equals ("DECLARE"))
          continue;

        // Read the name of the program point
        String pptname = decl_file.readLine();
        DeclPpt ppt = new DeclPpt (pptname);
        ppts.put (pptname, ppt);

        // Read each of the variables in this program point.  The variables
        // are terminated by a blank line.
        decl_file.mark (4000);
        line = decl_file.readLine();
        while ((line != null) && (line.length() != 0)) {
          decl_file.reset();
          ppt.read_var (decl_file);
          decl_file.mark (4000);
          line = decl_file.readLine();
        }
      }
    } catch (Exception e) {
      throw new Error ("Error reading comparability decl file", e);
    }
  }

  public void dump() {

    for (String ppt_name : ppts.keySet()) {
      System.out.printf ("Comp Ppt: %s%n", ppt_name);
      DeclPpt ppt = ppts.get (ppt_name);
      for (String var_name : ppt.vars.keySet()) {
        System.out.printf ("  var %s%n", var_name);
      }
    }
  }

  public DeclPpt find_ppt (String ppt_name) {
    return ppts.get (ppt_name);
  }
}
