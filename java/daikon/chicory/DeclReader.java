package daikon.chicory;

import java.util.*;
import java.io.*;

import utilMDE.*;

/**
 * Reads declaration files and provides methods to access the information
 * within them.  A declaration file consists of a number of program points
 * and the variables for each program point
 */
public class DeclReader {

  /**
   * Prints only the average set size for each specified file
   */
  @Option("Print average set size (only) for each specified file")
  public static boolean avg_size = false;

  /**
   * Reads in a decl file with arbitrary comparability and writes
   * out a file with comparability based on the primitive declaration
   * types.  All hashcodes are left in a single set.  Each named primitive
   * type has a distinct set.
   */
  @Option("Output a decl file with declaration comparability")
  public static boolean declaration_type_comparability = false;

  /**
   * Reads in a decl file with arbitrary comparability and writes
   * out a file with comparability based on the rep types (ie, there
   * are comparability sets for int, boolean, string, and hashcode)
   * Two filenames are required:  input-filename output-filename.
   */
  @Option("Output a decl file with representation type comparability")
  public static boolean rep_type_comparability = false;

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
     * Returns the type name.  get_type() returns the entire entry including
     * auxiliary information
     */
    public String get_type_name() {
      return type.replaceFirst (" .*", "");
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

    public String get_basic_comparability() {
      return comparability.replaceFirst ("\\[.*\\]", "");
    }

    public void set_comparability (String comparability) {
      this.comparability = comparability;
    }

    public String toString() {
      return name;
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

      String name = decl_file.readLine().intern();
      String type = decl_file.readLine().intern();
      String rep_type = decl_file.readLine().intern();
      String comparability = decl_file.readLine().intern();

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
        assert pptname.contains (":::");
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

  /**
   * Reads a decl file and dumps statistics
   */
  public static void main (String[] args) throws IOException {

    Options options = new Options ("DeclReader [options] decl-files...",
                                   DeclReader.class);
    String[] files = options.parse_and_usage (args);
    boolean print_each_set = !avg_size;

    // If determining declaration type comparability, setup the comparability
    // base on the declared type of primitives and write out the result
    if (declaration_type_comparability) {
      DeclReader dr = new DeclReader();
      dr.read (new File (files[0]));
      dr.primitive_declaration_types();
      dr.write_decl (files[1], "implicit");
      return;
    }

    // If determining representation type comparability, setup comparability
    // on the rep type of each variable and write out the result.
    if (rep_type_comparability) {
      DeclReader dr = new DeclReader();
      dr.read (new File (files[0]));
      dr.rep_types();
      dr.write_decl (files[1], "implicit");
      return;
    }

    for (String filename : files) {
      DeclReader dr = new DeclReader();
      dr.read (new File (filename));

      int num_sets = 0;
      int total_set_size = 0;

      // Loop through each ppt
      for (DeclPpt ppt : dr.ppts.values()) {
        if (print_each_set)
          System.out.printf ("ppt %s%n", ppt.name);

        // Build a map from comparabilty to all of the variables with that comp
        Map<String,List<VarInfo>> comp_map
          = new LinkedHashMap<String,List<VarInfo>>();
        for (VarInfo vi : ppt.vars.values()) {
          String comp = vi.get_basic_comparability();
          if (!comp_map.containsKey (comp)) {
            comp_map.put (comp, new ArrayList<VarInfo>());
          }
          List<VarInfo> vi_list = comp_map.get (comp);
          vi_list.add (vi);
        }

        // Print out the variables with each comparability
        for (List<VarInfo> vi_list : comp_map.values()) {
          num_sets++;
          total_set_size += vi_list.size();
          if (print_each_set) {
            System.out.printf ("%-5s : [%d] %s%n",
                               vi_list.get(0).get_basic_comparability(),
                               vi_list.size(), vi_list);
          }
        }
      }

      if (avg_size) {
        System.out.printf ("%-35s %,6d sets of average size %f found%n",
                           filename, num_sets,
                           ((double)total_set_size) / num_sets);
      }
    }
  }

  /**
   * Sets the comparability to match primitive declaration types.
   * The comparability for each non-hashcode is set so that each
   * declaration type is in a separate set.  Hashcodes are all set
   * to a single comparability regardless of their declared type
   */
  public void primitive_declaration_types () {

    Map<String,Integer> type_comp = new LinkedHashMap<String,Integer>();
    int next_comparability = 1;

    // Loop through each program point
    for (DeclPpt ppt : ppts.values()) {

      // Loop through each variable
      for (VarInfo vi : ppt.vars.values()) {

        // Determine the comparabilty for this declared type.  Hashcodes
        // are not included because hashcodes of different declared types
        // can still be sensibly compared (eg, subclasses/superclasses)
        String declared_type;
        if (vi.get_rep_type().startsWith ("hashcode"))
          declared_type = "hashcode";
        else
          declared_type = vi.get_type_name();
        Integer comparability = type_comp.get (declared_type);
        if (comparability == null) {
          comparability = new Integer (next_comparability);
          next_comparability++;
          type_comp.put (declared_type, comparability);
          System.out.printf ("declared type %s has comparability %d%n",
                             declared_type, comparability);
        }
        vi.set_comparability (comparability.toString());
      }
    }
  }

  /**
   * Sets the comparability to match the rep types.
   */
  public void rep_types () {

    Map<String,Integer> type_comp = new LinkedHashMap<String,Integer>();
    int next_comparability = 1;

    // Loop through each program point
    for (DeclPpt ppt : ppts.values()) {

      // Loop through each variable
      for (VarInfo vi : ppt.vars.values()) {

        // Determine the comparabilty for this declared type.  Hashcodes
        // are not included because hashcodes of different declared types
        // can still be sensibly compared (eg, subclasses/superclasses)
        String declared_type = vi.get_rep_type();
        Integer comparability = type_comp.get (declared_type);
        if (comparability == null) {
          comparability = new Integer (next_comparability);
          next_comparability++;
          type_comp.put (declared_type, comparability);
          System.out.printf ("declared type %s has comparability %d%n",
                             declared_type, comparability);
        }
        vi.set_comparability (comparability.toString());
      }
    }
  }

  /**
   * Writes the declaration to the specified file.  If the filename
   * is -, writes to stdout.
   */
  public void write_decl (String filename, String comparability)
    throws IOException{

    // Get the output stream
    PrintStream decl_file;
    if (filename.equals ("-"))
      decl_file = System.out;
    else {
      decl_file = new PrintStream (filename);
    }

    decl_file.printf ("// Declaration file written by DeclReader%n%n");
    decl_file.printf ("VarComparability%n%s%n", comparability);

    // Loop through each program point
    for (DeclPpt ppt : ppts.values()) {

      decl_file.printf ("%nDECLARE%n%s%n", ppt.name);

      // Loop through each variable
      for (VarInfo vi : ppt.vars.values()) {
        decl_file.printf ("%s%n%s%n%s%n%s%n", vi.get_name(), vi.get_type(),
                          vi.get_rep_type(), vi.get_comparability());
      }
    }

    decl_file.close();
  }



}
