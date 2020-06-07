package daikon.chicory;

import daikon.plumelib.options.Option;
import daikon.plumelib.options.Options;
import daikon.plumelib.util.EntryReader;
import daikon.plumelib.util.Intern;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.checkerframework.checker.interning.qual.Interned;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;

/**
 * Reads declaration files and provides methods to access the information within them. A declaration
 * file consists of a number of program points and the variables for each program point.
 */
public class DeclReader {

  /** Prints only the average set size for each specified file. */
  @Option("Print average set size (only) for each specified file")
  public static boolean avg_size = false;

  /**
   * Reads in a decl file with arbitrary comparability and writes out a file with comparability
   * based on the primitive declaration types. All hashcodes are left in a single set. Each named
   * primitive type has a distinct set.
   */
  @Option("Output a decl file with primitive declaration comparability")
  public static boolean primitive_declaration_type_comparability = false;

  /**
   * Reads in a decl file with arbitrary comparability and writes out a file with comparability
   * based on declaration types. Each variable with a different typename will be in a different set.
   * For example, java.util.List, java.util.ArrayList, float, short, double, and int are each in a
   * distinct set.
   */
  @Option("Output a decl file with declaration comparability")
  public static boolean declaration_type_comparability = false;

  /**
   * Reads in a decl file with arbitrary comparability and writes out a file with comparability
   * based on the rep types (ie, there are comparability sets for int, boolean, string, and
   * hashcode) Two filenames are required: input-filename output-filename.
   */
  @Option("Output a decl file with representation type comparability")
  public static boolean rep_type_comparability = false;

  @Option("Read and dump a dtrace file")
  public static boolean dump_dtrace = false;

  @Option("Specify the comparability type (implicit or none)")
  public static String comparability = "implicit";

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

    /** Returns the variable's name. */
    public String get_name() {
      return name;
    }

    /** Returns the variable's declared type as specified in the decl file b. */
    public String get_type() {
      return type;
    }

    /**
     * Returns the type name. get_type() returns the entire entry including auxiliary information.
     */
    public String get_type_name() {
      return type.replaceFirst(" .*", "");
    }

    /**
     * Returns the representation type of the variable as specified in the decl file. The static
     * value (if any) is discarded.
     */
    public String get_rep_type() {
      return rep_type.replaceFirst(" .*", "");
    }

    /**
     * Return true if this variable has floating-point type (float or double).
     *
     * @return ture if this variable has floating-point type
     */
    @Pure
    public boolean is_double() {
      return (rep_type.equals("double") || rep_type.equals("float"));
    }

    @Pure
    public boolean is_string() {
      return (rep_type.equals("string"));
    }

    @Pure
    public boolean is_string_array() {
      return (rep_type.equals("string[]"));
    }

    @Pure
    public boolean is_int() {
      return (rep_type.equals("int"));
    }

    /** Returns the comparability string from the decl file. */
    public String get_comparability() {
      return comparability;
    }

    public String get_basic_comparability() {
      return comparability.replaceFirst("\\[.*\\]", "");
    }

    public void set_comparability(String comparability) {
      this.comparability = comparability;
    }

    @SideEffectFree
    @Override
    public String toString(@GuardSatisfied DeclVarInfo this) {
      return String.format("%s [%s] %s", type, rep_type, name);
    }

    /**
     * Reads a single value for this variable and returns it. The result is null exactly if the
     * value is nonsensical. The return value is interned.
     */
    public @Nullable @Interned Object read_data(EntryReader reader) throws IOException {
      String var_name = reader.readLine();
      if (var_name == null) {
        throw new Error(
            "file " + reader.getFileName() + " terminated prematurely, expeced var " + this.name);
      }
      if (!var_name.equals(this.name)) {
        throw new Error(var_name + " found where " + this.name + " expected ");
      }
      String value = reader.readLine();
      String mod_bit = reader.readLine();
      if ((value == null) || (mod_bit == null)) {
        throw new Error("file " + reader.getFileName() + " terminated prematurely");
      }
      if (value.equals("nonsensical")) {
        return null;
      } else if (is_int()) {
        try {
          return (Intern.intern(value)); // interning bugfix
        } catch (NumberFormatException t) {
          throw new Error("Unexpected value '" + value + "' for integer variable " + this.name);
        }
      } else if (is_double()) {
        try {
          return (Intern.intern(value)); // interning bugfix
        } catch (NumberFormatException t) {
          throw new Error("Unexpected string '" + value + "' for double variable " + this.name);
        }
      } else if (is_string()) {
        if (value.startsWith("\"") && value.endsWith("\"")) {
          value = value.substring(1, value.length() - 1);
        }
        return value.intern();
      } else if (is_string_array()) {
        return (null); // treating arrays as nonsensical for now
      } else {
        throw new Error("unexpected rep type " + rep_type);
      }
    }
  }

  /**
   * Information about the program point that is contained in the decl file. This consists of the
   * ppt name and a list of the declared variables.
   */
  public static class DeclPpt {
    public String name;
    public HashMap<String, DeclVarInfo> vars = new LinkedHashMap<>();

    /**
     * List of values for the program point. There is one entry in the list for each time the
     * program point is executed. That entry is a list of the values for each variable in the same
     * order as the variables were defined.
     */
    List<List<@Interned Object>> data_values = new ArrayList<>();

    public DeclPpt(String name) {
      this.name = name;
    }

    /**
     * Read a single variable declaration from decl_file. The file must be positioned immediately
     * before the variable name.
     */
    public DeclVarInfo read_var(EntryReader decl_file) throws java.io.IOException {

      String name = decl_file.readLine();
      String type = decl_file.readLine();
      String rep_type = decl_file.readLine();
      String comparability = decl_file.readLine();
      if ((name == null) || (type == null) || (rep_type == null) || (comparability == null)) {
        throw new Error("File " + decl_file.getFileName() + " ends prematurely");
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
     * Adds a record of data for this ppt. The data must have one element for each variable in the
     * ppt and be ordered in the same way.
     */
    public void add_var_data(List<@Interned Object> var_data_list) {
      assert var_data_list.size() == vars.size();
      data_values.add(var_data_list);
    }

    public List<List<@Interned Object>> get_var_data() {
      return data_values;
    }

    /** Returns the DeclVarInfo named var_name or null if it doesn't exist. */
    public @Nullable DeclVarInfo find_var(String var_name) {
      return vars.get(var_name);
    }

    /** Returns the ppt name. */
    public String get_name() {
      return name;
    }

    /** Returns the name without the :::EXIT, :::ENTER, etc. */
    public String get_short_name() {
      return name.replaceFirst(":::.*", "");
    }

    @SideEffectFree
    @Override
    public String toString(@GuardSatisfied DeclPpt this) {
      return name;
    }

    /** Returns the list of variables in their standard order. */
    public List<DeclVarInfo> get_all_vars() {
      return new ArrayList<DeclVarInfo>(vars.values());
    }
  }

  public DeclReader() {}

  /** Read declarations from the specified pathname. */
  public void read(File pathname) throws IOException {

    // have caller deal with FileNotFound
    EntryReader decl_file = new EntryReader(pathname, "^(//|#).*", null);

    try {
      for (String line = decl_file.readLine(); line != null; line = decl_file.readLine()) {
        if (!line.equals("DECLARE")) {
          continue;
        }

        // Read the declaration
        read_decl(decl_file);
      }
    } catch (Exception e) {
      throw new Error("Error reading comparability decl file", e);
    }
  }

  /**
   * Reads a single declaration from decl_file. The opening "DECLARE" line should have already been
   * read. Returns the ppt.
   */
  protected DeclPpt read_decl(EntryReader decl_file) throws IOException {

    // Read the name of the program point
    String pptname = decl_file.readLine();
    if (pptname == null) throw new Error("file " + decl_file.getFileName() + " ends prematurely");
    assert pptname.contains(":::");
    DeclPpt ppt = new DeclPpt(pptname);
    ppts.put(pptname, ppt);

    // Read each of the variables in this program point.  The variables
    // are terminated by a blank line.
    String line = decl_file.readLine();
    while ((line != null) && (line.length() != 0)) {
      decl_file.putback(line);
      ppt.read_var(decl_file);
      line = decl_file.readLine();
    }

    return ppt;
  }

  public void dump_decl() {

    for (String ppt_name : ppts.keySet()) {
      System.out.printf("Comp Ppt: %s%n", ppt_name);
      DeclPpt ppt = ppts.get(ppt_name);
      for (String var_name : ppt.vars.keySet()) {
        System.out.printf("  var %s%n", var_name);
      }
    }
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

  /** Reads a decl file and dumps statistics. */
  public static void main(String[] args) throws IOException {

    Options options = new Options("DeclReader [options] decl-files...", DeclReader.class);
    String[] files = options.parse(true, args);
    boolean print_each_set = !avg_size;

    if (files.length < 1) {
      System.err.println("No decl-files provided.");
      System.exit(1);
    }

    // If reading/dumping dtrace file, just read one file and dump it
    if (dump_dtrace) {
      if (files.length != 1) {
        System.err.println("One decl-file expected, received " + files.length + ".");
        System.exit(1);
      }
      DTraceReader trace = new DTraceReader();
      trace.read(new File(files[0]));
      trace.dump_data();
      return;
    }

    // If determining primitive declaration type comparability, setup
    // the comparability based on the declared type of primitives and
    // write out the result.
    if (primitive_declaration_type_comparability) {
      if (files.length != 2) {
        System.err.println("Two decl-files expected, received " + files.length + ".");
        System.exit(1);
      }
      DeclReader dr = new DeclReader();
      dr.read(new File(files[0]));
      dr.primitive_declaration_types();
      dr.write_decl(files[1], comparability);
      return;
    }

    // If determining declaration type comparability, setup the comparability
    // based on the declared types and write out the result.
    if (declaration_type_comparability) {
      if (files.length != 2) {
        System.err.println("Two decl-files expected, received " + files.length + ".");
        System.exit(1);
      }
      DeclReader dr = new DeclReader();
      dr.read(new File(files[0]));
      dr.declaration_types();
      dr.write_decl(files[1], comparability);
      return;
    }

    // If determining representation type comparability, setup comparability
    // on the rep type of each variable and write out the result.
    if (rep_type_comparability) {
      if (files.length != 2) {
        System.err.println("Two decl-files expected, received " + files.length + ".");
        System.exit(1);
      }
      DeclReader dr = new DeclReader();
      dr.read(new File(files[0]));
      dr.rep_types();
      dr.write_decl(files[1], comparability);
      return;
    }

    for (String filename : files) {
      DeclReader dr = new DeclReader();
      dr.read(new File(filename));

      int num_sets = 0;
      int total_set_size = 0;
      int num_type_integer_vars = 0;
      int num_rep_type_integer_vars = 0;
      int num_vars = 0;

      // Map from rep type to the count of each declared type for that rep
      Map<String, Map<String, Integer>> rep_map = new LinkedHashMap<>();

      // Loop through each ppt
      for (DeclPpt ppt : dr.ppts.values()) {
        if (print_each_set) {
          System.out.printf("ppt %s%n", ppt.name);
        }

        int ppt_num_sets = 0;
        int ppt_total_set_size = 0;

        // Build a map from comparabilty to all of the variables with that comp
        Map<String, List<DeclVarInfo>> comp_map = new LinkedHashMap<>();
        for (DeclVarInfo vi : ppt.vars.values()) {

          // Update the map that tracks the declared types for each rep type
          Map<String, Integer> dec_map = rep_map.get(vi.get_rep_type());
          if (dec_map == null) {
            dec_map = new LinkedHashMap<>();
            rep_map.put(vi.get_rep_type(), dec_map);
          }
          Integer cnt = dec_map.get(vi.get_type_name());
          if (cnt == null) cnt = 0;
          dec_map.put(vi.get_type_name(), ++cnt);

          num_vars++;
          if (vi.get_type_name().equals("int")) num_type_integer_vars++;
          if (vi.get_rep_type().equals("int")) num_rep_type_integer_vars++;
          if (!vi.get_type_name().equals("int") && vi.get_rep_type().equals("int")) {
            System.out.printf("huh '%s' - '%s'%n", vi.get_type_name(), vi.rep_type);
          }
          String comp = vi.get_basic_comparability();
          if (!comp_map.containsKey(comp)) {
            comp_map.put(comp, new ArrayList<DeclVarInfo>());
          }
          assert comp_map.containsKey(comp); // XXX flow could compute this
          List<DeclVarInfo> vi_list = comp_map.get(comp);
          vi_list.add(vi);
        }

        // Print out the variables with each comparability
        for (List<DeclVarInfo> vi_list : comp_map.values()) {
          num_sets++;
          total_set_size += vi_list.size();
          if (print_each_set) {
            ppt_num_sets++;
            ppt_total_set_size += vi_list.size();
            System.out.printf(
                "  %-5s : [%d] %s%n",
                vi_list.get(0).get_basic_comparability(), vi_list.size(), vi_list);
          }
        }
        if (print_each_set && (ppt_num_sets > 0)) {
          System.out.printf(
              "  %d sets of average size %f%n",
              ppt_num_sets, ((double) ppt_total_set_size) / ppt_num_sets);
        }
      }

      if (avg_size) {
        System.out.printf(
            "%-35s %,6d sets of average size %f found%n",
            filename, num_sets, ((double) total_set_size) / num_sets);
      }
      for (String rep_type : rep_map.keySet()) {
        Map<String, Integer> dec_map = rep_map.get(rep_type);
        System.out.printf("Rep Type %s (%d declared types):%n", rep_type, dec_map.size());
        for (String dec_type : dec_map.keySet()) {
          System.out.printf("  %5d - %s%n", dec_map.get(dec_type), dec_type);
        }
      }
      System.out.printf("Total variables of declared type int = %d%n", num_type_integer_vars);
      System.out.printf("Total variables of rep type int = %d%n", num_rep_type_integer_vars);
      System.out.printf("Total number of variables = %d%n", num_vars);
    }
  }

  /**
   * Sets the comparability to match declaration types. The comparability for each variable is set
   * so that each declaration type is in a separate set. Each hashcode with a different type name
   * will be in a different set. Primitives with different type names will be in different sets.
   */
  public void declaration_types() {

    Map<String, Integer> type_comp = new LinkedHashMap<>();
    int next_comparability = 1;

    // Loop through each program point
    for (DeclPpt ppt : ppts.values()) {

      // Loop through each variable
      for (DeclVarInfo vi : ppt.vars.values()) {

        // Determine the comparabilty for this declared type.  Hashcodes
        // are not included because hashcodes of different declared types
        // can still be sensibly compared (eg, subclasses/superclasses)
        String declared_type = vi.get_type_name();
        Integer comparability = type_comp.get(declared_type);
        if (comparability == null) {
          comparability = Integer.valueOf(next_comparability);
          next_comparability++;
          type_comp.put(declared_type, comparability);
          System.out.printf(
              "declared type %s has comparability %d%n", declared_type, comparability);
        }
        vi.set_comparability(comparability.toString());
      }
    }
  }
  /**
   * Sets the comparability to match primitive declaration types. The comparability for each
   * non-hashcode is set so that each declaration type is in a separate set. Hashcodes are all set
   * to a single comparability regardless of their declared type.
   */
  public void primitive_declaration_types() {

    Map<String, Integer> type_comp = new LinkedHashMap<>();
    int next_comparability = 1;

    // Loop through each program point
    for (DeclPpt ppt : ppts.values()) {

      // Loop through each variable
      for (DeclVarInfo vi : ppt.vars.values()) {

        // Determine the comparabilty for this declared type.  Hashcodes
        // are not included because hashcodes of different declared types
        // can still be sensibly compared (eg, subclasses/superclasses)
        String declared_type;
        if (vi.get_rep_type().startsWith("hashcode")) {
          declared_type = "hashcode";
        } else {
          declared_type = vi.get_type_name();
        }
        Integer comparability = type_comp.get(declared_type);
        if (comparability == null) {
          comparability = Integer.valueOf(next_comparability);
          next_comparability++;
          type_comp.put(declared_type, comparability);
          System.out.printf(
              "declared type %s has comparability %d%n", declared_type, comparability);
        }
        vi.set_comparability(comparability.toString());
      }
    }
  }

  /** Sets the comparability to match the rep types. */
  public void rep_types() {

    Map<String, Integer> type_comp = new LinkedHashMap<>();
    int next_comparability = 1;

    // Loop through each program point
    for (DeclPpt ppt : ppts.values()) {

      // Loop through each variable
      for (DeclVarInfo vi : ppt.vars.values()) {

        // Determine the comparabilty for this rep type.
        String rep_type = vi.get_rep_type();
        Integer comparability = type_comp.get(rep_type);
        if (comparability == null) {
          comparability = Integer.valueOf(next_comparability);
          next_comparability++;
          type_comp.put(rep_type, comparability);
          System.out.printf(
              "representation type %s has comparability %d%n", rep_type, comparability);
        }
        vi.set_comparability(comparability.toString());
      }
    }
  }

  /** Writes the declaration to the specified file. If the filename is "-", writes to stdout. */
  public void write_decl(String filename, String comparability) throws IOException {

    // Get the output stream
    PrintStream decl_file;
    if (filename.equals("-")) {
      decl_file = System.out;
    } else {
      decl_file = new PrintStream(filename);
    }

    decl_file.printf("// Declaration file written by DeclReader%n%n");
    decl_file.printf("VarComparability%n%s%n%n", comparability);

    // Loop through each program point
    for (DeclPpt ppt : ppts.values()) {

      decl_file.printf("%nDECLARE%n%s%n", ppt.name);

      // Loop through each variable
      for (DeclVarInfo vi : ppt.vars.values()) {
        decl_file.printf(
            "%s%n%s%n%s%n%s%n",
            vi.get_name(), vi.get_type(), vi.get_rep_type(), vi.get_comparability());
      }
    }

    decl_file.close();
  }

  /** Returns a list of all of the program points. */
  public List<DeclPpt> get_all_ppts() {
    return new ArrayList<DeclPpt>(ppts.values());
  }
}
