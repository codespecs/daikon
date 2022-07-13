package daikon.config;

import com.sun.javadoc.*;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import org.checkerframework.checker.nullness.qual.KeyFor;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.regex.qual.Regex;
import org.checkerframework.checker.signature.qual.ClassGetName;
import org.plumelib.reflection.ReflectionPlume;

/**
 * InvariantDoclet is a Javadoc doclet that collects information about the invariants defined within
 * Daikon. Class documentation is collected about each class that is derived (either directly or
 * indirectly) from daikon.inv.Invariant. To specify the output format, use one of the following:
 *
 * <dl>
 *   <dt>{@code --texinfo FILENAME}
 *   <dd>Texinfo format, for inclusion in the manual.
 *   <dt>{@code --text FILENAME}
 *   <dd>Text format, with each name preceded by "+" characters to indicate depth in the tree.
 * </dl>
 */
@SuppressWarnings({
  "deprecation", // in JDK 9+, ClassDoc, RootDoc, etc. are deprecated
  "removal" // in JDK 11, ClassDoc, RootDoc, etc. are deprecated and marked for removal
})
public class InvariantDoclet {

  private static final String lineSep = System.lineSeparator();

  /**
   * Invariants that match any of the specified regular expressions are purposefully missing enable
   * variables. Any others will throw an exception.
   */
  private static @Regex String[] invs_without_enables =
      new @Regex String[] {
        "FunctionBinary.*",
        ".*RangeFloat.*",
        ".*RangeInt.*",
        "AndJoiner",
        "DummyInvariant",
        "Equality",
        "GuardingImplication",
        "Implication",
      };

  /** Entry point for this doclet (invoked by javadoc). */
  public static boolean start(RootDoc doc) throws IOException {
    InvariantDoclet pd = new InvariantDoclet(doc);
    pd.process();

    return true;
  }

  /**
   * Invoked by javadoc to query whether an option is allowed.
   *
   * @return number of tokens used by one option
   */
  public static int optionLength(String opt) {
    if ("--texinfo".equals(opt)) return 2; // == 1 tag + 1 argument

    if ("--text".equals(opt)) return 2; // == 1 tag + 1 argument

    return 0; // unknown option
  }

  // ======================== NON-STATIC METHODS ==============================

  protected RootDoc root; // root document
  protected Map<ClassDoc, Set<@KeyFor("cmap") ClassDoc>>
      cmap; // map of classdoc to derived classes for the class
  protected boolean dump_class_tree = false;

  public InvariantDoclet(RootDoc doc) {
    root = doc;
    cmap = new TreeMap<>();
  }

  /** Process a javadoc tree and create the specified invariant output. */
  public void process() throws IOException {

    @SuppressWarnings("keyfor") // the loop below makes all these keys to cmap
    @KeyFor("this.cmap") ClassDoc[] clazzes = root.classes();

    // go through all of the classes and intialize the map
    for (ClassDoc cd : clazzes) {
      cmap.put(cd, new TreeSet<@KeyFor("cmap") ClassDoc>());
    }

    // go through the list again and put in the derived class information
    for (ClassDoc cd : clazzes) {
      ClassDoc super_c = cd.superclass();
      if (super_c != null) {
        if (!cmap.containsKey(super_c)) {
          // System.out.println ("NO SUPER: " + cd + " s: " + super_c);
        } else {
          // System.out.println ("   SUPER: " + cd + " s: " + super_c);
          Set<@KeyFor("cmap") ClassDoc> derived = cmap.get(super_c);
          derived.add(cd);
        }
      } else {
        // System.out.println ("NO SUPER: " + cd + " s: null");
      }
    }

    if (dump_class_tree) {
      // loop through each class in order
      for (ClassDoc cd : cmap.keySet()) {

        // if this is a top level class
        if ((cd.superclass() == null) || (cmap.get(cd.superclass()) == null)) {
          process_class_tree_txt(System.out, cd, 0);
        }
      }
    }

    // do the specified work
    String[][] options = root.options();
    for (int i = 0; i < options.length; i++) {
      String[] optset = options[i];
      String opt = optset[0];

      if ("--texinfo".equals(opt)) {

        String fname = optset[1];
        System.out.println("Opening " + fname + " for output...");
        try (PrintStream outf = new PrintStream(new FileOutputStream(fname))) {
          @SuppressWarnings("keyfor") // Invariant is always a key
          @KeyFor("this.cmap") ClassDoc inv = root.classNamed("daikon.inv.Invariant");
          process_class_sorted_texinfo(outf, inv);
        }

      } else if ("--text".equals(opt)) {

        String fname = optset[1];
        System.out.println("Opening " + fname + " for output...");
        try (PrintStream outf = new PrintStream(new FileOutputStream(fname))) {
          @SuppressWarnings("keyfor") // Invariant is always present
          @KeyFor("cmap") ClassDoc inv = root.classNamed("daikon.inv.Invariant");
          process_class_tree_txt(outf, inv, 0);
        }
      }
    }
  }

  /**
   * Prints a class and all of its derived classes as a simple indented tree.
   *
   * @param out stream to which to print
   * @param cd starting class
   * @param indent starting indent for the derived class (normally 0)
   */
  public void process_class_tree_txt(
      PrintStream out, @KeyFor("this.cmap") ClassDoc cd, int indent) {
    assert cmap.containsKey(cd);

    String prefix = "";

    // create the prefix string
    for (int i = 0; i < indent; i++) {
      prefix += "+";
    }

    // put out this class
    String is_abstract = "";
    if (cd.isAbstract()) is_abstract = " (Abstract)";
    out.println(prefix + cd + is_abstract);
    String comment = cd.commentText();
    comment = "         " + comment;
    comment = comment.replace(lineSep, lineSep + "        ");
    out.println(comment);

    // put out each derived class
    Set<@KeyFor("cmap") ClassDoc> derived = cmap.get(cd);
    for (ClassDoc dc : derived) {
      process_class_tree_txt(out, dc, indent + 1);
    }
  }

  /**
   * Prints a class and all of its derived classes with their documentation in a simple sorted (by
   * name) list in Texinfo format. Suitable for inclusion in the manual.
   *
   * @param out stream to which write output
   * @param cd class to process
   */
  public void process_class_sorted_texinfo(PrintStream out, @KeyFor("this.cmap") ClassDoc cd) {

    out.println("@c BEGIN AUTO-GENERATED INVARIANTS LISTING");
    out.println("@c Automatically generated by " + getClass());
    out.println();

    // Function binary values
    String last_fb = "";
    String fb_type = "";
    String permutes = "";
    String last_comment = "";
    int permute_cnt = 0;

    TreeSet<ClassDoc> list = new TreeSet<>();
    gather_derived_classes(cd, list);
    for (ClassDoc dc : list) {
      if (dc.isAbstract()) {
        continue;
      }
      if (dc.qualifiedName().indexOf(".test.") != -1) {
        continue;
      }

      // setup the comment for info
      String comment = dc.commentText();

      // System.err.println("COMMENT: " + comment);
      comment = HtmlToTexinfo.javadocHtmlToTexinfo(comment);

      if (dc.name().startsWith("FunctionBinary")) {
        String[] parts = dc.name().split("[._]");
        String fb_function = parts[1];
        String fb_permute = parts[2];
        if (last_fb.equals(fb_function)) {
          permutes += ", " + fb_permute;
          permute_cnt++;
        } else /* new type of function binary */ {
          if (!last_fb.equals("")) { // actually, == test would work here
            out.println();
            out.println("@item " + fb_type + "." + last_fb + "_@{" + permutes + "@}");
            out.println(last_comment);
            assert (permute_cnt == 3) || (permute_cnt == 6);
            if (permute_cnt == 3) {
              out.println(
                  "Since the function is symmetric, only the "
                      + "permutations xyz, yxz, and zxy are checked.");
            } else {
              out.println(
                  "Since the function is non-symmetric, all six "
                      + "permutations of the variables are checked.");
            }
          }
          last_fb = fb_function;
          permutes = fb_permute;
          last_comment = comment;
          fb_type = parts[0];
          permute_cnt = 1;
        }
      } else {
        out.println();
        out.println("@item " + dc.name());
        out.println(comment);
      }

      // Make sure that all invariants have enable variables
      Boolean enabledInitValue = find_enabled(dc);
      if (enabledInitValue == null) {
        boolean ok_without_enable = false;
        for (String re : invs_without_enables) {
          if (dc.name().matches("^" + re + "$")) {
            ok_without_enable = true;
            break;
          }
        }
        if (!ok_without_enable) {
          throw new Error("No enable variable for " + dc.name());
        }
      }

      // Note whether this invariant is turned off by default
      if (enabledInitValue != null && !enabledInitValue) {
        out.println();
        out.println("This invariant is not enabled by default.  See the configuration option");
        out.println("@samp{" + dc + ".enabled}.");
      }

      // get a list of any other configuration variables
      List<FieldDoc> config_vars = find_fields(dc, Configuration.PREFIX);
      for (int i = 0; i < config_vars.size(); i++) {
        FieldDoc f = config_vars.get(i);
        if (f.name().equals(Configuration.PREFIX + "enabled")) {
          config_vars.remove(i);
          break;
        }
      }

      // Note the other configuration variables

      if (config_vars.size() > 0) {
        out.println();
        out.println(
            "See also the following configuration option"
                + (config_vars.size() > 1 ? "s" : "")
                + ":");
        out.println("    @itemize @bullet");
        for (FieldDoc f : config_vars) {
          out.print("    @item ");
          out.println("@samp{" + f.qualifiedName().replace(Configuration.PREFIX, "") + "}");
        }
        out.println("    @end itemize");
      }
    }

    out.println();
    out.println("@c END AUTO-GENERATED INVARIANTS LISTING");
  }

  /**
   * Gathers up all of the classes under cd and adds them to the specified TreeSet. They are sorted
   * by their name.
   *
   * @param cd the base class from which to start the search
   * @param set the set to add classes to. Should start out empty.
   */
  public void gather_derived_classes(@KeyFor("this.cmap") ClassDoc cd, TreeSet<ClassDoc> set) {
    assert cmap.containsKey(cd);

    // System.out.println ("Processing " + cd);
    Set<@KeyFor("cmap") ClassDoc> derived = cmap.get(cd);
    for (ClassDoc dc : derived) {
      set.add(dc);
      gather_derived_classes(dc, set);
    }
  }

  /**
   * Looks for a field named dkconfig_enabled in the class and find out what it is initialized to.
   *
   * @param cd class in which to look for dkconfig_enabled
   * @return the setting for the dkconfig_enabled variable in the class, or null if no such field
   */
  public @Nullable Boolean find_enabled(ClassDoc cd) {

    String enable_name = Configuration.PREFIX + "enabled";
    // System.out.println ("Looking for " + enable_name);

    FieldDoc[] fields = cd.fields();
    for (int j = 0; j < fields.length; j++) {
      FieldDoc field = fields[j];
      if (enable_name.equals(field.name())) {
        // System.out.println ("Found " + field.qualifiedName());
        try {
          String fullname = field.qualifiedName();
          int i = fullname.lastIndexOf('.');
          @SuppressWarnings("signature") // application invariant, substring
          @ClassGetName String classname = fullname.substring(0, i);
          Class<?> c;
          try {
            c = ReflectionPlume.classForName(classname);
          } catch (Throwable e) {
            throw new Error(
                String.format(
                    "Exception in ReflectionPlume.classForName(%s); fullname=%s%n",
                    classname, fullname),
                e);
          }
          Field f = c.getField(enable_name);
          @SuppressWarnings("nullness") // f has boolean type, so result is non-null Boolean
          @NonNull Object value = f.get(null);
          return (Boolean) value;
        } catch (Exception e) {
          throw new Error("In find_enabled(" + cd + ")", e);
        }
      }
    }
    return null;
  }

  /**
   * Look for fields in the specified class that begin with the specified prefix.
   *
   * @param cd the class to search
   * @param prefix string that must be at the beginning of the field name
   * @return vector of FieldDoc entries for each field that matches. If no fields are found, a zero
   *     length vector is returned (not null).
   */
  public List<FieldDoc> find_fields(ClassDoc cd, String prefix) {

    List<FieldDoc> list = new ArrayList<>();

    for (FieldDoc f : cd.fields()) {
      if (f.name().startsWith(prefix)) {
        list.add(f);
      }
    }

    return list;
  }
}
