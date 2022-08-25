package daikon.config;

import com.sun.javadoc.*;
import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import org.checkerframework.checker.nullness.qual.KeyFor;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.signature.qual.ClassGetName;
import org.plumelib.reflection.ReflectionPlume;
import org.plumelib.util.CollectionsPlume;
import org.plumelib.util.FilesPlume;

/**
 * ParameterDoclet is a JavaDoc doclet that collects information about the run-time configuration
 * options for the Daikon tools. Refer to the {@code --config} command-line option in the Daikon
 * manual for an introduction to the configuration system.
 */
@SuppressWarnings({
  "deprecation", // in JDK 9+, ClassDoc, RootDoc, etc. are deprecated
  "removal" // in JDK 11, ClassDoc, RootDoc, etc. are deprecated and marked for removal
})
public class ParameterDoclet {

  /** Entry point for this doclet (invoked by javadoc). */
  public static boolean start(RootDoc doc) throws IOException {
    ParameterDoclet pd = new ParameterDoclet(doc);

    pd.process();

    String[][] options = doc.options();
    for (int i = 0; i < options.length; i++) {
      String[] optset = options[i];
      String opt = optset[0];

      if ("--texinfo".equals(opt)) {
        String fname = optset[1];
        System.out.println("Opening " + fname + " for output...");
        PrintWriter outf = new PrintWriter(FilesPlume.newBufferedFileWriter(fname));
        pd.writeTexInfo(outf);
        outf.close();
      } else if ("--text".equals(opt)) {
        String fname = optset[1];
        System.out.println("Opening " + fname + " for output...");
	try (PrintWriter outf = new PrintWriter(FilesPlume.newBufferedFileWriter(fname))) {
          pd.writeText(outf);
        }
      } else if ("--list".equals(opt)) {
        String fname = optset[1];
        System.out.println("Opening " + fname + " for output...");
	try (PrintWriter outf = new PrintWriter(FilesPlume.newBufferedFileWriter(fname))) {
          pd.writeList(outf);
	}
      }
    }

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

    if ("--list".equals(opt)) return 2; // == 1 tag + 1 argument

    return 0; // unknown option
  }

  // ============================== NON-STATIC METHODS ==============================

  /**
   * A document category, or a subsection of the "List of configuration options" section in the
   * manual.
   */
  static class DocCategory {

    /**
     * If non-null, then include only variables whose fully-qualified name starts with this prefix.
     */
    public @Nullable String prefix;
    /**
     * If non-null, then include only variables with this simple name. Note that this is a field
     * name that starts with "dkconfig_", not a configuration option name.
     */
    public @Nullable String fieldName;
    /** Include all configuration options whose fully-qualified name is in the list. */
    public List<String> fqNames;

    /** The subsection name. */
    public String description;
    /** Text that appears in the subsection before the list of configuration options. */
    public String blurb;
    /** A map from a field name to its description. */
    public Map<String, String> fields;

    /**
     * Create a new DocCategory.
     *
     * @param prefix if non-null, include only variables whose fully-qualified name starts with this
     *     prefix
     * @param simpleName if non-null, include only configuration options with this simple name
     * @param fqConfigNames if non-null, include only configuration options whose fully-qualified
     *     name is in this list
     * @param description the subsection name
     * @param blurbLines text that appears in the subsection before the list of configuration
     *     options
     */
    public DocCategory(
        @Nullable String prefix,
        @Nullable String simpleName,
        @Nullable List<String> fqConfigNames,
        String description,
        String[] blurbLines) {
      this(
          prefix,
          simpleName,
          fqConfigNames,
          description,
          String.join(System.lineSeparator(), blurbLines));
    }

    /**
     * Create a new DocCategory.
     *
     * @param prefix if non-null, include only variables whose fully-qualified name starts with this
     *     prefix
     * @param simpleName if non-null, include only configuration options with this simple name
     * @param fqConfigNames if non-null, include only configuration options whose fully-qualified
     *     name is in this list
     * @param description the subsection name
     * @param blurb text that appears in the subsection before the list of configuration options
     */
    public DocCategory(
        @Nullable String prefix,
        @Nullable String simpleName,
        @Nullable List<String> fqConfigNames,
        String description,
        String blurb) {
      this.prefix = prefix;
      if (simpleName == null) {
        fieldName = null;
      } else {
        fieldName = Configuration.PREFIX + simpleName;
      }
      if (fqConfigNames == null) {
        this.fqNames = Collections.emptyList();
      } else {
        this.fqNames = fqConfigNames;
      }
      this.description = description;
      this.blurb = blurb;
      fields = new HashMap<>();
    }

    /**
     * Return true if the given variable (that represents a configuration option) should be included
     * in this section.
     *
     * @param fullConfigName the fully-qualified name of a Daikon configuration variable (no
     *     "dkconfig_")
     * @param simpleFieldName the simple name of the variable (starts with "dkconfig_")
     * @return true if the given field should be included in this section of the manual
     */
    public boolean matches(String fullConfigName, String simpleFieldName) {
      return ((prefix == null || fullConfigName.startsWith(prefix))
              && (fieldName == null || fieldName.equals(simpleFieldName)))
          || fqNames.contains(fullConfigName);
    }
  }

  protected RootDoc root; // root document
  protected DocCategory[] categories;

  public ParameterDoclet(RootDoc doc) {
    root = doc;
    categories =
        new DocCategory[] {
          // Do not re-order these options!  The pattern-matching is sensitive to the order
          // and the parent document knows the filter option comes first (for node linking).
          new DocCategory(
              "daikon.inv.filter.",
              "enabled",
              null,
              "Options to enable/disable filters",
              new String[] {
                "@cindex filters, enabling/disabling",
                "These configuration options enable or disable filters that suppress printing",
                "of certain invariants.  Invariants are filtered if they are redundant.",
                "See @ref{Invariant filters}, for more information.",
                "Also see configuration option @code{daikon.PrintInvariants.print_all}."
              }),
          new DocCategory(
              "daikon.inv.",
              "enabled",
              null,
              "Options to enable/disable specific invariants",
              new String[] {
                "@cindex invariants, enabling/disabling",
                "These options control whether Daikon looks for specific kinds of invariants.",
                "See @ref{Invariant list}, for more information about the corresponding invariants."
              }),
          new DocCategory(
              "daikon.inv.",
              null,
              null,
              "Other invariant configuration parameters",
              new String[] {
                "@cindex invariants, configuring",
                "The configuration options listed in this section parameterize the behavior of"
                    + " certain invariants.",
                "See @ref{Invariant list}, for more information about the invariants."
              }),
          new DocCategory(
              "daikon.derive.",
              null,
              null,
              "Options to enable/disable derived variables",
              new String[] {
                "@cindex derived variables, enabling/disabling",
                "These options control whether Daikon looks for invariants involving certain forms"
                    + " of derived variables.",
                "Also see @ref{Variable names}."
              }),
          new DocCategory(
              "daikon.simplify.",
              null,
              null,
              "Simplify interface configuration options",
              new String[] {
                "@cindex Simplify theorem prover, configuring",
                "The configuration options in this section are used to customize the interface to"
                    + " the Simplify theorem prover.",
                "See the description of the @option{--suppress_redundant} command-line option in"
                    + " @ref{Options to control invariant detection}."
              }),
          new DocCategory(
              "daikon.split.",
              null,
              null,
              "Splitter options",
              new String[] {
                "@cindex Splitters, configuring",
                "The configuration options in this section are used to customize the behavior"
                    + " of splitters,",
                "which yield conditional invariants and implications"
                    + " (@pxref{Conditional invariants})."
              }),
          new DocCategory(
              "daikon.Debug.",
              null,
              null,
              "Debugging options",
              new String[] {
                "@cindex Splitters, configuring",
                "The configuration options in this section are used to cause extra output that is"
                    + " useful for debugging.",
                "Also see section \"Daikon debugging options\" (@pxref{Daikon debugging options})."
              }),
          new DocCategory(
              "dummy prefix that won't match anything",
              "dummy simple name that won't match anything",
              Arrays.asList(
                  "daikon.Daikon.quiet",
                  "daikon.PrintInvariants.print_all",
                  // Progress output
                  "daikon.Daikon.progress_delay",
                  "daikon.Daikon.progress_display_width",
                  "daikon.FileIO.count_lines",
                  "daikon.FileIO.dtrace_line_count",
                  // Statistics
                  "daikon.PrintInvariants.true_inv_cnt",
                  "daikon.Daikon.print_sample_totals",
                  // Other
                  "daikon.PrintInvariants.print_inv_class",
                  "daikon.Daikon.output_conditionals",
                  "daikon.Daikon.guardNulls",
                  "daikon.FileIO.unmatched_procedure_entries_quiet",
                  "daikon.FileIO.verbose_unmatched_procedure_entries"),
              "Quantity of output",
              new String[] {
                "@cindex Output, quantity of",
                "The configuration options in this section make Daikon print more or less output.",
                "They do not affect which invariants Daikon computes, only how it ouputs them.",
                "Also see the following section."
              }),
          new DocCategory(
              null,
              null,
              null,
              "General configuration options",
              new String[] {"This section lists miscellaneous configuration options for Daikon."})
        };
  }

  /** Process a javadoc tree and call processField for each field found. */
  public void process() {
    ClassDoc[] clazzes = root.classes();
    for (int i = 0; i < clazzes.length; i++) {
      ClassDoc clazz = clazzes[i];
      FieldDoc[] fields = clazz.fields();
      for (int j = 0; j < fields.length; j++) {
        processField(fields[j]);
      }
    }
  }

  /**
   * Call Process(String, String, String) for each configuration field found. Intended to be
   * overridden.
   */
  public void processField(FieldDoc field) {
    String name = field.name();
    if (name.startsWith(Configuration.PREFIX)) {
      String fullname = field.qualifiedName();
      int snip = fullname.indexOf(Configuration.PREFIX);
      fullname =
          fullname.substring(0, snip) + fullname.substring(snip + Configuration.PREFIX.length());
      String desc = field.commentText();
      process(fullname, name, desc);
    }
  }

  public static String NO_DESCRIPTION = "(no description provided)";
  public static String UNKNOWN_DEFAULT = "The default value is not known.";
  public static Pattern endOfSentence;

  static {
    endOfSentence = Pattern.compile("[.?!>](\\))?$");
  }

  /** Add (name, desc) pair to the map field 'fields' for the appropriate category. */
  public void process(String fullname, String name, String desc) {
    // System.out.printf("%s - %s%n", fullname, name);

    if ("".equals(desc.trim())) {
      desc = NO_DESCRIPTION;
    } else if (!endOfSentence.matcher(desc).find()) {
      // Add period if there is no end-of-sentence delimiter
      desc = desc + ".";
    }

    for (int i = 0; i < categories.length; i++) {
      if (categories[i].matches(fullname, name)) {
        categories[i].fields.put(fullname, desc);
        break;
      }
    }
  }

  private String getDefaultString(String field) {
    try {
      int i = field.lastIndexOf('.');
      @SuppressWarnings("signature") // application invariant
      @ClassGetName String classname = field.substring(0, i);
      String fieldname = field.substring(i + 1);
      Class<?> c = ReflectionPlume.classForName(classname);
      Field f = c.getField(Configuration.PREFIX + fieldname);
      if (!Modifier.isStatic(f.getModifiers())) {
        throw new Error("Field " + Configuration.PREFIX + fieldname + " should be static");
      }
      @SuppressWarnings("nullness") // the field is static
      Object value = f.get(null);
      return "The default value is `" + value + "'.";
    } catch (Exception e) {
      System.err.println(e);
      return UNKNOWN_DEFAULT;
    }
  }

  public void writeTexInfo(PrintWriter out) {
    out.println("@c BEGIN AUTO-GENERATED CONFIG OPTIONS LISTING");
    out.println();

    out.println("@menu");
    for (int c = 0; c < categories.length; c++) {
      out.println("* " + categories[c].description + "::");
    }
    out.println("@end menu");
    out.println();

    String up = "List of configuration options";
    for (int c = 0; c < categories.length; c++) {
      String node = categories[c].description;
      String next = (c + 1 < categories.length) ? categories[c + 1].description : "";
      String prev = (c > 0) ? categories[c - 1].description : up;
      out.println("@node " + node + ", " + next + ", " + prev + ", " + up);
      out.println("@subsubsection " + node);
      out.println();
      out.println(categories[c].blurb);
      out.println();
      out.println("@table @option");
      out.println();

      for (
      @KeyFor("categories[c].fields") String field : CollectionsPlume.sortedKeySet(categories[c].fields)) {
        String desc = categories[c].fields.get(field);
        String defaultString = getDefaultString(field);

        // Simpler format for debugging
        if (false) {
          String value = defaultString.replaceFirst(".*`", "");
          value = value.replaceFirst("'.*", "");
          out.printf("@item %s %s%n%n", value, field);
          continue;
        }

        // @item [field]
        //  [desc]
        out.println("@item " + field);
        desc = HtmlToTexinfo.javadocHtmlToTexinfo(desc);
        out.println(desc);
        if (!desc.contains("The default value is")) {
          out.println(defaultString);
        }
        out.println();
      }
      out.println("@end table");
      out.println();
    }

    out.println("@c END AUTO-GENERATED CONFIG OPTIONS LISTING");
    out.println();
  }

  public void writeText(PrintWriter out) {
    for (int c = 0; c < categories.length; c++) {
      out.println(categories[c].description);
      out.println();

      for (
      @KeyFor("categories[c].fields") String field : CollectionsPlume.sortedKeySet(categories[c].fields)) {
        String desc = categories[c].fields.get(field);
        String defaultString = getDefaultString(field);

        // [field]
        //   [desc]
        out.println("  " + field);
        out.println("    " + desc);
        if (!desc.contains("The default value is")) {
          out.println("    " + defaultString);
        }
        out.println();
      }
    }
  }

  public void writeList(PrintWriter out) {
    for (int c = 0; c < categories.length; c++) {
      for (String field : CollectionsPlume.sortedKeySet(categories[c].fields)) {
        out.println(field);
      }
    }
  }
}
