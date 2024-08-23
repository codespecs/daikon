package daikon.config;

import com.sun.source.doctree.DocCommentTree;
import com.sun.source.doctree.DocTree;
import com.sun.source.util.DocTrees;
import java.io.PrintWriter;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.TypeElement;
import javax.tools.Diagnostic;
import jdk.javadoc.doclet.Doclet;
import jdk.javadoc.doclet.DocletEnvironment;
import jdk.javadoc.doclet.Reporter;
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
@SuppressWarnings("nullness") // need help with this
public class ParameterDoclet implements Doclet {

  /** A value that indicates a method completed successfully. */
  private static final boolean OK = true;

  /** A value that indicates a method did not complete successfully. */
  private static final boolean FAILED = false;

  /** File name of destination for output. */
  private @Nullable String outFilename = null;

  /** If true, then output format is Texinfo. */
  private boolean formatTexinfo = false;

  /** The DocTrees instance assocated with the DocletEnvironment. */
  private DocTrees docTrees;

  /** Used to report errors. */
  private Reporter reporter;

  /** A data structure for the document categories. */
  protected DocCategory[] categories;

  // ///////////////////////////////////////////////////////////////////////////
  // Doclet-specific methods
  //

  @Override
  public void init(Locale locale, Reporter reporter) {
    this.reporter = reporter;
  }

  @Override
  public String getName() {
    return getClass().getSimpleName();
  }

  @Override
  public Set<? extends Doclet.Option> getSupportedOptions() {
    return docletOptions;
  }

  @Override
  public SourceVersion getSupportedSourceVersion() {
    return SourceVersion.latest();
  }

  /** Entry point for this doclet (invoked by javadoc). */
  @Override
  public boolean run(DocletEnvironment denv) {

    postprocessOptions();
    docTrees = denv.getDocTrees();
    initDocCategories();

    // Save a list of class elements for subsequent processing
    List<TypeElement> clazzes = new ArrayList<>();
    for (Element item : denv.getSpecifiedElements()) {
      if (!isTypeElement(item)) {
        throw new Error(
            String.format("Unexpected specified element of kind %s: %s", item.getKind(), item));
      }
      clazzes.add((TypeElement) item);
    }

    // go through the list of classes and put in the derived class information
    for (TypeElement te : clazzes) {
      for (Element field : te.getEnclosedElements()) {
        if (field.getKind() != ElementKind.FIELD) continue;
        processField(field);
      }
    }

    // output the field data
    System.out.println("Opening " + outFilename + " for output...");
    PrintWriter outf;
    try {
      outf = new PrintWriter(FilesPlume.newBufferedFileWriter(outFilename));
    } catch (Exception e) {
      throw new Error("Unable to open file: " + outFilename, e);
    }

    if (formatTexinfo) {
      writeTexInfo(outf);
    } else {
      writeText(outf);
    }
    outf.close();

    return OK;
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

  /** Initialize the categories data structure. */
  public void initDocCategories() {
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

  /**
   * Call Process(String, String, String) for each configuration field found. Intended to be
   * overridden.
   *
   * @param field the javadoc element for a member field.
   */
  public void processField(Element field) {
    String name = field.getSimpleName().toString();
    if (name.startsWith(Configuration.PREFIX)) {
      String fullname = field.getEnclosingElement().toString() + "." + name;
      int snip = fullname.indexOf(Configuration.PREFIX);
      fullname =
          fullname.substring(0, snip) + fullname.substring(snip + Configuration.PREFIX.length());
      // setup the comment for info
      String desc = getDocComment(docTrees.getDocCommentTree(field));
      process(fullname, name, desc);
    }
  }

  /** A value that indicates no description was found. */
  public static String NO_DESCRIPTION = "(no description provided)";

  /** A value that indicates no default value was found. */
  public static String UNKNOWN_DEFAULT = "The default value is not known.";

  /** A Pattern used to check for the end of a sentence. */
  public static Pattern endOfSentence;

  static {
    endOfSentence = Pattern.compile("[.?!>](\\))?$");
  }

  /**
   * Add (name, desc) pair to the map field 'fields' for the appropriate category.
   *
   * @param fullname the fully-qualified name of a Daikon configuration variable (no "dkconfig_")
   * @param name the simple name of the variable (starts with "dkconfig_")
   * @param desc the javadoc comment for this variable
   */
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

  /**
   * Get the default value of a field as a string.
   *
   * @param field the field to inspect
   * @return the default value
   */
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

  /**
   * Output the parameter info in textinfo format.
   *
   * @param out where to write the data
   */
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

  /**
   * Output the parameter info in text format.
   *
   * @param out where to write the data
   */
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

  // ///////////////////////////////////////////////////////////////////////////
  // Helper methods
  //

  /**
   * Fetch the comment string from a DocCommentTree node.
   *
   * @param node the DocCommentTree to process
   * @return the entire body of the documentation comment as a string. If no comments are found, an
   *     empty string is returned (not null).
   */
  public String getDocComment(DocCommentTree node) {
    if (node == null) return "";
    StringBuilder sb = new StringBuilder();
    for (DocTree t : node.getFullBody()) {
      sb.append(t.toString());
    }
    return sb.toString();
  }

  /**
   * Returns true if the given element kind is a type, i.e., a class, enum, interface, or annotation
   * type.
   *
   * @param element the element to test
   * @return true, iff the given kind is a type kind
   */
  public static boolean isTypeElement(Element element) {
    ElementKind elementKind = element.getKind();
    return elementKind.isClass() || elementKind.isInterface();
  }

  // ///////////////////////////////////////////////////////////////////////////
  // Javadoc command-line options
  //

  // The doclet cannot use the Options class itself because  Javadoc specifies its own way of
  // handling command-line arguments.

  /**
   * A command-line option to the Javadoc doclet; implements the {@link Doclet.Option} interface.
   *
   * <p>Is abstract because it does not provide an implementation of the {@code process} method.
   */
  abstract static class DocletOption implements Doclet.Option {
    /** The number of arguments this option will consume. */
    private int argumentCount;

    /** The user-friendly description of the option. */
    private String description;

    /**
     * The list of names and aliases that may be used to identify the option. Each starts with "-"
     * or "--".
     */
    private List<String> names;

    /**
     * A user-friendly string description of the option's parameters, or the empty string if this
     * option has no parameters.
     */
    private String parameters;

    /**
     * Creates a new DocletOption.
     *
     * @param name the option's name, starting with "-" or "--"
     * @param parameters a user-friendly string description of the option's parameters, or the empty
     *     string if this option has no parameters
     * @param argumentCount the number of arguments this option will consume
     * @param description the user-friendly description of the option
     */
    DocletOption(String name, String parameters, int argumentCount, String description) {
      this.argumentCount = argumentCount;
      this.description = description;
      this.names = List.of(name);
      this.parameters = parameters;
    }

    /**
     * Creates a new DocletOption with an aliased name
     *
     * @param name the option's name, starting with "-" or "--"
     * @param alias the option's alias, starting with "-" or "--"
     * @param parameters a user-friendly string description of the option's parameters, or the empty
     *     string if this option has no parameters
     * @param argumentCount the number of arguments this option will consume
     * @param description the user-friendly description of the option
     */
    DocletOption(
        String name, String alias, String parameters, int argumentCount, String description) {
      this.argumentCount = argumentCount;
      this.description = description;
      this.names = List.of(name, alias);
      this.parameters = parameters;
    }

    @Override
    public int getArgumentCount() {
      return argumentCount;
    }

    @Override
    public String getDescription() {
      return description;
    }

    @Override
    public Doclet.Option.Kind getKind() {
      return Doclet.Option.Kind.STANDARD;
    }

    @Override
    public List<String> getNames() {
      return names;
    }

    @Override
    public String getParameters() {
      return parameters;
    }
  }

  /** The command-line options for OptionsDoclet. */
  @SuppressWarnings(
      "nullness:method.invocation" // when methods such as printError() are called, the receiver
  // (an OptionsDoclet) is initialized
  )
  private final Set<DocletOption> docletOptions =
      Set.of(
          new DocletOption(
              "--texinfo", "-texinfo", "file", 1, "write texinfo format output to this file") {
            @Override
            public boolean process(String option, List<String> arguments) {
              if (outFilename != null) {
                printError("must only specify one output option");
                return FAILED;
              }
              outFilename = arguments.get(0);
              formatTexinfo = true;
              return OK;
            }
          },
          new DocletOption("--text", "-text", "file", 1, "write text format output to this file") {
            @Override
            public boolean process(String option, List<String> arguments) {
              if (outFilename != null) {
                printError("must only specify one output option");
                return FAILED;
              }
              outFilename = arguments.get(0);
              formatTexinfo = false;
              return OK;
            }
          });

  /**
   * Sets variables that can only be set after all command-line options have been processed. Isuses
   * errors and halts if any command-line options are incompatible with one another.
   */
  private void postprocessOptions() {
    boolean hasError = false;
    if (outFilename == null) {
      printError("either --texinfo or --text must be specified");
      hasError = true;
    }

    if (hasError) {
      System.exit(1);
    }
  }

  /**
   * Print error message via delegation to {@link #reporter}.
   *
   * @param msg message to print
   */
  private void printError(String msg) {
    reporter.print(Diagnostic.Kind.ERROR, msg);
  }
}
