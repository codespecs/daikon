package daikon.config;

import com.sun.source.doctree.DocCommentTree;
import com.sun.source.doctree.DocTree;
import com.sun.source.util.DocTrees;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.Modifier;
import javax.lang.model.element.NestingKind;
import javax.lang.model.element.TypeElement;
import javax.lang.model.type.TypeMirror;
import javax.tools.Diagnostic;
import jdk.javadoc.doclet.Doclet;
import jdk.javadoc.doclet.DocletEnvironment;
import jdk.javadoc.doclet.Reporter;
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
  "nullness", // need help with this
  "keyfor" // need help with this
})
public class InvariantDoclet implements Doclet {

  /** System line separator value. */
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

  /** Map from a class name to its ClassInfo. */
  private Map<String, ClassInfo> cmap;

  /**
   * This method mimics the JDK 8 com.sun.javadoc.Doc.name() method. It returned a 'non-qualified
   * name' which differs from JDK 11+ javax.lang.model.element.TypeElement.getSimpleName() for a
   * nested class. name() would return 'Outer$Inner' while getSimpleName() returns 'Inner'. Note
   * that this version returns 'Outer.Inner'.
   *
   * @param te a class TypeElement
   * @return the class's 'partially' qualified name
   */
  public static String getBinaryName(TypeElement te) {
    String qualifiedName = te.getQualifiedName().toString();
    int i2 = qualifiedName.lastIndexOf('.');
    if (te.getNestingKind() == NestingKind.MEMBER) {
      i2 = qualifiedName.substring(0, i2).lastIndexOf('.');
    }
    return qualifiedName.substring(i2 + 1);
  }

  /** Compare TypeElements by their simple name. */
  static class sortBySimpleName implements Comparator<TypeElement> {
    @Override
    public int compare(TypeElement te1, TypeElement te2) {
      if (te1 == null && te2 == null) {
        return 0;
      }
      if (te1 == null) {
        return -1;
      }
      if (te2 == null) {
        return 1;
      }
      return te1.getSimpleName()
          .toString()
          .toLowerCase(Locale.ROOT)
          .compareTo(te2.getSimpleName().toString().toLowerCase(Locale.ROOT));
    }
  }

  /** Compare TypeElements by their binary name. */
  static class sortByBinaryName implements Comparator<TypeElement> {
    @Override
    public int compare(TypeElement te1, TypeElement te2) {
      if (te1 == null && te2 == null) {
        return 0;
      }
      if (te1 == null) {
        return -1;
      }
      if (te2 == null) {
        return 1;
      }
      return getBinaryName(te1).toLowerCase(Locale.ROOT).compareTo(getBinaryName(te2).toLowerCase(Locale.ROOT));
    }
  }

  /** Compare TypeElements by their fully qualified name. */
  static class sortByQualifiedName implements Comparator<TypeElement> {
    @Override
    public int compare(TypeElement te1, TypeElement te2) {
      if (te1 == null && te2 == null) {
        return 0;
      }
      if (te1 == null) {
        return -1;
      }
      if (te2 == null) {
        return 1;
      }
      return te1.getQualifiedName()
          .toString()
          .toLowerCase(Locale.ROOT)
          .compareTo(te2.getQualifiedName().toString().toLowerCase(Locale.ROOT));
    }
  }

  /** Container for class information. */
  static final class ClassInfo {
    /** Javadoc class information. */
    final TypeElement classInfo;

    /**
     * Name of classInfo's superclass. Value is null if classInfo is a top level class or its parent
     * is not included in list of classes passed to this doclet.
     */
    String superClass;

    /** Set of classInfo's derived classes. */
    final Set<TypeElement> derivedClasses;

    /**
     * Constructor for ClassInfo.
     *
     * @param classInfo initial value
     * @param derivedClasses initial value
     */
    public ClassInfo(TypeElement classInfo, Set<TypeElement> derivedClasses) {
      this.classInfo = classInfo;
      this.superClass = null;
      this.derivedClasses = derivedClasses;
    }

    /**
     * Setter for superClass.
     *
     * @param className new superClass value
     */
    void setSuperClass(String className) {
      superClass = className;
    }

    /**
     * Getter for classInfo.
     *
     * @return classInfo
     */
    TypeElement classInfo() {
      return classInfo;
    }

    /**
     * Getter for superClass.
     *
     * @return superClass
     */
    String superClass() {
      return superClass;
    }

    /**
     * Getter for derivedClasses.
     *
     * @return derivedClasses
     */
    Set<TypeElement> derivedClasses() {
      return derivedClasses;
    }
  }

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
    boolean dump_class_tree = false;

    postprocessOptions();
    docTrees = denv.getDocTrees();
    cmap = new TreeMap<>();

    // Save a list of class elements for subsequent processing
    List<TypeElement> clazzes = new ArrayList<>();
    for (Element item : denv.getSpecifiedElements()) {
      if (!isTypeElement(item)) {
        throw new Error(
            String.format("Unexpected specified element of kind %s: %s", item.getKind(), item));
      }

      // save item for subsequent processing
      clazzes.add((TypeElement) item);
      // note that for TypeElement item:
      //   item.toString() == item.getQualifiedName().toString()
      cmap.put(
          item.toString(),
          new ClassInfo((TypeElement) item, new TreeSet<TypeElement>(new sortByQualifiedName())));
    }

    // go through the list again and put in the derived class information
    for (TypeElement te : clazzes) {
      TypeMirror superClass = te.getSuperclass();
      // TypeMirror toString may include type parameters, we need to remove them
      String superName = superClass.toString().replaceFirst("<.*>", "");
      // System.out.println (te + ", " + superName + ", " + superClass.getKind());
      if (!superName.equals("none")) {
        if (!cmap.containsKey(superName)) {
          // System.out.println ("NO SUPER: " + te + " s: " + superName);
        } else {
          // System.out.println ("   SUPER: " + te + " s: " + superName);
          cmap.get(te.toString()).setSuperClass(superName);
          Set<TypeElement> derived = cmap.get(superName).derivedClasses();
          derived.add(te);
        }
      } else {
        // System.out.println ("NO SUPER: " + te + " s: null");
      }
    }

    if (dump_class_tree) {
      // loop through each class in order
      for (String className : cmap.keySet()) {
        ClassInfo cd = cmap.get(className);

        // if this is a top level class
        if (cd.superClass() == null) {
          process_class_tree_txt(System.out, cd, 0);
        }
      }
    }

    // do the specified work
    System.out.println("Opening " + outFilename + " for output...");
    try (PrintStream outf = new PrintStream(new FileOutputStream(outFilename))) {
      ClassInfo inv = cmap.get("daikon.inv.Invariant");
      if (formatTexinfo) {
        process_class_sorted_texinfo(outf, inv);
      } else {
        process_class_tree_txt(outf, inv, 0);
      }
    } catch (Exception e) {
      throw new Error("Unable to open file: " + outFilename, e);
    }

    return OK;
  }

  /**
   * Prints a class and all of its derived classes as a simple indented tree.
   *
   * @param out stream to which to print
   * @param cd starting class
   * @param indent starting indent for the derived class (normally 0)
   */
  public void process_class_tree_txt(PrintStream out, ClassInfo cd, int indent) {

    // create the prefix string
    String prefix = "";
    for (int i = 0; i < indent; i++) {
      prefix += "+";
    }

    TypeElement item = cd.classInfo();

    // put out this class
    String is_abstract = "";
    if (item.getModifiers().contains(Modifier.ABSTRACT)) is_abstract = " (Abstract)";
    out.println(prefix + item + is_abstract);
    String comment = getDocComment(docTrees.getDocCommentTree(item));
    comment = "         " + comment;
    comment = comment.replace(lineSep, lineSep + "        ");
    out.println(comment);

    // put out each derived class
    Set<TypeElement> derived = cd.derivedClasses();
    for (TypeElement dc : derived) {
      process_class_tree_txt(out, cmap.get(dc.toString()), indent + 1);
    }
  }

  /**
   * Prints a class and all of its derived classes with their documentation in a simple sorted (by
   * name) list in Texinfo format. Suitable for inclusion in the manual.
   *
   * @param out stream to which write output
   * @param cd class to process
   */
  public void process_class_sorted_texinfo(PrintStream out, @KeyFor("this.cmap") ClassInfo cd) {

    out.println("@c BEGIN AUTO-GENERATED INVARIANTS LISTING");
    out.println("@c Automatically generated by " + getClass());
    out.println();

    // Function binary values
    String last_fb = "";
    String fb_type = "";
    String permutes = "";
    String last_comment = "";
    int permute_cnt = 0;

    TreeSet<TypeElement> list = new TreeSet<>(new sortByBinaryName());
    gather_derived_classes(cd, list);
    for (TypeElement dc : list) {
      if (dc.getModifiers().contains(Modifier.ABSTRACT)) {
        continue;
      }
      // skip daikon.test.diff.DiffDummyInvariant
      if (dc.getQualifiedName().toString().indexOf(".test.") != -1) {
        continue;
      }

      // setup the comment for info
      String comment = getDocComment(docTrees.getDocCommentTree(dc));

      // System.out.println(dc.getQualifiedName() + ": COMMENT: " + comment);
      comment = HtmlToTexinfo.javadocHtmlToTexinfo(comment);

      String name = getBinaryName(dc);
      if (name.startsWith("FunctionBinary")) {
        String[] parts = name.split("[._]");
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
        out.println("@item " + name);
        out.println(comment);
      }

      // Make sure that all invariants have enable variables
      Boolean enabledInitValue = find_enabled(dc);
      if (enabledInitValue == null) {
        boolean ok_without_enable = false;
        for (String re : invs_without_enables) {
          if (name.matches("^" + re + "$")) {
            ok_without_enable = true;
            break;
          }
        }
        if (!ok_without_enable) {
          throw new Error("No enable variable for " + name);
        }
      }

      // Note whether this invariant is turned off by default
      if (enabledInitValue != null && !enabledInitValue) {
        out.println();
        out.println("This invariant is not enabled by default.  See the configuration option");
        out.println("@samp{" + dc + ".enabled}.");
      }

      // get a list of any other configuration variables
      List<Element> config_vars = find_fields(dc, Configuration.PREFIX);
      for (int i = 0; i < config_vars.size(); i++) {
        Element f = config_vars.get(i);
        if (f.getSimpleName().toString().equals(Configuration.PREFIX + "enabled")) {
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
        for (Element f : config_vars) {
          out.print("    @item ");
          String fullname = f.getEnclosingElement().toString() + "." + f.getSimpleName();
          out.println("@samp{" + fullname.replace(Configuration.PREFIX, "") + "}");
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
  public void gather_derived_classes(@KeyFor("this.cmap") ClassInfo cd, Set<TypeElement> set) {
    assert cmap.containsKey(cd.classInfo().toString());

    // System.out.println("Processing " + cd.classInfo());
    Set<TypeElement> derived = cd.derivedClasses();
    for (TypeElement te : derived) {
      set.add(te);
      gather_derived_classes(cmap.get(te.toString()), set);
    }
  }

  /**
   * Looks for a field named dkconfig_enabled in the class and find out what it is initialized to.
   *
   * @param cd class in which to look for dkconfig_enabled
   * @return the setting for the dkconfig_enabled variable in the class, or null if no such field
   */
  public @Nullable Boolean find_enabled(TypeElement cd) {

    String enable_name = Configuration.PREFIX + "enabled";
    // System.out.println("Looking for " + enable_name);

    for (Element field : cd.getEnclosedElements()) {
      if (field.getKind() != ElementKind.FIELD) continue;
      if (enable_name.equals(field.getSimpleName().toString())) {
        // System.out.println("Found in " + field.getEnclosingElement());
        try {
          String fullname = field.getEnclosingElement().toString() + "." + field.getSimpleName();
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
  public List<Element> find_fields(TypeElement cd, String prefix) {

    List<Element> list = new ArrayList<>();

    for (Element field : cd.getEnclosedElements()) {
      if (field.getKind() != ElementKind.FIELD) continue;
      if (field.getSimpleName().toString().startsWith(prefix)) {
        list.add(field);
      }
    }

    return list;
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
