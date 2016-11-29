package daikon.config;

import com.sun.javadoc.*;
import java.io.*;
import java.lang.reflect.*;
import java.util.*;
import java.util.regex.*;
import plume.*;

/*>>>
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.checker.signature.qual.*;
*/

/**
 * ParameterDoclet is a JavaDoc doclet that collects information about the runtime configuration
 * options for the Daikon tools. Refer to the {@code --config} command-line option in the Daikon
 * manual for an introduction to the configuration system.
 */
public class ParameterDoclet {

  private static final String lineSep = System.getProperty("line.separator");

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
        PrintWriter outf = new PrintWriter(UtilMDE.bufferedFileWriter(fname));
        pd.writeTexInfo(outf);
        outf.close();
      } else if ("--text".equals(opt)) {
        String fname = optset[1];
        System.out.println("Opening " + fname + " for output...");
        PrintWriter outf = new PrintWriter(UtilMDE.bufferedFileWriter(fname));
        pd.writeText(outf);
        outf.close();
      } else if ("--list".equals(opt)) {
        String fname = optset[1];
        System.out.println("Opening " + fname + " for output...");
        PrintWriter outf = new PrintWriter(UtilMDE.bufferedFileWriter(fname));
        pd.writeList(outf);
        outf.close();
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

  static class DocCategory {
    public /*@Nullable*/ String prefixPattern;
    public /*@Nullable*/ String fieldName;
    public String description;
    public String longBlurb;
    public Map<String, String> fields; // field -> description

    public DocCategory(
        /*@Nullable*/ String prefix, /*@Nullable*/ String name, String desc, String blurb) {
      prefixPattern = prefix;
      if (name == null) {
        fieldName = null;
      } else {
        fieldName = Configuration.PREFIX + name;
      }
      description = desc;
      longBlurb = blurb;
      fields = new HashMap<String, String>();
    }

    public boolean matches(String fullname, String name) {
      return (((prefixPattern == null) || fullname.startsWith(prefixPattern))
          && ((fieldName == null) || name.equals(fieldName)));
    }
  }

  protected RootDoc root; // root document
  protected DocCategory[] categories;

  public ParameterDoclet(RootDoc doc) {
    root = doc;
    categories =
        new DocCategory[] {
          // Do not re-order these options!  The pattern-matching is sensitive
          // to the order and the parent document knows the filter option comes
          // first (for node linking).
          new DocCategory(
              "daikon.inv.filter.",
              "enabled",
              "Options to enable/disable filters",
              "@cindex filters, enabling/disabling\n"
                  + "These configuration options enable or disable filters that suppress printing of certain invariants.  Invariants are filtered if they are found to be true but are considered uninteresting or redundant.  See @ref{Invariant filters}, for more information."),
          new DocCategory(
              "daikon.inv.",
              "enabled",
              "Options to enable/disable specific invariants",
              "@cindex invariants, enabling/disabling\n"
                  + "These options control whether Daikon looks for specific kinds of invariants.  See @ref{Invariant list}, for more information about the corresponding invariants."),
          new DocCategory(
              "daikon.inv.",
              null,
              "Other invariant configuration parameters",
              "@cindex invariants, configuring\n"
                  + "The configuration options listed in this section parameterize the behavior of certain invariants.  See @ref{Invariant list}, for more information about the invariants."),
          new DocCategory(
              "daikon.derive.",
              null,
              "Options to enable/disable derived variables",
              "@cindex derived variables, enabling/disabling\n"
                  + "These options control whether Daikon looks for invariants involving certain forms of derived variables.  Also see @ref{Variable names}."),
          new DocCategory(
              "daikon.simplify.",
              null,
              "Simplify interface configuration options",
              "@cindex Simplify theorem prover, configuring\n"
                  + "The configuration options in this section are used to customize the interface to the Simplify theorem prover.  See the description of the @option{--suppress_redundant} command-line option in @ref{Options to control invariant detection}."),
          new DocCategory(
              "daikon.split.",
              null,
              "Splitter options",
              "@cindex Splitters, configuring\n"
                  + "The configuration options in this section are used to customize the the behavior of splitters, which yield conditional invariants and implications (@pxref{Conditional invariants})."),
          new DocCategory(
              "daikon.Debug.",
              null,
              "Debugging options",
              "@cindex Splitters, configuring\n"
                  + "The configuration options in this section are used to cause extra output that is useful for debugging."),
          new DocCategory(
              null,
              null,
              "General configuration options",
              "This section lists miscellaneous configuration options for Daikon.")
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
    // System.out.printf ("%s - %s%n", fullname, name);

    if ("".equals(desc.trim())) {
      desc = NO_DESCRIPTION;
    } else if (!endOfSentence.matcher(desc).find()) {
      // Add period if there is no end-of-sentence documentation
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
      /*@ClassGetName*/ String classname = field.substring(0, i);
      String fieldname = field.substring(i + 1);
      Class<?> c = UtilMDE.classForName(classname);
      Field f = c.getField(Configuration.PREFIX + fieldname);
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
      out.println(categories[c].longBlurb);
      out.println();
      out.println("@table @option");
      out.println();

      for (
      /*@KeyFor("categories[c].fields")*/ String field :
          UtilMDE.sortedKeySet(categories[c].fields)) {
        String desc = categories[c].fields.get(field);
        String defstr = getDefaultString(field);

        // Simpler format for debugging
        if (false) {
          String value = defstr.replaceFirst(".*`", "");
          value = value.replaceFirst("'.*", "");
          out.printf("@item %s %s%n%n", value, field);
          continue;
        }

        // @item [field]
        //  [desc]
        out.println("@item " + field);
        desc = HtmlToTexinfo.htmlToTexinfo(desc);
        out.println(desc);
        if (!desc.contains("The default value is")) {
          out.println(defstr);
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
      /*@KeyFor("categories[c].fields")*/ String field :
          UtilMDE.sortedKeySet(categories[c].fields)) {
        String desc = categories[c].fields.get(field);
        String defstr = getDefaultString(field);

        // [field]
        //   [desc]
        out.println("  " + field);
        out.println("    " + desc);
        if (!desc.contains("The default value is")) {
          out.println("    " + defstr);
        }
        out.println();
      }
    }
  }

  public void writeList(PrintWriter out) {
    for (int c = 0; c < categories.length; c++) {
      for (String field : UtilMDE.sortedKeySet(categories[c].fields)) {
        out.println(field);
      }
    }
  }
}
