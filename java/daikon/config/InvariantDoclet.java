package daikon.config;

import java.util.*;
import java.io.*;
import java.lang.reflect.*;
import com.sun.javadoc.*;
import utilMDE.*;

/**
 * InvariantDoclet is a JavaDoc doclet that collects information about
 * the invariants defined within Daikon.  Class documentation is collected
 * about each class that is derived (either directly or indirectly) from
 * daikon.inv.Invariant
 **/
public class InvariantDoclet
{

  /**
   * Entry point for this doclet (invoked by javadoc).
   **/
  public static boolean start(RootDoc doc)
    throws IOException
  {
    InvariantDoclet pd = new InvariantDoclet(doc);
    pd.process();

    return true;
  }

  /**
   * Invoked by javadoc to query whether an option is allowed.
   * @return number of tokens used by one option.
   **/
  public static int optionLength(String opt) {
    if ("--texinfo".equals(opt))
      return 2; // == 1 tag + 1 argument

    if ("--text".equals(opt))
      return 2; // == 1 tag + 1 argument

    if ("--list".equals(opt))
      return 2; // == 1 tag + 1 argument

    return 0;   // unknown option
  }

  // ======================== NON-STATIC METHODS ==============================

  protected RootDoc root;   // root document
  protected TreeMap cmap;   // map of classdoc to derived classes for the class
  protected HashMap fields;
  protected boolean dump_class_tree = false;


  public InvariantDoclet(RootDoc doc) {
    root = doc;
    fields = new HashMap();
    cmap = new TreeMap();
  }

  /**
   * Process a javadoc tree and call processField for each field found.
   **/
  public void process()
    throws IOException {

    ClassDoc[] clazzes = root.classes();

    //go through all of the classes and intialize the map
    for (int i = 0; i < clazzes.length; i++) {
      ClassDoc cd = clazzes[i];
      cmap.put (cd, new TreeSet());
    }

    //go through the list again and put in the derived class information
    for (int i = 0; i < clazzes.length; i++) {
      ClassDoc cd = clazzes[i];
      ClassDoc super_c = cd.superclass();
      if (super_c != null) {
        TreeSet derived = (TreeSet) cmap.get  (super_c);
        if (derived == null) {
           // System.out.println ("NO SUPER: " + cd + " s: " + super_c);
        } else {
          // System.out.println ("   SUPER: " + cd + "s: " + super_c);
          derived.add (cd);
        }
      }
    }

    if (dump_class_tree) {
      //loop through each class in order
      for (Iterator itor = cmap.entrySet().iterator(); itor.hasNext(); ) {

        //get the classdoc
        Map.Entry entry = (Map.Entry) itor.next();
        ClassDoc cd = (ClassDoc) entry.getKey();

        //if this is a top level class
        if ((cd.superclass() == null) || (cmap.get (cd.superclass()) == null)) {
          process_class_tree_txt (System.out, cd, 0);
        }
      }
    }

    //do the specified work
    String[][] options = root.options();
    for (int i = 0; i < options.length; i++) {
      String[] optset = options[i];
      String opt = optset[0];

      if ("--texinfo".equals(opt)) {

        String fname = optset[1];
        System.out.println("Opening " + fname + " for output...");
        PrintStream outf = new PrintStream (new FileOutputStream (fname));

        ClassDoc inv = root.classNamed ("daikon.inv.Invariant");
        process_class_sorted_texinfo (outf, inv);
        outf.close();

      } else if ("--text".equals(opt)) {

        String fname = optset[1];
        System.out.println("Opening " + fname + " for output...");
        PrintStream outf = new PrintStream (new FileOutputStream(fname));
        ClassDoc inv = root.classNamed ("daikon.inv.Invariant");
        process_class_tree_txt (outf, inv, 0);
        outf.close();

      } else if ("--list".equals(opt)) {

        String fname = optset[1];
        System.out.println("Opening " + fname + " for output...");
        PrintStream outf = new PrintStream(new FileOutputStream (fname));
        outf.close();
      }
    }
  }

  /**
   * Prints a class and all of its derived classes as a simple indented tree.
   *
   * @param out     Stream to which to print
   * @param cd      Starting class
   * @param indent  Starting indent for the derived class (normally 0)
   */
  public void process_class_tree_txt (PrintStream out, ClassDoc cd, int indent) {

    String prefix = "";

    //create the prefix string
    for (int i = 0; i < indent; i++)
      prefix += "+";

    //put out this class
    String is_abstract = "";
    if (cd.isAbstract())
      is_abstract = " (Abstract)";
    out.println (prefix + cd + is_abstract);
    String comment = cd.commentText();
    comment = "         " + comment;
    comment = UtilMDE.replaceString (comment, "\n", "\n        ");
    out.println (comment);

    //put out each derived class
    TreeSet derived = (TreeSet) cmap.get (cd);
    for (Iterator itor = derived.iterator(); itor.hasNext(); ) {
      ClassDoc dc = (ClassDoc) itor.next();
      process_class_tree_txt (out, dc, indent + 1);
    }
  }


  /**
   * Prints a class and all of its derived classes with their documentation
   * in a simple sorted (by name) list in texinfo format.  Suitable for
   * inclusion in the manual.
   *
   * @param out     stream to which write output
   * @param cd      Starting class
   */
  public void process_class_sorted_texinfo (PrintStream out, ClassDoc cd) {

    out.println("@c BEGIN AUTO-GENERATED INVARIANTS LISTING");
    out.println();

    // Function binary values
    String last_fb = "";
    String fb_type = "";
    String permutes = "";
    String last_comment = "";
    int permute_cnt = 0;

    TreeSet list = new TreeSet();
    gather_derived_classes (cd, list);
    for (Iterator itor = list.iterator(); itor.hasNext(); ) {
      ClassDoc dc = (ClassDoc) itor.next();
      if (dc.isAbstract())
        continue;
      if (dc.qualifiedName().indexOf (".test.") != -1)
        continue;

      // setup the comment for info
      String comment = dc.commentText();
      // Do not indent the blocks; the extra spacing throws off Info.
      // comment = "    " + comment;
      // comment = UtilMDE.replaceString (comment, "\n", "\n   ");
      // Remove leading spaces, which throw off Info.
      UtilMDE.replaceString (comment, "\n ", "\n");
      comment = UtilMDE.replaceString (comment, "{", "@{");
      comment = UtilMDE.replaceString (comment, "}", "@}");
      comment = UtilMDE.replaceString (comment, "<br>", "@*");
      comment = UtilMDE.replaceString (comment, "<p>", "@*@*");

      if (dc.name().startsWith ("FunctionBinary")) {
        String[] parts = dc.name().split ("[._]");
        String fb_function = parts[1];
        String fb_permute = parts[2];
        if (last_fb.equals (fb_function)) {
          permutes += ", " + fb_permute;
          permute_cnt++;
        } else /* new type of function binary */ {
          if (last_fb != "") {  // interned
            out.println ();
            out.println ("@item " + fb_type + "." + last_fb + "_@{" + permutes
                       + "@}");
            out.println (last_comment);
            Assert.assertTrue ((permute_cnt == 3) || (permute_cnt == 6));
            if (permute_cnt == 3)
              out.println ("Since the function is symmetric, only the "
                           + "permutations xyz, yxz, and zxy are checked. ");
            else
              out.println ("Since the function is non-symmetric, all six "
                           + "permutations of the variables are checked");
          }
          last_fb = fb_function;
          permutes = fb_permute;
          last_comment = comment;
          fb_type = parts[0];
          permute_cnt = 1;
        }
      } else {
        out.println ();
        out.println ("@item " + dc.name());
        out.println (comment);
      }

      //Note if this invariant is turned off by default
      if (find_enabled (dc) == 0) {
        out.println ();
        out.println("This invariant is not enabled by default.  "
                    + "See the configuration option");
        out.println(dc + ".enabled");
      }

      //get a list of any other configuration variables
      Vector fields = find_fields (dc, Configuration.PREFIX);
      for (int i = 0; i < fields.size(); i++) {
        FieldDoc f =   (FieldDoc) fields.get (i);
        if (f.name().equals (Configuration.PREFIX + "enabled")) {
          fields.remove (i);
          break;
        }
      }

      //note the other configuration variables

      if (fields.size() > 0) {
        out.println();
        out.println("See also the following configuration option"
                    + (fields.size() > 1 ? "s" : "") + ":");
        out.println("    @itemize @bullet");
        for (int i = 0; i < fields.size(); i++) {
          out.print("    @item ");
          FieldDoc f = (FieldDoc) fields.get (i);
          out.println(UtilMDE.replaceString(f.qualifiedName(),
                                            Configuration.PREFIX, ""));
        }
        out.println("    @end itemize");
      }
    }

    out.println();
    out.println("@c END AUTO-GENERATED INVARIANTS LISTING");
  }

  /**
   * Gathers up all of the classes under cd and adds them to the
   * specified TreeSet.  They are sorted by their name.
   *
   * @param cd      The base class from which to start the search
   * @param list    The list to add classes to.  Should start out empty
   */

  public void gather_derived_classes (ClassDoc cd, TreeSet list) {

    // System.out.println ("Processing " + cd);
    TreeSet derived = (TreeSet) cmap.get (cd);
    for (Iterator itor = derived.iterator(); itor.hasNext(); ) {
      ClassDoc dc = (ClassDoc) itor.next();
      list.add (dc);
      gather_derived_classes (dc, list);
    }
  }


  /**
   * Looks for a field named dkconfig_enabled in the class and find
   * out what it is initialized to.
   *
   * @param cd      Class in which to look for dkconfig_enabled
   *
   * @return 1 for true, 0 for false, -1 if there was an error or
   * there was no such field
   */

  public int find_enabled (ClassDoc cd) {

    String enable_name = Configuration.PREFIX + "enabled";
    // System.out.println ("Looking for " + enable_name);

    FieldDoc[] fields = cd.fields();
    for (int j = 0; j < fields.length; j++) {
      FieldDoc field = fields[j];
      if (enable_name.equals (field.name())) {
        // System.out.println ("Found " + field.qualifiedName());
        try {
          String fullname = field.qualifiedName();
          int i = fullname.lastIndexOf('.');
          String classname = fullname.substring(0, i);
          Class c = Class.forName(classname);
          Field f = c.getField (enable_name);
          Object value = f.get(null);
          if (((Boolean) value).booleanValue())
            return (1);
          else
            return (0);
        } catch (Exception e) {
          System.err.println(e);
          return -1;
        }
      }
    }
    return (-1);
  }

  /**
   * Look for fields in the specified class that being with the
   * specified prefix.
   *
   * @param cd          ClassDoc of the class to search
   * @param prefix      String that must be at the beginning of the field name
   *
   * @return vector of FieldDoc entries for each field that matches.
   * If no fields are found, a zero length vector is returned (not
   * null)
   */

  public Vector find_fields (ClassDoc cd, String prefix) {

    Vector list = new Vector();

    FieldDoc[] fields = cd.fields();
    for (int i = 0; i < fields.length; i++) {
      FieldDoc f = fields[i];
      if (f.name().startsWith (Configuration.PREFIX))
        list.add (f);
    }

    return (list);
  }

  /**
   * Call Process(String, String) for each configuration field found.
   * Intended to be overridden.
   **/
  public void processField(FieldDoc field) {
    String name = field.name();
    if (name.startsWith(Configuration.PREFIX)) {
      String fullname = field.qualifiedName();
      int snip = fullname.indexOf(Configuration.PREFIX);
      fullname = fullname.substring(0, snip)
        + fullname.substring(snip + Configuration.PREFIX.length());
      String desc = field.commentText();
      process(fullname, desc);
    }
  }


  public static String NO_DESCRIPTION = "(no description provided)";
  public static String UNKNOWN_DEFAULT = "The default value is not known.";

  /**
   * Add <name, desc> pair to the map field 'fields'.
   **/
  public void process(String name, String desc) {
    if ("".equals(desc.trim()))
      desc = NO_DESCRIPTION;

    fields.put(name, desc);
  }

  private String getDefaultString(String field) {
    try {
      int i = field.lastIndexOf('.');
      String classname = field.substring(0, i);
      String fieldname = field.substring(i+1);
      Class c = Class.forName(classname);
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

    List keys = new ArrayList(fields.keySet());
    Collections.sort(keys);
    for (Iterator i = keys.iterator(); i.hasNext(); ) {
      String field = (String) i.next();
      String desc = (String) fields.get(field);
      String defstr = getDefaultString(field);

      // @item [field]
      //  [desc]
      out.println("@item " + field);
      out.println("  " + desc);
      out.println("  " + defstr);
      out.println();
    }

    out.println("@c END AUTO-GENERATED CONFIG OPTIONS LISTING");
    out.println();
  }

  public void writeText(PrintWriter out) {
    List keys = new ArrayList(fields.keySet());
    Collections.sort(keys);
    for (Iterator i = keys.iterator(); i.hasNext(); ) {
      String field = (String) i.next();
      String desc = (String) fields.get(field);
      String defstr = getDefaultString(field);

      // [field]
      //   [desc]
      out.println(field);
      out.println("  " + desc);
      out.println("  " + defstr);
      out.println();
    }
  }

  public void writeList(PrintWriter out) {
    List keys = new ArrayList(fields.keySet());
    Collections.sort(keys);
    for (Iterator i = keys.iterator(); i.hasNext(); ) {
      String field = (String) i.next();
      out.println(field);
    }
  }

}
