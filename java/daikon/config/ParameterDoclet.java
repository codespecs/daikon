package daikon.config;

import java.util.*;
import java.io.*;
import com.sun.javadoc.*;
import utilMDE.*;

/**
 * ParameterDoclet is a JavaDoc doclet that collects information about
 * the runtime configuration options for the Daikon tools.  Refer to
 * the "--config" switch in the Daikon maunal for an introduction to
 * the configuration system.
 **/
public class ParameterDoclet
{

  /**
   * Entry point for this doclet (invoked by javadoc)
   **/
  public static boolean start(RootDoc doc)
    throws IOException
  {
    ParameterDoclet pd = new ParameterDoclet(doc);
    pd.process();

    String[][] options = doc.options();
    for (int i = 0; i < options.length; i++) {
      String[] optset = options[i];
      String opt = optset[0];

      if ("--texinfo".equals(opt)) {
	String fname = optset[1];
	System.out.println("Opening " + fname + " for output...");
	PrintWriter outf = new PrintWriter(UtilMDE.BufferedFileWriter(fname));
	pd.writeTexInfo(outf);
	outf.close();
      } else if ("--text".equals(opt)) {
	String fname = optset[1];
	System.out.println("Opening " + fname + " for output...");
	PrintWriter outf = new PrintWriter(UtilMDE.BufferedFileWriter(fname));
	pd.writeText(outf);
	outf.close();
      }
    }

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

    return 0;   // unknown option
  }

  // ============================== INSTANCE METHODS ==============================

  protected RootDoc root; // root document
  protected Map fields;   // field -> description

  public ParameterDoclet(RootDoc doc) {
    root = doc;
    fields = new HashMap();
  }

  /**
   * Process a javadoc tree and call processField for each field found.
   **/
  public void process()
  {
    ClassDoc[] clazzes = root.classes();
    for (int i = 0; i < clazzes.length; i++) {
      ClassDoc clazz = clazzes[i];
      FieldDoc[] fields = clazz.fields();
      for (int j = 0; j < fields.length; j++){
        processField(fields[j]);
      }
    }
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

  /**
   * Add <name, desc> pair to the map field 'fields'.
   **/
  public void process(String name, String desc) {
    if ("".equals(desc.trim()))
      desc = NO_DESCRIPTION;

    fields.put(name, desc);
  }

  public void writeTexInfo(PrintWriter out)
  {
    out.println("@c BEGIN AUTO-GENERATED CONFIG OPTIONS LISTING");
    out.println();

    List keys = new ArrayList(fields.keySet());
    Collections.sort(keys);
    for (Iterator i = keys.iterator(); i.hasNext(); ) {
      String field = (String) i.next();
      String desc = (String) fields.get(field);

      // @item [field]
      //  [desc]
      out.println("@item " + field);
      out.println("  " + desc);
      out.println();
    }

    out.println("@c END AUTO-GENERATED CONFIG OPTIONS LISTING");
    out.println();
  }


  public void writeText(PrintWriter out)
  {
    List keys = new ArrayList(fields.keySet());
    Collections.sort(keys);
    for (Iterator i = keys.iterator(); i.hasNext(); ) {
      String field = (String) i.next();
      String desc = (String) fields.get(field);

      // [field]
      //   [desc]
      out.println(field);
      out.println("  " + desc);
      out.println();
    }
  }

}
