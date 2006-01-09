package utilMDE;

import java.io.*;
import java.util.*;
import java.util.regex.*;
import java.lang.reflect.*;
import java.lang.annotation.*;

/**
 * Class that reads and sets command line options.  The Option
 * annotation is used to identify fields that are associated with a
 * command line option.  Both the short (-) and long option formats
 * (--) are supported.  The name of the option is the name of the
 * field (with some exceptions noted below).  The types boolean, int,
 * double, String, and File are supported.  The primitive types can be
 * either primitives or their wrappers (Boolean, Integer or Double).
 * Use of the wrappers allows a primitive argument not to have a
 * default value. <p>
 *
 * The option annotation (@see Option) specifies the optional short
 * name, optional type name, and long description.  The long name is taken
 * from the name of the variable. <p>
 *
 * On the command line, the values for options are specified in the form
 * '--long=value' or '-short=value'.  The value is mandatory for all
 * options except booleans.  Booleans are set to true if no value is
 * specified. <p>
 *
 * Any non-options (entries that don't begin with --) in the
 * argument list are returned. <p>
 *
 * Limitations: <ul>
 *
 *  <li> Short options are only supported as separate entries (eg, -a -b)
 *  and not as a single group (eg -ab).
 *
 *  <li> It would be nice if option value could be specified as the next
 *  argument as well as with '=value'
 *
 *  <li> Other types (such as the remaining primitives and user defined
 *  types) are not supported.
 *
 *  <li> Arrays are not supported
 *
 *  <li>  Non option information could be supported in the same manner
 *  as options which would be cleaner than simply returning all of the
 *  non-options as an array
 * </ul>
 **/
public class Options {

  /** Information about an option **/
  private static class OptionInfo {

    /** Field containing the value of the option **/
    Field field;

    /** Option information for the field **/
    Option option;

    /** Object containing the field.  Null if the field is static **/
    Object obj;

    /** Short (one character) argument name **/
    String short_name;

    /** Long argument name **/
    String long_name;

    /** Argument description **/
    String description;

    /**
     * Name of the argument type.  Defaults to the type of the field, but
     * user can override this in the option string
     */
    String type_name;

    /** Default value of the option as a string **/
    String default_str = null;

    /**
     * Create the specified option.  If obj is null, the field must be
     * static.  The short name, type name, and description are taken
     * from the option annotation.  The long name is the name of the
     * field.  The default value is the current value of the field.
     */
    OptionInfo (Field field, Option option, Object obj) {
      this.field = field;
      this.option = option;
      this.obj = obj;

      // Get the short name, long name, and description
      String val = option.value();
      if (val.startsWith("-")) {
        short_name = val.substring (1, 2);
        description = val.substring (3);
      } else {
        short_name = null;
        description = val;
      }
      long_name = field.getName();

      // Get the type (if any) out of the description
      type_name = type_short_name (field.getType());
      if (description.startsWith ("<")) {
        type_name = description.substring (1).replaceFirst (">.*", "");
        description = description.replaceFirst ("<.*> ", "");
      }

      // Get the default value (if any)
      try {
        Object default_obj = field.get (obj);
        if (default_obj != null)
          default_str = default_obj.toString();
      } catch (Exception e) {
        throw new Error ("Unexpected error getting default for " + field, e);
      }

    }

    /**
     * Returns whether or not this option has a required argument
     */
    public boolean argument_required() {
      Class type = field.getType();
      return ((type != Boolean.TYPE) && (type != Boolean.class));
    }

    /**
     * Returns a short synopsis of the option in the form
     * -s --long=<type>
     **/
    public String synopsis() {
      String name = "--" + long_name;
      if (short_name != null)
        name = String.format ("-%s %s", short_name, name);
      name += String.format ("=<%s>", type_name);
      return (name);
    }

    /**
     * Returns a one line description of the option
     */
    public String toString() {
      String short_name_str = "";
      if (short_name != null)
        short_name_str = "-" + short_name + " ";
      return String.format ("%s--%s field %s", short_name_str, long_name,
                            field);
    }

  }

  /** List of all of the defined options **/
  private List<OptionInfo> options = new ArrayList<OptionInfo>();

  /** Map from option names (with leading dashes) to option information **/
  private Map<String,OptionInfo> name_map
    = new LinkedHashMap<String,OptionInfo>();

  /**
   * Setup option processing on the specified array of objects or
   * classes.  If an element is a class, each of the options must be
   * static.  Otherwise the element is assumed to be an object that
   * contains some option fields.  In that case, the fields can be either
   * static or instance fields.  The names must be unique over all of
   * the elements
   */
  public Options (Object... classes) {

    // Loop through each specified object or class
    for (Object obj : classes) {

      if (obj instanceof Class) {

        Field[] fields = ((Class) obj).getDeclaredFields();
        for (Field f : fields) {
          Option option = f.getAnnotation (Option.class);
          if (option == null)
            continue;
          if (!Modifier.isStatic (f.getModifiers()))
            throw new Error ("non-static option " + f + " in class " + obj);
          options.add (new OptionInfo (f, option, null));
        }

      } else { // must be an object that contains option fields
        Field[] fields = obj.getClass().getDeclaredFields();
        for (Field f : fields) {
          Option option = f.getAnnotation (Option.class);
          if (option == null)
            continue;
          options.add (new OptionInfo (f, option, obj));
        }
      }
    }

    // Add each option to the option name map
    for (OptionInfo oi : options) {
      if (oi.short_name != null) {
        if (name_map.containsKey ("-" + oi.short_name))
          throw new Error ("short name " + oi + " appears twice");
        name_map.put ("-" + oi.short_name, oi);
      }
      if (name_map.containsKey ("--" + oi.long_name))
        throw new Error ("long name " + oi + " appears twice");
      name_map.put ("--" + oi.long_name, oi);
    }
  }

  /**
   * Parses a command line and sets the options accordingly.  Any
   * non-option arguments are returned.  Any unknown option or other
   * errors throws an ArgException
   */
  public String[] parse (String[] args) throws ArgException {

    List<String> non_options = new ArrayList<String>();

    // Loop through each argument
    for (int ii = 0; ii < args.length; ii++) {
      String arg = args[ii];
      if (arg.startsWith ("--")) {
        int eq_pos = arg.indexOf ('=');
        String arg_name = arg;
        String arg_value = null;
        if (eq_pos > 0) {
          arg_name = arg.substring (0, eq_pos);
          arg_value = arg.substring (eq_pos+1);
        }
        OptionInfo oi = name_map.get (arg_name);
        if (oi == null)
          throw new ArgException ("unknown argument '" + arg + "'");
        if (oi.argument_required() && (arg_value == null)) {
          ii++;
          if (ii >= args.length)
            throw new ArgException ("option %s requires an argument", arg);
          arg_value = args[ii];
        }
        set_arg (oi, arg_name, arg_value);
      } else if (arg.startsWith ("-")) {
        int eq_pos = arg.indexOf ('=');
        String arg_name = arg;
        String arg_value = null;
        if (eq_pos > 0) {
          arg_name = arg.substring (0, eq_pos);
          arg_value = arg.substring (eq_pos+1);
        }
        OptionInfo oi = name_map.get (arg_name);
        if (oi == null)
          throw new ArgException ("unknown argument '" + arg + "'");
        if (oi.argument_required() && (arg_value == null)) {
          ii++;
          if (ii >= args.length)
            throw new ArgException ("option %s requires an argument", arg);
          arg_value = args[ii];
        }
        set_arg (oi, arg_name, arg_value);
      } else { // not an option
        non_options.add (arg);
      }

    }
    return (non_options.toArray (new String[non_options.size()]));
  }

  /**
   * Parses a command line and sets the options accordingly.  Any
   * non-option arguments are returned.  If an error occurs prints
   * the usage and terminates the program.  The program is terminated
   * rather than throwing an error to create cleaner output.  See parse().
   */
  public String[] parse_and_usage (String[] args, String usage) {

    String non_options[] = null;

    try {
      non_options = parse (args);
    } catch (ArgException ae) {
      System.out.printf ("%s, Usage: %s%n", ae.getMessage(), usage);
      for (String use : usage()) {
        System.out.printf ("  %s%n", use);
      }
      System.exit (-1);
      // throw new Error ("usage error: ", ae);
    }
    return (non_options);
  }


  /**
   * Set the specified option to the value specified in arg_value.  Throws
   * an ArgException if there are any errors
   */
  public void set_arg (OptionInfo oi, String arg_name, String arg_value)
    throws ArgException {

    Field f = oi.field;
    Class type = f.getType();

    try {
      if ((type == Boolean.TYPE) || (type == Boolean.class)) {
        boolean val = false;
        if (arg_value == null) {
          val = true;
        } else {
          arg_value = arg_value.toLowerCase();
          if (arg_value.equals ("true") || (arg_value.equals ("t")))
            val = true;
          else if (arg_value.equals ("false") || arg_value.equals ("f"))
            val = false;
          else
            throw new ArgException ("Bad boolean value for %s: %s", arg_name,
                                    arg_value);
        }
        if (type == Boolean.class)
          f.set (oi.obj, new Boolean (val));
        else
          f.setBoolean (oi.obj, val);
      } else if ((type == Integer.TYPE) || (type == Integer.class)) {
        if (arg_value == null) {
          throw new ArgException ("Integer value required for argument "
                                  + arg_name);
        } else {
          int val = 0;
          try {
            val = Integer.decode (arg_value);
          } catch (Exception e) {
            throw new ArgException ("Invalid integer (%s) for argument %s",
                                    arg_value, arg_name);
          }
          if (type == Integer.class)
            f.set (oi.obj, new Integer(val));
          else
            f.setInt (oi.obj, val);
        }
      } else if (type == Double.TYPE) {
        if (arg_value == null)
          throw new ArgException ("Double value required for argument "
                                  + arg_name);
        Double val = 0.0;
        try {
          val = Double.valueOf (arg_value);
        } catch (Exception e) {
          throw new ArgException ("Invalid double (%s) for argument %s",
                                  arg_value, arg_name);
        }
        f.setDouble (oi.obj, val);
      } else if (type == String.class) {
        if (arg_value == null)
          throw new ArgException ("String value required for argument "
                                  + arg_name);
        f.set (oi.obj, arg_value);
      } else if (type == File.class) {
        if (arg_value == null)
          throw new ArgException ("Filename value required for argument "
                                  + arg_name);
        f.set (oi.obj, new File (arg_value));
      } else { // not a supported type
        throw new ArgException ("%s is not a valid option field", f);
      }
    } catch (ArgException ae) {
      throw ae;
    } catch (Exception e) {
      throw new Error ("Unexpected error ", e);
    }
  }


  /**
   * Returns an array of Strings describing the usage of the command line
   * options defined in options (one element per option)
   **/
  public String[] usage () {

    List<String> uses = new ArrayList<String>();

    // Determine the length of the longest option
    int max_len = 0;
    for (OptionInfo oi : options) {
      int len = oi.synopsis().length();
      if (len > max_len)
        max_len = len;
    }

    // Create the usage string
    for (OptionInfo oi : options) {
      String default_str = "[no default]";
      if (oi.default_str != null)
        default_str = String.format ("[default %s]", oi.default_str);
      String use = String.format ("%-" + max_len + "s - %s %s",
                                  oi.synopsis(), oi.description, default_str);
      uses.add (use);
    }

    return uses.toArray (new String[uses.size()]);

  }

  /**
   * Prints the specified message followed by usage information
   */
  public void print_usage (String format, Object... args) {

    System.out.printf (format, args);
    System.out.println ("Usage: ");
    for (String use : usage()) {
      System.out.printf ("  %s%n", use);
    }
  }

  /**
   * Returns a short name for the specified type for use in messages
   */
  private static String type_short_name (Class type) {

    if ((type == Boolean.TYPE) || (type == Boolean.class)) {
      return "boolean";
    } else if ((type == Integer.TYPE) || (type == Integer.class)) {
      return "int";
    } else if ((type == Double.TYPE) || (type == Double.class))  {
      return "double";
    } else if (type == String.class) {
      return "string";
    } else if (type == File.class) {
      return "filename";
    } else {
      return type.getName();
    }
  }

  /**
   * returns a string containing the current setting for each option
   */
  public String settings () {

    String out = "";

    // Determine the length of the longest name
    int max_len = 0;
    for (OptionInfo oi : options) {
      int len = oi.long_name.length();
      if (len > max_len)
        max_len = len;
    }

    // Create the settings string
    for (OptionInfo oi : options) {
      String use = String.format ("%-" + max_len + "s = ", oi.long_name);
      try {
        use += oi.field.get (oi.obj);
      } catch (Exception e) {
        throw new Error ("unexpected exception reading field " + oi.field, e);
      }
      out += String.format ("%s%n", use);
    }

    return (out);
  }

  /**
   * Returns all of the defined options on separate lines
   */
  public String toString() {

    String out = "";

    for (OptionInfo oi: options) {
      out += String.format ("%s%n", oi);
    }
    return (out);
  }


  /**
   * Exceptions encountered during argument processing
   */
  public static class ArgException extends Exception {
    static final long serialVersionUID = 20051223L;
    public ArgException (String s) { super (s); }
    public ArgException (String format, Object... args) {
      super (String.format (format, args));
    }
  }


  /**
   * Test  class with some defined arguments
   */
  public static class Test {

    @Option ("-a <filename> argument 1") String arg1 = "/tmp/foobar";
    @Option ("argument 2") String arg2;
    @Option ("-d double value") double temperature;
    @Option ("-f the input file") File input_file;
  }

  /**
   * Simple example
   */
  public static void main (String[] args) throws ArgException {

    Test t = new Test();
    Options options = new Options (new Test());
    // System.out.printf ("Options:%n%s", options);
    options.parse_and_usage (args, "test");
    System.out.printf ("Results:%n%s", options.settings());
  }
}
