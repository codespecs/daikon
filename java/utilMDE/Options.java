// The two files
//   Option.java
//   Options.java
// together comprise the implementation of command-line processing.

package utilMDE;

import java.io.*;
import java.util.*;
import java.util.regex.*;
import java.lang.reflect.*;
import java.lang.annotation.*;
import com.sun.javadoc.RootDoc;
import com.sun.javadoc.ClassDoc;
import com.sun.javadoc.FieldDoc;

/**
 * The Options class parses command-line options and sets fields in your
 * program accordingly.  Each field that is annotated with @{@link
 * utilMDE.Option} is set from a command-line argument of the same name.
 * The Options class can also create usage messages and HTML documentation.
 * The Options class interprets annotations and Javadoc comments, so that
 * you do not have to write duplicative, boilerplate code and
 * documentation that could get out of sync with the rest of your program.
 * <p>
 *
 * The main entry point is {@link #parse_or_usage(String[])}.
 * Typical use in your program is:
 * <!-- Maybe expand this example a bit. -->
 * <pre>
 *    public static void main(String[] args) {
 *      MyProgram myInstance = new MyProgram();
 *      Options options = new Options("MyProgram [options] infile outfile",
 *                                    myInstance, MyUtilityClass.class);
 *      // Sets fields in object instance, and sets static fields in
 *      // class MyUtilityClass.
 *      // Returns the original command line, with all options removed.
 *      String[] file_args = options.parse_or_usage (args);
 *      ...
 * </pre>
 *
 * The {@link Option} annotation on a field specifies user documentation
 * and, optionally, a one-character short name that users may supply on the
 * command line.  The long name is taken from the name of the variable;
 * when the name contains an underscore, users may substitute a hyphen on
 * the command line instead. <p>
 *
 * On the command line, the values for options are specified in the form
 * '--longname=value', '-shortname=value', '--longname value', or
 * '-shortname value'.  The value is mandatory for all options except
 * booleans.  Booleans are set to true if no value is specified. <p>
 *
 * All arguments that start with '-' are processed as options.  To
 * terminate option processing at the first non-option argument, see {@link
 * #parse_options_after_arg(boolean)}.  Also, the special option '--'
 * terminates option processing; all subsequent arguments are passed to the
 * program (along with any preceding non-option arguments) without being
 * scanned for options. <p>
 *
 * An option may be specified multiple times.  If the field is a
 * list, each entry is be added to the list.  If the field is not a
 * list, then only the last occurrence is used (subsequent occurrences
 * overwrite the previous value).  <p>
 *
 * The field may be of the following types:
 * <ul>
 *   <li>Primitive types:  boolean, int, and double.
 *       (Primitives can also be represented as wrappers (Boolean,
 *       Integer, Double).  Use of a wrapper type allows the argument
 *       to have no default value.)
 *   <li>Reference types that have a constructor with a single string
 *       parameter.
 *   <li>java.util.regex.Pattern.
 *   <li>Lists of any supported reference type.  Lists must be initialized
 *       to a valid list (e.g., the empty list) before using Options on
 *       that list.
 * </ul> <p>
 *
 * <b>Example:</b> <p>
 *
 * <!-- Example needs some more words of explanation and example command lines. -->
 * <!-- Given this code: --> <pre>
 *
 *  public static class Test {
 *
 *    &#64;Option ("-o &lt;filename&gt; the output file ")
 *    public static File outfile = "/tmp/foobar";
 *
 *    &#64;Option ("-i ignore case")
 *    public static boolean ignore_case;
 *
 *    &#64;Option ("-t set the initial temperature")
 *    public static double temperature = 75.0;
 *
 *    public static void main (String[] args) {
 *      Options options = new Options ("Test [options] files", new Test());
 *      String[] file_args = options.parse_or_usage (args);
 *    }
 *  }
 *</pre>
 *
 * Limitations: <ul>
 *
 *  <li> Short options are only supported as separate entries
 *  (e.g., "-a -b") and not as a single group (e.g., "-ab").
 *
 *  <li> Not all primitive types are supported.
 *
 *  <li> Types without a string constructor are not supported.
 *
 *  <li> Non-option information could be supported in the same manner
 *  as options which would be cleaner than simply returning all of the
 *  non-options as an array.
 *
 *  <li> The "--no-long" option to turn off a boolean option named "long"
 *  is not supported; use "--long=false" instead.
 * </ul>
 *
 * @see utilMDE.Option
 **/
public class Options {

  /** Information about an option **/
  private class OptionInfo {

    /** Field containing the value of the option **/
    Field field;

    /** Option information for the field **/
    Option option;

    /** Object containing the field.  Null if the field is static. **/
    /*@Nullable*/ Object obj;

    /** Short (one character) argument name **/
    /*@Nullable*/ String short_name;

    /** Long argument name **/
    String long_name;

    /** Argument description **/
    String description;

    /** Javadoc description **/
    /*@Nullable*/ String jdoc;

    /**
     * Name of the argument type.  Defaults to the type of the field, but
     * user can override this in the option string.
     */
    String type_name;

    /**
     * Class type of this field.  If the field is a list, the basetype
     * of the list.
     */
    Class<?> base_type;

    /** Default value of the option as a string **/
    /*@Nullable*/ String default_str = null;

    /** If the option is a list, this references that list. **/
    // Not type-safe; must suppress warnings
    /*@Nullable*/ List<Object> list = null;

    /** Constructor that takes one String for the type **/
    /*@Nullable*/ Constructor<?> constructor = null;

    /** Factory that takes a string (some classes don't have a string constructor) */
    /*@Nullable*/ Method factory = null;

    /**
     * Create the specified option.  If obj is null, the field must be
     * static.  The short name, type name, and description are taken
     * from the option annotation.  The long name is the name of the
     * field.  The default value is the current value of the field.
     */
    OptionInfo (Field field, Option option, /*@Nullable*/ /*@Raw*/ Object obj) {
      this.field = field;
      this.option = option;
      @SuppressWarnings("rawness")
      /*@NonRaw*/ Object objNonRaw = obj;
      obj = objNonRaw;
      this.obj = objNonRaw;
      this.base_type = field.getType();

      // The long name is the name of the field
      long_name = field.getName();
      if (use_dashes)
        long_name = long_name.replace ('_', '-');

      // Get the default value (if any)
      Object default_obj = null;
      if (!Modifier.isPublic (field.getModifiers()))
        throw new Error ("option field is not public: " + field);
      try {
        default_obj = field.get (objNonRaw);
        if (default_obj != null)
          default_str = default_obj.toString();
      } catch (Exception e) {
        throw new Error ("Unexpected error getting default for " + field, e);
      }

      // Handle lists.  When a list argument is specified multiple times,
      // each argument value is appended to the list.
      Type gen_type = field.getGenericType();
      if (gen_type instanceof ParameterizedType) {
        ParameterizedType pt = (ParameterizedType) gen_type;
        Type raw_type = pt.getRawType();
        if (!raw_type.equals (List.class))
          throw new Error ("Unsupported option type " + pt);
        @SuppressWarnings("unchecked")
        List<Object> default_obj_as_list = (List<Object>) default_obj;
        this.list = default_obj_as_list;
        if (this.list == null)
          throw new Error ("List option " + field + " must be initialized");
        // System.out.printf ("list default = %s%n", list);
        this.base_type = (Class<?>) pt.getActualTypeArguments()[0];

        // System.out.printf ("Param type for %s = %s%n", field, pt);
        // System.out.printf ("raw type = %s, type = %s%n", pt.getRawType(),
        //                   pt.getActualTypeArguments()[0]);
      }

      // Get the short name, type name, and description from the annotation
      ParseResult pr = parse_option (option.value());
      short_name = pr.short_name;
      if (pr.type_name != null) {
        type_name = pr.type_name;
      } else {
        type_name = type_short_name (base_type);
        if (list != null)
          type_name += "[]";
      }
      description = pr.description;

      // Get a constructor for non-primitive base types
      if (!base_type.isPrimitive() && !base_type.isEnum()) {
        try {
          if (base_type == Pattern.class) {
            factory = Pattern.class.getMethod ("compile", String.class);
          } else { // look for a string constructor
            assert base_type != null; // nullness checker: problem with flow
            constructor = base_type.getConstructor (String.class);
          }
        } catch (Exception e) {
          throw new Error ("Option " + field
                           + " does not have a string constructor", e);
        }
      }
    }

    /**
     * Returns whether or not this option has a required argument.
     */
    public boolean argument_required() {
      Class<?> type = field.getType();
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
     * Returns a one-line description of the option.
     */
    public String toString() {
      String short_name_str = "";
      if (short_name != null)
        short_name_str = "-" + short_name + " ";
      return String.format ("%s--%s field %s", short_name_str, long_name,
                            field);
    }

    /** Returns the class that declares this option. **/
    public Class<?> get_declaring_class() {
      return field.getDeclaringClass();
    }
  }

  /**
   * Whether to parse options after a non-option command-line argument.
   * @see #parse_options_after_arg(boolean)
   **/
  private boolean parse_options_after_arg = true;

  /** All of the argument options as a single string **/
  private String options_str = "";

  /** First specified class.  Void stands for "not yet initialized". **/
  private Class<?> main_class = Void.TYPE;

  /** List of all of the defined options **/
  private List<OptionInfo> options = new ArrayList<OptionInfo>();

  /** Map from option names (with leading dashes) to option information **/
  private Map<String,OptionInfo> name_map
    = new LinkedHashMap<String,OptionInfo>();

  /**
   * Convert underscores to dashes in long options in usage messages.  Uses
   * may specify either the the underscore or dashed name on the command
   * line.
   */
  private boolean use_dashes = true;

  /**
   * Synopsis of usage.  Example:  "prog [options] arg1 arg2 ..."
   * <p>
   * This variable is public so that clients can reset it (useful for
   * masquerading as another program, based on parsed options).
   **/
  public /*@Nullable*/ String usage_synopsis = null;

  // Debug loggers
  private SimpleLog debug_options = new SimpleLog (false);

  /**
   * Prepare for option processing.  Creates an object that will set fields
   * in all the given arguments.  An argument to this method may be a
   * Class, in which case its static fields are set.  The names of all the
   * options (that is, the fields annotated with &#064;{@link Option}) must be
   * unique across all the arguments.
   */
  public Options (/*@Raw*/ Object... args) {
    this ("", args);
  }

  /**
   * Prepare for option processing.  Creates an object that will set fields
   * in all the given arguments.  An argument to this method may be a
   * Class, in which case its static fields are set.  The names of all the
   * options (that is, the fields annotated with &#064;{@link Option}) must be
   * unique across all the arguments.
   * @param usage_synopsis A synopsis of how to call your program
   */
  public Options (String usage_synopsis, /*@Raw*/ Object... args) {

    if (args.length == 0) {
      throw new Error("Must pass at least one object to Options constructor");
    }

    this.usage_synopsis = usage_synopsis;

    @SuppressWarnings("cast")
    Object[] argsNonRaw = (Object[]) args;

    // Loop through each specified object or class
    for (Object obj : argsNonRaw) {

      if (obj instanceof Class<?>) {

        if (main_class == Void.TYPE)
          main_class = (Class<?>) obj;

        Field[] fields = ((Class<?>) obj).getDeclaredFields();
        for (Field f : fields) {
          debug_options.log ("Considering field %s with annotations %s%n", f,
                             Arrays.toString(f.getDeclaredAnnotations()));
          Option option = f.getAnnotation (Option.class);
          if (option == null)
            continue;
          if (!Modifier.isStatic (f.getModifiers()))
            throw new Error ("non-static option " + f + " in class " + obj);
          options.add (new OptionInfo (f, option, null));
        }

      } else { // must be an object that contains option fields

        if (main_class == Void.TYPE)
          main_class = obj.getClass();

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
      if (use_dashes && oi.long_name.contains ("-"))
        name_map.put ("--" + oi.long_name.replace ('-', '_'), oi);
    }
  }

  /**
   * If true, Options will parse arguments even after a non-option
   * command-line argument.  Setting this to true is useful to permit users
   * to write options at the end of a command line.  Setting this to false
   * is useful to avoid processing arguments that are actually
   * options/arguments for another program that this one will invoke.
   */
  public void parse_options_after_arg (boolean val) {
    parse_options_after_arg = val;
  }

  /** @deprecated Use {@link #parse_options_after_arg(boolean)}. */
  @Deprecated
  public void ignore_options_after_arg (boolean val) {
    parse_options_after_arg = !val;
  }

  /**
   * Parses a command line and sets the options accordingly.
   * @return all non-option arguments
   * @throws ArgException if the command line contains unknown option or
   * misused options.
   */
  public String[] parse (String[] args) throws ArgException {

    List<String> non_options = new ArrayList<String>();
    boolean ignore_options = false;

    // Loop through each argument
    for (int ii = 0; ii < args.length; ii++) {
      String arg = args[ii];
      if (arg.equals ("--")) {
        ignore_options = true;
      } else if (arg.startsWith ("--") && !ignore_options) {
        int eq_pos = arg.indexOf ('=');
        String arg_name = arg;
        String arg_value = null;
        if (eq_pos > 0) {
          arg_name = arg.substring (0, eq_pos);
          arg_value = arg.substring (eq_pos+1);
        }
        OptionInfo oi = name_map.get (arg_name);
        if (oi == null)
          throw new ArgException ("unknown option '" + arg + "'");
        if (oi.argument_required() && (arg_value == null)) {
          ii++;
          if (ii >= args.length)
            throw new ArgException ("option %s requires an argument", arg);
          arg_value = args[ii];
        }
        // System.out.printf ("arg_name = '%s', arg_value='%s'%n", arg_name,
        //                    arg_value);
        set_arg (oi, arg_name, arg_value);
      } else if (arg.startsWith ("-") && !ignore_options) {
        int eq_pos = arg.indexOf ('=');
        String arg_name = arg;
        String arg_value = null;
        if (eq_pos > 0) {
          arg_name = arg.substring (0, eq_pos);
          arg_value = arg.substring (eq_pos+1);
        }
        OptionInfo oi = name_map.get (arg_name);
        if (oi == null)
          throw new ArgException ("unknown option '" + arg + "'");
        if (oi.argument_required() && (arg_value == null)) {
          ii++;
          if (ii >= args.length)
            throw new ArgException ("option %s requires an argument", arg);
          arg_value = args[ii];
        }
        set_arg (oi, arg_name, arg_value);
      } else { // not an option
        if (! parse_options_after_arg)
          ignore_options = true;
        non_options.add (arg);
      }

    }
    String[] result = non_options.toArray (new String[non_options.size()]);
    return result;
  }

  /**
   * Parses a command line and sets the options accordingly.  This method
   * splits the argument string into command line arguments, respecting
   * single and double quotes, then calls parse(String[]).
   * @return all non-option arguments
   * @throws ArgException if the command line contains unknown option or
   * misused options.
   * @see #parse(String[])
   */
  public String[] parse (String args) throws ArgException {

    // Split the args string on whitespace boundaries accounting for quoted
    // strings.
    args = args.trim();
    List<String> arg_list = new ArrayList<String>();
    String arg = "";
    char active_quote = 0;
    for (int ii = 0; ii < args.length(); ii++) {
      char ch = args.charAt (ii);
      if ((ch == '\'') || (ch == '"')) {
        arg+= ch;
        ii++;
        while ((ii < args.length()) && (args.charAt(ii) != ch))
          arg += args.charAt(ii++);
        arg += ch;
      } else if (Character.isWhitespace (ch)) {
        // System.out.printf ("adding argument '%s'%n", arg);
        arg_list.add (arg);
        arg = "";
        while ((ii < args.length()) && Character.isWhitespace(args.charAt(ii)))
          ii++;
        if (ii < args.length())
          ii--;
      } else { // must be part of current argument
        arg += ch;
      }
    }
    if (!arg.equals (""))
      arg_list.add (arg);

    String[] argsArray = arg_list.toArray (new String[arg_list.size()]);
    return parse (argsArray);
  }

  /**
   * Parses a command line and sets the options accordingly.  If an error
   * occurs, prints the usage and terminates the program.  The program is
   * terminated rather than throwing an error to create cleaner output.
   * @return all non-option arguments
   * @see #parse(String[])
   */
  public String[] parse_or_usage (String[] args) {

    String non_options[] = null;

    try {
      non_options = parse (args);
    } catch (ArgException ae) {
      String message = ae.getMessage();
      if (message != null) {
        print_usage (message);
      } else {
        print_usage ();
      }
      System.exit (-1);
      // throw new Error ("usage error: ", ae);
    }
    return (non_options);
  }

  /**
   * Parses a command line and sets the options accordingly.  If an error
   * occurs, prints the usage and terminates the program.  The program is
   * terminated rather than throwing an error to create cleaner output.
   * This method splits the argument string into command line arguments,
   * respecting single and double quotes, then calls parse_or_usage(String[]).
   * @return all non-option arguments
   * @see #parse_or_usage(String[])
   */
  public String[] parse_or_usage (String args) {

    String non_options[] = null;

    try {
      non_options = parse (args);
    } catch (ArgException ae) {
      String message = ae.getMessage();
      if (message != null) {
        print_usage (message);
      } else {
        print_usage ();
      }
      System.exit (-1);
      // throw new Error ("usage error: ", ae);
    }
    return (non_options);
  }

  /** @deprecated Use {@link #parse_or_usage(String[])}. */
  @Deprecated
  public String[] parse_and_usage (String[] args) {
    return parse_or_usage(args);
  }

  /** @deprecated Use {@link #parse_or_usage(String)}. */
  @Deprecated
  public String[] parse_and_usage (String args) {
    return parse_or_usage(args);
  }


  /// This is a lot of methods, but it does save a tad of typing for the
  /// programmer.

  /**
   * Prints usage information.  Uses the usage synopsis passed into the
   * constructor, if any.
   */
  public void print_usage (PrintStream ps) {
    if (usage_synopsis != null) {
      ps.printf ("Usage: %s%n", usage_synopsis);
    }
    for (String use : usage()) {
      ps.printf ("  %s%n", use);
    }
  }

  /**
   * Prints, to standard output, usage information.
   */
  public void print_usage () {
    print_usage (System.out);
  }


  // This method is distinct from
  //   print_usage (PrintStream ps, String format, Object... args)
  // because % characters in the message are not interpreted.
  /**
   * Prints a message followed by indented usage information.
   * The message is printed in addition to (not replacing) the usage synopsis.
   **/
  public void print_usage (PrintStream ps, String msg) {
    ps.println (msg);
    print_usage (ps);
  }

  /**
   * Prints, to standard output, a message followed by usage information.
   * The message is printed in addition to (not replacing) the usage synopsis.
   **/
  public void print_usage (String msg) {
    print_usage (System.out, msg);
  }

  /**
   * Prints a message followed by usage information.
   * The message is printed in addition to (not replacing) the usage synopsis.
   */
  public void print_usage (PrintStream ps, String format, /*@Nullable*/ Object... args) {
    ps.printf (format, args);
    if (! format.endsWith("%n")) {
      ps.println();
    }
    print_usage (ps);
  }

  /**
   * Prints, to standard output, a message followed by usage information.
   * The message is printed in addition to (not replacing) the usage synopsis.
   */
  public void print_usage (String format, /*@Nullable*/ Object... args) {
    print_usage(System.out, format, args);
  }

  /**
   * Returns an array of Strings, where each String describes the usage of
   * one command-line option.  Does not include the usage synopsis.
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

    String[] result = uses.toArray (new String[uses.size()]);
    return result;

  }

  /**
   * Set the specified option to the value specified in arg_value.  Throws
   * an ArgException if there are any errors.
   */
  public void set_arg (OptionInfo oi, String arg_name, /*@Nullable*/ String arg_value)
    throws ArgException {

    Field f = oi.field;
    Class<?> type = oi.base_type;

    // Keep track of all of the options specified
    if (options_str.length() > 0)
      options_str += " ";
    options_str += arg_name;
    if (arg_value != null) {
      if (! arg_value.contains (" ")) {
        options_str += "=" + arg_value;
      } else if (! arg_value.contains ("'")) {
        options_str += "='" + arg_value + "'";
      } else if (! arg_value.contains ("\"")) {
        options_str += "=\"" + arg_value + "\"";
      } else {
        throw new ArgException("Can't quote for interal debugging: " + arg_value);
      }
    }
    // Argument values are required for everything but booleans
    if (arg_value == null) {
      if ((type != Boolean.TYPE)
          || (type != Boolean.class)) {
        arg_value = "true";
      } else {
        throw new ArgException ("Value required for option " + arg_name);
      }
    }

    try {
      if (type.isPrimitive()) {
        if (type == Boolean.TYPE) {
          boolean val;
          arg_value = arg_value.toLowerCase();
          if (arg_value.equals ("true") || (arg_value.equals ("t")))
            val = true;
          else if (arg_value.equals ("false") || arg_value.equals ("f"))
            val = false;
          else
            throw new ArgException ("Bad boolean value for %s: %s", arg_name,
                                    arg_value);
          arg_value = (val) ? "true" : "false";
          // System.out.printf ("Setting %s to %s%n", arg_name, val);
          f.setBoolean (oi.obj, val);
        } else if (type == Integer.TYPE) {
          int val = 0;
          try {
            val = Integer.decode (arg_value);
          } catch (Exception e) {
            throw new ArgException ("Invalid integer (%s) for argument %s",
                                    arg_value, arg_name);
          }
          f.setInt (oi.obj, val);
        } else if (type == Double.TYPE) {
          Double val = 0.0;
          try {
            val = Double.valueOf (arg_value);
          } catch (Exception e) {
            throw new ArgException ("Invalid double (%s) for argument %s",
                                    arg_value, arg_name);
          }
          f.setDouble (oi.obj, val);
        } else { // unexpected type
          throw new Error ("Unexpected type " + type);
        }
      } else { // reference type

        // Create an instance of the correct type by passing the argument value
        // string to the constructor.  The only expected error is some sort
        // of parse error from the constructor.
        Object val = null;
        try {
          if (oi.constructor != null) {
            val = oi.constructor.newInstance (arg_value);
          } else if (oi.base_type.isEnum()) {
            @SuppressWarnings({"unchecked","rawness","rawtypes"}) /// XXX rawness bug
            Object tmpVal = Enum.valueOf ((Class<? extends Enum>)oi.base_type, arg_value);
            val = tmpVal;
          } else {
            if (oi.factory == null) {
              throw new Error("No constructor or factory for argument " + arg_name);
            }
            val = oi.factory.invoke (null, arg_value);
          }
        } catch (Exception e) {
          throw new ArgException ("Invalid argument (%s) for argument %s",
                                  arg_value, arg_name);
        }

        assert val != null : "@SuppressWarnings(nullness)";

        // Set the value
        if (oi.list != null)
          oi.list.add (val); // unchecked cast
        else
          f.set (oi.obj, val);
      }
    } catch (ArgException ae) {
      throw ae;
    } catch (Exception e) {
      throw new Error ("Unexpected error ", e);
    }
  }


  /**
   * Returns a short name for the specified type for use in messages.
   */
  private static String type_short_name (Class<?> type) {

    if (type.isPrimitive())
      return type.getName();
    else if (type == File.class)
      return "filename";
    else if (type == Pattern.class)
      return "regex";
    else if (type.isEnum())
      return ("enum");
    else
      return UtilMDE.unqualified_name (type.getName()).toLowerCase();
  }

  /**
   * Returns a string containing all of the options that were set and their
   * arguments.  This is essentially the contents of args[] with all
   * non-options removed.
   * @see #settings()
   */
  public String get_options_str() {
    return (options_str);
  }

  /**
   * Returns a string containing the current setting for each option, in a
   * format that can be parsed by Options.  This differs from
   * get_options_str() in that it contains each known option exactly once:
   * it never contains duplicates, and it contains every known option even
   * if the option was not specified on the command line.
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
   * Returns a description of all of the known options.
   * Each option is described on its own line in the output.
   */
  public String toString() {

    String out = "";

    for (OptionInfo oi: options) {
      out += String.format ("%s%n", oi);
    }
    return (out);
  }


  /**
   * Exceptions encountered during argument processing.
   */
  public static class ArgException extends Exception {
    static final long serialVersionUID = 20051223L;
    public ArgException (String s) { super (s); }
    public ArgException (String format, /*@Nullable*/ Object... args) {
      super (String.format (format, args));
    }
  }

  /**
   * Entry point for creating HTML documentation.
   */
  public void jdoc (RootDoc doc) {

    // Find the overall documentation (on the main class)
    ClassDoc main = find_class_doc (doc, main_class);
    if (main == null) {
      throw new Error ("can't find main class " + main_class);
    }

    // Process each option and add in the javadoc info
    for (OptionInfo oi : options) {
      ClassDoc opt_doc = find_class_doc (doc, oi.get_declaring_class());
      String nameWithUnderscores = oi.long_name.replace('-', '_');
      if (opt_doc != null) {
        for (FieldDoc fd : opt_doc.fields()) {
          if (fd.name().equals (nameWithUnderscores)) {
            oi.jdoc = fd.commentText();
            break;
          }
        }
      }
    }

    // Write out the info as HTML
    System.out.println (main.commentText());
    System.out.println ("<p>Command line options: <p>");
    System.out.println ("<ul>");
    for (OptionInfo oi : options) {
      String default_str = "[no default]";
      if (oi.default_str != null)
        default_str = String.format ("[default %s]", oi.default_str);
      String synopsis = oi.synopsis();
      synopsis = synopsis.replaceAll ("<", "&lt;");
      System.out.printf ("  <li> <b>%s</b>. %s %s<p>%n", synopsis,
                         oi.jdoc, default_str);
    }
    System.out.println ("</ul>");

  }

  /*@Nullable*/ ClassDoc find_class_doc (RootDoc doc, Class<?> c) {

    for (ClassDoc cd : doc.classes()) {
      if (cd.qualifiedName().equals (c.getName())) {
        return cd;
      }
    }
    return (null);
  }

  private static class ParseResult {
    /*@Nullable*/ String short_name;
    /*@Nullable*/ String type_name;
    String description;
    ParseResult(/*@Nullable*/ String short_name, /*@Nullable*/ String type_name, String description) {
      this.short_name = short_name;
      this.type_name = type_name;
      this.description = description;
    }
  }


  /**
   * Parse an option value and return its three components (short_name,
   * type_name, and description).  The short_name and type_name are null
   * if they are not specified in the string.
   */
  private static ParseResult parse_option (String val) {

    // Get the short name, long name, and description
    String short_name;
    String type_name;
    /*@NonNull*/ String description;

    // Get the short name (if any)
    if (val.startsWith("-")) {
      assert val.substring(2,3).equals(" ");
      short_name = val.substring (1, 2);
      description = val.substring (3);
    } else {
      short_name = null;
      description = val;
    }

    // Get the type name (if any)
    if (description.startsWith ("<")) {
      type_name = description.substring (1).replaceFirst (">.*", "");
      description = description.replaceFirst ("<.*> ", "");
    } else {
      type_name = null;
    }

    // Return the result
    return new ParseResult(short_name, type_name, description);
  }

//   /**
//    * Test class with some defined arguments.
//    */
//   private static class Test {
//
//     @Option ("generic") List<Pattern> lp = new ArrayList<Pattern>();
//     @Option ("-a <filename> argument 1") String arg1 = "/tmp/foobar";
//     @Option ("argument 2") String arg2;
//     @Option ("-d double value") double temperature;
//     @Option ("-f the input file") File input_file;
//   }
//
//   /**
//    * Simple example
//    */
//   private static void main (String[] args) throws ArgException {
//
//     Options options = new Options ("test", new Test());
//     System.out.printf ("Options:%n%s", options);
//     options.parse_or_usage (args);
//     System.out.printf ("Results:%n%s", options.settings());
//   }

}
