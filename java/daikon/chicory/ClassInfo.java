package daikon.chicory;

import java.io.*;
import java.util.*;
import java.lang.reflect.*;

/**
 * Keeps information about a class that is useful for writing out
 * decl and/or dtrace information.  Original information is filled
 * out during the transformation and other information is added the
 * after the class is first loaded
 */
public class ClassInfo {

  /** fully qualified name of the  class **/
  public String class_name;

  /** reflection object for this class **/
  public Class clazz;

  /** list of methods in the class **/
  public List<MethodInfo> method_infos;

  /** this class's classloader**/
  private ClassLoader loader;

  /** Mapping from field name to string representation of its value**/
  //only for static final primitives
  //which are declared by a CONSTANT VALUE in the code
  public Map <String, String> staticMap = new HashMap<String,String>();

  /** Create ClassInfo with specified name **/
  public ClassInfo (String class_name, ClassLoader theLoader) {
    this.class_name = class_name;
    loader = theLoader;
  }

  /** Set the list of methods **/
  public void set_method_infos (List<MethodInfo> method_infos) {
    this.method_infos = method_infos;
  }

  public List<MethodInfo> get_method_infos() {
    return (method_infos);
  }

  /**
   * Gets the reflection object Class for this class and the Method objects
   * for each method
   */
  public void get_reflection() {

    // get the reflection class
    try {
      //clazz = Class.forName (class_name);
      //change class loading

        //TODO referring class?
      clazz = Class.forName (class_name, false, loader);

    } catch (Exception e) {
      throw new Error (e);
    }

    for (MethodInfo mi : method_infos)
      mi.get_reflection();
  }

  /** dumps all of the class info to the specified stream **/
  public void dump (PrintStream ps) {
    ps.printf ("ClassInfo for %s [%s]\n", class_name, clazz);
    for (MethodInfo mi : method_infos) {
      ps.printf ("  method %s [%s]\n", mi.method_name, mi.member);
      ps.printf ("    arguments: ");
      for (int ii = 0; ii < mi.arg_names.length; ii++) {
        if (ii > 0)
          ps.printf (", ");
        ps.printf ("%s [%s] %s", mi.arg_type_strings[ii], mi.arg_types[ii],
                   mi.arg_names[ii]);
      }
      ps.printf ("\n    exits: ");
      for (Integer exit_loc : mi.exit_locations)
        ps.printf ("%s ", exit_loc);
      ps.printf ("\n");
    }
  }
}
