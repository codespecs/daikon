package daikon.chicory;

import java.io.*;
import java.util.*;
import java.lang.reflect.*;

/**
 * Keeps information about a method that is useful for writing out
 * decl and/or dtrace information.  Original information is filled
 * out during the transformation and other information is added the
 * first time a method is called.
 */
public class MethodInfo {

  /** Class that contains this method **/
  public ClassInfo class_info = null;

  /** Reflection information on this method **/
  public Member member = null;

  /** Method name **/
  public String method_name;

  /** Array of argument names for this method **/
  public String[] arg_names;

  /** Array of argument types for this method **/
  public String[] arg_type_strings;

  /** Array of argument types as classes for this method **/
  public Class[] arg_types;

  /** exit locations for this method **/
  public List <Integer> exit_locations;

  /** Tells whether each exit point in method is instrumented, based on filters **/
  public List <Boolean> is_included;
  /**
   * Creates a MethodInfo with the specified class, arg_names, and
   * exit locations
   */
  public MethodInfo (ClassInfo class_info, String method_name,
                     String[] arg_names, String[] arg_type_strings,
                     List <Integer> exit_locations,
                     List <Boolean> is_included) {

    this.class_info = class_info;
    this.method_name = method_name;
    this.arg_names = arg_names;
    this.arg_type_strings = arg_type_strings;
    this.exit_locations = exit_locations;
    this.is_included = is_included;
  }

  private static HashMap primitive_classes = new HashMap(8);
  static {
    primitive_classes.put("Z", Boolean.TYPE);
    primitive_classes.put("B", Byte.TYPE);
    primitive_classes.put("C", Character.TYPE);
    primitive_classes.put("D", Double.TYPE);
    primitive_classes.put("F", Float.TYPE);
    primitive_classes.put("I", Integer.TYPE);
    primitive_classes.put("J", Long.TYPE);
    primitive_classes.put("S", Short.TYPE);
  }

  public void get_reflection () {

    // Get the Class for each argument type
    arg_types = new Class[arg_names.length];
    for (int ii = 0; ii < arg_type_strings.length; ii++) {
      try {
        String aname = arg_type_strings[ii];
        Class c = (Class) primitive_classes.get (aname);
        
        if (c == null)
        {
          //c = Class.forName (aname);
          //change class loading
          //TODO referring class?
          c = Class.forName (aname, false, this.class_info.clazz.getClassLoader());
        }
        
        arg_types[ii] = c;
      } catch (Exception e) {
        throw new Error ("can't find class for " + arg_type_strings[ii]
                         + " in  method "+ class_info.class_name + "."
                         + method_name + ": " + e);
      }
    }

    // Look up the method
    try {
      if (is_constructor())
        member = class_info.clazz.getDeclaredConstructor (arg_types);
      else
        member = class_info.clazz.getDeclaredMethod (method_name, arg_types);
    } catch (Exception e) {
      throw new Error ("can't find method " + method_name + " : " + e);
    }
  }

  public boolean is_constructor() {
    return (method_name.equals ("<init>") || method_name.equals(""));
  }

}
