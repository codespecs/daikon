package daikon.chicory;

import java.lang.reflect.Constructor;
import java.lang.reflect.Member;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.HashMap;
import java.util.List;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;
import org.checkerframework.checker.nullness.qual.RequiresNonNull;
import org.checkerframework.checker.signature.qual.ClassGetName;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;

/**
 * Keeps information about a method that is useful for writing out decl and/or dtrace information.
 * Original information is filled out during the transformation and other information is added the
 * first time a method is called.
 */
@SuppressWarnings("nullness") // to do.  member field is tricky.
public class MethodInfo {

  /** Class that contains this method. */
  public ClassInfo class_info;

  /**
   * Reflection information on this method. Null if a class initializer, {@code <clinit>} (see
   * {@link #is_class_initializer()}.
   */
  // The code often assumes that member != null.
  public @MonotonicNonNull Member member = null;

  /**
   * Method name. For example: "public static void sort(int[] arr)" would have method_name "sort".
   */
  public String method_name;

  /** Array of argument names for this method. */
  public String[] arg_names;

  /**
   * Array of argument types for this method (fully qualified). For example: "public static void
   * examineObject(Object x)" would have arg_types {"java.lang.Object"}.
   */
  public @ClassGetName String[] arg_type_strings;

  /** Array of argument types as classes for this method. */
  public Class<?>[] arg_types;

  /** Exit locations for this method. */
  public List<Integer> exit_locations;

  /** Tells whether each exit point in method is instrumented, based on filters. */
  public List<Boolean> is_included;

  /**
   * The root of the variable tree for the method entry program point.
   *
   * <p>Set by Runtime and read by DeclWriter and DTraceWriter.
   */
  public @MonotonicNonNull RootInfo traversalEnter = null;

  /**
   * The root of the variable tree for the method exit program point(s).
   *
   * <p>Set by Runtime and read by DeclWriter and DTraceWriter.
   */
  public @MonotonicNonNull RootInfo traversalExit = null;

  /** The number of times this method has been called. */
  public int call_cnt = 0;

  /** The number of times we have captured the output for this method. */
  public int capture_cnt = 0;

  /**
   * Whether or not the method is pure (has no side-effects). Will only be set to true if the {@code
   * --purity-analysis} command-line option is given to Chicory, and the method returns some value.
   * Only set during initViaReflection() method.
   */
  private boolean isPure;

  /** Creates a MethodInfo with the specified class, arg_names, and exit locations. */
  public MethodInfo(
      ClassInfo class_info,
      String method_name,
      String[] arg_names,
      @ClassGetName String[] arg_type_strings,
      List<Integer> exit_locations,
      List<Boolean> is_included) {

    this.class_info = class_info;
    this.method_name = method_name;
    this.arg_names = arg_names;
    this.arg_type_strings = arg_type_strings;
    this.exit_locations = exit_locations;
    this.is_included = is_included;
  }

  // Use reserved keyword for basic type rather than signature to
  // avoid conflicts with user defined types.
  private static HashMap<String, Class<?>> primitive_classes = new HashMap<>(8);

  static {
    primitive_classes.put("boolean", Boolean.TYPE);
    primitive_classes.put("byte", Byte.TYPE);
    primitive_classes.put("char", Character.TYPE);
    primitive_classes.put("double", Double.TYPE);
    primitive_classes.put("float", Float.TYPE);
    primitive_classes.put("int", Integer.TYPE);
    primitive_classes.put("long", Long.TYPE);
    primitive_classes.put("short", Short.TYPE);
  }

  /** Populates this class with data from reflection. */
  public void initViaReflection() {

    // Get the Class for each argument type
    arg_types = new Class<?>[arg_names.length];
    for (int ii = 0; ii < arg_type_strings.length; ii++) {
      try {
        String aname = arg_type_strings[ii];
        Class<?> c = primitive_classes.get(aname);

        if (c == null) {
          // c = Class.forName (aname);
          // change class loading
          // TODO referring class?
          c = Class.forName(aname, false, this.class_info.clazz.getClassLoader());
        }

        arg_types[ii] = c;
      } catch (Exception e) {
        throw new Error(
            "can't find class for "
                + arg_type_strings[ii]
                + " in  method "
                + class_info.class_name
                + "."
                + method_name
                + ": "
                + e);
      }
    }

    // Look up the method
    try {
      if (is_class_initializer()) {
        member = null;
        // This case DOES occur at run time.  -MDE 1/22/2010
      } else if (is_constructor()) {
        member = class_info.clazz.getDeclaredConstructor(arg_types);
      } else {
        member = class_info.clazz.getDeclaredMethod(method_name, arg_types);
      }
    } catch (Exception e) {
      throw new Error("can't find method " + method_name, e);
    }

    if (ChicoryPremain.shouldDoPurity() && (member != null)) {
      int mod = member.getModifiers();

      // Only consider purity on non-abstract, non-static, and non-constructor
      // methods which return a value!
      if (!Modifier.isAbstract(mod)
          && !Modifier.isStatic(mod)
          && !(member instanceof Constructor<?>)
          && !((Method) member).getReturnType().equals(Void.TYPE)) {
        if (ChicoryPremain.isMethodPure(member)) {
          isPure = true;
        }
      }
    }
  }

  /**
   * Returns true iff this method is a constructor.
   *
   * @return true iff this method is a constructor
   */
  @Pure
  public boolean is_constructor() {
    return method_name.equals("<init>") || method_name.equals("");
  }

  /**
   * Returns true iff this method is a class initializer.
   *
   * @return true iff this method is a class initializer
   */
  @Pure
  public boolean is_class_initializer() {
    return method_name.equals("<clinit>");
  }

  /**
   * Returns true iff this method is static.
   *
   * @return true iff this method is static
   */
  @RequiresNonNull("member")
  @Pure
  public boolean is_static() {
    return Modifier.isStatic(member.getModifiers());
  }

  /**
   * Initialize the enter and exit daikon variable trees (traversalEnter and traversalExit). The
   * reflection information must have already been initialized.
   */
  /*TO DO: @PostNonNull({"traversalEnter", "traversalExit"})*/
  public void init_traversal(int depth) {

    traversalEnter = RootInfo.enter_process(this, depth);
    // System.out.printf("Method %s.%s: %n ", class_info.clazz.getName(),
    //                    this);
    // System.out.printf("Enter daikon variable tree%n%s%n",
    //                    traversalEnter.treeString());

    traversalExit = RootInfo.exit_process(this, depth);
    // System.out.printf("Exit daikon variable tree%n%s%n",
    //                    traversalExit.treeString());
  }

  @SideEffectFree
  @Override
  public String toString(@GuardSatisfied MethodInfo this) {
    String out = "";
    if (class_info != null) {
      out = class_info.class_name + ".";
    }
    out += method_name + "(";
    for (int ii = 0; ii < arg_names.length; ii++) {
      if (ii > 0) {
        out += ", ";
      }
      out += arg_type_strings[ii] + " " + arg_names[ii];
    }
    return (out + ")");
  }

  public boolean isPure() {
    return isPure;
  }

  /**
   * Returns the return type of this method, or Void.TYPE for a constructor.
   *
   * @return the return type of this method
   */
  public Class<?> return_type() {
    if (member instanceof Method) {
      Method m = (Method) member;
      return m.getReturnType();
    } else {
      return Void.TYPE;
    }
  }
}
