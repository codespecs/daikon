package daikon.chicory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.EnsuresNonNull;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;
import org.checkerframework.checker.signature.qual.BinaryName;
import org.checkerframework.dataflow.qual.SideEffectFree;

/**
 * Keeps information about a class that is useful for writing out decl and/or dtrace information.
 * Original information is filled out during the transformation and other information is added after
 * the class is first loaded.
 */
public class ClassInfo {

  /** binary name of the class. */
  public @BinaryName String class_name;

  // set by initViaReflection()
  /** reflection object for this class. */
  public @MonotonicNonNull Class<?> clazz;

  // Does not include class initializers, so each element's .member field
  // is non-null.
  /** list of methods in the class. */
  public List<MethodInfo> method_infos = new ArrayList<>();

  /** This class's classloader. */
  private ClassLoader loader;

  // traversalClass and traversalObject are set by init_traversal().
  /** DaikonVariables for the object program point (instance and static variables). */
  public @MonotonicNonNull RootInfo traversalObject;

  /** DaikonVariables for the class program point (static variables only). */
  public @MonotonicNonNull RootInfo traversalClass;

  /** Whether or not any methods in this class were instrumented. */
  public boolean shouldInclude = false;

  /** Mapping from field name to string representation of its value* */
  // only for static final primitives
  // which are declared by a CONSTANT VALUE in the code
  public Map<String, String> staticMap = new HashMap<>();

  /** Create ClassInfo with specified name. */
  public ClassInfo(@BinaryName String class_name, ClassLoader theLoader) {
    this.class_name = class_name;
    loader = theLoader;
  }

  /** Set the list of methods. */
  public void set_method_infos(List<MethodInfo> method_infos) {
    this.method_infos = method_infos;
  }

  public List<MethodInfo> get_method_infos() {
    return method_infos;
  }

  /**
   * Gets the reflection object Class for this class, and the Method objects for each method that is
   * already in method_infos.
   */
  @EnsuresNonNull("clazz")
  public void initViaReflection() {

    // get the reflection class
    try {
      // clazz = Class.forName (class_name);
      // change class loading

      // TODO referring class?
      clazz = Class.forName(class_name, false, loader);

    } catch (Exception e) {
      throw new Error(e);
    }

    for (MethodInfo mi : method_infos) {
      mi.initViaReflection();
    }

    if (ChicoryPremain.shouldDoPurity()) {
      for (String pureMeth : ChicoryPremain.getPureMethods()) {
        if (isInThisClass(pureMeth)) {
          boolean foundMatch = false;
          for (MethodInfo mi : method_infos) {
            assert mi.member != null
                : "@AssumeAssertion(nullness): member of method_infos have"
                    + " .member field"; // dependent type
            // System.out.printf("compare %s to pure %s%n",
            //                  mi.member.toString() , pureMeth);
            if (mi.member.toString().trim().equals(pureMeth)) {
              foundMatch = true;
              break;
            }
          }

          if (!foundMatch) {
            // pureMeth must not actually be in this class
            throw new Error(
                String.format("Could not find pure method \"%s\" in class %s", pureMeth, clazz));
          }
        }
      }
    }
  }

  /**
   * Determines if fully qualified method name is in this class. Example methodName:
   *
   * <pre>public static String mypackage.MyClass.doStuff(int, java.lang.Object)</pre>
   */
  private boolean isInThisClass(String methodName) {
    // A heuristical way to determine if the method is in this class.
    // Match anything of the form: ____class_name.____(____
    // Where ____ corresponds to any sequence of characters
    return methodName.matches(".*" + Pattern.quote(class_name) + "\\..*\\(.*");
  }

  /**
   * Initializes the daikon variables for the object and class ppts.
   *
   * @param depth how deeply to nest variables, as in "a.b.field"
   */
  public void init_traversal(int depth) {
    if (traversalObject == null) {
      traversalObject = RootInfo.getObjectPpt(this, depth);
    }
    if (traversalClass == null) {
      traversalClass = RootInfo.getClassPpt(this, depth);
    }
    assert traversalObject != null : class_name;
    assert traversalClass != null : class_name;
  }

  @SideEffectFree
  @Override
  public String toString(@GuardSatisfied ClassInfo this) {
    return String.format("ClassInfo %s [%s] %s", System.identityHashCode(this), class_name, clazz);
  }
}
