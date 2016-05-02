package daikon.dcomp;

import static daikon.dcomp.DCInstrument.MethodDef;

import daikon.DynComp;
import daikon.chicory.ClassInfo;
import daikon.chicory.DaikonWriter;
import daikon.chicory.MethodInfo;
import daikon.util.*;
import java.io.*;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.*;
import java.util.regex.*;
import org.apache.commons.bcel6.generic.*;
import org.apache.commons.bcel6.verifier.*;
import org.apache.commons.bcel6.verifier.structurals.*;
import org.apache.commons.io.*;

/*>>>
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.checker.interning.qual.*;
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.checker.signature.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

/**
 * Information about DF summary routines.  These routines are called in
 * place of JDK routines when for dataflow.  In most cases, the
 * make appropriate dataflow calculations and then call the original
 * method.
 *
 * Methods that are used for summaries are marked with the DFSum annotation.
 * Currently, all such methods are presumed to be in the class DCRuntime.
 *
 * DFInstrument looks for calls to any of the summarized methods and
 * replaces them with calls to the marked methods.
 */
public class SummaryInfo {

  @Option("-v Print information about each summary as it is processed")
  public static boolean verbose = false;

  public static final String synopsis = "daikon.dcomp.SummaryInfo [options]";

  /** Map from JDK classname to summary map for that class **/
  static Map<String, Map<MethodDef, String>> jdk_method_map =
      new LinkedHashMap<String, Map<MethodDef, String>>();

  // initialize jdk_method_map
  static {
    init();
  }

  /*@Interned*/ String invoke_type; // interned
  /*@BinaryNameForNonArray*/ String original_classname;
  String original_methodname;
  java.lang.reflect.Method method;

  /**
   * Exception thrown if the summary annotation cannot be correctly parsed
   */
  public static class BadSummaryAnnotation extends RuntimeException {
    static final long serialVersionUID = 20080703L;

    @SuppressWarnings("formatter") // acts as format method wrapper
    public BadSummaryAnnotation(String format, /*@Nullable*/ Object... args) {
      super(String.format(format, args));
    }
  }

  /**
   * Parses the annotation string to create summary information.  Throws
   * BadSummaryAnnotation if there are any parse errors.  Does not lookup
   * the original call to ensure that it exists so that class loading order
   * is not changed.
   */
  public SummaryInfo(java.lang.reflect.Method method, String annotation) {

    // Split around the - separator and make sure there are only two tokens
    String[] si = annotation.split("-");
    if (si.length != 2) {
      throw new BadSummaryAnnotation(
          "method %s: annotation not in the form" + " 'static|instance-fully_qualified_name'",
          method);
    }
    invoke_type = si[0].intern();

    // Make sure the invoke type is either instance or static
    if ((invoke_type != "instance") && (invoke_type != "static")) // interned
    throw new BadSummaryAnnotation(
          "method %s: found '%s' where " + "'instance' or 'static' expected", method, invoke_type);

    // Find the class and method name of the original call
    String full_name = si[1];
    int lastdot = full_name.lastIndexOf('.');
    @SuppressWarnings("signature") // substring
    /*@BinaryNameForNonArray*/ String bnfna = full_name.substring(0, lastdot);
    original_classname = bnfna;
    original_methodname = full_name.substring(lastdot + 1);

    // Remember the new routine
    this.method = method;
  }

  /**
   * Returns true if the specifiec call exists, false otherwise.  Checks
   * via reflection, so the specified class is loaded.
   */
  public boolean original_exists() {

    try {
      Class<?> c = Class.forName(original_classname);
      Method original_method = c.getMethod(original_methodname, original_params());
      return true;
    } catch (NoSuchMethodException e) {
      return false;
    } catch (ClassNotFoundException e) {
      return false;
    }
  }

  /** Returns the type of each parameter of the original method **/
  public Class<?>[] original_params(/*>>>@GuardSatisfied SummaryInfo this*/ ) {
    if (invoke_type == "static") { // interned
      return method.getParameterTypes();
    } else {
      Class<?>[] all_args = method.getParameterTypes();
      Class<?>[] args = new Class<?>[all_args.length - 1];
      for (int ii = 1; ii < all_args.length; ii++) {
        args[ii - 1] = all_args[ii];
      }
      return args;
    }
  }

  /**
   * Returns the full signature of the original method.  Uses reflection
   * to get the parameter types and may change class loading order
   */
  /*@SideEffectFree*/ public String toString(/*>>>@GuardSatisfied SummaryInfo this*/ ) {
    List<String> param_names = new ArrayList<String>();
    for (Class<?> p : original_params()) {
      param_names.add(p.getSimpleName());
    }

    return String.format(
        "%s %s.%s(%s)",
        invoke_type,
        original_classname,
        original_methodname,
        UtilMDE.join(param_names, ","));
  }

  /**
   * Initializes the map that specifies each JDK routine that has a summary
   */
  public static void init() {

    Class<DCRuntime> dcr = daikon.dcomp.DCRuntime.class;
    java.lang.reflect.Method[] methods = dcr.getDeclaredMethods();
    for (java.lang.reflect.Method m : methods) {
      DFSum dfs = m.getAnnotation(DFSum.class);
      if (dfs == null) continue;
      if (verbose) System.out.printf("Summary on method %s is %s%n", m, dfs.value());
      SummaryInfo si = new SummaryInfo(m, dfs.value());

      // Get the map for this class and create it if it is not there
      Map<MethodDef, String> class_map = jdk_method_map.get(si.original_classname);
      if (class_map == null) {
        class_map = new LinkedHashMap<MethodDef, String>();
        jdk_method_map.put(si.original_classname, class_map);
      }

      // Build the method definition
      Type[] arg_types = Type.getTypes(si.original_params());
      MethodDef md = new MethodDef(si.original_methodname, arg_types);
      class_map.put(md, si.method.getName());
    }
  }

  /**
   * Checks all of the summaries in DCRuntime to ensure that they
   * are syntactically correct and that the methods they describe
   * exist
   */
  public static void main(String[] args) throws ClassNotFoundException {

    Options options = new Options(synopsis, SummaryInfo.class);
    options.parse_or_usage(args);

    Class<DCRuntime> dcr = daikon.dcomp.DCRuntime.class;
    int errs = 0;

    java.lang.reflect.Method[] methods = dcr.getDeclaredMethods();
    for (java.lang.reflect.Method m : methods) {
      DFSum dfs = m.getAnnotation(DFSum.class);
      if (dfs == null) continue;
      if (verbose) System.out.printf("Summary on method %s is %s%n", m, dfs.value());
      try {
        SummaryInfo si = new SummaryInfo(m, dfs.value());
        if (!si.original_exists()) {
          System.out.printf("Error: %s does not exist%n", si);
          errs++;
        }
      } catch (Exception e) {
        System.out.printf("Error: %s%n", e.getMessage());
        errs++;
      }
    }

    if (errs == 0) {
      System.out.printf("No errors%n");
    } else if (errs == 1) {
      System.out.printf("1 error found%n");
    } else {
      System.out.printf("%d errors found%n", errs);
    }
  }
}
