package daikon;

import daikon.PptTopLevel.PptType;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.checkerframework.checker.interning.qual.Interned;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.Pure;
import org.plumelib.options.Option;
import org.plumelib.reflection.Signatures;

/**
 * AnnotateNullable reads a Daikon invariant file and determines which reference variables have seen
 * any null values. It writes to standard out an <a
 * href="https://checkerframework.org/annotation-file-utilities/annotation-file-format.html">annotation
 * file</a> with those variables. It determines which variables have seen null values by looking at
 * the NonZero invariant. If that invariant is NOT present, then the variable must have been null at
 * least once.
 *
 * <p>Since only the NonZero invariant is used, Daikon processing time can be significantly reduced
 * by turning off derived variables and all invariants other than daikon.inv.unary.scalar.NonZero.
 * This is not necessary, however, for correct operation. File {@code annotate_nullable.config} in
 * the distribution does this.
 */
public class AnnotateNullable2 {

  // Why is this variable static?
  static PptMap ppts = new PptMap(); // dummy value, to satisfy Nullness Checker

  // static SimpleLog verbose = new SimpleLog(/*enabled=*/ false);
  // static SimpleLog debug = new SimpleLog(/*enabled=*/ false);

  /** Map from a class name to the list of static functions for that class. */
  static Map<String, List<PptTopLevel>> class_map = new LinkedHashMap<>();

  // The package for the previous class.  Used to reduce duplication in
  // output file.
  static @Interned String last_package = "";

  /**
   * Write an output file in the stub class format (see the Checker Framework Manual), instead of in
   * annotation file format.
   */
  @Option("Use the stub class file format")
  public static boolean stub_format = false;

  /**
   * Adds NonNull annotations as well as Nullable annotations. Unlike Nullable annotations, NonNull
   * annotations are not necessarily correct.
   */
  @Option("-n Insert NonNull as well as Nullable annotations")
  public static boolean nonnull_annotations = false;

  // Returns null if no corresponding class ppt exists
  private static @Nullable PptTopLevel class_for_object(PptTopLevel object_ppt) {
    if (object_ppt.parents.size() == 0) {
      return null;
    }
    assert object_ppt.parents.size() == 1 : object_ppt;
    return object_ppt.parents.get(0).parent;
  }

  // Process a class, including all its methods.
  // Takes the object program point as its argument.
  public static void process_class(PptTopLevel object_ppt) {

    // Get the class program point (if any)
    PptTopLevel class_ppt = class_for_object(object_ppt);

    String class_samples = "-";
    if (class_ppt != null) {
      class_samples = String.format("%d", class_ppt.num_samples());
    }
    String ppt_package = object_ppt.ppt_name.getPackageName();
    if (ppt_package == null) {
      ppt_package = "";
    } else {
      ppt_package = ppt_package.intern();
    }
    if (stub_format) {
      if (ppt_package != last_package) {
        // This will print the empty string if we switch from a package to the
        // unnamed package.  That is intentional.
        System.out.printf("package %s;%n%n", ppt_package);
        last_package = ppt_package;
      }
      System.out.printf(
          "class %s { // %d/%s obj/class samples%n",
          object_ppt.ppt_name.getFullClassName(), object_ppt.num_samples(), class_samples);
    } else {
      System.out.printf("package %s:%n", ppt_package);
      System.out.printf(
          "class %s : // %d/%s obj/class samples%n",
          object_ppt.ppt_name.getShortClassName(), object_ppt.num_samples(), class_samples);
    }

    // Process static fields
    if (class_ppt != null) {
      process_obj_fields(class_ppt);
    }

    // Process member (non-static) fields
    process_obj_fields(object_ppt);

    // Process static methods
    if (class_ppt != null) {
      for (PptRelation child_rel : class_ppt.children) {
        PptTopLevel child = child_rel.child;
        // Skip enter ppts, all of the info is at the exit.
        if ((child.type == PptType.ENTER) || (child.type == PptType.OBJECT)) {
          continue;
        }
        // debug.log("processing static method %s, type %s", child, child.type);
        process_method(child);
      }
    } else {
      String classname = object_ppt.ppt_name.getFullClassName();
      assert classname != null;
      @SuppressWarnings("nullness") // map: class_map has entry per classname
      @NonNull List<PptTopLevel> static_methods = class_map.get(classname);
      assert static_methods != null : classname;
      for (PptTopLevel child : static_methods) {
        process_method(child);
      }
    }

    // Process member (non-static) methods
    for (PptRelation child_rel : object_ppt.children) {
      PptTopLevel child = child_rel.child;
      // Skip enter ppts, all of the info is at the exit.
      if (child.type == PptType.ENTER) {
        continue;
      }
      // debug.log("processing method %s, type %s", child, child.type);
      process_method(child);
    }

    if (stub_format) {
      System.out.printf("}");
    }
    System.out.println();
    System.out.println();
  }

  /**
   * Get the annotation for the specified variable. Returns @Nullable if samples were found for this
   * variable and at least one sample contained a null value. Returns an (interned) empty string if
   * no annotation is applicable. Otherwise, the return value contains a trailing space.
   */
  public static String get_annotation(PptTopLevel ppt, VarInfo vi) {

    if (vi.type.isPrimitive()) {
      return "";
    }

    String annotation = (nonnull_annotations ? "NonNull" : "");
    if ((ppt.num_samples(vi) > 0) && !ppt.is_nonzero(vi)) {
      annotation = "Nullable";
    }
    if (annotation != "") { // interned
      // if (! stub_format) {
      //   annotation = "org.checkerframework.checker.nullness.qual." + annotation;
      // }
      annotation = "@" + annotation;
    }
    return annotation;
  }

  /** Print out the annotations for the specified method. */
  public static void process_method(PptTopLevel ppt) {

    assert ppt.type == PptType.EXIT : ppt;

    // Get all of the parameters to the method and the return value
    List<VarInfo> params = new ArrayList<>();
    VarInfo retvar = null;
    for (VarInfo vi : ppt.var_infos) {
      if (vi.var_kind == VarInfo.VarKind.RETURN) {
        retvar = vi;
      } else {
        if (vi.isParam()
            && (vi.name() != "this") // interned
            && !vi.isPrestate()) {
          params.add(vi);
        }
      }
    }

    // The formatted annotation for the return value with a leading space, or empty string
    String return_annotation = (retvar == null ? "" : " " + get_annotation(ppt, retvar));

    // Look up the annotation for each parameter.
    List<String> names = new ArrayList<>();
    List<String> annos = new ArrayList<>();
    for (VarInfo param : params) {
      String annotation = "";
      names.add(param.name());
      if (param.file_rep_type.isHashcode()) {
        annotation = get_annotation(ppt, param);
      }
      annos.add(annotation);
    }

    // Print out the method declaration
    if (stub_format) {
      System.out.printf(" %s %s(", return_annotation, ppt.ppt_name.getMethodName());
      for (int i = 0; i < params.size(); i++) {
        if (i != 0) System.out.printf(" ,");
        System.out.printf("%s %s %s", annos.get(i), "type-goes-here", names.get(i));
      }
      System.out.printf("); // %d samples%n", ppt.num_samples());
    } else {
      System.out.printf("  method %s : // %d samples%n", jvm_signature(ppt), ppt.num_samples());
      System.out.printf("    return:%s%n", return_annotation);
      for (int i = 0; i < params.size(); i++) {
        // Print the annotation for this parameter
        System.out.printf("    parameter #%d : %s // %s%n", i, annos.get(i), names.get(i));
      }
    }
  }

  /** Print out the annotations for each field in the object or class. */
  public static void process_obj_fields(PptTopLevel ppt) {

    for (VarInfo vi : ppt.var_infos) {

      assert !vi.isPrestate() : vi;

      // Skip anyone with a parent in the hierarchy.  We are only
      // interested in them at the top (e.g., we don't want to see
      // object fields in each method).
      if (!vi.parents.isEmpty()) {
        continue;
      }

      // Skip variables that are always non-null.
      if (vi.aux.isNonNull()) {
        continue;
      }

      // Skip any variable that is enclosed by a variable other than 'this'.
      // These are fields and can only be annotated where they are declared.
      VarInfo evar = vi.get_enclosing_var();
      if ((evar != null) && !evar.name().equals("this")) {
        // System.out.printf("  enclosed %s %s%n", vi.type, vi.name());
        continue;
      }

      // print out the entry for this field
      String annotation = "";
      if (vi.file_rep_type.isHashcode()) {
        annotation = get_annotation(ppt, vi);
      }
      if (stub_format) {
        System.out.printf("  field %s %s {} // %s%n", field_name(vi), annotation, vi.type);
      } else {
        System.out.printf("  field %s : %s // %s%n", field_name(vi), annotation, vi.type);
      }
    }
  }

  /** Returns a JVM signature for the method. */
  public static String jvm_signature(PptTopLevel ppt) {

    @SuppressWarnings("nullness") // Java method, so getMethodName() != null
    @NonNull String method = ppt.ppt_name.getMethodName();
    @SuppressWarnings("nullness") // Java method, so getSignature() != null
    @NonNull String java_sig = ppt.ppt_name.getSignature();
    String java_args = java_sig.replace(method, "");
    // System.out.printf("m/s/a = %s %s %s%n", method, java_sig, java_args);
    if (method.equals(ppt.ppt_name.getShortClassName())) {
      method = "<init>";
    }

    // Problem:  I need the return type, but Chicory does not output it.
    // So, I could try to retrieve it from the "return" variable in the
    // program point (which is, fortunately, always an exit point), or
    // change Chicory to output it.
    VarInfo returnVar = ppt.find_var_by_name("return");
    @SuppressWarnings(
        "signature" // application invariant: returnVar.type.toString() is a binary name (if
    // returnVar is non-null), because we are processing a Java program
    )
    String returnType =
        returnVar == null ? "V" : Signatures.binaryNameToFieldDescriptor(returnVar.type.toString());

    return method + Signatures.arglistToJvm(java_args) + returnType;
  }

  /**
   * Returns the field name of the specified variable. This is the relative name for instance
   * fields, but the relative name is not specified for static fields (because there is no enclosing
   * variable with the full name). The field name is obtained in that case, by removing the
   * package/class specifier.
   */
  public static String field_name(VarInfo vi) {

    if (vi.relative_name != null) {
      return vi.relative_name;
    }

    String field_name = vi.name();
    int pt = field_name.lastIndexOf('.');
    if (pt == -1) {
      return field_name;
    } else {
      return field_name.substring(pt + 1);
    }
  }

  /**
   * Returns whether or not the method of the specified ppt is static or not. The ppt must be an
   * exit ppt. Exit ppts that do not have an object as a parent are inferred to be static. This does
   * not work for enter ppts, because constructors do not have the object as a parent on entry.
   */
  @Pure
  public static boolean is_static_method(PptTopLevel ppt) {

    assert ppt.is_exit() : ppt;
    for (PptRelation rel : ppt.parents) {
      if (rel.parent.is_object()) {
        return false;
      }
    }

    return true;
  }
}
