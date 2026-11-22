package daikon;

import daikon.PptRelation.PptRelationType;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.StringJoiner;
import org.checkerframework.checker.interning.qual.Interned;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.Pure;
import org.plumelib.options.Option;
import org.plumelib.options.Options;
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
public class AnnotateNullable {

  /** Do not instantiate. */
  private AnnotateNullable() {
    throw new UnsupportedOperationException("Do not instantiate");
  }

  // Why is this variable static?
  static PptMap ppts = new PptMap(); // dummy value, to satisfy Nullness Checker

  // static SimpleLog verbose = new SimpleLog(/*enabled=*/ false);
  // static SimpleLog debug = new SimpleLog(/*enabled=*/ false);

  /** The program points of a class. */
  public static final class ClassPpts {
    /** The class's OBJECT program point, or null if the class has none. */
    public @Nullable PptTopLevel object_ppt = null;

    /** The class's CLASS program point, or null if the class has none. */
    public @Nullable PptTopLevel class_ppt = null;

    /** The combined exits of the class's static methods. */
    public final List<PptTopLevel> static_methods = new ArrayList<>();
  }

  /**
   * Map from the fully-qualified name of a class, such as "DataStructures.StackAr", to the class's
   * program points.
   */
  static Map<String, ClassPpts> class_map = new LinkedHashMap<>();

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

  /** If true, print a usage message and exit. */
  @Option("-h Display usage information")
  public static boolean help = false;

  public static void main(String[] args) throws IOException {

    Options options =
        new Options("daikon.AnnotateNullable [options] <inv_file>", AnnotateNullable.class);
    String[] inv_files = options.parse(true, args);
    if (help) {
      options.printUsage();
      System.exit(0);
    }
    if (inv_files.length != 1) {
      System.err.println("must specify exactly one invariant file");
      options.printUsage(System.err);
      System.exit(1);
    }

    // Read the serialized invariant file
    File inv_file = new File(inv_files[0]);
    ppts = FileIO.read_serialized_pptmap(inv_file, true);
    Daikon.all_ppts = ppts;
    // verbose.log("Finished reading %d program points", ppts.size());

    // Setup the list of proto invariants and initialize NIS suppressions
    Daikon.setup_proto_invs();
    Daikon.setup_NISuppression();

    // Write out the definitions of our annotations
    if (stub_format) {
      System.out.println("import org.checkerframework.checker.nullness.qual.Nullable;");
      System.out.println("import org.checkerframework.checker.nullness.qual.NonNull;");
      System.out.println();
    } else {
      System.out.println("package org.checkerframework.checker.nullness.qual:");
      System.out.println("annotation @Nullable:");
      System.out.println("annotation @NonNull:");
      System.out.println();
    }

    // Group the program points by class.  The first loop maps the classes that have an OBJECT
    // ppt, so that those classes are output in the order of their OBJECT ppts.
    for (PptTopLevel ppt : ppts.pptIterable()) {
      if (ppt.is_object()) {
        ClassPpts class_ppts = class_ppts_for(ppt);
        assert class_ppts.object_ppt == null : ppt;
        class_ppts.object_ppt = ppt;
      }
    }
    for (PptTopLevel ppt : ppts.pptIterable()) {
      if (ppt.is_class()) {
        ClassPpts class_ppts = class_ppts_for(ppt);
        assert class_ppts.class_ppt == null : ppt;
        class_ppts.class_ppt = ppt;
      }
    }

    // Add the combined exit of each static method to its class.  A static method can be
    // identified because its combined exit does not have an OBJECT ppt as a parent.  The combined
    // exit need not have a CLASS ppt as a parent either, because Chicory creates a CLASS ppt only
    // for a class that has static variables.
    for (PptTopLevel ppt : ppts.pptIterable()) {
      if (!ppt.is_combined_exit() || !is_static_method(ppt)) {
        continue;
      }
      if (!has_method_name(ppt)) {
        System.err.printf(
            "Warning: skipping %s, whose name does not have the form Class.method(args)%n", ppt);
        continue;
      }
      class_ppts_for(ppt).static_methods.add(ppt);
    }

    // Debug print all of the static methods
    if (false) {
      for (Map.Entry<String, ClassPpts> entry : class_map.entrySet()) {
        System.out.printf(
            "class %s static methods: %s%n", entry.getKey(), entry.getValue().static_methods);
      }
    }

    // Make sure that the static methods found by inference match those found for any class ppts
    for (ClassPpts class_ppts : class_map.values()) {
      PptTopLevel class_ppt = class_ppts.class_ppt;
      if (class_ppt != null) {
        List<PptTopLevel> class_methods = combined_exit_children(class_ppt);
        for (PptTopLevel child : class_methods) {
          assert class_ppts.static_methods.contains(child) : child;
        }
        assert class_methods.size() == class_ppts.static_methods.size() : class_ppts.static_methods;
      }
    }

    // Process each class.
    for (Map.Entry<String, ClassPpts> entry : class_map.entrySet()) {
      String classname = entry.getKey();

      // Skip synthetic classes
      if (classname.startsWith("$")) {
        continue;
      }

      process_class(classname, entry.getValue());
    }
  }

  /**
   * Returns the program points of the class whose name is in the name of the given program point.
   * Adds an entry to {@link #class_map} if it has none for the class.
   *
   * @param ppt a program point whose name contains a class name, as in
   *     "DataStructures.StackAr:::OBJECT" or "DataStructures.StackAr.push(java.lang.Object):::EXIT"
   * @return the program points of the class whose name is in the name of {@code ppt}
   */
  private static ClassPpts class_ppts_for(PptTopLevel ppt) {
    String classname = ppt.ppt_name.getFullClassName();
    if (classname == null) {
      throw new IllegalArgumentException("No class name in " + ppt);
    }
    return class_map.computeIfAbsent(classname, k -> new ClassPpts());
  }

  /**
   * Returns the children of the given program point that are combined exits and that are related
   * to it by a PARENT relation. AnnotateNullable obtains all the information about a method from
   * the method's combined exit, so it ignores the other children, such as entries and points. It
   * also ignores children related by a USER relation, which are methods of other classes, and
   * combined exits whose names do not have the form Class.method(args); see {@link
   * #has_method_name}.
   *
   * @param ppt a program point
   * @return the children of {@code ppt} that are combined exits with a method name and are related
   *     to {@code ppt} by a PARENT relation
   */
  private static List<PptTopLevel> combined_exit_children(PptTopLevel ppt) {
    List<PptTopLevel> result = new ArrayList<>();
    for (PptRelation child_rel : ppt.children) {
      PptTopLevel child = child_rel.child;
      if (child_rel.getRelationType() == PptRelationType.PARENT
          && child.is_combined_exit()
          && has_method_name(child)) {
        result.add(child);
      }
    }
    return result;
  }

  /**
   * Returns true if the name of the given program point has the form Class.method(args), that is,
   * if it has both a class name and a method signature. AnnotateNullable cannot produce annotations
   * for a method program point without such a name.
   *
   * @param ppt a program point
   * @return true if the name of {@code ppt} has both a class name and a method signature
   */
  private static boolean has_method_name(PptTopLevel ppt) {
    return ppt.ppt_name.getFullClassName() != null && ppt.ppt_name.getSignature() != null;
  }

  /**
   * Print out the annotations for the given class, including all its fields and methods.
   *
   * @param classname the fully-qualified name of a class, such as "DataStructures.StackAr"
   * @param class_ppts the program points of the class
   */
  public static void process_class(String classname, ClassPpts class_ppts) {

    PptTopLevel object_ppt = class_ppts.object_ppt;
    PptTopLevel class_ppt = class_ppts.class_ppt;

    String object_samples = "-";
    if (object_ppt != null) {
      object_samples = String.format("%d", object_ppt.num_samples());
    }
    String class_samples = "-";
    if (class_ppt != null) {
      class_samples = String.format("%d", class_ppt.num_samples());
    }
    int lastdot = classname.lastIndexOf('.');
    String ppt_package = (lastdot == -1 ? "" : classname.substring(0, lastdot)).intern();
    if (stub_format) {
      if (ppt_package != last_package) {
        // This will print the empty string if we switch from a package to the
        // unnamed package.  That is intentional.
        System.out.printf("package %s;%n%n", ppt_package);
        last_package = ppt_package;
      }
      System.out.printf(
          "class %s { // %s/%s obj/class samples%n", classname, object_samples, class_samples);
    } else {
      System.out.printf("package %s:%n", ppt_package);
      System.out.printf(
          "class %s : // %s/%s obj/class samples%n",
          classname.substring(lastdot + 1), object_samples, class_samples);
    }

    // Process static fields
    if (class_ppt != null) {
      process_obj_fields(class_ppt);
    }

    // Process member (non-static) fields
    if (object_ppt != null) {
      process_obj_fields(object_ppt);
    }

    // Process static methods
    if (class_ppt != null) {
      for (PptTopLevel child : combined_exit_children(class_ppt)) {
        // debug.log("processing static method %s, type %s", child, child.type);
        process_method(child);
      }
    } else {
      for (PptTopLevel child : class_ppts.static_methods) {
        process_method(child);
      }
    }

    // Process member (non-static) methods
    if (object_ppt != null) {
      for (PptTopLevel child : combined_exit_children(object_ppt)) {
        // debug.log("processing method %s, type %s", child, child.type);
        process_method(child);
      }
    }

    if (stub_format) {
      System.out.printf("}");
    }
    System.out.println();
    System.out.println();
  }

  /**
   * Returns the annotation for the specified variable. Returns @Nullable if samples were found for
   * this variable and at least one sample contained a null value. Returns an (interned) empty
   * string if no annotation is applicable. Otherwise, the return value contains a trailing space.
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

    assert ppt.is_combined_exit() : ppt;

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
      System.out.printf(" %s %s", return_annotation, ppt.ppt_name.getMethodName());
      StringJoiner sj = new StringJoiner(", ", "(", ")");
      for (int i = 0; i < params.size(); i++) {
        sj.add(String.format("%s %s %s", annos.get(i), "type-goes-here", names.get(i)));
      }
      System.out.printf("%s", sj);
      System.out.printf("; // %d samples%n", ppt.num_samples());
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
   * Returns true if the method of the specified ppt is static or not. The ppt must be an exit ppt.
   * Exit ppts that do not have an object as a parent are inferred to be static. This does not work
   * for enter ppts, because constructors do not have the object as a parent on entry.
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
