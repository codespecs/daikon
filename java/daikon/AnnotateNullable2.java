package daikon;

import daikon.PptTopLevel.PptType;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.checkerframework.checker.interning.qual.Interned;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.plumelib.options.Option;

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

    // Process static methods
    if (class_ppt != null) {
      for (PptRelation child_rel : class_ppt.children) {
        PptTopLevel child = child_rel.child;
        // Skip enter ppts, all of the info is at the exit.
        if ((child.type == PptType.ENTER) || (child.type == PptType.OBJECT)) {
          continue;
        }
        // debug.log("processing static method %s, type %s", child, child.type);
      }
    } else {
      String classname = object_ppt.ppt_name.getFullClassName();
      assert classname != null;
      @SuppressWarnings("nullness") // map: class_map has entry per classname
      @NonNull List<PptTopLevel> static_methods = class_map.get(classname);
      assert static_methods != null : classname;
      for (PptTopLevel child : static_methods) {}
    }

    // Process member (non-static) methods
    for (PptRelation child_rel : object_ppt.children) {
      PptTopLevel child = child_rel.child;
      // Skip enter ppts, all of the info is at the exit.
      if (child.type == PptType.ENTER) {
        continue;
      }
      // debug.log("processing method %s, type %s", child, child.type);
    }

    if (stub_format) {
      System.out.printf("}");
    }
    System.out.println();
    System.out.println();
  }
}
