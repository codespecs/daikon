package daikon;

import java.io.*;
import java.util.*;
import java.util.regex.*;
import utilMDE.*;
import static daikon.PptTopLevel.PptType;

/**
 * AnnotateNullable reads in a daikon invariant file and determines
 * which reference variables have seen any null values.  It writes
 * an annotation file (to standard out) with those variables.  It
 * determines which variables have seen null values by looking at the
 * NonZero invariant.  If that invariant is NOT present, then the
 * variables must have seen at least one null value.
 *
 * Since only the NonZero invariant is used, Daikon processing time can be
 * significantly reduced by turning off derived variables and all invariants
 * other than daikon.inv.unary.scalar.NonZero.  This is not necessary, however,
 * for correct operation.
 */
public class AnnotateNullable {

  static PptMap ppts = null;

  static SimpleLog verbose = new SimpleLog (false);

  static SimpleLog debug = new SimpleLog (false);

  // The package for the previous class.  Used to reduce duplication in
  // output file.
  static String last_package = null;

  /**
   * Write an output file in the stub class format (see the Checker
   * Framework Manual), instead of in annotation file format.
   */
  @Option ("Use the stub class file format")
  public static boolean stub_format = false;

  /**
   * Adds NonNull annotations as well as Nullable annotations.  Unlike Nullable
   * annotation, NonNull annotations are not necessarily correct.
   */
  @Option ("-n Insert NonNull as well as Nullable annotations")
  public static boolean nonnull_annotations = false;

  public static void main (String[] args) throws IOException {

    Options options = new Options ("utilMDE.AnnotateNullable [options] "
                                   + "<inv_file>", AnnotateNullable.class);
    String[] inv_files = options.parse_and_usage (args);
    assert inv_files.length == 1;

    // Read the serialized invariant file
    File inv_file = new File (inv_files[0]);
    ppts = FileIO.read_serialized_pptmap(inv_file, true);
    Daikon.all_ppts = ppts;
    verbose.log ("Finished reading %d program points", ppts.size());

    // Setup the list of proto invariants and initialize NIS suppressions
    Daikon.setup_proto_invs();
    Daikon.setup_NISuppression();

    // Write out the definitions of our annotations
    if (stub_format) {
      System.out.println ("import checkers.nullness.quals.Nullable;");
      System.out.println ("import checkers.nullness.quals.NonNull;");
      System.out.println();
    } else {
      System.out.println ("package checkers.quals:");
      System.out.println ("annotation visible @Nullable");
      System.out.println();
      System.out.println ("package checkers.quals:");
      System.out.println ("annotation visible @NonNull");
      System.out.println();
    }

    // Process each class.
    for (Iterator<PptTopLevel> ii = ppts.pptIterator(); ii.hasNext(); ) {
      PptTopLevel ppt = ii.next();

      // Skip synthetic program points
      if (ppt.name().startsWith ("$"))
        continue;

      // Skip program points that are not OBJECT ppts
      if (ppt.is_object())
        process_class (ppt);

    }
  }

  // Process a class, including all its methods.
  // Takes the object program point as its argument.
  public static void process_class (PptTopLevel object_ppt) {

    // Get the class program point (if any)
    PptTopLevel class_ppt = null;
    if (object_ppt.parents.size() > 0) {
      assert object_ppt.parents.size() == 1 : object_ppt;
      class_ppt = object_ppt.parents.get(0).parent;
    }

    String class_samples = "-";
    if (class_ppt != null)
      class_samples = String.format ("%d", class_ppt.num_samples());
    String ppt_package = object_ppt.ppt_name.getPackageName().intern();
    if (stub_format) {
      if (ppt_package != last_package) {
        // This will print "null" if we switch from a package to the
        // unnamed package.  That is intentional.
        System.out.printf ("package %s;\n\n", ppt_package);
        last_package = ppt_package;
      }
      System.out.printf ("class %s { // %d/%s obj/class samples\n",
                         object_ppt.ppt_name.getFullClassName(),
                         object_ppt.num_samples(),
                         class_samples);
    } else {
      System.out.printf ("package %s:\n", ppt_package);
      System.out.printf ("class %s : // %d/%s obj/class samples\n",
                         object_ppt.ppt_name.getShortClassName(),
                         object_ppt.num_samples(),
                         class_samples);
    }

    // Process static fields
    if (class_ppt != null) {
      process_obj_fields (class_ppt);
    }

    // Process member (non-static) fields
    process_obj_fields (object_ppt);

    // Process each method
    for (PptRelation child_rel : object_ppt.children) {
      PptTopLevel child = child_rel.child;
      // Skip enter ppts, all of the info is at the exit.
      if (child.type == PptType.ENTER)
        continue;
      debug.log ("processing method %s, type %s", child, child.type);
      process_method (child);
    }

    if (stub_format)
      System.out.printf ("}\n\n");
    else
      System.out.printf ("\n\n");
  }

  /**
   * Get the annotation for the specified variable.  Returns @Nullable
   * if samples were found for this variable and at least one sample
   * contained a null value.
   */
  public static String get_annotation (PptTopLevel ppt, VarInfo vi) {

    String annotation = "";
    if (nonnull_annotations)
      // annotation = "@checkers.nullness.quals.NonNull";
      annotation = "@NonNull";
    if ((ppt.num_samples (vi) > 0) && !ppt.is_nonzero (vi))
      // annotation = "@checkers.nullness.quals.Nullable";
      annotation = "@Nullable";
    return annotation;
  }

  /**
   * Print out the annotations for the specified method.
   */
  public static void process_method (PptTopLevel ppt) {

    assert ppt.type == PptType.EXIT : ppt;

    // Get all of the parameters to the method and the return value
    List<VarInfo> params = new ArrayList<VarInfo>();
    VarInfo retvar = null;
    for (VarInfo vi : ppt.var_infos) {
      if (vi.var_kind == VarInfo.VarKind.RETURN)
        retvar = vi;
      else if (vi.isParam()
               && (vi.name() != "this") // interned
               && !vi.isPrestate())
        params.add (vi);
    }

    // Get the annotation for the return value
    String return_annotation = "";
    if (retvar != null) {
      return_annotation = get_annotation (ppt, retvar);
    }

    // Look up the annotation for each parameter.
    List<String> names = new ArrayList<String>();
    List<String> annos = new ArrayList<String>();
    for (VarInfo param : params) {
      String annotation = "";
      names.add(param.name());
      if (param.file_rep_type.isHashcode()) {
        annotation = get_annotation (ppt, param);
      }
      annos.add(annotation);
    }

    // Print out the method declaration
    if (stub_format) {
      System.out.printf ("  %s %s(", return_annotation, ppt.ppt_name.getMethodName());
      for (int i = 0; i < params.size(); i++) {
        if (i != 0)
          System.out.printf (" ,");
        System.out.printf ("%s %s %s", annos.get(i), "type-goes-here", names.get(i));
      }
      System.out.printf ("); // %d samples%n", ppt.num_samples());
    } else {
      System.out.printf ("  method %s : %s // %d samples\n", jvm_signature(ppt),
                         return_annotation, ppt.num_samples());
      for (int i = 0; i < params.size(); i++) {
        // Print the annotation for this parameter
        System.out.printf ("    parameter #%d : %s // %s\n", i, annos.get(i), names.get(i));
      }
    }
  }


  /**
   * Print out the annotations for each field in the object or class.
   */
  public static void process_obj_fields (PptTopLevel ppt) {

    for (VarInfo vi : ppt.var_infos) {

      assert (!vi.isPrestate()) : vi;

      // Skip anyone with a parent in the hierarchy.  We are only
      // interested in them at the top (eg, we don't want to see
      // object fields in each method
      if (vi.parent_ppt != null)
        continue;

      // Skip 'this' variables
      if (vi.name().equals ("this"))
        continue;

      // Skip any variable that is enclosed by a variable other than
      // 'this'.  These are fields and can only be annotated where they
      // are declared
      VarInfo evar = vi.get_enclosing_var();
      if ((evar != null) && (!evar.name().equals ("this"))) {
        // System.out.printf ("  enclosed %s %s\n", vi.type, vi.name());
        continue;
      }

      // print out the entry for this field
      String annotation = "";
      if (vi.file_rep_type.isHashcode()) {
        annotation = get_annotation (ppt, vi);
      }
      if (stub_format) {
        System.out.printf ("  field %s %s {} // %s\n", field_name(vi),
                           annotation, vi.type);
      } else {
        System.out.printf ("  field %s : %s // %s\n", field_name(vi),
                           annotation, vi.type);
      }
    }
  }

  /**
   * Returns a JVM signature for the method.
   */
  public static String jvm_signature (PptTopLevel ppt) {

    String method = ppt.ppt_name.getMethodName();
    String java_sig = ppt.ppt_name.getSignature();
    String java_args = java_sig.replace (method, "");
    // System.out.printf ("m/s/a = %s %s %s\n", method, java_sig, java_args);
    if (method.equals (ppt.ppt_name.getShortClassName()))
      method = "<init>";
    return method + UtilMDE.arglistToJvm(java_args);
  }


  /**
   * Returns the field name of the specified variable.  This is the relative
   * name for instance fields, but the relative name is not specified for
   * static fields (because there is no enclosing variable with the full
   * name).  The field name is obtained in that case, by removing the
   * package/class specifier.
   */
  public static String field_name (VarInfo vi) {

    if (vi.relative_name != null)
      return vi.relative_name;

    String field_name = vi.name();
    int pt = field_name.lastIndexOf ('.');
    if (pt == -1)
      return field_name;
    else
      return field_name.substring(pt+1);
  }


}
