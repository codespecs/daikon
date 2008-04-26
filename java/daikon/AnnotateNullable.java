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
 *
 * The out_package (-p) command line option must be specified
 */
public class AnnotateNullable {

  static PptMap ppts = null;

  static SimpleLog verbose = new SimpleLog (false);

  static SimpleLog debug = new SimpleLog (false);

  /**
   * Specifies the package to be specified in the output file.  All
   * output classes must be in that package.  If not specified, the
   * package name is taken from the first program point encountered
   */
  @Option ("-p Specifies the package")
  public static String out_package = null;

  /**
   * Specifies that the new output format for the annotation file should be
   * used.  This format is not yet supported by the annotation file utilities
   */
  @Option ("Use the new annotation file format")
  public static boolean new_format = false;

  /**
   * Adds NonNull annotations as well as Nullable annotations.  Unlike Nullable
   * annotation, NonNull annotations are not necessarily correct
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

    // Write out the package
    if (new_format)
      System.out.printf ("package %s;\n\n", out_package);

    // Write out the definitions of our annotations
    if (new_format) {
      System.out.printf ("annotation @checkers.quals.Nullable {}\n");
      System.out.printf ("annotation @checkers.quals.NonNull {}\n");
      System.out.println();
    } else {
      System.out.printf ("package checkers.quals:\n");
      System.out.printf ("annotation visible @Nullable\n\n");
      System.out.printf ("package checkers.quals:\n");
      System.out.printf ("annotation visible @NonNull\n\n");
    }

    for (Iterator<PptTopLevel> ii = ppts.pptIterator(); ii.hasNext(); ) {
      PptTopLevel ppt = ii.next();

      // Skip subexit and enter ppts, all of the info is at the exit
      if ((ppt.type == PptType.SUBEXIT) || (ppt.type == PptType.ENTER))
        continue;

      // Skip synthetic program points
      if (ppt.name().startsWith ("$"))
        continue;

      // Skip program points that are not OBJECT ppts
      if (!ppt.is_object())
        continue;

      process_object (ppt);
    }
  }

  public static void process_object (PptTopLevel ppt) {

    // Get the class program point (if any)
    PptTopLevel class_ppt = null;
    if (ppt.parents.size() > 0) {
      assert ppt.parents.size() == 1 : ppt;
      class_ppt = ppt.parents.get(0).parent;
    }

    String class_samples = "-";
    if (class_ppt != null)
      class_samples = String.format ("%d", class_ppt.num_samples());
    if (new_format) {
      System.out.printf ("class %s { // %d/%s samples\n",
                         ppt.ppt_name.getFullClassName(), ppt.num_samples(),
                         class_samples);
    } else {
      System.out.printf ("package %s:\n", out_package);
      System.out.printf ("class %s : // %d/%s samples\n",
                         ppt.ppt_name.getShortClassName(), ppt.num_samples(),
                         class_samples);
    }

    // Get any variables at the class ppt
    if (class_ppt != null) {
      process_obj_fields (class_ppt);
    }

    // Get the variables at the object ppt
    process_obj_fields (ppt);

    // Process each method in this object
    for (PptRelation child_rel : ppt.children) {
      PptTopLevel child = child_rel.child;
      if (child.type == PptType.ENTER)
        continue;
      debug.log ("processing method %s, type %s", child, child.type);
      process_method (child);
    }

    if (new_format)
      System.out.printf ("}\n\n");
    else
      System.out.printf ("\n\n");
  }

  /**
   * Get the annotation for the specified variable.  Returns @Nullable
   * if samples were found for this variable and at least one sample
   * contained a null value
   */
  public static String get_annotation (PptTopLevel ppt, VarInfo vi) {

    String annotation = "";
    if (nonnull_annotations)
      annotation = "@checkers.quals.NonNull";
    if ((ppt.num_samples (vi) > 0) && !ppt.is_nonzero (vi))
      annotation = "@checkers.quals.Nullable";
    return annotation;
  }

  /**
   * Print out the annotations for the specified method
   */
  public static void process_method (PptTopLevel ppt) {

    // Get all of the parameters to the method and the return value
    List<VarInfo> params = new ArrayList<VarInfo>();
    VarInfo retvar = null;
    for (VarInfo vi : ppt.var_infos) {
      if (vi.var_kind == VarInfo.VarKind.RETURN)
        retvar = vi;
      else if (vi.isParam() && (vi.name() != "this") && !vi.isPrestate())
        params.add (vi);
    }

    // Get the annotation for the return value
    String annotation = "";
    if (retvar != null) {
      annotation = get_annotation (ppt, retvar);
    }

    // Print out the method declaration
    if (new_format) {
      System.out.printf ("  method %s %s { // %d samples\n", annotation,
                         ppt.ppt_name.getSignature(), ppt.num_samples());
    } else {
      System.out.printf ("  method %s : %s // %d samples\n", jvm_signature(ppt),
                         annotation, ppt.num_samples());
    }


    // Put out each parameter
    for (int i = 0; i < params.size(); i++) {
      VarInfo vi = params.get(i);

      // Print the annotation for this parameter
      annotation = "";
      if (vi.file_rep_type.isHashcode()) {
        annotation = get_annotation (ppt, vi);
      }
      if (new_format) {
        System.out.printf ("    parameter #%d %s {} // %s\n", i, annotation,
                           vi.name());
      } else {
        System.out.printf ("    parameter #%d : %s // %s\n", i, annotation,
                           vi.name());
      }
    }

    // Terminate the method
    if (new_format)
      System.out.printf ("  }\n");
  }

  /**
   * Print out the annotations for each field in the object or class
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
      if (new_format) {
        System.out.printf ("  field %s %s {} // %s\n", field_name(vi),
                           annotation, vi.type);
      } else {
        System.out.printf ("  field %s : %s // %s\n", field_name(vi),
                           annotation, vi.type);
      }
    }
  }

  /**
   * Returns a JVM signature for the method
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
   * package/class specifier
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
