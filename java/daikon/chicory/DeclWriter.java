package daikon.chicory;

import daikon.Chicory;
import daikon.FileIO;
import daikon.PptTopLevel.PptType;
import daikon.plumelib.bcelutil.SimpleLog;
import java.io.PrintStream;
import java.lang.reflect.Member;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.EnsuresNonNullIf;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;

/**
 * DeclWriter writes the {@code .decls} file to a stream. It uses traversal pattern trees (see
 * {@link DaikonVariableInfo}) for each program point. These are also used by the {@link
 * DTraceWriter}.
 */
@SuppressWarnings("nullness") // to do
public class DeclWriter extends DaikonWriter {
  // Notes:
  //
  //  Class.getName() returns JVM names (eg, [Ljava.lang.String;)

  /** Header string before each new method entry or exit point. */
  public static final String declareHeader = "DECLARE";

  boolean debug = false;

  // If the --comparability-file option is active, there might be
  // variables for which DynComp saw no interactions and did not
  // generate a comparability value. In order to reduce the number
  // of useless invariants (when running Daikon on the output of
  // Chicory), we generate a unique, dummy comparability value in
  // these cases.  We start at the maximum integer and decrement
  // for each use.  At the start of each method, object or class
  // we reset the value back - though it's hard to imagine a case
  // where there would be a collision with DynComp's values which
  // increment up from an initial value of 1.
  private static int initial_compare_value = Integer.MAX_VALUE;
  private static int unique_compare_value;

  /** Stream to write to. */
  private PrintStream outFile;

  /**
   * Enable parent relations other than methods to their class objects. Turned off for now to match
   * previous behavior.
   */
  private static boolean enable_object_user = false;

  /**
   * Constructs a DeclWriter, preparing it to receive messages.
   *
   * @param writer stream to write to
   */
  public DeclWriter(PrintStream writer) {
    super();
    outFile = writer;
    debug = Chicory.debug_decl_print;
  }

  /**
   * Prints header information to the decls file. Should be called once before emitting any other
   * declarations.
   *
   * @param className name of the top-level class (used only for printing comments)
   */
  public void printHeaderInfo(String className) {
    outFile.println("// Declarations for " + className);
    outFile.println("// Declarations written " + LocalDateTime.now(ZoneId.systemDefault()));
    outFile.println();

    // Determine comparability string
    String comparability = "none";
    if (Runtime.comp_info != null) {
      comparability = "implicit";
    }

    if (Chicory.new_decl_format) {
      outFile.printf("decl-version 2.0%n%n");
      outFile.printf("var-comparability %s%n%n", comparability);
    } else {
      outFile.printf("VarComparability%n%s%n%n", Runtime.comp_info != null ? "implicit" : "none");

      outFile.println("ListImplementors");
      outFile.println("java.util.List");
      outFile.println();
    }
  }

  /**
   * Returns the correctly formulated ":::OBJECT" name of the class (i.e., the program point name)
   *
   * @param type the ClassType type
   * @return the correctly formulated String
   */
  public static String classObjectName(Class<?> type) {
    return (type.getName() + ":::OBJECT");
  }

  /**
   * Prints declarations for all the methods in the indicated class. This method is called in
   * Runtime to print decls info for a class.
   *
   * @param cinfo class whose declarations should be printed
   */
  public void printDeclClass(ClassInfo cinfo, @Nullable DeclReader comp_info) {
    if (Chicory.new_decl_format) {
      print_decl_class(cinfo, comp_info);
      return;
    }

    // Print all methods and constructors
    for (MethodInfo mi : cinfo.get_method_infos()) {
      Member member = mi.member;

      // Don't want to instrument these types of methods
      if (!shouldInstrumentMethod(member)) {
        continue;
      }

      // Gset the root of the method's traversal pattern
      RootInfo enterRoot = mi.traversalEnter;
      assert enterRoot != null : "Traversal pattern not initialized at method " + mi.method_name;

      printMethod(enterRoot, methodEntryName(member), comp_info);

      // Print exit program point for EACH exit location in the method
      // (that was encountered during this execution of the program).
      Set<Integer> theExits = new HashSet<>(mi.exit_locations);
      assert theExits.size() > 0 : mi;
      for (Integer exitLoc : theExits) {
        // Get the root of the method's traversal pattern
        RootInfo exitRoot = mi.traversalExit;
        assert enterRoot != null : "Traversal pattern not initialized at method " + mi.method_name;

        printMethod(exitRoot, methodExitName(member, exitLoc.intValue()), comp_info);
      }
    }

    printClassPpt(cinfo, cinfo.class_name + ":::CLASS", comp_info);
    printObjectPpt(cinfo, classObjectName(cinfo.clazz), comp_info);
  }

  /**
   * Prints a method's program point. This includes the declare header ("DECLARE"), the program
   * point name, and the variable information.
   *
   * <p>This method uses variable information from the traversal tree.
   *
   * @param root the root of the traversal tree
   * @param name the program point name
   * @param comp_info comparability information
   */
  private void printMethod(RootInfo root, String name, DeclReader comp_info) {
    outFile.println(declareHeader);
    outFile.println(name);

    if (debug) {
      System.out.println("printMethod " + name);
    }

    for (DaikonVariableInfo childOfRoot : root) {
      traverseDecl(childOfRoot, ((comp_info == null) ? null : comp_info.find_ppt(name)));
    }

    outFile.println();
  }

  /**
   * Prints the .decls information for a single DaikonVariableInfo object, and recurses on its
   * children. If the current variable has comparability defined in decl_ppt, that comparability is
   * used. Otherwise -1 is used if there is comparability information available and the information
   * in the variable is used if it is not.
   */
  private void traverseDecl(DaikonVariableInfo curInfo, DeclReader.@Nullable DeclPpt decl_ppt) {
    if (curInfo.declShouldPrint()) {

      if (!(curInfo instanceof StaticObjInfo)) {
        outFile.println(curInfo.getName());
        outFile.println(curInfo.getTypeName());
        outFile.println(curInfo.getRepTypeName());
        String comp_str = curInfo.getCompareString();
        if (decl_ppt != null) {
          comp_str = "-1";
          DeclReader.DeclVarInfo varinfo = decl_ppt.find_var(curInfo.getName());
          if (varinfo != null) comp_str = varinfo.get_comparability();
        }
        outFile.println(comp_str);
      }
    }

    // Go through all of the current node's children
    // and recurse
    for (DaikonVariableInfo child : curInfo) {
      traverseDecl(child, decl_ppt);
    }
  }

  /**
   * Prints the object program point. This contains the "this" object and the class's fields.
   *
   * @param cinfo information about the class whose program point to print
   * @param name the program point name
   * @param comp_info a declaration file
   */
  private void printObjectPpt(ClassInfo cinfo, String name, DeclReader comp_info) {
    outFile.println(declareHeader);
    outFile.println(name);

    RootInfo root = RootInfo.getObjectPpt(cinfo, Runtime.nesting_depth);
    for (DaikonVariableInfo childOfRoot : root) {
      traverseDecl(childOfRoot, ((comp_info == null) ? null : comp_info.find_ppt(name)));
    }

    outFile.println();
  }

  /**
   * Prints the class program point. This contains only the static variables. If there are no static
   * variables to print, this method does nothing.
   */
  private void printClassPpt(ClassInfo cinfo, String name, DeclReader comp_info) {
    if (num_class_vars(cinfo) == 0) {
      return;
    }

    boolean printedHeader = false;
    RootInfo root = RootInfo.getClassPpt(cinfo, Runtime.nesting_depth);

    for (DaikonVariableInfo childOfRoot : root) {
      // If we are here, there is at least 1 child
      if (!printedHeader) {
        outFile.println(declareHeader);
        outFile.println(name);
        printedHeader = true;
      }

      // Should just print out static fields
      traverseDecl(childOfRoot, ((comp_info == null) ? null : comp_info.find_ppt(name)));
    }

    if (printedHeader) outFile.println();
  }

  /**
   * Prints declarations for all the methods in the indicated class. This method is called in
   * Runtime to print decls info for a class.
   *
   * @param cinfo class whose declarations should be printed
   * @param comp_info comparability information
   */
  public void print_decl_class(ClassInfo cinfo, @Nullable DeclReader comp_info) {

    if (debug) {
      System.out.println("Enter print_decl_class: " + cinfo);
    }

    // Print all methods and constructors
    for (MethodInfo mi : cinfo.get_method_infos()) {

      Member member = mi.member;

      // Don't want to instrument these types of methods
      if (!shouldInstrumentMethod(member)) {
        continue;
      }

      // Gset the root of the method's traversal pattern
      RootInfo enterRoot = mi.traversalEnter;
      assert enterRoot != null : "Traversal pattern not initialized at method " + mi.method_name;

      String entryName =
          (member != null
              ? methodEntryName(member)
              : mi.class_info.class_name + ".<clinit>" + FileIO.enter_tag);
      print_method(mi, enterRoot, entryName, PptType.ENTER, comp_info);

      // Print exit program point for EACH exit location in the method
      // Note that there may not be any exits.  They may get filtered out,
      // or some methods don't have an exit (only a throw)
      Set<Integer> theExits = new HashSet<>(mi.exit_locations);
      for (Integer exitLoc : theExits) {
        // Get the root of the method's traversal pattern
        RootInfo exitRoot = mi.traversalExit;
        assert enterRoot != null : "Traversal pattern not initialized at method " + mi.method_name;

        String exitName =
            (member != null
                ? methodExitName(member, exitLoc)
                : mi.class_info.class_name + ".<clinit>" + FileIO.exit_tag + exitLoc);
        print_method(mi, exitRoot, exitName, PptType.SUBEXIT, comp_info);
      }
    }

    print_class_ppt(cinfo, cinfo.class_name + ":::CLASS", comp_info);
    print_object_ppt(cinfo, classObjectName(cinfo.clazz), comp_info);

    if (debug) System.out.println("Exit print_decl_class");
  }

  /**
   * Prints a method's program point. This includes the ppt declaration, all of the ppt records, and
   * records for each variable.
   *
   * <p>This method uses variable information from the traversal tree.
   *
   * @param mi the method information for the method
   * @param root the root of the traversal tree
   * @param name the program point name
   * @param ppt_type the type of the program point (enter, exit, etc)
   * @param comp_info comparability information
   */
  private void print_method(
      MethodInfo mi, RootInfo root, String name, PptType ppt_type, @Nullable DeclReader comp_info) {

    if (debug) {
      System.out.println("Enter print_method: " + name);
    }

    // reset dummy comparability value
    unique_compare_value = initial_compare_value;

    outFile.println("ppt " + escape(name));

    outFile.println("ppt-type " + ppt_type.name().toLowerCase());

    // Look for and print any hierarchy relations
    List<VarRelation> relations = new ArrayList<>();
    for (DaikonVariableInfo child : root) {
      find_relations(null, mi.is_static(), null, child, relations);
    }
    for (VarRelation relation : relations) {
      outFile.println("parent parent " + relation.parent_ppt_name + " " + relation.id);
    }

    // Print each variable
    for (DaikonVariableInfo childOfRoot : root) {
      if (debug) System.out.println("method var: " + childOfRoot.getName());
      traverse_decl(
          null,
          mi.is_static(),
          null,
          childOfRoot,
          null,
          relations,
          ((comp_info == null) ? null : comp_info.find_ppt(name)));
    }

    outFile.println();

    if (debug) System.out.println("Exit print_method ");
  }

  /**
   * Prints the class program point. This contains only the static variables. If there are no static
   * variables to print, this method does nothing.
   *
   * @param cinfo class whose static declarations should be printed
   * @param name the program point name
   * @param comp_info comparability information
   */
  private void print_class_ppt(ClassInfo cinfo, String name, DeclReader comp_info) {

    if (debug) {
      System.out.println("Enter print_class_ppt: " + cinfo);
    }

    // reset dummy comparability value
    unique_compare_value = initial_compare_value;

    if (num_class_vars(cinfo) > 0) {

      outFile.println("ppt " + escape(name));
      outFile.println("ppt-type class");

      // Print out the static fields
      for (DaikonVariableInfo childOfRoot : RootInfo.getClassPpt(cinfo, Runtime.nesting_depth)) {
        if (debug) System.out.println("class var: " + childOfRoot.getName());
        traverse_decl(
            null,
            false,
            null,
            childOfRoot,
            null,
            null,
            ((comp_info == null) ? null : comp_info.find_ppt(name)));
      }

      outFile.println();
    }

    if (debug) System.out.println("Exit print_class_ppt");
  }

  /**
   * Prints the object program point. This contains the "this" object and the class' fields.
   *
   * @param cinfo class whose object program point should be printed
   * @param name the program point name
   * @param comp_info comparability information
   */
  private void print_object_ppt(ClassInfo cinfo, String name, DeclReader comp_info) {

    if (debug) {
      System.out.println("Enter print_object_ppt: " + cinfo);
    }

    // reset dummy comparability value
    unique_compare_value = initial_compare_value;

    outFile.println("ppt " + escape(name));
    outFile.println("ppt-type object");
    RootInfo root = RootInfo.getObjectPpt(cinfo, Runtime.nesting_depth);

    // If there are any static variables, add the relation to
    // the class ppt
    List<VarRelation> relations = new ArrayList<>();
    if (num_class_vars(cinfo) > 0) {
      VarRelation relation = new VarRelation(cinfo.class_name + ":::CLASS", "parent");
      relation.id = 1;
      relations.add(relation);
    }

    // Look for and print any object-user relations
    for (DaikonVariableInfo child : root) {
      find_relations(cinfo, false, null, child, relations);
    }
    for (VarRelation relation : relations) {
      outFile.println(
          "parent " + relation.type + " " + relation.parent_ppt_name + " " + relation.id);
    }

    // Write out the variables
    for (DaikonVariableInfo childOfRoot : root) {
      if (debug) System.out.println("object var: " + childOfRoot.getName());
      traverse_decl(
          cinfo,
          false,
          null,
          childOfRoot,
          null,
          relations,
          ((comp_info == null) ? null : comp_info.find_ppt(name)));
    }

    outFile.println();

    if (debug) System.out.println("Exit print_object_ppt");
  }

  /**
   * Object program points are constructed for invariants about an object. We define an object
   * invariant as one that is true at the entrance and exit of each public method and also each time
   * an instance of the object is available to another method (eg, when it is passed as a parameter
   * or available as a static). Daikon implements object invariants by merging the invariants from
   * each public method and each user of the object. We refer to the relationship between variables
   * at these program points as a Program point / variable hierarchy. This relationship must be
   * defined in the declaration record. The VarRelation class tracks one relation.
   */
  private static class VarRelation {
    /** Name of the program point for the parent. */
    String parent_ppt_name;
    /** Prefix of the variable name that is not part of the parent name. */
    String local_prefix;
    /** Prefix of the parent that replaces the local prefix. Normally 'this'. */
    String parent_prefix;
    /** Top level variable for the relation. */
    String local_variable;
    /** Type of the relation (parent, user, etc) */
    String type;
    /** Number that identifies this relation within this ppt. */
    int id;

    static SimpleLog debug = new SimpleLog(false);

    /** Create a VarRelation. */
    public VarRelation(
        String parent_ppt_name,
        String type,
        String local_prefix,
        String parent_prefix,
        String local_variable) {
      this.parent_ppt_name = parent_ppt_name;
      this.type = type;
      this.local_prefix = local_prefix;
      this.parent_prefix = parent_prefix;
      this.local_variable = local_variable;
      debug.log("Created %s", this);
    }

    /** Create a var relation with the matching names. */
    public VarRelation(String parent_ppt_name, String type) {
      this(parent_ppt_name, type, null, null, null);
    }

    @SideEffectFree
    @Override
    public String toString(@GuardSatisfied VarRelation this) {
      return String.format(
          "VarRelation %s (%s->%s) %s [%s]",
          parent_ppt_name, local_prefix, parent_prefix, local_variable, type);
    }

    /**
     * Returns whether or not this relation is from a static variable in an object ppt to its
     * matching variable at the class level.
     */
    @Pure
    public boolean is_class_relation() {
      return (parent_ppt_name.endsWith(":::CLASS"));
    }

    /**
     * Returns the string defining the relation for the specified variable The format is
     * parent-ppt-name id parent-variable-name. If the variable is static, it always has the same
     * name in the parent (since fully specified names are used for static variables).
     */
    public String relation_str(DaikonVariableInfo var) {
      String out = parent_ppt_name + " " + id;
      if (!var.isStatic() && (local_prefix != null) && !local_prefix.equals(parent_prefix)) {
        out += " " + var.getName().replaceFirst(Pattern.quote(local_prefix), parent_prefix);
      }
      return out;
    }

    /** Two VarRelations are equal if the refer to the same program point and local variable. */
    @Override
    @EnsuresNonNullIf(result = true, expression = "#1")
    @Pure
    public boolean equals(@GuardSatisfied VarRelation this, @GuardSatisfied @Nullable Object o) {
      if (!(o instanceof VarRelation) || (o == null)) {
        return false;
      }
      VarRelation vr = (VarRelation) o;
      return (vr.parent_ppt_name.equals(parent_ppt_name)
          && (((vr.local_variable == null) && local_variable == null)
              || ((vr.local_variable != null) && vr.local_variable.equals(local_variable))));
    }

    @Override
    @Pure
    public int hashCode(@GuardSatisfied VarRelation this) {
      return (parent_ppt_name.hashCode()
          + ((local_variable == null) ? 0 : local_variable.hashCode()));
    }
  }

  /**
   * Prints the .decls information for a single DaikonVariableInfo object, and recurses on its
   * children. If the current variable has comparability defined in compare_ppt, that comparability
   * is used. Otherwise -1 is used if there is comparability information available and the
   * information in the variable is used if it is not.
   */
  private void traverse_decl(
      ClassInfo cinfo,
      boolean is_static_method,
      DaikonVariableInfo parent,
      DaikonVariableInfo var,
      VarRelation relation,
      List<VarRelation> relations,
      DeclReader.DeclPpt compare_ppt) {

    if (debug) {
      System.out.println("Enter traverse_decl: " + cinfo + ", " + var + ", " + parent);
    }

    if (!var.declShouldPrint()) {
      // don't do anything
    } else if (!(var instanceof StaticObjInfo)) {

      // Write out the variable and its name
      outFile.println("variable " + escape(var.getName()));

      // Write out the kind of variable and its relative name
      VarKind kind = var.get_var_kind();
      String relative_name = var.get_relative_name();
      outFile.print("  var-kind " + out_name(kind));
      if (relative_name != null) {
        outFile.print(" " + relative_name);
      }
      outFile.println();

      // Write out the enclosing variable.
      // If we are in an inner class, we need to special case the
      // 'hidden' field that holds the outer class 'this' pointer.
      // If the field name ends with ".this", it can only be this
      // special case and we need to not output the enclosing-var.
      if ((parent != null)
          && !var.isStatic()
          && !((relative_name != null) && relative_name.endsWith(".this"))) {
        if (debug) System.out.println("traverse var parent: " + parent.getName());
        outFile.println("  enclosing-var " + escape(parent.getName()));
      }

      // If this variable has multiple value, indicate it is an array
      if (var.isArray()) {
        outFile.println("  array 1");
      }

      // Write out the declared and representation types
      outFile.println("  dec-type " + escape(var.getTypeNameOnly()));
      outFile.println("  rep-type " + escape(var.getRepTypeNameOnly()));

      // Write out the constant value (if present)
      String const_val = var.get_const_val();
      if (const_val != null) {
        outFile.println("  constant " + const_val);
      }

      // Write out the arguments used to create (if present) the variable if it is a function
      String function_args = var.get_function_args();
      if (function_args != null) {
        outFile.println("  function-args " + function_args);
      }

      // Write out the variable flags if any are set
      EnumSet<VarFlags> var_flags = var.get_var_flags();
      if (var_flags.size() > 0) {
        outFile.print("  flags");
        for (Enum<?> e : var_flags) {
          outFile.print(" " + out_name(e));
        }
        outFile.println();
      }

      // Determine comparability and write it out
      // Currently, the value returned by getCompareString() is always 22.
      String comp_str = var.getCompareString();
      if (compare_ppt != null) {
        comp_str = "-1";
        DeclReader.DeclVarInfo varinfo = compare_ppt.find_var(var.getName());
        if (varinfo != null) comp_str = varinfo.get_comparability();
      } else {
        // Check to see if DynComp data is present.
        if (Runtime.comp_info != null) {
          // There is no comparability value for this variable as DynComp
          // saw no interactions. In order to reduce the number of useless
          // invariants, we will generate a unique, dummy comparability value.
          comp_str = Integer.toString(unique_compare_value--);
          if (var.isArray()) {
            // Should output n index values to match number of dimensions.
            // However, that value is hard to obtain at this point, so just
            // always do one.  May cause some multi-dimension variables to
            // be put into incorrect comparability set.  This is certainly
            // no worse than previous algorithm. (markro)
            comp_str = comp_str + "[" + Integer.toString(unique_compare_value--) + "]";
          }
        }
      }
      outFile.println("  comparability " + comp_str);

      // Determine if there is a ppt for variables of this type
      // If found this should match one of the previously found relations
      // for this ppt.
      // Once a relation has been found, we don't look for recursive
      // relationships
      if ((relation == null) && (relations != null)) {
        relation = find_relation(cinfo, is_static_method, parent, var);
        if (relation != null) {
          // System.out.printf("Found relation %s, variable %s%n", relation,
          //                   var);
          int index = relations.indexOf(relation);
          assert (index != -1) : "Relation " + relation + " not found in " + relations;
          relation = relations.get(index);
        }
      }

      // Put out the variable relation (if one exists).
      if (relation != null) {
        outFile.println("  parent " + relation.relation_str(var));
      }

    } else { // this is the dummy root for class statics
      if ((relations != null) && (relations.size() > 0)) {
        relation = find_relation(cinfo, true, parent, var);
        if (relation != null) {
          int index = relations.indexOf(relation);
          assert (index != -1) : "Relation " + relation + " not found in " + relations;
          relation = relations.get(index);
          // System.out.printf("Found class relation %s for cinfo %s%n",
          //                   relation, cinfo);
        } else {
          System.out.printf("No class relation found for cinfo %s%n", cinfo);
        }
      }
    }

    // Go through all of the current node's children
    // and recurse
    for (DaikonVariableInfo child : var) {
      if (debug) System.out.println("traverse var: " + child.getName());
      traverse_decl(cinfo, is_static_method, var, child, relation, relations, compare_ppt);
    }

    if (debug) System.out.println("Exit traverse_decl");
  }

  /**
   * Returns the string to write to the output file for the specified enum. Currently this is just
   * the name of the enum in lower case.
   */
  private String out_name(Enum<?> e) {
    return e.name().toLowerCase();
  }

  /**
   * Looks to see if there is a class that we are instrumenting that matches the type of this
   * variable. If so, returns a VarRelation that describes the hierarchy relationship between this
   * variable (and its field) and the variables within the object ppt for the class. If this is an
   * object ppt (ci != null), then each top level static variable has a relation to the class ppt.
   *
   * @param cinfo class of the object ppt. Null if this is not an object ppt.
   * @param is_static_method true if this ppt is a static method enter
   * @param parent parent of var in the variable tree
   * @param var variable whose relation is desired
   */
  private @Nullable VarRelation find_relation(
      @Nullable ClassInfo cinfo,
      boolean is_static_method,
      DaikonVariableInfo parent,
      DaikonVariableInfo var) {

    // Look for object->class static relationship.  This starts on each
    // static variable under 'this' (the static variables of a class are
    // placed in the CLASS ppt).
    if (cinfo != null && var.isStatic() && (parent instanceof ThisObjInfo)) {
      return new VarRelation(cinfo.class_name + ":::CLASS", "parent");
    }

    // Only hashcodes have object ppts
    if (!var.getRepTypeNameOnly().equals("hashcode")) {
      return null;
    }

    // Get the type (class) of this variable
    String decl_type = var.getTypeNameOnly();
    // System.out.printf("Looking for hierarchy type %s%n", decl_type);

    // If this ppt is the object ppt for this type, don't create a relation
    // to it.
    if ((cinfo != null) && cinfo.class_name.equals(decl_type)) {
      return null;
    }

    // Look to see if we are instrumenting this class.  If we are, then
    // there should be an object ppt for this class.  If this is a static
    // method and the relation is over the dummy static variable, it
    // relates directly to the class ppt, otherwise to the object
    // ppt.  Note that a relation to the class ppt is returned only if there
    // are static variables.
    for (ClassInfo ci : SharedData.all_classes) {
      if (ci.class_name.equals(decl_type)) {
        // System.out.printf("*Found match for %s : %s%n", decl_type, ci);
        String ppt_marker = ":::OBJECT";
        if (is_static_method && (var instanceof StaticObjInfo)) {
          // System.out.printf("num_class_vars for classinfo %s%n", ci);
          if (num_class_vars(ci) == 0) {
            return null;
          }
          ppt_marker = ":::CLASS";
        }
        if (!enable_object_user && !var.getName().equals("this")) {
          return null;
        }
        return new VarRelation(
            decl_type + ppt_marker, "parent", var.getName(), "this", var.getName());
      }
    }

    return null;
  }

  /**
   * Looks for all of the object-user ppt/variable hiearchy relations beginning at var. Once a
   * relation is found, no more relations are looked for under that variable. In most cases, it
   * would be expected that only one relation will be found (either var is a class with a
   * corresponding object ppt or it is not). However, depending on what classes are being
   * instrumented, it might be possible for a class not to have an object ppt while multiple
   * children do have object ppts.
   *
   * <p>Any relations that are found are added to the relations list.
   */
  private void find_relations(
      ClassInfo ci,
      boolean is_static_method,
      DaikonVariableInfo parent,
      DaikonVariableInfo var,
      List<VarRelation> relations) {

    // If there is a new relation for this variable add it to the list and
    // return it.  Note that each static variable in an object ppt will
    // have a relation to the matching static variable in class ppt.  Only
    // one of these should go in the list of relations.
    VarRelation relation = find_relation(ci, is_static_method, parent, var);
    if (relation != null) {
      if ((relations.size() == 0)
          || (relations.get(0).is_class_relation() && relation.is_class_relation())) {
        relations.add(relation);
        relation.id = relations.size();
        return;
      }
    }

    // Look for a relation in each child.
    for (DaikonVariableInfo child : var) {
      find_relations(ci, is_static_method, parent, child, relations);
    }
  }

  /**
   * Returns the number of variables in the CLASS program point. The CLASS ppt contains all of the
   * static variables in the class (if any).
   */
  private int num_class_vars(ClassInfo cinfo) {

    RootInfo class_root = RootInfo.getClassPpt(cinfo, Runtime.nesting_depth);
    assert class_root.children.size() == 1;
    DaikonVariableInfo static_root = class_root.children.get(0);
    return static_root.children.size();
  }
}
