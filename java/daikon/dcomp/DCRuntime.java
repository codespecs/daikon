package daikon.dcomp;

import java.util.*;
import java.lang.reflect.*;
import java.io.PrintStream;

import daikon.chicory.*;
import utilMDE.WeakIdentityHashMap;
import utilMDE.SimpleLog;
import utilMDE.ArraysMDE;

public final class DCRuntime {

  /** List of all instrumented methods **/
  public static final List<MethodInfo> methods = new ArrayList<MethodInfo>();

  /**
   * Keep track of whether or not we are already processing an enter/exit
   * so we can avoid recursion.  Only really necessary during debugging
   * (where we call toString()
   */
  private static boolean in_enter_exit = false;

  /** Object used to represent nonsensical values **/
  private static final NonsensicalObject nonsensical
    = NonsensicalObject.getInstance();

  /** Object used to represent nonsensical list values **/
  private static final NonsensicalList nonsensical_list
    = NonsensicalList.getInstance();

  /** Depth to follow fields in classes **/
  public static int depth = 2;

  /**
   * Map from each primitive static name to the offset in static_tags
   */
  public static Map<String,Integer> static_map
    = new LinkedHashMap<String,Integer>();

  /** Storage for each static tag **/
  public static List<Object> static_tags = new ArrayList<Object>();

  /** Tag stack **/
  public static Stack<Object> tag_stack = new Stack<Object>();

  /**
   * Object used to mark procedure entries in the tag stack.  It is pushed
   * on the stack at entry and checked on exit to make sure it is in on the
   * top of the stack.  That allows us to determine which method caused
   * a tag stack problem.
   */
  public static Object method_marker = new Object();

  // Control debug printing
  public static final boolean debug = true;
  public static final boolean debug_tag_frame = false;
  public static final boolean debug_objects = true;
  public static final SimpleLog merge_dv = new SimpleLog(false);
  public static final SimpleLog debug_arr_index = new SimpleLog(false);
  public static final SimpleLog debug_primitive = new SimpleLog(false);
  public static final SimpleLog debug_merge_comp = new SimpleLog (true);

  /** Simplifies printouts for debugging if we ignore toString **/
  private static boolean ignore_toString = true;

  /**
   * Map from each object to the tags used for each primitive value in
   * the object
   */
  public static WeakIdentityHashMap<Object,Object[]> field_map
    = new WeakIdentityHashMap<Object,Object[]>();

  /**
   * List of all classes encountered.  These are the classes that will
   * have comparability output
   **/
  private static List<ClassInfo> all_classes = new ArrayList<ClassInfo>();

  /** Set of classes whose static initializer has run **/
  private static Set<String> init_classes = new HashSet<String>();

  /**
   * Class used as a tag for primitive constants.  Only different from
   * Object for debugging purposes
   */
  private static class Constant {
  }

  /**
   * Handle object comparison.  Marks the two objects as comparable and
   * returns whether or not they are equal.  Used as part of a replacement
   * for IF_ACMPEQ
   */
  public static boolean object_eq (Object obj1, Object obj2) {

    if (debug_objects)
      System.out.printf ("comparing (eq) '%s' and '%s'%n", obj_str(obj1),
                         obj_str(obj2));

    // Note that obj1 and obj2 are comparable
    TagEntry.union (obj1, obj2);

    return (obj1 == obj2);
  }

  /**
   * Handle object comparison.  Marks the two objects as comparable and
   * returns whether or not they are equal.  Used as part of a replacement
   * for IF_ACMPNE
   */
  public static boolean object_ne (Object obj1, Object obj2) {

    if (debug_objects)
      System.out.printf ("comparing (ne) '%s' and '%s'%n", obj_str(obj1),
                         obj_str(obj2));
    // Note that obj1 and obj2 are comparable
    TagEntry.union (obj1, obj2);

    return (obj1 != obj2);
  }

  /**
   * Create the tag frame for this method.  Pop the tags for any
   * primitive parameters off of the tag stack and store them in the
   * tag frame.
   *
   * @param params Encodes the position of the primitive parameters into
   * a string.  The first character is size of the tag frame.  The
   * remaining characters indicate where each parameter on the tag stack
   * should be stored into the frame.  For example "20" allocates a tag
   * frame with two elements and stores the top of the tag stack into
   * element 0.  A string is used for simplicity in code generation since
   * strings can easily be placed into the constant portion of the class
   * file.  Since there is a max radix of 36, this will fail if there are
   * more than 36 parameters.
   *
   * @return the allocated and initialized tag frame
   */
  public static Object[] create_tag_frame (String params) {

    int frame_size = Character.digit (params.charAt(0), Character.MAX_RADIX);
    Object[] tag_frame = new Object[frame_size];
    if (debug_tag_frame)
      System.out.printf ("Creating tag frame of size %d [%s] for %s%n",
                         frame_size, params, caller_name());
    for (int ii = 1; ii < params.length(); ii++) {
      int offset = Character.digit (params.charAt(ii), Character.MAX_RADIX);
      check_method_marker();
      tag_frame[offset] = tag_stack.pop();
      if (debug_tag_frame)
        System.out.printf ("popped %s into tag_frame[%d]%n", tag_frame[offset],
                           offset);
    }

    // Push the method marker on the tag stack (now that we have removed
    // the parameters
    tag_stack.push (method_marker);

    return (tag_frame);
  }

  /**
   * Make sure the tag stack for this method is empty before exit
   */
  public static void normal_exit() {

    Object top = tag_stack.pop();
    assert top == method_marker;
    if (debug)
      System.out.printf ("Normal exit ok%n");
  }

  /**
   * Called for exits from methods with a primitive return type.  Pop the
   * return type off of the tag stack, make sure the tags stack is empty for
   * this method and then put the return value back on the tag stack
   */
  public static void normal_exit_primitive() {

    Object ret_tag = tag_stack.pop();
    Object top = tag_stack.pop();
    assert top == method_marker;
    if (debug)
      System.out.printf ("Normal exit primitive ok%n");
    tag_stack.push (ret_tag);
  }

  /**
   * Clean up the tag stack on an exception exit from a method.  Pops
   * items off of the tag stack until the method marker is found
   */
  public static void exception_exit() {

    if (debug)
      System.out.printf ("Exception exit from %s%n", caller_name());
    while (!tag_stack.empty())
      if (tag_stack.pop() == method_marker)
        return;

    System.out.printf ("Method marker not found in exception exit%n");
  }

  /**
   * Cleans up the tag stack when an exception is thrown
   */
  public static void throw_op() {
    while (tag_stack.peek() != method_marker)
      tag_stack.pop();
  }

  /** Pushes the tag at tag_frame[index] on the tag stack */
  public static void push_local_tag (Object[] tag_frame, int index) {

    debug_primitive.log ("push_local_tag[%d] %s%n", index, tag_frame[index]);
    assert tag_frame[index] != null : "index " + index;
    tag_stack.push (tag_frame[index]);
  }

  /** Pops the top of the tag stack into tag_frame[index] **/
  public static void pop_local_tag (Object[] tag_frame, int index) {

    check_method_marker();
    tag_frame[index] = tag_stack.pop();
    debug_primitive.log ("pop_local_tag[%d] %s%n", index, tag_frame[index]);

  }

  /** Pushes the argument tag on the tag stack **/
  public static void push_tag (Object tag) {
    tag_stack.push (tag);
  }

  /** Pops the top tag from the tag stack and returns it **/
  public static Object pop_tag() {
    return pop_check();
  }

  /** Pushes the tag associated with the static static_num on the tag stack */
  public static void push_static_tag (int static_num) {

    Object static_tag = static_tags.get (static_num);
    assert static_tag != null;
    tag_stack.push (static_tag);
    debug_primitive.log ("push_static_tag[%d] %s%n", static_num, static_tag);
  }

  /** Pops the top of the tag stack into the tag storage for static_num **/
  public static void pop_static_tag (int static_num) {

    check_method_marker();
    static_tags.set (static_num, tag_stack.pop());
    assert static_tags.get(static_num) != null;
    debug_primitive.log ("pop_static_tag[%d] %s%n", static_num,
                         static_tags.get(static_num));
  }

  /**
   * Discard the tag on the top of the tag stack.  Called when primitives
   * are pushed but not used in expressions (such as when allocating arrays)
   */
  public static void discard_tag(int cnt) {

    while (--cnt >= 0) {
      check_method_marker();
      tag_stack.pop();
    }
  }

  private static void primitive_array_store (Object arr_ref, int length,
                                             int index) {

    // look for the tag storage for this array
    Object[] obj_tags = field_map.get (arr_ref);

    // If none has been allocated, allocate the space and associate it with
    // the array
    if (obj_tags == null) {
      obj_tags = new Object[length];
      field_map.put (arr_ref, obj_tags);
    }

    // Pop the tag off of the stack and assign it into the tag storage for
    // this index
    obj_tags[index] = pop_check();
    debug_primitive.log ("array store %s[%d] = %s%n", arr_ref, index,
                         obj_tags[index]);

    // Mark the arry and its index as comparable
    Object index_tag = pop_check();
    debug_arr_index.log ("Merging array '%s' and index '%s'", arr_ref,
                         index_tag);
    TagEntry.union (arr_ref, index_tag);
  }

  /**
   * Execute an aastore instruction and mark the array and its index as
   * comparable.
   */
  public static void aastore (Object[] arr, int index, Object val) {

    // Mark the array and its index as comparable
    Object index_tag = pop_check();
    debug_arr_index.log ("Merging array '%s' and index '%s'", arr, index_tag);
    TagEntry.union (arr, index_tag);

    // Store the value
    arr[index] = val;
  }

  /**
   * Execute an iastore instruction and manipulate the tags accordingly.
   * The tag at the top of stack is stored into the tag storage for the
   * array.
   */
  public static void bastore (byte[] arr, int index, byte val) {

    // Store the tag for val in the tag storage for array and mark
    // the array and the index as comparable.
    primitive_array_store (arr, arr.length, index);

    // Execute the array store
    arr[index] = val;
  }

  /**
   * Execute an iastore instruction and manipulate the tags accordingly.
   * The tag at the top of stack is stored into the tag storage for the
   * array.
   */
  public static void castore (char[] arr, int index, char val) {

    // Store the tag for val in the tag storage for array and mark
    // the array and the index as comparable.
    primitive_array_store (arr, arr.length, index);

    // Execute the array store
    arr[index] = val;
  }
  /**
   * Execute an iastore instruction and manipulate the tags accordingly.
   * The tag at the top of stack is stored into the tag storage for the
   * array.
   */
  public static void dastore (double[] arr, int index, double val) {

    // Store the tag for val in the tag storage for array and mark
    // the array and the index as comparable.
    primitive_array_store (arr, arr.length, index);

    // Execute the array store
    arr[index] = val;
  }

  /**
   * Execute an iastore instruction and manipulate the tags accordingly.
   * The tag at the top of stack is stored into the tag storage for the
   * array.
   */
  public static void fastore (float[] arr, int index, float val) {

    // Store the tag for val in the tag storage for array and mark
    // the array and the index as comparable.
    primitive_array_store (arr, arr.length, index);

    // Execute the array store
    arr[index] = val;
  }

  /**
   * Execute an iastore instruction and manipulate the tags accordingly.
   * The tag at the top of stack is stored into the tag storage for the
   * array.
   */
  public static void iastore (int[] arr, int index, int val) {

    // Store the tag for val in the tag storage for array and mark
    // the array and the index as comparable.
    primitive_array_store (arr, arr.length, index);

    // Execute the array store
    arr[index] = val;
  }

  /**
   * Execute an lastore instruction and manipulate the tags accordingly.
   * The tag at the top of stack is stored into the tag storage for the
   * array.
   */
  public static void lastore (long[] arr, int index, long val) {

    // Store the tag for val in the tag storage for array and mark
    // the array and the index as comparable.
    primitive_array_store (arr, arr.length, index);

    // Execute the array store
    arr[index] = val;
  }

  /**
   * Execute an sastore instruction and manipulate the tags accordingly.
   * The tag at the top of stack is stored into the tag storage for the
   * array.
   */
  public static void sastore (short[] arr, int index, short val) {

    // Store the tag for val in the tag storage for array and mark
    // the array and the index as comparable.
    primitive_array_store (arr, arr.length, index);

    // Execute the array store
    arr[index] = val;
  }

  /**
   * Called when a user method is entered.  Any daikon variables whose current
   * values are comparable are marked as comparable.
   *
   * @param tag_frame tag_frame containing the tags for the primitive
   *        arguments of this method.
   * @param obj value of 'this'.  Null if the method is static
   * @param mi_index index into the list of all methods (methods)
   * @param args Array of the arguments to the method.
   */
  public static void enter (Object[] tag_frame, Object obj, int mi_index,
                            Object[] args) {

    // Don't be recursive
    if (in_enter_exit)
      return;
    in_enter_exit = true;

    if (debug) {
      Throwable stack = new Throwable ("enter");
      stack.fillInStackTrace();
      StackTraceElement[] ste_arr = stack.getStackTrace();
      StackTraceElement ste = ste_arr[1];
      if (ignore_toString && ste.getMethodName().equals ("toString")) {
        in_enter_exit = false;
        return;
      }
      System.out.printf ("%s.%s():::ENTER%n%n", ste.getClassName(),
                         ste.getMethodName());

      System.out.printf ("this = '%s', mi = %s%n", obj_str(obj),
                         methods.get(mi_index));
      System.out.printf ("args: ");
      for (Object arg : args)
        System.out.printf ("%s ", obj_str(arg));
      System.out.printf ("%n");
    }

    MethodInfo mi = methods.get (mi_index);
    ClassInfo ci = mi.class_info;
    if (ci.clazz == null) {
      ci.initViaReflection();
      all_classes.add (ci);
      daikon.chicory.Runtime.all_classes.add (ci);
      System.out.printf ("initializing traversal for %s%n", ci);
      ci.init_traversal(2);
    }
    if (mi.traversalEnter == null) {
      mi.init_traversal (2);
    }

    // Merge comparability information for the Daikon variables
    merge_dv.log ("processing method %s:::ENTER%n", mi);
    merge_dv.indent();
    process_all_vars (mi, mi.traversalEnter, tag_frame, obj, args, null);
    merge_dv.exdent();

    in_enter_exit = false;
  }

  /**
   * Called when a user method exits.  Any daikon variables whose current
   * values are comparable are marked as comparable.
   *
   * @param tag_frame tag_frame containing the tags for the primitive
   * arguments of this method.
   * @param obj value of 'this'.  Null if the method is static
   * @param mi_index index into the list of all methods (methods)
   * @param args Array of the arguments to the method.
   * @param ret_val Value returned by the method.  Null if the method is a
   * constructor or void,
   * @param exit_line_number the source line number of this exit point
   */

  public static void exit (Object[] tag_frame, Object obj, int mi_index,
                         Object[] args, Object ret_val, int exit_line_number) {

    // Don't be recursive
    if (in_enter_exit)
      return;
    in_enter_exit = true;

    if (debug) {
      Throwable stack = new Throwable ("exit");
      stack.fillInStackTrace();
      StackTraceElement[] ste_arr = stack.getStackTrace();
      StackTraceElement ste = ste_arr[1];
      if (ignore_toString && ste.getMethodName().equals ("toString")) {
        in_enter_exit = false;
        return;
      }
      System.out.printf ("%s.%s():::EXIT%n%n", ste.getClassName(),
                         ste.getMethodName());

      System.out.printf ("this = '%s', mi = %s%n", obj_str(obj),
                         methods.get(mi_index));
      System.out.printf ("args: ");
      for (Object arg : args)
        System.out.printf ("%s ", obj_str(arg));
      System.out.printf ("%n");
      System.out.printf ("ret_val = %s, exit_line_number= %d%n", ret_val,
                         exit_line_number);
    }

    MethodInfo mi = methods.get (mi_index);

    // Merge comparability information for the Daikon variables
    merge_dv.log ("processing method %s:::EXIT%n", mi);
    merge_dv.indent();
    process_all_vars (mi, mi.traversalExit, tag_frame, obj, args, ret_val);
    merge_dv.exdent();

    in_enter_exit = false;
  }

  /**
   * Process all of the daikon variables in the tree starting at root.
   * If the values referenced by those variables are comparable mark
   * the variables as comparable.
   */
  public static void process_all_vars (MethodInfo mi, RootInfo root,
                                       Object[] tag_frame, Object obj,
                                       Object[] args, Object ret_val) {


    merge_dv.log ("this: %s%n", obj);
    merge_dv.log ("arguments: %s%n", ArraysMDE.toString(args));

    // Map from an Object to the Daikon variable that currently holds
    // that object.
    IdentityHashMap<Object,DaikonVariableInfo> varmap
      = new IdentityHashMap<Object,DaikonVariableInfo>();

    for (DaikonVariableInfo dv : root) {
      if (dv instanceof ThisObjInfo) {
        merge_comparability (varmap, null, obj, dv);
      } else if (dv instanceof ParameterInfo) {
        ParameterInfo pi = (ParameterInfo) dv;
        Object p = args[pi.getArgNum()];
        Class arg_type = mi.arg_types[pi.getArgNum()];
        if (arg_type.isPrimitive())
          p = tag_frame[pi.get_param_offset() + ((obj == null) ? 0 : 1)];
        merge_comparability (varmap, null, p, pi);
      } else if (dv instanceof ReturnInfo) {
        ReturnInfo ri = (ReturnInfo) dv;
        if (mi.return_type().isPrimitive())
          ret_val = tag_stack.peek();
        merge_comparability (varmap, null, ret_val, dv);
      } else if (dv instanceof FieldInfo) {
        assert ((FieldInfo)dv).isStatic() : "non static field at root " + dv;
        merge_comparability (varmap, null, null, dv);
      } else {
        assert false : "unexpected node " + dv;
      }
    }
  }

  /**
   * Gets the tag for the specified field.  The tags for primitive instance
   * fields are stored in the tag storage for the parent.  Tags for
   * primitive static fields are stored a single global list (statics_tags)
   * indexed by the value for the static name in static_map.  If the field
   * is in an array a List<Object> with one tag per element is returned.
   *
   * The tag for non-primitive fields is the object itself.
   *
   * @param parent Value of dv's parent
   * @param obj    Value of dv
   * @param dv     DaikonVariable to process
   */
  static Object get_field_tag (FieldInfo fi, Object parent, Object obj) {

    Object tag = null;

    if (fi.isStatic()) {
      if (fi.getType().isPrimitive()) {
        tag = static_tags.get (static_map.get (fi.getName()));
      } else { // the tag is the object itself.  Get it via reflection
        Field field = fi.getField();
        if (is_class_init (field.getDeclaringClass())) {
          if (!field.isAccessible())
            field.setAccessible (true);
          try {
            tag = field.get (null);
          } catch (Exception e) {
            throw new RuntimeException("Can't get val for static field "
                                       + field, e);
          }
        } else {
          tag = nonsensical;
        }
      }
    } else {
      if (fi.getType().isPrimitive()) {
        String tag_field_name = tag_field_name(fi.getField().getName());
        if (fi.isArray()) {
          List<Object> parent_list = (List<Object>)parent;
          Field tag_field = null;
          List<Object> tag_list = new ArrayList<Object>(parent_list.size());
          for (Object parent_element : parent_list) {
            // if (tag_field == null)
              //tag_field = fi.get_tag_field (tag_field_name,
              //                              parent_element.getClass());
            Object[] tags = field_map.get (parent_element);
            assert tags != null : "array " + fi + " " + parent_element;
            tag_list.add (tags[fi.get_field_num()]);
                          // get_object_field (tag_field, parent_element));

          }
          tag = tag_list;
        } else {
          //Field tag_field = fi.get_tag_field (tag_field_name,
          //                                    parent.getClass());
          // tag = get_object_field (tag_field, parent);
          Object[] tags = field_map.get (parent);
          assert tags != null : fi + " " + parent;
          tag = tags[fi.get_field_num()];
        }
      } else {
        tag = obj;
      }
    }

    merge_dv.log ("Tag for field %s = %s%n", fi.getField(), tag);
    return (tag);
  }

  /**
   * Gets the object in field f in object obj.  Exceptions are turned
   * into Errors
   */
  public static Object get_object_field (Field f, Object obj) {
    try {
      return f.get (obj);
    } catch (Exception e) {
      throw new Error ("can't get field " + f + " in " + obj_str (obj), e);
    }
  }

  /**
   * Merges the comparability of the daikon variable dv and its children
   * whose current values are comparable.
   *
   * @param varmap Map from value set leaders to the first daikon variable
   *               encountered with that leader.  Whenever a second daikon
   *               variable is encountered whose value has the same leader,
   *               that daikon variable is merged with the first daikon
   *               variable
   * @param parent Value of dv's parent
   * @param obj    Value of dv
   * @param dv     DaikonVariable to process
   */
  static void merge_comparability (IdentityHashMap<Object,DaikonVariableInfo> varmap,
                                   Object parent, Object obj,
                                   DaikonVariableInfo dv) {

    merge_dv.log ("merge_comparability: checking var %s = '%s' %n",
                    dv, obj_str(obj));

    // Ignore ClassInfo and StringInfo variables.  These are not real
    // variables in the program
    if ((dv instanceof DaikonClassInfo) || (dv instanceof StringInfo))
      return;

    // Get the tag for this object.  For non-primitives this is normally the
    // object itself.  For static fields, the object is not passed in, but is
    // obtained via reflection.
    Object tag = obj;
    if (dv instanceof FieldInfo)
      tag = get_field_tag ((FieldInfo) dv, parent, obj);

    if (dv.isArray()) {
      List<Object> elements = (List<Object>)tag;
      for (Object atag : elements) {
        // Ignore null and nonsensical tags.  There is no reason to process
        // their children, because they can't have any with reasonable values
        if ((atag == null) || (atag == nonsensical)
            || (atag == nonsensical_list))
          continue;

        // Look up this object.  If it already is associated with a
        // DaikonVariable merge those variables.  Otherwise, add it to
        // the map
        Object leader = TagEntry.find(atag);
        merge_dv.log ("Leader for atag '%s' is '%s'%n", obj_str(atag),
                      obj_str (leader));
        DaikonVariableInfo current = varmap.get (leader);
        merge_dv.log ("Daikon variable for leader = %s%n", current);
        if (current != null)
          TagEntry.union (current, dv);
        else
          varmap.put (leader, dv);
      }
    } else {
      // Ignore null and nonsensical tags.  There is no reason to process
      // their children, because they can't have any with reasonable values
      if ((tag == null) || (tag == nonsensical) || (tag == nonsensical_list))
        return;

      // Look up this object.  If it already is associated with a
      // DaikonVariable merge those variables.  Otherwise, add it to
      // the map
      Object leader = TagEntry.find(tag);
      merge_dv.log ("Leader for tag '%s' is '%s'%n", obj_str(tag),
                    obj_str (leader));
      DaikonVariableInfo current = varmap.get (leader);
      merge_dv.log ("Daikon variable for leader = %s%n", current);
      if (current != null) {
        merge_dv.log ("Merging variable '%s' and '%s'%n", current, dv);
        TagEntry.union (current, dv);
      } else
        varmap.put (leader, dv);
    }

    // Process all of the children
    for (DaikonVariableInfo child : dv) {
      Object child_obj = child.getMyValFromParentVal (tag);
      merge_comparability (varmap, tag, child_obj, child);
    }
  }

  /**
   * Dumps out comparability information for all classes that were
   * processed.
   */
  public static void print_all_comparable(PrintStream ps) {

    for (ClassInfo ci : all_classes) {
      for (MethodInfo mi : ci.method_infos) {
        if (mi.is_class_init())
          continue;
        ps.printf ("%n");
        print_comparable (ps, mi);
      }
    }
  }

  public static void print_decl_file (PrintStream ps) {

    // Write the file header
    ps.printf ("// Declaration file written by daikon.dcomp%n%n");
    ps.printf ("VarComparability%nimplicit%n");

    // Write the information for each class
    for (ClassInfo ci : all_classes) {
      print_class_decl (ps, ci);
    }
  }

  /**
   * Calculates and prints the declarations for the specified class
   */
  public static void print_class_decl (PrintStream ps, ClassInfo ci) {

    // Make sure that two variables have the same comparability at all
    // program points
    merge_class_comparability (ci);

    // Write the class ppt
    ps.printf ("DECLARE%n");
    ps.printf ("%s:::CLASS%n", ci.class_name);
    print_decl_vars (ps, get_comparable (ci.traversalClass));
    ps.printf ("%n");

    // Write the object ppt
    ps.printf ("DECLARE%n");
    ps.printf ("%s:::OBJECT%n", ci.class_name);
    print_decl_vars (ps, get_comparable (ci.traversalObject));
    ps.printf ("%n");

    // Print the information for each enter/exit point
    for (MethodInfo mi : ci.method_infos) {
      if (mi.is_class_init())
        continue;
      ps.printf ("%n");
      print_decl (ps, mi);
    }
  }

  /**
   * Prints a decl ENTER/EXIT records with comparability.  Returns the
   * list of comparabile DVSets for the exit.
   */
  public static List<DVSet> print_decl (PrintStream ps, MethodInfo mi) {

    List<DVSet> l = get_comparable (mi.traversalEnter);
    if (l == null)
      return (null);

    // Print the enter point
    ps.printf ("DECLARE%n");
    ps.printf ("%s%n", clean_decl_name (DaikonWriter.methodEntryName
                                        (mi.member)));
    print_decl_vars (ps, l);
    ps.printf ("%n");

    // Print the exit points
    l = get_comparable (mi.traversalExit);
    for (Integer ii : mi.exit_locations) {
      ps.printf ("DECLARE%n");
      ps.printf ("%s%n", clean_decl_name (DaikonWriter.methodExitName
                                          (mi.member, ii)));
      print_decl_vars (ps, l);
      ps.printf ("%n");
    }

    return (l);
  }

  /**
   * Print the variables in sets to ps in DECL file format.  Each
   * variable in the same set is given the same comparability.  Constructed
   * classname variables are made comparable to opther classname variables
   * only.
   */
  private static void print_decl_vars (PrintStream ps, List<DVSet> sets) {

    // Map from array name to comparability for its indices (if any)
    HashMap<String, Integer> arr_index_map = new HashMap<String,Integer>();

    int class_comp = 1;
    int comp = 2;
    for (DVSet set : sets) {
      boolean hashcode_vars = false;
      boolean int_vars = false;
      for (DaikonVariableInfo dv : set) {
        if (dv.isHashcode())
          hashcode_vars = true;
        else if (dv.isInt())
          int_vars = true;
      }
      int int_comp = comp;
      int hashcode_comp = comp;
      if (hashcode_vars && int_vars)
        int_comp++;
      boolean used_comp = true;
      for (DaikonVariableInfo dv : set) {
        ps.printf ("%s%n", dv.getName());
        ps.printf ("%s%n", dv.getTypeName());
        ps.printf ("%s%n", dv.getRepTypeName());
        if (dv instanceof DaikonClassInfo) {
          ps.printf ("%d%n", class_comp);
          used_comp = false;
          assert set.size() == 1 : "odd set " + set;
        } else if (dv.isHashcode() && int_vars) {
          ps.printf ("%d%n", hashcode_comp);
          arr_index_map.put (dv.getName() + "[]", int_comp);
          // System.out.printf ("putting '%s[]' -> %d in map%n", dv.getName(),
          //                   int_comp);
        } else if (dv.isInt())
          ps.printf ("%d%n", int_comp);
        else if (dv.isArray()) {
          Integer index_comp = arr_index_map.get (dv.getName());
          // System.out.printf ("found %d index comp for '%s'%n", index_comp,
          //                    dv.getName());
          if (index_comp != null)
            ps.printf ("%d[%d]%n", comp, index_comp);
          else
            ps.printf("%d%n", comp);
        } else {
          ps.printf ("%d%n", comp);
        }
      }
      if (used_comp)
        comp++;
      if (hashcode_vars && int_vars)
        comp++;
    }
  }
  /**
   * Prints comparabilty information for the enter and exit points of
   * the specified method
   */
  public static void print_comparable (PrintStream ps, MethodInfo mi) {

    List<DVSet> l = get_comparable (mi.traversalEnter);
    ps.printf ("Daikon Variable sets for %s enter%n",
               clean_decl_name(mi.toString()));
    if (l == null)
      ps.printf ("  not called%n");
    else {
      for (DVSet set : l) {
        ps.printf ("  [%d] %s%n", set.size(), set);
      }
    }

    l = get_comparable (mi.traversalExit);
    ps.printf ("Daikon Variable sets for %s exit%n",
               clean_decl_name (mi.toString()));
    if (l == null)
      ps.printf ("  not called%n");
    else {
      for (DVSet set : l) {
        ps.printf ("  [%d] %s%n", set.size(), set);
      }
    }
  }

  /**
   * Set of Daikon variables.  Implements comparable on first DaikonVariable
   * in each set.
   */
  private static class DVSet extends ArrayList<DaikonVariableInfo>
    implements Comparable<DVSet> {
    static final long serialVersionUID = 20050923L;

    public int compareTo (DVSet s1) {
      if (s1.size() == 0)
        return 1;
      else if (size() == 0)
        return -1;
      else return get(0).compareTo(s1.get(0));
    }
    public void sort() {
      Collections.sort (this);
    }
  }

  /**
   * Gets a list of sets of comparable daikon variables.  For simplicity
   * the sets are represented as a list as well.  If the method has never
   * been executed returns null (it would probably be better to return
   * each variable in a separate set, but I wanted to differentiate this
   * case for now).
   *
   * The sets are calculated by processing each daikon variable and adding
   * it to a list associated with the leader of that set.
   */
  static List<DVSet> get_comparable (RootInfo root) {

    if (root == null)
      return (null);

    // List of all of the sets of comparable daikon variables
    Map<DaikonVariableInfo,DVSet> sets
      = new IdentityHashMap<DaikonVariableInfo, DVSet>();

    for (DaikonVariableInfo dv : root)
      add_variable (sets, dv);

    // Get each set, sort it, and add it to the list of all sets.  The sort
    // the list of all sets.  The sorting is not critical except to create
    // a reproducible order
    List<DVSet> set_list = new ArrayList<DVSet>(sets.size());
    for (DVSet dvs : sets.values()) {
      dvs.sort();
      set_list.add (dvs);
    }
    Collections.sort (set_list);

    return (set_list);

  }

  /**
   * Merges comparability so that the same variable have the same
   * comparability at all points in the program point hierarchy.
   * The comparability at the class/object points is calculated by
   * merging the comparability at each exit point (ie, if two variables
   * are in the same set it any exit point, they are in the same set at
   * the class point).  That comparability is then applied back to the
   * exit points so that if two class variables are comparable at any
   * exit point they are comparable at each exit point.  Finally exit
   * point comparability is merged to the enter point so that their
   * comparabilities are the same.
   *
   * This is not the only valid definition of comparability but it is
   * the one that Daikon expects because of how equality sets are handled.
   */
  static void merge_class_comparability (ClassInfo ci) {

    // Get the variables at the object and class point
    assert ci.traversalObject != null : ci;
    assert ci.traversalClass != null : ci;
    // ci.init_traversal (depth);

    // If any methods have not been executed, create their information
    // now (which will note all of their variables as not comparable)
    for (MethodInfo mi : ci.method_infos) {
      if (mi.is_class_init())
        continue;
      if (mi.traversalEnter == null) {
        // mi.initViaReflection();
        mi.init_traversal (depth);
        System.out.printf ("Warning: Method %s never executed%n", mi);
      }
    }

    // Merge the comparability from each exit point into the object point
    for (MethodInfo mi : ci.method_infos) {
      if (mi.is_class_init())
        continue;
      debug_merge_comp.log ("Merging %s exit to object%n", mi);
      merge_dv_comparability (mi.traversalExit, ci.traversalObject);
    }

    // Merge the comparability from the object point back to each exit point
    for (MethodInfo mi : ci.method_infos) {
      if (mi.is_class_init())
        continue;
      debug_merge_comp.log ("merging object to %s exit%n", mi);
      merge_dv_comparability (ci.traversalObject, mi.traversalExit);
    }

    // Merge the comparability for each exit point back to the enter
    for (MethodInfo mi : ci.method_infos) {
      if (mi.is_class_init())
        continue;
      debug_merge_comp.log ("merging %s exit to its enter%n", mi);
      merge_dv_comparability (mi.traversalExit, mi.traversalEnter);
    }

    // Merge the object comparability to the class
    debug_merge_comp.log ("merging %s object to class%n", ci);
    merge_dv_comparability (ci.traversalObject, ci.traversalClass);
  }

  /**
   * Merges any variables in the dest tree that are in the same set in
   * the source tree.  The source tree's comparability is unchanged.
   * Variables are identified by name
   */
  static void merge_dv_comparability (RootInfo src, RootInfo dest){

    debug_merge_comp.indent();

    // Create a map relating destination names to their variables
    Map<String,DaikonVariableInfo> dest_map
      = new LinkedHashMap<String,DaikonVariableInfo>();
    for (DaikonVariableInfo dvi : varlist(dest)) {
      dest_map.put (dvi.getName(), dvi);
    }

    // Get the variable sets for the source
    List<DVSet> src_sets = get_comparable (src);

    // Merge any destination variables that are in the same source set
    for (DVSet set : src_sets) {
      if (set.size() == 1)
        continue;
      DaikonVariableInfo first_match = null;
      for (DaikonVariableInfo dvi : set) {
        if (first_match == null) {
          first_match = dest_map.get (dvi.getName());
          continue;
        }
        DaikonVariableInfo second_match = dest_map.get (dvi.getName());
        if (second_match != null) {
          TagEntry.union (first_match, second_match);
          debug_merge_comp.log ("merged '%s' and '%s'%n", first_match,
                                second_match);
        }
      }
    }
    debug_merge_comp.exdent();
  }

  /**
   * Adds this daikon variable and all of its children into their appropriate
   * sets (those of their leader) in sets.
   */
  static void add_variable (Map<DaikonVariableInfo,DVSet> sets,
                            DaikonVariableInfo dv) {

    // Add this variable into the set of its leader
    DaikonVariableInfo leader = (DaikonVariableInfo) TagEntry.find (dv);
    DVSet set = sets.get (leader);
    if (set == null) {
      set = new DVSet();
      sets.put (leader, set);
    }
    set.add (dv);

    // Process the children
    for (DaikonVariableInfo child : dv)
      add_variable (sets, child);
  }

  /**
   * Pushes the tag associated with field_num in obj on the tag stack.
   * If a tag value has not yet been stored for any field in this object,
   * null is pushed as the tag.
   */
  public static void push_field_tag (Object obj, int field_num) {

    Object[] obj_tags = field_map.get (obj);
    if (obj_tags != null) {
      tag_stack.push (obj_tags[field_num]);
      debug_primitive.log ("push_field_tag %s [%s] %d = %s%n", obj,
                     obj.getClass().getName(), field_num, obj_tags[field_num]);
    } else {
      tag_stack.push (null);
      debug_primitive.log ("push_field_tag %s [%s] %d = null%n", obj,
                           obj.getClass().getName(), field_num);
    }
  }

  /**
   * Pops the tag from the top of the tag stack and stores it in the
   * tag storage for the specified field of the specified object.  If
   * tag storage was not previously allocated, it is allocated now
   */
  public static void pop_field_tag (Object obj, int field_num) {

    // Look for the tag storage for this object
    Object[] obj_tags = field_map.get (obj);

    // If none has been allocated, determine how many locations are
    // required (the number of primitive fields), allocate the space,
    // and associate it with the object.
    if (obj_tags == null) {
      Class obj_class = obj.getClass();
      int fcnt = num_prim_fields (obj.getClass());
      assert field_num < fcnt : obj.getClass() + " " + field_num + " " + fcnt;
      obj_tags = new Object[fcnt];
      field_map.put (obj, obj_tags);
      debug_primitive.log ("pop_field_tag: Created tag storage%n");
    }

    // Pop the tag off of the stack and assign into the tag storage for
    // this field.
    check_method_marker();
    obj_tags[field_num] = tag_stack.pop();
    debug_primitive.log ("pop_field_tag (%s [%s] %d = %s%n", obj,
                   obj.getClass().getName(), field_num, obj_tags[field_num]);

  }

    /**
     * Return the number of primitive fields in clazz and all of its
     * superclasses
     */
    public static int num_prim_fields (Class clazz) {
      if (clazz == Object.class)
        return 0;
      else {
        int field_cnt = num_prim_fields (clazz.getSuperclass());
        for (Field f : clazz.getDeclaredFields()) {
          if (f.getType().isPrimitive())
            field_cnt++;
        }
        return (field_cnt);
      }
    }

  /**
   * Handle a binary operation on the two items at the top of the tag
   * stack.  Binary operations pop the two items off of the top of the
   * stack perform an operation and push the result back on the stack.
   * The tags of the two items on the top of the stack must thus be
   * merged and a representative tag pushed back on the stack.
   */
  public static void binary_tag_op () {
    debug_primitive.log ("binary tag op%n");
    check_method_marker();
    Object tag1 = tag_stack.pop();
    check_method_marker();
    TagEntry.union (tag1, tag_stack.peek());
  }

  /**
   * Handles an i_cmpXX operation.  This opcode compares the two integers
   * on the top of the stack and jumps accordingly.  Thus the two tags on
   * the top of the stack are popped from the tag stack and merged.
   * Very similar to binary_tag_op except that nothing is pushed back on
   * the tag stack.
   */
  public static void cmp_op() {
    debug_primitive.log ("cmp_op%n");
    Object tag1 = pop_check();
    TagEntry.union (tag1, pop_check());
  }

  /** Handles a dup opcode on a primitive **/
  public static void dup() {
    check_method_marker();
    tag_stack.push (tag_stack.peek());
  }

  /** Handles a dup_x1 opcode on a primitive **/
  public static void dup_x1() {
    Object top = pop_check();
    Object nxt = pop_check();
    tag_stack.push (top);
    tag_stack.push (nxt);
    tag_stack.push (top);
  }

  /**
   * Handles a dup_x2 opcode on a primitive.  Currently only support
   * category 1 computational types
   **/
  public static void dup_x2() {
    Object top = pop_check();
    Object tag1 = pop_check();
    Object tag2 = pop_check();
    tag_stack.push (top);
    tag_stack.push (tag2);
    tag_stack.push (tag1);
    tag_stack.push (top);
  }

  public static void dup2() {
    Object top = pop_check();
    Object tag1 = pop_check();
    tag_stack.push (tag1);
    tag_stack.push (top);
    tag_stack.push (tag1);
    tag_stack.push (top);
  }

  public static void dup2_x1() {
    Object top = pop_check();
    Object tag1 = pop_check();
    Object tag2 = pop_check();
    tag_stack.push (tag1);
    tag_stack.push (top);
    tag_stack.push (tag2);
    tag_stack.push (tag1);
    tag_stack.push (top);
  }

  public static void dup2_x2() {
    Object top = pop_check();
    Object tag1 = pop_check();
    Object tag2 = pop_check();
    Object tag3 = pop_check();
    tag_stack.push (tag1);
    tag_stack.push (top);
    tag_stack.push (tag3);
    tag_stack.push (tag2);
    tag_stack.push (tag1);
    tag_stack.push (top);
  }

  /** swaps the two elements on the top of the tag stack **/
  public static void swap() {
    Object top = pop_check();
    Object tag1 = pop_check();
    tag_stack.push (top);
    tag_stack.push (tag1);
  }

  /**
   * Handles the various primitive (int, double, etc) array load instructions.
   * The array and its index are made comparable.  The tag for the
   * index is removed from the tag stack and the tag for the array
   * element is pushed on the stack.
   */
  public static void primitive_array_load (Object arr_ref, int index) {

    // Get the tag for the index and mark it as comparable with the array
    Object index_tag = pop_check();
    debug_arr_index.log ("Merging array '%s' and index '%s'", arr_ref,
                         index_tag);
    TagEntry.union (arr_ref, index_tag);

    // Push the tag for the element on the tag stack.
    Object[] obj_tags = field_map.get (arr_ref);
    if (obj_tags != null) {
      tag_stack.push (obj_tags[index]);
      debug_primitive.log ("arrayload %s[%d] = %s%n", arr_ref, index,
                           obj_str(obj_tags[index]));
    } else {
      tag_stack.push (null);
      debug_primitive.log ("iaload %s[%d]  = null%n", arr_ref, index);
    }
  }

  /**
   * Handles the aaload instruction.  The arry and its index are made
   * comparable.  The tag for the index is removed from the tag
   * stack.
   */
  public static void ref_array_load (Object arr_ref, int index) {

    // Get the tag for the index and mark it as comparable with the array
    Object index_tag = pop_check();
    debug_arr_index.log ("Merging array '%s' and index '%s'", arr_ref,
                         index_tag);
    TagEntry.union (arr_ref, index_tag);
  }

  /**
   * Allocate a new tag for the constant and push it on the tag stack.
   * Note that this allocates a new tag each time the constant is pushed.
   * If the same code is executed multiple time (eg, in a loop), and
   * different values interact with the constant each time, those values
   * will not end up comparable to each other.
   */
  public static void push_const() {
    Object tag = new Constant();
    debug_primitive.log ("pushing literal constant %s%n", tag);
    tag_stack.push (tag);
  }

  /**
   * Marks the specified class as initialized.  We don't look at static
   * variables in classes until they are initialized
   */
  public static void class_init (String classname) {
    init_classes.add (classname);
  }

  /**
   * Returns whether or not the specified class is initialized
   */
  public static boolean is_class_init (Class clazz) {
    return (init_classes.contains (clazz.getName()));
  }

  /**
   * Returns the name of the method that called the caller of
   * caller_name()
   */
  private static String caller_name() {

    Throwable stack = new Throwable ("caller");
    stack.fillInStackTrace();
    StackTraceElement[] ste_arr = stack.getStackTrace();
    StackTraceElement ste = ste_arr[2];
    return (ste.getClassName() + "." + ste.getMethodName());
  }

  /**
   * Make sure that the top of the stack is not the method marker.  Should
   * be called before every pop
   */
  private static void check_method_marker() {

    assert tag_stack.peek() != method_marker;
  }

  /**
   * Pops the top item off the stack after checking to insure that it
   * is not the marker between methods
   */
  private static Object pop_check() {
    check_method_marker();
    return tag_stack.pop();
  }

  /**
   * Returns a string description of the object that includes its class,
   * identity hash code, and the result of its toString() function (if it
   * overrides the default implementation
   */
  private static String obj_str (Object obj) {

    if (obj == null)
      return ("null");
    else {
      String tostring = obj.toString();
      String default_tostring = String.format ("%s@%x",
                                               obj.getClass().getName(),
                                               System.identityHashCode (obj));
      if (tostring.equals (default_tostring))
        return tostring;
      else
        return String.format ("%s [%s]", default_tostring, tostring);
    }
  }

  /**
   * Returns all of the daikonvariables in the tree rooted at dvi
   * in a list
   */
  private static List<DaikonVariableInfo> varlist (DaikonVariableInfo dvi) {

    List<DaikonVariableInfo> list = new ArrayList<DaikonVariableInfo>();
    list.add (dvi);
    for (DaikonVariableInfo child : dvi) {
      list.addAll (varlist (child));
    }
    return (list);
  }

  /**
   * Returns the name of the tag field that corresponds to the specified
   * field
   */
  public static String tag_field_name (String field_name) {
    return (field_name + "__$tag");
  }

  private static String clean_decl_name (String decl_name) {

    decl_name = decl_name.replace (", daikon.dcomp.DCompMarker marker", "");
    decl_name = decl_name.replace ("daikon.dcomp.DCompMarker marker", "");
    decl_name = decl_name.replace (", java.lang.DCompMarker marker", "");
    decl_name = decl_name.replace ("java.lang.DCompMarker marker", "");
    return decl_name;
  }
}
