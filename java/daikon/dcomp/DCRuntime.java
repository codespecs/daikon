package daikon.dcomp;

import java.util.*;

import daikon.chicory.*;

class DCRuntime {

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

  private static final NonsensicalList nonsensical_list
    = NonsensicalList.getInstance();

  /**
   * List of all classes encountered.  These are the classes that will
   * have comparability output
   **/
  private static List<ClassInfo> all_classes = new ArrayList<ClassInfo>();

  /** Control debug printouts **/
  private static final boolean debug = true;

  /** Simplifies printouts for debugging if we ignore toString **/
  private static boolean ignore_toString = true;

  public static boolean object_eq (Object obj1, Object obj2) {

    // Note that obj1 and obj2 are comparable
    TagEntry.union (obj1, obj2);

    return (obj1 == obj2);
  }

  public static boolean object_ne (Object obj1, Object obj2) {

    // Note that obj1 and obj2 are comparable
    TagEntry.union (obj1, obj2);

    return (obj1 != obj2);
  }

  /**
   * Called when a user method is entered.  Any daikon variables whose current
   * values are comparable are marked as comparable.
   *
   * @param obj value of 'this'.  Null if the method is static
   * @param mi_index index into the list of all methods (methods)
   * @param args Array of the arguments to the method.
   */
  public static void enter (Object obj, int mi_index, Object[] args) {

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

      System.out.printf ("this = %s, mi = %s%n", obj, methods.get(mi_index));
      System.out.printf ("args: ");
      for (Object arg : args)
        System.out.printf ("%s ", arg);
      System.out.printf ("%n");
    }

    MethodInfo mi = methods.get (mi_index);
    ClassInfo ci = mi.class_info;
    if (ci.clazz == null) {
      ci.initViaReflection();
      all_classes.add (ci);
    }
    if (mi.traversalEnter == null) {
      mi.init_traversal (2);
    }

    // Merge comparability information for the Daikon variables
    process_all_vars (mi.traversalEnter, obj, args, null);

    in_enter_exit = false;
  }

  /**
   * Called when a user method exits.  Any daikon variables whose current
   * values are comparable are marked as comparable.
   *
   * @param obj value of 'this'.  Null if the method is static
   * @param mi_index index into the list of all methods (methods)
   * @param args Array of the arguments to the method.
   * @param ret_val Value returned by the method.  Null if the method is a
   * constructor or void,
   * @param exit_line_number the source line number of this exit point
   */

  public static void exit (Object obj, int mi_index, Object[] args,
                           Object ret_val, int exit_line_number) {

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

      System.out.printf ("this = %s, mi = %s%n", obj, methods.get(mi_index));
      System.out.printf ("args: ");
      for (Object arg : args)
        System.out.printf ("%s ", arg);
      System.out.printf ("%n");
      System.out.printf ("ret_val = %s, exit_line_number= %d%n", ret_val,
                         exit_line_number);
    }

    MethodInfo mi = methods.get (mi_index);

    // Merge comparability information for the Daikon variables
    process_all_vars (mi.traversalExit, obj, args, ret_val);

    in_enter_exit = false;
  }

  /**
   * Process all of the daikon variables in the tree starting at root.
   * If the values referenced by those variables are comparable mark
   * the variables as comparable.
   */
  public static void process_all_vars (RootInfo root, Object obj,
                                       Object[] args, Object ret_val) {

    IdentityHashMap<Object,DaikonVariableInfo> varmap
      = new IdentityHashMap<Object,DaikonVariableInfo>();

    for (DaikonVariableInfo dv : root) {
      if (dv instanceof ThisObjInfo) {
        merge_comparability (varmap, obj, dv);
      } else if (dv instanceof ParameterInfo) {
        ParameterInfo pi = (ParameterInfo) dv;
        Object p = args[pi.getArgNum()];
        merge_comparability (varmap, p, pi);
      } else if (dv instanceof ReturnInfo) {
        merge_comparability (varmap, ret_val, dv);
      } else {
        assert false : "unexpected node " + dv;
      }
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
   * @param obj    Value of dv
   * @param dv     DaikonVariable to process
   */
  static void merge_comparability (IdentityHashMap<Object,DaikonVariableInfo>
                                   varmap, Object obj, DaikonVariableInfo dv) {

    System.out.printf ("merge_comparability: checking variable %s = [%s] %s%n",
                       dv, ((obj == null)? "": obj.getClass().getName()), obj);

    // Ignore null and nonsensical objects.  There is no reason to process
    // their children, because they can't have any with reasonable values
    if ((obj == null) || (obj == nonsensical) || (obj == nonsensical_list))
      return;

    // Look up this object.  If it already is associated with a DaikonVariable
    // merge those variables.  Otherwise, add it to the map
    DaikonVariableInfo current = varmap.get (TagEntry.find(obj));
    if (current != null)
      TagEntry.union (current, dv);
    else
      varmap.put (obj, dv);

    // Process all of the children
    for (DaikonVariableInfo child : dv) {
      Object child_obj = child.getMyValFromParentVal (obj);
      merge_comparability (varmap, child_obj, child);
    }
  }

  /**
   * Dumps out comparability information for all classes that were
   * processed.
   */
  public static void print_all_comparable() {

    for (ClassInfo ci : all_classes) {
      for (MethodInfo mi : ci.method_infos) {
        System.out.printf ("%n");
        print_comparable (mi);
      }
    }
  }

  /**
   * Prints comparabilty information for the enter and exit points of
   * the specified method
   */
  public static void print_comparable (MethodInfo mi) {

    List<List<DaikonVariableInfo>> l = get_comparable (mi.traversalEnter);
    System.out.printf ("Daikon Variable sets for %s enter%n", mi);
    if (l == null)
      System.out.printf ("  not called%n");
    else {
      for (List<DaikonVariableInfo> set : l) {
        System.out.printf ("-- %s%n", set);
      }
    }

    l = get_comparable (mi.traversalExit);
    System.out.printf ("Daikon Variable sets for %s exit%n", mi);
    if (l == null)
      System.out.printf ("  not called%n");
    else {
      for (List<DaikonVariableInfo> set : l) {
        System.out.printf ("-- %s%n", set);
      }
    }
  }

  /** Map from the daikon variable leader to the elements of its set **/
  private static class DVSet
    extends IdentityHashMap<DaikonVariableInfo,List<DaikonVariableInfo>> {
  }

  /**
   * Gets a list of sets of comparable daikon variables.  For simplicity
   * the sets are represented as a lsit as well.  If the method has never
   * been executed returns null (it would probably be better to return
   * each variable in a separate set, but I wanted to differentiate this
   * case for now).
   *
   * The sets are calculated by processing each daikon variable and adding
   * it to a list associated with the leader of that set.
   */
  static List<List<DaikonVariableInfo>> get_comparable (RootInfo root) {

    if (root == null)
      return (null);

    // List of all of the sets of comparable daikon variables
    DVSet sets = new DVSet();

    for (DaikonVariableInfo dv : root)
      add_variable (sets, dv);

    List<List<DaikonVariableInfo>> set_list
      = new ArrayList<List<DaikonVariableInfo>>(sets.size());
    for (List<DaikonVariableInfo> l : sets.values())
      set_list.add (l);
    return set_list;
  }

  /**
   * Adds this daikon variable and all of its children into their appropriate
   * sets (those of their leader) in sets.
   */
  static void add_variable (DVSet sets, DaikonVariableInfo dv) {

    // Add this variable into the set of its leader
    DaikonVariableInfo leader = (DaikonVariableInfo) TagEntry.find (dv);
    List<DaikonVariableInfo> set = sets.get (leader);
    if (set == null) {
      set = new ArrayList<DaikonVariableInfo>();
      sets.put (leader, set);
    }
    set.add (dv);

    // Process the children
    for (DaikonVariableInfo child : dv)
      add_variable (sets, child);
  }
}
