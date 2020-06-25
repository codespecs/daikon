package daikon.chicory;

import daikon.Chicory;
import java.io.PrintStream;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.Member;
import java.util.ArrayList;
import java.util.List;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.Nullable;

/**
 * DTraceWriter writes {@code .dtrace} program points to an output stream. It uses the trees created
 * by the {@link DeclWriter}.
 */
@SuppressWarnings("nullness")
public class DTraceWriter extends DaikonWriter {
  // Notes:
  //
  //  - methodExit(): handles exits from a method
  //    - printReturnValue: prints return value and related vars
  //      - checkForRuntimeClass: prints return.class and value
  //    - traceMethod(): prints out this and arguments of method
  //      - traceMethodVars: prints out this and arguments of method
  //        - traceLocalVars: prints arguments
  //          - traceLocalVar: prints one argument
  //          - checkForVarRecursion: recursive check on on argument
  //        - traceClassVars: prints fields in a class

  /** instance of a nonsensical value */
  private static NonsensicalObject nonsenseValue = NonsensicalObject.getInstance();
  /** instance of a nonsensical list */
  private static List<Object> nonsenseList = NonsensicalList.getInstance();

  // certain class names
  protected static final String classClassName = "java.lang.Class";
  protected static final String stringClassName = "java.lang.String";

  /** Where to print output. */
  private PrintStream outFile;

  /** debug information about daikon variables */
  private boolean debug_vars = false;

  /**
   * Initializes the DTraceWriter.
   *
   * @param writer stream to write to
   */
  public DTraceWriter(PrintStream writer) {
    super();
    outFile = writer;
  }

  /** Prints the method entry program point in the dtrace file. */
  public void methodEntry(
      @GuardSatisfied DTraceWriter this,
      MethodInfo mi,
      int nonceVal,
      @Nullable Object obj,
      Object[] args) {
    // don't print
    if (Runtime.dtrace_closed) {
      return;
    }

    Member member = mi.member;

    // get the root of the method's traversal pattern
    RootInfo root = mi.traversalEnter;
    if (root == null) {
      throw new RuntimeException("Traversal pattern not initialized at method " + mi.method_name);
    }

    if (debug_vars) {
      System.out.printf("Entering %s%n%s%n", DaikonWriter.methodEntryName(member), root);
      Throwable stack = new Throwable("enter traceback");
      stack.fillInStackTrace();
      stack.printStackTrace(System.out);
    }
    outFile.println(DaikonWriter.methodEntryName(member));
    printNonce(nonceVal);
    traverse(mi, root, args, obj, nonsenseValue);

    outFile.println();

    Runtime.incrementRecords();
  }

  /** Prints an entry program point for a static initializer in the dtrace file. */
  public void clinitEntry(@GuardSatisfied DTraceWriter this, String pptname, int nonceVal) {
    // don't print
    if (Runtime.dtrace_closed) {
      return;
    }
    outFile.println(pptname);
    printNonce(nonceVal);
    outFile.println();
    Runtime.incrementRecords();
  }

  /** Prints the method exit program point in the dtrace file. */
  public void methodExit(
      @GuardSatisfied DTraceWriter this,
      MethodInfo mi,
      int nonceVal,
      @Nullable Object obj,
      Object[] args,
      Object ret_val,
      int lineNum) {
    if (Runtime.dtrace_closed) {
      return;
    }

    Member member = mi.member;

    // gets the traversal pattern root for this method exit
    RootInfo root = mi.traversalExit;
    if (root == null) {
      throw new RuntimeException(
          "Traversal pattern not initialized for method " + mi.method_name + " at line " + lineNum);
    }

    // make sure the line number is valid
    // i.e., it is one of the exit locations in the MethodInfo for this method
    if (mi.exit_locations == null || !mi.exit_locations.contains(lineNum)) {
      throw new RuntimeException(
          "The line number "
              + lineNum
              + " is not found in the MethodInfo for method "
              + mi.method_name
              + DaikonWriter.lineSep
              + "No exit locations found in exit_locations set!");
    }

    outFile.println(DaikonWriter.methodExitName(member, lineNum));
    printNonce(nonceVal);
    traverse(mi, root, args, obj, ret_val);

    outFile.println();

    Runtime.incrementRecords();
  }

  /** Prints an exit program point for a static initializer in the dtrace file. */
  public void clinitExit(@GuardSatisfied DTraceWriter this, String pptname, int nonceVal) {
    // don't print
    if (Runtime.dtrace_closed) {
      return;
    }
    outFile.println(pptname);
    printNonce(nonceVal);
    outFile.println();
    Runtime.incrementRecords();
  }

  // prints an invocation nonce entry in the dtrace
  private void printNonce(@GuardSatisfied DTraceWriter this, int val) {
    outFile.println("this_invocation_nonce");
    outFile.println(val);
  }

  /**
   * Prints the method's return value and all relevant variables. Uses the tree of
   * DaikonVariableInfo objects.
   *
   * @param mi the method whose program point we are printing
   * @param root the root of the program point's tree
   * @param args the arguments to the method corrsponding to mi. Must be in the same order as the
   *     .decls info is in (which is the declared order in the source code).
   * @param thisObj the value of the "this" object at this point in the execution
   * @param ret_val the value returned from this method, only used for exit program points
   */
  private void traverse(
      @GuardSatisfied DTraceWriter this,
      MethodInfo mi,
      RootInfo root,
      Object[] args,
      Object thisObj,
      Object ret_val) {
    // go through all of the node's children
    for (DaikonVariableInfo child : root) {

      Object val;

      if (child instanceof ReturnInfo) {
        val = ret_val;
      } else if (child instanceof ThisObjInfo) {
        val = thisObj;
      } else if (child instanceof ParameterInfo) {
        val = args[((ParameterInfo) child).getArgNum()];
      } else if (child instanceof FieldInfo) {
        // can only occur for static fields
        // non-static fields will appear as children of "this"

        val = child.getMyValFromParentVal(null);
      } else if (child instanceof StaticObjInfo) {
        val = null;
      } else {
        throw new Error(
            "Unknown DaikonVariableInfo subtype "
                + child.getClass()
                + " in traversePattern in DTraceWriter for info named "
                + child.getName()
                + " in class "
                + "for method "
                + mi);
      }

      traverseValue(mi, child, val);
    }
  }

  // traverse from the traversal pattern data structure and recurse
  private void traverseValue(
      @GuardSatisfied DTraceWriter this, MethodInfo mi, DaikonVariableInfo curInfo, Object val) {

    if (curInfo.dTraceShouldPrint()) {
      if (!(curInfo instanceof StaticObjInfo)) {
        outFile.println(curInfo.getName());
        outFile.println(curInfo.getDTraceValueString(val));
      }

      if (debug_vars) {
        String out = curInfo.getDTraceValueString(val);
        if (out.length() > 20) out = out.substring(0, 20);
        System.out.printf(
            "  --variable %s [%d]= %s%n", curInfo.getName(), curInfo.children.size(), out);
      }
    }

    // go through all of the current node's children
    // and recurse on their values
    if (curInfo.dTraceShouldPrintChildren()) {
      for (DaikonVariableInfo child : curInfo) {
        Object childVal = child.getMyValFromParentVal(val);
        traverseValue(mi, child, childVal);
      }
    }
  }

  /**
   * Returns a list of values of the field for each Object in theObjects.
   *
   * @param theObjects list of Objects, each must have the Field field
   * @param field which field of theObjects we are probing
   */
  public static List<Object> getFieldValues(Field field, List<Object> theObjects) {
    if (theObjects == null || theObjects instanceof NonsensicalList) {
      return nonsenseList;
    }

    List<Object> fieldVals = new ArrayList<>();

    for (Object theObj : theObjects) {
      if (theObj == null) {
        fieldVals.add(nonsenseValue);
      } else {
        fieldVals.add(getValue(field, theObj));
      }
    }

    return fieldVals;
  }

  /**
   * Get the value of a certain field in theObj.
   *
   * @param classField which field we are interested in
   * @param theObj the object whose field we are examining. TheoObj must be null, Nonsensical, or of
   *     a type which contains the field classField.
   * @return the value of the classField field in theObj
   */
  @SuppressWarnings("deprecation") // in Java 9+, use canAccess instead of isAccessible
  public static Object getValue(Field classField, Object theObj) {
    // if we don't have a real object, return NonsensicalValue
    if ((theObj == null) || (theObj instanceof NonsensicalObject)) {
      return nonsenseValue;
    }

    Class<?> fieldType = classField.getType();

    if (!classField.isAccessible()) {
      classField.setAccessible(true);
    }

    try {
      if (fieldType.equals(int.class)) {
        return new Runtime.IntWrap(classField.getInt(theObj));
      } else if (fieldType.equals(long.class)) {
        return new Runtime.LongWrap(classField.getLong(theObj));
      } else if (fieldType.equals(boolean.class)) {
        return new Runtime.BooleanWrap(classField.getBoolean(theObj));
      } else if (fieldType.equals(float.class)) {
        return new Runtime.FloatWrap(classField.getFloat(theObj));
      } else if (fieldType.equals(byte.class)) {
        return new Runtime.ByteWrap(classField.getByte(theObj));
      } else if (fieldType.equals(char.class)) {
        return new Runtime.CharWrap(classField.getChar(theObj));
      } else if (fieldType.equals(short.class)) {
        return new Runtime.ShortWrap(classField.getShort(theObj));
      } else if (fieldType.equals(double.class)) {
        return new Runtime.DoubleWrap(classField.getDouble(theObj));
      } else {
        return classField.get(theObj);
      }
    } catch (IllegalArgumentException e) {
      throw new Error(e);
    } catch (IllegalAccessException e) {
      throw new Error(e);
    }
  }

  /**
   * Similar to {@link DTraceWriter#getValue}, but used for static fields.
   *
   * @param classField the field whose static value to return
   * @return the static value of the field
   */
  @SuppressWarnings("deprecation") // in Java 9+, use canAccess instead of isAccessible
  public static Object getStaticValue(Field classField) {
    if (!classField.isAccessible()) {
      classField.setAccessible(true);
    }

    Class<?> fieldType = classField.getType();

    if (Chicory.checkStaticInit) {
      // don't force initialization!
      if (!Runtime.isInitialized(classField.getDeclaringClass().getName())) {
        return nonsenseValue;
      }
    }

    try {
      if (fieldType.equals(int.class)) {
        return new Runtime.IntWrap(classField.getInt(null));
      } else if (fieldType.equals(long.class)) {
        return new Runtime.LongWrap(classField.getLong(null));
      } else if (fieldType.equals(boolean.class)) {
        return new Runtime.BooleanWrap(classField.getBoolean(null));
      } else if (fieldType.equals(float.class)) {
        return new Runtime.FloatWrap(classField.getFloat(null));
      } else if (fieldType.equals(byte.class)) {
        return new Runtime.ByteWrap(classField.getByte(null));
      } else if (fieldType.equals(char.class)) {
        return new Runtime.CharWrap(classField.getChar(null));
      } else if (fieldType.equals(short.class)) {
        return new Runtime.ShortWrap(classField.getShort(null));
      } else if (fieldType.equals(double.class)) {
        return new Runtime.DoubleWrap(classField.getDouble(null));
      } else {
        return classField.get(null);
      }
    } catch (IllegalArgumentException e) {
      throw new Error(e);
    } catch (IllegalAccessException e) {
      throw new Error(e);
    }
  }

  /**
   * Return a List derived from an aray.
   *
   * @param arrayVal must be an array type
   * @return a List (with correct primitive wrappers) corresponding to the array
   */
  public static List<Object> getListFromArray(Object arrayVal) {
    if (arrayVal instanceof NonsensicalObject) {
      return nonsenseList;
    }

    if (!arrayVal.getClass().isArray()) {
      throw new RuntimeException(
          String.format(
              "The object \"%s\" of type %s is not an array", arrayVal, arrayVal.getClass()));
    }

    int len = Array.getLength(arrayVal);
    List<Object> arrList = new ArrayList<>(len);

    Class<?> arrType = arrayVal.getClass().getComponentType();

    // have to wrap primitives in our wrappers
    // otherwise, couldn't distinguish from a wrapped object in the
    // target app
    if (arrType.equals(int.class)) {
      for (int i = 0; i < len; i++) {
        arrList.add(new Runtime.IntWrap(Array.getInt(arrayVal, i)));
      }
    } else if (arrType.equals(long.class)) {
      for (int i = 0; i < len; i++) {
        arrList.add(new Runtime.LongWrap(Array.getLong(arrayVal, i)));
      }
    } else if (arrType.equals(boolean.class)) {
      for (int i = 0; i < len; i++) {
        arrList.add(new Runtime.BooleanWrap(Array.getBoolean(arrayVal, i)));
      }
    } else if (arrType.equals(float.class)) {
      for (int i = 0; i < len; i++) {
        arrList.add(new Runtime.FloatWrap(Array.getFloat(arrayVal, i)));
      }
    } else if (arrType.equals(byte.class)) {
      for (int i = 0; i < len; i++) {
        arrList.add(new Runtime.ByteWrap(Array.getByte(arrayVal, i)));
      }
    } else if (arrType.equals(char.class)) {
      for (int i = 0; i < len; i++) {
        arrList.add(new Runtime.CharWrap(Array.getChar(arrayVal, i)));
      }
    } else if (arrType.equals(short.class)) {
      for (int i = 0; i < len; i++) {
        arrList.add(new Runtime.ShortWrap(Array.getShort(arrayVal, i)));
      }
    } else if (arrType.equals(double.class)) {
      for (int i = 0; i < len; i++) {
        arrList.add(new Runtime.DoubleWrap(Array.getDouble(arrayVal, i)));
      }
    } else {
      for (int i = 0; i < len; i++) {
        // non-primitives
        arrList.add(Array.get(arrayVal, i));
      }
    }

    return arrList;
  }

  /**
   * Returns a list of Strings which are the names of the runtime types in the theVals param.
   *
   * @param theVals list of ObjectReferences
   * @return a list of Strings which are the names of the runtime types in the theVals param
   */
  public static @Nullable List<String> getTypeNameList(List<Object> theVals) {
    // Return null rather than NonsensicalList as NonsensicalList is
    // an array of Object and not String.
    if (theVals == null || theVals instanceof NonsensicalList) {
      return null;
    }

    List<String> typeNames = new ArrayList<>(theVals.size());

    for (Object ref : theVals) {
      if (ref == null) {
        typeNames.add(null);
      } else {
        Class<?> type = ref.getClass();
        type = removeWrappers(ref, type, true);
        typeNames.add(type.getCanonicalName());
      }
    }

    return typeNames;
  }

  /**
   * Get the type of val, removing any PrimitiveWrapper if it exists For example, if we execute
   * removeWRappers(val, boolean.class, true) where (val instanceof Runtime.PrimitiveWrapper), then
   * the method returns boolean.class
   *
   * @param val the object whose type we are examining
   * @param declared the declared type of the variable corresponding to val
   * @param runtime should we use the runtime type or declared type?
   * @return the variable's type, with primitive wrappers removed, or null if the value is non-null
   */
  public static @Nullable Class<?> removeWrappers(Object val, Class<?> declared, boolean runtime) {
    if (!runtime) {
      return declared;
    }

    if (val instanceof Runtime.PrimitiveWrapper) {
      return ((Runtime.PrimitiveWrapper) val).primitiveClass();
    }

    if (val == null) {
      return null;
    }

    return val.getClass();
  }
}
