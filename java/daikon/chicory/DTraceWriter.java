package daikon.chicory;

import java.io.PrintStream;
import java.lang.reflect.*;
import java.util.*;

/**
 * @author Eric Fellheimer
 *         <p>
 *         DTraceListener extends TraceListener. It provides the means to
 *         produce a .dtrace file from the events it receives. It prints the
 *         dtrace file throughout the lifetime of the remote application it is
 *         "listening" to
 */

public class DTraceWriter extends DaikonWriter
{
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

    //turns off multi-dimensional array printing
    private static final boolean NoMultiDim = true;

    private static String noStackErrorMsg = "Could not find stack frame in the thread map";

    //instance of a nonsensical value
    private static NonsensicalObject nonsenseValue = NonsensicalObject.getInstance();
    //instance of a nonsensical list
    private static List nonsenseList = NonsensicalList.getInstance();

    //certain class names
    protected static final String classClassName = "java.lang.Class";
    protected static final String stringClassName = "java.lang.String";

    private PrintStream outFile;
    private int daikonDepth;

    /**
     * Initializes the DeclListener, preparing it to receive messages.
     *
     * @param writer
     *            Stream to write to
     * @param daikonDepth
     *            Tree recursion to traverse for each variable
     * @param excludes
     *            Formatted strings can include wildcards
     * @param thread
     *		  EventThread for the trace interface
     */
    public DTraceWriter(PrintStream writer, int depth, String[] excludes, List includes)
    {
        super(excludes, includes);

        outFile = writer;

        if (depth <= 0)
            throw new Error("Daikon depth must be positive");
        daikonDepth = depth;
    }

    /**
     * Instruments the method entry event in the dtrace file
     */
    public void methodEntry(MethodInfo mi, int nonceVal, Object obj, Object[] args)
    {
        // Thread thread = Thread.currentThread();

        // these can be removed, this is done in transform now
        /*
         if (!shouldInstrumentMethod(method))
         return;

         if (shouldExcludeClass(method.getDeclaringClass().getName()))
         return;
         */

        Member member = mi.member;

        //System.out.println("ENTER method " + method);
        //System.out.println(stackFrame);

        outFile.println(DaikonWriter.methodEntryName(member));
        printNonce(nonceVal);

        // Print args and this.  The nonsenseValue argument indicates
        // that no return value should be printed.  We can't use null
        // since that is a valid return value.
        traceMethod(mi, args, !mi.is_constructor(), obj, nonsenseValue);
        outFile.println();
    }

    /**
     * Instruments the method exit event in the dtrace file
     */
    public void methodExit(MethodInfo mi, int nonceVal, Object obj, Object[] args, Object ret_val, int lineNum)
    {
        // Thread thread = Thread.currentThread();

        Member member = mi.member;

        // these can be removed, this is done in transform now
        /*
         if (!shouldInstrumentMethod(member))
         return;

         if (shouldExcludeClass(method.getDeclaringClass().getName()))
         return;
         */

        // this is passed in
        /*
         int nonceVal = getNonce(method);
         */

        outFile.println(methodExitName(member, lineNum));
        printNonce(nonceVal);

        //always print class vars for method exits
        // pass object as well as argList in here.
        traceMethod(mi, args, true, obj, ret_val);
        outFile.println();
    }

    //prints an invocation nonce entry in the dtrace
    private void printNonce(int val)
    {
        outFile.println("this_invocation_nonce");
        outFile.println(val);
    }

    //prints the method's return value and all relevant variables
    private void traceMethod(MethodInfo mi, Object[] args,
                             boolean shouldPrintClassVars, Object thisObj,
                             Object ret_val)
    {
        // Note the class associated with this method (for visibility checking)
        current_class = mi.class_info.clazz;

        //print everything else!
        traceMethodVars(mi, "", daikonDepth, args, shouldPrintClassVars,
                        thisObj, ret_val);
    }

    //print return values
    private void printReturnValue(MethodInfo mi, Object returnVal)
    {
        Member method = mi.member;

        //print return type information for methods only and not constructors
        if (mi.member instanceof Method)
        {
            Class returnType = ((Method) method).getReturnType();
            if (!(returnType.equals(Void.TYPE)))
            {
                //output return value...
                outFile.println("return");
                printVal(returnVal, true);

                //check for list implementation, runtime class, recursion...
                checkForListTrace(returnType, returnVal, "return", "");
                checkForRuntimeClass(returnType, "return", "", returnVal);
                checkForString(returnType, "return", "", returnVal);

                checkForVarRecursion(returnType, "return", "", daikonDepth, returnVal);
            }
        }
    }

    //print's the method's arguments and class variables
    private void traceMethodVars(MethodInfo mi, String offset, int depth,
                                 Object[] args, boolean shouldPrintClassVars,
                                 Object thisObj, Object ret_val)
    {
        traceLocalVars(mi, args, offset, depth);

        if (!(ret_val instanceof NonsensicalObject))
          printReturnValue(mi, ret_val);

        // System.out.println ("looking at method " + method.name());
        //try {
        //  for (LocalVariable local : method.variables()) {
        //      System.out.println ("local : " + local);
        //  }
        //} catch (Exception e) {
        //  throw new RuntimeException ("local error: " + e);
        // }

        Member method = mi.member;

        if (shouldPrintClassVars)
            traceClassVars(Modifier.isStatic(method.getModifiers()), mi.class_info.clazz, offset, depth, thisObj);
    }

    //prints the method' arguments

    private void traceLocalVars(MethodInfo mi, Object[] args, String offset, int depth)
    {

        for (int i = 0; i < args.length; i++)
        {
            Class type = mi.arg_types[i];
            String name = mi.arg_names[i];
            Object val = args[i];

            traceLocalVar(type, name, offset, val);
            checkForVarRecursion(type, name, offset, depth, val);
        }

    }

    private Object getValue(String name, Object thisObject)
    {
        try
        {
            //TODO will getField work for private fields?
            return getValue(thisObject.getClass().getField(name), thisObject);
        }
        catch (SecurityException e)
        {
            throw new Error(e);
        }
        catch (NoSuchFieldException e)
        {
            throw new RuntimeException(e);
        }
    }

    //prints the class field's under a previously evaluated array of values,
    //passed in the theValues argument

    //theValues: list of ObjectReferences of type ref
    private void traceClassArray(Class ref, String offset, int depth, List /* <ObjectReference> */theValues)
    {
        //Class ref = ci.clazz;

        if (shouldExcludeClass(ref.getName()))
            return;

        Field[] fields = ref.getDeclaredFields();
        for (int i = 0; i < fields.length; i++)
        {
            Field classField = fields[i];

            // No need to print static fields in arrays
            if (Modifier.isStatic(classField.getModifiers()))
                continue;

            // Don't print non-visible fields
            // System.out.printf ("field %s visible = %b\n", classField,
            //                   isFieldVisible (current_class,classField));
            if (!isFieldVisible (current_class, classField))
                continue;

            Class type = classField.getType();

            List fieldValues = getFieldValues(classField, theValues);

            outFile.println(offset + classField.getName());
            showList(fieldValues);
            outFile.println();

            if (fieldValues instanceof NonsensicalList)
                outFile.println("2");
            else
                outFile.println("1");

            checkForRuntimeClass(type, classField.getName(), offset, getTypeNameList(fieldValues));
            checkForString(type, classField.getName(), offset, fieldValues);
            checkForVarRecursionArray(type, classField.getName(), offset, depth, fieldValues);
        }
    }

    //input: List of ObjectReferences whose type has field field
    //output: List of values of field for each object in theObjects
    private List getFieldValues(Field field, List /* <Object> */theObjects)
    {
        if (theObjects == null || theObjects instanceof NonsensicalList)
            return nonsenseList;

        List fieldVals = new ArrayList();

        for (Iterator iter = theObjects.iterator(); iter.hasNext();)
        {
            Object theObj = iter.next();

            if (theObj == null)
                fieldVals.add(nonsenseValue);
            else
                fieldVals.add(getValue(field, theObj));
        }

        return fieldVals;
    }

    /**
     * @param field
     * @param theObj
     * @return
     */
    private Object getValue(Field classField, Object theObj)
    {
        if ((theObj == null) || (theObj instanceof NonsensicalObject))
            return nonsenseValue;

        Class declaringClass = classField.getDeclaringClass();

        if (!classField.isAccessible())
            classField.setAccessible(true);

        if (true)
        {
            try {
                Object val = classField.get (theObj);
                // System.out.printf ("val for %s = %s\n", classField, val);

            } catch (Exception e) {
                System.out.printf ("looking in object of class %s %s\n",
                                   theObj.getClass(), theObj);
                throw new Error ("can't access field " + classField + ": " + e);
            }
        }

        // System.out.printf ("declaring class = %s\n", declaringClass);

        // It seems easier (and it seems to work) to pass the normal
        // Integer, Float, etc wrappes back and let them be handled there
        // rather than wrapping them in our wrappers.
        if (declaringClass.equals(int.class))
        {
            try
            {
                return new Runtime.IntWrap(classField.getInt(theObj));
            }
            catch (IllegalArgumentException e)
            {
                throw new Error(e);
            }
            catch (IllegalAccessException e)
            {
                throw new Error(e);
            }
        }
        else if (declaringClass.equals(long.class))
        {
            try
            {
                return new Runtime.LongWrap(classField.getLong(theObj));
            }
            catch (IllegalArgumentException e)
            {
                throw new Error(e);
            }
            catch (IllegalAccessException e)
            {
                throw new Error(e);
            }
        }
        else if (declaringClass.equals(boolean.class))
        {
            try
            {
                // System.out.printf ("val for %s = %b\n", classField,
                //                   classField.getBoolean(theObj));
                return new Runtime.BooleanWrap(classField.getBoolean(theObj));
            }
            catch (IllegalArgumentException e)
            {
                throw new Error(e);
            }
            catch (IllegalAccessException e)
            {
                throw new Error(e);
            }
        }
        else if (declaringClass.equals(float.class))
        {
            try
            {
                return new Runtime.FloatWrap(classField.getFloat(theObj));
            }
            catch (IllegalArgumentException e)
            {
                throw new Error(e);
            }
            catch (IllegalAccessException e)
            {
                throw new Error(e);
            }
        }
        else if (declaringClass.equals(byte.class))
        {
            try
            {
                return new Runtime.ByteWrap(classField.getByte(theObj));
            }
            catch (IllegalArgumentException e)
            {
                throw new Error(e);
            }
            catch (IllegalAccessException e)
            {
                throw new Error(e);
            }
        }
        else if (declaringClass.equals(char.class))
        {
            try
            {
                return new Runtime.CharWrap(classField.getChar(theObj));
            }
            catch (IllegalArgumentException e)
            {
                throw new Error(e);
            }
            catch (IllegalAccessException e)
            {
                throw new Error(e);
            }
        }
        else if (declaringClass.equals(short.class))
        {
            try
            {
                return new Runtime.ShortWrap(classField.getShort(theObj));
            }
            catch (IllegalArgumentException e)
            {
                throw new Error(e);
            }
            catch (IllegalAccessException e)
            {
                throw new Error(e);
            }
        }
        else if (declaringClass.equals(double.class))
        {
            try
            {
                return new Runtime.DoubleWrap(classField.getDouble(theObj));
            }
            catch (IllegalArgumentException e)
            {
                throw new Error(e);
            }
            catch (IllegalAccessException e)
            {
                throw new Error(e);
            }
        }
        else
        {
            try
            {
                return classField.get(theObj);
            }
            catch (IllegalArgumentException e)
            {
                throw new Error(e);
            }
            catch (IllegalAccessException e)
            {
                throw new Error(e);
            }
        }
    }

    //prints out the class variables values during an event
    private void traceClassVars(boolean dontPrintInst, Class ref, String offset, int depth, Object theObj)
    {
        //Class ref = ci.clazz;
        //must be first level of recursion to print "this" field

        if (!dontPrintInst && offset.equals(""))
        {
            //print the "this" field
            outFile.println("this");
            printVal(theObj, true);

            //.class variable
            if (shouldAddRuntimeClass(ref))
            {
                outFile.println("this.class");
                if (theObj == null || theObj instanceof NonsensicalObject)
                {
                    outFile.println("nonsencial");
                    outFile.println("2");
                }
                else
                {
                    outFile.println("\"" + theObj.getClass().getName() + "\"");
                    outFile.println("1");
                }

            }
        }

        Field[] fields = ref.getDeclaredFields();
        for (int i = 0; i < fields.length; i++)
        {
            Field classField = fields[i];

            if (!Modifier.isStatic(classField.getModifiers()) && dontPrintInst)
                continue;

            // Don't print non-visible fields
            // System.out.printf ("field %s visible = %b\n", classField,
            //                   isFieldVisible (current_class,classField));
            if (!isFieldVisible (current_class, classField))
                continue;

            //static const fields:
            //don't need to print value in dtrace file (already put in decls
            // file)
            if (staticConstField(classField))
                continue;

            Class type = classField.getType();

            Object val;
            if (Modifier.isStatic(classField.getModifiers()))
            {
                val = getStaticValue(classField.getDeclaringClass(), classField);
            }
            else
            {
                val = getValue(classField, theObj);
            }

            String newOffset = traceClassVar(classField, offset, val);
            checkForVarRecursion(type, classField.getName(), newOffset, depth, val);
        }

    }

    /**
     * @param declaringClass
     * @param classField
     * @return
     */
    private Object getStaticValue(Class declaringClass, Field classField)
    {
        //Class declaringClass = declareInfo.clazz;

        if (!classField.isAccessible())
            classField.setAccessible(true);

        if (declaringClass.equals(int.class))
        {
            try
            {
                return new Runtime.IntWrap(classField.getInt(null));
            }
            catch (IllegalArgumentException e)
            {
                throw new Error(e);
            }
            catch (IllegalAccessException e)
            {
                throw new Error(e);
            }
        }
        else if (declaringClass.equals(long.class))
        {
            try
            {
                return new Runtime.LongWrap(classField.getLong(null));
            }
            catch (IllegalArgumentException e)
            {
                throw new Error(e);
            }
            catch (IllegalAccessException e)
            {
                throw new Error(e);
            }
        }
        else if (declaringClass.equals(boolean.class))
        {
            try
            {
                return new Runtime.BooleanWrap(classField.getBoolean(null));
            }
            catch (IllegalArgumentException e)
            {
                throw new Error(e);
            }
            catch (IllegalAccessException e)
            {
                throw new Error(e);
            }
        }
        else if (declaringClass.equals(float.class))
        {
            try
            {
                return new Runtime.FloatWrap(classField.getFloat(null));
            }
            catch (IllegalArgumentException e)
            {
                throw new Error(e);
            }
            catch (IllegalAccessException e)
            {
                throw new Error(e);
            }
        }
        else if (declaringClass.equals(byte.class))
        {
            try
            {
                return new Runtime.ByteWrap(classField.getByte(null));
            }
            catch (IllegalArgumentException e)
            {
                throw new Error(e);
            }
            catch (IllegalAccessException e)
            {
                throw new Error(e);
            }
        }
        else if (declaringClass.equals(char.class))
        {
            try
            {
                return new Runtime.CharWrap(classField.getChar(null));
            }
            catch (IllegalArgumentException e)
            {
                throw new Error(e);
            }
            catch (IllegalAccessException e)
            {
                throw new Error(e);
            }
        }
        else if (declaringClass.equals(short.class))
        {
            try
            {
                return new Runtime.ShortWrap(classField.getShort(null));
            }
            catch (IllegalArgumentException e)
            {
                throw new Error(e);
            }
            catch (IllegalAccessException e)
            {
                throw new Error(e);
            }
        }
        else if (declaringClass.equals(double.class))
        {
            try
            {
                return new Runtime.DoubleWrap(classField.getDouble(null));
            }
            catch (IllegalArgumentException e)
            {
                throw new Error(e);
            }
            catch (IllegalAccessException e)
            {
                throw new Error(e);
            }
        }
        else
        {
            try
            {
                return classField.get(null);
            }
            catch (IllegalArgumentException e)
            {
                throw new Error(e);
            }
            catch (IllegalAccessException e)
            {
                throw new Error(e);
            }
        }
    }

    //prints a local variable's value after it prints its name
    private void traceLocalVar(Class type, String name, String offset, Object val)
    {
        //Class type = ciType.clazz;

        //String localName = type.getName();
        String localName = name;

        outFile.println(offset + localName);
        // System.out.printf ("printing value %s for name %s\n", val,
        //                   offset + localName);
        printVal(val, true);

        checkForListTrace(type, val, localName, offset);
        checkForRuntimeClass(type, localName, offset, val); //.class var

        checkForString(type, localName, offset, val); //.class var

    }

    //checks if type implements list
    //and prints correct dtrace info if it does
    private void checkForListTrace(Class type, Object val, String name, String offset)
    {
        //Class type = ciType.clazz;

        //no "multi-dim" arrays
        if (offset.contains("[]") || name.contains("[]"))
            return;

        if (implementsList((Class) type))
        {
            Object objRef = val;
            Class theClass = (Class) type;

            Method arrayMethod = null;
            try
            {
                arrayMethod = theClass.getMethod("toArray", new Class[0]);
            }
            catch (NoSuchMethodException e)
            {
                throw new Error(theClass.getName() + " seems to implement java.util.List, but method toArray() not found");
            }

            Object arrayVal = null;

            if (objRef != null && !(objRef instanceof NonsensicalObject))
            {

                try
                {
                    arrayVal = arrayMethod.invoke(objRef, new Object[0]);
                }
                catch (IllegalArgumentException e1)
                {
                    throw new Error(e1);
                }
                catch (IllegalAccessException e1)
                {
                    throw new Error(e1);
                }
                catch (InvocationTargetException e1)
                {
                    throw new Error(e1);
                }
            }
            else
                arrayVal = nonsenseValue;

            outFile.println(offset + name + "[]");
            printVal(arrayVal); //should print array of hashcodes

            //.class var
            outFile.println(offset + name + "[].class");

            if (arrayVal != null && !(arrayVal instanceof NonsensicalObject))
            {
                List theVals = getListFromArray(arrayVal);
                printStringList(getTypeNameList(theVals));
            }
            else
                printStringList(nonsenseList);
        }
    }

    /**
     * @param arrayVal
     * @return
     */
    private static List getListFromArray(Object arrayVal)
    {
        if (!arrayVal.getClass().isArray())
            throw new RuntimeException("The object --- " + arrayVal + " --- is not an array");

        int len = Array.getLength(arrayVal);
        List arrList = new ArrayList(len);

        Class arrType = arrayVal.getClass().getComponentType();

        for (int i = 0; i < len; i++)
        {
            if (arrType.equals(int.class))
            {
                arrList.add(new Runtime.IntWrap(Array.getInt(arrayVal, i)));
            }
            else if (arrType.equals(long.class))
            {
                arrList.add(new Runtime.LongWrap(Array.getLong(arrayVal, i)));
            }
            else if (arrType.equals(boolean.class))
            {
                arrList.add(new Runtime.BooleanWrap(Array.getBoolean(arrayVal, i)));
            }
            else if (arrType.equals(float.class))
            {
                arrList.add(new Runtime.FloatWrap(Array.getFloat(arrayVal, i)));
            }
            else if (arrType.equals(byte.class))
            {
                arrList.add(new Runtime.ByteWrap(Array.getByte(arrayVal, i)));
            }
            else if (arrType.equals(char.class))
            {
                arrList.add(new Runtime.CharWrap(Array.getChar(arrayVal, i)));
            }
            else if (arrType.equals(short.class))
            {
                arrList.add(new Runtime.ShortWrap(Array.getShort(arrayVal, i)));
            }
            else if (arrType.equals(double.class))
            {
                arrList.add(new Runtime.DoubleWrap(Array.getDouble(arrayVal, i)));
            }
            else
            {
                //non-primitives
                arrList.add(Array.get(arrayVal, i));
            }
        }

        return arrList;
    }

    //prints the value and the associated changed integer
    //if val == null prints nonsensical
    private void printVal(Object theValue, boolean hashArray)
    {
        showValueEndLine(theValue, hashArray);

        if (theValue instanceof NonsensicalObject)
            outFile.println("2");
        else
            outFile.println("1");
    }

    //print a value, followed by an endline
    //(showValue does not print an endline after it completes)
    private void showValueEndLine(Object val, boolean hashArr)
    {
        showValue(val, hashArr);
        outFile.println();
    }

    //prints the value
    //doesn't print endline
    //if hashArray is true, it prints the "hash value" of the array
    //and not its separate values
    private void showValue(Object theValue, boolean hashArray)
    {

        if (theValue == null)
        {
            outFile.print("null");
            return;
        }

        Class type = theValue.getClass();

        if (type.isPrimitive())
        {
            throw new RuntimeException("Objects cannot be primitive");
        }
        else if (theValue instanceof Runtime.PrimitiveWrapper)
        {
            showPrimitive(theValue);
        }
        else if (!hashArray && (type.isArray()))
        {
            showArray(theValue);
        }

        else if (theValue instanceof NonsensicalObject)
        {
            outFile.print("nonsensical");
        }
        else if (theValue instanceof Number)
        {
            outFile.print (theValue.toString());
        }
        else if (theValue instanceof Boolean)
        {
            outFile.print (theValue.toString());
        }
        else
        {
            showObject(theValue);
        }
    }

    //print values for primitive (wrapped) objects
    private void showPrimitive(Object obj)
    {
        if (!(obj instanceof Runtime.PrimitiveWrapper))
            throw new RuntimeException("Objects passed to showPrimitive must implement PrimitiveWrapper\nThis object is type: " + obj.getClass().getName());

        //use wrapper classes toString methods to print value
        outFile.print(obj);
    }

    //prints out a list of values as if it were an array
    private void showList(List /* <Object>*/theValues)
    {
        if (theValues == null)
        {
            outFile.print("null");
            return;
        }

        if (theValues instanceof NonsensicalList)
        {
            outFile.print("nonsensical");
            return;
        }

        outFile.print("[");
        for (Iterator iter = theValues.iterator(); iter.hasNext();)
        {
            Object elementVal = iter.next();
            showValue(elementVal, true);

            //put space between elements in array
            if (iter.hasNext())
                outFile.print(" ");
        }
        outFile.print("]");
    }

    //prints out a list of Strings as if it were an array
    private void showStringList(List /* <String> */theValues)
    {
        if (theValues == null)
        {
            outFile.print("null");
            return;
        }

        if (theValues instanceof NonsensicalList)
        {
            outFile.print("nonsensical");
            return;
        }

        outFile.print("[");
        for (Iterator iter = theValues.iterator(); iter.hasNext();)
        {
            String str = (String) iter.next();

            if (str == null)
                outFile.print(str);
            else
                outFile.print("\"" + encodeString(str) + "\"");

            //put space between elements in array
            if (iter.hasNext())
                outFile.print(" ");
        }
        outFile.print("]");
    }

    //prints theValues list and trace info
    //and nonsensical if list is null
    private void printList(List /* <Value> */theValues)
    {
        showList(theValues);
        outFile.println();

        if (theValues instanceof NonsensicalList)
            outFile.println("2");
        else
            outFile.println("1");
    }

    //prints theValues list and trace info
    //and nonsensical if list is null
    private void printStringList(List /* <String> */theValues)
    {
        showStringList(theValues);
        outFile.println();

        if (theValues instanceof NonsensicalList)
            outFile.println("2");
        else
            outFile.println("1");
    }

    private void printVal(Object theValue)
    {
        printVal(theValue, false);
    }

    //prints the Object's unique ID
    //in other words, a "hash code"
    private void showObject(Object theObject)
    {
        if (theObject == null)
            outFile.print("null");
        else if (theObject instanceof NonsensicalObject)
            outFile.print("nonsensical");
        else
            outFile.print(System.identityHashCode(theObject));
    }

    //encloses the string value in quotes
    private void showString(String stringRef)
    {
        outFile.print("\"" + encodeString(stringRef) + "\"");
    }

    //removes endlines in string
    private static String encodeString(String input)
    {
        return Runtime.quote(input);
    }

    //shows the values in an array
    private void showArray(Object array)
    {
        List theList = getListFromArray(array);
        showList(theList);
    }

    //prints a class field's value and variable type in the trace file
    private String traceClassVar(Field field, String offset, Object val)
    {
        String fieldName = field.getName();
        if (Modifier.isStatic(field.getModifiers()))
        {
            offset = offset + field.getDeclaringClass().getName() + ".";
            outFile.print(offset + field.getName());
        }
        else if (offset.length() > 0)
        {
            outFile.print(offset + fieldName);
        }
        else
        {
            offset = "this.";
            outFile.print(offset + fieldName);
        }

        outFile.println();
        printVal(val, true);

        checkForListTrace(field.getType(), val, fieldName, offset);
        checkForRuntimeClass(field.getType(), fieldName, offset, val); //.class var

        checkForString(field.getType(), fieldName, offset, val);

        return offset;
    }

    //prints nonsensical and corresponding modified integer
    private void printNonsensical()
    {
        outFile.println("nonsensical");
        outFile.println("2");
    }

    //checks for recursion on a class
    //when we already are in the context of an array
    private void checkForVarRecursionArray(Class type, String name, String offset, int depth, List theVals)
    {
        if (depth <= 0)
        {
            //don't recurse any more!
            return;
        }

        if (type.isArray())
        {
            //don't print multi-dim arrays!
            if (NoMultiDim)
                return;
        }
        else
        {
            traceClassArray(type, offset + name + ".", depth - 1, theVals);
        }
    }

    //checks the given type to see if it requires a .class
    //addition to the dtrace file
    //if so, it prints out the correct .class variable info
    private void checkForRuntimeClass(Class type, String name, String offset,
                                      Object val)
    {
        //Class type = ciType.clazz;

        type = removeWrappers(val, type, false);

        if (!shouldAddRuntimeClass(type))
        {
            return;
        }

        outFile.println(offset + name + ".class");

        //if the object is null or nonsensical, its class is surely nonsensical
        if (val == null || val instanceof NonsensicalObject)
        {
            outFile.println("nonsensical");
            outFile.println("2");
        }
        else
        {
            // JHP 2/6/05: The removeWrappers call does not appear to be
            // necessary. Also getCanonicalName() returns null in some
            // circumstances, which is incorrect.
            //if (name.equals ("return"))
            //    System.out.printf ("%s.class of %s [%s]\n", name,
            //             val.getClass().getSimpleName(),
            //             removeWrappers(val, type, true));
            // outFile.println("\"" + removeWrappers(val, type, true).getCanonicalName() + "\"");
            outFile.println ("\"" + stdClassName (val.getClass()) + "\"");
            outFile.println("1");
        }
    }

    //adds the ".toString" string to the end of the string and outputs its value
    private void checkForString(Class type, String name, String offset, Object val)
    {
        //Class type = ciType.clazz;

        if (!type.getName().equals(stringClassName))
            return;

        outFile.println(offset + name + ".toString");

        if (val == null)
            outFile.println("null");
        else if (val instanceof NonsensicalObject)
            outFile.println("nonsensical");
        else
        {
            showString((String) val);
            outFile.println();
        }

        if (val instanceof NonsensicalObject)
            outFile.println("2");
        else
            outFile.println("1");
    }

    private void checkForString(Class type, String name, String offset, List /*<StringReference*/theValues)
    {
        if (!type.getName().equals(stringClassName))
            return;

        outFile.println(offset + name + ".toString");

        if (theValues == null)
        {
            outFile.print("null");
        }
        else if (theValues instanceof NonsensicalList)
        {
            outFile.print("nonsensical");
            // return;
        }
        else
        {

            outFile.print("[");
            for (Iterator iter = theValues.iterator(); iter.hasNext();)
            {
                Object obj = iter.next();

                if (obj == null)
                    outFile.print("null");
                else if (obj instanceof NonsensicalObject)
                    outFile.print("nonsensical");
                else
                    outFile.print("\"" + ((String) obj) + "\"");

                //put space between elements in array
                if (iter.hasNext())
                    outFile.print(" ");
            }
            outFile.print("]");
        }
        outFile.println();

        if (theValues instanceof NonsensicalList)
            outFile.println("2");
        else
            outFile.println("1");
    }

    private void checkForRuntimeClass(Class type, String name, String offset, List /*<String>*/classNames)
    {
        if (!shouldAddRuntimeClass(type))
            return;

        outFile.println(offset + name + ".class");
        printStringList(classNames);
    }

    //the normal check for recursion during a trace
    private void checkForVarRecursion(Class type, String name, String offset, int depth, Object val)
    {
        //Class type = ciType.clazz;

        if (depth == 0 && !(type.isArray()))
        {
            //don't recurse any more!
            return;
        }

        if (type.isArray())
        {
            //don't need to check depth because
            //won't go into multi-dim arrays
            //if (depth == 0)
            //return;

            Class arrayType = type.getComponentType();

            //array of class objects

            //array of primitives
            if (arrayType.isPrimitive())
            {
                outFile.println(offset + name + "[]");
                printVal(val, false); //print the entire array
            }
            //multi-dimensional arrays!
            else if (arrayType.isArray())
            {
                outFile.println(offset + name + "[]");
                printVal(val, false); //print first dimension of array

                //don't print multi-dim array info
                if (NoMultiDim)
                    return;
            }
            else
            //regular "class" type in array
            {
                Class theType = (Class) arrayType;

                outFile.println(offset + name + "[]");
                printVal(val, false); //print out object hashCodes of the array
                // elements

                if (val != null && !(val instanceof NonsensicalObject))
                {

                    List theVals = getListFromArray(val);
                    // print out the class of each element in the array.  For
                    // some reason dfej doesn't include this on returned arrays
                    // or arguments.  We attempt to get at arguments because
                    // they don't have an offset (no this pointer)
                    if (!name.equals ("return") && !offset.equals (""))
                    {
                        checkForRuntimeClass(type, name + "[]", offset,
                                             getTypeNameList(theVals));
                    }
                    checkForString(arrayType, name + "[]", offset, theVals);
                    traceClassArray((Class) arrayType, offset + name + "[].", depth - 1, theVals);
                }
                else
                {

                    if (!name.equals ("return") && !offset.equals (""))
                      checkForRuntimeClass(type, name + "[]", offset,
                                           nonsenseList);
                    checkForString(arrayType, name + "[]", offset, nonsenseList);

                    traceClassArray((Class) arrayType, offset + name + "[].", depth - 1, nonsenseList);
                }

            }
        }
        else if (!type.isPrimitive())
        {
            traceClassVars(false, type, offset + name + ".", depth - 1, val);
        }
    }

    /*
     * Returns a list of Strings which are the names of the runtime types in the
     * theVals param @param theVals List of ObjectReferences @return
     */
    private List getTypeNameList(List theVals)
    {
        if (theVals == null || theVals instanceof NonsensicalList)
            return nonsenseList;

        List typeNames = new ArrayList(theVals.size());

        for (Iterator iter = theVals.iterator(); iter.hasNext();)
        {
            Object ref = iter.next();
            Class type = null;

            if (ref != null)
                type = ref.getClass();

            type = removeWrappers(ref, type, true);

            if (type == null)
                typeNames.add(null);
            else
                typeNames.add(type.getCanonicalName());
        }

        return typeNames;
    }

    /**
     * @param type
     * @return
     */
    private Class removeWrappers(Object val, Class declared, boolean runtime)
    {
        if (!(val instanceof Runtime.PrimitiveWrapper))
        {
            if(!runtime)
                return declared;
            else
            {
                if(val != null)
                    return val.getClass();
                else
                    return null;
            }
        }

        if (val instanceof Runtime.BooleanWrap)
        {
            return boolean.class;
        }
        else if (val instanceof Runtime.IntWrap)
        {
            return int.class;
        }
        else if (val instanceof Runtime.DoubleWrap)
        {
            return double.class;
        }
        else if (val instanceof Runtime.FloatWrap)
        {
            return float.class;
        }
        else if (val instanceof Runtime.LongWrap)
        {
            return long.class;
        }
        else if (val instanceof Runtime.ByteWrap)
        {
            return byte.class;
        }
        else if (val instanceof Runtime.CharWrap)
        {
            return char.class;
        }
        else if (val instanceof Runtime.ShortWrap)
        {
            return short.class;
        }
        else
            throw new RuntimeException("Could not find correct primitive wrapper class for class " + val.getClass());
    }

}
