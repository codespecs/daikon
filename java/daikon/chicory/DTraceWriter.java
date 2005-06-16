package daikon.chicory;

import java.io.PrintStream;
import java.lang.reflect.*;
import java.util.*;

import daikon.Chicory;

/**
 *  DTraceWriter writes .dtrace program points to an output stream.
 *  It uses the traversal pattern trees created by the {@link DeclWriter}.
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

    /** turns off multi-dimensional array printing*/
    private static final boolean NoMultiDim = true;


    /**instance of a nonsensical value*/
    private static NonsensicalObject nonsenseValue = NonsensicalObject.getInstance();
    /**instance of a nonsensical list*/
    private static List nonsenseList = NonsensicalList.getInstance();

    //certain class names
    protected static final String classClassName = "java.lang.Class";
    protected static final String stringClassName = "java.lang.String";

	/**Where to print output*/
    private PrintStream outFile;
	/**Maximum recursion depth*/
    private int daikonDepth;

    /**
     * Initializes the DeclListener, preparing it to receive messages.
     *
     * @param writer
     *            Stream to write to
     * @param depth
     *            Tree recursion to traverse for each variable
     * @param excludes
     *            Formatted strings can include wildcards
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

        if(Runtime.dtrace_closed)
            return;

        Member member = mi.member;


        RootInfo root = mi.traversalEnter;
        if(root == null)
            throw new RuntimeException("Traversal pattern not initialized at method " + mi.method_name);

        //System.out.println("ENTER method " + method);
        //System.out.println(stackFrame);


        // Print args and this.  The nonsenseValue argument indicates
        // that no return value should be printed.  We can't use null
        // since that is a valid return value.

        outFile.println(DaikonWriter.methodEntryName(member));
        printNonce(nonceVal);
        traversePattern(mi, root, args, obj, nonsenseValue);

        outFile.println();

        Runtime.incrementRecords();
    }

    /**
     * Instruments the method exit event in the dtrace file
     */
    public void methodExit(MethodInfo mi, int nonceVal, Object obj, Object[] args, Object ret_val, int lineNum)
    {
        // Thread thread = Thread.currentThread();

        if(Runtime.dtrace_closed)
            return;

        Member member = mi.member;

        RootInfo root = mi.traversalExit.get(lineNum);
        if(root == null)
            throw new RuntimeException("Traversal pattern not initialized for method " + mi.method_name + " at line " + lineNum);

        //make sure the line number is valid
        //i.e., it is one of the exit locations in the MethodInfo for this method
        //System.out.printf("reached line num " + lineNum + " at method " + mi.method_name + "\n");
        if (mi.exit_locations == null || !mi.exit_locations.contains(lineNum))
        {
            throw new RuntimeException("The line number " + lineNum + " is not found in the MethodInfo for method " + mi.method_name + "\nNo exit locations found in exit_locations set!");
        }

        outFile.println(DaikonWriter.methodExitName(member, lineNum));
        printNonce(nonceVal);
        traversePattern(mi, root, args,  obj, ret_val);


        outFile.println();

        Runtime.incrementRecords();
    }

    //prints an invocation nonce entry in the dtrace
    private void printNonce(int val)
    {
        outFile.println("this_invocation_nonce");
        outFile.println(val);
    }

    /** 
     * prints the method's return value and all relevant variables
     * uses the traversal pattern data structure of DaikonInfo subtypes
     *
    */
    private void traversePattern(MethodInfo mi, RootInfo root,
            Object[] args,
            Object thisObj,
            Object ret_val)
    {
        Object val;
        int whichArg = 0;

		//go through all of the node's children
        for(DaikonInfo child: root)
        {
            if(child instanceof ReturnInfo)
            {
                val = ret_val;
            }
            else if(child instanceof ThisObjInfo)
            {
                val = thisObj;
            }
            else if(child instanceof ArgInfo)
            {
				//keep track of how many args we've reached so far
                val = args[whichArg];
                whichArg++;
            }
            else if(child instanceof HolderInfo)
            {
                val = null;
            }
            else
            {
                throw new RuntimeException("Unknown DaikonInfo subtype " + child.getClass() +
                        " in traversePattern in DTraceWriter for info named " + child.getName());
            }

            traverseValue(mi, child, val);
        }
    }

    //traverse from the traversal pattern data structure and recurse
    private void traverseValue(MethodInfo mi, DaikonInfo curInfo, Object val)
    {
        if (!(curInfo instanceof HolderInfo))
        {
            outFile.println(curInfo.getName());
            outFile.println(curInfo.getValueString(val));
        }

        Object childVal;
        for (DaikonInfo child : curInfo)
        {
            childVal = child.getChildValue(val);
            traverseValue(mi, child, childVal);
        }

    }

    //input: List of ObjectReferences whose type has field field
    //output: List of values of field for each object in theObjects
    public static List getFieldValues(Field field, List /*<Object>*/ theObjects)
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
	 * Get the value of a certain field in theObj.
	 * @param classField which field we are interested in
	 * @param theObj The object whose field we are examining.
	 * TheoObj must be null, Nonsensical, or of a type which 
	 * contains the field classField
	 * @return The value of the classField field in theObj
	 */
    public static Object getValue(Field classField, Object theObj)
    {
        //if we dont have a real object, return NonsensicalValue
        if ((theObj == null) || (theObj instanceof NonsensicalObject))
            return nonsenseValue;

        Class fieldType = classField.getType();

        if (!classField.isAccessible())
            classField.setAccessible(true);

        /*if (true)
        {
            try {
                Object val = classField.get (theObj);
                // System.out.printf ("val for %s = %s\n", classField, val);

            } catch (Exception e) {
                System.out.printf ("looking in object of class %s %s\n",
                                   theObj.getClass(), theObj);
                throw new Error ("can't access field " + classField + ": " + e);
            }
        }*/

        // System.out.printf ("Field %s has type %s\n", classField, fieldType);
        // It seems easier (and it seems to work) to pass the normal
        // Integer, Float, etc wrappes back and let them be handled there
        // rather than wrapping them in our wrappers.
        if (fieldType.equals(int.class))
        {
            try
            {
                Runtime.IntWrap val
                    = new Runtime.IntWrap(classField.getInt(theObj));
                //System.out.printf ("field %s has val %s\n", classField, val);
                return val;
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
        else if (fieldType.equals(long.class))
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
        else if (fieldType.equals(boolean.class))
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
        else if (fieldType.equals(float.class))
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
        else if (fieldType.equals(byte.class))
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
        else if (fieldType.equals(char.class))
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
        else if (fieldType.equals(short.class))
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
        else if (fieldType.equals(double.class))
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


	/**
	 * Similar to getValue, but used for static fields
	 */
    public static Object getStaticValue(Field classField)
    {
        //Class declaringClass = declareInfo.clazz;

        if (!classField.isAccessible())
            classField.setAccessible(true);

        Class fieldType = classField.getType();


        if(Chicory.checkStaticInit)
        {
        //don't force initialization!
        if(!Runtime.isInitialized(classField.getDeclaringClass().getName()))
        {
            //System.out.println("SKIPPING " + classField.getDeclaringClass().getName() + " --- " + classField.getName());
            return nonsenseValue;
        }
        }

        if (fieldType.equals(int.class))
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
        else if (fieldType.equals(long.class))
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
        else if (fieldType.equals(boolean.class))
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
        else if (fieldType.equals(float.class))
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
        else if (fieldType.equals(byte.class))
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
        else if (fieldType.equals(char.class))
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
        else if (fieldType.equals(short.class))
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
        else if (fieldType.equals(double.class))
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

	/**
	 * Return a List derived from an aray
	 * @param arrayVal Must be an array type
	 * @return a List (with correct primitive wrappers) corresponding to the array
	 */
    public static List getListFromArray(Object arrayVal)
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


    //removes endlines in string
    private static String encodeString(String input)
    {
        return Runtime.quote(input);
    }

    //prints nonsensical and corresponding modified integer
    private void printNonsensical()
    {
        outFile.println("nonsensical");
        outFile.println("2");
    }

    /**
     * Returns a list of Strings which are the names of the runtime types in the
     * theVals param
     * @param theVals List of ObjectReferences
     * @return a list of Strings which are the names of the runtime types in the
     * theVals param
     */
    public static List getTypeNameList(List theVals)
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
	 * Get the "actual" type of val
	 * @param val The object whose type we are examining
	 * @param declared the declared type of the variable corresponding to val
	 * @param runtime Should we use the runtime type or declared type?
	 * @return The variable's type, with primitive wrappers removed
	 */
    public static Class removeWrappers(Object val, Class declared, boolean runtime)
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
