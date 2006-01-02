package daikon.chicory;

import java.lang.reflect.*;

/**
 * DaikonWriter is the parent class of DeclWriter and DTraceWriter.
 *
 */
public abstract class DaikonWriter
{
    /**
     * Controls whether modifiers and the return type are included in
     * the decl output
     **/
    protected static final boolean no_modifiers_ppt = true;

    /** Platform dependent line separator.  Should be "\n" on Unix **/
    public static final String lineSep;

    static
    {
        lineSep = System.getProperty("line.separator");
        assert lineSep != null : "Line separator cannot be null";
    }

    protected DaikonWriter()
    {
    }

    /**
     * Determines if this field warrants an [ = val ] entry in decls file
     *
     * @param field requires field != null
     * @return true iff field warrants an [ = val ] entry in the decls files
     */
    protected static boolean isStaticConstField(Field field)
    {
        Class type = field.getType();
        int mod = field.getModifiers();

        return Modifier.isFinal(mod) && Modifier.isStatic(mod) && type.isPrimitive();
    }

    /**
     * Given a method, returns the method entry program point name for Daikon
     * @param method non-null method
     * @return the decorated method entry name for Daikon
     */
    public static String methodEntryName(Member method)
    {
        //System.out.printf("(NORM)  %s ----  %s%n", method.toString(), method.getName());
        return methodName(method, "ENTER");
    }

    /**
     * Given a method, returns the method entry program point name for Daikon
     * method entry name for Daikon
     *
     * @param types Argument types
     * @return the decorated method entry name for Daikon
     */
    public static String methodEntryName(String fullClassName, String[] types, String name, String short_name)
    {
        // System.out.printf("(bytecodes)  %s ----  %s%n", name, short_name);
        return methodName(fullClassName, types, name, short_name, "ENTER");
    }

    /**
     * Given a method, returns the method exit program point name for Daikon
     * @param method require method != null
     * @param lineNum The line number of the exit point of the method
     * @return the decorated method exit name for Daikon
     */
    public static String methodExitName(Member method, int lineNum)
    {
        return methodName(method, "EXIT" + lineNum);
    }

    /**
     * Given a method, returns the method exit program point name for Daikon
     *
     * @param types Argument types
     * @param lineNum The line number of the exit point of the method
     * @return the decorated method entry name for Daikon
     */
    public static String methodExitName(String fullClassName, String[] types, String name, String short_name, int lineNum)
    {
        return methodName(fullClassName, types, name, short_name, "EXIT" + lineNum);
    }

    /**
     * Constructs the program point name (which includes the point
     * string at the end)
     *
     * @param fullClassName packageName.className
     * @param types String representation of the declared types of the
     *          parameters.  for example: {"int", "java.lang.Object", "float"}
     * @param name The method with modifiers and parameters
     * @param short_name Just the method's name (except it is "<init>"
     * for constructors)
     *
     * So a corresponding name/short_name pair could be:
     *    name: public static void DataStructures.StackArTester.doNew(int size)
     *    short_name: doNew
     *
     * @param point Usually "EXIT" or "ENTER"
     * @return Same thing as methodName(Member, point)
     */
    private static String methodName(String fullClassName, String[] types, String name,
            String short_name, String point)
    {
        //System.out.printf("fullclass: %s !!! name: %s !!! short_name: %s %n", fullClassName, name, short_name);

        String className = fullClassName.substring(fullClassName.lastIndexOf('.') + 1);

        boolean isConstructor = name.equals("<init>") || name.equals("");

        if (isConstructor) {
          // replace <init>'s with the actual class name
          // so "public void <init>" becomes "public void StackAr" for example
          name = name.replace("<init>", className);
          short_name = className;
        }

        // build up the string to go inside the parens
        StringBuilder paramTypes = new StringBuilder();
        paramTypes.append("(");
        for (int i = 0; i < types.length; i++)
        {
            paramTypes.append(types[i]);

            if (i != types.length - 1)
                paramTypes.append(",");
        }
        paramTypes.append(")");
        // Quote dollar signs, which replaceFirst would interpreted as a
        // group reference.
        String paramTypesString = paramTypes.toString().replace("$", "\\$");
        name = name.replaceFirst("\\(.*\\)", paramTypesString);
        // System.out.printf ("params = %s, newname = %s, short_name = %s%n",
        //                   paramTypesString, name, short_name);

        return methodName(name, short_name, isConstructor, point);
    }

    /**
     * Constructs the program point name (which includes the point
     * string at the end)
     *
     * @param method non-null method
     * @param point The point in the method, usually "EXIT" or "ENTRY"
     * @return the program point name which includes the point string
     */
    private static String methodName(Member method, String point)
    {
        String name = method.toString();
        String short_name = method.getName();
        boolean isConstructor = method instanceof Constructor;


        return methodName(name, short_name, isConstructor, point);
    }

    /**
     * Constructs the program point name (which includes the point
     * string at the end)
     *
     * @param name Looks like: "public boolean DataStructures.StackAr.push(java.lang.Object) throws Exception"
     * @param short_name Looks like: "push"
     * @param isConstructor
     * @param point Usually "ENTER" or "EXIT"
     */
    private static String methodName(String name, String short_name, boolean isConstructor, String point)
    {
        // System.out.printf("%s ---- %s %n", name, short_name);

        if (isConstructor)
        {
            name = fixDuplicateConstructorName(name, short_name);
        }

        // Remove the modifiers and the type
        if (no_modifiers_ppt)
        {
            // At this point, name might look something like:
            // public boolean DataStructures.StackAr.push(java.lang.Object) throws Exception

            // Get ride of throws and everything after it (and space right before it)
            name = name.replaceFirst (" throws.*", "");

            // Get rid of modifiers before the method name (public boolean in above example)
            String[] parts = name.split ("  *");
            name = parts[parts.length-1];
        }
        name = name.replaceAll (",", ", ");

        return (name + ":::" + point);
    }

    /**
     *
     * Dfej repeats the name of the class for
     * constructors (eg, DataStructures.StackAr() becomes
     * DataStructures.StackAr.StackAr().  This makes it clear
     * that SomePackage.ClassName.ClassName and SomePackage.ClassName.OtherMethod
     * are in the same class. Mimic that behavior.
     *
     */
    private static String fixDuplicateConstructorName(String name,
            String short_name)
    {
        // assert short_name.lastIndexOf(".") == -1 : "short_name: " + short_name
        //       + " should not contain a period ('.') character. ";

        int lastPeriod = short_name.lastIndexOf(".");

        if (lastPeriod == -1)
        {
            return name.replace(short_name + "(", short_name + "." + short_name
                    + "(");
        }
        else
        {
            // This case could occur for constructor names given as PackageName.ClassName

            short_name = short_name.substring(lastPeriod + 1);
            return name.replace("." + short_name + "(", "." + short_name + "."
                    + short_name + "(");
        }
    }

    /**
     * Determines if the given method should be instrumented
     */
    protected boolean shouldInstrumentMethod(Member method)
    {
        int modifiers = method.getModifiers();
        if (Modifier.isAbstract(modifiers) || Modifier.isNative(modifiers) || method.getName().equals("<clinit>"))
            return false;
        return true;
    }

    /**
     * Returns the class name of the specified class in 'java' format
     * (i.e., as the class would have been declared in java source code)
     */
    public static String stdClassName (Class type)
    {
        return Runtime.classnameFromJvm (type.getName());
    }

}
