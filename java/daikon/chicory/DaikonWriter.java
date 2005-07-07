package daikon.chicory;

import java.io.*;
import java.util.*;
import java.util.regex.*;
import java.util.regex.Pattern;
import java.lang.reflect.*;

/**
 * DaikonWriter is the parent class of DeclWriter and DTraceWriter.
 *
 */
public abstract class DaikonWriter
{
    /** Controls whether modifiers and the return type are included in the decl output **/
    protected static final boolean no_modifiers_ppt = true;

    /** Platform dependent line separator.  Should be "\n" on Unix **/
    public static final String lineSep;

    static
    {
        lineSep = System.getProperty("line.separator");
        assert lineSep != null : "Line separator cannot be null";
    }

    public DaikonWriter()
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
        //System.out.printf("(bytecodes)  %s ----  %s%n", name, short_name);
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
     * Constructs the program point name (which includes the point string at the end)
     *
     * @param fullClassName packageName.className
     * @param types String representation of the declared types of the parameters
     *          for example: {"int", "java.lang.Object", "float"}
     * @param name The method with modifiers and parameters
     * @param short_name Just the method's name (except it is "<init>" for constructors)
     *
     * So a corresponding name/short_name pair could be:
     *     name: public static void DataStructures.StackArTester.doNew(int size)
     *     short_name: doNew
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
        
        // replace <init>'s with the actual class name
        // so "public void <init>" becomes "public void StackAr" for example
        name = name.replace("<init>", className);
        short_name = className;

        // build up the string to go inside the parens
        StringBuilder paramTypes = new StringBuilder();
        paramTypes.append("(");
        for(int i = 0; i < types.length; i++)
        {
            paramTypes.append(types[i]);

            if(i != types.length - 1)
                paramTypes.append(", ");
        }
        paramTypes.append(")");
        // Quote dollar signs, which replaceFirst would interpreted as a
        // group reference.
        String paramTypesString = paramTypes.toString().replace("$", "\\$");
        name = name.replaceFirst("\\(.*\\)", paramTypesString);

        return methodName(name, short_name, isConstructor, point);
    }


    /**
     * Constructs the program point name (which includes the point string at the end)
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
     *
     * Constructs the program point name (which includes the point string at the end)
     *
     * @param name Looks like: "public boolean DataStructures.StackAr.push(java.lang.Object) throws Exception"
     * @param short_name Looks like: "push"
     * @param isConstructor
     * @param point Usually "ENTER" or "EXIT"
     */
    private static String methodName(String name, String short_name, boolean isConstructor, String point)
    {
        //System.out.printf("%s ---- %s %n", name, short_name);

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
     * Given a type, gets the representation type to be used in Daikon. For
     * example, the representation type of a class object is "hashcode."
     * 
     * @param type
     *            The type of the variable
     * @param asArray
     *            Whether the variable is being output as an array (true) or as
     *            a pointer (false).
     * @return The representation type as a string
     */
    public static String getRepName(Class type, boolean asArray)
    {
        if (type == null)
        {
            return "hashcode";
        }
        else if (type.isPrimitive())
        {
            if (type.equals(Double.TYPE))
                return "double";
            else if (type.equals(Float.TYPE))
                return "double";
            else if (type.equals (Boolean.TYPE))
                return "boolean";
            else
                return "int";
        }
        else if (type.getName().equals("java.lang.String"))
        {
            // if we are printing the actual array, the rep type is "java.lang.String"
            if (asArray)
                return "java.lang.String";
            // otherwise, it is just a hashcode
            else
                return "hashcode";
        }
        else
        {
            return "hashcode";
        }
    }

    /**
     * Returns the correctly formulated ":::OBJECT" name of the class
     * (ie, the program point name)
     *
     * @param type the ClassType type
     * @return the correctly formulated String
     */
    public static String classObjectName(Class type)
    {
        return (type.getName() + ":::OBJECT");
    }

    /**
     * Returns true iff type implements the List interface
     * @param type
     * @return true iff type implements the List interface
     */
    public static boolean implementsList(Class type)
    {
        //System.out.println(type);
        Class[] interfaces = type.getInterfaces();
        for (Class inter: interfaces)
        {
            //System.out.println("implements: " + inter.getName());
            if (inter.equals(java.util.List.class))
                return true;
        }
        return false;
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
     * Returns whether or not the fields of the specified class
     * should be included, based on whether the Class type
     * is a system class or not.  Right now, any system classes are
     * excluded, but a better way of determining this is probably
     * necessary
     */
    public boolean notSystemClass (Class type)
    {
        String class_name = type.getName();
        // System.out.printf ("type name is %s%n", class_name);
        if (class_name.startsWith ("java."))
            return (false);
        else
            return (true);
    }

    //the following comments are taken from dfej

    // Return true if the object's Class/type should be printed as well as its
    // hashvalue.
    // Outputting the ".class" field for every variable might lead to many
    // uninteresting invariants, such as "myFoo.class == Foo", so this test is
    // intended to reduce the number of variables that are output.
    //
    // This implementation outputs the type of variables that are
    // (heuristically) likely to be used generically (that is, to be used at
    // other than their declared type: Object, abstract classes, interfaces,
    // and arrays of non-primitive elements.
    // Additionally, if the class of variable lst implements List, then
    // lst[].class is also output (without calling this procedure).
    //
    // Another reasonable implementation would be "return true iff this is
    // not a final class".
    // Or "return true iff this class is subclassed in this program"

    /**
     * Determines if type needs a corresponding .class runtime class variable
     *
     * @param type
     *            The variable's Type
     */
    protected static boolean shouldAddRuntimeClass(Class type)
    {
        // For some reason, abstacts seems to be set on arrays
        // and primitives.  This is a temporary fix to get things
        // close.
        if (type.isPrimitive())
            return (false);
        if (type.isArray())
        {
            Class theType = type.getComponentType();
            return !(theType.isPrimitive());
        }

        if (type.getName().equals("java.lang.Object")) //Objects
        {
            // System.out.println ("type is object " + type);
            return true;
        }
        else if (Modifier.isAbstract(type.getModifiers()))
        {
            // System.out.printf ("Type [%s] is abstract %Xh %Xh %s%n", type,
            //                   type.getModifiers(), Modifier.ABSTRACT,
            //                   Modifier.toString (type.getModifiers()));
            return true;
        }
        else if (type.isInterface())
        {
            // System.out.println ("type is interface " + type);
            return true;
        }
        else if (type.isArray()) //arrays of non-primitive types
        {
            // System.out.println ("type is array " + type);
            Class theType = type.getComponentType();
            return !(theType.isPrimitive());
        }
        else
            return false;
    }

    /**
     * Returns whether or not the specified field is visible from the Class
     * current.  All fields within instrumented classes are considered
     * visible from everywhere (to match dfej behavior)
     */
    public boolean isFieldVisible (Class current, Field field)
    {
        Class fclass = field.getDeclaringClass();
        int modifiers = field.getModifiers();

        // If the field is within the current class, it is always visible
        if (current.equals (fclass))
            return (true);

        // If the field is in any instrumented class it is always visible
        synchronized(Runtime.all_classes)
        {
        for (ClassInfo ci : Runtime.all_classes)
        {
            // System.out.printf ("comparing %s vs %s%n", ci.class_name,
            //                    fclass.getName());
            if (ci.class_name.equals (fclass.getName()))
                {
                return (true);
                }
        }
        }

        // Otherwise we consider the variable not to be visible, even
        // though it is.  This mimics dfej behavior
        if (true)
            return (false);

        // If the field is in the same package, it's visible if it is
        // not private or protected
        if (current.getPackage().equals (fclass.getPackage())) {
            if (Modifier.isPrivate (modifiers)
                 || Modifier.isProtected (modifiers))
                return (false);
            else
                return (true);
        }

        // The field must be in an unrelated class, it must be marked
        // public to be visible
        return (Modifier.isPublic (modifiers));
    }

    /**
     * Determines implicit linked list with following rule: if class x contains
     * exactly 1 non-static field which of type x, then it is an implicit list
     *
     * @param type
     *            The ClassType to check for implicit list
     *
     * @return    The field in the class which is the "linked list field",
     *            or null if no such field exists.
     */
    public Field checkImplicitLinkedList(Class type)
    {
        Field linkField = null;

        for (Field field: type.getFields())
        {
            if (!Modifier.isStatic(field.getModifiers()))
            {
                if (field.getType().equals(type))
                {
                    //need exactly 1, not more than 1
                    if (linkField != null)
                        return null;
                    else
                        linkField = field;
                }
            }
        }

        return linkField;
    }

    /**
     * Returns the class name of the specified class in 'java' format
     * (ie, as the class would have been declared in java source code)
     */
    public static String stdClassName (Class type)
    {
        return Runtime.classnameFromJvm (type.getName());
    }

}
