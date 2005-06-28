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
public class DaikonWriter
{

    //include/exclude patterns
	
	/** Used to filter instrumentation **/
    private String[] excludeStrings;
	
	/** Only instrument whats in this list **/
    private List<String> includeOnly;

    /**
     * Controls whether modifiers and the return type are included in the decl output
     */
    protected static final boolean no_modifiers_ppt = true;

    /** the class of the method currently being processed **/
    protected Class current_class;
    
    /** Print visibility info in the program point **/
    private static final boolean showDeclVisibility = false;
	
	/** Platform dependent line separator.  Should be "\n" on Unix **/
	public static final String lineSep;
	
	static
	{
		lineSep = System.getProperty("line.separator");
		assert lineSep != null : "Line separator cannot be null";
	}

    public DaikonWriter(String[] excludes, List <String> includes)
    {
        if (excludes == null)
            throw new RuntimeException("Excludes may not be null");
        excludeStrings = excludes;

        if (includes == null || includes.isEmpty())
            includeOnly = null;
        else
            includeOnly = includes;
    }

    /**
     * Given a method, returns the decorated method entry name for Daikon
     * @param method non-null method
     * @return the decorated method entry name for Daikon
     */
    public static String methodEntryName(Member method)
    {       
        //System.out.printf("(NORM)  %s ----  %s\n", method.toString(), method.getName());
        return methodName(method, "ENTER");
    }
    
	/**
	 * Given information about a method, returns the decorated 
	 * method entry name for Daikon
	 * 
	 * @param types Argument types
	 * @return the decorated method entry name for Daikon
	 */
    public static String methodEntryName(String fullClassName, String[] types, String name, String short_name, boolean isConstructor)
    {
        //System.out.printf("(bytecodes)  %s ----  %s\n", name, short_name);
        return methodName(fullClassName, types, name, short_name, isConstructor, "ENTER");
    }

    
    /**
     * Determines if this field warrants an [ = val ] entry in decls file
     *
     * @param field - requires field != null
     * @return true iff field warrants an [ = val ] entry in the decls files
     */
    protected boolean staticConstField(Field field)
    {
        Class type = field.getType();
        int mod = field.getModifiers();

        return Modifier.isFinal(mod) && Modifier.isStatic(mod) && type.isPrimitive();
    }

    /**
     * Given a method, returns the decorated method exit name for Daikon
     * @param method non-null method
     * @param lineNum The line number of the exit point of the method
     * @return the decorated method exit name for Daikon
     */
    public static String methodExitName(Member method, int lineNum)
    {
        return methodName(method, "EXIT" + lineNum);
    }
    
	/**
	 * Given information about a method, returns the decorated 
	 * method exit name for Daikon
	 * 
	 * @param types Argument types
	 * @param lineNum The line number of the exit point of the method
	 * @return the decorated method entry name for Daikon
	 */
    public static String methodExitName(String fullClassName, String[] types, String name, String short_name, boolean isConstructor, int lineNum)
    {
        return methodName(fullClassName, types, name, short_name, isConstructor, "EXIT" + lineNum);
    }

    
    /**
     * Prints the decorated method name and appends the point string
     * 
     * @param name What member.toString() returns
     * @param short_name What member.name() returns
     * @param isConstructor Is the method a constructor
     * @param point Usually "EXIT" or "ENTER"
     * @param fullClassName packageName.className
     * @return Same thing as methodName(Member, point)
     */
    private static String methodName(String fullClassName, String[] types, String name, String short_name, boolean isConstructor, String point)
    {        
        String className = fullClassName.substring(fullClassName.lastIndexOf('.') + 1);
        name = name.replace("<init>", className);
        short_name = short_name.replace("<init>", className);
        
        //System.out.println("before " + name);
        name = name.replace(short_name + "(", fullClassName + "." + short_name + "(");
        //System.out.println("after " + name);
        
        String ppt = fullClassName + "." + short_name + "(";
        for(int i = 0; i < types.length; i++) 
        {
            ppt += types[i];
            
            if(i != types.length - 1)
                ppt += ", ";
        }
        ppt += ")" + ":::" + point;

        
        if (isConstructor)
        {
            // For some reason dfej repeats the name of the class for
            // constructors (eg, DataStructures.StackAr() becomes
            // DataStructures.StackAr.StackAr().  Mimic that behavior

            int lastPeriod = short_name.lastIndexOf(".");
            // System.out.printf ("short name=%s, name=%s, lastPeriod=%s\n",
              //                 short_name, name, lastPeriod);
            
            
            if (lastPeriod < 0)
            {
                name = name.replace (short_name + "(",
                                 short_name + "." + short_name + "(");
            }
            else
            {
                short_name = short_name.substring(lastPeriod + 1);
                name = name.replace ("." + short_name + "(",
                                 "." + short_name + "." + short_name + "(");
            }
        }

        // System.out.println ("method name = " + name);

        // Remove the modifiers and the type
        if (no_modifiers_ppt)
        {
            name = name.replaceFirst (" throws.*", "");
            String[] parts = name.split ("  *");
            name = parts[parts.length-1];
        }
        name = name.replaceAll (",", ", ");
        
        //TODO fix up this method, make it cleaner
        return ppt;
        //return (name + ":::" + point);
    }
    
    
    /**
     * Prints the decorated method name and appends the point string
     * @param method non-null method
     * @param point The point in the method, usually EXIT or ENTRY
     * @return the decorated method name appended the point string
     */
    private static String methodName(Member method, String point)
    {
        String name = method.toString();
        String short_name = method.getName();

        
        if (method instanceof Constructor)
        {          
            // For some reason dfej repeats the name of the class for
            // constructors (eg, DataStructures.StackAr() becomes
            // DataStructures.StackAr.StackAr().  Mimic that behavior

            int lastPeriod = short_name.lastIndexOf(".");
            // System.out.printf ("short name=%s, name=%s, lastPeriod=%s\n",
              //                 short_name, name, lastPeriod);
            
            
            if (lastPeriod < 0)
            {
                name = name.replace (short_name + "(",
                                 short_name + "." + short_name + "(");
            }
            else
            {
                short_name = short_name.substring(lastPeriod + 1);
                name = name.replace ("." + short_name + "(",
                                 "." + short_name + "." + short_name + "(");
            }
        }

        // System.out.println ("method name = " + name);
        
        // Remove the modifiers and the type
        if (no_modifiers_ppt)
        {
            name = name.replaceFirst (" throws.*", "");
            String[] parts = name.split ("  *");
            name = parts[parts.length-1];
        }
        name = name.replaceAll (",", ", ");
        
        //System.out.println("---> " + name + ":::" + point);
        
        if(showDeclVisibility)
        {
            int mod = method.getModifiers();
            
            if(Modifier.isPublic(mod))
            {
                name = "public " + name;
            }
            else if(Modifier.isPrivate(mod))
            {   
                name = "private " + name;
            }
            else if(Modifier.isPrivate(mod))
            {
                name = "protected " + name;
            }
            else
            {
                throw new RuntimeException("Bad Modifier for visibility for method " + method);
            }
        }
        
        return (name + ":::" + point);
    }

    /**
     * Given a type, gets the representation type to be used in Daikon.
     * For example, the representation type of a class object is "hashcode."
     * 
     * @param type The type of the variable
     * @param inArray Whether the variable is being output as an array
     * (true) or as a pointer (false).
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
        for (int i = 0; i < interfaces.length; i++)
        {
            Class inter = interfaces[i];

            //System.out.println("implements: " + inter.getName());
            if (inter.getName().equals("java.util.List"))
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
        // System.out.printf ("type name is %s\n", class_name);
        if (class_name.startsWith ("java."))
            return (false);
        else
            return (true);
    }

    /**
     * Tells whether given class should be excluded from trace,
     * based on the includeOnly and excludeStrings variables
     *
     * @return true iff we should exclude the given class (by name)
     */
    public boolean shouldFilterClass(String name)
    {
        if (includeOnly != null)
        {
            for (String incString: includeOnly)
            {
                if (Pattern.matches(incString, name))
                    return false;
            }

            return true;
        }

        for (String exString: excludeStrings)
        {
            if (Pattern.matches(exString, name))
            {
                return true;
            }
        }
        return false;
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
            // System.out.printf ("Type [%s] is abstract %Xh %Xh %s\n", type,
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
            // System.out.printf ("comparing %s vs %s\n", ci.class_name,
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
    public Field isImplicitLinkedList(Class type)
    {
        Field linkField = null;
        Field[] fields = type.getFields();

        for (int i = 0; i < fields.length; i++)
        {
            Field field = fields[i];

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
