package daikon.chicory;

import java.io.*;
import java.util.*;
import java.lang.reflect.*;

public class DeclWriter extends DaikonWriter
{
    // Notes:
    //
    //  - printLocalVars() processes each argument via printDeclVar() and
    //    checkForVarRecursion()
    //    - printDeclVar (4 args): prints decl info for a single argument.
    //      No Recursion
    //      - checkForImplicitList(): prints closure info for Classes with
    //        an field of the same type within them
    //      - checkForListDecl(): Treats classes that implement list as arrays
    //    - checkForVarRecursion(): expands arrays and classes
    //
    //  Class.getName() returns fully specified dot separated names with
    //  JVM array designations (eg, [Ljava.lang.String;)

    //default string for comparability info
    private static final String compareInfoDefault = "22";

    //header string before each new method entry or exit point
    public static final String declareHeader = "DECLARE";

    //used to assert that a given variable is a parameter to a method
    private static final String isParamString = " # isParam=true";

    //stores a mapping from methods to all of the method's exit Locations
    private Map /* <Method, Set <Integer>> */methodToExitLocs;

    //map method to its argument names
    private Map /* <Method, List<String>> */methodToArgNames;

    //keep track of Ppts we've already emitted
    private Set /* <String> */emittedPpts;

    //stream to write to
    private PrintStream outFile;

    //levels of recursion
    protected int daikonDepth;

    //certain class names
    protected static final String classClassName = "java.lang.Class";
    protected static final String stringClassName = "java.lang.String";

    /**
     * Initializes the DeclWriter, preparing it to receive messages.
     *
     * @param writer
     *            Stream to write to
     * @param depth
     *            Depth of tree recursion to traverse for each variable
     * @param excludes
     *            Formatted strings can include wildcards
     * @param includes
     *		Include only list
     */

    public DeclWriter(PrintStream writer, int depth, String[] excludes,
                      List includes)
    {
        super(excludes, includes);

        outFile = writer;
        if (depth <= 0)
            throw new Error("Daikon depth must be positive");
        daikonDepth = depth;

        methodToExitLocs = new HashMap();
        methodToArgNames = new HashMap();
        emittedPpts = new HashSet();
    }

    /**
     * Prints header information to the decls file.  Should be called once
     * before emitting any other declarations.
     *
     * @param className
     *		Name of the top-level class (used only for printing comments)
     *
     */

    public void printHeaderInfo(String className)
    {
        outFile.println("// Declarations for " + className);
        outFile.println("// Declarations written " + (new Date()));
        outFile.print("\n\n");

        outFile.println("VarComparability");
        outFile.println("none\n");

        outFile.println("ListImplementors");
        outFile.println("java.util.List\n\n");
    }

    /**
     * Register an exit point for a method or constructor.  If you're printing
     * decls in whole-class mode, you should call this first for every exit
     * point, otherwise no method exit Ppt decls will get generated.
     */

    public void registerExit(Member method, int exitLoc)
    {
        Set exitLocs = (Set) methodToExitLocs.get(method);
        //create new set for this method if it does not exist yet
        if (exitLocs == null)
        {
            exitLocs = new HashSet();
            methodToExitLocs.put(method, exitLocs);
        }
        exitLocs.add(new Integer(exitLoc));
    }

    /**
     * Register a list of argument names for the method.  We can't
     * get this out of the java.lang.reflect interface so it has to
     * come in from outside.
     */

    public void registerArgNames(Member method, List argnames)
    {
        methodToArgNames.put(method, argnames);
    }

    /**
     * Prints declarations for all the methods in the indicated class.  You
     * had better call registerExit first so that it will know what exit
     * Ppt decls to generate for each method/constructor in the class.
     *
     * @param cinfo
     *		Class whose declarations should be printed.
     *
     */

    public void printDeclClass (ClassInfo cinfo)
    {
        // Note current class
        current_class = cinfo.clazz;

        // Print all methods and constructors
        for (MethodInfo mi : cinfo.get_method_infos())
        {
            Member member = mi.member;
            List<String> argnames = Arrays.asList (mi.arg_names);

            //don't want to instrument these types of methods
            if (!shouldInstrumentMethod(member))
                continue;

            // System.out.printf ("method %s, syn=%b\n", member,
            //                     member.isSynthetic());

            printMethodEntryInternal(member, methodEntryName(member), argnames);

            //print exit program point for EACH exit location in the method
            //(that was encountered during this execution of the program)
            for (Integer exitLoc : mi.exit_locations)
            {
                String name = methodExitName(member, exitLoc.intValue());
                printMethodExitInternal(member, name, argnames);
            }
        }

        printClassPpt (cinfo.clazz, cinfo.class_name + ":::CLASS");

        printObjectInternal(cinfo.clazz, classObjectName(cinfo.clazz));
    }

    /**
     * Prints the decls info for a method/constructor entry, if the decl
     * has not already been emitted.  This can be called externally to print
     * Ppt decls one-at-a-time.
     */

    public void printMethodEntry(Member method, List argnames)
    {
        String name = methodEntryName(method);
        if (emittedPpts.contains(name))
            return;
        emittedPpts.add(name);
        if (!Modifier.isStatic(method.getModifiers()))
            printObject(method.getDeclaringClass());
        printMethodEntryInternal(method, name, argnames);
    }

    private void printMethodEntryInternal(Member method, String name, List argnames)
    {
        //don't print class vars at method entries for constructors
        outFile.println(declareHeader);
        outFile.println(name);
        printMethod(method, !(method instanceof Constructor), argnames);
        outFile.println();
    }

    /**
     * Prints the decls info for a method/constructor exit, if the decl
     * has not already been emitted.  This can be called externally to print
     * Ppt decls one-at-a-time.
     */
    public void printMethodExit(Member method, int exitLoc, List argnames)
    {
        String name = methodExitName(method, exitLoc);
        if (emittedPpts.contains(name))
            return;
        emittedPpts.add(name);
        printMethodExitInternal(method, name, argnames);
    }

    private void printMethodExitInternal(Member method, String name, List argnames)
    {
        outFile.println(declareHeader);
        outFile.println(name);

        // Print arguments
        printLocalVars(method, argnames, "", daikonDepth);

        //print return type information for methods only and not constructors
        if (method instanceof Method)
        {
            Class returnType = ((Method) method).getReturnType();
            if (!(returnType.equals(Void.TYPE)))
            {
                // System.out.printf ("return type for %s is %s\n", method,
                //                   returnType);

                outFile.println("return");
                outFile.println (stdClassName (returnType));
                outFile.println(getRepName(returnType, false));
                outFile.println(compareInfoDefault);

                checkForListDecl(returnType, "return", "", false);
                checkForImplicitList(returnType, "return", "", daikonDepth);
                checkForRuntimeClass(returnType, "return", ""); //.class var
                checkForString(returnType, "return", "");

                checkForVarRecursion(returnType, "return", "", daikonDepth, false);
            }
        }

        // Print class variables
        printClassVars( Modifier.isStatic(method.getModifiers()),
                        method.getDeclaringClass(), "", daikonDepth, false);

        outFile.println();
    }

    //print the Object Ppt decl.
    private void printObject(Class type)
    {
        String name = classObjectName(type);
        if (emittedPpts.contains(name))
            return;
        emittedPpts.add(name);
        printObjectInternal(type, name);
    }

    private void printObjectInternal(Class type, String name)
    {
        outFile.println(declareHeader);
        outFile.println(name);
        printClassVars(false, type, "", daikonDepth, false);
        outFile.println();
    }

    /**
     * Prints the class program point -- this contains only
     * the static variables
     */
    private void printClassPpt (Class type, String name)
    {
        Field[] fields = type.getDeclaredFields();

        // only print these points if we have at least one static field
        int static_fields = 0;
        for (Field f : fields)
            if (Modifier.isStatic(f.getModifiers()))
                static_fields++;
        if (static_fields == 0)
            return;

        outFile.println (declareHeader);
        outFile.println (name);

        // System.out.printf ("processing %d fields in %s\n", fields.length,
        //                    type);
        for (int i = 0; i < fields.length; i++)
        {
            Field classField = fields[i];

            if (!Modifier.isStatic(classField.getModifiers()))
                continue;

            if (!isFieldVisible (current_class, classField))
            {
                // System.out.printf ("skipping not visible field %s, in %s\n",
                //                     classField, type);
                continue;
            }

            Class fieldType = classField.getType();
            String newOffset = printDeclVar(classField, "", daikonDepth,
                                            false);
            checkForVarRecursion(fieldType, classField.getName(), newOffset,
                                 daikonDepth, false);
        }

        outFile.println();
    }


    //prints the decls info for a method
    //called by printMethodEntry (printMethodExit does this directly so it
    // can place the return after the arguments (to match dfej)
    private void printMethod(Member method, boolean shouldPrintClass, List argnames)
    {
        printLocalVars(method, argnames, "", daikonDepth);
        if (shouldPrintClass)
            printClassVars(Modifier.isStatic(method.getModifiers()), method.getDeclaringClass(), "", daikonDepth, false);
    }

    //print local variables (the arguments) of a method
    private void printLocalVars(Member method, List argnames, String offset, int depth)
    {
        Class[] arguments = (method instanceof Constructor) ? ((Constructor) method).getParameterTypes() : ((Method) method).getParameterTypes();
        Iterator argnamesiter = argnames.iterator();
        for (int i = 0; (i < arguments.length) && argnamesiter.hasNext(); i++)
        {
            Class type = arguments[i];
            String name = (String) argnamesiter.next();
            printDeclVar(type, name, offset, depth);
            checkForVarRecursion(type, name, offset, depth, false);
        }
    }

    //print class variables for the given type
    private void printClassVars(boolean dontPrintInst, Class type, String offset, int depth, boolean inArray)
    {
        //must be first level of recursion to print "this" field
        if (!dontPrintInst && offset.equals(""))
        {
            //print the "this" field
            outFile.println("this");
            outFile.println(type.getName() + isParamString);
            outFile.println(getRepName(type, false));
            outFile.println(compareInfoDefault);

            //.class variable
            if (shouldAddRuntimeClass(type))
            {
                outFile.println("this.class");
                outFile.println(classClassName);
                outFile.println(stringClassName);
                outFile.println(compareInfoDefault);
            }
        }

        if (shouldExcludeClass(type.getName()))
        {
            //System.out.println("Excluding recursion on " + type.getName());
            return;
        }

        Field[] fields = type.getDeclaredFields();
        // System.out.printf ("processing %d fields in %s\n", fields.length,
        //                    type);
        for (int i = 0; i < fields.length; i++)
        {
            Field classField = fields[i];

            if (!Modifier.isStatic(classField.getModifiers()) && dontPrintInst)
                continue;

            if (!isFieldVisible (current_class, classField))
            {
                // System.out.printf ("skipping not visible field %s, in %s\n",
                //                     classField, type);
                continue;
            }

            Class fieldType = classField.getType();
            String newOffset = printDeclVar(classField, offset, depth, inArray);
            checkForVarRecursion(fieldType, classField.getName(), newOffset, depth, inArray);
        }
    }

    //check for recursion of a variable based on depth and type
    private void checkForVarRecursion(Class type, String name, String offset,
                                      int depth, boolean inArray)
    {
        if (type.isPrimitive())
            return;
        else if (type.isArray())
        {
            // System.out.printf ("cfvr: type=%s name=%s inArray=%b\n", type,
            //                  name, inArray);

            //don't go into multi-dimensional array
            if (inArray)
                return;

            Class arrayType = type.getComponentType();
            if (arrayType.isPrimitive())
            {

                outFile.println(offset + name + "[]");
                outFile.println(arrayType.getName() + "[]");
                outFile.println(getRepName(arrayType, true) + "[]");
                outFile.println(compareInfoDefault); //no comparability info yet
            }
            //multi-dimensional arrays!
            else if (arrayType.isArray())
            {
                outFile.println(offset + name + "[]");
                outFile.println(arrayType.getName() + "[]");
                outFile.println(getRepName(arrayType, true) + "[]");
                outFile.println(compareInfoDefault); //no comparability info yet
                checkForVarRecursion(arrayType, "", offset + name + "[]", depth, true);
            }
            else
            // regular class
            {
                outFile.println(offset + name + "[]");
                outFile.println(arrayType.getName() + "[]");
                // System.out.printf ("getRepName for %s on %s\n", arrayType,
                //                   offset + name);
                outFile.println(getRepName(arrayType, true) + "[]");
                outFile.println(compareInfoDefault); //no comparability info yet

                // print out the class of each element in the array.  For
                // some reason dfej doesn't include this on returned arrays
                // or arguments.  We attempt to get at arguments because
                // they don't have an offset (no this pointer)
                if (!name.equals ("return") && !offset.equals (""))
                  checkForRuntimeClass (type, name + "[]", offset);

                //System.out.println(name + " --- " + arrayType.getName());

                //if (classSet.contains(arrayType))
                if (!shouldExcludeClass(arrayType.getName()))
                {
                    checkForString(arrayType, name + "[]", offset);
                    printClassVars(false, arrayType, offset + name + "[].", depth - 1, true);
                }
                else
                {
                    //.class vars
                    checkForRuntimeClass(type, name + "[]", offset);
                    checkForString(arrayType, name + "[]", offset);
                }
            }
        }
        else
        // regular old class type
        {
            if (depth <= 0)
            {
                //don't recurse any more!
                return;
            }
            //if (classSet.contains(type))
            //if(!shouldExcludeClass(type.getName()))
            if (shouldEnterClass (type))
                printClassVars(false, type, offset + name + ".", depth - 1,
                               inArray);
        }
    }

    //prints the decl info for a single local variable
    private void printDeclVar(Class type, String name, String offset, int depth)
    {
        outFile.println(offset + name);
        outFile.print(stdClassName(type));
        outFile.print(isParamString);
        outFile.print("\n");
        outFile.println(getRepName(type, false));
        outFile.println(compareInfoDefault); //no comparability info right now

        checkForListDecl(type, name, offset, false);
        checkForImplicitList(type, name, offset, depth);
        checkForRuntimeClass(type, name, offset); //.class var
        checkForString(type, name, offset);
    }

    //determines if type has exactly 1 non-static field of same type
    // (implicit list) and prints asociated decls
    private void checkForImplicitList(Class type, String name, String offset, int depth)
    {
        //disable this method until dtracewriter finishes analogous method
        if(1 == 1)
            return;
        
        if (type.isPrimitive() || type.isArray())
            return;

        //no "multi-dim" arrays
        if (offset.contains("[]") || name.contains("[]"))
            return;

        //if(offset.startsWith("this."))
        //  offset = "";

        Field linkField = isImplicitLinkedList(type);

        //if linkField != null, we have found an implicit linked list
        if (linkField != null)
        {
            name = "~" + name + "~[]";

            //System.out.println("name is " + name);
            //System.out.println("offset is " + offset);

            //outFile.println(offset + name);
            //outFile.println(type.getName() + "[]");
            //outFile.println(getRepName(type) + "[]");
            //outFile.println(compareInfoDefault);

            checkForVarRecursion(type, name, offset, depth - 1, true);
        }
    }

    //determines if type implements list
    //and prints associated decls, if necessary
    private void checkForListDecl(Class type, String name, String offset,
                                  boolean inArray)
    {
        if (inArray || type.isPrimitive() || type.isArray())
            return;

        if (implementsList(type))
        {
            outFile.println(offset + name + "[]");
            outFile.println(type.getName());
            outFile.println("hashcode[]");
            outFile.println(compareInfoDefault);

            //.class var
            outFile.println(offset + name + "[].class");
            outFile.println(classClassName + "[]");
            outFile.println(stringClassName + "[]");
            outFile.println(compareInfoDefault);
        }
    }

    //prints the decl info for a single class variable
    private String printDeclVar(Field field, String offset, int depth,
                                boolean inArray)
    {
        String arr_str = "";
        if (inArray)
            arr_str = "[]";

        //we want to access all fields...
        if(!field.isAccessible())
            field.setAccessible(true);

        Class type = field.getType();
        String name = field.getName();
        int modifiers = field.getModifiers();

        if (offset.length() > 0) //already starting with "this"
        {
            outFile.print(offset + name);
        }
        else if (Modifier.isStatic(modifiers))
        {
            offset = offset + field.getDeclaringClass().getName() + ".";
            outFile.print(offset + name);
        }
        else
        //instance field, first recursion step
        {
            offset = "this.";
            outFile.print(offset + name);
        }

        outFile.println();
        String type_name = stdClassName (type);
        // System.out.printf ("transformed type name from %s to %s\n",
        //                    type.getName(), type_name);
        outFile.println (type_name + arr_str);
        outFile.print(getRepName(type, inArray) + arr_str);
        if (Modifier.isStatic(modifiers) && Modifier.isFinal(modifiers)
            && type.isPrimitive())
        {
            try
            {
                // Note, getting the value of a static constant triggers
                // class initialization!!
                Object val = field.get(null);
                outFile.println(" = " + val.toString());
            }
            catch (IllegalAccessException e)
            {
                throw new Error ("field " + field + ": " + e);
                // outFile.println();
            }
        }
        else
            outFile.println();

        outFile.println(compareInfoDefault); //no comparability info right now

        checkForListDecl(type, name, offset, inArray);
        checkForImplicitList(type, name, offset, depth);
        checkForRuntimeClass(type, name, offset); //.class var
        checkForString(type, name, offset);
        return offset;
    }

    //checks the given type to see if it is a string
    //if so, it prints out the correct decs info
    private void checkForString(Class type, String name, String offset)
    {
        if (!type.getName().equals(stringClassName))
            return;

        String postString = ""; //either array braces or an empty string

        if (name.endsWith("[]"))
            postString = "[]";

        outFile.println(offset + name + ".toString");
        outFile.println(stringClassName + postString);
        outFile.println(stringClassName + postString);
        outFile.println(compareInfoDefault);

    }

    //checks the given type to see if it requires a .class
    //addition to the decls file
    //if so, it prints out the correct .class variable info
    private void checkForRuntimeClass(Class type, String name, String offset)
    {
        if (!shouldAddRuntimeClass(type))
            return;

        String postString = ""; //either array braces or an empty string

        if (name.contains("[]") || offset.contains ("[]"))
            postString = "[]";
        // System.out.printf ("offset  = %s, name = %s, postString = '%s'\n",
        //                    offset, name, postString);

        outFile.println(offset + name + ".class");
        outFile.println(classClassName + postString);
        outFile.println(stringClassName + postString);
        outFile.println(compareInfoDefault);
    }

}
