package daikon.chicory;

import java.io.*;
import java.util.*;
import java.lang.reflect.*;


/**
 *
 * DeclWriter writes the .decls file to a stream.  As it does this, it
 * constructs traversal pattern trees (see {@link DaikonVariableInfo})  for eachs
 * program point.  These are later used by the {@link DTraceWriter}.
 *
 */
public class DeclWriter extends DaikonWriter
{
    // Notes:
    //
    //  Class.getName() returns JVM names (eg, [Ljava.lang.String;)

    /**default string for comparability info**/
    private static final String compareInfoDefault = "22";

    /**header string before each new method entry or exit point**/
    public static final String declareHeader = "DECLARE";

    /**used to assert that a given variable is a parameter to a method**/
    private static final String isParamString = " # isParam=true";

    /**map method to its argument names**/
    private Map <Method, List<String>> methodToArgNames;

    /**keep track of Ppts we've already emitted**/
    private Set <String> emittedPpts;

    /**stream to write to**/
    private PrintStream outFile;

    /**levels of recursion**/
    protected int daikonDepth;

    //certain hardcoded class names
    protected static final String classClassName = "java.lang.Class";
    protected static final String stringClassName = "java.lang.String";

    /** debug information about daikon variables  **/
    private boolean debug_vars = false;

    /**
     * Constructs a DeclWriter, preparing it to receive messages.
     *
     * @param writer
     *            Stream to write to
     * @param depth
     *            Depth of tree recursion to traverse for each variable
     */
    public DeclWriter(PrintStream writer, int depth)
    {
        super();

        outFile = writer;

        if (depth <= 0)
            throw new RuntimeException("Daikon depth must be positive");

        daikonDepth = depth;

        methodToArgNames = new HashMap <Method, List<String> >();
        emittedPpts = new HashSet <String> ();
    }

    /**
     * Prints header information to the decls file.  Should be called once
     * before emitting any other declarations.
     *
     * @param className
     *        Name of the top-level class (used only for printing comments)
     *
     */
    public void printHeaderInfo(String className)
    {
        outFile.println("// Declarations for " + className);
        outFile.println("// Declarations written " + (new Date()));
        outFile.println();
        outFile.println();

        outFile.println("VarComparability");
        outFile.println("none" + DaikonWriter.lineSep);

        outFile.println("ListImplementors");
        outFile.println("java.util.List" + DaikonWriter.lineSep + DaikonWriter.lineSep);
    }

    /**
     * Register a list of argument names for the method.  We can't
     * get this out of the java.lang.reflect interface so it has to
     * come in from outside.
     */
    public void registerArgNames(Member method, List <String> argnames)
    {
        methodToArgNames.put((Method) method, argnames);
    }

    /**
     * Prints declarations for all the methods in the indicated class.  You
     * had better call registerExit first so that it will know what exit
     * Ppt decls to generate for each method/constructor in the class.
     *
     * This method is called in Runtime to print decls info for a class
     * @param cinfo
     *        Class whose declarations should be printed.
     *
     */
    public void printDeclClass (ClassInfo cinfo)
    {
        // Print all methods and constructors
        for (MethodInfo mi : cinfo.get_method_infos())
        {
            Member member = mi.member;
            List<String> argnames = Arrays.asList (mi.arg_names);

            //don't want to instrument these types of methods
            if (!shouldInstrumentMethod(member))
                continue;

            //initialize the root node for this program point
            RootInfo entryRoot = new RootInfo();
            mi.traversalEnter = entryRoot;
            printMethodEntry (cinfo, entryRoot, member,
                              methodEntryName(member), argnames);
            if (debug_vars)
                System.out.printf ("variables for %s:%n%s%n", member,
                                   entryRoot);

            //print exit program point for EACH exit location in the method
            //(that was encountered during this execution of the program)
            Set<Integer> theExits = new HashSet<Integer>(mi.exit_locations);
            for (Integer exitLoc : theExits)
            {
                //initialize root node for this exit program point
                RootInfo exitRoot = new RootInfo();
                mi.traversalExit.put(exitLoc, exitRoot);
                String name = methodExitName(member, exitLoc.intValue());
                printMethodExit(cinfo, exitRoot, member, name, argnames);
            }
        }

        printClassPpt (cinfo, cinfo.clazz, cinfo.class_name + ":::CLASS");
        printObjectInternal(cinfo, cinfo.clazz, classObjectName(cinfo.clazz));
    }

    private void printMethodEntry(ClassInfo cinfo, DaikonVariableInfo curNode, Member method, String name, List argnames)
    {
        outFile.println(declareHeader);
        outFile.println(name);

        if (debug_vars)
            System.out.printf ("Processing %s Entry%n", name);
        printMethodVars(cinfo, curNode, method, argnames);

        outFile.println();
    }

    private void printMethodExit(ClassInfo cinfo, DaikonVariableInfo curNode, Member method, String name, List argnames)
    {
        outFile.println(declareHeader);
        outFile.println(name);

        // Print arguments
        printLocalVars(cinfo, curNode, method, argnames, "", daikonDepth);

        // Print return type information for methods only and not constructors
        if (method instanceof Method)
        {
            Class returnType = ((Method) method).getReturnType();
            if (!(returnType.equals(Void.TYPE)))
            {
                outFile.println("return");
                outFile.println(stdClassName (returnType));
                outFile.println(getRepName(returnType, false));
                outFile.println(compareInfoDefault);

                // add a new ReturnInfo object to the traversal tree
                DaikonVariableInfo retInfo = new ReturnInfo();
                curNode.addChild(retInfo);

                checkForDerivedVariables(retInfo, returnType, "return", "", /*inArray = */ false);

                printChildren(cinfo, returnType, retInfo, "return", "", daikonDepth, false);
            }
        }

        // Print class variables
        printClassVars( cinfo, curNode, Modifier.isStatic(method.getModifiers()),
                        method.getDeclaringClass(), "", daikonDepth, false);

        outFile.println();
    }

    // print the Object Ppt decl.
    private void printObject(ClassInfo cinfo, Class type)
    {
        String name = classObjectName(type);
        if (emittedPpts.contains(name))
            return;
        emittedPpts.add(name);
        printObjectInternal(cinfo, type, name);
    }

    // a wrapper around printObject which adds the decl header and the object's program point name
    private void printObjectInternal(ClassInfo cinfo, Class type, String name)
    {
        outFile.println(declareHeader);
        outFile.println(name);
        printClassVars(cinfo, new RootInfo(), false, type, "", daikonDepth, false);
        outFile.println();
    }

    /**
     * Prints the class program point -- this contains only
     * the static variables
     */
    private void printClassPpt (ClassInfo cinfo, Class type, String name)
    {
        Field[] fields = type.getDeclaredFields();

        // only print these points if we have at least one static field
        int static_fields = 0;
        for (Field f : fields)
            if (Modifier.isStatic(f.getModifiers()) && isFieldVisible (cinfo.clazz, f))
                static_fields++;
        if (static_fields == 0)
            return;

        outFile.println (declareHeader);
        outFile.println (name);

        for (Field classField: fields)
        {
            // only process static fields
            if (!Modifier.isStatic(classField.getModifiers()))
                continue;

            if (!isFieldVisible (cinfo.clazz, classField))
            {
                // skip if not visible (see isFieldVisible)
                continue;
            }

            Class fieldType = classField.getType();
            StringBuffer buf = new StringBuffer();

            // don't worry about populating traversal pattern here...
            // just pass in new RootInfo()
            DaikonVariableInfo newChild = printDeclVar(cinfo, new RootInfo(), classField, "", daikonDepth,
                                            false, buf);
            String newOffset = buf.toString();
            printChildren(cinfo, fieldType, newChild, classField.getName(), newOffset,
                                 daikonDepth, false);
        }

        outFile.println();
    }


    /**
     *    Prints the decls info for a method's local variables (parameters) and class variables.
     *    The .decls headers are not printed from this method, however.
     *
     *    Called by printMethodEntry (printMethodExit does this directly so it
     *    can place the return after the arguments to match dfej).
     */
    private void printMethodVars(ClassInfo cinfo, DaikonVariableInfo curNode, Member method,
            List argnames)
    {
        // don't print class vars at method entries for constructors
        boolean shouldPrintClass = !(method instanceof Constructor);

        printLocalVars(cinfo, curNode, method, argnames, /*offset = */ "", daikonDepth);
        if (shouldPrintClass)
            printClassVars(cinfo, curNode, Modifier.isStatic(method.getModifiers()),
                    method.getDeclaringClass(), /*offset = */ "", daikonDepth, /*inArray = */ false);
    }

    /**
     * Print local variables (the parameters) of a method
     */
    private void printLocalVars(ClassInfo cinfo, DaikonVariableInfo curNode, Member method, List argnames, String offset, int depth)
    {
        Class[] arguments = (method instanceof Constructor) ? ((Constructor) method).getParameterTypes() : ((Method) method).getParameterTypes();
        Iterator<String> argnamesiter = argnames.iterator();
        for (int i = 0; (i < arguments.length) && argnamesiter.hasNext(); i++)
        {
            Class type = arguments[i];
            String name = argnamesiter.next();
            DaikonVariableInfo theChild = printDeclVar(cinfo, curNode, type, name, offset, depth, i);
            printChildren(cinfo, type, theChild, name, offset, depth, false);
        }
    }

    /**
     * Print class variables (ie, the fields) for the given type and attach new nodes as children of curNode
     */
    private void printClassVars(ClassInfo cinfo, DaikonVariableInfo curNode, boolean dontPrintInstanceVars,
            Class type, String offset, int depth, boolean inArray)
    {
        //DaikonVariableInfo corresponding to the "this" object
        DaikonVariableInfo thisInfo;

        //must be first level of recursion to print "this" field
        if (!dontPrintInstanceVars && offset.equals(""))
        {
            thisInfo = new ThisObjInfo();

            //print the "this" field
            outFile.println("this");
            outFile.println(type.getName() + isParamString);
            outFile.println(getRepName(type, false));
            outFile.println(compareInfoDefault);

            curNode.addChild(thisInfo);

            //.class variable
            if (shouldAddRuntimeClass(type))
            {
                outFile.println("this.class");
                outFile.println(classClassName);
                outFile.println(stringClassName);
                outFile.println(compareInfoDefault);

                DaikonVariableInfo thisClass = new DaikonClassInfo("this.class", false);
                thisInfo.addChild(thisClass);
            }
        }
        else
            thisInfo = curNode;

        Field[] fields = type.getDeclaredFields();
        if (debug_vars)
            System.out.printf ("%s: [%s] %d dontPrintInstanceVars = %b, "
                               + "inArray = %b%n", type, offset, fields.length,
                               dontPrintInstanceVars, inArray);
        for (int i = 0; i < fields.length; i++)
        {

            Field classField = fields[i];
            if (debug_vars)
                System.out.printf ("considering field %s%n", classField);

            if (!Modifier.isStatic(classField.getModifiers()) &&
                dontPrintInstanceVars){
                if (debug_vars)
                    System.out.printf ("--field !static and instance var %b%n",
                                   dontPrintInstanceVars);
                continue;
            }

            // don't print arrays of the same static field
            if(Modifier.isStatic(classField.getModifiers()) && inArray) {
                if (debug_vars)
                    System.out.printf ("--field static and inArray%n");
                continue;
            }

            if (!isFieldVisible (cinfo.clazz, classField))
            {
                if (debug_vars)
                    System.out.printf ("--field not visible%n");
                continue;
            }

            Class fieldType = classField.getType();

            StringBuffer buf = new StringBuffer();
            DaikonVariableInfo newChild = printDeclVar(cinfo, thisInfo,
                                     classField, offset, depth, inArray, buf);
            if (debug_vars)
                System.out.printf ("--Created DaikonVariable %s%n", newChild);
            String newOffset = buf.toString();
            printChildren(cinfo, fieldType, newChild, classField.getName(),
                          newOffset, depth, inArray);
        }
        
        // If appropriate, print out decls information for pure methods
        // and add to the tree
        // Check dontPrintInstanceVars is basically checking if the program point method
        // (not the pure method) is static.  If it is, don't continue because we can't
        // call instance methods (all pure methods we consider are instance methods)
        // from static methods
        if (ChicoryPremain.shouldDoPurity() && !dontPrintInstanceVars)
        {
            ClassInfo typeInfo = null;

            try
            {
                typeInfo = Runtime.getClassInfoFromClass(type);
            }
            catch (RuntimeException e)
            {
                // Could not find the class... no further purity analysis
                typeInfo = null;
            }

            if (typeInfo != null)
            {
                for (MethodInfo meth : typeInfo.method_infos)
                {
                    if (meth.isPure())
                    {
                        StringBuffer buf = new StringBuffer();
                        DaikonVariableInfo newChild = printPureMethodDecl(
                                cinfo, thisInfo, meth, offset, depth, inArray,
                                buf);
                        String newOffset = buf.toString();
                        printChildren(cinfo, ((Method) meth.member)
                                .getReturnType(), newChild, meth.member
                                .getName(), newOffset, depth, inArray);

                    }
                }
            }
        }
    }

    /**
     *
     * Checks for "derived" Chicory variables: .class, .tostring, and java.util.List implementors
     * and adds appropriate .decls information.
     */
    private void checkForDerivedVariables(DaikonVariableInfo node, Class type, String name,
            String offset, boolean inArray)
    {
        checkForListDecl(node, type, name, offset, inArray); // implements java.util.List?

        //Not fully implemented yet, don't call
        //checkForImplicitList(cinfo, type, name, offset, depth);

        checkForRuntimeClass(node, type, name, offset); //.class var
        checkForString(node, type, name, offset); //.tostring var
    }

    /**
     *     Explores the tree one level deeper (see {@link DaikonVariableInfo}).
     *     This method adds child nodes to curNode.
     *
     *     For example: "recurse" on a hashcode array object to print the actual array of values
     *     or recurse on hashcode variable to print its fields.
     *     Also accounts for derived variables (.class, .tostring) and "recurses" on arrays (that is,
     *     adds a variable to print out the arrays's elements as opposed to just the hashcode of the array).
     *
     *     @param name The name of the variable currently being examined, such as "ballCount"
     *     @param offset The representation of the variables we have previously examined.
     *                   For examples, offset could be "this." in which case offset + name would be
     *                   "this.ballCount."
     */
    private void printChildren(ClassInfo cinfo, Class type, DaikonVariableInfo curNode, String name, String offset,
                                      int depthRemaining, boolean inArray)
    {
        if (type.isPrimitive())
            return;
        else if (type.isArray())
        {
            // don't go into more than one dimension of a multi-dimensional array
            if (inArray)
                return;

            Class arrayType = type.getComponentType();
            if (arrayType.isPrimitive())
            {
                outFile.println(offset + name + "[]");
                outFile.println(arrayType.getName() + "[]");
                outFile.println(getRepName(arrayType, true) + "[]");
                outFile.println(compareInfoDefault); //no comparability info yet

                DaikonVariableInfo newChild = new ArrayInfo(offset + name + "[]");
                curNode.addChild(newChild);
            }
            // multi-dimensional arrays (not currently used)
            else if (arrayType.isArray())
            {
                outFile.println(offset + name + "[]");
                outFile.println(arrayType.getName() + "[]");
                outFile.println(getRepName(arrayType, true) + "[]");
                outFile.println(compareInfoDefault); //no comparability info yet

                DaikonVariableInfo newChild = new ArrayInfo(offset + name + "[]");
                curNode.addChild(newChild);

                printChildren(cinfo, arrayType, newChild, "", offset + name + "[]", depthRemaining, true);
            }
            // array is 1-dimensional and element type is a regular class
            else
            {
                outFile.println(offset + name + "[]");
                outFile.println(arrayType.getName() + "[]");
                outFile.println(getRepName(arrayType, true) + "[]");
                outFile.println(compareInfoDefault); //no comparability info yet

                DaikonVariableInfo newChild = new ArrayInfo(offset + name + "[]");
                curNode.addChild(newChild);

                // Print out the class of each element in the array.  For
                // some reason dfej doesn't include this on returned arrays
                // or parameters.
                // The offset will only be equal to ""
                // if we are examining a local variable (parameter).
                if (!name.equals ("return") && !offset.equals (""))
                  checkForRuntimeClass (newChild, type, name + "[]", offset);


                checkForString(newChild, arrayType, name + "[]", offset);
                printClassVars(cinfo, newChild, false, arrayType, offset + name + "[].", depthRemaining - 1, true);

            }
        }
        // regular old class type
        else
        {
            if (depthRemaining <= 0)
            {
                // don't recurse any more!
                return;
            }
            if (notSystemClass (type))
                printClassVars(cinfo, curNode, false, type, offset + name + ".", depthRemaining - 1,
                               inArray);
        }
    }

    /**
     * Prints the decl info for a single local variable (usually a
     * method argument)
     * @return The newly created DaikonVariableInfo object, whose
     * parent is curNode
     */
    private DaikonVariableInfo printDeclVar(ClassInfo cinfo, DaikonVariableInfo curNode, Class type, String name, String offset, int depth, int argNum)
    {
        outFile.println(offset + name);
        outFile.print(stdClassName(type));
        outFile.print(isParamString);
        outFile.print(DaikonWriter.lineSep);
        outFile.println(getRepName(type, false));
        outFile.println(compareInfoDefault); //no comparability info right now

        // add this variable to the tree as a child of curNode
        DaikonVariableInfo newChild = new ParameterInfo(offset + name, argNum);
        curNode.addChild(newChild);

        checkForDerivedVariables(newChild, type, name, offset, false);

        return newChild;
    }

//    /**
//     *
//     * Determines if type has exactly 1 non-static field of same type
//     * (implicit list) and prints asociated decls.
//     *
//     **/
//    private void checkForImplicitList(ClassInfo cinfo, Class type, String name, String offset, int depth)
//    {
//        //disable this method until dtracewriter finishes analogous method
//        if(1 == 1)
//            return;
//
//        if (type.isPrimitive() || type.isArray())
//            return;
//
//        //no "multi-dim" arrays
//        if (offset.contains("[]") || name.contains("[]"))
//            return;
//
//        //if(offset.startsWith("this."))
//        //  offset = "";
//
//        Field linkField = isImplicitLinkedList(type);
//
//        //if linkField != null, we have found an implicit linked list
//        if (linkField != null)
//        {
//            name = "~" + name + "~[]";
//
//            //System.out.println("name is " + name);
//            //System.out.println("offset is " + offset);
//
//            //outFile.println(offset + name);
//            //outFile.println(type.getName() + "[]");
//            //outFile.println(getRepName(type) + "[]");
//            //outFile.println(compareInfoDefault);
//
//            //TODO get actual DaikonVariableInfo here
//            printChildren(cinfo, type, new RootInfo(), name, offset, depth - 1, true);
//        }
//    }

    /**
     * Determines if type implements list
     * and prints associated decls, if necessary
     */
    private void checkForListDecl(DaikonVariableInfo curNode, Class type, String name, String offset,
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


            DaikonVariableInfo child = new ListInfo(offset + name + "[]", type);
            curNode.addChild(child);

            //.class var
            outFile.println(offset + name + "[].class");
            outFile.println(classClassName + "[]");
            outFile.println(stringClassName + "[]");
            outFile.println(compareInfoDefault);

            DaikonVariableInfo childClass = new DaikonClassInfo(offset + name + "[].class", true);
            curNode.addChild(childClass);
        }
    }

    /**
     * Prints the decl info for a single class variable (a field)
     */
    private DaikonVariableInfo printDeclVar(ClassInfo curClass, DaikonVariableInfo curNode, Field field, String offset, int depth,
                                boolean inArray, StringBuffer buf)
    {
        String arr_str = "";
        if (inArray)
            arr_str = "[]";

        boolean changedAccess = false;

        //we want to access all fields...
        if(!field.isAccessible())
        {
            changedAccess = true;
            field.setAccessible(true);
        }

        Class type = field.getType();
        String name = field.getName();
        int modifiers = field.getModifiers();

        if (offset.length() > 0) // offset already starts with "this"
        {
        }
        else if (Modifier.isStatic(modifiers))
        {
            offset = offset + field.getDeclaringClass().getName() + ".";
        }
        // instance field, first recursion step
        else
        {
            offset = "this.";
        }
        outFile.println(offset + name);

        String type_name = stdClassName (type);

        // Print declared type, followed by auxiliary information
        outFile.print (type_name + arr_str);

        // Print auxiliary information
        appendAuxInfo(field);
        outFile.println();

        outFile.print(getRepName(type, inArray) + arr_str);

        boolean addFieldToTree = true;
        if (DaikonWriter.isStaticConstField(field) && !inArray)
        {
            ClassInfo cinfo = Runtime.getClassInfoFromClass(field.getDeclaringClass());
            String value = cinfo.staticMap.get(name);

            // System.out.printf ("static final value = %s%n", value);

            // in this case, we don't want to print this variable to
            // the dtrace file
            if(value != null)
            {
                outFile.print(" = " + value);
                addFieldToTree = false;
            }
            //else
            //{
                // don't print anything
                // because this field wasn't declared with an actual "hardcoded" constant
            //}

        }

        outFile.println();
        outFile.println(compareInfoDefault); //no comparability info right now


        DaikonVariableInfo newField = new FieldInfo(offset + name, field, inArray);
        if(addFieldToTree)
        {
            curNode.addChild(newField);
            if (debug_vars)
                System.out.printf ("Added field %s to node %s%n", newField,
                                   curNode);
        }

        checkForDerivedVariables(newField, type, name, offset, inArray);

        buf.append(offset);

        if(changedAccess)
        {
            field.setAccessible(false);
        }

        return newField;
    }
    
    /**
     * Prints the decl info for a pure method
     */
    
    //TODO factor out shared code with printDeclVar
    private DaikonVariableInfo printPureMethodDecl(ClassInfo curClass, DaikonVariableInfo curNode, 
            MethodInfo minfo, String offset, int depth,
            boolean inArray, StringBuffer buf)
    {
        String arr_str = "";
        if (inArray)
            arr_str = "[]";

        Method meth = (Method) minfo.member;
        
        
        boolean changedAccess = false;

        //we want to access all fields...
        if(!meth.isAccessible())
        {
            changedAccess = true;
            meth.setAccessible(true);
        }

        Class type = meth.getReturnType();
        
        String name = meth.getName() + "()";
        int modifiers = meth.getModifiers();

        if (offset.length() > 0) // offset already starts with "this"
        {
        }
        else
        {
            offset = "this.";
        }
        outFile.println(offset + name);

        String type_name = stdClassName (type);

        outFile.println (type_name + arr_str);
        outFile.println(getRepName(type, inArray) + arr_str);
        outFile.println(compareInfoDefault); //no comparability info right now


        DaikonVariableInfo newPure = new PureMethodInfo(offset + name, minfo,
                inArray);

        curNode.addChild(newPure);
        if (debug_vars)
            System.out.printf("Added pure method %s to node %s%n", newPure, curNode);

        checkForDerivedVariables(newPure, type, name, offset, inArray);

        buf.append(offset);

        if(changedAccess)
        {
            meth.setAccessible(false);
        }

        return newPure;
    }

    // Appends as auxiliary information:
    // the package name of the declaring class
    private void appendAuxInfo(Field field) {
        // int modifiers = field.getModifiers();

        Package p = field.getDeclaringClass().getPackage();
        String pkgName = (p == null ? null : p.getName());

        //System.out.printf("Package name for type  %s is %s%n", type, pkgName);

        // String staticString = (Modifier.isStatic(modifiers)) ? "true" : "false";
        outFile.print(" # ");
        if (pkgName != null) {
            outFile.print("declaringClassPackageName=" + pkgName + ", ");
        }
        // outFile.printf ("%s, ", field);
    }


    /**
     * checks the given type to see if it is a string
     * if so, it prints out the correct decls info (a .toString var)
     */
    private void checkForString(DaikonVariableInfo curNode, Class type, String name, String offset)
    {
        if (!type.equals(String.class))
            return;

        String postString = ""; //either array braces or an empty string

        if ((offset + name).contains("[]"))
            postString = "[]";

        outFile.println(offset + name + ".toString");
        outFile.println(stringClassName + postString);
        outFile.println(stringClassName + postString);
        outFile.println(compareInfoDefault);


        // add DaikonVariableInfo type
        DaikonVariableInfo stringInfo = new StringInfo(offset + name + ".toString",
                (offset+name).contains("[]"));
        curNode.addChild(stringInfo);

    }

    /**
     * Checks the given type to see if it requires a .class
     * addition to the decls file.
     * If so, it prints out the correct .class variable info.
     */
    private void checkForRuntimeClass(DaikonVariableInfo curNode, Class type, String name, String offset)
    {
        if (!shouldAddRuntimeClass(type))
            return;

        String postString = ""; //either array braces or an empty string

        if (name.contains("[]") || offset.contains ("[]"))
            postString = "[]";
        // System.out.printf ("offset  = %s, name = %s, postString = '%s'%n",
        //                    offset, name, postString);

        outFile.println(offset + name + ".class");
        outFile.println(classClassName + postString);
        outFile.println(stringClassName + postString);
        outFile.println(compareInfoDefault);

        //add daikoninfo type
        DaikonVariableInfo classInfo = new DaikonClassInfo(offset + name + ".class",
                (offset+name).contains("[]"));
        curNode.addChild(classInfo);
    }

}
