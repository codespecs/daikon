package daikon.chicory;

import java.io.*;
import java.util.*;
import java.lang.reflect.*;

import sun.misc.Unsafe;

/**
 * 
 * DeclWriter writes the .decls file to a stream.  As it does this, it
 * constructs traversal pattern trees (see {@link DaikonInfo})  for eachs
 * program point.  These are later used by the {@link DTraceWriter}.
 *
 */
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

    /**default string for comparability info**/
    private static final String compareInfoDefault = "22";

    /**header string before each new method entry or exit point**/
    public static final String declareHeader = "DECLARE";

    /**used to assert that a given variable is a parameter to a method**/
    private static final String isParamString = " # isParam=true";

    /**stores a mapping from methods to all of the method's exit Locations**/
    private Map <Method, Set <Integer>> methodToExitLocs;

    /**map method to its argument names**/
    private Map <Method, List<String>> methodToArgNames;

    /**keep track of Ppts we've already emitted**/
    private Set <String> emittedPpts;

    /**stream to write to**/
    private PrintStream outFile;

    /**levels of recursion**/
    protected int daikonDepth;

    //certain class names
    protected static final String classClassName = "java.lang.Class";
    protected static final String stringClassName = "java.lang.String";

    /**
     * Constructs a DeclWriter, preparing it to receive messages.
     *
     * @param writer
     *            Stream to write to
     * @param depth
     *            Depth of tree recursion to traverse for each variable
     * @param excludes
     *            Formatted strings can include wildcards
     * @param includes
     *		     Include only list
     */
    public DeclWriter(PrintStream writer, int depth, String[] excludes,
                      List includes)
    {
        super(excludes, includes);

        outFile = writer;
        
        if (depth <= 0)
            throw new RuntimeException("Daikon depth must be positive");
        
        daikonDepth = depth;

        methodToExitLocs = new HashMap <Method, Set<Integer> > ();
        methodToArgNames = new HashMap <Method, List<String> >();
        emittedPpts = new HashSet <String> ();
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
        outFile.print(DaikonWriter.lineSep + DaikonWriter.lineSep);

        outFile.println("VarComparability");
        outFile.println("none" + DaikonWriter.lineSep);

        outFile.println("ListImplementors");
        outFile.println("java.util.List" + DaikonWriter.lineSep + DaikonWriter.lineSep);
    }

    /**
     * Register an exit point for a method or constructor.  If you're printing
     * decls in whole-class mode, you should call this before printDeclClass 
     * for every exit point, otherwise no method exit Ppt decls will get generated.
     */
    public void registerExit(Member method, int exitLoc)
    {
        Set<Integer> exitLocs = methodToExitLocs.get(method);
        //create new set for this method if it does not exist yet
        if (exitLocs == null)
        {
            exitLocs = new HashSet<Integer> ();
            methodToExitLocs.put((Method) method, exitLocs);
        }
        exitLocs.add(new Integer(exitLoc));
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

			//initialize the root node for this program point
            RootInfo entryRoot;
            entryRoot = new RootInfo();
            mi.traversalEnter = entryRoot;
            printMethodEntryInternal(cinfo, entryRoot, member, methodEntryName(member), argnames);

            //print exit program point for EACH exit location in the method
            //(that was encountered during this execution of the program)
            Set<Integer> theExits = new HashSet<Integer>(mi.exit_locations);
            for (Integer exitLoc : theExits)
            {
				//initialize root node for this exit program point
                RootInfo exitRoot;
                exitRoot = new RootInfo();
                mi.traversalExit.put(exitLoc, exitRoot);
                String name = methodExitName(member, exitLoc.intValue());
                printMethodExitInternal(cinfo, exitRoot, member, name, argnames);
            }
        }

        printClassPpt (cinfo, cinfo.clazz, cinfo.class_name + ":::CLASS");
        printObjectInternal(cinfo, cinfo.clazz, classObjectName(cinfo.clazz));
    }

    /**
     * Prints the decls info for a method/constructor entry, if the decl
     * has not already been emitted.  This can be called externally to print
     * Ppt decls one-at-a-time.
     */
    public void printMethodEntry(ClassInfo cinfo, DaikonInfo curNode, Member method, List argnames)
    {
        String name = methodEntryName(method);
        if (emittedPpts.contains(name))
            return;
        emittedPpts.add(name);
        if (!Modifier.isStatic(method.getModifiers()))
            printObject(cinfo, method.getDeclaringClass());
        printMethodEntryInternal(cinfo, curNode, method, name, argnames);
    }

    private void printMethodEntryInternal(ClassInfo cinfo, DaikonInfo curNode, Member method, String name, List argnames)
    {
        outFile.println(declareHeader);
        outFile.println(name);
		
        //don't print class vars at method entries for constructors
        printMethod(cinfo, curNode, method, argnames);
		
        outFile.println();
    }

    /**
     * Prints the decls info for a method/constructor exit, if the decl
     * has not already been emitted.  This can be called externally to print
     * Ppt decls one-at-a-time.
     */
    public void printMethodExit(ClassInfo cinfo, DaikonInfo curNode, Member method, int exitLoc, List argnames)
    {
        String name = methodExitName(method, exitLoc);
        if (emittedPpts.contains(name))
            return;
        emittedPpts.add(name);
        printMethodExitInternal(cinfo, curNode, method, name, argnames);
    }

    private void printMethodExitInternal(ClassInfo cinfo, DaikonInfo curNode, Member method, String name, List argnames)
    {
        outFile.println(declareHeader);
        outFile.println(name);
		
        // Print arguments
        printLocalVars(curNode, method, argnames, "", daikonDepth);

        //print return type information for methods only and not constructors
        if (method instanceof Method)
        {
            Class returnType = ((Method) method).getReturnType();
            if (!(returnType.equals(Void.TYPE)))
            {
                outFile.println("return");
                outFile.println (stdClassName (returnType));
                outFile.println(getRepName(returnType, false));
                outFile.println(compareInfoDefault);
             
				//add a new ReturnInfo object to the traversal pattern
                DaikonInfo retInfo = new ReturnInfo();
                curNode.addChild(retInfo);

                checkForListDecl(retInfo, returnType, "return", "", false);
                checkForImplicitList(returnType, "return", "", daikonDepth);
                checkForRuntimeClass(retInfo, returnType, "return", ""); //.class var
                checkForString(retInfo, returnType, "return", "");

                exploreFurtherDepth(returnType, retInfo, "return", "", daikonDepth, false);
            }
        }

        // Print class variables
        printClassVars( cinfo, curNode, Modifier.isStatic(method.getModifiers()),
                        method.getDeclaringClass(), "", daikonDepth, false);

        outFile.println();
    }

    //print the Object Ppt decl.
    private void printObject(ClassInfo cinfo, Class type)
    {
        String name = classObjectName(type);
        if (emittedPpts.contains(name))
            return;
        emittedPpts.add(name);
        printObjectInternal(cinfo, type, name);
    }

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
            if (Modifier.isStatic(f.getModifiers()))
                static_fields++;
        if (static_fields == 0)
            return;

        outFile.println (declareHeader);
        outFile.println (name);

        for (int i = 0; i < fields.length; i++)
        {
            Field classField = fields[i];

			//only process static fields
            if (!Modifier.isStatic(classField.getModifiers()))
                continue;

            if (!isFieldVisible (current_class, classField))
            {
				//skip if not visible (see isFieldVisible)
                continue;
            }

            Class fieldType = classField.getType();
            StringBuffer buf = new StringBuffer();
			
			//don't worry about populating traversal pattern here...
			//just pass in new RootInfo()
            DaikonInfo newChild = printDeclVar(new RootInfo(), classField, "", daikonDepth,
                                            false, buf);
            String newOffset = buf.toString();
            exploreFurtherDepth(fieldType, newChild, classField.getName(), newOffset,
                                 daikonDepth, false);
        }

        outFile.println();
    }


	/**
	 *    prints the decls info for a method
	 *    called by printMethodEntry (printMethodExit does this directly so it
	 *    can place the return after the arguments to match dfej)
	 */
    private void printMethod(ClassInfo cinfo, DaikonInfo curNode, Member method, 
            List argnames)
    {
        boolean shouldPrintClass = !(method instanceof Constructor);
        
        printLocalVars(curNode, method, argnames, "", daikonDepth);
        if (shouldPrintClass)
            printClassVars(cinfo, curNode, Modifier.isStatic(method.getModifiers()), method.getDeclaringClass(), "", daikonDepth, false);
    }

    /**
     * Print local variables (the parameters) of a method
     */
    private void printLocalVars(DaikonInfo curNode, Member method, List argnames, String offset, int depth)
    {
        Class[] arguments = (method instanceof Constructor) ? ((Constructor) method).getParameterTypes() : ((Method) method).getParameterTypes();
        Iterator argnamesiter = argnames.iterator();
        for (int i = 0; (i < arguments.length) && argnamesiter.hasNext(); i++)
        {
            Class type = arguments[i];
            String name = (String) argnamesiter.next();
            DaikonInfo theChild = printDeclVar(curNode, type, name, offset, depth);
            exploreFurtherDepth(type, theChild, name, offset, depth, false);
        }
    }

    /**
     * Print class variables (ie, the fields) for the given type
     */
    private void printClassVars(ClassInfo cinfo, DaikonInfo curNode, boolean dontPrintInst, Class type, String offset, int depth, boolean inArray)
    {        
		//DaikonInfo corresponding to the "this" object
        DaikonInfo thisInfo;
        
        //must be first level of recursion to print "this" field
        if (!dontPrintInst && offset.equals(""))
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
                
                DaikonInfo thisClass = new DaikonClassInfo("this.class", false);
                thisInfo.addChild(thisClass);
            }
        }
        else
            thisInfo = curNode;
        

        if (shouldFilterClass(type.getName()))
        {
            return;
        }

        Field[] fields = type.getDeclaredFields();
        for (int i = 0; i < fields.length; i++)
        {
            Field classField = fields[i];

            if (!Modifier.isStatic(classField.getModifiers()) && dontPrintInst)
                continue;

            //don't print arrays of the same static field
            if(Modifier.isStatic(classField.getModifiers()) && inArray)
                continue;

            if (!isFieldVisible (current_class, classField))
            {
                continue;
            }

            Class fieldType = classField.getType();
            
            StringBuffer buf = new StringBuffer();
            DaikonInfo newChild = printDeclVar(thisInfo, classField, offset, depth, inArray, buf);
            String newOffset = buf.toString();
            exploreFurtherDepth(fieldType, newChild, classField.getName(), newOffset, depth, inArray);
        }
    }

    /**
     *     Explores the traversal pattern tree one level deeper (see {@link DaikonInfo}).
     *     For example: "recurse" on a hashcode array object to print the actual array of values
     *     or recurse on hashcode variable to print its fields.
     *     Also accounts for derived variables (.class, .tostring). 
     */
    private void exploreFurtherDepth(Class type, DaikonInfo curNode, String name, String offset,
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
     
                DaikonInfo newChild = new ArrayInfo(offset + name + "[]");
                curNode.addChild(newChild);
            }
            // multi-dimensional arrays (not currently used)
            else if (arrayType.isArray())
            {
                outFile.println(offset + name + "[]");
                outFile.println(arrayType.getName() + "[]");
                outFile.println(getRepName(arrayType, true) + "[]");
                outFile.println(compareInfoDefault); //no comparability info yet
                
                DaikonInfo newChild = new ArrayInfo(offset + name + "[]");
                curNode.addChild(newChild);
                
                exploreFurtherDepth(arrayType, newChild, "", offset + name + "[]", depthRemaining, true);
            }
			//regular class (while in array)
            else
            {
                outFile.println(offset + name + "[]");
                outFile.println(arrayType.getName() + "[]");
                outFile.println(getRepName(arrayType, true) + "[]");
                outFile.println(compareInfoDefault); //no comparability info yet
                
                DaikonInfo newChild = new ArrayInfo(offset + name + "[]");
                curNode.addChild(newChild);

                // print out the class of each element in the array.  For
                // some reason dfej doesn't include this on returned arrays
                // or arguments.  We attempt to get at arguments because
                // they don't have an offset (no this pointer)
                if (!name.equals ("return") && !offset.equals (""))
                  checkForRuntimeClass (newChild, type, name + "[]", offset);

                if (!shouldFilterClass(arrayType.getName()))
                {
                    checkForString(newChild, arrayType, name + "[]", offset);
                    printClassVars(null, newChild, false, arrayType, offset + name + "[].", depthRemaining - 1, true);
                }
                else
                {
                    //.class vars
                    checkForRuntimeClass(newChild, type, name + "[]", offset);
                    checkForString(newChild, arrayType, name + "[]", offset);
                }
            }
        }
        // regular old class type
        else
        {
            if (depthRemaining <= 0)
            {
                //don't recurse any more!
                return;
            }
            if (notSystemClass (type))
                printClassVars(null, curNode, false, type, offset + name + ".", depthRemaining - 1,
                               inArray);
        }
    }

    /**
     * Prints the decl info for a single local variable (usually a method argument)
     * @return The newly created DaikonInfo object, whose parent is curNode
     */
    private DaikonInfo printDeclVar(DaikonInfo curNode, Class type, String name, String offset, int depth)
    {
        outFile.println(offset + name);
        outFile.print(stdClassName(type));
        outFile.print(isParamString);
        outFile.print("\n");
        outFile.println(getRepName(type, false));
        outFile.println(compareInfoDefault); //no comparability info right now
        
        //traversal
        DaikonInfo newChild = new ParameterInfo(offset + name);
        curNode.addChild(newChild);
            

        checkForListDecl(newChild, type, name, offset, false); //implements java.util.List
        checkForImplicitList(type, name, offset, depth); 
        checkForRuntimeClass(newChild, type, name, offset); //.class var
        checkForString(newChild, type, name, offset); //.tostring var
        
        return newChild;
    }

    /**
     *
     *Determines if type has exactly 1 non-static field of same type
     *(implicit list) and prints asociated decls.
     *
     **/
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

            //TODO get actual DaikonInfo here
            exploreFurtherDepth(type, new RootInfo(), name, offset, depth - 1, true);
        }
    }

	/**
	 * Determines if type implements list
	 * and prints associated decls, if necessary
	 */
    private void checkForListDecl(DaikonInfo curNode, Class type, String name, String offset,
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
            
            
            DaikonInfo child = new ListInfo(offset + name + "[]", type);
            curNode.addChild(child);

            //.class var
            outFile.println(offset + name + "[].class");
            outFile.println(classClassName + "[]");
            outFile.println(stringClassName + "[]");
            outFile.println(compareInfoDefault);
            
            DaikonInfo childClass = new DaikonClassInfo(offset + name + "[].class", true);
            curNode.addChild(childClass);
        }
    }

    /**
     * Prints the decl info for a single class variable (a field)
     */
    private DaikonInfo printDeclVar(DaikonInfo curNode, Field field, String offset, int depth,
                                boolean inArray, StringBuffer buf)
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

        // Print declared type, followed by auxiliary information
        outFile.print (type_name + arr_str);

        // Print auxiliary information
        appendAuxInfo(field);

        outFile.println();

        outFile.print(getRepName(type, inArray) + arr_str);
        
        
        //don't put info directly off the root
        if(curNode instanceof RootInfo)
        {
            DaikonInfo holder = new HolderInfo();
            curNode.addChild(holder);
            curNode = holder;
        }
        
        if (Modifier.isStatic(modifiers) && Modifier.isFinal(modifiers)
            && type.isPrimitive() && !inArray)
        {
            ClassInfo cinfo = Runtime.getClassInfoFromClass(field.getDeclaringClass());
            String value = cinfo.staticMap.get(name);

            if(value != null)
            {
                outFile.print(" = " + value);
            }
            //else
            //{
                //don't print anything
                //because this field wasn't declared with an actual "hardcoded" constant   
            //}
            
            //in this case, we don't want to print this variable to the dtrace file
            curNode = new RootInfo();
        }
        
        outFile.println();
        outFile.println(compareInfoDefault); //no comparability info right now

        
        DaikonInfo newField = new ObjectInfo(offset + name, field, inArray);
        curNode.addChild(newField);

        checkForListDecl(newField, type, name, offset, inArray);
        checkForImplicitList(type, name, offset, depth);
        checkForRuntimeClass(newField, type, name, offset); //.class var
        checkForString(newField, type, name, offset);
        
        buf.append(offset);
        
        return newField;
    }

    // Appends as auxiliary information:
    // the package name of the declaring class
    // whether the type is static
    private void appendAuxInfo(Field field) {
        int modifiers = field.getModifiers();

        Package p = field.getDeclaringClass().getPackage();
        String pkgName = (p == null ? null : p.getName());

        //System.out.printf("Package name for type  %s is %s\n", type, pkgName);

        String staticString = (Modifier.isStatic(modifiers)) ? "true" : "false";
        outFile.print(" # ");
        if (pkgName != null) {
            outFile.print("declaringClassPackageName=" + pkgName + ", ");
        }
    }


	/**
	 * checks the given type to see if it is a string
	 * if so, it prints out the correct decls info (a .toString var)
	 */
    private void checkForString(DaikonInfo curNode, Class type, String name, String offset)
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
        
        
        //add daikoninfo type
        DaikonInfo stringInfo = new StringInfo(offset + name + ".toString",
                (offset+name).contains("[]"));
        curNode.addChild(stringInfo);

    }

    /**
     * checks the given type to see if it requires a .class
     * addition to the decls file
     * if so, it prints out the correct .class variable info
     */
    private void checkForRuntimeClass(DaikonInfo curNode, Class type, String name, String offset)
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
        
        //add daikoninfo type
        DaikonInfo classInfo = new DaikonClassInfo(offset + name + ".class",
                (offset+name).contains("[]"));
        curNode.addChild(classInfo);
    }

}
