package daikon.chicory;

import java.io.*;
import java.util.*;
import java.lang.reflect.*;


/**
 *
 * DeclWriter writes the .decls file to a stream.  It
 * uses traversal pattern trees (see {@link DaikonVariableInfo})  for each
 * program point.  These are also used by the {@link DTraceWriter}.
 *
 */
public class DeclWriter extends DaikonWriter
{
    // Notes:
    //
    //  Class.getName() returns JVM names (eg, [Ljava.lang.String;)


    /** Header string before each new method entry or exit point **/
    public static final String declareHeader = "DECLARE";

    /** Stream to write to **/
    private PrintStream outFile;


    /**
     * Constructs a DeclWriter, preparing it to receive messages.
     *
     * @param writer
     *            Stream to write to
     */
    public DeclWriter(PrintStream writer)
    {
        super();
        outFile = writer;
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
        if (Runtime.comp_info != null)
            outFile.println ("implicit");
        else
            outFile.println("none" + DaikonWriter.lineSep);

        outFile.println("ListImplementors");
        outFile.println("java.util.List" + DaikonWriter.lineSep + DaikonWriter.lineSep);
    }

    /**
     * Returns the correctly formulated ":::OBJECT" name of the class
     * (i.e., the program point name)
     *
     * @param type the ClassType type
     * @return the correctly formulated String
     */
    public static String classObjectName(Class type)
    {
        return (type.getName() + ":::OBJECT");
    }


    /**
     * Prints declarations for all the methods in the indicated class.
     * This method is called in Runtime to print decls info for a class.
     *
     * @param cinfo
     *        Class whose declarations should be printed.
     *
     */
    public void printDeclClass (ClassInfo cinfo, DeclReader comp_info)
    {
        // Print all methods and constructors
        for (MethodInfo mi : cinfo.get_method_infos())
        {
            Member member = mi.member;

            // Don't want to instrument these types of methods
            if (!shouldInstrumentMethod(member))
                continue;

            // Gset the root of the method's traversal pattern
            RootInfo enterRoot = mi.traversalEnter;
            assert enterRoot != null : "Traversal pattern not initialized at method " + mi.method_name;

            printMethod(enterRoot, methodEntryName(member), comp_info);

            // Print exit program point for EACH exit location in the method
            // (that was encountered during this execution of the program)
            Set<Integer> theExits = new HashSet<Integer>(mi.exit_locations);
            for (Integer exitLoc : theExits)
            {
                // Get the root of the method's traversal pattern
                RootInfo exitRoot = mi.traversalExit;
                assert enterRoot != null : "Traversal pattern not initialized at method " + mi.method_name;

                printMethod(exitRoot,methodExitName(member,exitLoc.intValue()),
                            comp_info);
            }
        }

        printClassPpt (cinfo, cinfo.class_name + ":::CLASS", comp_info);
        printObjectPpt(cinfo, classObjectName(cinfo.clazz), comp_info);
    }

    /**
     * Prints a method's program point.  This includes the declare header ("DECLARE"),
     * the program point name, and the variable information.
     *
     * This method uses variable information from the traversal tree.
     *
     * @param root The root of the traversal tree.
     * @param name The program point name.
     * @param comp_info Comparability information
     */
    private void printMethod(RootInfo root, String name, DeclReader comp_info)
    {
        outFile.println(declareHeader);
        outFile.println(name);

        for(DaikonVariableInfo childOfRoot: root)
        {
            traverseDecl(childOfRoot, ((comp_info == null) ? null
                                       : comp_info.find_ppt (name)));
        }

        outFile.println();
    }

    /**
     * Prints the .decls information for a single DaikonVariableInfo
     * object, and recurses on its children.  If the current variable has
     * comparability defined in decl_ppt, that comparability is used.
     * Otherwise -1 is used if there is comparability information available
     * and the information in the variable is used if it is not.
     */
    private void traverseDecl(DaikonVariableInfo curInfo,
                              DeclReader.DeclPpt decl_ppt)
    {
        if (!curInfo.declShouldPrint())
            return;

        outFile.println(curInfo.getName());
        outFile.println(curInfo.getTypeName());
        outFile.println(curInfo.getRepTypeName());
        String comp_str = curInfo.getCompareString();
        if (decl_ppt != null)
        {
            comp_str = "-1";
            DeclReader.VarInfo varinfo = decl_ppt.find_var (curInfo.getName());
            if (varinfo != null)
                comp_str = varinfo.get_comparability();
        }
        outFile.println(comp_str);

        // Go through all of the current node's children
        // and recurse
        for (DaikonVariableInfo child : curInfo)
        {
            traverseDecl(child, decl_ppt);
        }

    }

    /**
     * Prints the object program point.  This contains the "this" object and the class' fields.
     */
    private void printObjectPpt(ClassInfo cinfo, String name,
                                DeclReader comp_info)
    {
        outFile.println(declareHeader);
        outFile.println(name);

        RootInfo root = RootInfo.getObjectPpt(cinfo, Runtime.nesting_depth);
        for(DaikonVariableInfo childOfRoot: root)
        {
            traverseDecl(childOfRoot, ((comp_info == null) ? null
                                       : comp_info.find_ppt(name)));
        }

        outFile.println();
    }

    /**
     * Prints the class program point. This contains only
     * the static variables.  If there are no static variables to print,
     * this method does nothing.
     */
    private void printClassPpt (ClassInfo cinfo, String name,
                                DeclReader comp_info)
    {
        boolean printedHeader = false;
        RootInfo root = RootInfo.getClassPpt(cinfo, Runtime.nesting_depth);

        for(DaikonVariableInfo childOfRoot: root)
        {
            // If we are here, there is at least 1 child
            if (!printedHeader)
            {
                outFile.println (declareHeader);
                outFile.println (name);
                printedHeader = true;
            }

            // Should just print out static fields
            traverseDecl(childOfRoot, ((comp_info == null) ? null
                                       : comp_info.find_ppt (name)));
        }

        if (printedHeader)
            outFile.println();
    }
}
