package daikon.chicory;

import java.lang.reflect.*;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import daikon.Chicory;

/**
 *  This is a subtype of DaikonVariableInfo and is used as a
 *  "placeholder" for the root of the tree.  It contains no variable
 *  information other than what is stored in its children.
 */
public class RootInfo extends DaikonVariableInfo
{
    private RootInfo()
    {
        //the root needs no name
        super(null);
    }

    @Override
    public Object getMyValFromParentVal(Object value)
    {
        return null;
    }

    /**
     * Creates a RootInfo object for a method entry program point.
     */
    public static RootInfo enter_process (MethodInfo mi, int depth)
    {
        RootInfo root = new RootInfo();

        // Don't build a tree for class initializers.
        if (mi.is_class_init())
            return (root);

        Set<Class> staticTraversedClasses = null;
        if(Chicory.shouldWatchStatics())
        {
            staticTraversedClasses = new HashSet <Class> ();
        }

        root.addParameters(mi.class_info, mi.member,
                           Arrays.asList(mi.arg_names), /*offset = */ "",
                           depth, staticTraversedClasses);

        if (!(mi.member instanceof Constructor))
            root.addClassVars(mi.class_info,
                              Modifier.isStatic(mi.member.getModifiers()),
                              mi.member.getDeclaringClass(), /*offset = */ "",
                              depth, staticTraversedClasses);

        return root;
    }

    /**
     * Creates a RootInfo object for a method exit program point.
     */
    public static RootInfo exit_process(MethodInfo mi, int depth)
    {
        RootInfo root = new RootInfo();

        // Don't build a tree for class initializers.
        if (mi.is_class_init())
            return (root);

        Set<Class> staticTraversedClasses = null;
        if(Chicory.shouldWatchStatics())
        {
            staticTraversedClasses = new HashSet <Class> ();
        }

        // Print arguments
        root.addParameters(mi.class_info, mi.member, Arrays.asList(mi.arg_names), /*offset = */ "",
                depth, staticTraversedClasses);

        // Print return type information for methods only and not constructors
        if (mi.member instanceof Method)
        {
            Class returnType = ((Method) mi.member).getReturnType();
            if (!(returnType.equals(Void.TYPE)))
            {
                // add a new ReturnInfo object to the traversal tree
                DaikonVariableInfo retInfo = new ReturnInfo();

                retInfo.typeName = stdClassName(returnType);
                retInfo.repTypeName = getRepName(returnType, false);
                root.addChild(retInfo);

                retInfo.checkForDerivedVariables(returnType, "return", "");

                retInfo.addChildNodes(mi.class_info, returnType, "return", "",
                        depth, staticTraversedClasses);
            }
        }

        // Print class variables
        root.addClassVars(mi.class_info,
                Modifier.isStatic(mi.member.getModifiers()), mi.member
                        .getDeclaringClass(), "", depth, staticTraversedClasses);

        return root;
    }

    /**
     * Creates a RootInfo object for an object program point.
     * This will include the class' fields and the "this" object.
     */
    public static RootInfo getObjectPpt(ClassInfo cinfo, int depth)
    {
        RootInfo root = new RootInfo();

        Set<Class> staticTraversedClasses = null;
        if(Chicory.shouldWatchStatics())
        {
            staticTraversedClasses = new HashSet <Class> ();
        }

        root.addClassVars(cinfo, /*dontPrintInstanceVars = */ false,
                cinfo.clazz, /*offset = */ "", depth, staticTraversedClasses);

        return root;
    }

    /**
     * Creates a RootInfo object for a class program point.
     * This will just include static fields.
     */
    public static RootInfo getClassPpt(ClassInfo cinfo, int depth)
    {
        RootInfo root = new RootInfo();

        Set<Class> staticTraversedClasses = null;
        if(Chicory.shouldWatchStatics())
        {
            staticTraversedClasses = new HashSet <Class> ();
        }

        root.addClassVars(cinfo, /*dontPrintInstanceVars = */ true,
                cinfo.clazz, /*offset = */ "", depth, staticTraversedClasses);

        return root;
    }

}
