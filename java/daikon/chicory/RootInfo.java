package daikon.chicory;

import java.lang.reflect.*;

/**
 *  This is a subtype of DaikonVariableInfo and is used as a
 *  "placeholder" for the root of the tree.  It contains no variable
 *  information other than what is stored in its children.
 */
public class RootInfo extends DaikonVariableInfo
{
    public RootInfo()
    {
        //the root needs no name
        super(null);
    }

    public Object getMyValFromParentVal(Object value)
    {
        return null;
    }

    /**
     * Adds the children for a method entry point.  These are 'this'
     * and the parameters
     */
    public void enter_process (MethodInfo mi, int depth)
    {
        // If this is not a constructor, add the class and its children
        if (!(mi.member instanceof Constructor))
        {
            ThisObjInfo this_obj = new ThisObjInfo (mi.class_info.clazz);
            addChild (this_obj);
            this_obj.process (depth);
        }

        // Add each parameter
        for (int ii = 0; ii < mi.arg_names.length; ii++) {
            ParameterInfo pi = new ParameterInfo (mi, ii);
            addChild (pi);
            pi.process (depth);
        }
    }

    /**
     * Adds the children for a method exit point.  These are 'this',
     * the parameters, and the return value
     */
    public void exit_process (MethodInfo mi, int depth)
    {
        // Add the class and its children
        ThisObjInfo this_obj = new ThisObjInfo (mi.class_info.clazz);
        addChild (this_obj);
        this_obj.process (depth);

        // Add each parameter
        for (int ii = 0; ii < mi.arg_names.length; ii++) {
            ParameterInfo pi = new ParameterInfo (mi, ii);
            addChild (pi);
            pi.process (depth);
        }

        // Add the return value (if not a constructor and not void)
        if (mi.member instanceof Method)
        {
            Class return_type = ((Method) mi.member).getReturnType();
            if (!return_type.equals (Void.TYPE))
            {
                ReturnInfo ret_info = new ReturnInfo (return_type);
                addChild (ret_info);
                ret_info.process (depth);
            }
        }
    }

}
