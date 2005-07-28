package daikon.chicory;

import java.lang.reflect.*;
import java.lang.reflect.Field;
import java.util.*;


/**
 * The OjbectInfo class is a subtype of DaikonVariableInfo used for variable types which are
 * class fields.
 */
public class FieldInfo extends DaikonVariableInfo
{
    /** The corresponding Field **/
    private Field field;

    public FieldInfo(String theName, Field theField, boolean isArr)
    {
       super(theName, isArr);
       field = theField;
    }

    public FieldInfo (String parent_name, Field theField)
    {
        this (parent_name + "." + theField.getName(), theField,
              theField.getType().isArray());
    }

    public Object getMyValFromParentVal(Object val)
    {
        if(isArray)
        {
            return DTraceWriter.getFieldValues(field, (List <Object>) val);
        }
        else
        {
            if(Modifier.isStatic(field.getModifiers()))
            {
                return DTraceWriter.getStaticValue(field);
            }
            else
            {
                return DTraceWriter.getValue(field, val);
            }
        }
    }

    /** Processes any children of this field and adds them to the tree **/
    public void process (int depth, boolean in_arr)
    {
        process_children (field.getType(), depth, in_arr);
    }
}
