package daikon.chicory;

import java.lang.reflect.*;
import java.lang.reflect.Field;
import java.util.*;
import java.util.ArrayList;


/**
 * The OjbectInfo class is a subtype of DaikonInfo used for variable types which are 
 * class fields.
 */
public class ObjectInfo extends DaikonInfo
{
	/** The corresponding Field **/
    private Field field;
    
    public ObjectInfo(String theName, Field theField, boolean isArr)
    {
       super(theName, isArr);
       field = theField;
    }


    public Object getMyValFromParentVal(Object val)
    {
        if(isArray)
        {
            return DTraceWriter.getFieldValues(field, (List) val);
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
}

