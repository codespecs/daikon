package daikon.chicory;

import java.lang.reflect.*;
import java.lang.reflect.Field;
import java.util.*;
import java.util.ArrayList;


/**
 */
public class ObjectInfo extends DaikonInfo
{
    private Field field;
    
    public ObjectInfo(String theName, Field theField, boolean isArr)
    {
       super(theName, isArr);
       field = theField;
    }


    public Object getChildValue(Object val)
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

