package daikon.chicory;

import java.lang.reflect.*;
import java.lang.reflect.Field;
import java.util.*;


/**
 * The OjbectInfo class is a subtype of DaikonVariableInfo used for
 * variable types which are class fields.
 */
public class FieldInfo extends DaikonVariableInfo
{
    /** The corresponding Field **/
    private Field field;

    /** The offset of this field in its containing class **/
    private int field_num;

    public FieldInfo(String theName, Field theField, boolean isArr)
    {
       super(theName, isArr);
       field = theField;

       // Calculate the offset of this field in its class
       field_num = 0;
       for (Field f : field.getDeclaringClass().getDeclaredFields())
       {
           if (f.equals (field))
               return;
           if (f.getType().isPrimitive())
               field_num++;
       }
       assert false : "Can't find " + field + " in "+field.getDeclaringClass();
    }

    /**
     * Returns true iff the corresponding field is static.
     */
    public boolean isStatic()
    {
        return Modifier.isStatic(field.getModifiers());
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

    public Field getField()
    {
        return field;
    }

    public Class getType()
    {
        return (field.getType());
    }

    public int get_field_num()
    {
        return (field_num);
    }
}
