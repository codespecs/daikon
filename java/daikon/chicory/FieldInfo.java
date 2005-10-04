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
       Class clazz = field.getDeclaringClass();
       if (!field.getType().isPrimitive() || clazz.isInterface()) {
           field_num = -1;
           return;
       }
       field_num = num_prim_fields (clazz.getSuperclass());
       for (Field f : clazz.getDeclaredFields())
       {
           if (f.equals (field)) {
               // System.out.printf ("field %s has field num %d\n", field,
               //                   field_num);
               return;
           }
           if (Modifier.isStatic(f.getModifiers()))
               continue;
           if (f.getType().isPrimitive())
               field_num++;
       }
       assert false : "Can't find " + field + " in "+field.getDeclaringClass();
    }

    /**
     * Return the number of primitive fields in clazz and all of its
     * superclasses
     */
    public static int num_prim_fields (Class clazz)
    {
        if (clazz == Object.class)
            return 0;
        else
        {
            int field_cnt = num_prim_fields (clazz.getSuperclass());
            for (Field f : clazz.getDeclaredFields())
            {
                if (Modifier.isStatic(f.getModifiers()))
                    continue;
                if (f.getType().isPrimitive())
                    field_cnt++;
            }
            return (field_cnt);
        }
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
            return DTraceWriter.getFieldValues(field, (List <Object>) val); // unchecked cast
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

    Field tag_field = null;
    public Field get_tag_field (String tag_field_name, Class parent_class)
    {
        if (tag_field == null)
        {
            try {
                tag_field = parent_class.getDeclaredField (tag_field_name);
            } catch (Exception e) {
                throw new Error ("can't get field " + tag_field_name + " in "
                                 + parent_class, e);
            }
        }
        return tag_field;
    }
}
