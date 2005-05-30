/*
 * Created on May 3, 2005
 *
 */
package daikon.chicory;

import java.lang.reflect.*;
import java.util.List;


/**
 * @author Eric Fellheimer
 *
 */
public class ListInfo extends DaikonInfo
{

    private Class listType;
    
    /**
     * @param theName
     */
    public ListInfo(String theName, Class theType)
    {
        super(theName, true);
        listType = theType;
    }

    
    public Object getChildValue(Object value)
    {
        Method arrayMethod = null;
        try
        {
            arrayMethod = listType.getMethod("toArray", new Class[0]);
        }
        catch (NoSuchMethodException e)
        {
            throw new Error(listType.getName() + " seems to implement java.util.List, but method toArray() not found");
        }

        Object arrayVal = null;

        if (value != null && !(value instanceof NonsensicalObject))
        {

            try
            {
                arrayVal = arrayMethod.invoke(value, new Object[0]);
            }
            catch (IllegalArgumentException e1)
            {
                throw new Error(e1);
            }
            catch (IllegalAccessException e1)
            {
                throw new Error(e1);
            }
            catch (InvocationTargetException e1)
            {
                throw new Error(e1);
            }
        }
        else
            arrayVal = NonsensicalObject.getInstance();


        return DTraceWriter.getListFromArray(arrayVal);
    }
}
