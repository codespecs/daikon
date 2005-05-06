package daikon.chicory;

import java.lang.reflect.Field;
import java.util.List;


/**
 */
public class ArrayInfo extends DaikonInfo
{
    public ArrayInfo(String theName)
    {
        super(theName, true);
    }
    
    public Object getChildValue(Object value)
    {
        if(value == null)
        {
            return null;
        }
        else if(value instanceof NonsensicalObject)
        {
            return NonsensicalList.getInstance();
        }
        else
            return DTraceWriter.getListFromArray(value);
    }
    
    /*public String getValueString(Object value)
    {
        if(!(value instanceof List) && value != null)
        {
            throw new RuntimeException("Invalid runtime type for value in ArrayInfo.getValueString()");
        }
        
        return showList((List)value);
    }*/

}
