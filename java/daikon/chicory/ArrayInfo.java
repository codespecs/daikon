package daikon.chicory;

import java.lang.reflect.Field;
import java.util.List;


/**
 *  The ArrayInfo class is a subtype of DaikonInfo used for variable types which are 
 * arrays (ie, their name ends with "[]").
 */
public class ArrayInfo extends DaikonInfo
{
	/**
	 * Constructs an ArrayInfo object with the specified name
	 * @param theName The variable name. Should end with "[]"
	 */
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
		//the "child" value of an array is the actual list of array values
		//as opposed to just the "hashcode" object
        else
            return DTraceWriter.getListFromArray(value);
    }
   

}
