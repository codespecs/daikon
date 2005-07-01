package daikon.chicory;


/**
 *  The ArrayInfo class is a subtype of DaikonVariableInfo used for variable types which are
 * arrays (ie, their name ends with "[]").
 */
public class ArrayInfo extends DaikonVariableInfo
{
    /**
     * Constructs an ArrayInfo object with the specified name
     * @param theName The variable name. Should end with "[]"
     */
    public ArrayInfo(String theName)
    {
        super(theName, true);
    }

    public Object getMyValFromParentVal(Object value)
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
