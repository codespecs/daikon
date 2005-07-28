package daikon.chicory;

import java.lang.reflect.Field;

/**
 *  The ArrayInfo class is a subtype of DaikonVariableInfo used for
 * variable types which are arrays (ie, their name ends with "[]").
 */
public class ArrayInfo extends DaikonVariableInfo
{
    /** Component type of the array **/
    Class array_type = null;

    /**
     * Constructs an ArrayInfo object with the specified name
     * @param theName The variable name. Should end with "[]"
     */
    public ArrayInfo(String theName)
    {
        super(theName, true);
    }

    public ArrayInfo (String parent_name, Class array_type) {

        super (parent_name + "[]", true);
        this.array_type = array_type;
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

    /** Processes any children of this array and adds them to the tree **/
    public void process (int depth)
    {
        process_children (array_type, depth, true);
    }


}
