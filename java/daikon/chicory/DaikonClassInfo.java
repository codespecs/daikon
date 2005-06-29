/*
 * Created on May 3, 2005
 */

package daikon.chicory;

import java.util.List;

/**
 * The DaikonClassInfo class is a subtype of DaikonVariableInfo used for variables which
 * represent the runtime type of a variable.
 */
public class DaikonClassInfo extends DaikonVariableInfo
{

    /**
     * Constructs a DaikonClassInfo object
     * @param theName The name of the variable
     * @param isArr True iff the variable represents an array of runtime classes
     */
    public DaikonClassInfo(String theName, boolean isArr)
    {
        super(theName, isArr);
    }

	//.class variables are derived, so just keep the parent value
    public Object getMyValFromParentVal(Object value)
    {   
        return value;
    }

    public String getDTraceValueString(Object val)
    {
        if(isArray)
        {
			//a list of the runtime type of each value in the array
            return StringInfo.showStringList(DTraceWriter.getTypeNameList((List) val));
        }
        else
        {
            return getValueStringNonArr(val);
        }
    }
    
	/**
	 * Get a String representation of the given Object's runtime type and the
	 * corresponding "modified" value
	 * @param val The Object whose runtime class we wish to get a String representation of
	 * @return String representation of the given Object's runtime type
	 */
    public String getValueStringNonArr(Object val)
    {
        String valString;

        if (val == null || val instanceof NonsensicalObject)
        {
            valString = "nonsensical" + DaikonWriter.lineSep +"2";
        }
        else
        {
            valString = ("\"" + DTraceWriter.stdClassName(val.getClass()) + "\"") + DaikonWriter.lineSep + "1";
        }
        
        return valString;
    }
}
