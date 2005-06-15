/*
 * Created on May 3, 2005
 */

package daikon.chicory;

import java.util.List;

/**
 * The DaikonClassInfo class is a subtype of DaikonInfo used for variable types which
 * represent the runtime type of a variable.
 */
public class DaikonClassInfo extends DaikonInfo
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

    public Object getChildValue(Object value)
    {   
        return value;
    }

    public String getValueString(Object val)
    {
        if(isArray)
        {
            return StringInfo.showStringList(DTraceWriter.getTypeNameList((List) val));
        }
        else
        {
            return getValueStringNonArr(val);
        }
    }
    
	/**
	 * Get a String representation of the given Object's runtime type
	 * @param val The Object whose runtime class we wish to get a String representation of
	 * @return String representation of the given Object's runtime type
	 */
    public String getValueStringNonArr(Object val)
    {
        String valString;

        if (val == null || val instanceof NonsensicalObject)
        {
            valString = "nonsensical\n2";
        }
        else
        {
            // JHP 2/6/05: The removeWrappers call does not appear to be
            // necessary. Also getCanonicalName() returns null in some
            // circumstances, which is incorrect.
            //if (name.equals ("return"))
            //    System.out.printf ("%s.class of %s [%s]\n", name,
            //             val.getClass().getSimpleName(),
            //             removeWrappers(val, type, true));
            // outFile.println("\"" + removeWrappers(val, type, true).getCanonicalName() + "\"");
            valString = ("\"" + DTraceWriter.stdClassName(val.getClass()) + "\"") + "\n1";
        }
        
        return valString;
    }
}
