/*
 * Created on May 3, 2005
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */

package daikon.chicory;

import java.util.List;

/**
 * @author Eric Fellheimer
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class DaikonClassInfo extends DaikonInfo
{

    /**
     * @param theName
     */
    public DaikonClassInfo(String theName, boolean isArr)
    {
        super(theName, isArr);
        // TODO Auto-generated constructor stub
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
