
package daikon.chicory;

import java.lang.reflect.Field;


/**
 */
public class ThisObjInfo extends DaikonInfo
{
    
    public ThisObjInfo()
    {
        super("this");
    }

    /* (non-Javadoc)
     * @see daikon.chicory.DaikonInfo#getChildValue(java.lang.Object)
     */
    public Object getChildValue(Object val)
    {
        return null;
    }

    
}
