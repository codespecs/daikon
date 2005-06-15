
package daikon.chicory;

import java.lang.reflect.Field;


/**
 * The ThisObjInfo class is a subtype of DaikonInfo used for variable types which
 * represent the "this" object.s
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
