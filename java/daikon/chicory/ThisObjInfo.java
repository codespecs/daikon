
package daikon.chicory;

import java.lang.reflect.Field;


/**
 * The ThisObjInfo class is a subtype of DaikonVariableInfo used for variable types which
 * represent the "this" object.s
 */
public class ThisObjInfo extends DaikonVariableInfo
{
    public ThisObjInfo()
    {
        super("this");
    }

    /* (non-Javadoc)
     * @see daikon.chicory.DaikonVariableInfo#getChildValue(java.lang.Object)
     */
    public Object getMyValFromParentVal(Object val)
    {
        return null;
    }

    
}
