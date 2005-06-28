/*
 * Created on May 4, 2005
 *
 */
package daikon.chicory;


/**
 * This subclass of DaikonVariableInfo is used as a "placeholder." It contains no variable
 * information directly, but provides useful information to the DTraceWriter during
 * traversal.
 */
public class HolderInfo extends DaikonVariableInfo
{

    public HolderInfo()
    {
        super(null);
    }

    /* (non-Javadoc)
     * @see daikon.chicory.DaikonVariableInfo#getChildValue(java.lang.Object)
     */
    public Object getMyValFromParentVal(Object val)
    {
        return null;
    }

}
