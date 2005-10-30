package daikon.chicory;

/**
 * The ThisObjInfo class is a subtype of DaikonVariableInfo used for
 * variable types which represent the "this" object.s
 */
public class ThisObjInfo extends DaikonVariableInfo
{
    public Class type;

    public ThisObjInfo()
    {
        super("this");
    }

    public ThisObjInfo (Class type)
    {
        super ("this");
        this.type = type;
    }

    /* (non-Javadoc)
     * @see daikon.chicory.DaikonVariableInfo#getChildValue(java.lang.Object)
     */
    public Object getMyValFromParentVal(Object val)
    {
        return null;
    }

}
