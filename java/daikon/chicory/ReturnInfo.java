
package daikon.chicory;


/**
 * The ReturnInfo class is a subtype of DaikonVariableInfo used for variables which are
 * returned from procedures
 */
public class ReturnInfo extends DaikonVariableInfo
{
    public ReturnInfo()
    {
        super("return");
    }
    
    public Object getMyValFromParentVal(Object value)
    {
        return null;
    }
    
}
