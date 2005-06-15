
package daikon.chicory;


/**
 * The ReturnInfo class is a subtype of DaikonInfo used for variables which are
 * returned from procedures
 */
public class ReturnInfo extends DaikonInfo
{
    public ReturnInfo()
    {
        super("return");
    }
    
    public Object getChildValue(Object value)
    {
        return null;
    }
    
}
