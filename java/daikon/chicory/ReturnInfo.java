
package daikon.chicory;


/**
 * The ReturnInfo class is a subtype of DaikonVariableInfo used for variables which are
 * returned from procedures
 */
public class ReturnInfo extends DaikonVariableInfo
{
    Class return_type = null;

    public ReturnInfo()
    {
        super("return");
    }

    public ReturnInfo (Class return_type)
    {
        super("return");
        this.return_type = return_type;
    }

    public Object getMyValFromParentVal(Object value)
    {
        throw new RuntimeException("Don't call getMyValFromParentVal on ReturnInfo objects");
    }

    /** Processes any children of the return and adds them to the tree **/
    public void process (int depth)
    {
        process_children (return_type, depth, false);
    }
}
