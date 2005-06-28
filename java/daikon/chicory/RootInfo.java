
package daikon.chicory;


/**
 *  This is a subtype of DaikonInfo and is used as a "placeholder" for the 
 *  root of the tree.  It contains no variable information other than what is
 *  stored in its children.
 */
public class RootInfo extends DaikonInfo
{
    public RootInfo()
    {
		//the root needs no name
        super(null);
    }
    
    public Object getMyValFromParentVal(Object value)
    {
        return null;
    }
}
