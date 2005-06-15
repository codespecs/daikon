
package daikon.chicory;


/**
 * The ArgInfo class is a subtype of DaikonInfo used for variable types which are 
 * arguments to procedures.
 */
public class ArgInfo extends DaikonInfo
{

    /**
     * Constructs an ArgInfo object with the specified name
     * @param theName The variable name (used in the decl file)
     */
    public ArgInfo(String theName)
    {
        super(theName);
    }
    
    public Object getChildValue(Object value)
    {
        return null;
    }

}
