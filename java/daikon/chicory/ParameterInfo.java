
package daikon.chicory;


/**
 * The ParameterInfo class is a subtype of DaikonVariableInfo used for variable types which are 
 * arguments to procedures.
 * 
 * This class takes "precedence" over when a seeming conflict could arise.  For instance, consider:
 * public static sort(int array[]).  Is the "array" parameter represented as an ParameterInfo object
 * or an ArrayInfo object?  Because ParameterInfo takes precedence, it is the former.  This makes sense
 * because the arrays are really treated as hash codes at the first level, so such a parameter needs
 * no array-specific processing (at this level of the tree at least).
 */
public class ParameterInfo extends DaikonVariableInfo
{

    /**
     * Constructs an ParameterInfo object with the specified name
     * @param theName The variable name (used in the decl file)
     */
    public ParameterInfo(String theName)
    {
        super(theName);
    }
    
    public Object getMyValFromParentVal(Object value)
    {
        //a parameter has no parent value
        assert false : "Parameters have no parent value";
        throw new RuntimeException("Parameters have no parent value");
    }

}
