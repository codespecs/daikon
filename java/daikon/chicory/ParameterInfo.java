package daikon.chicory;

import java.lang.reflect.*;
import java.util.*;

/**
 * The ParameterInfo class is a subtype of DaikonVariableInfo used for
 * variable types which are arguments to procedures.
 *
 * This class takes "precedence" over when a seeming conflict could
 * arise.  For instance, consider: public static sort(int array[]).
 * Is the "array" parameter represented as an ParameterInfo object or
 * an ArrayInfo object?  Because ParameterInfo takes precedence, it is
 * the former.  This makes sense because the arrays are really treated
 * as hash codes at the first level, so such a parameter needs no
 * array-specific processing (at this level of the tree at least).
 */
public class ParameterInfo extends DaikonVariableInfo
{
    /**
     * The argument number for this parameter.
     * For instance, consider the method void x(int a, double b, Object c).
     * Then a, b, and c have argument numbers 0, 1 and 2 respectively.
     */
    private final int argNum;

    /** Argument type **/
    private final Class argType;

    /**
     * Constructs an ParameterInfo object with the specified name
     * @param theName The variable name (used in the decl file)
     */
    public ParameterInfo(String theName, int theArgNum)
    {
        super(theName);

        argNum = theArgNum;
        argType = null;
    }

    /**
     * Constructs a PamterInfo object with the name/type specified for this
     * the specified argument number in mi.
     */
    public ParameterInfo (MethodInfo mi, int theArgNum)
    {
        super (mi.arg_names[theArgNum]);
        argNum = theArgNum;
        argType = mi.arg_types[argNum];
    }

    /**
     * Returns the argument number for this parameter
     */
    public int getArgNum()
    {
        return argNum;
    }

    public Object getMyValFromParentVal(Object value)
    {
        //a parameter has no parent value
        assert false : "Parameters have no parent value";
        throw new RuntimeException("Parameters have no parent value");
    }

    public Class getType()
    {
        return (argType);
    }
}
