
package daikon.chicory;

import java.lang.reflect.Field;
import java.util.*;


/**
 * Each DaikonVariableInfo object is a node in the tree structure of the
 * variables in the target application.  The tree structure is built in the
 * DeclWriter and traversed in the DTraceWriter.  There is such a tree
 * structure associated with every program point.  This architecture makes
 * it possible to avoid the issue of "traversal pattern duplication" in
 * which both the DeclWriter and DTraceWriter must traverse the target
 * application's variables identically.  In general, the variable a will be
 * the parent of the variables a.b and a.c in the tree, where b and c are
 * fields in a's class.
 *
 * Each node can have any non-negative number of child nodes.
 * DaikonVariableInfo is an abstract class.  Its subtypes are designed to
 * represent specific types of variables, such as arguments, arrays, etc.
 */
public abstract class DaikonVariableInfo implements Iterable<DaikonVariableInfo>
{
    /** The variable name, if appropriate to the subtype **/
    private final String name;

    /** The child nodes **/
    public List<DaikonVariableInfo> children;

    /** True iff this variable is an array **/
    protected final boolean isArray;

    /** Constructs a non-array type DaikonVariableInfo object
     * @param theName The name of the variable
     */
    public DaikonVariableInfo(String theName)
    {
        this(theName, false);
    }

    /**
     * Constructs a DaikonVariableInfo object
     * @param theName The variable's name
     * @param arr True iff the variable is an array
     */
    public DaikonVariableInfo(String theName, boolean arr)
    {
        // Intern the names because there will be many of the
        // same variable names at different program points within
        // the same class
        if(theName == null)
            name = null;
        else
            name = theName.intern();


        children = new ArrayList <DaikonVariableInfo> ();

        isArray = arr;
    }

    public String getName()
    {
        return name;
    }

    /**
     * Add a child to this node.
     * Should only be called while the tree is being constructed.
     *
     * @param info The child object, must be non-null.
     */
    public void addChild(DaikonVariableInfo info)
    {
        assert info!=null : "info cannot be null in DaikonVariableInfo.addChild()";

        children.add(info);
    }

    /**
     * Returns a string representation of this node
     */
    public String toString()
    {
        return getClass().getName() + ":" + getName();
    }

    /** Returns a string representative of this node and its children **/
    public String treeString()
    {
        return getStringBuffer(new StringBuffer("--")).toString();
    }

    /**
     * Return a StringBuffer which contains the name of this node
     * and all ancestors of this node.
     * Longer indentations correspond to further distance in the tree.
     * @param offset The offset to begin each line with.
     * @return StringBuffer which contains all children of this node
     */
    private StringBuffer getStringBuffer(StringBuffer offset)
    {
        StringBuffer theBuf = new StringBuffer();

        theBuf.append(offset + name + DaikonWriter.lineSep);

        StringBuffer childOffset = new StringBuffer(offset);
        childOffset.append("--");
        for(DaikonVariableInfo info: children)
        {
            theBuf.append(info.getStringBuffer(childOffset));
        }

        return theBuf;
    }

    /**
     * Return an iterator over all the node's children.
     * Don't modify the list of children through the iterator,
     * as an unmodifiable list is used to generate the iterator.
     */
    public Iterator<DaikonVariableInfo> iterator()
    {
        return Collections.unmodifiableList(children).iterator();
    }

    /**
     * Given an object value corresponding to the parent of this DaikonVariableInfo variable,
     * return the value (of the corresponding value in the target application)
     * of this DaikonVariableInfo variable.
     *
     * For instance, if the variable a has a field b, then calling
     * getMyValParentVal(val_of_a) will return the value of a.b
     *
     * @param parentVal The parent object
     */
    public abstract Object getMyValFromParentVal(Object parentVal);

    /**
     * Returns a String representation of this object suitable for a .dtrace file
     * @param val The object whose value to print
     */
    public String getDTraceValueString(Object val)
    {
        if(isArray)
        {
            return getValueStringOfListWithMod((List<Object>) val);
        }
        else
        {
            return getValueStrignOfObjectWithMod(val, true);
        }
    }

    /**
     *
     * Gets the value of an object and concatenates
     * the associated "modified" integer.
     */
    protected String getValueStrignOfObjectWithMod(Object theValue, boolean hashArray)
    {
        String retString =getValueStringOfObject(theValue, hashArray) + DaikonWriter.lineSep;

        if (theValue instanceof NonsensicalObject)
            retString += "2";
        else
            retString += "1";

        return retString;
    }

    /**
     * Gets the value, but with no endline.
     * If hashArray is true, it prints the "hash code" of the array
     * and not its separate values.
     */
    private String getValueStringOfObject(Object theValue, boolean hashArray)
    {
        if (theValue == null)
        {
            return "null";
        }

        Class type = theValue.getClass();

        assert !(type.isPrimitive()) : "Objects cannot be primitive";

        if (theValue instanceof Runtime.PrimitiveWrapper)
        {
            return getPrimitiveValueString(theValue);
        }
        else if (!hashArray && (type.isArray()))
        {
            //show the full array
            return getValueStringOfArray(theValue);
        }
        else if (theValue instanceof NonsensicalObject)
        {
            return ("nonsensical");
        }
        else
        {
            //basically, show the hashcode of theValue
            return getObjectHashCode(theValue);
        }
    }

    /**
     * Get value string for a primitive (wrapped) object
     */
    private String getPrimitiveValueString(Object obj)
    {
        assert (obj instanceof Runtime.PrimitiveWrapper) : "Objects passed to showPrimitive must implement PrimitiveWrapper" + DaikonWriter.lineSep +"This object is type: " + obj.getClass().getName();

        //use wrapper classes toString methods to print value
        return (obj.toString());
    }


    /**
     * Gets a string representation of the values in an array
     */
    private String getValueStringOfArray(Object array)
    {
        List <Object> theList = DTraceWriter.getListFromArray(array);
        return getValueStringOfList(theList);
    }

    /**
     *
     *  Gets the Object's unique ID as a string.
     *  In other words, a "hash code"
     */
    private String getObjectHashCode(Object theObject)
    {
        if (theObject == null)
            return ("null");
        else if (theObject instanceof NonsensicalObject)
            return ("nonsensical");
        else
            return Integer.toString(System.identityHashCode(theObject));
    }

    /**
     *
     *     Gets the list of values (as a string) from getValueStringOfList
     *  and concatenates the "modified" value
     */
    private String getValueStringOfListWithMod(List <Object> theValues)
    {
        String retString = getValueStringOfList(theValues) + DaikonWriter.lineSep;

        if (theValues instanceof NonsensicalList)
            retString += ("2");
        else
            retString += ("1");

        return retString;
    }

    /**
     * Returns a string representation of the values
     * of a list of values as if it were an array
     *
     * @param theValues The values to print out
     */
    protected String getValueStringOfList(List <Object> theValues)
    {
        if (theValues == null)
        {
            return ("null");
        }

        if (theValues instanceof NonsensicalList)
        {
            return ("nonsensical");
        }

        StringBuffer buf = new StringBuffer();

        buf.append("[");
        for (Iterator <Object> iter = theValues.iterator(); iter.hasNext();)
        {
            Object elementVal = iter.next();

            //hash arrays...
            //don't want to print arrays within arrays
            buf.append(getValueStringOfObject(elementVal, true));

            //put space between elements in array
            if (iter.hasNext())
                buf.append(" ");
        }
        buf.append("]");

        return buf.toString();
    }

    /**
     * Process the children of the specified type and add them to the
     * tree.
     *
     * @param type Type of the variable whose children should be processed.
     *             Can be any valid type (including arrays and primitives)
     * @param depth Number of times to recurse proessing children
     * @param in_arr True if we are processing an array at this level
     *               or higher.  Required because we do not process arrays
     *               nested in arrays.
     */
    protected void process_children (Class type, int depth, boolean in_arr)
    {
        if (type.isPrimitive())
            return;
        else if (type.isArray())
        {
            // Don't attempt arrays in arrays
            if (in_arr)
                return;

            // Add the contents of the array
            Class array_type = type.getComponentType();
            ArrayInfo ai = new ArrayInfo (getName(), array_type);
            addChild (ai);
            ai.process (depth-1);
        }
        else // must be a class
        {
            if (depth <= 0)
                return;

            // Don't include fields in system variables
            if (DaikonWriter.systemClass (type))
                return;

            // Add each field
            Field[] fields = type.getDeclaredFields();
            for (Field field : fields)
            {
                FieldInfo field_var = new FieldInfo (getName(), field);
                field_var.process (depth-1, in_arr);
                addChild (field_var);
            }
        }
    }

}
