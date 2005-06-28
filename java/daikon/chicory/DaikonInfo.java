
package daikon.chicory;

import java.util.*;


/**
 * Each DaikonInfo object is a node  in the tree structure of the variables in 
 * the target application.  The tree structure is built in the 
 * DeclWriter and traversed in the DTraceWriter.
 * There is such a tree structure associated with every program point.  This architecture makes it
 * possible to avoid the issue of "traversal pattern duplication" in which both the 
 * DeclWriter and DTraceWriter must traverse the target application's variables identically.
 * In general, the variable a will be the parent of the variables a.b and a.c in the tree, where b and c
 * are fields in a's class.
 * 
 * Each node can have any non-negative
 * number of child nodes.  DaikonInfo is an abstract class.  Its subtypes are designed
 * to represent specific types of variables, such as arguments, arrays, etc. 
 */
public abstract class DaikonInfo implements Iterable<DaikonInfo>
{
	/** The variable name, if appropriate to the subtype **/
    private String name;
	
	/** The child nodes **/
    private List<DaikonInfo> children;
	
	/** True iff this variable is an array **/
    protected boolean isArray;
    
	/** Constructs a non-array type DaikonInfo object 
	 * @param theName The name of the variable
	 */
    public DaikonInfo(String theName)
    {
        this(theName, false);
    }
    
	/**
	 * Constructs a DaikonInfo object
	 * @param theName The variable's name
	 * @param arr True iff the variable is an array
	 */
    public DaikonInfo(String theName, boolean arr)
    {
        name = theName;
        children = new ArrayList <DaikonInfo> ();
        
        isArray = arr;
    }

    public String getName()
    {
        return name;
    }
    
    /**
     * Add a child to this node.
     * Should only be called while the traversal tree is being constructed.
     * 
     * @param info The child object, must be non-null.
     */
    public void addChild(DaikonInfo info)
    {	
		assert info!=null : "info cannot be null in DaikonInfo.addChild()";
        
        children.add(info);
    }
    
	/**
	 * Returns a string representation of this node and all of its children
	 */
    public String toString()
    {
        return getString(new StringBuffer("--")).toString();
    }
    
	/**
	 * Return a StringBuffer which contains all ancestors of this node.
	 * Longer indentations correspond to further distance in the tree.
	 * @param offset The offset to begin each line with.
	 * @return StringBuffer which contains all children of this node
	 */
    private StringBuffer getString(StringBuffer offset)
    {
        StringBuffer theBuf = new StringBuffer();
        
        theBuf.append(offset + name + DaikonWriter.lineSep);
        
        StringBuffer childOffset = new StringBuffer(offset);
        childOffset.append("--");
        for(DaikonInfo info: children)
        {
            theBuf.append(info.getString(childOffset));
        }
        
        return theBuf;
    }

    /**
     * Return an iterator over all the node's children
     * Don't modify the list of children through the iterator,
     * as an unmodifiable list is used to generator the iterator.
     */
    public Iterator<DaikonInfo> iterator()
    {
        return Collections.unmodifiableList(children).iterator();
    }

    /**
     * Given an object value corresponding to the parent of this DaikonInfo variable, 
     * return the value (of the corresponding value in the target application)
     * of this DaikonInfo variable. 
     * 
     * For instance, if the variable a has a field b, then calling
     * getMyValParentVal(val_of_a) will return the value of a.b
     * 
     * @param val The parent object
     */
    public abstract Object getMyValFromParentVal(Object val);
    
	/**
	 * Returns a String representation of this object suitable for a .decls file
	 * @param val The object whose value to print
	 */
    public String getDeclValueString(Object val)
    {
        if(isArray)
        {
            return getValueStringOfListWithMod((List) val);
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
	 * If val == null prints nonsensical
	 */
    protected String getValueStrignOfObjectWithMod(Object theValue, boolean hashArray)
    {
        String retString = getValueStringOfObjectEndLine(theValue, hashArray);

        if (theValue instanceof NonsensicalObject)
            retString += "2";
        else
            retString += "1";
        
        return retString;
    }
    
	/**
	 * Get a value of an object (as a string), followed by an endline 
	 */
    private String getValueStringOfObjectEndLine(Object val, boolean hashArr)
    {
        return getValueStringOfObject(val, hashArr) + DaikonWriter.lineSep;
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
	 * 	Gets the list of values (as a string) from getValueStringOfList
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
}
