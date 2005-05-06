
package daikon.chicory;

import java.lang.reflect.Field;
import java.util.*;


/**
 */
public abstract class DaikonInfo implements Iterable<DaikonInfo>
{
    private String name;
    private List<DaikonInfo> children;
    protected boolean isArray;
    
    public DaikonInfo(String theName)
    {
        this(theName, false);
    }
    
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
     * @param info
     */
    public void addChild(DaikonInfo info)
    {
        if(info == null)
            throw new RuntimeException("info cannot be null in DaikonInfo.addChild()");
        
        children.add(info);
    }
    
    public String toString()
    {
        return getString(new StringBuffer("--")).toString();
    }
    
    public StringBuffer getString(StringBuffer offset)
    {
        StringBuffer theBuf = new StringBuffer();
        
        theBuf.append(offset  + name + "\n");
        
        StringBuffer childOffset = new StringBuffer(offset);
        childOffset.append("--");
        for(DaikonInfo info: children)
        {
            theBuf.append(info.getString(childOffset));
        }
        
        return theBuf;
    }

    /**
     * @return
     */
    public Iterator<DaikonInfo> iterator()
    {
        return children.iterator();
    }

    /**
     * @param val
     */
    public abstract Object getChildValue(Object val);
    
    public String getValueString(Object val)
    {
        if(isArray)
        {
            return printList((List) val);
        }
        else   
        {
        return printValString(val, true);
        }
    }
    
    private String printValString(Object theValue)
    {
        return printValString(theValue, true);
    }
    
    //prints the value and the associated changed integer
    //if val == null prints nonsensical
    protected String printValString(Object theValue, boolean hashArray)
    {
        String retString = showValueEndLine(theValue, hashArray);

        if (theValue instanceof NonsensicalObject)
            retString += "2";
        else
            retString += "1";
        
        return retString;
    }
    
    //print a value, followed by an endline
    //(showValue does not print an endline after it completes)
    private String showValueEndLine(Object val, boolean hashArr)
    {
        return showValue(val, hashArr) + "\n";
    }
    
//  prints the value
    //doesn't print endline
    //if hashArray is true, it prints the "hash value" of the array
    //and not its separate values
    private String showValue(Object theValue, boolean hashArray)
    {
        if (theValue == null)
        {
            return "null";
        }

        Class type = theValue.getClass();

        if (type.isPrimitive())
        {
            throw new RuntimeException("Objects cannot be primitive");
        }
        else if (theValue instanceof Runtime.PrimitiveWrapper)
        {
            return showPrimitive(theValue);
        }
        else if (!hashArray && (type.isArray()))
        {
            return showArray(theValue);
        }

        else if (theValue instanceof NonsensicalObject)
        {
            return ("nonsensical");
        }
        /*else if (theValue instanceof Number)
        {
            outFile.print (theValue.toString());
        }
        else if (theValue instanceof Boolean)
        {
            outFile.print (theValue.toString());
        }*/
        else
        {
            return showObject(theValue);
        }
    }
    
    //print values for primitive (wrapped) objects
    private String showPrimitive(Object obj)
    {
        if (!(obj instanceof Runtime.PrimitiveWrapper))
            throw new RuntimeException("Objects passed to showPrimitive must implement PrimitiveWrapper\nThis object is type: " + obj.getClass().getName());

        //use wrapper classes toString methods to print value
        return (obj.toString());
    }
    
    //shows the values in an array
    private String showArray(Object array)
    {
        List theList = DTraceWriter.getListFromArray(array);
        return showList(theList);
    }
    
    //prints the Object's unique ID
    //in other words, a "hash code"
    private String showObject(Object theObject)
    {
        if (theObject == null)
            return ("null");
        else if (theObject instanceof NonsensicalObject)
            return ("nonsensical");
        else
            return Integer.toString(System.identityHashCode(theObject));
    }
    
    private String printList(List /* <Value> */theValues)
    {
        String retString = showList(theValues) + "\n";

        if (theValues instanceof NonsensicalList)
            retString += ("2");
        else
            retString += ("1");
        
        return retString;
    }
    
//  prints out a list of values as if it were an array
    protected String showList(List /* <Object>*/theValues)
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
        for (Iterator iter = theValues.iterator(); iter.hasNext();)
        {
            Object elementVal = iter.next();
            
            buf.append(showValue(elementVal, true));

            //put space between elements in array
            if (iter.hasNext())
                buf.append(" ");
        }
        buf.append("]");
        
        return buf.toString();
    }
}
