package daikon.chicory;

import java.util.*;


/**
 */
public class StringInfo extends DaikonInfo
{   
    public StringInfo(String theName, boolean arr)
    {
        super(theName, arr);
    }

    public Object getChildValue(Object value)
    {
        return value;
    }
    
    
    
    public String getValueString(Object val)
    {
        if(isArray)
        {
            return showStringList((List)val);
        }
        else
        {
            return getValueStringNonArr(val);
        }
    }
    
    public static String showStringList(List /* <String> */theValues)
    {
        StringBuffer buf = new StringBuffer();
        
        if (theValues == null)
        {
            //buf.append("null");
            return "null\n1";
        }

        if (theValues instanceof NonsensicalList)
        {
            //buf.append("nonsensical");
            return "nonsensical\n2";
        }
        
        

        buf.append("[");
        for (Iterator iter = theValues.iterator(); iter.hasNext();)
        {
            String str = (String) iter.next();

            if (str == null)
                buf.append(str);
            else
                buf.append("\"" + encodeString(str) + "\"");

            //put space between elements in array
            if (iter.hasNext())
                buf.append(" ");
        }
        buf.append("]");
        
        if (theValues instanceof NonsensicalList)
            buf.append("\n2");
        else
            buf.append("\n1");
        
        return buf.toString();
    }
    
   
    public String getValueStringNonArr(Object val)
    {
        String retString;
        
        if (val == null)
            retString = ("null\n");
        else if (val instanceof NonsensicalObject)
            retString = ("nonsensical\n");
        else
        {
            retString = showString((String) val);
            retString += "\n";
        }

        if (val instanceof NonsensicalObject)
            retString += ("2");
        else
            retString += ("1");
        
        return retString;
    }
    
    private String showString(String stringRef)
    {
        return ("\"" + encodeString(stringRef) + "\"");
    }

    //removes endlines in string
    private static String encodeString(String input)
    {
        return Runtime.quote(input);
    }
}
