/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util;

import java.util.*;

public class StringUtils {
    private static Hashtable abbreviatedWords = createAbbreviations();
    
    private static Hashtable createAbbreviations() {
        Hashtable t = new Hashtable();
        
        t.put("subobject", "so.");
        t.put("subchunk", "sc.");
        t.put("for", "");
        t.put("class", "");
        t.put("method", "");
        
        return t;
    }
    
    private static String makeAbbreviation(String s) {
        String abbr = (String)abbreviatedWords.get(s);
            
        if (abbr != null) {
            return abbr;
        } else {
            int lastDot = s.lastIndexOf('.');
            
            if (lastDot >= 0) {
                return s.substring(lastDot + 1);
            } else {
                return s;
            }
        }
    }
    
    public static String[] split(String s, char ch) {
        Vector strings = new Vector();
        
        while (s != null) {
            int newline = s.indexOf(ch);
            
            if (newline < 0) {
                if (s.length() > 0) {
                    strings.addElement(s);
                }
                s = null;
            } else {
                if (newline > 0) {
                    strings.addElement(s.substring(0, newline));
                }
                s = s.substring(newline + 1);
            }
        }
        
        String[] result = new String[strings.size()];
        
        strings.copyInto(result);
        return result;
    }
    
    public static String[] splitOnWhitespace(String s) {
        int len = s.length();
        int startWord = -1;
        Vector words = new Vector();
        boolean inWhitespace = true;
        
        for (int i = 0; i < len; i++) {
            boolean newInWhitespace = Character.isWhitespace(s.charAt(i));
            
            if (newInWhitespace != inWhitespace) {
                if (newInWhitespace) {
                    words.addElement(s.substring(startWord, i));
                } else {
                    startWord = i;
                }
            }
            
            inWhitespace = newInWhitespace;
        }
        if (!inWhitespace) {
            words.addElement(s.substring(startWord));
        }
        
        String[] result = new String[words.size()];
        
        words.copyInto(result);
        return result;
    }
    
    public static String join(String glue, String[] array) {
        StringBuffer buf = new StringBuffer();
        
        for (int i = 0; i < array.length; i++) {
            if (i > 0) {
                buf.append(glue);
            }
            buf.append(array[i]);
        }
        return buf.toString();
    }
    
    public static String abbreviate(String s) {
        StringBuffer buf = new StringBuffer();
        String[] words = splitOnWhitespace(s);
        
        for (int i = 0; i < words.length; i++) {
            String abbr = makeAbbreviation(words[i]);
            
            if (abbr.length() > 0) {
                if (buf.length() > 0) {
                    buf.append(" ");
                }
                
                buf.append(abbr);
            }
        }
        
        return buf.toString();
    }
}
