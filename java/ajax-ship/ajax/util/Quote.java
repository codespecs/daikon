/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util;

public class Quote {
    public static String quote(String s) {
        StringBuffer buf = null;
        int len = s.length();
        int copyFrom = 0;
        
        for (int i = 0; i < len; i++) {
            switch (s.charAt(i)) {
                case '\\':
                case '"':
                    if (buf == null) {
                        buf = new StringBuffer(s.substring(0, i));
                    } else {
                        buf.append(s.substring(copyFrom, i));
                    }
                    buf.append('\\');
                    copyFrom = i;
                    break;
                    
                case '\n':
                    if (buf == null) {
                        buf = new StringBuffer(s.substring(0, i));
                    } else {
                        buf.append(s.substring(copyFrom, i));
                    }
                    buf.append("\\n");
                    copyFrom = i + 1;
                    break;

                case '\r':
                    if (buf == null) {
                        buf = new StringBuffer(s.substring(0, i));
                    } else {
                        buf.append(s.substring(copyFrom, i));
                    }
                    buf.append("\\r");
                    copyFrom = i + 1;
                    break;
            }
        }
        
        if (buf != null) {
            buf.append(s.substring(copyFrom));
            return buf.toString();
        } else {
            return s;
        }
    }
}
