/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.protocol;

import java.io.*;

public class SourceResponse implements Serializable {
    private String text;
    private ClassDescriptor forClass;
    
    public SourceResponse(ClassDescriptor forClass, String text) {
        this.forClass = forClass;
        this.text = text;
    }
    
    public String getText() {
        return text;
    }
    
    public ClassDescriptor getForClass() {
        return forClass;
    }
    
    public String toString() {
        return "Source for " + forClass + ": "
            + (text != null ? text.substring(0, 50) : "<unknown>");
    }
}
