/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver.util;

import ajax.solver.*;
import java.util.*;

class BindingListAnnotator implements ConstraintAnnotator {
    private Hashtable nodeLabels = new Hashtable();

    BindingListAnnotator(Vector bindingList) {
        for (Enumeration e = bindingList.elements(); e.hasMoreElements();) {
            Object[] pair = (Object[])e.nextElement();
            Vector labels = (Vector)nodeLabels.get(((Variable)pair[1]).getHead());
            
            if (labels == null) {
                labels = new Vector();
                nodeLabels.put(((Variable)pair[1]).getHead(), labels);
            }
            
            labels.addElement(pair[0]);
        }
    }
    
    Enumeration getBoundVars() {
        return nodeLabels.keys();
    }
    
    public String getVarLabel(Variable v) {
        Vector names = (Vector)nodeLabels.get(v.getHead());
        String result;
        
        if (names == null) {
            result = "";
        } else {
            int line_len = (int)Math.sqrt(names.size()) + 1;
            int i = 0;
            StringBuffer buf = new StringBuffer();
            
            for (Enumeration e = names.elements(); e.hasMoreElements();) {
                String s = (String)e.nextElement();
                
                if (!s.equals("")) {
                    if (i % line_len == 0) {
                        if (i > 0) {
                            buf.append("\\n");
                        }
                    } else {
                        buf.append(", ");
                    }
                    i++;
                
                    buf.append(s);
                }
            }
            
            result = buf.toString();
        }
        
        return result + "\\n(" + v.getClusterID() + ")";
    }
    
    public void countSource() {
    }
    
    public void countInstance() {
    }
    
    public void countParent() {
    }
    
    public void countComponent() {
    }
}
