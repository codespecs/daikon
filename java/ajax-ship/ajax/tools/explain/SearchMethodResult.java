/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.explain;

import ajax.jbc.*;

public class SearchMethodResult extends SearchItem {
    private JBCMethod method;
    
    SearchMethodResult(JBCMethod method) {
        this.method = method;
    }
    
    public JBCType getType() {
        return method.getMethodType().getReturnType();
    }
    
    public JBCMethod getMethod() {
        return method;
    }
    
    public String toString() {
        return "Formal result in " + getMethod();
    }

    public int hashCode() {
        return method.hashCode()*130143 + 104161;
    }
    
    public boolean equals(Object o) {
        if (o instanceof SearchMethodResult) {
            SearchMethodResult p = (SearchMethodResult)o;
            
            return p.method.equals(method);
        } else {
            return false;
        }
    }
}
