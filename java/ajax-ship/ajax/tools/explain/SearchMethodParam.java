/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.explain;

import ajax.jbc.*;

public class SearchMethodParam extends SearchItem {
    private JBCMethod method;
    private int paramIndex;
    
    SearchMethodParam(JBCMethod method, int paramIndex) {
        this.method = method;
        this.paramIndex = paramIndex;
    }
    
    public JBCType getType() {
        return method.getMethodType().getParameterTypes()[paramIndex];
    }
    
    public JBCMethod getMethod() {
        return method;
    }
    
    public int getParamIndex() {
        return paramIndex;
    }
    
    public String toString() {
        return "Formal parameter " + getParamIndex() + " in " + getMethod();
    }
    
    public int hashCode() {
        return method.hashCode()*130143 + paramIndex*1491 + 1412;
    }
    
    public boolean equals(Object o) {
        if (o instanceof SearchMethodParam) {
            SearchMethodParam p = (SearchMethodParam)o;
            
            return p.method.equals(method) && p.paramIndex == paramIndex;
        } else {
            return false;
        }
    }
}
