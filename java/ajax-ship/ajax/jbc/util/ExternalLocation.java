/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.jbc.*;

public class ExternalLocation extends Location {
    private Object fgName;
    private ExternalCFGNode node;
    
    private ExternalLocation(Object fgName, ExternalCFGNode node) {
        this.fgName = fgName;
        this.node = node;
    }
    
    public ExternalLocation(JBCMethod method, ExternalCFGNode node) {
        this((Object)method, node);
    }
    
    public ExternalLocation(String name, ExternalCFGNode node) {
        this((Object)name, node);
    }
    
    public JBCMethod getMethod() {
        return fgName instanceof JBCMethod ? (JBCMethod)fgName : null;
    }
    
    public String getFlowgraphName() {
        return fgName instanceof String ? (String)fgName : null;
    }
    
    public ExternalCFGNode getNode() {
        return node;
    }
    
    public int hashCode() {
        return fgName.hashCode()*18311 + node.hashCode()*103147 + 193143;
    }
    
    public boolean equals(Object o) {
        if (o instanceof ExternalLocation) {
            ExternalLocation l = (ExternalLocation)o;
            
            return l.node.equals(node) && l.fgName.equals(fgName);
        } else {
            return false;
        }
    }
    
    public String toString() {
        return "Node " + node + " in " + fgName;
    }
    
    public JBCMethod getCalledMethod() {
        ExternalCFGNode node = getNode();
            
        if (node instanceof ExternalCFGMethodInvocationDefNode) {
            ExternalCFGMethodInvocationDefNode n = (ExternalCFGMethodInvocationDefNode)node;
                
            return n.getMethod();
        } else {
            return null;
        }
    }
    
    public JBCField getAccessedField() {
        ExternalCFGNode node = getNode();
            
        if (node instanceof ExternalCFGFieldDefNode) {
            return ((ExternalCFGFieldDefNode)node).getField();
        } else if (node instanceof ExternalCFGFieldAssignmentDefNode) {
            return ((ExternalCFGFieldAssignmentDefNode)node).getField();
        } else {
            return null;
        }
    }
    
    public Object getFunction() {
        return fgName;
    }
}
