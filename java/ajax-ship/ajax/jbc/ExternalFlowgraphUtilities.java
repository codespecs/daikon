/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

import java.util.*;

public class ExternalFlowgraphUtilities {
    private static void addNodes(Hashtable nodes, ExternalCFGNode node) {
        if (nodes.get(node) == null) {
            nodes.put(node, node);
            
            for (Enumeration e = node.getSuccessors(); e.hasMoreElements();) {
                addNodes(nodes, (ExternalCFGNode)e.nextElement());
            }
        }
    }
    
    public static Vector getDefNodes(ExternalFlowgraph fg) {
        Hashtable nodes = new Hashtable();
        
        addNodes(nodes, fg.getCFGRoot());
        
        Vector result = new Vector();
        
        for (Enumeration e = nodes.keys(); e.hasMoreElements();) {
            Object o = e.nextElement();
            
            if (o instanceof ExternalCFGDefNode) {
                result.addElement(o);
            }
        }
        
        return result;
    }
}
