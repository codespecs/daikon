/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util.salamis;

import ajax.jbc.*;
import java.util.Hashtable;

class SalamisCatchDefNode extends SalamisDefNode implements ExternalCFGCatchDefNode {
    private String[] labels;
    private ExternalCFGDefNode[] nodes = null;
    private String className;
    private SalamisFlowgraph fg;
    
    SalamisCatchDefNode(SalamisParser p, String defVar, String className, String[] labels) {
        super(p, defVar);
        this.labels = labels;
        this.className = className;
    }

    public ExternalCFGDefNode[] getSourceNodes() {
        return nodes;
    }
    
    public JBCClass getCatchClass() {
        return className != null ? fg.resolveClass(className) : null;
    }
    
    void resolve(Hashtable labelTable, SalamisFlowgraph fg) {
        nodes = new ExternalCFGDefNode[labels.length];
        
        for (int i = 0; i < nodes.length; i++) {
            nodes[i] = (ExternalCFGDefNode)labelTable.get(labels[i]);
        }
        
        labels = null;
        this.fg = fg;
    }
}
