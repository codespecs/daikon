/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.jbc.*;
import java.io.Serializable;

public class ExternalCatchNode extends ExternalDefNode implements ExternalCFGCatchDefNode, Serializable {
    private ExternalCFGDefNode[] sourceNodes;
    private JBCClass catchClass;
    
    private void init(ExternalCFGDefNode[] sourceNodes, JBCClass catchClass) {
        this.sourceNodes = sourceNodes;
        this.catchClass = catchClass;
    }
    
    public ExternalCatchNode(ExternalCFGDefNode[] sourceNodes, JBCClass catchClass) {
        super();
        init(sourceNodes, catchClass);
    }
    
    public ExternalCatchNode(ExternalCFGVariable def, ExternalCFGDefNode[] sourceNodes, JBCClass catchClass) {
        super(def);
        init(sourceNodes, catchClass);
    }
    
    public ExternalCatchNode(ExternalCFGNode successor, ExternalCFGVariable def, ExternalCFGDefNode[] sourceNodes, JBCClass catchClass) {
        super(successor, def);
        init(sourceNodes, catchClass);
    }
    
    public ExternalCFGDefNode[] getSourceNodes() {
        return sourceNodes;
    }
    
    public JBCClass getCatchClass() {
        return catchClass;
    }
}
