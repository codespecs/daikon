/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

public interface ExternalCFGCatchDefNode extends ExternalCFGDefNode {
    public ExternalCFGDefNode[] getSourceNodes();
    public JBCClass getCatchClass();
}
