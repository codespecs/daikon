/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

public interface ExternalCFGFieldAssignmentDefNode extends ExternalCFGDefNode {
    public ExternalCFGVariable getObject();
    public JBCField getField();
    public ExternalCFGVariable getValue();
}
