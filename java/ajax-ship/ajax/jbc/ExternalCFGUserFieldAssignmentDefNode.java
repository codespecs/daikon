/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

public interface ExternalCFGUserFieldAssignmentDefNode extends ExternalCFGDefNode {
    public ExternalCFGVariable getObject();
    public UserField getField();
    public ExternalCFGVariable getValue();
}
