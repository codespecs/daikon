/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

public interface ExternalCFGUserFieldDefNode extends ExternalCFGDefNode {
    public ExternalCFGVariable getObject();
    public UserField getField();
}
