/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

public interface ExternalCFGFieldDefNode extends ExternalCFGDefNode {
    public ExternalCFGVariable getObject();
    public JBCField getField();
}
