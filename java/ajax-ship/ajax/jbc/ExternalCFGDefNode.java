/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

public interface ExternalCFGDefNode extends ExternalCFGNode {
    public ExternalCFGVariable[] getDefinedVariables();
}
