/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

/**
NOTE: Invocation nodes must return TWO variables --- the first one for the result,
and the second one for the possible exception. Don't use the second one though.
*/
public interface ExternalCFGFlowgraphInvocationDefNode extends ExternalCFGDefNode {
    public String getFunctionName();
    public ExternalCFGVariable[] getParameters();
}
