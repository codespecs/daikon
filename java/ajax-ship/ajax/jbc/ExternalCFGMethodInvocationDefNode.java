/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

/**
NOTE: Invocation nodes must return TWO variables --- the first one for the result,
and the second one for the possible exception. Don't use the second one though,
it's taken care of internally.
*/
public interface ExternalCFGMethodInvocationDefNode extends ExternalCFGDefNode {
    public ExternalCFGVariable[] getParameters();
    public JBCMethod getMethod();
}
