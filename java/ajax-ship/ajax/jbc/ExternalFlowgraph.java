/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

/**
An ExternalFlowgraph object serves to provide information about foreign
code (code not directly available to the analyzer).

These objects must be immutable.
*/
public interface ExternalFlowgraph {
    public ExternalCFGNode getCFGRoot();
    public ExternalCFGDefNode getResultDef();
    public ExternalCFGDefNode getExceptionDef();
}
