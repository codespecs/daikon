/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.util;

import ajax.analyzer.*;
import ajax.jbc.*;
import java.util.*;

/**
This class is the basis for breaking up a tool into mutiple reusable components.

One component is the code that defines the type of "intermediate values"
and the lattice operations on those values. This code is inserted by
subclassing this class. (See, e.g., ClassTracker, an implementation
that sets intermediate values to be classes and defines the lattice
operations according to the class hierarchy.)

The other components are implementations of DatumSpecifier. They are
responsible for constructing the tool's S and T sets. All the "live
method found" callbacks are multicast to each DatumSpecifier. Each
DatumSpecifier responds by registering any S and/or T values that it finds
interesting. All updateResult callbacks are also multicast to each
DatumSpecifier.

For example, the downcast checking tool consists of a ClassTracker,
a NewObjectClassTracker and a DowncastClassTracker. The ClassTracker
implements the lattice operations to find the l.u.b. of classes in the
class hierarchy. The NewObjectClassTracker adds an S value for each
"new object" operation in the program, setting its class to be the
intermediate value. The DowncastClassTracker adds a T value for each
downcast appearing in the program, and listens for updateResult callbacks.
It compares the results with the downcast bound and reports an error
when necessary.
*/
abstract public class IndirectionQueryFamily extends JBCQueryFamily {
    private DatumSpecifier[] specifiers;
    private ResultListener[] results;
    
    public IndirectionQueryFamily(Analyzer analyzer, DatumSpecifier[] specifiers, ResultListener[] results) {
        super(analyzer);
        
        this.specifiers = specifiers;
        this.results = results;
    }
    
    public void addSourceDatum(int offset, JBCExpression expression, Object intermediate) {
        super.addSourceDatum(offset, expression, intermediate);
    }
    
    public void addSourceDatum(ExternalCFGNode node, JBCExpression expression, Object intermediate) {
        super.addSourceDatum(node, expression, intermediate);
    }
    
    public void addTargetDatum(int offset, JBCExpression expression, Object targetCookie) {
        registerTarget(targetCookie);
        super.addTargetDatum(offset, expression, targetCookie);
    }
    
    public void addTargetDatum(ExternalCFGNode node, JBCExpression expression, Object targetCookie) {
        registerTarget(targetCookie);
        super.addTargetDatum(node, expression, targetCookie);
    }
    
    protected void registerTarget(Object targetCookie) {
        for (int i = 0; i < results.length; i++) {
            results[i].registerTarget(targetCookie);
        }
    }
    
    protected void notifyAnalysisComplete() {
        for (int i = 0; i < results.length; i++) {
            results[i].notifyAnalysisComplete();
        }
    }
    
    protected void updateResult(Object targetCookie, Object intermediate) {
        for (int i = 0; i < results.length; i++) {
            results[i].updateResult(targetCookie, intermediate);
        }
    }
    
    protected void identifyQueryData(JBCMethod method, byte[] code, boolean[] instructions) {
        for (int i = 0; i < specifiers.length; i++) {
            specifiers[i].identifyQueryData(this, method, code, instructions);
        }
    }
    
/**
As above, but for native code specifications.

@return an Enumeration of FlowgraphQueryDatums
*/
    protected void identifyQueryData(String name, ExternalFlowgraph fg, Vector externalNodes) {
        for (int i = 0; i < specifiers.length; i++) {
            specifiers[i].identifyQueryData(this, name, fg, externalNodes);
        }
    }
    
/**
As above, but for native methods.

@return an Enumeration of FlowgraphQueryDatums
*/
    protected void identifyQueryData(JBCMethod method, ExternalFlowgraph fg, Vector externalNodes) {
        for (int i = 0; i < specifiers.length; i++) {
            specifiers[i].identifyQueryData(this, method, fg, externalNodes);
        }
    }
    
    public void dispose() {
        super.dispose();
    }
}
