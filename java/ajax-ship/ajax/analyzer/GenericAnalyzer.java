/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer;

import ajax.jbc.*;
import ajax.analyzer.semantics.*;

/**
This is the interface exported by an analysis algorithm. It is not to be
used directly by tools. Tools should use an Analyzer object to wrap up
a GenericAnalyzer into a complete working analysis.
*/
public interface GenericAnalyzer {
    public void setConsumer(GenericAnalyzerConsumer consumer);
    public void setSemantics(Semantics s);
    public boolean isNativeSpecificationAccurate();
    
    public JBCWorld getWorld();

    public boolean workBeforeLiveProcessing() throws PrematureTerminationException;
    public boolean work();
    
    public void notifyLive(JBCMethod method);
    public void notifyLive(JBCMethod method, ExternalFlowgraph spec);
    public void notifyLive(String flowgraphName);
    
    public void addQueryFamily(JBCQueryFamily family);
    public void removeQueryFamily(JBCQueryFamily family);
    
    public void addSourceDatum(JBCQueryFamily family, JBCMethod method, int offset, JBCExpressionContext context, JBCExpression expression, Object intermediate);
    public void addSourceDatum(JBCQueryFamily family, JBCMethod method, ExternalCFGNode node, JBCExpressionContext context, JBCExpression expression, Object intermediate);
    public void addSourceDatum(JBCQueryFamily family, String name, ExternalCFGNode node, JBCExpressionContext context, JBCExpression expression, Object intermediate);
    
/* Target cookies must not be IdentityCompactSets! */
    public void addTargetDatum(JBCQueryFamily family, JBCMethod method, int offset, JBCExpressionContext context, JBCExpression expression, Object targetCookie);
    public void addTargetDatum(JBCQueryFamily family, JBCMethod method, ExternalCFGNode node, JBCExpressionContext context, JBCExpression expression, Object targetCookie);
    public void addTargetDatum(JBCQueryFamily family, String name, ExternalCFGNode node, JBCExpressionContext context, JBCExpression expression, Object targetCookie);
}
