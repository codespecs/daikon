/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.hybrid;

import ajax.analyzer.*;
import ajax.analyzer.semantics.*;
import ajax.util.*;
import ajax.jbc.*;
import java.util.*;
import ajax.Globals;

public class HybridAnalysis implements GenericAnalyzer {
    private GenericAnalyzerConsumer consumer = null;
    private HybridConsumer[] wrappers = null;
    private GenericAnalyzer[] sources;
    private int worker = 0;
    
    GenericAnalyzerConsumer getConsumer() {
        return consumer;
    }
    
    public HybridAnalysis(GenericAnalyzer[] sources) {
        this.sources = sources;
        
        if (Globals.debug && sources.length < 1) {
            Globals.nonlocalError("Must have at least one source for hybridization");
        }
        if (Globals.debug) {
            for (int i = 1; i < sources.length; i++) {
                if (!sources[i].getWorld().equals(getWorld())) {
                    Globals.nonlocalError("JBCWorld mismatch");
                }
            }
        }
    }
    
    public boolean isNativeSpecificationAccurate() {
        for (int i = 0; i < sources.length; i++) {
            if (!sources[i].isNativeSpecificationAccurate()) {
                return false;
            }
        }
        
        return true;
    }
    
    public void setSemantics(Semantics s) {
        for (int i = 0; i < sources.length; i++) {
            sources[i].setSemantics(s);
        }
    }
    
    public void setConsumer(GenericAnalyzerConsumer consumer) {
        this.consumer = consumer;
        
        wrappers = new HybridConsumer[sources.length];
        for (int i = 0; i < wrappers.length; i++) {
            HybridConsumer wrapper = new HybridConsumer(this, i);
            
            wrappers[i] = wrapper;
            sources[i].setConsumer(wrapper);
        }
    }
    
    public JBCWorld getWorld() {
        return sources[0].getWorld();
    }
    
    public CoveragePolicy getCoveragePolicy() {
        return consumer.getCoveragePolicy();
    }

    public boolean workBeforeLiveProcessing() throws PrematureTerminationException {
        for (int numTries = 0; numTries < sources.length; numTries++) {
            if (sources[worker].workBeforeLiveProcessing()) {
                return true;
            }
            
            worker++;
            if (worker >= sources.length) {
                worker = 0;
            }
        }
        
        return false;
    }

    public boolean work() {
        for (int numTries = 0; numTries < sources.length; numTries++) {
            if (sources[worker].work()) {
                return true;
            }
            
            worker++;
            if (worker >= sources.length) {
                worker = 0;
            }
        }
        
        return false;
    }
    
    public void notifyLive(JBCMethod method) {
        for (int i = 0; i < sources.length; i++) {
            sources[i].notifyLive(method);
        }
    }
    
    public void notifyLive(JBCMethod method, ExternalFlowgraph fg) {
        for (int i = 0; i < sources.length; i++) {
            sources[i].notifyLive(method, fg);
        }
    }
    
    public void notifyLive(String flowgraphName) {
        for (int i = 0; i < sources.length; i++) {
            sources[i].notifyLive(flowgraphName);
        }
    }
    
    public void addQueryFamily(JBCQueryFamily family) {
        for (int i = 0; i < sources.length; i++) {
            sources[i].addQueryFamily(family);
        }
        for (int i = 0; i < sources.length; i++) {
            wrappers[i].addQueryFamily(family);
        }
    }
    
    public void removeQueryFamily(JBCQueryFamily family) {
        for (int i = 0; i < sources.length; i++) {
            sources[i].removeQueryFamily(family);
        }
        for (int i = 0; i < sources.length; i++) {
            wrappers[i].removeQueryFamily(family);
        }
    }

    void updateResult(JBCQueryFamily family, Object targetCookie) {
        Object[] results = new Object[sources.length];
        
        for (int i = 0; i < sources.length; i++) {
            Object element = wrappers[i].getResult(family, targetCookie);
            
            if (element == null) {
                consumer.updateResult(family, targetCookie, null);
                return;
            } else {
                results[i] = element;
            }
        }
        
        Object result = results[0];
        
        for (int i = 1; i < results.length; i++) {
            result = family.intersectIntermediates(result, results[i]);
        }
        
        consumer.updateResult(family, targetCookie, result);
    }
    
    public void addSourceDatum(JBCQueryFamily family, JBCMethod method, int offset, JBCExpressionContext context, JBCExpression expression, Object intermediate) {
        for (int i = 0; i < sources.length; i++) {
            sources[i].addSourceDatum(family, method, offset, context, expression, intermediate);
        }
    }
    
    public void addSourceDatum(JBCQueryFamily family, JBCMethod method, ExternalCFGNode defNode, JBCExpressionContext context, JBCExpression expression, Object intermediate) {
        for (int i = 0; i < sources.length; i++) {
            sources[i].addSourceDatum(family, method, defNode, context, expression, intermediate);
        }
    }
    
    public void addSourceDatum(JBCQueryFamily family, String name, ExternalCFGNode defNode, JBCExpressionContext context, JBCExpression expression, Object intermediate) {
        for (int i = 0; i < sources.length; i++) {
            sources[i].addSourceDatum(family, name, defNode, context, expression, intermediate);
        }
    }
    
    public void addTargetDatum(JBCQueryFamily family, JBCMethod method, int offset, JBCExpressionContext context, JBCExpression expression, Object targetCookie) {
        for (int i = 0; i < sources.length; i++) {
            sources[i].addTargetDatum(family, method, offset, context, expression, targetCookie);
        }
    }
    
    public void addTargetDatum(JBCQueryFamily family, JBCMethod method, ExternalCFGNode defNode, JBCExpressionContext context, JBCExpression expression, Object targetCookie) {
        for (int i = 0; i < sources.length; i++) {
            sources[i].addTargetDatum(family, method, defNode, context, expression, targetCookie);
        }
    }
    
    public void addTargetDatum(JBCQueryFamily family, String name, ExternalCFGNode defNode, JBCExpressionContext context, JBCExpression expression, Object targetCookie) {
        for (int i = 0; i < sources.length; i++) {
            sources[i].addTargetDatum(family, name, defNode, context, expression, targetCookie);
        }
    }
}
