/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.benchmarks;

import java.util.*;
import ajax.jbc.*;
import ajax.jbc.util.*;
import java.io.*;
import ajax.analyzer.*;
import ajax.analyzer.semantics.*;
import ajax.analyzer.util.*;
import ajax.util.*;
import ajax.util.graph.*;

class ObjectModellerClassQuery implements ResultListener, DatumSpecifier {
    private CompactSet newNodes;
    private JBCLocation location;
    private Hashtable results = new Hashtable();
    
    ObjectModellerClassQuery(JBCLocation location, CompactSet newNodes) {
        this.location = location;
        this.newNodes = newNodes;
    }

    public void registerTarget(Object targetCookie) {
    }
    
    public void notifyAnalysisComplete() {
    }
    
    public void updateResult(Object targetCookie, Object intermediate) {
        if (intermediate != null) {
            results.put(targetCookie, intermediate);
        } else {
            results.remove(targetCookie);
        }
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, ExternalFlowgraph fg, Vector externalNodes) {
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, String name, ExternalFlowgraph fg, Vector externalNodes) {
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, byte[] code, boolean[] instructions) {
        if (method.equals(location.getMethod())) {
            int offset = location.getOffset();
            
            for (Enumeration e = newNodes.elements(); e.hasMoreElements();) {
                JBCExpression node = (JBCExpression)e.nextElement();
                
                family.addTargetDatum(offset, node, node);
            }
        }
    }
    
    public void updateClassSets(Hashtable classSets) {
        for (Enumeration e = results.keys(); e.hasMoreElements();) {
            Object key = e.nextElement();
            
            classSets.put(key, results.get(key));
        }
    }
}
