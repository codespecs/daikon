/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.benchmarks;

import java.util.*;
import ajax.jbc.*;
import ajax.jbc.util.*;
import ajax.analyzer.*;
import ajax.analyzer.util.*;
import ajax.util.*;

public class ObjectModellerQuery implements ResultListener, DatumSpecifier {
    private CompactSet targets;
    private CompactSet sourcesAndTargets;
    private JBCLocation location;
    private Hashtable results = new Hashtable();
    
    ObjectModellerQuery(JBCLocation location, CompactSet targets, CompactSet sourcesAndTargets) {
        this.location = location;
        this.targets = targets;
        this.sourcesAndTargets = sourcesAndTargets;
    }

    ObjectModellerQuery(JBCLocation location, CompactSet sourcesAndTargets) {
        this(location, null, sourcesAndTargets);
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
            
            if (targets != null) {
                for (Enumeration e = targets.elements(); e.hasMoreElements();) {
                    JBCExpression node = (JBCExpression)e.nextElement();
                    
                    family.addTargetDatum(offset, node, node);
                }
            }
            
            for (Enumeration e = sourcesAndTargets.elements(); e.hasMoreElements();) {
                JBCExpression node = (JBCExpression)e.nextElement();
                
                family.addSourceDatum(offset, node, node);
                family.addTargetDatum(offset, node, node);
            }
        }
    }
    
    Hashtable getResults() {
        return results;
    }
}
