/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.hybrid;

import ajax.analyzer.*;
import ajax.util.*;
import ajax.jbc.*;
import java.util.*;
import ajax.Globals;

class HybridConsumer implements GenericAnalyzerConsumer {
    private HybridAnalysis hybrid;
    private Hashtable familiesToCookieMemories = new Hashtable();
    
    HybridConsumer(HybridAnalysis hybrid, int index) {
        this.hybrid = hybrid;
    }
    
    public boolean isNewQueryFamilyDisabled() {
        return hybrid.getConsumer().isNewQueryFamilyDisabled();
    }
    
    void addQueryFamily(JBCQueryFamily family) {
        familiesToCookieMemories.put(family, new Hashtable());
    }
    
    void removeQueryFamily(JBCQueryFamily family) {
        familiesToCookieMemories.remove(family);
    }
    
    public void updateResult(JBCQueryFamily family, Object targetCookie, Object intermediate) {
        Hashtable cookieMemory = (Hashtable)familiesToCookieMemories.get(family);
        
        if (intermediate != null) {
            cookieMemory.put(targetCookie, intermediate);
        } else {
            cookieMemory.remove(targetCookie);
        }
        hybrid.updateResult(family, targetCookie);
    }
    
    Object getResult(JBCQueryFamily family, Object targetCookie) {
        return ((Hashtable)familiesToCookieMemories.get(family)).get(targetCookie);
    }
    
    public void makeFlowgraphLive(String flowgraphName) {
        hybrid.getConsumer().makeFlowgraphLive(flowgraphName);
    }
    
    public CoveragePolicy getCoveragePolicy() {
        return hybrid.getCoveragePolicy();
    }
}
