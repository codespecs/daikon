/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.explain;

import ajax.Globals;
import ajax.jbc.*;
import ajax.analyzer.*;
import ajax.analyzer.util.*;
import ajax.util.*;

public class ExplanationGatherer implements OpcodeConstants {
    private boolean shortCircuitTermination = false;
    private boolean done = false;
    private ExplanationListener listener;
    private ExplanationSearch fromPoint1;
    private ExplanationSearch fromPoint2;
    private CompactSet fromPoint1Items = new CompactSet();
    private CompactSet fromPoint2Items = new CompactSet();
    private Analyzer analyzer;
    
    public ExplanationGatherer(Analyzer analyzer, ExplanationListener listener) {
        this.listener = listener;
        this.analyzer = analyzer;
        
        fromPoint1 = new ExplanationSearch(this, analyzer);
        fromPoint2 = new ExplanationSearch(this, analyzer);
        fromPoint1.start();
        fromPoint2.start();
    }
    
    public void addPoint1(JBCValuePoint p) {
        fromPoint1.addSourcePoint(p);
        fromPoint2.addTargetPoint(p);
    }
    
    public void addPoint2(JBCValuePoint p) {
        fromPoint2.addSourcePoint(p);
        fromPoint1.addTargetPoint(p);
    }
    
    void reportTermination(ExplanationSearch origin) {
        done = true;
        
        checkTermination();
    }
    
    private void checkTermination() {
        if (done && shortCircuitTermination) {
            listener.terminate();
        }
    }
    
    private void addItem(CompactSet origin, CompactSet other, SearchItem item) {
        if (origin.get(item) == null) {
            origin.addUnconditionally(item);
            if (other.get(item) != null) {
                listener.notifyReached(item);
            }
        }
    }
    
    void reportResult(ExplanationSearch origin, SearchItem item) {
        if (origin == fromPoint1) {
            listener.notifyReachedFromPoint1(item);
            addItem(fromPoint1Items, fromPoint2Items, item);
        } else if (!Globals.debug || origin == fromPoint2) {
            listener.notifyReachedFromPoint2(item);
            addItem(fromPoint2Items, fromPoint1Items, item);
        } else {
            Globals.localError("Unknown origin!");
        }
    }
    
    public void setShortCircuitTermination() {
        setShortCircuitTermination(true);
    }
    
    public void setShortCircuitTermination(boolean set) {
        shortCircuitTermination = set;
        
        checkTermination();
    }
    
    public void dispose() {
        fromPoint1.dispose();
        fromPoint2.dispose();
    }
}
