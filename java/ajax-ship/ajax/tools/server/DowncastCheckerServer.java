/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.server;

import ajax.analyzer.*;
import ajax.analyzer.util.*;
import ajax.tools.protocol.*;
import java.util.*;
import ajax.jbc.*;
import ajax.jbc.util.*;

public class DowncastCheckerServer implements AnalysisServer, ResultListener {
    private Analyzer analyzer;
    private Server server;
    private MessagePort port;
    private Hashtable queries = new Hashtable();
    private IndirectionQueryFamily queryEngine = null;
    private AnalysisRequest request;
    
    public DowncastCheckerServer() {
    }
    
    public void removedFromServer() {
        if (queryEngine != null) {
            queryEngine.dispose();
            queryEngine = null;
        }
    }
    
    public void notifyAnalysisComplete() {
        for (Enumeration e = queries.keys(); e.hasMoreElements();) {
            JBCLocation loc = (JBCLocation)e.nextElement();
            DowncastCheckerData data = (DowncastCheckerData)queries.get(loc);
                    
            if (data.isInBound()) {
                Object intermediate = data.getIntermediate();
                JBCClass actual;
                            
                if (intermediate == null || intermediate instanceof JBCClass) {
                    actual = (JBCClass)intermediate;
                } else {
                    actual = ((JBCClass[])intermediate)[0];
                }
                            
                DowncastCheckerUpdate update = new DowncastCheckerUpdate(request.getAnalysisToken(),
                    DescriptorFactory.makeDescriptor(loc), DowncastCheckerUpdate.SAFE);
                
                update.setFoundBound(DescriptorFactory.makeDescriptor(actual));
                port.sendMessage(update);
            }
        }
    }
    
    private boolean isLocationRelevant(JBCLocation loc) {
        return loc.getMethod().getContainingClass().getClassLoader()
            .equals(server.getAppClassLoader());
    }
    
    public void registerTarget(Object targetCookie) {
        server.doMoreWork();
        
        JBCLocation loc = (JBCLocation)targetCookie;
            
        if (isLocationRelevant(loc)) {
            DowncastCheckerData data = new DowncastCheckerData(loc);
            DowncastCheckerUpdate update = new DowncastCheckerUpdate(request.getAnalysisToken(),
                DescriptorFactory.makeDescriptor(loc), DowncastCheckerUpdate.UNKNOWN);
            
            update.setActualBound(DescriptorFactory.makeDescriptor(data.getBound()));
            port.sendMessage(update);
                
            queries.put(loc, data);
        }
    }
    
    public void updateResult(Object targetCookie, Object intermediate) {
        JBCLocation loc = (JBCLocation)targetCookie;
        DowncastCheckerData data = (DowncastCheckerData)queries.get(loc);
            
        if (data != null) {
            boolean alreadyInError = data.isInError();
             
            data.updateResult(intermediate);
                
            if (!alreadyInError && data.isInError()) {
                port.sendMessage(
                    new DowncastCheckerUpdate(request.getAnalysisToken(),
                        DescriptorFactory.makeDescriptor(loc), DowncastCheckerUpdate.UNSAFE));
            }
        }
    }
    
    public void init(Server server, Analyzer analyzer, MessagePort port, AnalysisRequest request) {
        this.server = server;
        this.analyzer = analyzer;
        this.port = port;
        this.request = request;
        
        synchronized (server) {
            DatumSpecifier[] specifiers = { new DowncastInputTarget(), new NewObjectClassSource() };
            ResultListener[] listeners = { this };
                        
            queryEngine = new ClassTracker(analyzer, specifiers, listeners);
            queryEngine.start();
        }
    }
}
