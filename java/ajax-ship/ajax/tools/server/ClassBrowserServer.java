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
import ajax.util.*;

public class ClassBrowserServer implements AnalysisServer {
    private Analyzer analyzer;
    private Server server;
    private MessagePort port;
    private AnalysisRequest request;
    private CompactSet classes = new CompactSet();
    
    public ClassBrowserServer() {
    }
    
    public void removedFromServer() {
    }
    
    public void notifyAnalysisComplete() {
    }
    
    private boolean isMethodRelevant(JBCMethod m) {
        return m.getContainingClass().getClassLoader()
            .equals(server.getAppClassLoader());
    }
    
    public void init(Server server, Analyzer analyzer, final MessagePort port, AnalysisRequest request) {
        this.server = server;
        this.analyzer = analyzer;
        this.port = port;
        this.request = request;
        
        synchronized (server) {
            DatumSpecifier[] specifiers = { new DatumSpecifier() {
	            public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, ExternalFlowgraph fg, Vector externalNodes) {}
                    public void identifyQueryData(IndirectionQueryFamily family, String name, ExternalFlowgraph fg, Vector externalNodes) {}
                    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, byte[] code, boolean[] instructions) {
  		        if (isMethodRelevant(method)) {
		            JBCClass c = method.getContainingClass();
			    if (classes.get(c) == null) {
			        classes.add(c);
				port.sendMessage(DescriptorFactory.makeDescriptor(c));
			    }
			}
		    }
                }};
            ResultListener[] listeners = {};
                        
            ClassTracker queryEngine = new ClassTracker(analyzer, specifiers, listeners);
            queryEngine.start();
        }
    }
}
