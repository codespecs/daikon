/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.server;

import ajax.analyzer.*;
import ajax.Globals;
import ajax.tools.protocol.*;
import ajax.tools.misc.*;
import java.util.*;
import ajax.jbc.*;
import java.io.*;
import java.net.*;
import ajax.util.*;

/**
Protocol:

Client C connects to Server S:
(    ( C -->{AnalysisRequest} S     // Create analysis with given name
       ( S -->{AnalysisUpdate} C
       )*
     )
  || ( C -->{ClassDescriptor} S     // Request source
       S -->{SourceResponse} C      // Send source
     )
)*
*/
public class Server implements MessageHandler {
    private Analyzer analyzer;
    private boolean exiting = false;
    private Object analysisRemovalLock = new Object();
    private Vector analysesToRemove = null;
    private JBCClassLoader appClassLoader;
    private Hashtable analysesByPort = new Hashtable();
    
    protected Server(Analyzer analyzer, JBCClassLoader appClassLoader) throws IOException {
        super();
        
        this.analyzer = analyzer;
        this.appClassLoader = appClassLoader;
    }
    
    public void terminate() {
	    synchronized (this) {
	        exiting = true;
	        notify();
	    }
    }

    public static Server create(String serverName, Analyzer analyzer, JBCClassLoader appClassLoader) {
        try {
            Server s = new Server(analyzer, appClassLoader);
            MessagePort port = new SocketMessagePort(new DummyHandler(), "localhost", ServerRegistry.DEFAULT_PORT);
            ServerSocket socket = new ServerSocket(0);

            SocketMessagePort.runServer(s, socket);
            
            port.sendMessage(new ServerData(serverName, socket.getLocalPort()));
            
            return s;
        } catch (Exception ex) {
            Globals.writeLog(null, "Failed to create server: " + ex.getMessage());
            return null;
        }
    }
    
    public void run() {
        try {
            while (!exiting && !Thread.interrupted()) {
                boolean firstReady = false;
                
                synchronized (this) {
                    synchronized (analysisRemovalLock) {
                        if (analysesToRemove != null) {
                            for (Enumeration e = analysesToRemove.elements(); e.hasMoreElements();) {
                                ((AnalysisServer)e.nextElement()).removedFromServer();
                            }
                            
                            analysesToRemove = null;
                        }
                    }
                    
                    if (!analyzer.work()) {
                        if (!firstReady) {
                            Globals.writeLog(this, "Analysis COMPLETE");
                            firstReady = true;
                        }
                        wait();
                    }
                }
            }
        } catch (InterruptedException ex) {
        } catch (PrematureTerminationException ex) {
        }
    }
    
    public void doMoreWork() {
        synchronized (this) {
            notify();
        }
    }
    
    public JBCClassLoader getAppClassLoader() {
        return appClassLoader;
    }
    
    private void queueRemoveAnalysis(AnalysisServer s) {
        synchronized (analysisRemovalLock) {
            if (analysesToRemove == null) {
                analysesToRemove = new Vector();
            }
            analysesToRemove.addElement(s);
        }
    }
    
    synchronized public void handleMessage(MessagePort port, Object o) {
        if (o instanceof AnalysisRequest) {
            loadAnalysis(port, (AnalysisRequest)o);
        } else if (o instanceof ClassDescriptor) {
            readSourceFile(port, (ClassDescriptor)o);
        } else if (o instanceof PortErrorMsg) {
            CompactSet analyses = (CompactSet)analysesByPort.remove(port);
            
            if (analyses != null) {
                for (Enumeration e = analyses.elements(); e.hasMoreElements();) {
                    queueRemoveAnalysis((AnalysisServer)e.nextElement());
                }
            }
        } else if (o instanceof PortStatusMsg) {
        } else {
            Globals.userError("Unknown message: " + o);
        }
    }
    
    private void readSourceFile(MessagePort port, ClassDescriptor forClass) {
	    String className = forClass.getClassName();
        JBCClass c = getAppClassLoader().getClass(className);
        String answer = null;

        if (c != null) {
		    answer = getAppClassLoader()
		        .loadClassSource(className, c.getSourceFileName());
	    }
	        
	    port.sendMessage(new SourceResponse(forClass, answer));
    }

    private void loadAnalysis(MessagePort port, AnalysisRequest request) {
        String analysis = request.getAnalysisName();
        
        if (analysis.startsWith("ajax.tools.server.")) {
            try {
                AnalysisServer s = (AnalysisServer)Class.forName(analysis).newInstance();
                
                s.init(this, analyzer, port, request);
                
                CompactSet list = (CompactSet)analysesByPort.get(port);
                
                if (list == null) {
                    list = new CompactSet();
                    analysesByPort.put(port, list);
                }
                list.addUnconditionally(s);
            } catch (ClassNotFoundException ex) {
                Globals.userError("Analysis not found: " + analysis);
            } catch (IllegalAccessException ex) {
                Globals.userError("Analysis not accessible: " + analysis);
            } catch (InstantiationException ex) {
                Globals.userError("Cannot create analysis: " + analysis);
            } catch (ClassCastException ex) {
                Globals.userError("Not a valid analysis: " + analysis);
            }
        } else {
            Globals.userError("Invalid analysis name: " + analysis);
        }
    }
}
