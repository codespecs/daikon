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

public class JGrepServer implements AnalysisServer, ResultListener, OpcodeConstants {
    private Analyzer analyzer;
    private Server server;
    private MessagePort port;
    private Hashtable queries = new Hashtable();
    private IndirectionQueryFamily queryEngine = null;
    private AnalysisRequest request;
    
    public JGrepServer() {
    }
    
    public void removedFromServer() {
        if (queryEngine != null) {
            queryEngine.dispose();
            queryEngine = null;
        }
    }
    
    public void notifyAnalysisComplete() {
    }
    
    private boolean isLocationRelevant(JBCLocation loc) {
        return loc.getMethod().getContainingClass().getClassLoader()
            .equals(server.getAppClassLoader());
    }
    
    public void registerTarget(Object targetCookie) {
        server.doMoreWork();
    }
    
    public void updateResult(Object targetCookie, Object intermediate) {
        JBCLocation loc = (JBCLocation)targetCookie;

        if (intermediate != null && isLocationRelevant(loc)) {
            JGrepUpdate update = makeUpdate(loc);

            if (update != null) {
                port.sendMessage(update);
            }
	}
    }
    
    private static JGrepUpdate makeUpdate(JBCLocation l) {
            JBCLocation j = (JBCLocation)l;
            JBCMethod method = j.getMethod();
            byte[] code = method.getData().getCode();
            int offset = j.getOffset();
            LocationDescriptor d = DescriptorFactory.makeDescriptor(l);
            
            switch (code[offset] & 0xFF) {
                case OP_invokevirtual:
                case OP_invokespecial:
                case OP_invokeinterface:
                    return new JGrepUpdate(JGrepUpdate.CALL, JBCCodeUtilities.resolveInstructionMethod(method, code, offset).getMethodName(), d);
                    
                case OP_putfield:
                    return new JGrepUpdate(JGrepUpdate.WRITE, JBCCodeUtilities.resolveInstructionField(method, code, offset).getFieldName(), d);
                    
                case OP_aastore:
                case OP_fastore:
                case OP_iastore:
                case OP_sastore:
                case OP_castore:
                case OP_bastore:
                case OP_dastore:
                case OP_lastore:
                    return new JGrepUpdate(JGrepUpdate.WRITE, "<arrayelement>", d);

                case OP_aaload:
                case OP_faload:
                case OP_daload:
                case OP_laload:
                case OP_iaload:
                case OP_saload:
                case OP_caload:
                case OP_baload:
                    return new JGrepUpdate(JGrepUpdate.READ, "<arrayelement>", d);
                    
                case OP_arraylength:
                    return new JGrepUpdate(JGrepUpdate.READ, "<arraylength>", d);
                    
                case OP_getfield:
                    return new JGrepUpdate(JGrepUpdate.READ, JBCCodeUtilities.resolveInstructionField(method, code, offset).getFieldName(), d);
                    
                case OP_newarray:
                case OP_anewarray:
                case OP_new:
                case OP_multianewarray:
                    return new JGrepUpdate(JGrepUpdate.NEW, JBCCodeUtilities.resolveInstructionClass(method, code, offset).getClassName(), d);
            }
            return null;
    }
    
    public void init(Server server, Analyzer analyzer, MessagePort port, AnalysisRequest request) {
        this.server = server;
        this.analyzer = analyzer;
        this.port = port;
        this.request = request;
        
        synchronized (server) {
            DatumSpecifier sources;
            String expr;
        
            if (request instanceof JGrepRequest) {
                expr = ((JGrepRequest)request).getExpression();
	    } else {
                port.dispose();
                return;
	    }

	    try {
	      sources = ExplicitValuePointSource.makeFromQuery(server.getAppClassLoader(), expr);
	    } catch (ParseException ex) {
	      port.sendMessage("Expression '" + expr + "' not understood: " +
                  ex.getMessage());
              port.dispose();
	      return;
	    }
        
	    Vector targets = new Vector();
        
            targets.addElement(new AnyFieldReadTarget());
            targets.addElement(new AnyFieldWriteTarget());
            targets.addElement(new NewObjectTarget());
            targets.addElement(new AnyMethodInvocationOnThisTarget());
        
	    DatumSpecifier[] specifiers = new DatumSpecifier[targets.size() + 1];
        
	    targets.copyInto(specifiers);
	    specifiers[specifiers.length - 1] = sources;
        
	    ResultListener[] listeners = { this };
        
            queryEngine = new SingletonTracker(analyzer, specifiers, listeners);
            queryEngine.start();
        }
    }
}
