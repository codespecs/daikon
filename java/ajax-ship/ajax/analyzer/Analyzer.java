/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer;

import ajax.jbc.*;
import ajax.jbc.util.reflect.*;
import ajax.util.*;
import java.util.*;
import ajax.Globals;
import ajax.analyzer.semantics.*;

/**
An Analyzer produces semantic information to be consumed by tools.

To perform an analysis, construct an Analyzer object and pass it the
GenericAnalyzer representing the particular algorithm to use. Then
repeatedly call work() until it returns false. The Analyzer will analyze
the program associated with whatever JBCWorld he GenericAnalyzer has
been configured with.

To actually get data out of the Analyzer, create a JBCQueryFamily passing
the Analyzer object as a construction parameter. Refer to JBCQueryFamily
for more information.

See ajax.analyzer.semi.test.Test1 for example code that performans an
analysis.

All the operations on the Analyzer, its analyses, and the underlying
JBCWorld should be peformed by the same thread and NEVER during a work()
call on the Analyzer (i.e. don't do these things inside an updateResult()
callback on a JBCQueryFamily).

An Analyzer object drives the analysis by keeping track of which methods
may be live (i.e. actually called during execution of the program). This
is done by a LivenessQueryFamily object. Whenever the analysis algorithm
proves that a method may be called, the method is added to a queue. The
elements of this queue are fed back to be analyzed themselves. The process
is bootstrapped by observing when the JBCWorld receives an addMainInvocation
method call, indicating that a particular native method is being called as
the "primordial invocation" of the program.
*/
public class Analyzer implements JBCObserver, GenericAnalyzerConsumer {
    private GenericAnalyzer producer;
    private IdentityCompactSet queryFamilies = new IdentityCompactSet();
    private CompactSet analyzedCode = new CompactSet();
    private CompactSet notifiedCode = new CompactSet();
    private Vector liveData = null;
    private Vector additionalConsumers = null;
    private AnalyzerStats stats = null;
    private ReflectionHandler reflectionHandler = null;
    private boolean logging = false;
    private int iterations = 0;
    private boolean disabledNewQueryFamilies = false;
    private int iterationLimit = Integer.MAX_VALUE;
    private String iterationLimitFunction = null;
    private String lastFunction = null;
    private int lastFunctionIteration;
    private boolean resolveLivenessFirst = true;
    private CoveragePolicy policy = new CoveragePolicy();
    private Hashtable methodFlowgraphs = new Hashtable();
    
    private boolean dumpIterations;
    
/**
Construct an Analyzer that drives the analysis using a given algorithm

@param producer the analysis algorithm to use
*/
    public Analyzer(GenericAnalyzer producer, boolean dumpIterations) {
        this.dumpIterations = dumpIterations;
        init(producer);
    }
    
    public Analyzer(GenericAnalyzer producer) {
        this(producer, false);
    }
    
    public boolean isNewQueryFamilyDisabled() {
        return disabledNewQueryFamilies;
    }
    
    public void setSemantics(Semantics s) {
        producer.setSemantics(s);
    }
    
    public void setReflectionHandler(ReflectionHandler reflectionHandler) {
        this.reflectionHandler = reflectionHandler;
    }

    private void init(GenericAnalyzer producer) {
        this.producer = producer;
        
        getWorld().addObserver(this);
        producer.setConsumer(this);
        producer.setSemantics(StandardSemantics.get());
        
        (new LivenessQueryFamily(this)).start();
    }
    
    public void setCoveragePolicy(CoveragePolicy policy) {
        this.policy = policy;
    }
    
    public CoveragePolicy getCoveragePolicy() {
        return policy;
    }
    
    public void enableLogging() {
        enableLogging(true);
    }
    
    public void enableLogging(boolean enabled) {
        logging = enabled;
    }
    
    public void setDelayLiveProcessing() {
        resolveLivenessFirst = false;
    }
    
    public void setStatisticsListener(AnalyzerStats stats) {
        if (Globals.debug && this.stats != null) {
            Globals.nonlocalError("Overwriting Analyzer.stats!");
        }
        
        this.stats = stats;
    }
    
    public int getIterations() {
        return iterations;
    }
    
    public void setIterationLimit(int iterationLimit, String inFunction) {
        this.iterationLimit = iterationLimit;
        this.iterationLimitFunction = inFunction;
    }
    
/**
Do the analysis work.

@return true if more work remains to be done after this method returns
*/
    public boolean work() throws PrematureTerminationException {
    /* make stuff live before we run the solver.
        This should be faster, because the more information you have about
        the constraint network, the faster the solver should be able to
        converge to the solution. */
        boolean moreToDo =
            producer.workBeforeLiveProcessing()
                || (resolveLivenessFirst
                    ? (processLiveItems() || producer.work())
                    : (producer.work() || processLiveItem()));
                    
        if (moreToDo) {
            iterations++;
            if (dumpIterations) {
                Globals.writeLog(this, "Iteration: " + iterations);
            }
                
            Globals.flushLog();
                
            if (iterationLimitFunction == null) {
                if (iterations >= iterationLimit) {
                    throw new PrematureTerminationException("Iteration limit reached");
                }
            } else if (lastFunction.indexOf(iterationLimitFunction) >= 0
                && iterations - lastFunctionIteration >= iterationLimit) {
                throw new PrematureTerminationException("Function analysis reached");
            }
        } else {
            for (Enumeration e = queryFamilies.elements(); e.hasMoreElements();) {
                ((JBCQueryFamily)e.nextElement()).notifyAnalysisComplete();
            }
        }
            
        return moreToDo;
    }
    
    public JBCWorld getWorld() {
        return producer.getWorld();
    }
    
    public void disableNewQueryFamilies() {
        disabledNewQueryFamilies = true;
    }

    public void updateResult(JBCQueryFamily family, Object targetCookie, Object intermediate) {
        family.updateResult(targetCookie, intermediate);
        
        if (additionalConsumers != null) {
            for (Enumeration e = additionalConsumers.elements(); e.hasMoreElements();) {
                ((GenericAnalyzerConsumer)e.nextElement()).updateResult(family, targetCookie, intermediate);
            }
        }
    }
    
    private void analyzeSpecification(JBCMethod method, ExternalFlowgraph fg, boolean isChangedItem) {
        Vector nodes = ExternalFlowgraphUtilities.getDefNodes(fg);
                                        
        lastFunction = method.toString();
        lastFunctionIteration = iterations;
        if (!isChangedItem) {
            producer.notifyLive(method, fg);
        }
                                     
        methodFlowgraphs.put(method, fg);
        for (Enumeration e2 = queryFamilies.elements(); e2.hasMoreElements();) {
            ((JBCQueryFamily)e2.nextElement()).examineCode(method, fg, nodes);
        }
    }
    
    private void handleUnknownMethod(JBCMethod method, boolean isChangedItem) {
        int disposition = getCoveragePolicy().getUnknownMethodDisposition(method);
        
        getCoveragePolicy().notifyUnknownCode(method);

        if ((disposition & CoveragePolicy.OPTIMISTIC) != 0) {
            if (logging) {
                Globals.writeLog(this, "Using optimistic handling for unknown " + method);
            }
        } else {
            ExternalFlowgraph fg = getWorld().getUnknownBehavior(method);
            
            if (fg == null) {
                Globals.userError("Cannot perform pessimistic handling for unknown " + method);
            } else {
                if (logging) {
                    Globals.writeLog(this, "Generating pessimistic constraints for " + lastFunction);
                }

                analyzeSpecification(method, fg, isChangedItem);
            }
        }
    }
    
/**
This is called once for each method or flowgraph function known to be
live.
*/
    private void processLiveItem(Object liveObj, boolean isChangedItem) {
        if (liveObj instanceof JBCMethod) {
            JBCMethod method = (JBCMethod)liveObj;
            
            if (!isChangedItem && reflectionHandler != null) {
                reflectionHandler.notifyLiveMethod(method);
            }
        
            if (Globals.debug && method.isAbstract()) {
                Globals.localError("Live " + method + " is abstract!");
            } else if (!getCoveragePolicy().isMethodKnowable(method)) {
                handleUnknownMethod(method, isChangedItem);
            } else if (method.isNative()) {
                ExternalFlowgraph fg = getWorld().getNativeCode(method);
                                
                if (fg == null || !producer.isNativeSpecificationAccurate()) {
                    handleUnknownMethod(method, isChangedItem);
                } else {
                    if (logging) {
                        Globals.writeLog(this, "Generating constraints for native " + method);
                    }

                    analyzeSpecification(method, fg, isChangedItem);
                }
            } else {
                byte[] code = method.getData().getCode();
                boolean[] instructions = JBCCodeUtilities.getInstructionStarts(method.getData());

                lastFunction = method.toString();
                lastFunctionIteration = iterations;
                if (logging) {
                    Globals.writeLog(this, "Generating constraints for " + lastFunction);
                }
                if (!isChangedItem) {
                    producer.notifyLive(method);
                }
                            
                for (Enumeration e2 = queryFamilies.elements(); e2.hasMoreElements();) {
                    ((JBCQueryFamily)e2.nextElement()).examineCode(method, code, instructions);
                }
            }
        } else {
            String flowgraphName = (String)liveObj;
            ExternalFlowgraph fg = getWorld().getNativeCode(flowgraphName);
                    
            if (fg == null) {
                Globals.userError("Specification not found for native function " + flowgraphName);
            } else {
                Vector nodes = ExternalFlowgraphUtilities.getDefNodes(fg);
                        
                lastFunction = flowgraphName;
                lastFunctionIteration = iterations;
                if (logging) {
                    Globals.writeLog(this, "Generating constraints for native " + lastFunction);
                }
                if (!isChangedItem) {
                    producer.notifyLive(flowgraphName);
                }
                
                for (Enumeration e2 = queryFamilies.elements(); e2.hasMoreElements();) {
                    ((JBCQueryFamily)e2.nextElement()).examineCode(flowgraphName, fg, nodes);
                }
            }
        }

        if (!isChangedItem) {
            analyzedCode.addUnconditionally(liveObj);
            notifiedCode.remove(liveObj);
        }
    }
    
    private boolean processLiveItem() {
        if (liveData == null) {
            return false;
        } else {
            Object liveItem = liveData.elementAt(0);
            
            liveData.removeElementAt(0);
            
            if (liveData.size() == 0) {
                liveData = null;
            }
            
            processLiveItem(liveItem, false);
            return true;
        }
    }
    
    private boolean processLiveItems() {
        if (liveData == null) {
            return false;
        } else {
            Enumeration e = liveData.elements();
            
            liveData = null;
            
            while (e.hasMoreElements()) {
                processLiveItem(e.nextElement(), false);
            }
            
            return true;
        }
    }
    
    public boolean isCodeLive(Object code) {
        return analyzedCode.get(code) != null
            || notifiedCode.get(code) != null;
    }
    
    private void internalNotifyLive(Object code) {
        if (!isCodeLive(code)) {
            if (liveData == null) {
                liveData = new Vector();
            }
            
            liveData.addElement(code);
            notifiedCode.addUnconditionally(code);
            
            if (stats != null) {
                if (code instanceof JBCMethod) {
                    stats.notifyMethodLive((JBCMethod)code);
                } else {
                    stats.notifyNativeCodeLive((String)code);
                }
            }
        }
    }
    
    void notifyLive(JBCMethod method) {
        if (Globals.debug && method.isAbstract()) {
            Globals.localError("Live " + method + " is abstract!");
        } 
        
        internalNotifyLive(method);
    }
    
    void notifyLive(String flowgraphName) {
        internalNotifyLive(flowgraphName);
    }
    
    public void makeFlowgraphLive(String flowgraphName) {
        internalNotifyLive(flowgraphName);
        
        if (additionalConsumers != null) {
            for (Enumeration e = additionalConsumers.elements(); e.hasMoreElements();) {
                ((GenericAnalyzerConsumer)e.nextElement()).makeFlowgraphLive(flowgraphName);
            }
        }
    }
    
/* This code is no longer used, but it shouldn't hurt to leave it in. */
    void addConsumer(GenericAnalyzerConsumer consumer) {
        if (additionalConsumers == null) {
            additionalConsumers = new Vector();
        }
        
        additionalConsumers.addElement(consumer);
    }
    
    void addSourceDatum(JBCQueryFamily family, JBCMethod method, int offset, JBCExpressionContext context, JBCExpression expression, Object intermediate) {
        producer.addSourceDatum(family, method, offset, context, expression, intermediate);
    }
    
    void addSourceDatum(JBCQueryFamily family, String name, ExternalCFGNode node, JBCExpressionContext context, JBCExpression expression, Object intermediate) {
        producer.addSourceDatum(family, name, node, context, expression, intermediate);
    }
    
    void addSourceDatum(JBCQueryFamily family, JBCMethod method, ExternalCFGNode node, JBCExpressionContext context, JBCExpression expression, Object intermediate) {
        producer.addSourceDatum(family, method, node, context, expression, intermediate);
    }
    
    void addTargetDatum(JBCQueryFamily family, JBCMethod method, int offset, JBCExpressionContext context, JBCExpression expression, Object targetCookie) {
        producer.addTargetDatum(family, method, offset, context, expression, targetCookie);
    }
    
    void addTargetDatum(JBCQueryFamily family, String name, ExternalCFGNode node, JBCExpressionContext context, JBCExpression expression, Object targetCookie) {
        producer.addTargetDatum(family, name, node, context, expression, targetCookie);
    }
    
    void addTargetDatum(JBCQueryFamily family, JBCMethod method, ExternalCFGNode node, JBCExpressionContext context, JBCExpression expression, Object targetCookie) {
        producer.addTargetDatum(family, method, node, context, expression, targetCookie);
    }
    
    void addQueryFamily(JBCQueryFamily f) {
        if (disabledNewQueryFamilies) {
            Globals.nonlocalError("New query families are disabled");
        } else {
            JBCWorld world = getWorld();
            
            queryFamilies.addUnconditionally(f);
            producer.addQueryFamily(f);
            
            for (Enumeration e = analyzedCode.elements(); e.hasMoreElements();) {
                Object liveObj = e.nextElement();
                
                if (liveObj instanceof JBCMethod) {
                    JBCMethod method = (JBCMethod)liveObj;
                    ExternalFlowgraph fg = (ExternalFlowgraph)methodFlowgraphs.get(method);
                    
                    if (fg != null) {
                        Vector nodes = ExternalFlowgraphUtilities.getDefNodes(fg);
                            
                        f.examineCode(method, fg, nodes);
                    } else if (!method.isNative()) {
                        byte[] code = method.getData().getCode();
                        boolean[] instructions = JBCCodeUtilities.getInstructionStarts(method.getData());
                        
                        f.examineCode(method, code, instructions);
                    }
                } else {
                    String flowgraphName = (String)liveObj;
                    ExternalFlowgraph fg = world.getNativeCode(flowgraphName);
                    
                    if (fg != null) {
                        Vector nodes = ExternalFlowgraphUtilities.getDefNodes(fg);
                        
                        f.examineCode(flowgraphName, fg, nodes);
                    }
                }
            }
        }
    }

    void removeQueryFamily(JBCQueryFamily f) {
        queryFamilies.remove(f);
        producer.removeQueryFamily(f);
    }
    
    public void notifyClassLoaded(JBCClass c) {
    }
    
    public void notifyClassChanged(JBCClass c, ClassData from, ClassData to) {
        for (Enumeration e = c.getMethods(); e.hasMoreElements();) {
            JBCMethod m = (JBCMethod)e.nextElement();
            
            if (analyzedCode.get(m) != null) {
                processLiveItem(m, true);
            }
        }
    }
    
    public void notifyNativeCodeLoaded(JBCMethod m, ExternalFlowgraph fg) {
    }
    
    public void notifyNativeCodeLoaded(String name, ExternalFlowgraph fg) {
    }
    
    public void notifyNativeCodeChanged(JBCMethod m, ExternalFlowgraph from, ExternalFlowgraph to) {
        if (analyzedCode.get(m) != null) {
            processLiveItem(m, true);
        }
    }
    
    public void notifyNativeCodeChanged(String name, ExternalFlowgraph from, ExternalFlowgraph to) {
        if (analyzedCode.get(name) != null) {
            processLiveItem(name, true);
        }
    }
    
    public void notifyAddedMainInvocation(String name) {
        notifyLive(name);
    }
}
