/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi;

import ajax.jbc.*;
import ajax.solver.*;
import java.util.*;
import ajax.Globals;
import ajax.analyzer.semantics.*;
import ajax.analyzer.semi.constraints.*;
import java.io.*;
import ajax.solver.util.*;
import ajax.analyzer.*;
import ajax.util.SingletonEnumerator;
import java.util.zip.GZIPOutputStream;

public class SEMIAnalyzer implements GenericAnalyzer, AnalyzerEnvironment, VarObserver, JBCObserver, ConstraintAnnotator {
    private JBCWorld world;
    private World solver;
    private TokenManager tokenManager = Globals.debug ? new TokenManager() : null;
    private Hashtable familyData = new Hashtable();
    private JBCTypeManager typeManager =
        (Globals.debug && ConstraintManager.useControlFlowPolymorphism) ? new JBCTypeManager(this) : null;
    private ConstraintManager constraintManager;
    private GenericAnalyzerConsumer consumer = null;
    private Analyzer prephase;
    private boolean doneConfiguration = false;
    private Semantics semantics;
    
    private String dumpDirectory = "d:\\tmp\\";
   
    /* debugging only */
    private static int dumpedGraphNum = 0;
    
    private int numInstances;
    private int numSources;
    private int numComponents;
    private int numParents;

    public static final boolean applyPolarities = true; // CONFIG
    
    private static final Object DEFAULT_QUERY_FIELD = new String("DEFAULT_QUERY_FIELD");
    
    private SEMIAnalyzer(JBCWorld world, Analyzer prephase) {
        solver = new World();
        
        this.world = world;
        this.prephase = prephase;
        
        world.addObserver(this);
        solver.addVarObserver(this);

        constraintManager = new ConstraintManager(this);
    }
    
    public SEMIAnalyzer(Analyzer prephase) {
        this(prephase.getWorld(), prephase);
    }
    
    public SEMIAnalyzer(JBCWorld world) {
        this(world, null);
    }

    public boolean isNativeSpecificationAccurate() {
        return true;
    }
    
    public void setSemantics(Semantics s) {
        if (s instanceof StandardSemantics) {
            semantics = s;
        } else if (s instanceof CombiningSemantics) {
            semantics = s;
	    constraintManager.setTrackArrayIndices(((CombiningSemantics)semantics).isTrackingArrayIndices());
            // typeManager = null;
        } else if (s instanceof TransitiveSemantics) {
            semantics = s;
        } else {
            throw new Error("Semantics not supported by SEMI!");
        }
    }
    
    public Semantics getSemantics() {
        return semantics;
    }
    
    public String getDumpDirectory() {
    	return dumpDirectory;
    }

    public void setDumpDirectory(String s) {
	    dumpDirectory = s;
	    if (!dumpDirectory.endsWith(File.separator)) {
	        dumpDirectory = dumpDirectory + File.separator;
	    }
    }
    
    public void setConsumer(GenericAnalyzerConsumer consumer) {
        this.consumer = consumer;
    }
    
    public boolean isNewQueryFamilyDisabled() {
        return consumer.isNewQueryFamilyDisabled();
    }
    
    public void setCheckConsistency(boolean b) {
        solver.setCheckConsistency(b);
    }
    
    public void setDumpOperations(boolean b) {
        solver.setDumpOperations(b);
    }
    
    public void setCoalesceClusterLevelInstances(boolean b) {
        solver.setCoalesceClusterLevelInstances(b);
    }
    
    public void setUseSubchunks(boolean b) {
        constraintManager.setUseSubchunks(b);
    }
    
    public void setUseSubobjects(boolean b) {
        constraintManager.setUseSubobjects(b);
    }
    
    public void setBreakoutDowncastSubobjects(boolean b) {
        constraintManager.setBreakoutDowncastSubobjects(b);
    }
    
    public void setNondeterministicVirtuals(boolean nondeterministicVirtuals) {
        constraintManager.setNondeterministicVirtuals(nondeterministicVirtuals);
    }
    
    public void notifyVarsMerged(Variable to, Variable from) {
        if (tokenManager != null) {
            tokenManager.notifyVarsMerged(to, from);
        }
        
        if (typeManager != null) {
            typeManager.notifyVarsMerged(to, from);
        }
        
        for (Enumeration e = familyData.elements(); e.hasMoreElements();) {
            ((SEMIQueryFamily)e.nextElement()).notifyVarsMerged(to, from);
        }
    }
    
    public void notifyNewInstance(Variable instance, Variable of) {
        if (typeManager != null) {
            typeManager.notifyNewInstance(instance, of);
        }

        for (Enumeration e = familyData.elements(); e.hasMoreElements();) {
            ((SEMIQueryFamily)e.nextElement()).notifyNewInstance(instance, of);
        }
    }
    
    public void notifyNewComponent(Variable component, Variable of) {
    }
    
    public World getSolver() {
        return solver;
    }
    
    public JBCWorld getWorld() {
        return world;
    }
    
    public Analyzer getPrephase() {
        return prephase;
    }
    
    public void notifyClassChanged(JBCClass c, ClassData from, ClassData to) {
        for (Enumeration e = c.getMethods(); e.hasMoreElements();) {
            constraintManager.updateChange((JBCMethod)e.nextElement());
        }
    }
    
    public void notifyClassLoaded(JBCClass c) {
    }
    
    public void notifyNativeCodeLoaded(JBCMethod m, ExternalFlowgraph fg) {
        /* do nothing, we don't care */
    }
    
    public void notifyNativeCodeLoaded(String name, ExternalFlowgraph fg) {
        /* do nothing, we don't care */
    }
    
    public void notifyNativeCodeChanged(JBCMethod m, ExternalFlowgraph from, ExternalFlowgraph to) {
        constraintManager.updateChange(m, to);
    }
    
    public void notifyNativeCodeChanged(String name, ExternalFlowgraph from, ExternalFlowgraph to) {
        constraintManager.updateChange(name);
    }
    
    public void notifyAddedMainInvocation(String name) {
        constraintManager.notifyTopLevel(name);
    }
    
    public void addToken(Variable v, Token t) {
        if (solver.getDumpOperations()) {
            Globals.writeLog(this, "Setting token on " + Globals.getHexID(v.getHead())
                + " to " + t);
        }
        
        if (tokenManager != null) {
            tokenManager.addToken(v, t);
        }
    }
    
    public void removeToken(Variable v, Token t) {
        if (tokenManager != null) {
            tokenManager.removeToken(v, t);
        }
    }
    
    public Token getFirstToken(Variable v) {
        if (tokenManager != null) {
            return tokenManager.getFirstToken(v);
        } else {
            return null;
        }
    }
    
    public void setJBCType(Variable v, JBCType t) {
        if (typeManager != null) {
            typeManager.setJBCType(v, t);
        }
    }
    
    public String getVarLabel(Variable vHead) {
        StringBuffer buf = new StringBuffer();
        JBCType type = typeManager != null ? typeManager.getJBCType(vHead) : null;
        
        if (tokenManager != null) {
            buf.append(tokenManager.getVarLabel(vHead));
        }
        
        if (type != null) {
            buf.append("\n(type=").append(type).append(")");
        }
        
        return buf.toString();
    }
    
    public String dumpVarInfo(Variable v) {
        if (tokenManager != null) {
            return dumpVarInfo(new SingletonEnumerator(v));
        } else {
            return "<no token manager, no dump>";
        }
    }
    
    public String dumpVarInfo() {
        if (tokenManager != null) {
            return dumpVarInfo(tokenManager.getTokenVars());
        } else {
            return "<no token manager, no dump>";
        }
    }
    
    private boolean isConsistent() {
        try {
            StringWriter buf = new StringWriter();
            
            numInstances = 0;
            numSources = 0;
            numParents = 0;
            numComponents = 0;
            
            SolverDebug.dumpVarInfo(this, buf, getSolver(), tokenManager.getTokenVars());
            
            if (numInstances != numSources) {
                buf.write("// Source/instance count mismatch: " + numSources + ", " + numInstances);
            }
            
            if (numComponents != numParents) {
                buf.write("// Parent/component count mismatch: " + numParents + ", " + numComponents);
            }
            
            String s = buf.toString();
            
            return s.indexOf("INVALID: Missing") < 0 && s.indexOf("INVALID: Mismatched") < 0;
        } catch (IOException ex) {
            return false;
        }
    }
    
    public String dumpVarInfo(Enumeration vars) {
        try {
            String fileName = getDumpDirectory() + "vardump" + dumpedGraphNum;
            Writer w = new BufferedWriter(new OutputStreamWriter(
                new FileOutputStream(fileName)));
            
            dumpedGraphNum++;
            
            numInstances = 0;
            numSources = 0;
            numParents = 0;
            numComponents = 0;
            
            SolverDebug.dumpVarInfo(this, w, getSolver(), vars);
            
            if (numInstances != numSources) {
                w.write("// Source/instance count mismatch: " + numSources + ", " + numInstances);
            }
            
            if (numComponents != numParents) {
                w.write("// Parent/component count mismatch: " + numParents + ", " + numComponents);
            }
            
            w.close();
            
            return fileName;
        } catch (IOException ex) {
            return "<error writing dump file>";
        }
    }
    
    public String dumpQueryInfo() {
        try {
            String fileName = getDumpDirectory() + "querydump";
            Writer w = new BufferedWriter(new OutputStreamWriter(
                new FileOutputStream(fileName)));
            
            for (Enumeration e = familyData.elements(); e.hasMoreElements();) {
                ((SEMIQueryFamily)e.nextElement()).dump(w);
            }

            w.close();
            
            return fileName;
        } catch (IOException ex) {
            return "<error writing query dump file>";
        }
    }
    
    public void countSource() {
        numSources++;
    }
    
    public void countInstance() {
        numInstances++;
    }
    
    public void countParent() {
        numParents++;
    }
    
    public void countComponent() {
        numComponents++;
    }
    
    ConstraintManager getConstraintManager() {
        return constraintManager;
    }

    public boolean workBeforeLiveProcessing() throws PrematureTerminationException {
        if (!doneConfiguration) {
            doneConfiguration = true;
            constraintManager.configureAnalyses();
        }
        
        return prephase != null && prephase.work();
    }

    public boolean work() {
        boolean result = solver.work();
        
        return result && (!solver.isCheckingConsistency() || isConsistent());
    }
    
    public void makeFlowgraphLive(String name) {
        consumer.makeFlowgraphLive(name);
    }
    
    public void notifyLive(JBCMethod method) {
        constraintManager.notifyLive(method);
    }
    
    public void notifyLive(JBCMethod method, ExternalFlowgraph fg) {
        constraintManager.notifyLive(method, fg);
    }
    
    public void notifyLive(String flowgraphName) {
        constraintManager.notifyLive(flowgraphName);
    }
    
    public void addQueryFamily(JBCQueryFamily family) {
        familyData.put(family, new SEMIQueryFamily(getSolver(), family, consumer));
    }
    
    public void removeQueryFamily(JBCQueryFamily family) {
        familyData.remove(family);
    }
    
    private static Object getQueryField(JBCExpression expression) {
        Object queryField = expression.getQueryField();
        
        return queryField == null ? DEFAULT_QUERY_FIELD : queryField;
    }
    
    private SEMIQueryFamily getFamilyData(JBCQueryFamily family) {
        return (SEMIQueryFamily)familyData.get(family);
    }
    
    public void addSourceDatum(JBCQueryFamily family, JBCMethod method, int offset, JBCExpressionContext context, JBCExpression expression, Object intermediate) {
        getFamilyData(family)
            .addSourceDatum(
                constraintManager.getVar(method, offset, expression.removeQueryField(), false),
                context, intermediate, getQueryField(expression));
    }
    
    public void addSourceDatum(JBCQueryFamily family, String name, ExternalCFGNode node, JBCExpressionContext context, JBCExpression expression, Object intermediate) {
        getFamilyData(family)
            .addSourceDatum(
                constraintManager.getVar(name, node, expression.removeQueryField(), false),
                context, intermediate, getQueryField(expression));
    }
    
    public void addSourceDatum(JBCQueryFamily family, JBCMethod method, ExternalCFGNode node, JBCExpressionContext context, JBCExpression expression, Object intermediate) {
        getFamilyData(family)
            .addSourceDatum(
                constraintManager.getVar(method, node, expression.removeQueryField(), false),
                context, intermediate, getQueryField(expression));
    }
    
    public void addTargetDatum(JBCQueryFamily family, JBCMethod method, int offset, JBCExpressionContext context, JBCExpression expression, Object targetCookie) {
        getFamilyData(family)
            .addTargetDatum(
                constraintManager.getVar(method, offset, expression.removeQueryField(), true),
                context, targetCookie, getQueryField(expression));
    }
    
    public void addTargetDatum(JBCQueryFamily family, String name, ExternalCFGNode node, JBCExpressionContext context, JBCExpression expression, Object targetCookie) {
        getFamilyData(family)
            .addTargetDatum(
                constraintManager.getVar(name, node, expression.removeQueryField(), true),
                context, targetCookie, getQueryField(expression));
    }

    public void addTargetDatum(JBCQueryFamily family, JBCMethod method, ExternalCFGNode node, JBCExpressionContext context, JBCExpression expression, Object targetCookie) {
        getFamilyData(family)
            .addTargetDatum(
                constraintManager.getVar(method, node, expression.removeQueryField(), true),
                context, targetCookie, getQueryField(expression));
    }

    public void dumpInfo() {
        dumpVarInfo();
        // dumpClusterInfo();
        dumpQueryInfo();
    }
}
