/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.benchmarks;

import ajax.Globals;
import java.util.*;
import ajax.jbc.*;
import ajax.jbc.util.*;
import java.io.*;
import ajax.analyzer.*;
import ajax.analyzer.semantics.*;
import ajax.analyzer.util.*;
import ajax.util.*;
import ajax.util.graph.*;

public class ObjectModeller extends Benchmark implements DatumSpecifier, OpcodeConstants {
    private JBCClassLoader appClassLoader;
    private GeneralBenchmark core;
    private String locationName;
    private JBCLocation location = null;
    private Analyzer analyzer;
    private CompactSet fieldSet = new CompactSet();
    private CompactSet userFieldSet = new CompactSet();
    private IndirectionQueryFamily fieldFinder = null;
    private Hashtable canonicalExpressionLabels = new Hashtable();
    private Hashtable canonicalExpressionClassSets = new Hashtable();
    private CompactSet liveCanonicalExpressions = new CompactSet();
    private CompactSet rootExpressions = null;
    private Hashtable fieldExpressionsToCanonicalExpressions = new Hashtable();
    private int dumpCount = 0;
    private boolean dumpDebugGraphs;
    private boolean includeGlobals;
    private boolean includeNonObjects;
    private boolean compressGraph;
    private boolean showClasses;
    private boolean mergeSuperclasses;
    private boolean pruneTopObject;
    private boolean pruneUselessSuperclasses;
    private int unlinkBlobSize;
    private CompactSet blobNodes = new CompactSet();
    private Hashtable unlink = new Hashtable();
    private String scopePackages;
    private Graph resultGraph = null;
    private JBCClass javaLangObject;
    private JBCClass javaLangString;
    
    protected ObjectModeller(GeneralBenchmark core) {
        this.core = core;
    }
    
    protected void notifyAppClassLoader(JBCClassLoader appClassLoader) {
        this.appClassLoader = appClassLoader;
        
        javaLangObject = appClassLoader.getWorld().getSpecialClass("java.lang.Object");
        javaLangString = appClassLoader.getWorld().getSpecialClass("java.lang.String");
    }
    
    private void scanNodes(Vector nodes) {
        for (Enumeration e = nodes.elements(); e.hasMoreElements();) {
            Object o = e.nextElement();
            
            if (o instanceof ExternalCFGFieldAssignmentDefNode) {
                fieldSet.add(((ExternalCFGFieldAssignmentDefNode)o).getField());
            } else if (o instanceof ExternalCFGFieldDefNode) {
                fieldSet.add(((ExternalCFGFieldDefNode)o).getField());
            } else if (o instanceof ExternalCFGUserFieldAssignmentDefNode) {
                userFieldSet.add(((ExternalCFGUserFieldAssignmentDefNode)o).getField());
            } else if (o instanceof ExternalCFGUserFieldDefNode) {
                userFieldSet.add(((ExternalCFGUserFieldDefNode)o).getField());
            }
        }
    }

    protected boolean usesDynamicQueryFamilies() {
        return true;
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, ExternalFlowgraph fg, Vector externalNodes) {
        scanNodes(externalNodes);
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, String name, ExternalFlowgraph fg, Vector externalNodes) {
        scanNodes(externalNodes);
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, byte[] code, boolean[] instructions) {
        for (int i = 0; i < instructions.length; i++) {
            if (instructions[i]) {
                switch (code[i] & 0xFF) {
                    case OP_getstatic:
                    case OP_putstatic:
                    case OP_getfield:
                    case OP_putfield: {
                        JBCField f = JBCCodeUtilities.resolveInstructionField(method, code, i);
                        
                        if (f != null) {
                            fieldSet.add(f);
                        }
                        break;
                    }
                }
            }
        }
    }
    
    private CompactSet makeCandidateExpressions() {
        CompactSet candidateNodes = new CompactSet();
        MethodData data = location.getMethod().getData();
        String[] varNames =
            JBCCodeUtilities.getLocalVariableNames(location.getMethod(), location.getOffset());
            
        for (int i = data.getMaxLocalWords() - 1; i >= 0; i--) {
            JBCExpression expr = JBCExpression.makeLocalVarExpression(i);
            
            if (includeCanonicalExpression(expr)) {
                candidateNodes.add(expr);
                canonicalExpressionLabels.put(expr, varNames[i]);
            }
        }
        for (int i = data.getMaxStackWords() - 1; i >= 0; i--) {
            JBCExpression expr = JBCExpression.makeStackElemExpression(i);
            
            if (includeCanonicalExpression(expr)) {
                candidateNodes.add(expr);
                canonicalExpressionLabels.put(expr, "stack-" + i);
            }
        }
        
        for (Enumeration e = fieldSet.elements(); e.hasMoreElements();) {
            JBCField f = (JBCField)e.nextElement();
                    
            if (includeGlobals && f.isStatic()) {
                JBCExpression expr = JBCExpression.makeStaticFieldExpression(f);
                    
                if (includeCanonicalExpression(expr)) {
                    candidateNodes.add(expr);
                    canonicalExpressionLabels.put(expr, getPrettyClassName(f.getContainingClass()) + "." + f.getFieldName());
                }
            }
        }
        
        return candidateNodes;
    }
    
    private static JBCType getTypeOfExpression(JBCExpression expr) {
        if (expr instanceof JBCFieldExpression) {
            return ((JBCFieldExpression)expr).getField().getFieldType();
        } else {
            return null;
        }
    }
    
    private boolean includeCanonicalExpression(JBCExpression expr) {
        JBCType t = getTypeOfExpression(expr);
        
        return t == null || includeNonObjects || t instanceof JBCObjectType;
    }
/* UNSAFE code, I think    
    private CompactSet makeFieldExtensionExpressions(CompactSet expressions) {
        CompactSet result = new CompactSet();
        
        for (Enumeration e = expressions.elements(); e.hasMoreElements();) {
            JBCExpression expr = (JBCExpression)e.nextElement();
            JBCType t = getTypeOfExpression(expr);
            JBCClass c;
            boolean isObjectType;
                
            if (t == null) {
                c = null;
                isObjectType = true;
            } else if (t instanceof JBCObjectType) {
                c = ((JBCObjectType)t).getClassDef();
                isObjectType = true;
            } else {
                c = null;
                isObjectType = false;
            }

            if (isObjectType) {
                for (Enumeration e2 = fieldSet.elements(); e2.hasMoreElements();) {
                    JBCField f = (JBCField)e2.nextElement();
                        
                    if (!f.isStatic()) {
                        JBCClass fieldClass = f.getContainingClass();
                            
                        if (c == null || fieldClass.isSubclassOf(c) || c.isSubclassOf(fieldClass)) {
                            JBCExpression fieldExpr = expr.makeNonstaticFieldExpression(f);
                                
                            if (includeCanonicalExpression(fieldExpr)) {
                                result.add(fieldExpr);
                            }
                        }
                    }
                }
            }
                
            for (Enumeration e2 = userFieldSet.elements(); e2.hasMoreElements();) {
                UserField f = (UserField)e2.nextElement();
                    
                if (!f.isStatic()) {
                    JBCClass fieldClass = f.getContainingClass();
                        
                    if (c == null || fieldClass.isSubclassOf(c) || c.isSubclassOf(fieldClass)) {
                        JBCExpression fieldExpr = expr.makeUserFieldExpression(f);
                            
                        if (includeCanonicalExpression(fieldExpr)) {
                            result.add(fieldExpr);
                        }
                    }
                }
            }
        }
        
        return result;
    }
*/    
    private CompactSet makeFieldExtensionExpressions(CompactSet expressions) {
        CompactSet result = new CompactSet();
        
        for (Enumeration e = expressions.elements(); e.hasMoreElements();) {
            JBCExpression expr = (JBCExpression)e.nextElement();

            for (Enumeration e2 = fieldSet.elements(); e2.hasMoreElements();) {
                JBCField f = (JBCField)e2.nextElement();
                        
                if (!f.isStatic()) {
                    JBCExpression fieldExpr = expr.makeNonstaticFieldExpression(f);
                                
                    if (includeCanonicalExpression(fieldExpr)) {
                        result.add(fieldExpr);
                    }
                }
            }
                
            for (Enumeration e2 = userFieldSet.elements(); e2.hasMoreElements();) {
                UserField f = (UserField)e2.nextElement();
                    
                if (!f.isStatic()) {
                    JBCExpression fieldExpr = expr.makeUserFieldExpression(f);
                            
                    if (includeCanonicalExpression(fieldExpr)) {
                        result.add(fieldExpr);
                    }
                }
            }
        }
        
        return result;
    }
    
    private static String getPrettyClassName(JBCClass c) {
        String name = JBCObjectType.convertClassNameToHumanReadable(c.getClassName());
        int lastDot = name.lastIndexOf('.');
        
        if (lastDot >= 0) {
            return name.substring(lastDot + 1);
        } else {
            return name;
        }
    }
    
    private void dumpDebugGraph() {
        try {
            Writer w = new BufferedWriter(new FileWriter("debug" + dumpCount + ".dot"));
            
            DotWriter.write(w, makeGraph(), "objectmodel", new DotGraphFormatter());
            w.close();
        } catch (IOException ex) {
            System.err.println(ex);
        }
        
        dumpCount++;
    }
    
    private void combineExpressionLabels(JBCExpression into, JBCExpression from) {
        String label = (String)canonicalExpressionLabels.remove(from);
                    
        if (label != null) {
            String curLabel = (String)canonicalExpressionLabels.get(into);
                        
            if (curLabel == null) {
                canonicalExpressionLabels.put(into, label);
            } else {
                canonicalExpressionLabels.put(into, curLabel + "\n" + label);
            }
        }
    }
    
    private Hashtable performNodeMatchingQuery(ObjectModellerQuery query, int bound) throws PrematureTerminationException {
        DatumSpecifier[] specifiers = { query };
        ResultListener[] listeners = { query };
        BoundedSetTracker tracker = new BoundedSetTracker(analyzer, specifiers, listeners, bound);
            
        tracker.setKeepOverflowData(true);
        tracker.start();
            
        while (analyzer.work()) {
        }
            
        Hashtable results = query.getResults();
            
        tracker.dispose();
        
        return results;
    }

    private static JBCExpression chaseNodeMapping(Hashtable nodeMap, JBCExpression node) {
        Object o = nodeMap.get(node);
        
        if (o == null || o.equals(node)) {
            return node;
        } else {
            JBCExpression map = chaseNodeMapping(nodeMap, (JBCExpression)o);
            
            nodeMap.put(node, map);
            return map;
        }
    }

/**
@return deleted node
*/
    private void mergeNodes(CompactSet newNodes, Hashtable nodeMap, JBCExpression toNode, JBCExpression fromNode) {
        toNode = chaseNodeMapping(nodeMap, toNode);
        fromNode = chaseNodeMapping(nodeMap, fromNode);
        
        if (!toNode.equals(fromNode)) {
            nodeMap.put(fromNode, toNode);
            combineExpressionLabels(toNode, fromNode);
            newNodes.remove(fromNode);
        }
    }

/**
On entry, liveCanonicalExpressions is a set of pairwise unrelated expressions. 
'newNodes' is a set of expressions.

On exit, liveCanonicalExpressions is a set of pairwise unrelated expressions. 
Every element of newNodes related to something is in liveCanonicalExpressions or related to
an element of liveCanonicalExpressions.

We return the set "liveCanonicalExpressions' - liveCanonicalExpressions".

The "newNodes" parameter is destroyed.
*/
    private CompactSet compactExpressions(CompactSet newNodes) throws PrematureTerminationException {
        Globals.writeLog(this, "Compacting " + newNodes.size() + " nodes into "
            + liveCanonicalExpressions.size() + " nodes");

        Hashtable newNodeMap = new Hashtable();
        Hashtable results =
            performNodeMatchingQuery(new ObjectModellerQuery(location, liveCanonicalExpressions, newNodes), 1);
        CompactSet resultNodes = new CompactSet();
            
        for (Enumeration e = newNodes.elements(); e.hasMoreElements();) {
            JBCExpression node = (JBCExpression)e.nextElement();
            Object r = results.get(node);

            if (r == null) {
                newNodes.remove(node);
                newNodeMap.put(node, node);
            } else {
                JBCExpression matchingNewNode =
                    (JBCExpression)BoundedSetTracker.enumerateIntermediate(r).nextElement();
                
                mergeNodes(newNodes, newNodeMap, node, matchingNewNode);
            }
        }
        
        for (Enumeration e = liveCanonicalExpressions.elements(); e.hasMoreElements();) {
            JBCExpression node = (JBCExpression)e.nextElement();
            Object r = results.get(node);

            if (r != null) {
                JBCExpression matchingNewNode =
                    (JBCExpression)BoundedSetTracker.enumerateIntermediate(r).nextElement();
                
                mergeNodes(newNodes, newNodeMap, node, matchingNewNode);
            }
        }

        while (newNodes.size() > 0) {
            Globals.writeLog(this, "Compaction iteration: newNodes.size() = " + newNodes.size());
            
            Hashtable newResults = performNodeMatchingQuery(new ObjectModellerQuery(location, newNodes), 2);

            for (Enumeration e = newNodes.elements(); e.hasMoreElements();) {
                JBCExpression node = (JBCExpression)e.nextElement();
                Object r = newResults.get(node);
                
                if (Globals.debug && r == null) {
                    Globals.writeLog(this, "ERROR: Expression node " + node + " had result " + results.get(node) + ", now it's null??");
                } else if (r.equals(node)) {
                    newNodes.remove(node);
                    liveCanonicalExpressions.addUnconditionally(node);
                    resultNodes.addUnconditionally(node);
                    newNodeMap.put(node, node);
                } else {
                    for (Enumeration e2 = BoundedSetTracker.enumerateIntermediate(r); e2.hasMoreElements();) {
                        JBCExpression incomingNode = (JBCExpression)e2.nextElement();

                        mergeNodes(newNodes, newNodeMap, node, incomingNode);
                    }
                }
            }
        }
        
        for (Enumeration e = newNodeMap.keys(); e.hasMoreElements();) {
            JBCExpression node = (JBCExpression)e.nextElement();
                
            if (node instanceof JBCFieldExpression
                || node instanceof JBCUserFieldExpression) {
                fieldExpressionsToCanonicalExpressions.put(node, chaseNodeMapping(newNodeMap, node));
            }
        }
        
        Globals.writeLog(this, "Compaction result: " + liveCanonicalExpressions.size() + " nodes");
        
        return resultNodes;
    }
    
    private void computeExpressionClasses() throws PrematureTerminationException {
        ObjectModellerClassQuery classQuery = new ObjectModellerClassQuery(location, liveCanonicalExpressions);
        DatumSpecifier[] classSpecifiers = { classQuery, new NewObjectClassSource() };
        ResultListener[] classListeners = { classQuery };
        UnboundedSetTracker classTracker = new UnboundedSetTracker(analyzer, classSpecifiers, classListeners);
        
        classTracker.start();
        
        while (analyzer.work()) {
        }
        
        classQuery.updateClassSets(canonicalExpressionClassSets);
        
        classTracker.dispose();
    }
    
    private CompactSet findBlobExpressions() {
        CompactSet result = new CompactSet();
        
        if (unlinkBlobSize < 0) {
            return result;
        }
        
        Hashtable degreeBounds = new Hashtable();
        
        for (Enumeration e = fieldExpressionsToCanonicalExpressions.keys(); e.hasMoreElements();) {
            JBCExpression fieldExpr = (JBCExpression)e.nextElement();
            JBCExpression expr = (JBCExpression)fieldExpressionsToCanonicalExpressions.get(fieldExpr);
            Object exprClass = findLeastCommonSuperclass(expr);
            
            if (exprClass == null || exprClass.equals(javaLangObject)) {
                int[] bounds = (int[])degreeBounds.get(expr);

                if (bounds == null) {
                    bounds = new int[1];
                    degreeBounds.put(expr, bounds);
                }
                
                bounds[0]++;
                
                if (bounds[0] == unlinkBlobSize) {
                    result.addUnconditionally(expr);
                }
            }
        }
        
        return result;
    }
    
    public boolean work(Analyzer analyzer) {
        try {
            // Collect fieldSet
            while (analyzer.work()) {
            }
            
            Globals.writeLog(this, "Entering modelling phase");
            
            fieldFinder.dispose();

            if (rootExpressions == null) {
                rootExpressions = makeCandidateExpressions();
            }
            
            CompactSet newNodes = compactExpressions((CompactSet)rootExpressions.clone());
            
            if (liveCanonicalExpressions.size() == 0) {
                System.err.println("The specified location is dead: " + location);
                return true;
            }
            
            while (newNodes.size() > 0) {
                if (dumpDebugGraphs) {
                    dumpDebugGraph();
                }
                
                newNodes = compactExpressions(makeFieldExtensionExpressions(newNodes));
            }
            
            computeExpressionClasses();
            resultGraph = makeGraph();
                    
            return false;
        } catch (PrematureTerminationException ex) {
            return true;
        }
    }
    
    private static GraphNode makeGraphNode(Graph resultGraph, JBCClass c, int[] nodeCount, Hashtable nodesToBaseClasses) {
        DotGraphNode graphNode = new DotGraphNode("_" + nodeCount[0]);
        String className;
        
        nodeCount[0]++;
            
        if (c == null) {
            className = "<primtype>";
            nodesToBaseClasses.put(graphNode, className);
        } else {
            className = getPrettyClassName(c);
            nodesToBaseClasses.put(graphNode, c);
        }
        
        graphNode.setAttribute("label", className);
        graphNode.setAttribute("shape", "box");
        
        resultGraph.addNode(graphNode);
        
        return graphNode;
    }
    
    private JBCClass findLeastCommonSuperclass(JBCExpression expr) {
        Object c = null;
        
        for (Enumeration e = UnboundedSetTracker.enumerateIntermediate(canonicalExpressionClassSets.get(expr)); e.hasMoreElements();) {
            JBCClass cl = (JBCClass)e.nextElement();
            
            if (c == null) {
                c = cl;
            } else {
                c = ClassTracker.join(c, cl);
            }
        }
        
        return c != null ? ClassTracker.getActualClass(c) : null;
    }
    
    private static DotGraphEdge makeSubclassEdge(JBCClass subclass, Hashtable edgesToFields, boolean superReached) {
        DotGraphEdge edge = new DotGraphEdge();
        
        edge.setAttribute("style", "dotted");
        
        edgesToFields.put(edge, superReached ? subclass : GraphCompression.ignoreID);
        
        return edge;
    }
    
    private static boolean isSubclassEdge(GraphEdge g) {
        if (g instanceof DotGraphEdge) {
            String style = ((DotGraphEdge)g).getAttribute("style");
            
            return style != null && style.equals("dotted");
        } else {
            return false;
        }
    }
    
    private static Object getExpressionField(JBCExpression expr) {
        if (expr instanceof JBCFieldExpression) {
            return ((JBCFieldExpression)expr).getField();
        } else if (expr instanceof JBCUserFieldExpression) {
            return ((JBCUserFieldExpression)expr).getField();
        } else {
            return null;
        }
    }
    
    private static DotGraphEdge getFieldEdge(JBCExpression expr, Hashtable edgesToFields) {
        Object f = getExpressionField(expr);
        DotGraphEdge graphEdge = new DotGraphEdge();
        String name = f instanceof JBCField ? ((JBCField)f).getFieldName()
            : ((UserField)f).getFieldName();
        JBCClass c = f instanceof JBCField ? ((JBCField)f).getContainingClass()
            : ((UserField)f).getContainingClass();
            
        edgesToFields.put(graphEdge, f);
            
        graphEdge.setAttribute("label", name);
        return graphEdge;
    }
    
    private static JBCExpression getBaseExpression(JBCExpression expr) {
        if (expr instanceof JBCCompoundExpression) {
            return ((JBCCompoundExpression)expr).getBase();
        } else {
            return null;
        }
    }
    
    private boolean isClassInScope(JBCClass c) {
        if (scopePackages == null) {
            return true;
        } else {
            String name = c.getClassName();
            String[] packages = StringUtils.split(scopePackages, ',');
            
            for (int i = 0; i < packages.length; i++) {
                if (name.startsWith(packages[i])) {
                    return true;
                }
            }
            
            return false;
        }
    }
    
    private GraphNode selectNode(Hashtable expressionsToNodes, JBCExpression expr, JBCClass c) {
        Object o = expressionsToNodes.get(expr);
        
        if (o instanceof Hashtable) {
            if (c == null) {
                c = analyzer.getWorld().getSpecialClass("java.lang.Object");
            }
            
            Object result = ((Hashtable)o).get(c);
            
            if (result == null && c.isArray()) {
                Hashtable classNodes = (Hashtable)o;
                
                for (Enumeration e = classNodes.keys(); e.hasMoreElements();) {
                    JBCClass c2 = (JBCClass)e.nextElement();
                        
                    if (c2.isArray()) {
                        return (GraphNode)classNodes.get(c2);
                    }
                }
                
                return (GraphNode)classNodes.get(analyzer.getWorld().getSpecialClass("java.lang.Object"));
            } else {
                return (GraphNode)result;
            }
        } else {
            return (GraphNode)o;
        }
    }
    
    private boolean unlinkClass(JBCClass c) {
        boolean atRealClass = true;
        
        do {
            String name = c == null ? "java.lang.Object" : c.getClassName();
            Boolean b = (Boolean)unlink.get(name);
            
            if (b != null && (b.booleanValue() || atRealClass)) {
                return true;
            }
            
            if (c != null) {
                c = c.getSuperClass();
            }
            atRealClass = false;
        } while (c != null);
        
        return false;
    }

    private GraphNode selectDestinationNode(Hashtable expressionsToNodes, JBCExpression expr, JBCClass c,
        Hashtable nodesToBaseClasses, int[] nodeCount, Graph resultGraph) {
        if (c != null && c.isInterface()) {
            c = javaLangObject;
        }
        
        GraphNode node = selectNode(expressionsToNodes, expr, c);
        
        if (node == null) {
            return null;
        }
        
        if (blobNodes.get(node) != null || unlinkClass(c)) {
            node = makeGraphNode(resultGraph, c == null ? javaLangObject : c, nodeCount, nodesToBaseClasses);
                
            if (node instanceof DotGraphNode) {
                ((DotGraphNode)node).setAttribute("style", "invis");
            }
        }
        
        return node;
    }
    
    private void visitChasingFieldsInSupers(Graph g, GraphNode node, CompactSet visitedChasingFieldsInSupers,
        CompactSet visitedChasingFieldsInSubs) {
        if (visitedChasingFieldsInSupers.get(node) == null) {
            visitedChasingFieldsInSupers.addUnconditionally(node);
            
            visitChasingComponents(g, node, visitedChasingFieldsInSupers, visitedChasingFieldsInSubs);
            
            for (Enumeration e = g.getPredecessorEdges(node); e.hasMoreElements();) {
                DotGraphEdge edge = (DotGraphEdge)e.nextElement();
                
                if (isSubclassEdge(edge)) {
                    visitChasingFieldsInSupers(g, g.getFromNode(edge), visitedChasingFieldsInSupers, visitedChasingFieldsInSubs);
                }
            }
        }
    }
    
    private void visitChasingFieldsInSubs(Graph g, GraphNode node, CompactSet visitedChasingFieldsInSupers,
        CompactSet visitedChasingFieldsInSubs) {
        if (visitedChasingFieldsInSubs.get(node) == null) {
            visitedChasingFieldsInSubs.addUnconditionally(node);
            
            visitChasingComponents(g, node, visitedChasingFieldsInSupers, visitedChasingFieldsInSubs);
            
            for (Enumeration e = g.getSuccessorEdges(node); e.hasMoreElements();) {
                DotGraphEdge edge = (DotGraphEdge)e.nextElement();
                
                if (isSubclassEdge(edge)) {
                    visitChasingFieldsInSubs(g, g.getToNode(edge), visitedChasingFieldsInSupers, visitedChasingFieldsInSubs);
                }
            }
        }
    }
    
    private void visitChasingComponents(Graph g, GraphNode node, CompactSet visitedChasingFieldsInSupers,
        CompactSet visitedChasingFieldsInSubs) {
        for (Enumeration e = g.getSuccessorEdges(node); e.hasMoreElements();) {
            DotGraphEdge edge = (DotGraphEdge)e.nextElement();
                
            if (!isSubclassEdge(edge)) {
                GraphNode destNode = g.getToNode(edge);
                
                visitChasingFieldsInSupers(g, destNode, visitedChasingFieldsInSupers, visitedChasingFieldsInSubs);
                visitChasingFieldsInSubs(g, destNode, visitedChasingFieldsInSupers, visitedChasingFieldsInSubs);
            }
        }
    }
    
    private void pruneUnreachableNodes(Graph g, Hashtable expressionsToNodes) {
        CompactSet visitedChasingFieldsInSupers = new CompactSet();
        CompactSet visitedChasingFieldsInSubs = new CompactSet();
        
        for (Enumeration e = rootExpressions.elements(); e.hasMoreElements();) {
            Object o = expressionsToNodes.get(e.nextElement());
            
            if (o instanceof Hashtable) {
                for (Enumeration e2 = ((Hashtable)o).elements(); e2.hasMoreElements();) {
                    GraphNode node = (GraphNode)e2.nextElement();
                    
                    visitChasingFieldsInSupers(g, node, visitedChasingFieldsInSupers, visitedChasingFieldsInSubs);
                    visitChasingFieldsInSubs(g, node, visitedChasingFieldsInSupers, visitedChasingFieldsInSubs);
                }
            } else if (o != null) {
                GraphNode node = (GraphNode)o;
                
                visitChasingFieldsInSupers(g, node, visitedChasingFieldsInSupers, visitedChasingFieldsInSubs);
                visitChasingFieldsInSubs(g, node, visitedChasingFieldsInSupers, visitedChasingFieldsInSubs);
            }
        }
        
        CompactSet nodesToRemove = new CompactSet();
        
        for (Enumeration e = g.getNodes(); e.hasMoreElements();) {
            Object o = e.nextElement();
            
            if (visitedChasingFieldsInSupers.get(o) == null && visitedChasingFieldsInSubs.get(o) == null) {
                nodesToRemove.addUnconditionally(o);
            }
        }
        
        for (Enumeration e = nodesToRemove.elements(); e.hasMoreElements();) {
            g.removeNode((GraphNode)e.nextElement());
        }
    }
    
    private static boolean hasFieldSuccessorEdges(Graph g, GraphNode node, Hashtable edgesToFields) {
        for (Enumeration e = g.getSuccessorEdges(node); e.hasMoreElements();) {
            if (edgesToFields.get(e.nextElement()) instanceof JBCField) {
                return true;
            }
        }
        
        return false;
    }
    
    private void pruneUselessClasses(Graph g, Hashtable expressionsToNodes, Hashtable nodesToBaseClasses, Hashtable edgesToFields) {
        if (pruneTopObject) {
            CompactSet rootNodes = new CompactSet();
            for (Enumeration e = rootExpressions.elements(); e.hasMoreElements();) {
                Object o = expressionsToNodes.get(e.nextElement());
                
                if (o instanceof Hashtable) {
                    for (Enumeration e2 = ((Hashtable)o).elements(); e2.hasMoreElements();) {
                        rootNodes.add(e2.nextElement());
                    }
                } else if (o != null) {
                    rootNodes.add(o);
                }
            }
            
            boolean changed;
            
            do {
                changed = false;
                
                for (Enumeration e = nodesToBaseClasses.keys(); e.hasMoreElements();) {
                    GraphNode node = (GraphNode)e.nextElement();
                    
                    if (g.hasNode(node)
                        && !g.getPredecessorEdges(node).hasMoreElements()
                        && !hasFieldSuccessorEdges(g, node, edgesToFields)
                        && (nodesToBaseClasses.get(node).equals(javaLangObject)
                            || (pruneUselessSuperclasses && rootNodes.get(node) == null))) {
                        g.removeNode(node);
                        changed = true;
                    }
                }
            } while (changed && pruneUselessSuperclasses);
        }
    }
    
    private void setNodeLabel(GraphNode node, String label) {
        if (node instanceof DotGraphNode) {
            DotGraphNode dotNode = (DotGraphNode)node;
                    
            dotNode.setAttribute("label", dotNode.getAttribute("label") + "\n" + label);
        }
    }
    
    private static boolean isFieldCompatibleWithArray(String fieldName, String className) {
        switch (className.charAt(1)) {
            case 'L': return fieldName.equals("arrayelement");
            case 'J': return fieldName.equals("longarrayelement");
            case 'D': return fieldName.equals("doublearrayelement");
            case 'F': return fieldName.equals("floatarrayelement");
            
            case 'I':
            case 'Z':
            case 'C':
            case 'B':
            case 'S': return fieldName.equals("intarrayelement");
            
            default:
                return false;
        }
    }
    
    private Graph makeGraph() {
        Hashtable expressionsToNodes = new Hashtable();
        Hashtable nodesToBaseClasses = new Hashtable();
        Hashtable edgesToFields = new Hashtable();
        CompactSet reachedClassNodes = new CompactSet();
        Graph resultGraph = new Graph();
        int[] nodeCount = new int[1];
        JBCClass objArrayClass = analyzer.getWorld().getSystemClassLoader().getClass("[Ljava.lang.Object;");
        
        // build node set
        for (Enumeration e = liveCanonicalExpressions.elements(); e.hasMoreElements();) {
            JBCExpression expr = (JBCExpression)e.nextElement();
            Object classSet = canonicalExpressionClassSets.get(expr);
            
            if (includeNonObjects || (classSet != null && !showClasses)) {
                expressionsToNodes.put(expr,
                    makeGraphNode(resultGraph, findLeastCommonSuperclass(expr), nodeCount, nodesToBaseClasses));
            } else if (showClasses && classSet != null) {
                Hashtable classNodes = new Hashtable();
                JBCClass arrayClass = null;
                
                for (Enumeration e2 = UnboundedSetTracker.enumerateIntermediate(classSet); e2.hasMoreElements();) {
                    JBCClass c = (JBCClass)e2.nextElement();
                    
                    if (c.isArray()) {
                        if (arrayClass == null) {
                            arrayClass = c;
                        } else if (!arrayClass.equals(c)) {
                            arrayClass = objArrayClass;
                        }
                    } else {
                        while (c != null) {
                            if (classNodes.get(c) == null) {
                                classNodes.put(c,
                                    makeGraphNode(resultGraph, c, nodeCount, nodesToBaseClasses));
                            }
                            c = c.getSuperClass();
                        }
                    }
                }
                
                if (arrayClass != null) {
                    classNodes.put(arrayClass,
                        makeGraphNode(resultGraph, arrayClass, nodeCount, nodesToBaseClasses));
                    
                    if (classNodes.get(javaLangObject) == null) {
                        classNodes.put(javaLangObject,
                            makeGraphNode(resultGraph, javaLangObject, nodeCount, nodesToBaseClasses));
                    }
                }
                
                expressionsToNodes.put(expr, classNodes);
            }
        }
        
        for (Enumeration e = findBlobExpressions().elements(); e.hasMoreElements();) {
            JBCExpression expr = (JBCExpression)e.nextElement();
            GraphNode blobNode = (GraphNode)selectNode(expressionsToNodes, expr, javaLangObject);
            
            if (blobNode != null) {
                blobNodes.add(blobNode);
            }
        }
        
        // add field edges
        for (Enumeration e = fieldExpressionsToCanonicalExpressions.keys(); e.hasMoreElements();) {
            JBCExpression expr = (JBCExpression)e.nextElement();
            JBCExpression from = getBaseExpression(expr);
            JBCExpression to = (JBCExpression)fieldExpressionsToCanonicalExpressions.get(expr);
            
            if (from != null && to != null) {
                Object f = getExpressionField(expr);
                JBCClass fromClass = null;
                JBCType t = getTypeOfExpression(expr);
                JBCClass toClass = findLeastCommonSuperclass(to);
                
                if (t instanceof JBCObjectType) {
                    JBCClass declaredClass = ((JBCObjectType)t).getClassDef();
                    
                    if (toClass == null || declaredClass.isSubclassOf(toClass)) {
                        toClass = declaredClass;
                    }
                }
                
                if (f instanceof JBCField) {
                    fromClass = ((JBCField)f).getContainingClass();
                } else {
                    UserField uField = (UserField)f;
                    String uFieldName = uField.getFieldName();
                    
                    if (uFieldName.indexOf("array") >= 0) {
                        fromClass = null;
                        
                        for (Enumeration e2 = UnboundedSetTracker.enumerateIntermediate(canonicalExpressionClassSets.get(from)); e2.hasMoreElements();) {
                            JBCClass c = (JBCClass)e2.nextElement();
                            
                            if (c.isArray() && isFieldCompatibleWithArray(uFieldName, c.getClassName())) {
                                if (fromClass != null) {
                                    fromClass = objArrayClass;
                                } else {
                                    fromClass = c;
                                }
                            }
                        }
                        
                        if (uFieldName.equals("arrayelement") && fromClass != null) {
                            JBCClass declaredClass = fromClass.getArrayComponentClass();
                            
                            if (declaredClass != null
                                && (toClass == null || declaredClass.isSubclassOf(toClass))) {
                                toClass = declaredClass;
                            }
                        }
                    } else {
                        fromClass = uField.getContainingClass();
                    }
                }
                
                GraphNode fromNode = selectNode(expressionsToNodes, from, fromClass);
                GraphNode toNode = selectDestinationNode(expressionsToNodes, to, toClass,
                    nodesToBaseClasses, nodeCount, resultGraph);
                
                if (fromNode != null && toNode != null) {
                    resultGraph.addEdge(getFieldEdge(expr, edgesToFields), fromNode, toNode);
                    reachedClassNodes.add(toNode);
                }
            }
        }
        
        // add inheritance edges
        for (Enumeration e = expressionsToNodes.keys(); e.hasMoreElements();) {
            Object key = e.nextElement();
            Object value = expressionsToNodes.get(key);
            
            if (value instanceof Hashtable) {
                Hashtable classNodes = (Hashtable)value;
                
                for (Enumeration e2 = classNodes.keys(); e2.hasMoreElements();) {
                    JBCClass c = (JBCClass)e2.nextElement();
                    JBCClass cSuper = c.getSuperClass();
                        
                    if (cSuper != null) {
                        GraphNode superNode = (GraphNode)classNodes.get(cSuper);
                        
                        if (superNode != null) {
                            GraphNode subNode = (GraphNode)classNodes.get(c);
                            JBCClass superIterator = cSuper;
                            boolean superReached = false;
                                
                            while (!superReached && superIterator != null) {
                                superReached = reachedClassNodes.get(classNodes.get(superIterator)) != null;
                                superIterator = superIterator.getSuperClass();
                            }
                            
                            resultGraph.addEdge(makeSubclassEdge(c, edgesToFields, superReached), superNode, subNode);
                        }
                    }
                }
            }
        }
        
        // prune nodes that are unreachable (usually orphan classes that can't really exist)
        pruneUnreachableNodes(resultGraph, expressionsToNodes);
        
        // nuke any unneecessary superclasses
        if (showClasses) {
            pruneUselessClasses(resultGraph, expressionsToNodes, nodesToBaseClasses, edgesToFields);
        }
        
        // compress graph
        if (compressGraph) {
            Hashtable map = GraphCompression.computeIsomorphs(resultGraph, nodesToBaseClasses,
                edgesToFields);
                
            for (Enumeration e = expressionsToNodes.keys(); e.hasMoreElements();) {
                Object k = e.nextElement();
                Object node = expressionsToNodes.get(k);
                
                if (node instanceof Hashtable) {
                    Hashtable classNodes = (Hashtable)node;
                    
                    for (Enumeration e2 = classNodes.keys(); e2.hasMoreElements();) {
                        Object c = e2.nextElement();
                        Object mappedNode = map.get(classNodes.get(c));
                        
                        if (mappedNode != null) {
                            classNodes.put(c, mappedNode);
                        }
                    }
                } else {
                    Object mappedNode = map.get(node);
                    
                    if (mappedNode != null) {
                        expressionsToNodes.put(k, mappedNode);
                    }
                }
            }
            
            GraphCompression.applyIsomorphismMap(resultGraph, map, edgesToFields);
        }

        // remove redundant inheritance edges
        for (Enumeration e = resultGraph.getNodes(); e.hasMoreElements();) {
            boolean seenInheritanceEdge = false;
            GraphNode node = (GraphNode)e.nextElement();
            Vector edgesToDelete = new Vector();
            
            for (Enumeration e2 = resultGraph.getPredecessorEdges(node); e2.hasMoreElements();) {
                DotGraphEdge edge = (DotGraphEdge)e2.nextElement();
                
                if (isSubclassEdge(edge)) {
                    if (seenInheritanceEdge) {
                        edgesToDelete.addElement(edge);
                    } else {
                        seenInheritanceEdge = true;
                    }
                }
            }
            
            for (Enumeration e2 = edgesToDelete.elements(); e2.hasMoreElements();) {
                resultGraph.removeEdge((GraphEdge)e2.nextElement());
            }
        }
        
        // add variable name labels
        for (Enumeration e = canonicalExpressionLabels.keys(); e.hasMoreElements();) {
            Object k = e.nextElement();
            Object node = expressionsToNodes.get(k);
            String label = (String)canonicalExpressionLabels.get(k);
                
            if (node instanceof Hashtable) {
                Hashtable classNodes = (Hashtable)node;
                
                for (Enumeration e2 = classNodes.keys(); e2.hasMoreElements();) {
                    JBCClass c = (JBCClass)e2.nextElement();
                    JBCClass cSuper = c.getSuperClass();
                    
                    if (cSuper == null || !resultGraph.hasNode((GraphNode)classNodes.get(cSuper))) {
                        setNodeLabel((GraphNode)classNodes.get(c), label);
                    }
                }
            } else if (node != null) {
                setNodeLabel((GraphNode)node, label);
            }
        }
        
        return resultGraph;
    }
    
    private static void fillInReachableInstructionStarts(byte[] code, int offset,
        int[] singleton, boolean[] result) {
        while (true) {
            if (!result[offset]) {
                int opcode = code[offset] & 0xFF;
                int[] successors = JBCCodeUtilities.getReachableSuccessors(code, offset, singleton);
                
                result[offset] = true;
            
                if (successors.length == 1 || opcode == OpcodeConstants.OP_jsr || opcode == OpcodeConstants.OP_jsr_w) {
                    offset = successors[0];
                    continue;
                } else {
                    for (int i = 0; i < successors.length; i++) {
                        fillInReachableInstructionStarts(code, successors[i], singleton, result);
                    }
                }
            }
            
            return;
        }
    }
    
    private static int findDefaultOffset(JBCMethod method) {
        int[] singleton = new int[1];
        byte[] code = method.getData().getCode();
        boolean[] result = new boolean[code.length];
        
        fillInReachableInstructionStarts(code, 0, singleton, result);
        
        for (int i = result.length - 1; i > 0; i--) {
            if (result[i]) {
                return i;
            }
        }
        
        return 0;
    }
    
    public void configure(Analyzer analyzer) {
        DatumSpecifier[] specifiers = { this };
        
        this.analyzer = analyzer;
        
        try {
            int colon = locationName == null ? -1 : locationName.indexOf(':');
            
            if (colon >= 0) {
                CompactSet exprs = QueryExpressionParser.parseExpression(appClassLoader, locationName);
                
                rootExpressions = new CompactSet();
                
                for (Enumeration e = exprs.elements(); e.hasMoreElements();) {
                    JBCValuePoint vp = (JBCValuePoint)e.nextElement();
                    
                    if (location == null) {
                        location = (JBCLocation)vp.getLocation();
                    } else if (!location.equals(vp.getLocation())) {
                        System.err.println("Underspecified expression " + locationName + "!");
                        System.exit(2);
                        return;
                    }
                    
                    JBCExpression expr = vp.getExpression();
                    
                    if (includeCanonicalExpression(expr)) {
                        rootExpressions.add(expr);
                        canonicalExpressionLabels.put(expr, locationName.substring(colon + 1));
                    }
                }
                
                if (rootExpressions.size() == 0) {
                    System.err.println("Expression " + locationName + " doesn't match any objects!");
                    System.exit(2);
                    return;
                }
            } else {
                if (locationName == null) {
                    locationName = core.getMainClassName() + ".main";
                } else if (locationName.indexOf('.') < 0) {
                    locationName = core.getMainClassName() + "." + locationName;
                }
                
                location = QueryExpressionParser.parseLocation(appClassLoader,
                    locationName, QueryExpressionParser.ALLOW_NO_INDEX);
            }
        } catch (ParseException ex) {
            System.err.println("Error in location " + locationName + ": "
                + ex.getMessage());
            System.exit(2);
            return;
        }
        
        if (location.getOffset() == -1) {
            JBCMethod method = location.getMethod();
            
            location = new JBCLocation(method, findDefaultOffset(method));
        }
 
        analyzer.setSemantics(new TransitiveSemantics(location));
        
        fieldFinder = new UnboundedSetTracker(analyzer, specifiers, new ResultListener[0]);
        fieldFinder.start();
    }

    public void printReport(Writer w) throws IOException {
        DotWriter.write(w, resultGraph, "objectmodel", new DotGraphFormatter());
    }
    
    public void parseArgs(Args args) {
        dumpDebugGraphs = args.extractBoolOption("-dumpgraphs");
        includeGlobals = args.extractBoolOption("-globals");
        includeNonObjects = args.extractBoolOption("-nonobjects");
        compressGraph = args.extractBoolOption("-compress");
        showClasses = args.extractBoolOption("-classes");
        mergeSuperclasses = !args.extractBoolOption("-nomerge");
        pruneTopObject = !args.extractBoolOption("-topobject");
        pruneUselessSuperclasses = !args.extractBoolOption("-emptyclasses");
        
        String unlinkBlobStr = args.extractStringOption("-unlinkblobs", "-1");
        
        try {
            unlinkBlobSize = Integer.parseInt(unlinkBlobStr);
        } catch (NumberFormatException ex) {
            unlinkBlobSize = -1;
        }
        
        String unlinkObject;
        
        do {
            unlinkObject = args.extractStringOption("-unlink", null);
            
            if (unlinkObject != null) {
                unlink.put(unlinkObject, new Boolean(false));
            }
        } while (unlinkObject != null);
        
        do {
            unlinkObject = args.extractStringOption("-unlinksubclasses", null);
            
            if (unlinkObject != null) {
                unlink.put(unlinkObject, new Boolean(true));
            }
        } while (unlinkObject != null);
        
        scopePackages = args.extractStringOption("-packages", null);
        
        locationName = args.extractNextOptionalArg();
    }
    
    public static void main(String[] args) {
        GeneralBenchmark core = new GeneralBenchmark();
        ObjectModeller tool = new ObjectModeller(core);
        
        core.go(args, tool,
            " [<location>|<expression>] [-dumpgraphs] [-globals] [-nonobjects] [-compress] [-classes] [-nomerge] [-topobject] [-unlink <classname>] [-unlinksubclasses <classname>] [-packages <packages>]",
            "RTA-SEMI");
    }
}
