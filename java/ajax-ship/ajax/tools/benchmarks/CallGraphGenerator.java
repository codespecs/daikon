/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.benchmarks;

import java.util.*;
import ajax.jbc.*;
import ajax.jbc.util.*;
import java.io.*;
import ajax.analyzer.*;
import ajax.analyzer.util.*;
import ajax.util.*;
import ajax.util.graph.*;
import java.awt.Color;

public class CallGraphGenerator extends Benchmark implements ResultListener, OpcodeConstants, GraphFormatter {
    private Hashtable queries = new Hashtable();
    private StaticCallRecorder staticCalls = new StaticCallRecorder();
    
    private Hashtable functionsToNodes;
    
    private static final Object NONE = new String("NONE");

    protected CallGraphGenerator() {
    }

    public void notifyAnalysisComplete() {
    }
    
    public void registerTarget(Object targetCookie) {
    }

    public void updateResult(Object targetCookie, Object intermediate) {
        if (intermediate != null) {
            queries.put((JBCMethod)targetCookie, intermediate);
        } else {
            queries.remove((JBCMethod)targetCookie);
        }
    }
    
    public void configure(Analyzer analyzer) {
        DatumSpecifier[] specifiers = { new NewObjectMethodTarget(), new VirtualCallReceiverSource(),
            staticCalls };
        ResultListener[] listeners = { this };
        
        (new UnboundedSetTracker(analyzer, specifiers, listeners)).start();
    }
    
    public String getNodeLabel(GraphNode node) {
        return node.toString();
    }
    
    public int getNodeLineStyle(GraphNode node) {
        return LINE_SOLID;
    }
    
    public Color getNodeLineColor(GraphNode node) {
        return Color.black;
    }
    
    public Color getNodeShadeColor(GraphNode node) {
        return Color.black;
    }
    
    public int getNodeShape(GraphNode node) {
        return SHAPE_ELLIPSE;
    }
    
    public String getEdgeLabel(GraphEdge edge) {
        return null;
    }
    
    public int getEdgeLineStyle(GraphEdge edge) {
        return LINE_SOLID;
    }
    
    public Color getEdgeLineColor(GraphEdge edge) {
        return Color.black;
    }
    
    private GraphNode getNode(Object function) {
        Object result = functionsToNodes.get(function);
        
        if (result != null) {
            return (GraphNode)result;
        } else {
            GraphNode newNode = new CallGraphNode(function);
            
            functionsToNodes.put(function, newNode);
            return newNode;
        }
    }
    
    private void addCall(Graph graph, Location location, Object callee) {
        addEdgeConditionally(graph, getNode(location.getFunction()), getNode(callee));
    }
    
    private static boolean addEdgeConditionally(Graph graph, GraphNode from, GraphNode to) {
        if (!graph.getEdges(from, to).hasMoreElements()) {
            graph.addEdge(new StdGraphEdge(), from, to);
            return true;
        } else {
            return false;
        }
    }
    
    private void suppressNode(Graph graph, GraphNode node) {
        if (graph.hasNode(node)) {
            CompactSet fromNodes = new CompactSet();
            CompactSet toNodes = new CompactSet();
            
            for (Enumeration e = graph.getPredecessorEdges(node); e.hasMoreElements();) {
                fromNodes.add(graph.getFromNode((GraphEdge)e.nextElement()));
            }
            for (Enumeration e = graph.getSuccessorEdges(node); e.hasMoreElements();) {
                toNodes.add(graph.getToNode((GraphEdge)e.nextElement()));
            }
            
            for (Enumeration e = fromNodes.elements(); e.hasMoreElements();) {
                GraphNode fromNode = (GraphNode)e.nextElement();
                
                for (Enumeration e2 = toNodes.elements(); e2.hasMoreElements();) {
                    GraphNode toNode = (GraphNode)e2.nextElement();
                    
                    addEdgeConditionally(graph, fromNode, toNode);
                }
            }
            
            graph.removeNode(node);
        }
    }
    
    private void removeNativeSpecFunctions(Graph graph) {
        for (Enumeration e = functionsToNodes.keys(); e.hasMoreElements();) {
            Object fun = e.nextElement();
            
            if (fun instanceof String) {
                suppressNode(graph, (GraphNode)functionsToNodes.get(fun));
            }
        }
    }
    
    public void printReport(Writer w) throws IOException {
        Graph graph = new Graph();
        
        functionsToNodes = new Hashtable();
        
        for (Enumeration e = queries.keys(); e.hasMoreElements();) {
            JBCMethod m = (JBCMethod)e.nextElement();
            Object value = queries.get(m);
            
            if (value instanceof CompactSet) {
                for (Enumeration e2 = ((CompactSet)value).elements(); e2.hasMoreElements();) {
                    addCall(graph, (Location)e2.nextElement(), m);
                }
            } else {
                addCall(graph, (Location)value, m);
            }
        }
        
        for (Enumeration e = staticCalls.getStaticCallLocations(); e.hasMoreElements();) {
            Location location = (Location)e.nextElement();
            
            addCall(graph, location, staticCalls.getStaticCallee(location));
        }
        
        removeNativeSpecFunctions(graph);
        
        DotWriter.write(w, graph, "call_graph", this);
    }
    
    public static void main(String[] args) {
        GeneralBenchmark.run(args, new CallGraphGenerator());
    }
}
