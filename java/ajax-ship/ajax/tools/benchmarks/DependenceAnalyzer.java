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
import ajax.analyzer.util.*;
import ajax.util.*;
import ajax.util.graph.*;
import java.awt.Color;

public class DependenceAnalyzer extends Benchmark implements ResultListener, GraphFormatter {
    private Hashtable callSites = new Hashtable();
    private StaticCallRecorder staticCalls = new StaticCallRecorder();
    private NewRecorder newSites = new NewRecorder();

    private String startArg;
    private CompactSet startMethods = null;
    
    private Hashtable thingsToNodes;
    
    private static final Object NONE = new String("NONE");

    protected DependenceAnalyzer() {
    }
    
    public void notifyAnalysisComplete() {
    }
    
    public void parseArgs(Args args) {
        startArg = args.extractNextOptionalArg();
    }

    public void registerTarget(Object targetCookie) {
        callSites.put(targetCookie, NONE);
    }
    
    public void updateResult(Object targetCookie, Object intermediate) {
        if (intermediate != null) {
            callSites.put(targetCookie, intermediate);
        } else {
            callSites.put(targetCookie, NONE);
        }
    }
    
    public void configure(Analyzer analyzer) {
        if (startArg != null) {
            try {
                startMethods = JBCParserUtilities.parseFunctionSet(
                    analyzer.getWorld().getSystemClassLoader(), startArg);
            } catch (UnresolvedClassException ex) {
                Globals.userError("Function " + startArg + " is invalid; graph will not be pruned\n"
                   + "(" + ex.getMessage() + ")");
            } catch (MissingMethodException ex) {
                Globals.userError("Function " + startArg + " is invalid; graph will not be pruned\n"
                   + "(" + ex.getMessage() + ")");
            } catch (AmbiguousMethodException ex) {
                Globals.userError("Function " + startArg + " is invalid; graph will not be pruned\n"
                   + "(" + ex.getMessage() + ")");
            }
        }
        
        DatumSpecifier[] specifiers = { new NewObjectMethodSource(), new VirtualCallReceiverTarget(),
            staticCalls, newSites };
        ResultListener[] listeners = { this };
        
        (new SingletonTracker(analyzer, specifiers, listeners)).start();
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
        return SHAPE_BOX;
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
    
    private static boolean addEdgeConditionally(Graph graph, GraphNode from, GraphNode to) {
        if (!graph.getEdges(from, to).hasMoreElements()) {
            graph.addEdge(new StdGraphEdge(), from, to);
            return true;
        } else {
            return false;
        }
    }
    
    private GraphNode getFunctionNode(Object fun) {
        Object o = thingsToNodes.get(fun);
        
        if (o == null) {
            GraphNode result = new DependenceFunctionNode(fun);
            
            thingsToNodes.put(fun, result);
            return result;
        } else {
            return (GraphNode)o;
        }
    }
    
    private GraphNode getClassNode(JBCClass c) {
        Object o = thingsToNodes.get(c);
        
        if (o == null) {
            GraphNode result = new DependenceClassNode(c);
            
            thingsToNodes.put(c, result);
            return result;
        } else {
            return (GraphNode)o;
        }
    }
    
    private void addCallReference(Graph g, Object caller, Object callee) {
        addEdgeConditionally(g, getFunctionNode(caller), getFunctionNode(callee));
    }
    
    private void addNewReference(Graph g, Object caller, JBCClass c) {
        addEdgeConditionally(g, getFunctionNode(caller), getClassNode(c));
    }
    
    private void addMethodReference(Graph g, JBCClass c, JBCMethod method) {
        addEdgeConditionally(g, getClassNode(c), getFunctionNode(method));
    }
    
    private void addMethodReferences(Graph g, CompactSet unresolvedMethods,
        JBCClass base, JBCClass c, CompactSet visited) {
        if (visited.get(c) == null) {
            visited.addUnconditionally(c);
            
            for (Enumeration e = c.getMethods(); e.hasMoreElements();) {
                JBCMethod m = (JBCMethod)e.nextElement();
                
                if (unresolvedMethods.get(m) != null) {
                    addMethodReference(g, base, base.getInheritedMethod(m));
                }
            }
            
            JBCClass[] superInterfaces = c.getSuperInterfaces();
            
            for (int i = 0; i < superInterfaces.length; i++) {
                addMethodReferences(g, unresolvedMethods, base,
                    superInterfaces[i], visited);
            }
            
            JBCClass s = c.getSuperClass();
            
            if (s != null) {
                addMethodReferences(g, unresolvedMethods, base, s, visited);
            }
        }
    }
    
    public void printReport(Writer w) throws IOException {
        Graph graph = new Graph();
        CompactSet unresolvedMethods = new CompactSet();
        CompactSet classes = new CompactSet();
        
        thingsToNodes = new Hashtable();
        
        for (Enumeration e = callSites.keys(); e.hasMoreElements();) {
            Location l = (Location)e.nextElement();
            Object value = callSites.get(l);
            
            if (value instanceof JBCMethod) {
                addCallReference(graph, l.getFunction(), value);
            } else if (value != NONE) {
                JBCMethod m = l.getCalledMethod();
                
                if (m != null) {
                    unresolvedMethods.add(m);
                }
            }
        }
        
        for (Enumeration e = staticCalls.getStaticCallLocations(); e.hasMoreElements();) {
            Location l = (Location)e.nextElement();
            
            addCallReference(graph, l.getFunction(), staticCalls.getStaticCallee(l));
        }
        
        for (Enumeration e = newSites.getNewLocations(); e.hasMoreElements();) {
            Location l = (Location)e.nextElement();
            JBCClass c = newSites.getNewClass(l);
            
            if (c != null) {
                classes.add(c);
                addNewReference(graph, l.getFunction(), c);
            }
        }
        
        for (Enumeration e = classes.elements(); e.hasMoreElements();) {
            JBCClass c = (JBCClass)e.nextElement();
            
            addMethodReferences(graph, unresolvedMethods, c, c, new CompactSet());
        }
        
        if (startMethods != null) {
            CompactSet startNodes = new CompactSet();
            
            for (Enumeration e = startMethods.elements(); e.hasMoreElements();) {
                Object fun = e.nextElement();
                GraphNode startNode = (GraphNode)thingsToNodes.get(fun);
            
                if (startNode != null) {
                    startNodes.add(startNode);
                }
            }
            
            if (startNodes.size() > 0) {
                CompactSet nodesToDelete =
                    GraphUtils.complementNodes(graph,
                        GraphReachability.findReachableNodesFrom(graph, startNodes));
                
                for (Enumeration e = nodesToDelete.elements(); e.hasMoreElements();) {
                    graph.removeNode((GraphNode)e.nextElement());
                }
            } else {
                w.write("!!! start position " + startArg + " was not live\n");
                return;
            }
        }
        
        DotWriter.write(w, graph, "ref_graph", this);
    }
    
    public static void main(String[] args) {
        GeneralBenchmark.run(args, new DependenceAnalyzer(), "[<prune-to-method>]");
    }
}
