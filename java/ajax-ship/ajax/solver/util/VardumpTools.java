/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver.util;

import ajax.Globals;
import ajax.util.graph.*;
import java.util.*;
import java.io.*;
import ajax.util.*;

public class VardumpTools {
    public static final int UNKNOWN = -1;
    public static final int COMPONENT = 0;
    public static final int INSTANCE = 1;
    
    private DotReader r;
        
    public VardumpTools(Reader in) throws IOException {
        try {
            r = DotReader.readGraph(in);
        } catch (DotReaderException ex) {
            Globals.nonlocalError("Invalid vardump: " + ex.getMessage());
        }
    }
    
    private static String getCluster(DotGraphNode node) {
        String label = node.getAttribute("label");
            
        if (label != null) {
            String[] labels = StringUtils.split(label, '\n');
            
            for (int i = 0; i < labels.length; i++) {
                String l = labels[i];
                
                if (l.startsWith("{")) {
                    int spaceIndex = l.indexOf(' ');
                    
                    if (spaceIndex > 1) {
                        return l.substring(1, spaceIndex);
                    }
                }
            }
        }
        
        return null;
    }
    
    private static String getVar(DotGraphNode node) {
        return node.getName();
    }
    
    private Hashtable makeNodeClusterMap() {
        Hashtable result = new Hashtable();
        
        for (Enumeration e = r.getGraph().getNodes(); e.hasMoreElements();) {
            DotGraphNode node = (DotGraphNode)e.nextElement();
            String cluster = getCluster(node);
            
            if (cluster != null) {
                result.put(node, cluster);
            }
        }
        
        return result;
    }
    
    private Hashtable makeClusterNodes(Hashtable nodesToClusters, Graph g) {
        Hashtable result = new Hashtable();
        
        for (Enumeration e = nodesToClusters.elements(); e.hasMoreElements();) {
            String cluster = (String)e.nextElement();
            Object o = result.get(cluster);
            DotGraphNode node;
            
            if (o == null) {
                node = new DotGraphNode(cluster);
                
                result.put(cluster, node);
                g.addNode(node);
            } else {
                node = (DotGraphNode)o;
            }
        }
        
        return result;
    }
    
    private static int getEdgeKind(DotGraphEdge edge) {
        String style = edge.getAttribute("style");
        
        if (style == null) {
            return COMPONENT;
        } else if (style.equals("dashed")) {
            return INSTANCE;
        } else {
            return UNKNOWN;
        }
    }
    
    public Graph getGraph() {
        return r.getGraph();
    }
    
    public Graph removeGlobals() {
        Graph result = (Graph)r.getGraph().clone();
        CompactSet nodesToDelete = new CompactSet();
        
        for (Enumeration e = result.getNodes(); e.hasMoreElements();) {
            DotGraphNode node = (DotGraphNode)e.nextElement();
            String label = node.getAttribute("label");
            
            if (label != null && label.indexOf("GLOBAL") >= 0) {
                nodesToDelete.add(node);
            }
        }
        
        for (Enumeration e = nodesToDelete.elements(); e.hasMoreElements();) {
            result.removeNode((GraphNode)e.nextElement());
        }
        
        return result;
    }
    
    private static DotGraphNode makeMirrorNode(Graph g, Hashtable nodesToMirrors, DotGraphNode node) {
        Object o = nodesToMirrors.get(node);
        
        if (o != null) {
            return (DotGraphNode)o;
        } else {
            DotGraphNode result = new DotGraphNode("M-" + node.getName());
            DotGraphEdge mirrorEdge = new DotGraphEdge();
            
            result.setAttribute("label", "Mirror of " + node.getAttribute("label"));
            
            mirrorEdge.setAttribute("label", "Mirror edge");
            
            nodesToMirrors.put(node, result);
            
            g.addEdge(mirrorEdge, node, result);
            
            return result;
        }
    }
    
    public Graph makeInstanceSearchGraph() {
        Graph g = new Graph();
        Hashtable nodesToMirrors = new Hashtable();
        Graph dot = r.getGraph();
        
        for (Enumeration e = dot.getEdges(); e.hasMoreElements();) {
            DotGraphEdge edge = (DotGraphEdge)e.nextElement();
            
            if (getEdgeKind(edge) == INSTANCE) {
                DotGraphNode fromNode = (DotGraphNode)dot.getFromNode(edge);
                DotGraphNode toNode = (DotGraphNode)dot.getToNode(edge);
                DotGraphNode toMirrorNode = makeMirrorNode(g, nodesToMirrors, toNode);
                DotGraphNode fromMirrorNode = makeMirrorNode(g, nodesToMirrors, fromNode);
                DotGraphEdge newEdge = new DotGraphEdge();
                
                newEdge.setAttribute("label", "Mirror of " + edge.getAttribute("label"));
                
                g.addEdge(edge, fromNode, toNode);
                g.addEdge(newEdge, toMirrorNode, fromMirrorNode);
            }
        }
        
        return g;
    }
    
    public Graph makeClusterGraph() {
        Graph g = new Graph();
        Hashtable nodesToClusters = makeNodeClusterMap();
        Hashtable clustersToNodes = makeClusterNodes(nodesToClusters, g);
        Graph dot = r.getGraph();
        
        for (Enumeration e = dot.getEdges(); e.hasMoreElements();) {
            DotGraphEdge edge = (DotGraphEdge)e.nextElement();
            
            if (getEdgeKind(edge) == INSTANCE) {
                String fromCluster = (String)nodesToClusters.get((DotGraphNode)dot.getFromNode(edge));
                String toCluster = (String)nodesToClusters.get((DotGraphNode)dot.getToNode(edge));
                
                if (fromCluster != null && toCluster != null) {
                    DotGraphNode fromClusterNode = (DotGraphNode)
                        clustersToNodes.get(fromCluster);
                    DotGraphNode toClusterNode = (DotGraphNode)
                        clustersToNodes.get(toCluster);
                
                    if (!g.getEdges(fromClusterNode, toClusterNode).hasMoreElements()) {
                        g.addEdge(edge, fromClusterNode, toClusterNode);
                    }
                }
            }
        }
        
        return g;
    }
}
