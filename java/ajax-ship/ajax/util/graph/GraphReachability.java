/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util.graph;

import java.util.*;
import ajax.util.*;

public class GraphReachability {
    private static void addNextNodes(GraphView g, GraphNode n, boolean undirected, Vector result) {
        for (Enumeration e = g.getSuccessorEdges(n); e.hasMoreElements();) {
            result.addElement(g.getToNode((GraphEdge)e.nextElement()));
        }
        
        if (undirected) {
            for (Enumeration e = g.getPredecessorEdges(n); e.hasMoreElements();) {
                result.addElement(g.getFromNode((GraphEdge)e.nextElement()));
            }
        }
    }

    public static Vector findShortestPathUndirected(GraphView g, GraphNode from, GraphNode to) {
        return findShortestPath(g, from, to, true);
    }
    
    public static Vector findShortestPath(GraphView g, GraphNode from, GraphNode to) {
        return findShortestPath(g, from, to, false);
    }
    
    public static Vector findShortestPath(GraphView g, GraphNode from, GraphNode to, boolean undirected) {
        Hashtable predecessors = new Hashtable();
        Vector frontier = new Vector();
        Vector nextNodes = new Vector();
        
        frontier.addElement(from);
        predecessors.put(from, from);
        
        while (frontier.size() > 0) {
            Enumeration nodes = frontier.elements();
            
            frontier = new Vector();
            
            while (nodes.hasMoreElements()) {
                GraphNode n = (GraphNode)nodes.nextElement();
                
                nextNodes.removeAllElements();
                addNextNodes(g, n, undirected, nextNodes);
                
                for (Enumeration e = nextNodes.elements(); e.hasMoreElements();) {
                    GraphNode next = (GraphNode)e.nextElement();
                    
                    if (predecessors.get(next) == null) {
                        if (next.equals(to)) {
                            Stack result = new Stack();
                            
                            result.push(to);
                            for (GraphNode node = n; node != from;
                                node = (GraphNode)predecessors.get(node)) {
                                result.push(node);
                            }
                            result.push(from);
                            
                            Vector resultVector = new Vector();
                            
                            while (!result.isEmpty()) {
                                resultVector.addElement(result.pop());
                            }
                            return resultVector;
                        }
                        
                        predecessors.put(next, n);
                        frontier.addElement(next);
                    }
                }
            }
        }
        
        return null;
    }
    
    private static void addReachable(GraphView g, GraphNode node, CompactSet result, boolean undirected) {
        if (result.get(node) == null) {
            Stack stack = new Stack();
            Vector nextNodes = new Vector();
            
            stack.push(node);
            result.addUnconditionally(node);
            
            while (!stack.isEmpty()) {
                GraphNode n = (GraphNode)stack.pop();
                
                nextNodes.removeAllElements();
                addNextNodes(g, n, undirected, nextNodes);
                
                for (Enumeration e = nextNodes.elements(); e.hasMoreElements();) {
                    GraphNode next = (GraphNode)e.nextElement();
                    
                    if (result.get(next) == null) {
                        stack.push(next);
                        result.addUnconditionally(next);
                    }
                }
            }
        }
    }
    
    public static CompactSet findReachableNodesFromUndirected(GraphView g, CompactSet nodes) {
        return findReachableNodesFrom(g, nodes, true);
    }

    public static CompactSet findReachableNodesFrom(GraphView g, CompactSet nodes) {
        return findReachableNodesFrom(g, nodes, false);
    }

    public static CompactSet findReachableNodesFrom(GraphView g, CompactSet nodes, boolean undirected) {
        CompactSet result = new CompactSet();
        
        for (Enumeration e = nodes.elements(); e.hasMoreElements();) {
            addReachable(g, (GraphNode)e.nextElement(), result, undirected);
        }
        
        return result;
    }
    
    public static CompactSet findReachableNodesFromUndirected(GraphView g, GraphNode node) {
        return findReachableNodesFrom(g, node, true);
    }
    
    public static CompactSet findReachableNodesFrom(GraphView g, GraphNode node) {
        return findReachableNodesFrom(g, node, false);
    }
    
    public static CompactSet findReachableNodesFrom(GraphView g, GraphNode node, boolean undirected) {
        CompactSet result = new CompactSet();
        
        addReachable(g, node, result, undirected);
        
        return result;
    }
}
