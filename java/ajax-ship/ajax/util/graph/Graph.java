/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util.graph;

import ajax.Globals;
import ajax.util.*;
import java.util.*;

public class Graph implements GraphView, Cloneable {
    private Hashtable nodeToNodeData = new Hashtable();
    private Hashtable edgeToEdgeData = new Hashtable();
    
    public Graph() {
    }

    private GraphNodeData internalAddNode(GraphNode node) {
        GraphNodeData newData = new GraphNodeData(node);
        
        nodeToNodeData.put(node, newData);
        return newData;
    }
    
    public void addNode(GraphNode node) {
        if (nodeToNodeData.get(node) == null) {
            internalAddNode(node);
        }
    }
    
    public void removeNode(GraphNode node) {
        GraphNodeData nodeData = (GraphNodeData)nodeToNodeData.remove(node);
        
        if (nodeData != null) {
            for (Enumeration e = nodeData.getInEdges(); e.hasMoreElements();) {
                GraphEdgeData edgeData = (GraphEdgeData)e.nextElement();
                
                edgeData.getFrom().removeOutEdge(edgeData);
                edgeToEdgeData.remove(edgeData.getEdge());
            }

            for (Enumeration e = nodeData.getOutEdges(); e.hasMoreElements();) {
                GraphEdgeData edgeData = (GraphEdgeData)e.nextElement();
                
                edgeData.getTo().removeInEdge(edgeData);
                edgeToEdgeData.remove(edgeData.getEdge());
            }
        }
    }
    
    public Enumeration getEdges(GraphNode from, GraphNode to) {
        Object fromData = nodeToNodeData.get(from);
        
        if (fromData != null) {
            return ((GraphNodeData)fromData).getEdges(to);
        } else {
            return EmptyEnumerator.get();
        }
    }
    
    public void addEdge(GraphEdge edge, GraphNode from, GraphNode to) {
        Object data = edgeToEdgeData.get(edge);
        
        if (data == null) {
            GraphNodeData fromData = (GraphNodeData)nodeToNodeData.get(from);
            
            if (fromData == null) {
                fromData = internalAddNode(from);
            }
            
            GraphNodeData toData = (GraphNodeData)nodeToNodeData.get(to);
            
            if (toData == null) {
                toData = internalAddNode(to);
            }
            
            GraphEdgeData newData = new GraphEdgeData(edge, fromData, toData);
            
            edgeToEdgeData.put(edge, newData);
            
            fromData.addOutEdge(newData);
            toData.addInEdge(newData);
        } else if (Globals.debug) {
            GraphEdgeData edgeData = (GraphEdgeData)data;
            
            if (!edgeData.getFrom().getNode().equals(from)
                || !edgeData.getTo().getNode().equals(to)) {
                Globals.nonlocalError("Adding mismatching edge");
            }
        }
    }
    
    public void removeEdge(GraphEdge edge) {
        GraphEdgeData data = (GraphEdgeData)edgeToEdgeData.remove(edge);
        
        if (data != null) {
            data.getFrom().removeOutEdge(data);
            data.getTo().removeInEdge(data);
        }
    }
    
    public Enumeration getNodes() {
        return nodeToNodeData.keys();
    }
    
    public Enumeration getEdges() {
        return edgeToEdgeData.keys();
    }
    
    public boolean hasNode(GraphNode node) {
        return nodeToNodeData.get(node) != null;
    }
    
    public boolean hasEdge(GraphEdge edge) {
        return edgeToEdgeData.get(edge) != null;
    }
    
    public GraphNode getFromNode(GraphEdge edge) {
        GraphEdgeData edgeData = (GraphEdgeData)edgeToEdgeData.get(edge);
        
        if (edgeData != null) {
            return edgeData.getFrom().getNode();
        } else {
            return null;
        }
    }
    
    public GraphNode getToNode(GraphEdge edge) {
        GraphEdgeData edgeData = (GraphEdgeData)edgeToEdgeData.get(edge);
        
        if (edgeData != null) {
            return edgeData.getTo().getNode();
        } else {
            return null;
        }
    }
    
    public Enumeration getSuccessorEdges(GraphNode node) {
        return new EdgeDataToEdgeFilter(((GraphNodeData)nodeToNodeData.get(node)).getOutEdges());
    }
    
    public Enumeration getPredecessorEdges(GraphNode node) {
        return new EdgeDataToEdgeFilter(((GraphNodeData)nodeToNodeData.get(node)).getInEdges());
    }
    
    public Object clone() {
        return copyFrom(this);
    }
    
    public static Graph copyFrom(GraphView g) {
        Graph result = new Graph();
        
        for (Enumeration e = g.getNodes(); e.hasMoreElements();) {
            result.addNode((GraphNode)e.nextElement());
        }

        for (Enumeration e = g.getEdges(); e.hasMoreElements();) {
            GraphEdge edge = (GraphEdge)e.nextElement();
            
            result.addEdge(edge, g.getFromNode(edge), g.getToNode(edge));
        }
        
        return result;
    }
}
