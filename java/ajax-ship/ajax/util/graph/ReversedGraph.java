/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util.graph;

import java.util.*;

public class ReversedGraph implements GraphView {
    private GraphView g;
    
    public ReversedGraph(GraphView g) {
        this.g = g;
    }
    
    public Enumeration getNodes() {
        return g.getNodes();
    }
    
    public Enumeration getEdges() {
        return g.getEdges();
    }
    
    public GraphNode getFromNode(GraphEdge edge) {
        return g.getToNode(edge);
    }
    
    public GraphNode getToNode(GraphEdge edge) {
        return g.getFromNode(edge);
    }
    
    public Enumeration getSuccessorEdges(GraphNode node) {
        return g.getPredecessorEdges(node);
    }
    
    public Enumeration getPredecessorEdges(GraphNode node) {
        return g.getSuccessorEdges(node);
    }
}
