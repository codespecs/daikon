/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util.graph;

class GraphEdgeData {
    private GraphEdge edge;
    private GraphNodeData from;
    private GraphNodeData to;
    
    GraphEdgeData(GraphEdge edge, GraphNodeData from, GraphNodeData to) {
        this.edge = edge;
        this.from = from;
        this.to = to;
    }
    
    public boolean equals(Object o) {
        if (o instanceof GraphEdgeData) {
            GraphEdgeData d = (GraphEdgeData)o;
            
            return d.from.equals(from) && d.to.equals(to);
        } else {
            return false;
        }
    }
    
    public int hashCode() {
        return from.hashCode()*147091 + to.hashCode()*40101;
    }
    
    GraphNodeData getFrom() {
        return from;
    }
    
    GraphNodeData getTo() {
        return to;
    }
    
    GraphEdge getEdge() {
        return edge;
    }
}
