/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util.graph;

import java.util.*;

public interface GraphView {
    public Enumeration getNodes();
    public Enumeration getEdges();
    public GraphNode getFromNode(GraphEdge edge);
    public GraphNode getToNode(GraphEdge edge);
    public Enumeration getSuccessorEdges(GraphNode node);
    public Enumeration getPredecessorEdges(GraphNode node);
}
