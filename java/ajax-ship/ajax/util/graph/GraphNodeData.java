/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util.graph;

import ajax.Globals;
import java.util.*;
import ajax.util.*;

class GraphNodeData {
    private GraphNode node;
    
    // null, singleton, or an IdentityCompactSet
    private Object inEdges = null;
    // null, singleton or a Hashtable mapping nodes to singletons or IdentityCompactSets of EdgeDatas
    private Object outEdges = null;
    
    GraphNodeData(GraphNode node) {
        this.node = node;
    }
    
    GraphNode getNode() {
        return node;
    }
    
    void addInEdge(GraphEdgeData edge) {
        if (inEdges == null) {
            inEdges = edge;
        } else if (inEdges instanceof IdentityCompactSet) {
            ((IdentityCompactSet)inEdges).addUnconditionally(edge);
        } else {
            IdentityCompactSet set = new IdentityCompactSet();
            
            set.addUnconditionally(inEdges);
            set.addUnconditionally(edge);
            inEdges = set;
        }
    }
    
    void removeInEdge(GraphEdgeData edge) {
        if (inEdges instanceof IdentityCompactSet) {
            IdentityCompactSet set = (IdentityCompactSet)inEdges;
            
            if (set.remove(edge) == null) {
                if (Globals.debug) {
                    Globals.localError("Edge not found");
                }
            }
            
            if (Globals.debug && set.size() == 0) {
                Globals.localError("Empty in edge set?");
            }
                
            if (set.size() == 1) {
                inEdges = set.elements().nextElement();
            }
        } else {
            if (Globals.debug && inEdges != edge) {
                Globals.localError("Edge mismatch");
            }
            
            inEdges = null;
        }
    }
    
    private void addToOutMap(Hashtable map, GraphEdgeData edge) {
        GraphNode toNode = edge.getTo().getNode();
        Object item = map.get(toNode);
        
        if (item == null) {
            map.put(toNode, edge);
        } else if (item instanceof IdentityCompactSet) {
            ((IdentityCompactSet)item).addUnconditionally(edge);
        } else {
            IdentityCompactSet set = new IdentityCompactSet();
            
            set.addUnconditionally(item);
            set.addUnconditionally(edge);
            map.put(toNode, set);
        }
    }
    
    void addOutEdge(GraphEdgeData edge) {
        if (outEdges == null) {
            outEdges = edge;
        } else if (outEdges instanceof Hashtable) {
            addToOutMap((Hashtable)outEdges, edge);
        } else {
            Hashtable map = new Hashtable();
            
            addToOutMap(map, (GraphEdgeData)outEdges);
            addToOutMap(map, edge);
            outEdges = map;
        }
    }
    
    void removeOutEdge(GraphEdgeData edge) {
        if (outEdges instanceof Hashtable) {
            Hashtable table = (Hashtable)outEdges;
            GraphNode toNode = edge.getTo().getNode();
            Object nodeEdges = table.get(toNode);
            
            if (nodeEdges instanceof IdentityCompactSet) {
                IdentityCompactSet set = (IdentityCompactSet)nodeEdges;
                
                if (set.remove(edge) == null) {
                    if (Globals.debug) {
                        Globals.localError("Edge not found");
                    }
                }
                
                if (Globals.debug && set.size() == 0) {
                    Globals.localError("Empty out edge set?");
                }
                
                if (set.size() == 1) {
                    table.put(toNode, set.elements().nextElement());
                }
            } else if (Globals.debug && nodeEdges == null) {
                Globals.localError("Edge not found");
            } else {
                table.remove(toNode);
            }

            if (table.size() == 1) {
                Object o = table.elements().nextElement();
                    
                if (o instanceof GraphEdgeData) {
                    outEdges = o;
                }
            }
        } else {
            if (Globals.debug && outEdges != edge) {
                Globals.localError("Edge mismatch");
            }
            
            outEdges = null;
        }
    }
    
    Enumeration getEdges(GraphNode toNode) {
        if (outEdges == null) {
            return EmptyEnumerator.get();
        } else if (outEdges instanceof Hashtable) {
            Object edgesToNode = ((Hashtable)outEdges).get(toNode);
            
            if (edgesToNode == null) {
                return EmptyEnumerator.get();
            } else if (edgesToNode instanceof IdentityCompactSet) {
                return new EdgeSetEnumerator(((IdentityCompactSet)edgesToNode).elements());
            } else {
                return new SingletonEnumerator(((GraphEdgeData)edgesToNode).getEdge());
            }
        } else {
            GraphEdgeData data = (GraphEdgeData)outEdges;
            
            if (data.getTo().getNode().equals(toNode)) {
                return new SingletonEnumerator(data.getEdge());
            } else {
                return EmptyEnumerator.get();
            }
        }
    }
    
    Enumeration getInEdges() {
        if (inEdges == null) {
            return EmptyEnumerator.get();
        } else if (inEdges instanceof IdentityCompactSet) {
            return ((IdentityCompactSet)inEdges).elements();
        } else {
            return new SingletonEnumerator(inEdges);
        }
    }
    
    Enumeration getOutEdges() {
        if (outEdges == null) {
            return EmptyEnumerator.get();
        } else if (outEdges instanceof Hashtable) {
            return new OutEdgeEnumerator(((Hashtable)outEdges).elements());
        } else {
            return new SingletonEnumerator(outEdges);
        }
    }
}
