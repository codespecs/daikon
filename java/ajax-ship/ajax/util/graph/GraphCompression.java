/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util.graph;

import ajax.Globals;
import java.util.*;
import ajax.util.*;

public class GraphCompression {
    public static final Object ignoreID = new StringBuffer("Ignore-Me-ID");
    
    private static void addItem(Hashtable table, Object key, Object value) {
        Object o = table.get(key);
        
        if (o == null) {
            table.put(key, value);
        } else if (o instanceof CompactSet) {
            ((CompactSet)o).add(value);
        } else if (!o.equals(value)) {
            CompactSet set = new CompactSet();
            
            set.add(o);
            set.add(value);
            table.put(key, set);
        }
    }
    
    private static Enumeration enumerateItems(Object o) {
        if (o == null) {
            return EmptyEnumerator.get();
        } else if (o instanceof CompactSet) {
            return ((CompactSet)o).elements();
        } else {
            return new SingletonEnumerator(o);
        }
    }

    private static void checkForCycles(Hashtable iso, Object key, CompactSet visited, CompactSet stack, String msg) {
        if (stack.get(key) != null) {
            System.err.println("Isomorphism:");
            for (Enumeration e = iso.keys(); e.hasMoreElements();) {
                Object k = e.nextElement();
                
                System.err.println(k + " ---> " + iso.get(k));
            }
            System.err.println("Cycle:");
            for (Enumeration e = stack.elements(); e.hasMoreElements();) {
                System.err.println(e.nextElement());
            }
            
            throw Globals.localError("Found a cycle: " + msg);
        } else if (visited.get(key) == null) {
            visited.addUnconditionally(key);
            
            stack.addUnconditionally(key);
            
            Object map = iso.get(key);
            
            if (map != null) {
                checkForCycles(iso, map, visited, stack, msg);
            }
            
            stack.remove(key);
        }
    }
    
    private static void checkForCycles(Hashtable iso, String msg) {  /* currently not used */
        CompactSet visited = new CompactSet();
        
        for (Enumeration e = iso.keys(); e.hasMoreElements();) {
            checkForCycles(iso, e.nextElement(), visited, new CompactSet(), msg);
        }
    }

    private static GraphNode mapNode(Hashtable iso, GraphNode node) {
        Object o = iso.get(node);
        
        if (o == null) {
            return null;
        } else {
            GraphNode map = (GraphNode)o;
            GraphNode result = mapNode(iso, map);
            
            if (result == null) {
                return map;
            } else {
                iso.put(node, result);
                return result;
            }
        }
    }
    
    private static GraphNode findMappedNode(Hashtable iso, GraphNode node) {
        GraphNode target = mapNode(iso, node);
        
        return target == null ? node : target;
    }
    
    public static void applyIsomorphismMap(Graph g, Hashtable iso, Hashtable edgeIDs) {
        for (Enumeration e = iso.keys(); e.hasMoreElements();) {
            GraphNode obsoleteNode = (GraphNode)e.nextElement();
            Vector successorEdges = new Vector();
                        
            for (Enumeration e2 = g.getSuccessorEdges(obsoleteNode); e2.hasMoreElements();) {
                successorEdges.addElement(e2.nextElement());
            }
            for (Enumeration e2 = successorEdges.elements(); e2.hasMoreElements();) {
                GraphEdge edge = (GraphEdge)e2.nextElement();
                
                if (edgeIDs.get(edge) == ignoreID) {
                    GraphNode newFromNode = mapNode(iso, obsoleteNode);
                    GraphNode newToNode = findMappedNode(iso, g.getToNode(edge));
                    
                    g.removeEdge(edge);
                    g.addEdge(edge, newFromNode, newToNode);
                } else {
                    g.removeEdge(edge);
                }
            }
        }

        for (Enumeration e = iso.keys(); e.hasMoreElements();) {
            GraphNode obsoleteNode = (GraphNode)e.nextElement();
                        
            for (Enumeration e2 = g.getPredecessorEdges(obsoleteNode); e2.hasMoreElements();) {
                GraphEdge edge = (GraphEdge)e2.nextElement();
                GraphNode predNode = g.getFromNode(edge);
                            
                g.removeEdge(edge);
                g.addEdge(edge, predNode, mapNode(iso, obsoleteNode));
            }
                        
            g.removeNode(obsoleteNode);
        }
    }
    
    private static boolean areNodesCompatible(GraphNode n1, GraphNode n2, Hashtable nodeIDs) {
        Object ID1 = nodeIDs.get(n1);
        Object ID2 = nodeIDs.get(n2);
        
        return ID1 != ignoreID && ID2 != ignoreID &&
            (n1.equals(n2)
                || (ID1 == null ? ID2 == null : ID2 != null && ID1.equals(ID2)));
    }
    
    private static boolean isIsomorphic(Graph g, Hashtable iso, GraphNode keepNode, GraphNode testNode,
        Hashtable nodesToIDsToEdges, Hashtable nodeIDs, Hashtable edgeIDs) {
        keepNode = findMappedNode(iso, keepNode);
        testNode = findMappedNode(iso, testNode);
        
        if (keepNode.equals(testNode)) {
            return true;
        } else if (areNodesCompatible(keepNode, testNode, nodeIDs)) {
            Hashtable keepIDsToEdges = (Hashtable)nodesToIDsToEdges.get(keepNode);
            CompactSet keepEdges = new CompactSet();
                    
            iso.put(testNode, keepNode);
                
            for (Enumeration e = g.getSuccessorEdges(keepNode); e.hasMoreElements();) {
                Object o = e.nextElement();
                
                if (edgeIDs.get(o) != ignoreID) {
                    keepEdges.add(o);
                }
            }
                
            int edgeCount = 0;
            
            for (Enumeration e = g.getSuccessorEdges(testNode); e.hasMoreElements();) {
                CompactSet matchingEdges;
                GraphEdge edge = (GraphEdge)e.nextElement();
                Object edgeID = edgeIDs.get(edge);
                GraphNode edgeTarget = g.getToNode(edge);
                    
                if (edgeID == null) {
                    matchingEdges = (CompactSet)keepEdges.clone();
                } else if (edgeID == ignoreID) {
                    continue;
                } else if (keepIDsToEdges != null) {
                    matchingEdges = new CompactSet();
                    for (Enumeration e2 = enumerateItems(keepIDsToEdges.get(edgeID)); e2.hasMoreElements();) {
                        matchingEdges.add(e2.nextElement());
                    }
                } else {
                    return false;
                }
                    
                for (Enumeration e2 = matchingEdges.elements(); e2.hasMoreElements();) {
                    GraphEdge edge2 = (GraphEdge)e2.nextElement();
                        
                    if (!areNodesCompatible(edgeTarget, g.getToNode(edge2), nodeIDs)) {
                        matchingEdges.remove(edge2);
                    }
                }
                    
                switch (matchingEdges.size()) {
                    case 0:
                        return false;
                            
                    case 1:
                        if (!isIsomorphic(g, iso, g.getToNode((GraphEdge)matchingEdges.elements().nextElement()),
                            edgeTarget, nodesToIDsToEdges, nodeIDs, edgeIDs)) {
                            return false;
                        } else {
                            break;
                        }
                        
                    default:
                        Globals.localError("Cannot handle isomorphism detection without unique edge IDs!");
                }
                    
                edgeCount++;
            }
                
            return edgeCount == keepEdges.size();
        } else {
            return false;
        }
    }
    
    public static Graph compressIsomorphs(GraphView g, Hashtable nodeIDs, Hashtable edgeIDs) {
        Graph result = Graph.copyFrom(g);
        
        computeIsomorphs(result, g, nodeIDs, edgeIDs);
        return result;
    }
    
    public static Hashtable computeIsomorphs(GraphView g, Hashtable nodeIDs, Hashtable edgeIDs) {
        return computeIsomorphs(Graph.copyFrom(g), g, nodeIDs, edgeIDs);
    }
    
    private static Hashtable computeIsomorphs(Graph result, GraphView g, Hashtable nodeIDs, Hashtable edgeIDs) {
        Hashtable nodesToIDsToEdges = new Hashtable();
        Hashtable isomorphism = new Hashtable();
        Hashtable IDsToNodes = new Hashtable();
        CompactSet scannedNodes = new CompactSet();
        CompactSet finalNodes = new CompactSet();
        
        for (Enumeration e = edgeIDs.keys(); e.hasMoreElements();) {
            GraphEdge edge = (GraphEdge)e.nextElement();
            GraphNode node = g.getFromNode(edge);
            
            if (node != null) {
                Hashtable IDsToEdges = (Hashtable)nodesToIDsToEdges.get(node);
                
                if (IDsToEdges == null) {
                    IDsToEdges = new Hashtable();
                    nodesToIDsToEdges.put(node, IDsToEdges);
                }
                
                addItem(IDsToEdges, edgeIDs.get(edge), edge);
            }
        }
        
        for (Enumeration e = g.getNodes(); e.hasMoreElements();) {
            GraphNode node = (GraphNode)e.nextElement();
            
            if (scannedNodes.get(node) == null) {
                scannedNodes.addUnconditionally(node);
                
                Enumeration e2;
                Object ID = nodeIDs.get(node);
                    
                if (ID != null) {
                    e2 = enumerateItems(IDsToNodes.get(ID));
                } else {
                    e2 = finalNodes.elements();
                }
                    
                Hashtable iso = null;
                    
                while (e2.hasMoreElements() && iso == null) {
                    GraphNode node2 = (GraphNode)e2.nextElement();
                        
                    if (!node2.equals(node) && finalNodes.get(node2) != null) {
                        iso = new Hashtable();
                            
                        if (!isIsomorphic(result, iso, node, node2, nodesToIDsToEdges,
                            nodeIDs, edgeIDs)) {
                            iso = null;
                        }
                    }
                }
                    
                if (iso != null) {
                    applyIsomorphismMap(result, iso, edgeIDs);
                        
                    for (Enumeration e3 = iso.keys(); e3.hasMoreElements();) {
                        Object k = e3.nextElement();
                        Object r = mapNode(iso, (GraphNode)k);
                        Object rID = nodeIDs.get(r);
                            
                        scannedNodes.add(k);
                        finalNodes.remove(k);
                        scannedNodes.add(r);
                        
                        finalNodes.add(r);
                        if (rID != null) {
                            addItem(IDsToNodes, rID, r);
                        }
                        
                        isomorphism.put(k, r);
                    }
                } else {
                    finalNodes.add(node);
                    if (ID != null) {
                        addItem(IDsToNodes, ID, node);
                    }
                }
            }
        }
        
        for (Enumeration e = isomorphism.keys(); e.hasMoreElements();) {
            mapNode(isomorphism, (GraphNode)e.nextElement());
        }
        
        return isomorphism;
    }
}
