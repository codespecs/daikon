/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util.graph.test;

import java.util.*;
import ajax.util.graph.*;

public class Test1 {
    private final static int N = 7;
    private final static int K = 1;
    private final static int L = 3;
    
    private static int succ(int i, int j) {
        return (i + L*(j + 1))%N;
    }
    
    private static void error() {
        System.err.println("Err!");
    }
    
    private static void runTest() {
        GraphNode[] nodes = new GraphNode[N];
        
        Graph g = new Graph();
        
        for (int i = 0; i < N; i++) {
            nodes[i] = new Test1Node(i);
            g.addNode(nodes[i]);
        }
        
        for (int i = 0; i < N; i++) {
            for (int j = 0; j < K; j++) {
                g.addEdge(new StdGraphEdge(), nodes[i], nodes[succ(i, j)]);
            }
        }
        
        for (int i = 0; i < N; i++) {
            int count = 0;
            
            for (Enumeration e = g.getSuccessorEdges(nodes[i]); e.hasMoreElements();) {
                GraphEdge edge = (GraphEdge)e.nextElement();
                count++;
            }
            
            if (count != K) {
                error();
            }
        }
        
        for (int i = 0; i < N; i++) {
            int count = 0;
            
            for (Enumeration e = g.getPredecessorEdges(nodes[i]); e.hasMoreElements();) {
                GraphEdge edge = (GraphEdge)e.nextElement();
                count++;
            }
            
            if (count != K) {
                error();
            }
        }
        
        for (int i = 0; i < N*20; i += N) {
            if (GraphReachability.findReachableNodesFrom(g, nodes[i/20]).size() != N) {
                error();
            }
        }
    }
    
    public static void main(String[] args) {
        runTest();
    }
}
