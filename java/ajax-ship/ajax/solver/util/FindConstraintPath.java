/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver.util;

import ajax.Globals;
import ajax.util.*;
import ajax.util.graph.*;
import java.util.*;
import java.io.*;

public class FindConstraintPath {
    public static void main(String[] argStrings) {
        Args args = new Args(argStrings,
            "Usage: <from-node> <to-node>");
        String fromArg = args.extractNextArg("from node");
        String toArg = "M-" + args.extractNextArg("to node");
        
        args.checkDone();
            
        try {
            VardumpTools tools = new VardumpTools(new BufferedReader(new InputStreamReader(System.in)));
            Writer out = new BufferedWriter(new OutputStreamWriter(System.out));
            Graph graph = tools.makeInstanceSearchGraph();

            DotGraphNode fromNode;
            DotGraphNode toNode;
            
            fromNode = DotReader.findNode(graph, fromArg);
            toNode = DotReader.findNode(graph, toArg);
            
            if (fromNode == null) {
                System.err.println("Couldn't find node: " + fromArg);
            } else if (toNode == null) {
                System.err.println("Couldn't find node: " + toArg);
            } else {
                Vector path = GraphReachability.findShortestPath(graph, fromNode, toNode);
                
                if (path == null) {
                    out.write("No path found\n");
                } else {
                    DotGraphNode lastNode = null;
                    
                    for (Enumeration e = path.elements(); e.hasMoreElements();) {
                        DotGraphNode node = (DotGraphNode)e.nextElement();
                        
                        if (lastNode != null) {
                            for (Enumeration e2 = graph.getEdges(lastNode, node); e2.hasMoreElements();) {
                                DotGraphEdge edge = (DotGraphEdge)e2.nextElement();
                                
                                System.out.println(lastNode.getName() + " -> " + node.getName() + " [label="
                                    + DotWriter.dotQuote(edge.getAttribute("label")) + "]");
                            }
                        }
                        
                        System.out.println(node.getName() + " [label="
                            + DotWriter.dotQuote(node.getAttribute("label")) + "]");
                        
                        lastNode = node;
                    }
                }
                
                out.flush();
            }
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }
}
