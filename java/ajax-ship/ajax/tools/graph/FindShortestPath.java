/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.graph;

import java.util.*;
import ajax.util.graph.*;
import java.io.*;
import ajax.util.*;

public class FindShortestPath {
    public static void main(String[] argStrings) {
        Args args = new Args(argStrings,
            "Usage: <from-node> <to-node> [-undirected]");
        boolean isUndirected = args.extractBoolOption("-undirected");
        String fromArg = args.extractNextArg("from node");
        String toArg = args.extractNextArg("to node");
        
        args.checkDone();
            
        try {
            Reader in = new BufferedReader(new InputStreamReader(System.in));
            Writer out = new BufferedWriter(new OutputStreamWriter(System.out));
            DotReader r = DotReader.readGraph(in);

            DotGraphNode fromNode;
            DotGraphNode toNode;
            
            fromNode = r.findNode(fromArg);
            toNode = r.findNode(toArg);
            
            if (fromNode == null) {
                System.err.println("Couldn't find node: " + fromArg);
            } else if (toNode == null) {
                System.err.println("Couldn't find node: " + toArg);
            } else {
                Graph graph = r.getGraph();
                Vector path = GraphReachability.findShortestPath(graph, fromNode, toNode, isUndirected);
                
                if (path == null) {
                    out.write("No path found\n");
                } else {
                    CompactSet nodesInPath = new CompactSet();
                    
                    for (Enumeration e = path.elements(); e.hasMoreElements();) {
                        nodesInPath.add(e.nextElement());
                    }
                    
                    CompactSet nodesToDelete =
                        GraphUtils.complementNodes(graph, nodesInPath);
                            
                    for (Enumeration e = nodesToDelete.elements(); e.hasMoreElements();) {
                        graph.removeNode((GraphNode)e.nextElement());
                    }
                    
                    DotWriter.write(out, graph, r.getName(), new DotGraphFormatter());
                }
                
                out.flush();
            }
        } catch (DotReaderException ex) {
            ex.printStackTrace();
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }
}
