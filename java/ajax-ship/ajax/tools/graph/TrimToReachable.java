/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.graph;

import java.util.*;
import ajax.util.graph.*;
import java.io.*;
import ajax.util.*;

public class TrimToReachable {
    public static void main(String[] argStrings) {
        Args args = new Args(argStrings,
            "Usage: [+|-]<source-node> [-undirected]");
        boolean isUndirected = args.extractBoolOption("-undirected");
        String arg = args.extractNextArg("source node");
        
        args.checkDone();
            
        try {
            Reader in = new BufferedReader(new InputStreamReader(System.in));
            Writer out = new BufferedWriter(new OutputStreamWriter(System.out));
            DotReader r = DotReader.readGraph(in);

            boolean reversed;
            DotGraphNode startNode;
            
            if (arg.startsWith("-")) {
                startNode = r.findNode(arg.substring(1));
                reversed = true;
            } else if (arg.startsWith("+")) {
                startNode = r.findNode(arg.substring(1));
                reversed = false;
            } else {
                startNode = r.findNode(arg);
                reversed = false;
            }
            
            if (startNode == null) {
                System.err.println("Couldn't find node: " + arg);
            } else {
                Graph graph = r.getGraph();
                GraphView searchGraph = reversed ? (GraphView)(new ReversedGraph(graph)) : (GraphView)graph;
                CompactSet nodesToDelete =
                    GraphUtils.complementNodes(graph,
                        GraphReachability.findReachableNodesFrom(searchGraph, startNode, isUndirected));
                        
                for (Enumeration e = nodesToDelete.elements(); e.hasMoreElements();) {
                    graph.removeNode((GraphNode)e.nextElement());
                }

                DotWriter.write(out, graph, r.getName(), new DotGraphFormatter());
                out.flush();
            }
        } catch (DotReaderException ex) {
            ex.printStackTrace();
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }
}
