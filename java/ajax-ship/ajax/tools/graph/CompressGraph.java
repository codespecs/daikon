/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.graph;

import java.util.*;
import ajax.util.graph.*;
import java.io.*;
import ajax.util.*;

public class CompressGraph {
    public static void main(String[] argStrings) {
        Args args = new Args(argStrings, "[<inputfile>] [-ignore <style>]");
        String ignoreStyle = args.extractStringOption("-ignore", null);
        String inFile = args.extractNextOptionalArg();
        
        args.checkDone();
            
        try {
            Reader in = new BufferedReader(
                inFile != null ? new FileReader(inFile)
                    : new InputStreamReader(System.in));
            Writer out = new BufferedWriter(new OutputStreamWriter(System.out));
            DotReader r = DotReader.readGraph(in);
            Hashtable nodeIDs = new Hashtable();
            Hashtable edgeIDs = new Hashtable();
            Graph g = r.getGraph();
            
            for (Enumeration e = g.getNodes(); e.hasMoreElements();) {
                GraphNode node = (GraphNode)e.nextElement();
                
                if (node instanceof DotGraphNode) {
                    String label = ((DotGraphNode)node).getAttribute("label");
                    
                    if (label != null) {
                        nodeIDs.put(node, label);
                    }
                }
            }
            
            for (Enumeration e = g.getEdges(); e.hasMoreElements();) {
                GraphEdge edge = (GraphEdge)e.nextElement();
                
                if (edge instanceof DotGraphEdge) {
                    String style = ((DotGraphEdge)edge).getAttribute("style");
                    String label = ((DotGraphEdge)edge).getAttribute("label");
                    
                    if (ignoreStyle != null && style.equals(ignoreStyle)) {
                        edgeIDs.put(edge, GraphCompression.ignoreID);
                    } else if (label != null) {
                        edgeIDs.put(edge, label);
                    }
                }
            }
            
            Hashtable map = GraphCompression.computeIsomorphs(g, nodeIDs, edgeIDs);
                
            GraphCompression.applyIsomorphismMap(g, map, edgeIDs);
            
            DotWriter.write(out, g, r.getName(), new DotGraphFormatter());
            out.flush();
        } catch (DotReaderException ex) {
            ex.printStackTrace();
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }
}
