/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util.graph.test;

import java.util.*;
import ajax.util.graph.*;
import java.io.*;
import ajax.util.*;

public class DotTest {
    public static void main(String[] args) {
        try {
            Reader in = new BufferedReader(new InputStreamReader(System.in));
            Writer out = new BufferedWriter(new OutputStreamWriter(System.out));
            DotReader r = DotReader.readGraph(in);
            Graph g = r.getGraph();
            CompactSet nodes = new CompactSet();
           
            for (Enumeration e = g.getNodes(); e.hasMoreElements();) {
                DotGraphNode node = (DotGraphNode)e.nextElement();
                
                if (node.getAttribute("label") == null) {
                    nodes.add(node);
                }
            }
            
            for (Enumeration e = nodes.elements(); e.hasMoreElements();) {
                g.removeNode((GraphNode)e.nextElement());
            }
            
            DotWriter.write(out, g, r.getName(), new DotGraphFormatter());
            out.flush();
        } catch (DotReaderException ex) {
            ex.printStackTrace();
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }
}
