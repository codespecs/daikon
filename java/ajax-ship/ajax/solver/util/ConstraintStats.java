/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver.util;

import ajax.Globals;
import ajax.util.*;
import ajax.util.graph.*;
import java.util.*;
import java.io.*;

public class ConstraintStats {
    private static final Integer one = new Integer(1);
    
    private static void incrementTableCount(Hashtable table, int key) {
        Integer k = new Integer(key);
        Object o = table.get(k);
        
        if (o == null) {
            table.put(k, one);
        } else {
            table.put(k, new Integer(((Integer)o).intValue() + 1));
        }
    }
    
    private static void printTable(Writer w, Hashtable table, String keyName, String valName) throws IOException {
        w.write(keyName + "\t" + valName + "\tCumulative\tCumulative%\n");
        
        int maxKey = 0;
        
        for (Enumeration e = table.keys(); e.hasMoreElements();) {
            int key = ((Integer)e.nextElement()).intValue();
            
            if (key > maxKey) {
                maxKey = key;
            }
        }
        
        int[] values = new int[maxKey + 1];
        int total = 0;
        
        for (Enumeration e = table.keys(); e.hasMoreElements();) {
            Integer key = (Integer)e.nextElement();
            int val = ((Integer)table.get(key)).intValue();
            
            total += val;
            values[key.intValue()] = val;
        }
        
        int cumulant = 0;
        
        for (int i = 0; i < values.length; i++) {
            int val = values[i];
            
            if (val > 0) {
                cumulant += val;
                w.write(i + "\t" + val + "\t" + cumulant
                    + "\t" + (cumulant*100/total) + "\n");
            }
        }
    }
    
    private static void dumpEdgeStats(Graph g, Writer w, boolean incoming, boolean instances) throws IOException {
        Hashtable table = new Hashtable();
        
        for (Enumeration e = g.getNodes(); e.hasMoreElements();) {
            DotGraphNode node = (DotGraphNode)e.nextElement();
            int numEdges = 0;
            
            for (Enumeration e2 = incoming ? g.getPredecessorEdges(node) : g.getSuccessorEdges(node); e2.hasMoreElements();) {
                DotGraphEdge edge = (DotGraphEdge)e2.nextElement();
                
                if ((edge.getAttribute("style") != null) == instances) {
                    numEdges++;
                }
            }
            
            incrementTableCount(table, numEdges);
        }
        
        printTable(w, table,
            "Number of " + (incoming ? "incoming" : "outgoing") + " "
                + (instances ? "instance" : "component") + " edges",
            "Number of nodes");
    }
    
    public static void main(String[] argStrings) {
        try {
            VardumpTools tools = new VardumpTools(new BufferedReader(new InputStreamReader(System.in)));
            Writer out = new BufferedWriter(new OutputStreamWriter(System.out));
            Graph graph = tools.getGraph();

            dumpEdgeStats(graph, out, false, false);
            dumpEdgeStats(graph, out, false, true);
            dumpEdgeStats(graph, out, true, false);
            dumpEdgeStats(graph, out, true, true);
            out.flush();
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }
}
