/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util.graph;

import java.io.*;
import java.util.*;
import java.awt.Color;

public class DotReader {
    private String name;
    private StreamTokenizer stream;
    private Graph graph = new Graph();
    private Hashtable namesToNodes = new Hashtable();
    
    private DotReader(StreamTokenizer stream) {
        this.stream = stream;
    }
    
    private DotReaderException error(String s) {
        return new DotReaderException(s + ", reading: " + stream.toString());
    }
    
    private void skipConstant(String w) throws DotReaderException {
        if (stream.ttype != stream.TT_WORD || !stream.sval.equals(w)) {
            throw error("Expected '" + w + "'");
        }
    }
    
    private String readWord(String description) throws DotReaderException {
        if (stream.ttype != stream.TT_WORD && stream.ttype != '"'
            && stream.ttype != '\'') {
            throw error("Expected " + description);
        } else {
            return stream.sval.intern();
        }
    }
    
    private void skipToken(char ch) throws DotReaderException {
        if (stream.ttype != ch) {
            throw error("Expected character '" + ch + "'");
        }
    }
    
    private DotGraphNode getNode(String name) {
        Object o = namesToNodes.get(name);
        
        if (o == null) {
            DotGraphNode n = new DotGraphNode(name);
            
            namesToNodes.put(name, n);
            
            graph.addNode(n);
            
            return n;
        } else {
            return (DotGraphNode)o;
        }
    }
    
    private Vector readAttributes() throws IOException, DotReaderException {
        Vector attrs = new Vector();
        
        if (stream.ttype == '[') {
            stream.nextToken();
            
            while (stream.ttype != ']') {
                String attrName = readWord("attribute name");
                
                stream.nextToken();
                skipToken('=');
                
                stream.nextToken();
                
                String attrValue = readWord("attribute value");
                
                attrs.addElement(attrName);
                attrs.addElement(attrValue);
                
                stream.nextToken();
                if (stream.ttype == ',') {
                    stream.nextToken();
                }
            }
            stream.nextToken();
        }
        
        return attrs;
    }
    
    private void readNode(String w) throws IOException, DotReaderException {
        DotGraphNode node = getNode(w);
        
        for (Enumeration e = readAttributes().elements(); e.hasMoreElements();) {
            node.setAttribute((String)e.nextElement(), (String)e.nextElement());
        }
    }
    
    private void readEdge(String w) throws IOException, DotReaderException {
        DotGraphNode from = getNode(w);
        
        skipToken('-');
        
        stream.nextToken();
        skipToken('>');
        
        stream.nextToken();
        
        DotGraphNode to = getNode(readWord("node name"));
        
        stream.nextToken();
        
        DotGraphEdge edge = new DotGraphEdge();
        
        graph.addEdge(edge, from, to);
        
        for (Enumeration e = readAttributes().elements(); e.hasMoreElements();) {
            edge.setAttribute((String)e.nextElement(), (String)e.nextElement());
        }
    }
    
    private void read() throws IOException, DotReaderException {
        stream.nextToken();
        skipConstant("digraph");
        stream.nextToken();
        name = readWord("graph name");
        stream.nextToken();
        skipToken('{');
        
        stream.nextToken();
        
        while (stream.ttype != '}') {
            String w = readWord("node name");
                
            stream.nextToken();

            if (stream.ttype == '-') {
                readEdge(w);
            } else {
                readNode(w);
            }
            
            if (stream.ttype == ';') {
                stream.nextToken();
            }
        }
    }
    
    public static DotReader readGraph(Reader r) throws IOException, DotReaderException {
        StreamTokenizer t = new StreamTokenizer(r);
        
        t.wordChars('0', '9');
        t.wordChars('_', '_');
        t.ordinaryChar('-');
        t.ordinaryChar('.');
        
        DotReader result = new DotReader(t);
        
        result.read();
        
        return result;
    }
    
    public String getName() {
        return name;
    }
    
    public Graph getGraph() {
        return graph;
    }
    
    public DotGraphNode findNode(String name) {
        return findNode(getGraph(), name);
    }
        
    public static DotGraphNode findNode(Graph graph, String name) {
        for (Enumeration e = graph.getNodes(); e.hasMoreElements();) {
            DotGraphNode node = (DotGraphNode)e.nextElement();
                    
            if (node.getName().equals(name)) {
                return node;
            }
        }
        
        return null;
    }
}
