/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util.graph;

import java.io.*;
import java.util.*;
import ajax.util.*;
import java.awt.Color;

public class DotWriter {
    private GraphView g;
    private GraphFormatter format;
    private Writer w;
    
    private DotWriter(GraphView g, GraphFormatter format, Writer w) {
        this.g = g;
        this.format = format;
        this.w = w;
    }
    
    public static void write(Writer w, GraphView g, String name, GraphFormatter format) throws IOException {
        (new DotWriter(g, format, w)).write(name);
    }
    
    public static boolean isSimpleLabel(String l) {
        int len = l.length();
        
        for (int i = 0; i < len; i++) {
            char ch = l.charAt(i);
            
            if (ch != '"' && !Character.isUnicodeIdentifierPart(ch)) {
                return false;
            }
        }
        
        return true;
    }
    
    public static String dotQuote(String s) {
        return '"' + Quote.quote(s) + '"';
    }
    
    private static String makeAttributeString(Vector attributes) {
        if (attributes.size() == 0) {
            return "";
        } else {
            StringBuffer buf = new StringBuffer();
            
            buf.append(" [");
            
            for (Enumeration e = attributes.elements(); e.hasMoreElements();) {
                if (buf.length() > 2) {
                    buf.append(", ");
                }
                buf.append(dotQuote((String)e.nextElement())).append('=')
                    .append(dotQuote((String)e.nextElement()));
            }
            
            buf.append(']');
            
            return buf.toString();
        }
    }
    
    private static void addAttrPair(Vector attributes, String attr, String val) {
        attributes.addElement(attr);
        attributes.addElement(val);
    }
    
    private static void addStyle(Vector attributes, int style) {
        switch (style) {
            case GraphFormatter.LINE_DOTTED:
                addAttrPair(attributes, "style", "dotted");
                break;
            case GraphFormatter.LINE_DASHED:
                addAttrPair(attributes, "style", "dashed");
                break;
            default:
                ;
        }
    }
    
    private static void addShape(Vector attributes, int shape, int lineStyle) {
        if (lineStyle == GraphFormatter.LINE_INVISIBLE) {
            addAttrPair(attributes, "shape", "plaintext");
        } else {
            switch (shape) {
                case GraphFormatter.SHAPE_BOX:
                    addAttrPair(attributes, "shape", "box");
                    break;
                case GraphFormatter.SHAPE_ELLIPSE:
                    addAttrPair(attributes, "shape", "ellipse");
                    break;
                default:
                    ;
            }
        }
    }
    
    private void write(String name) throws IOException {
        Hashtable nodesToIndices = new Hashtable();
        CompactSet usedNames = new CompactSet();
        int nodeCount = 0;
        
        w.write("digraph " + name + " {\n");

        Vector attributes = new Vector();
        
        for (Enumeration e = g.getNodes(); e.hasMoreElements();) {
            GraphNode node = (GraphNode)e.nextElement();
            String s;
            String label = format.getNodeLabel(node);
            
            if (node instanceof DotGraphNode) {
                s = ((DotGraphNode)node).getName();
            } else if (label != null && isSimpleLabel(label)) {
                s = label;
            } else {
                s = null;
            }
            
            while (s == null || usedNames.get(s) != null) {
                s = "_" + nodeCount;
                nodeCount++;
            }
            
            usedNames.addUnconditionally(s);
            nodesToIndices.put(node, s);
            
            int lineStyle = format.getNodeLineStyle(node);
            int shape = format.getNodeShape(node);
            Color lineColor = format.getNodeLineColor(node);
            Color shadeColor = format.getNodeShadeColor(node);
            
            attributes.removeAllElements();
            
            addStyle(attributes, lineStyle);
            addShape(attributes, shape, lineStyle);
            if (label == null || !label.equals(s)) {
                attributes.addElement("label");
                attributes.addElement(label != null ? label : "");
            }
            
            if (attributes.size() > 0) {
                w.write(dotQuote(s) + makeAttributeString(attributes) + "\n");
            }
        }
        
        for (Enumeration e = g.getEdges(); e.hasMoreElements();) {
            GraphEdge edge = (GraphEdge)e.nextElement();
            String label = format.getEdgeLabel(edge);
            int lineStyle = format.getEdgeLineStyle(edge);
            Color lineColor = format.getEdgeLineColor(edge);
            
            attributes.removeAllElements();
            
            addStyle(attributes, lineStyle);
            if (label != null) {
                attributes.addElement("label");
                attributes.addElement(label);
            }
            
            w.write(dotQuote((String)nodesToIndices.get(g.getFromNode(edge))) + " -> "
                + dotQuote((String)nodesToIndices.get(g.getToNode(edge)))
                + makeAttributeString(attributes) + "\n");
        }
        
        w.write("}\n");
    }
}
