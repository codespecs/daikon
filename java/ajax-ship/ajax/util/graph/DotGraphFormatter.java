/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util.graph;

import java.io.*;
import java.util.*;
import java.awt.Color;

public class DotGraphFormatter implements GraphFormatter {
    public String getNodeLabel(GraphNode node) {
        DotGraphNode n = (DotGraphNode)node;
        String label = n.getAttribute("label");
        
        if (label != null) {
            return label;
        } else {
            return n.getName();
        }
    }
    
    private int convertShape(String shape) {
        if (shape == null) {
            return SHAPE_ELLIPSE;
        } else if (shape.equals("box")) {
            return SHAPE_BOX;
        } else {
            return SHAPE_ELLIPSE;
        }
    }
    
    private int convertStyle(String style) {
        if (style == null) {
            return LINE_SOLID;
        } else if (style.equals("dashed")) {
            return LINE_DASHED;
        } else if (style.equals("dotted")) {
            return LINE_DOTTED;
        } else if (style.equals("invis")) {
            return LINE_INVISIBLE;
        } else {
            return LINE_SOLID;
        }
    }
    
    public int getNodeLineStyle(GraphNode node) {
        DotGraphNode n = (DotGraphNode)node;
        String shape = n.getAttribute("shape");
        
        if (shape == null || !shape.equals("plaintext")) {
            return convertStyle(((DotGraphNode)node).getAttribute("style"));
        } else {
            return LINE_INVISIBLE;
        }
    }
    
    public int getNodeShape(GraphNode node) {
        return convertShape(((DotGraphNode)node).getAttribute("shape"));
    }
    
    public Color getNodeLineColor(GraphNode node) {
        return Color.black;
    }
    
    public Color getNodeShadeColor(GraphNode node) {
        return null;
    }
    
    public String getEdgeLabel(GraphEdge edge) {
        return ((DotGraphEdge)edge).getAttribute("label");
    }
    
    public int getEdgeLineStyle(GraphEdge edge) {
        return convertStyle(((DotGraphEdge)edge).getAttribute("style"));
    }
    
    public Color getEdgeLineColor(GraphEdge edge) {
        return Color.black;
    }
}
