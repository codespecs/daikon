/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util.graph;

import java.awt.Color;

public interface GraphFormatter {
    public static final int LINE_SOLID     = 1;
    public static final int LINE_DOTTED    = 2;
    public static final int LINE_DASHED    = 3;
    public static final int LINE_INVISIBLE = 4;
    
    public static final int SHAPE_ELLIPSE = 1;
    public static final int SHAPE_BOX     = 2;
    
    public String getNodeLabel(GraphNode node);
    public int getNodeLineStyle(GraphNode node);
    public int getNodeShape(GraphNode node);
    public Color getNodeLineColor(GraphNode node);
    public Color getNodeShadeColor(GraphNode node);
    
    public String getEdgeLabel(GraphEdge edge);
    public int getEdgeLineStyle(GraphEdge edge);
    public Color getEdgeLineColor(GraphEdge edge);
}
