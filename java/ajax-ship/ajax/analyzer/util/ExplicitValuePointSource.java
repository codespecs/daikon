/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.util;

import ajax.Globals;
import ajax.jbc.*;
import ajax.jbc.util.*;
import ajax.analyzer.*;
import java.util.*;
import ajax.util.*;

public class ExplicitValuePointSource implements DatumSpecifier {
/** The valuepoints indexed by function */
    private Hashtable sourcePoints = new Hashtable();
    private boolean pointsUsed = false;
    
    public static ExplicitValuePointSource makeFromQuery(JBCClassLoader loader, String expr) throws ParseException {
        ExplicitValuePointSource sources = new ExplicitValuePointSource();
        CompactSet p = QueryExpressionParser.parseExpression(loader, expr);

        for (Enumeration e = p.elements(); e.hasMoreElements();) {
            sources.addPoint((JBCValuePoint)e.nextElement());
        }
        
        return sources;
    }
    
    public void addPoint(JBCValuePoint p) {
        if (Globals.debug && pointsUsed) {
            Globals.localError("Points used early");
        }
        
        Object key = p.getLocation().getFunction();
        Object o = sourcePoints.get(key);
        CompactSet set;
        
        if (o == null) {
            set = new CompactSet();
            sourcePoints.put(key, set);
        } else {
            set = (CompactSet)o;
        }
        
        set.add(p);
    }

    private void registerPoints(IndirectionQueryFamily family, Object key) {
        if (Globals.debug) {
            pointsUsed = true;
        }
        
        Object o = sourcePoints.get(key);
        
        if (o != null) {
            for (Enumeration e = ((CompactSet)o).elements(); e.hasMoreElements();) {
                JBCValuePoint p = (JBCValuePoint)e.nextElement();
                Location l = p.getLocation();
                 
                if (l instanceof ExternalLocation) {
                    family.addSourceDatum(((ExternalLocation)l).getNode(), p.getExpression(), p);
                } else {
                    family.addSourceDatum(((JBCLocation)l).getOffset(), p.getExpression(), p);
                }
            }
        }
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, ExternalFlowgraph fg, Vector externalNodes) {
        registerPoints(family, method);
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, String name, ExternalFlowgraph fg, Vector externalNodes) {
        registerPoints(family, name);
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, byte[] code, boolean[] instructions) {
        registerPoints(family, method);
    }
}
