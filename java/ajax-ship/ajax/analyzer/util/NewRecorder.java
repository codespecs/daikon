/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.util;

import java.util.*;
import ajax.jbc.*;
import ajax.jbc.util.*;
import ajax.analyzer.*;
import ajax.util.*;
import ajax.Globals;

public class NewRecorder implements DatumSpecifier, OpcodeConstants {
    private Vector newSites = new Vector();
    
/**
This returns an Enumeration of the locations of live "new" instructions.

@return the Enumeration of static call locations
*/
    public Enumeration getNewLocations() {
        return newSites.elements();
    }
    
    public static JBCClass getNewClass(Location location) {
        if (location instanceof JBCLocation) {
            JBCLocation l = (JBCLocation)location;
            JBCMethod m = l.getMethod();
            
            return JBCCodeUtilities.resolveInstructionClass(m, m.getData().getCode(), l.getOffset());
        } else if (!Globals.debug || location instanceof ExternalLocation) {
            ExternalCFGNode node = ((ExternalLocation)location).getNode();
            
            if (Globals.debug && !(node instanceof ExternalCFGNewObjectDefNode)) {
                throw Globals.nonlocalError("No callee at location: " + location);
            }
            
            ExternalCFGNewObjectDefNode newNode = (ExternalCFGNewObjectDefNode)node;

            return newNode.getObjectClass(); 
        } else {
            throw Globals.nonlocalError("No new at location: " + location);
        }
    }
    
    private void addNewSite(Location location) {
        newSites.addElement(location);
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, ExternalFlowgraph fg, Vector externalNodes) {
        for (Enumeration e = externalNodes.elements(); e.hasMoreElements();) {
            Object o = e.nextElement();
            
            if (o instanceof ExternalCFGNewObjectDefNode) {
                ExternalCFGNewObjectDefNode node = (ExternalCFGNewObjectDefNode)o;
                
                addNewSite(new ExternalLocation(method, node));
            }
        }
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, String name, ExternalFlowgraph fg, Vector externalNodes) {
        for (Enumeration e = externalNodes.elements(); e.hasMoreElements();) {
            Object o = e.nextElement();
            
            if (o instanceof ExternalCFGNewObjectDefNode) {
                ExternalCFGNewObjectDefNode node = (ExternalCFGNewObjectDefNode)o;
                
                addNewSite(new ExternalLocation(name, node));
            }
        }
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, byte[] code, boolean[] instructions) {
        for (int i = 0; i < code.length; i++) {
            if (instructions[i]) {
                switch (code[i] & 0xFF) {
                    case OP_new:
                    case OP_newarray:
                    case OP_anewarray:
                    case OP_multianewarray:
                        addNewSite(new JBCLocation(method, i));
                        break;
                }
            }
        }
    }
}
