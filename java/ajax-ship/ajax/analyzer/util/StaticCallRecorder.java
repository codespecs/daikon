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

public class StaticCallRecorder implements DatumSpecifier, OpcodeConstants {
    private Vector staticCallSites = new Vector();
    
/**
This returns an Enumeration of the locations of live static calls.
Use the getStaticCallee method below to find the callee.

@return the Enumeration of static call locations
*/
    public Enumeration getStaticCallLocations() {
        return staticCallSites.elements();
    }
    
    public static Object getStaticCallee(Location location) {
        JBCMethod callee = location.getCalledMethod();
        
        if (callee == null) {
            if (Globals.debug && !(location instanceof ExternalLocation)) {
                throw Globals.nonlocalError("No callee at location: " + location);
            }
            
            ExternalCFGNode node = ((ExternalLocation)location).getNode();
            
            if (Globals.debug && !(node instanceof ExternalCFGFlowgraphInvocationDefNode)) {
                throw Globals.nonlocalError("No callee at location: " + location);
            }
            
            ExternalCFGFlowgraphInvocationDefNode funNode = (ExternalCFGFlowgraphInvocationDefNode)node;

            return funNode.getFunctionName(); 
        } else {
            return callee;
        }
    }
    
    private void addMethodCall(Location location) {
        staticCallSites.addElement(location);
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, ExternalFlowgraph fg, Vector externalNodes) {
        for (Enumeration e = externalNodes.elements(); e.hasMoreElements();) {
            Object o = e.nextElement();
            
            if (o instanceof ExternalCFGMethodInvocationDefNode) {
                ExternalCFGMethodInvocationDefNode node = (ExternalCFGMethodInvocationDefNode)o;
                JBCMethod m = node.getMethod(); 
               
                if (JBCCodeUtilities.useStaticDispatch(m)) {
                    addMethodCall(new ExternalLocation(method, node));
                }
            } else if (o instanceof ExternalCFGFlowgraphInvocationDefNode) {
                ExternalCFGFlowgraphInvocationDefNode node = (ExternalCFGFlowgraphInvocationDefNode)o;
               
                addMethodCall(new ExternalLocation(method, node));
            }
        }
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, String name, ExternalFlowgraph fg, Vector externalNodes) {
        for (Enumeration e = externalNodes.elements(); e.hasMoreElements();) {
            Object o = e.nextElement();
            
            if (o instanceof ExternalCFGMethodInvocationDefNode) {
                ExternalCFGMethodInvocationDefNode node = (ExternalCFGMethodInvocationDefNode)o;
                JBCMethod m = node.getMethod(); 
               
                if (JBCCodeUtilities.useStaticDispatch(m)) {
                    addMethodCall(new ExternalLocation(name, node));
                }
            } else if (o instanceof ExternalCFGFlowgraphInvocationDefNode) {
                ExternalCFGFlowgraphInvocationDefNode node = (ExternalCFGFlowgraphInvocationDefNode)o;
               
                addMethodCall(new ExternalLocation(name, node));
            }
        }
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, byte[] code, boolean[] instructions) {
        for (int i = 0; i < code.length; i++) {
            if (instructions[i]) {
                switch (code[i] & 0xFF) {
                    case OP_invokestatic:
                    case OP_invokespecial: {
                        JBCMethod m = JBCCodeUtilities.resolveInstructionMethod(method, code, i);

                        if (m != null) {
                            addMethodCall(new JBCLocation(method, i));
                        }
                        break;
                    }
                    
                    case OP_invokevirtual:
                    case OP_invokeinterface: {
                        JBCMethod m = JBCCodeUtilities.resolveInstructionMethod(method, code, i);

                        if (m != null && JBCCodeUtilities.useStaticDispatch(m)) {
                            addMethodCall(new JBCLocation(method, i));
                        }
                        break;
                    }
                }
            }
        }
    }
}
