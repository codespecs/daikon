/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util.salamis;

import java.util.*;
import ajax.Globals;
import ajax.jbc.*;
import ajax.jbc.util.*;
import ajax.util.IdentityManager;

public class SalamisFlowgraph extends ExternalFG implements java.io.Serializable {
    private Hashtable params = null;
    private JBCClassLoader classLoader = null;
    
    SalamisFlowgraph(ExternalCFGNode root, ExternalCFGDefNode result,
        ExternalCFGDefNode exception) {
        super(root, result, exception);
    }
    
    public void setMacroParameters(Hashtable params) {
        if (this.params != null) {
            Globals.nonlocalError("Macro parameters may only be set once");
        }
        
        this.params = params;
    }
    
    public void setClassLoader(JBCClassLoader classLoader) {
        if (this.classLoader != null) {
            Globals.nonlocalError("Class loader may only be set once");
        }
        
        this.classLoader = classLoader;
    }
    
    void verify() throws InvalidFlowgraphError {
        for (Enumeration e = ExternalFlowgraphUtilities.getDefNodes(this).elements();
            e.hasMoreElements();) {
            Object o = e.nextElement();
            
            if (o instanceof SalamisNode) {
                ((SalamisNode)o).verify();
            }
        }
    }
    
    JBCClass resolveClass(String name) {
        if (params != null) {
            String param = (String)params.get(name);
            
            if (param != null) {
                name = param;
            }
        }
        
        if (name.startsWith("[L")) {
            name = name + ";";
        }
        
        JBCClass c = classLoader.getClass(name);
        
        if (c == null) {
            Globals.nonlocalError("Class " + name + " not found in Salamis class resolution");
        }
        
        return c;
    }
    
    JBCField resolveField(String name) {
        if (params != null) {
            String param = (String)params.get(name);
            
            if (param != null) {
                name = param;
            }
        }
        
        int lastDot = name.lastIndexOf('.');
        String className = name.substring(0, lastDot);
        String fieldName = name.substring(lastDot + 1);
        JBCClass c = classLoader.getClass(className);
        
        if (c == null) {
            Globals.nonlocalError("Class " + className + " not found in Salamis field resolution");
        }
        
        return c.getField(fieldName);
    }
    
    UserField resolveUserField(String name, boolean isStatic) {
        if (params != null) {
            String param = (String)params.get(name);
            
            if (param != null) {
                name = param;
            }
        }
        
        int lastHash = name.lastIndexOf('#');
        String className = name.substring(0, lastHash);
        String fieldName = name.substring(lastHash + 1);
        JBCClass c = classLoader.getClass(className);
        
        if (c == null) {
            Globals.nonlocalError("Class " + className + " not found in Salamis user field resolution");
        }
        
        return c.registerUserField(fieldName, isStatic);
    }
    
    JBCMethod resolveMethod(String name, String signature, int numParams)
        throws UnresolvedClassException, MissingMethodException, AmbiguousMethodException {
        if (params != null) {
            String param = (String)params.get(name);
            
            if (param != null) {
                name = param;
            }
        }
        
        return JBCParserUtilities.findMethod(classLoader, name, signature, numParams, false);
    }
    
    String resolveFlowgraph(String name) {
        if (params != null) {
            String param = (String)params.get(name);
            
            if (param != null) {
                name = param;
            }
        }
        
        if (name.lastIndexOf('.') >= 0) {
            Globals.nonlocalError("Expected a native function, got method: " + name);
        }
        
        return name;
    }
    
    public int hashCode() {
        return IdentityManager.getIdentityHashCode(this);
    }
}
