/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util.salamis;

import ajax.jbc.*;
import ajax.jbc.util.*;
import java.util.*;
import java.io.*;
import ajax.Globals;

public class SalamisCodeLoader extends JBCNativeCodeLoader {
    private static long uniqueID = 0;
    
    private Hashtable codeTable = new Hashtable();
    private JBCClassLoader loader;
    
    public static final String defaultMainSpecificationResource = "main-harness.csal";
    public static final String defaultNativeSpecificationResource = "native-spec.csal";
    
    public SalamisCodeLoader(JBCClassLoader loader) {
        super(loader.getWorld());
        
        this.loader = loader;
        
        loadNativeCodeSpecifications(defaultNativeSpecificationResource);
    }
    
    protected void installCode(Hashtable newCode) {
        for (Enumeration e = newCode.keys(); e.hasMoreElements();) {
            Object key = e.nextElement();
            
            codeTable.put(key, newCode.get(key));
        }
    }
    
    protected Hashtable readCode(JBCClassLoader overrideLoader, String resourceName, Hashtable params) {
        try {
            ObjectInputStream in = new ObjectInputStream(new BufferedInputStream(
                getClass().getResourceAsStream("/" + resourceName)));
            Hashtable code = (Hashtable)in.readObject();
            Vector elementsToDelete = new Vector();
            
            for (Enumeration e = code.keys(); e.hasMoreElements();) {
                Object key = e.nextElement();
                SalamisFlowgraph fg = (SalamisFlowgraph)code.get(key);
                
                if (params != null) {
                    fg.setMacroParameters(params);
                }
                if (overrideLoader != null) {
                    fg.setClassLoader(overrideLoader);
                } else {
                    fg.setClassLoader(loader);
                }
                
                try {
                    fg.verify();
                } catch (InvalidFlowgraphError ex) {
                    Globals.writeLog(this, "ERROR: loading native code specification for " + key + ": " + ex.getMessage());
                    elementsToDelete.addElement(key);
                }
            }
            
            for (Enumeration e = elementsToDelete.elements(); e.hasMoreElements();) {
                code.remove(e.nextElement());
            }
            
            return code;
        } catch (IOException ex) {
            Globals.userError("Cannot read native code specification resource: " + resourceName
                + " (" + ex + ")");
            return new Hashtable();
        } catch (ClassNotFoundException ex) {
            Globals.nonlocalError("Required \"Salamis\" code not included in this program (missing class "
                + ex + ")");
            return new Hashtable();
        }
    }
    
    public String makeMainInvocation(JBCClassLoader loader, String className) throws UnresolvedClassException, MissingMethodException, AmbiguousMethodException {
        String result = "SalamisCodeLoader-main#" + uniqueID++;
        Hashtable params = new Hashtable();
        String methodName = className + ".main";

	// Make sure that the user's main method can be resolved
        JBCParserUtilities.findMethod(loader, methodName, "([Ljava.lang.String;)V", 1, false);
        
        params.put("$.main", methodName);
        
        if (loader.getClass("java.lang.System").getMethod("initializeSystemClass", "()V")
            != null) {
            params.put("$initSystem", "initSystemWithInitializeSystemClass");
        } else {
            params.put("$initSystem", "initSystemWithoutInitializeSystemClass");
        }            
        
        Hashtable code = readCode(loader, defaultMainSpecificationResource, params);
        SalamisFlowgraph mainFun = (SalamisFlowgraph)code.remove("main");
        
        if (mainFun == null) {
	    Globals.nonlocalError("Main invocation specification file does not contain a main function");
	    return null;
        }

        code.put(result, mainFun);
        installCode(code);
        
        return result;
    }
    
    public void loadNativeCodeSpecifications(String fileName) {
        installCode(readCode(null, fileName, null));
    }
    
    protected ExternalFlowgraph loadFunctionCode(String name) {
        return (ExternalFlowgraph)codeTable.get(name);
    }
    
    protected ExternalFlowgraph loadMethodCode(JBCMethod m) {
        return (ExternalFlowgraph)codeTable.get(m.getContainingClass().getClassName()
            + "." + m.getMethodName());
    }
}
