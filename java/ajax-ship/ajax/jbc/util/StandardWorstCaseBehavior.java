/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.Globals;
import ajax.util.*;
import ajax.jbc.*;
import java.util.*;

/**
This implementation of worst case behavior assumes that the analysis
does not detect strong updates.
*/
public class StandardWorstCaseBehavior extends JBCNativeCodeLoader implements UnknownBehavior, OpcodeConstants {
    private static final int FIELD_INT = 0;
    private static final int FIELD_LONG = 1;
    private static final int FIELD_FLOAT = 2;
    private static final int FIELD_DOUBLE = 3;
    private static final int FIELD_OBJECT = 4;
    private static final String[] fieldNames = {
        "int", "long", "float", "double", "Object"
    };
    
    private static int globalSerialNumber = 0;
    
    private int serialNumber = makeSerialNumber();
    private CompactSet accessibleClasses;
    private CompactSet classes;
    private Hashtable generatedFunctions = null;
    private Hashtable generatedMethods = new Hashtable();
    private UserField[] fields = new UserField[5];
    
    private int makeSerialNumber() {
        int result = globalSerialNumber;
        
        globalSerialNumber++;
        return result;
    }
    
    public StandardWorstCaseBehavior(JBCWorld w, CompactSet classes, CompactSet accessibleClasses) {
        super(w);
        
        initUserFields();
        
        this.classes = classes;
        this.accessibleClasses = accessibleClasses;
    }
    
    private void initUserFields() {
        JBCClass object = getWorld().getSpecialClass("java.lang.Object");
        
        for (int i = 0; i < fields.length; i++) {
            String name = getPrefix() + fieldNames[i];
            
            fields[i] = object.registerUserField(name, true);
        }
    }
    
    private String getPrefix() {
        return "StandardWorstCaseBehavior_" + serialNumber + "_";
    }
    
    private ExternalNode makeFieldGetter(JBCField f, ExternalCFGVariable tmp,
        ExternalCFGVariable obj, ExternalNode succ) {
        ExternalNode getF;
        
        if (f.isStatic()) {
            getF = new ExternalFieldNode(tmp, f);
        } else {
            getF = new ExternalFieldNode(tmp, obj, f);
        }
        
        ExternalNode set =
            new ExternalUserFieldAssignmentNode(getTypeField(f.getFieldType()), tmp);
        
        getF.setSuccessor(set);
        set.setSuccessor(succ);
        return getF;
    }
    
    private ExternalNode makeFieldSetter(JBCField f, ExternalCFGVariable tmp,
        ExternalCFGVariable obj, ExternalNode succ) {
        // Remember, native code can set final fields, so checking for 'final'
        // here would be incorrect
        ExternalNode setF;
        
        if (f.isStatic()) {
            setF = new ExternalFieldAssignmentNode(f, tmp);
        } else {
            setF = new ExternalFieldAssignmentNode(obj, f, tmp);
        }
        
        ExternalNode get =
            new ExternalUserFieldNode(tmp, getTypeField(f.getFieldType()));
        
        get.setSuccessor(setF);
        setF.setSuccessor(succ);
        return get;
    }
    
    private ExternalNode makeMethodCall(JBCMethod m, ExternalCFGVariable tmp,
        ExternalCFGVariable obj, ExternalCFGVariable exn, ExternalNode succ) {
        JBCMethodType mType = m.getMethodType();
        JBCType[] paramTypes = mType.getParameterTypes();
        ExternalAnonymousVariable[] params = new ExternalAnonymousVariable[paramTypes.length];
        ExternalNode firstNode = null;
        ExternalNode lastParamNode = null;
        
        for (int i = 0; i < params.length; i++) {
            params[i] = new ExternalAnonymousVariable();
            
            ExternalNode getParam = new ExternalUserFieldNode(params[i], getTypeField(paramTypes[i]));
            
            if (lastParamNode != null) {
                lastParamNode.setSuccessor(getParam);
            } else {
                firstNode = getParam;
            }
            lastParamNode = getParam;
        }
        
        ExternalMethodCallNode callNode = new ExternalMethodCallNode(tmp, m, params);
        JBCType returnType = mType.getReturnType();
        ExternalNode set = returnType.equals(JBCType.VOID) ? (ExternalNode)callNode :
            new ExternalUserFieldAssignmentNode(getTypeField(returnType), tmp);
        ExternalDefNode[] callList = { callNode };
        ExternalNode catcher = new ExternalCatchNode(exn, callList, null);
        ExternalNode set2 =
            new ExternalUserFieldAssignmentNode(fields[FIELD_OBJECT], exn);
        ExternalNode[] catcherSuccessors = { succ, set2 };
        
        if (firstNode == null) {
            firstNode = callNode;
        } else {
            lastParamNode.setSuccessor(callNode);
        }
        callNode.setSuccessor(set);
        set.setSuccessor(catcher);
        catcher.setSuccessors(catcherSuccessors);
        set2.setSuccessor(succ);
        return firstNode;
    }
    
    private ExternalNode makeNew(JBCClass c, ExternalCFGVariable tmp,
        ExternalNode succ) {
        throw new Error("Unimplemented");
    }
        
    private static void conditionalAdd(Vector v, Object o) {
        if (o != null) {
            v.addElement(o);
        }
    }
    
    private ExternalFlowgraph generateExecutionEngine() {
        ExternalNode root = new ExternalNode();
        Vector operations = new Vector();
        ExternalAnonymousVariable tmp = new ExternalAnonymousVariable();
        ExternalAnonymousVariable obj = new ExternalAnonymousVariable();
        ExternalAnonymousVariable exn = new ExternalAnonymousVariable();
        ExternalNode setObj = new ExternalUserFieldNode(obj, fields[FIELD_OBJECT]);
        
        root.setSuccessor(setObj);
        
        ExternalNode branch = setObj;
        
        for (Enumeration e = accessibleClasses.elements(); e.hasMoreElements();) {
            JBCClass c = (JBCClass)e.nextElement();
            
            conditionalAdd(operations, makeNew(c, tmp, root));
           
            for (Enumeration e2 = c.getFields(); e2.hasMoreElements();) {
                JBCField f = (JBCField)e2.nextElement();
                
                conditionalAdd(operations, makeFieldGetter(f, tmp, obj, root));
                conditionalAdd(operations, makeFieldSetter(f, tmp, obj, root));
            }

            for (Enumeration e2 = c.getMethods(); e2.hasMoreElements();) {
                JBCMethod m = (JBCMethod)e2.nextElement();
                
                conditionalAdd(operations, makeMethodCall(m, tmp, obj, exn, root));
            }
        }
        
        ExternalNode[] nodes = new ExternalNode[operations.size()];
        
        operations.copyInto(nodes);
       
        branch.setSuccessors(nodes);

        return new ExternalFG(root);
    }
    
    private void generateBasicFunctions() {
        StandardClassLoader loader = new StandardClassLoader(getWorld());
        JBCClass operators = loader.getClass("WorstCaseOperators");
        
        if (operators == null) {
            Globals.userError("Cannot load class WorstCaseOperators");
        } else {
            generatedFunctions = new Hashtable();
            
            generatedFunctions.put(getPrefix() + "execute", generateExecutionEngine());
        }
    }
    
    private UserField getTypeField(JBCType t) {
        if (t instanceof JBCObjectType) {
            return fields[FIELD_OBJECT];
        } else {
            String name = ((JBCBaseType)t).getName();
                
            for (int index = FIELD_DOUBLE; index > FIELD_INT; index--) {
                if (fieldNames[index].equals(name)) {
                    return fields[index];
                }
            }
            
            return fields[FIELD_INT];
        }
    }
    
    private ExternalFlowgraph generateMethodBehavior(JBCMethod m) {
        if (generatedFunctions == null) {
            generateBasicFunctions();
            
            if (generatedFunctions == null) {
                return null;
            }
        }
        
        JBCMethodType methodType = m.getMethodType();
        JBCType[] params = methodType.getParameterTypes();
        
        ExternalNode root = new ExternalNode();
        ExternalNode last = root;
        ExternalAnonymousVariable tmp = new ExternalAnonymousVariable();
            
        for (int i = 0; i < params.length; i++) {
            ExternalParameterNode param =
                new ExternalParameterNode(tmp, i);
                
            ExternalUserFieldAssignmentNode node =
                new ExternalUserFieldAssignmentNode(getTypeField(params[i]), tmp);
                
            param.setSuccessor(node);
                
            // make the assignment conditional
            ExternalNode join = new ExternalNode();
            
            node.setSuccessor(join);
                
            ExternalNode[] succs = { param, join };
                
            last.setSuccessors(succs);
            last = join;
        }
        
        ExternalFlowgraphCallNode call =
            new ExternalFlowgraphCallNode(getPrefix() + "execute", new ExternalCFGVariable[0]);
            
        JBCType returnType = methodType.getReturnType();
        ExternalDefNode result;
        
        if (returnType.equals(JBCType.VOID)) {
            result = call;
        } else {
            result = new ExternalUserFieldNode(null, getTypeField(returnType));
            
            call.setSuccessor(result);
        }

        return new ExternalFG(root, result);
    }
    
    protected ExternalFlowgraph loadMethodCode(JBCMethod method) {
        return null;
    }
    
    protected ExternalFlowgraph loadFunctionCode(String name) {
        return (ExternalFlowgraph)generatedFunctions.get(name);
    }
    
    public ExternalFlowgraph getBehavior(JBCMethod m) {
        ExternalFlowgraph result = (ExternalFlowgraph)generatedMethods.get(m);
        
        if (result == null) {
            result = generateMethodBehavior(m);
            generatedMethods.put(m, result);
        }
        
        return result;   
    }
}
