/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.Globals;
import ajax.solver.*;
import ajax.analyzer.semi.*;
import ajax.analyzer.semantics.*;
import ajax.jbc.util.*;
import ajax.jbc.*;
import java.util.*;

class FlowgraphConstraintGenerator {
    private ConstraintManager manager;
    private String name;
    private JBCMethod method;
    private InvocationContext context;
    private ExternalFlowgraph fg;
    private int uniqueOffset = 0;
    private Variable combiner;
    private boolean makeInitialized = true;
    
    private static final Object NEED_CARRIED = new String("NEED_CARRIED");
    private static final Object IS_CARRIED = new String("IS_CARRIED");
    
    FlowgraphConstraintGenerator(ConstraintManager manager, String name,
        InvocationContext context) {
        this(manager, name, null, manager.getNativeCode(name), context);
    }

    FlowgraphConstraintGenerator(ConstraintManager manager, JBCMethod method,
        ExternalFlowgraph fg, InvocationContext context) {
        this(manager, null, method, fg, context);
    }
    
    private static String getCompleteMethodName(JBCMethod method) {
        return method.getContainingClass().getClassName() + "." + method.getMethodName();
    }

    private FlowgraphConstraintGenerator(ConstraintManager manager,
        String name, JBCMethod method, ExternalFlowgraph fg, InvocationContext context) {
        this.manager = manager;
        this.name = name;
        this.method = method;
        this.context = context;
        this.fg = fg;
        
        if (manager.getSemantics() instanceof CombiningSemantics) {
            combiner = new Variable(manager.getSolver());
        } else {
            combiner = null;
        }
        
        if (method != null) {
            if (getCompleteMethodName(method).equals("java.lang.System.arraycopy")) {
                makeInitialized = false;
            }
        }
    }
    
    private ExternalLocation makeLocalLocation(ExternalCFGNode node) {
        return name != null ? new ExternalLocation(name, node)
            : new ExternalLocation(method, node);
    }
    
    private String getName() {
        return name != null ? name.toString() : method.toString();
    }
    
    private static void invalid(String s) {
        throw new InvalidFlowgraphError(s);
    }
    
    private void combineWith(World w, Variable v) {
        if (combiner != null) {
            combiner.makeEqual(w, v);
        }
    }
    
    private static void computePredecessorInfo(ExternalCFGNode node,
        Hashtable nodeInfo, Hashtable varInfo) {
        if (nodeInfo.get(node) == null) {
            ExternalCFGNodeInfo info = new ExternalCFGNodeInfo();
            
            nodeInfo.put(node, info);
            
            for (Enumeration e = node.getSuccessors(); e.hasMoreElements();) {
                ExternalCFGNode successor = (ExternalCFGNode)e.nextElement();
                
                computePredecessorInfo(successor, nodeInfo, varInfo);
                
                ((ExternalCFGNodeInfo)nodeInfo.get(successor)).predecessors
                    .addElement(node);
            }
            
            if (node instanceof ExternalCFGDefNode) {
                ExternalCFGVariable[] variables = ((ExternalCFGDefNode)node).getDefinedVariables();
                
                if (node instanceof ExternalCFGFlowgraphInvocationDefNode
                    || node instanceof ExternalCFGMethodInvocationDefNode) {
                    info.defVars = new Variable[3];
                } else {
                    info.defVars = new Variable[variables.length];
                }
                
                for (int i = 0; i < variables.length; i++) {
                    ExternalCFGVariable variable = variables[i];
                    
                    if (varInfo.get(variable) == null) {                    
                        varInfo.put(variable, new ExternalCFGVariableInfo());
                    }
                }
            }
        }
    }
    
    private static void computeBlockInfo(ExternalCFGNode node, Hashtable nodeInfo,
        Hashtable varInfo) {
        ExternalCFGNodeInfo info = (ExternalCFGNodeInfo)nodeInfo.get(node);
        
        if (info.blockHead == null) {
            Vector info_predecessors = info.predecessors;
            ExternalCFGNode head;
            
            if (info_predecessors.size() != 1) {
                head = node;
            } else {
                ExternalCFGNode pred = (ExternalCFGNode)info_predecessors.elementAt(0);
                
                computeBlockInfo(pred, nodeInfo, varInfo);
                
                ExternalCFGNodeInfo predInfo = (ExternalCFGNodeInfo)nodeInfo.get(pred);
                
                head = predInfo.blockHead;
            }
            
            info.blockHead = head;
            
            if (node instanceof ExternalCFGDefNode) {
                ExternalCFGVariable[] variables = ((ExternalCFGDefNode)node).getDefinedVariables();
                
                for (int i = 0; i < variables.length; i++) {
                    ExternalCFGVariableInfo variableInfo = (ExternalCFGVariableInfo)varInfo.get(variables[i]);
                    
                    variableInfo.defNodeHeads.put(head, head);
                }
            }
        }
    }
    
    private static void makePredecessorEdgeInstances(Hashtable nodeInfo, Hashtable varInfo, World w) {
        for (Enumeration e = nodeInfo.keys(); e.hasMoreElements();) {
            ExternalCFGNode node = (ExternalCFGNode)e.nextElement();
            ExternalCFGNodeInfo info = (ExternalCFGNodeInfo)nodeInfo.get(node);
            Vector info_predecessors = info.predecessors;
            int info_predecessors_size = info_predecessors.size();
            
            if (info_predecessors_size > 1) {
                InstanceLabel[] instances = new InstanceLabel[info_predecessors_size];
                Enumeration preds = info_predecessors.elements();
                
                info.funVar = new Variable(w);
                
                for (int i = 0; i < info_predecessors_size; i++) {
                    instances[i] = new ExternalCFGEdgeInstance(
                        (ExternalCFGNode)preds.nextElement(), node);
                }
                
                info.predecessorEdgesVarsCarried = new Hashtable();
                info.predecessorEdgesComponentsCarried = new Hashtable();
                info.predecessorEdgeInstances = instances;
            }
            
            computeBlockInfo(node, nodeInfo, varInfo);
        }
    }
    
/**
This function returns a Variable that represents the value of the
'v' variable. This Variable is in the context of the type environment
associated with the 'from' node.
*/
    private Variable traceNodeDef(ExternalCFGNode from,
        ExternalCFGVariable v, Hashtable nodeInfo, Hashtable varInfo, World w) {
        if (Globals.debug && v == null) {
            invalid("Missing variable specification");
        }
            
        ExternalCFGVariableInfo vInfo = (ExternalCFGVariableInfo)varInfo.get(v);
        
        if (Globals.debug && vInfo == null) {
            invalid("Cannot find any definition for variable " + v);
        }
        
        ExternalCFGNodeInfo fromInfo = (ExternalCFGNodeInfo)nodeInfo.get(from);
        ExternalCFGNode fromInfo_blockHead = fromInfo.blockHead;

        if (vInfo.defNodeHeads.get(fromInfo_blockHead) != null) {
            Vector predecessors = fromInfo.predecessors;
            
            while (predecessors.size() == 1) {
                ExternalCFGNode pred = (ExternalCFGNode)predecessors.elementAt(0);
                ExternalCFGNodeInfo predInfo = (ExternalCFGNodeInfo)nodeInfo.get(pred);
                
                if (pred instanceof ExternalCFGDefNode) {
                    ExternalCFGVariable[] defs = ((ExternalCFGDefNode)pred).getDefinedVariables();

                    for (int i = 0; i < defs.length; i++) {
                        if (defs[i].equals(v)) {
                            addNodeConstraints(pred, nodeInfo, varInfo, w);
                            
                            return predInfo.defVars[i];
                        }
                    }
                }
                
                predecessors = predInfo.predecessors;
            }
        }
        
        ExternalCFGNodeInfo headInfo = (ExternalCFGNodeInfo)nodeInfo.get(fromInfo_blockHead);
        Vector headInfo_predecessors = headInfo.predecessors;
        int headInfo_predecessors_size = headInfo_predecessors.size();
        
        if (Globals.debug && headInfo_predecessors_size == 0) {
            invalid("Cannot find def node on path from root, var="
                + v + ", use=" + from);
        }
        
        ComponentLabel component = vInfo.component;
        
        if (component == null) {
            component = new ExternalCFGVariableComponent(v);
            vInfo.component = component;
        }
        
        Hashtable varsCarried = headInfo.predecessorEdgesVarsCarried;
        Object carried = varsCarried.get(v);
        Variable result = headInfo.funVar.getComponent(w, Variable.CMODE, component);
        
        if (carried == null) {
            varsCarried.put(v, NEED_CARRIED);
        }
        
        return result;
    }
    
    private Variable makeNewObject(ExternalCFGNewObjectDefNode node, Hashtable nodeInfo, JBCClass c) {
        Variable objVar = manager.getClassVar(c);
        JBCMethod finalizer = c.getInheritedMethod("finalize", "()V");
        World w = manager.getSolver();
        
        if (!manager.getSingletonUsageDetector().isClassInstantiatedOnce(c)) {
            objVar = objVar.getInstance(w, ExternalCFGNewObjectInstance.get(node));
        }
        
        for (Enumeration e = c.getInheritedFields(); e.hasMoreElements();) {
            JBCField f = (JBCField)e.nextElement();
                
            if (!f.isStatic() && !(f.getFieldType() instanceof JBCObjectType)) {
                manager.makeInitializedValue(manager.makeNonstaticField(Variable.CMODE, objVar, f, Variable.DMODE));
            }
        }
        
        if (finalizer != null && !finalizer.getContainingClass().equals(manager.getJavaLangObject())) {
            InvocationContext callContext =
                ExternalFlowgraphInvocationContext.get(context, fg, node);
            Variable finalizerVar = manager.getMethodVar(callContext, finalizer)
                .getInstance(w, ExternalCFGMethodInvocationInstance.get(node));

            if (manager.makeGlobalComponents) {
                finalizerVar.getComponent(w, Variable.DMODE, JBCMethodGlobalsArgComponent.get()).
                    makeEqual(w, pushToRoot(node, JBCMethodGlobalsArgComponent.get(), nodeInfo, w));
            }
            finalizerVar.getComponent(w, Variable.DMODE, JBCMethodArgComponent.get(0))
                .makeEqual(w, objVar);
        }
        
        return objVar;
    }
    
    private static Variable traceNodeDefAndPropagateExaminingCurrent(ExternalCFGNode pred,
        ExternalCFGVariable v, Hashtable nodeInfo, Hashtable varInfo, World w) {
        ExternalCFGNodeInfo predInfo = (ExternalCFGNodeInfo)nodeInfo.get(pred);
                
        if (pred instanceof ExternalCFGDefNode) {
            ExternalCFGVariable[] defs = ((ExternalCFGDefNode)pred).getDefinedVariables();
                    
            if (Globals.debug && defs == null) {
                invalid("Cannot get definitions from " + pred);
            }

            for (int i = 0; i < defs.length; i++) {
                if (Globals.debug && defs[i] == null) {
                    invalid("null variable definition in " + pred);
                }
                        
                if (defs[i].equals(v)) {
                    return predInfo.defVars[i];
                }
            }
        }
        
        return traceNodeDefAndPropagate(pred, v, nodeInfo, varInfo, w);
    }
    
    static Variable traceNodeDefAndPropagate(ExternalCFGNode from,
        ExternalCFGVariable v, Hashtable nodeInfo, Hashtable varInfo, World w) {
        ExternalCFGNodeInfo fromInfo = (ExternalCFGNodeInfo)nodeInfo.get(from);
        ExternalCFGVariableInfo vInfo = (ExternalCFGVariableInfo)varInfo.get(v);
        
        if (Globals.debug && vInfo == null) {
            invalid("Cannot find any definition for variable " + v);
        }

        if (Globals.debug && fromInfo == null) {
            invalid("Cannot find any information for node " + from);
        }
        
        ExternalCFGNode fromInfo_blockHead = fromInfo.blockHead;
        
        if (vInfo.defNodeHeads.get(fromInfo_blockHead) != null) {
            Vector predecessors = fromInfo.predecessors;
            
            while (predecessors.size() == 1) {
                ExternalCFGNode pred = (ExternalCFGNode)predecessors.elementAt(0);
                ExternalCFGNodeInfo predInfo = (ExternalCFGNodeInfo)nodeInfo.get(pred);
                
                if (pred instanceof ExternalCFGDefNode) {
                    ExternalCFGVariable[] defs = ((ExternalCFGDefNode)pred).getDefinedVariables();
                    
                    if (Globals.debug && defs == null) {
                        invalid("Cannot get definitions from " + pred);
                    }

                    for (int i = 0; i < defs.length; i++) {
                        if (Globals.debug && defs[i] == null) {
                            invalid("null variable definition in " + pred);
                        }
                        
                        if (defs[i].equals(v)) {
                            return predInfo.defVars[i];
                        }
                    }
                }
                
                predecessors = predInfo.predecessors;
            }
        }
        
        ExternalCFGNodeInfo headInfo = (ExternalCFGNodeInfo)nodeInfo.get(fromInfo_blockHead);
        
        if (Globals.debug && headInfo == null) {
            Globals.localError("Cannot find head information for node " + fromInfo_blockHead);
        }
        
        Vector headInfo_predecessors = headInfo.predecessors;
        int headInfo_predecessors_size = headInfo_predecessors.size();
        ComponentLabel component = vInfo.component;
        
        if (component == null) {
            component = new ExternalCFGVariableComponent(v);
            vInfo.component = component;
        }
        
        Hashtable varsCarried = headInfo.predecessorEdgesVarsCarried;
        
        if (Globals.debug && varsCarried == null) {
            Globals.localError("Cannot find carrying information for node " + fromInfo_blockHead + " for " + v);
        }
        
        Object carried = varsCarried.get(v);
        Variable result = headInfo.funVar.getComponent(w, Variable.CMODE, component);
        
        if (carried == null || carried == NEED_CARRIED) {
            InstanceLabel[] instances = headInfo.predecessorEdgeInstances;
        
            varsCarried.put(v, IS_CARRIED);
            
            for (int i = 0; i < headInfo_predecessors_size; i++) {
                ExternalCFGNode pred = (ExternalCFGNode)headInfo_predecessors.elementAt(i);
            
                if (ConstraintManager.useControlFlowPolymorphism) {
                    result.getInstance(w, instances[i]).makeEqual(w,
                        traceNodeDefAndPropagateExaminingCurrent(pred, v, nodeInfo, varInfo, w));
                } else {
                    result.makeEqual(w,
                        traceNodeDefAndPropagateExaminingCurrent(pred, v, nodeInfo, varInfo, w));
                }
            }
        }
        
        return result;
    }
    
    static Variable pushToRoot(ExternalCFGNode from, ComponentLabel component,
        Hashtable nodeInfo, World w) {
        ExternalCFGNodeInfo fromInfo = (ExternalCFGNodeInfo)nodeInfo.get(from);
        ExternalCFGNodeInfo headInfo = (ExternalCFGNodeInfo)nodeInfo.get(fromInfo.blockHead);
        Vector headInfo_predecessors = headInfo.predecessors;
        int headInfo_predecessors_size = headInfo_predecessors.size();
        Variable result = headInfo.funVar.getComponent(w, Variable.CMODE, component);
        
        if (headInfo_predecessors_size > 0) {
            Hashtable componentsCarried = headInfo.predecessorEdgesComponentsCarried;
            
            if (componentsCarried.get(component) == null) {
                InstanceLabel[] instances = headInfo.predecessorEdgeInstances;
            
                componentsCarried.put(component, component);
                
                for (int i = 0; i < headInfo_predecessors_size; i++) {
                    ExternalCFGNode pred = (ExternalCFGNode)headInfo_predecessors.elementAt(i);
                
                    if (ConstraintManager.useControlFlowPolymorphism) {
                        result.getInstance(w, instances[i]).makeEqual(w,
                            pushToRoot(pred, component, nodeInfo, w));
                    } else {
                        result.makeEqual(w, pushToRoot(pred, component, nodeInfo, w));
                    }
                }
            }
        }
        
        return result;
    }
    
    private void addNodeConstraints(ExternalCFGNode node, 
        Hashtable nodeInfo, Hashtable varInfo, World w) {
        if (node instanceof ExternalCFGDefNode) {
            ExternalCFGNodeInfo info = (ExternalCFGNodeInfo)nodeInfo.get(node);
            Variable[] defVars = info.defVars;
            
            if (defVars[0] == null) {
                if (node instanceof ExternalCFGChoiceDefNode) {
                    ExternalCFGVariable[] choices = ((ExternalCFGChoiceDefNode)node).getChoiceValues();
                    
                    if (Globals.debug && defVars.length != 1) invalid("Invalid number of defined variables");
                    
                    if (choices.length == 0) {
                        defVars[0] = new Variable(w);
                    } else {
                        Variable v = traceNodeDef(node, choices[0], nodeInfo, varInfo, w);
                        
                        defVars[0] = v;
                        for (int i = 1; i < choices.length; i++) {
                            v.makeEqual(w, traceNodeDef(node, choices[i], nodeInfo, varInfo, w));
                        }
                    }
                } else if (node instanceof ExternalCFGCatchDefNode) {
                    ExternalCFGCatchDefNode catchNode = (ExternalCFGCatchDefNode)node;

                    if (Globals.debug && defVars.length != 1) invalid("Invalid number of defined variables");
                    
                    ExternalCFGDefNode[] defNodes = catchNode.getSourceNodes();
                    JBCClass c = catchNode.getCatchClass();
                    Variable resultVar = null;
                    
                    if (manager.makeExceptionComponents) {
                        for (int i = 0; i < defNodes.length; i++) {
                            ExternalCFGDefNode defNode = defNodes[i];
                        
                            if (Globals.debug
                                && !(defNode instanceof ExternalCFGMethodInvocationDefNode)
                                && !(defNode instanceof ExternalCFGFlowgraphInvocationDefNode)) {
                                invalid("Catching exception from non-exception-throwing node");
                            }
                            
                            ExternalCFGNodeInfo defNodeInfo = (ExternalCFGNodeInfo)nodeInfo.get(defNode);
                        
                            if (Globals.debug && defNodeInfo == null) {
                                invalid("Source node not reachable from root");
                            }
                            
                            addNodeConstraints(defNode, nodeInfo, varInfo, w);
                        
                            Variable[] defNodeInfo_defVars = defNodeInfo.defVars;

                            if (defNodeInfo_defVars[1] == null) {
                                defNodeInfo_defVars[1] = defNodeInfo_defVars[2].getComponent(w,
                                    Variable.DMODE,
                                    JBCMethodExceptionComponent.get());
                                manager.setJBCType(defNodeInfo_defVars[1], JBCType.OBJECT);
                            }
                        
                            Variable v = traceNodeDef(node, defNode.getDefinedVariables()[1], nodeInfo, varInfo, w);
                        
                            if (c != null) {
                                v = manager.makeDowncast(v, c);
                            }
                            
                            if (resultVar == null) {
                                resultVar = v;
                            } else {
                                resultVar.makeEqual(w, v);
                            }
                            manager.setJBCType(resultVar, JBCType.OBJECT);
                        }
                    } else {
                        resultVar = manager.getExceptionVar();
                    }
                        
                    defVars[0] = resultVar;
                } else if (node instanceof ExternalCFGFieldAssignmentDefNode) {
                    ExternalCFGFieldAssignmentDefNode assignDefNode =
                        (ExternalCFGFieldAssignmentDefNode)node;
                    JBCField field = assignDefNode.getField();
                    Variable slot;
                    
                    if (Globals.debug && defVars.length != 1) invalid("Invalid number of defined variables");

                    if (field.isStatic()) {
                        Variable globalsVar;
                        
                        if (manager.makeGlobalComponents) {
                            JBCMethodGlobalsArgComponent globals = JBCMethodGlobalsArgComponent.get();
                            
                            globalsVar = pushToRoot(node, globals, nodeInfo, w);
                            globalsVar.setGlobal(w);
                        } else {
                            globalsVar = manager.getGlobalsVar();
                        }
                                
                        slot = manager.makeStaticField(Variable.CMODE, globalsVar, field, Variable.DMODE);
                                
                        addClassInitializationIfNeeded(node, nodeInfo, field.getContainingClass());
                    } else {
                        Variable objVar = traceNodeDef(node, assignDefNode.getObject(), nodeInfo, varInfo, w);

                        slot = manager.makeNonstaticField(Variable.CMODE, objVar, field, Variable.DMODE);
                    }
                        
                    traceNodeDef(node, assignDefNode.getValue(), nodeInfo, varInfo, w)
                        .makeEqual(w, slot);
                        
                    defVars[0] = slot;
                } else if (node instanceof ExternalCFGFieldDefNode) {
                    ExternalCFGFieldDefNode fieldDefNode = (ExternalCFGFieldDefNode)node;
                    JBCField field = fieldDefNode.getField();
                    
                    if (Globals.debug && defVars.length != 1) invalid("Invalid number of defined variables");

                    if (field.isStatic()) {
                        Variable globalsVar;
                        
                        if (manager.makeGlobalComponents) {
                            ComponentLabel globals = JBCMethodGlobalsArgComponent.get();
                            
                            globalsVar = pushToRoot(node, globals, nodeInfo, w);
                            globalsVar.setGlobal(w);
                        } else {
                            globalsVar = manager.getGlobalsVar();
                        }
                            
                        addClassInitializationIfNeeded(node, nodeInfo, field.getContainingClass());
                            
                        defVars[0] = manager.makeStaticField(Variable.DMODE, globalsVar, field, Variable.DMODE);
                    } else {
                        Variable objVar = traceNodeDef(node, fieldDefNode.getObject(), nodeInfo, varInfo, w);
                            
                        defVars[0] = manager.makeNonstaticField(Variable.DMODE, objVar, field, Variable.DMODE);
                    }
                    
                    manager.setJBCType(defVars[0], field.getFieldType());
                } else if (node instanceof ExternalCFGUserFieldAssignmentDefNode) {
                    ExternalCFGUserFieldAssignmentDefNode assignDefNode =
                        (ExternalCFGUserFieldAssignmentDefNode)node;
                    UserField field = assignDefNode.getField();
                    Variable slot;
                    
                    if (Globals.debug && defVars.length != 1) invalid("Invalid number of defined variables");

                    if (field.isStatic()) {
                        Variable globalsVar;
                        
                        if (manager.makeGlobalComponents) {
                            JBCMethodGlobalsArgComponent globals = JBCMethodGlobalsArgComponent.get();
                            
                            globalsVar = pushToRoot(node, globals, nodeInfo, w);
                            globalsVar.setGlobal(w);
                        } else {
                            globalsVar = manager.getGlobalsVar();
                        }
                        
                        slot = manager.makeStaticUserField(Variable.CMODE, globalsVar, field, Variable.DMODE);
                    } else {
                        Variable objVar = traceNodeDef(node, assignDefNode.getObject(), nodeInfo, varInfo, w);

                        slot = manager.makeNonstaticUserField(Variable.CMODE, objVar, field, Variable.DMODE);
                    }
                    
                    traceNodeDef(node, assignDefNode.getValue(), nodeInfo, varInfo, w)
                        .makeEqual(w, slot);
                    
                    defVars[0] = slot;
                } else if (node instanceof ExternalCFGUserFieldDefNode) {
                    ExternalCFGUserFieldDefNode fieldDefNode = (ExternalCFGUserFieldDefNode)node;
                    UserField field = fieldDefNode.getField();
                    
                    if (Globals.debug && defVars.length != 1) invalid("Invalid number of defined variables");

                    if (field.isStatic()) {
                        Variable globalsVar;
                        
                        if (manager.makeGlobalComponents) {
                            ComponentLabel globals = JBCMethodGlobalsArgComponent.get();
                            
                            globalsVar = pushToRoot(node, globals, nodeInfo, w);
                            globalsVar.setGlobal(w);
                        } else {
                            globalsVar = manager.getGlobalsVar();
                        }
                        
                        defVars[0] = manager.makeStaticUserField(Variable.DMODE, globalsVar, field, Variable.DMODE);
                    } else {
                        Variable objVar = traceNodeDef(node, fieldDefNode.getObject(), nodeInfo, varInfo, w);
                        
                        // manager.setJBCType(objVar, JBCType.OBJECT);
                        defVars[0] = manager.makeNonstaticUserField(Variable.DMODE, objVar, field, Variable.DMODE);
                    }
                } else if (node instanceof ExternalCFGFlowgraphInvocationDefNode) {
                    ExternalCFGFlowgraphInvocationDefNode fgDefNode = (ExternalCFGFlowgraphInvocationDefNode)node;
                    Variable v = manager.getFlowgraphVar(
                            ExternalFlowgraphInvocationContext.get(context, fg, node),
                            fgDefNode.getFunctionName())
                        .getInstance(w, new ExternalCFGFlowgraphInvocationInstance(fgDefNode));
                    ExternalCFGVariable[] params = fgDefNode.getParameters();
                    
                    if (Globals.debug && defVars.length != 3) invalid("Invalid number of defined variables");

                    for (int i = 0; i < params.length; i++) {
                        v.getComponent(w, Variable.DMODE, JBCMethodArgComponent.get(i))
                            .makeEqual(w, traceNodeDef(node, params[i], nodeInfo, varInfo, w));
                    }
                    
                    if (manager.makeGlobalComponents) {
                        JBCMethodGlobalsArgComponent globals = JBCMethodGlobalsArgComponent.get();
                        Variable globalsVar = pushToRoot(node, globals, nodeInfo, w);
                        
                        v.getComponent(w, Variable.DMODE, globals).makeEqual(w, globalsVar);
                        manager.setJBCType(globalsVar, ConstraintManager.GLOBALS);
                        globalsVar.setGlobal(w);
                    }
                    
                    defVars[0] = v.getComponent(w, Variable.DMODE, JBCMethodResultComponent.get());
                    defVars[2] = v;
                } else if (node instanceof ExternalCFGMethodInvocationDefNode) {
                    ExternalCFGMethodInvocationDefNode methDefNode = (ExternalCFGMethodInvocationDefNode)node;
                    JBCMethod m = methDefNode.getMethod();
                    ExternalCFGVariable[] params = methDefNode.getParameters();
                    Variable v;
                    
                    if (Globals.debug && defVars.length != 3) invalid("Invalid number of defined variables");
                    
                    if (m == null) invalid("Method call could not be resolved");

                    InvocationContext callContext = ExternalFlowgraphInvocationContext.get(context, fg, node);

                    if (manager.useStaticDispatch(callContext, m)) {
                        SEMISingletonUsageDetector singletonDetector = manager.getSingletonUsageDetector();
                        int callStatus = singletonDetector.getMethodCallStatus(m, makeLocalLocation(node));
                        
                        v = manager.getMethodVar(callContext, m);
                        
                        if (callStatus != singletonDetector.CALL_SINGLE_CALLER) {
                            v = v.getInstance(w, new ExternalCFGMethodInvocationInstance(methDefNode));
                        }
                    } else {
                        try {
                            Variable objVar = traceNodeDef(node, params[0], nodeInfo, varInfo, w);
                            
                            v = new Variable(w);
                            
                            if (method != null) {
                                manager.getCallResolver().resolveCall(callContext, method, methDefNode,
                                    m, objVar, v);
                            } else {
                                manager.getCallResolver().resolveCall(callContext, name, methDefNode,
                                    m, objVar, v);
                            }
                        } catch (ArrayIndexOutOfBoundsException ex) {
                            if (params.length < 1) {
                                invalid("Must supply object parameter for nonstatic method invocation; method call ignored");
                                v = new Variable(w);
                            } else {
                                throw ex;
                            }
                        }
                    }
                    
                    JBCMethodType methodType = m.getMethodType();
                    
                    // manager.setJBCType(v, methodType);
                    
                    JBCType[] paramTypes = methodType.getParameterTypes();
                    int paramWords = 0;
                    
                    if (params.length != paramTypes.length) {
                        invalid("Invalid parameter length");
                    }
                    
                    for (int i = 0; i < params.length; i++) {
                        Variable paramVar = traceNodeDef(node, params[i], nodeInfo, varInfo, w);
                        JBCType paramType = paramTypes[i];
                        
                        v.getComponent(w, Variable.DMODE, JBCMethodArgComponent.get(paramWords))
                            .makeEqual(w, paramVar);
                        manager.setJBCType(paramVar, paramType);
                        paramWords += paramType.getWordSize();
                    }
                    
                    if (manager.makeGlobalComponents) {
                        JBCMethodGlobalsArgComponent globals = JBCMethodGlobalsArgComponent.get();
                        Variable globalsVar = pushToRoot(node, globals, nodeInfo, w);
                    
                        v.getComponent(w, Variable.DMODE, globals).makeEqual(w, globalsVar);
                        manager.setJBCType(globalsVar, ConstraintManager.GLOBALS);
                        globalsVar.setGlobal(w);
                    }
                        
                    defVars[0] = v.getComponent(w, Variable.DMODE, JBCMethodResultComponent.get());
                    defVars[2] = v;

                    JBCType returnType = methodType.getReturnType();
                    
                    if (!returnType.equals(JBCType.VOID)) {
                        manager.setJBCType(defVars[0], returnType);
                    }
                } else if (node instanceof ExternalCFGNewObjectDefNode) {
                    ExternalCFGNewObjectDefNode objDefNode = (ExternalCFGNewObjectDefNode)node;
                    JBCClass c = objDefNode.getObjectClass();
                    Variable v = makeNewObject(objDefNode, nodeInfo, c);
                    
                    if (Globals.debug && defVars.length != 1) invalid("Invalid number of defined variables");

                    manager.setJBCType(v, JBCType.OBJECT);

                    defVars[0] = v;
                } else if (node instanceof ExternalCFGParameterDefNode) {
                    ExternalCFGParameterDefNode paramDefNode = (ExternalCFGParameterDefNode)node;
                    
                    if (Globals.debug && defVars.length != 1) invalid("Invalid number of defined variables");

                    defVars[0] = pushToRoot(node,
                        JBCMethodArgComponent.get(paramDefNode.getParameterIndex()),
                        nodeInfo, w);
                } else {
                    invalid("Unknown node type: " + node);
                }
                
                for (int i = 0; i < defVars.length; i++) {
                    if (defVars[i] != null) {
                        combineWith(w, defVars[i]);
                        
                        if (makeInitialized) {
                            manager.makeInitializedValue(defVars[i]);
                        }
                    }
                }
            }
        }
    }
    
    private void addClassInitializationIfNeeded(ExternalCFGNode from,
        Hashtable nodeInfo, JBCClass c) {
        if (method != null && method.getContainingClass().isSubclassOf(c)) {
            return;
        }
        
        addClassInitialization(from, nodeInfo, c);
    }
    
    private void addClassInitialization(ExternalCFGNode from,
        Hashtable nodeInfo, JBCClass c) {
        boolean doSuperClass;
        World w = manager.getSolver();
            
        if (manager.hoistClassInitializers) {
            if (manager.addTopLevelClassInitialization(c)) {
                Variable v = manager.getClassInitializerFunction();
                Variable globalsVar;
                Variable exceptionVar;
                
                if (manager.makeGlobalComponents) {
                    globalsVar = v.getComponent(w, JBCMethodGlobalsArgComponent.get());
                } else {
                    globalsVar = manager.getGlobalsVar();
                }
                
                if (manager.makeExceptionComponents) {
                    exceptionVar = v.getComponent(w, JBCMethodExceptionComponent.get());
                } else {
                    exceptionVar = manager.getExceptionVar();
                }
                
                FlowgraphExceptionWrapperInstance wrapperInstance =
                    name != null ? FlowgraphExceptionWrapperInstance.get(name, from)
                        : FlowgraphExceptionWrapperInstance.get(method, from);
                
                doSuperClass = JBCConstraintGenerator.doClassInitialization(w, manager, globalsVar,
                    exceptionVar, manager.getRootInvocationContext(),
                    null, wrapperInstance, c);
            } else {
                doSuperClass = false;
            }
        } else {
            Variable globalsVar;
            Variable exceptionVar;
            
            if (manager.makeGlobalComponents) {
                globalsVar = pushToRoot(from, JBCMethodGlobalsArgComponent.get(), nodeInfo, w);
            } else {
                globalsVar = manager.getGlobalsVar();
            }
            
            if (manager.makeExceptionComponents) {
                exceptionVar = pushToRoot(from, JBCMethodExceptionComponent.get(), nodeInfo, w);
            } else {
                exceptionVar = manager.getExceptionVar();
            }
            
            FlowgraphExceptionWrapperInstance wrapperInstance =
                name != null ? FlowgraphExceptionWrapperInstance.get(name, from)
                    : FlowgraphExceptionWrapperInstance.get(method, from);
                
            doSuperClass = JBCConstraintGenerator.doClassInitialization(w, manager,
                globalsVar, exceptionVar,
                ExternalFlowgraphInvocationContext.get(context, fg, from),
                new ExternalCFGMethodInvocationInstance(from),
                wrapperInstance, c);
        }

        if (doSuperClass) {
            JBCClass superClass = c.getSuperClass();
            
            if (superClass != null) {
                addClassInitializationIfNeeded(from, nodeInfo, c);
            }
        }
    }
    
    void makeLive() {
        if (fg == null) {
            Globals.userError("Native code not found for "
                + getName() + "; assuming it does NOTHING");
            return;
        }
            
        try {
            World w = manager.getSolver();
            Hashtable nodeInfo = new Hashtable();
            ExternalCFGNode startNode = fg.getCFGRoot();
            Hashtable varInfo = new Hashtable();
            
            computePredecessorInfo(startNode, nodeInfo, varInfo);
            
            ExternalCFGNodeInfo startNodeInfo = (ExternalCFGNodeInfo)nodeInfo.get(startNode);
            
            startNode = new DummyCFGStartNode(startNode);
            startNodeInfo.predecessors.addElement(startNode);
            startNodeInfo = new ExternalCFGNodeInfo();
            nodeInfo.put(startNode, startNodeInfo);
            
            if (name != null) {
                startNodeInfo.funVar = manager.getFlowgraphVar(context, name);
            } else {
                startNodeInfo.funVar = manager.getMethodVar(context, method);
            }
            
            makePredecessorEdgeInstances(nodeInfo, varInfo, w);
            
            for (Enumeration e = nodeInfo.keys(); e.hasMoreElements();) {
                addNodeConstraints((ExternalCFGNode)e.nextElement(), nodeInfo, varInfo, w);
            }
            
            /* Set the result node's result component to its def value */
            ExternalCFGNode resultNode = fg.getResultDef();
            
            if (resultNode != null) {
                ExternalCFGNodeInfo resultInfo = (ExternalCFGNodeInfo)nodeInfo.get(resultNode);
                
                try {
                    resultInfo.defVars[0].makeEqual(w,
                        pushToRoot(resultNode, JBCMethodResultComponent.get(), nodeInfo, w));
                } catch (NullPointerException ex) {
                    if (resultInfo == null) {
                        invalid("Result node not reachable from root");
                    } else if (resultInfo.defVars == null
                        || resultInfo.defVars.length < 1
                        || resultInfo.defVars[0] == null) {
                        invalid("Result node does not compute a value");
                    }
                    
                    throw ex;
                }   
            }
            
            ExternalCFGNode exceptionNode = fg.getExceptionDef();
            
            if (exceptionNode != null) {
                ExternalCFGNodeInfo exceptionInfo = (ExternalCFGNodeInfo)nodeInfo.get(exceptionNode);
                Variable exceptionVar;
                
                if (manager.makeExceptionComponents) {
                    exceptionVar = pushToRoot(exceptionNode, JBCMethodExceptionComponent.get(), nodeInfo, w);
                } else {
                    exceptionVar = manager.getExceptionVar();
                }

                try {
                    exceptionInfo.defVars[0].makeEqual(w, exceptionVar);
                } catch (NullPointerException ex) {
                    if (exceptionInfo == null) {
                        invalid("Exception node not reachable from root");
                    } else if (exceptionInfo.defVars == null
                        || exceptionInfo.defVars.length < 1
                        || exceptionInfo.defVars[0] == null) {
                        invalid("Exception node does not compute a value");
                    }
                    
                    throw ex;
                }
                
                manager.setJBCType(exceptionInfo.defVars[0], JBCType.OBJECT);
                if (manager.makeExceptionsGlobal) {
                    exceptionInfo.defVars[0].setGlobal(w);
                }
            }
            
            for (Enumeration e = nodeInfo.keys(); e.hasMoreElements();) {
                ExternalCFGNode node = (ExternalCFGNode)e.nextElement();
                ExternalCFGNodeInfo info = (ExternalCFGNodeInfo)nodeInfo.get(node);
                Hashtable varsCarried = info.predecessorEdgesVarsCarried;
                
                if (varsCarried != null) {
                    for (Enumeration edges = varsCarried.keys(); edges.hasMoreElements();) {
                        ExternalCFGVariable v = (ExternalCFGVariable)edges.nextElement();
                        
                        if (varsCarried.get(v) != IS_CARRIED) {
                            traceNodeDefAndPropagate(node, v, nodeInfo, varInfo, w);
                        }
                    }
                }
            }
            
            if (method != null) {
                addClassInitialization(startNode, nodeInfo, method.getContainingClass());
            }
            
            if (method != null) {
                manager.setFlowgraphExpressionEvaluator(method, new FlowgraphExpressionEvaluator(method, nodeInfo, varInfo));
            } else {
                manager.setFlowgraphExpressionEvaluator(name, new FlowgraphExpressionEvaluator(name, nodeInfo, varInfo));
            }
        } catch (InvalidFlowgraphError ex) {
            Globals.userError("Invalid native code flowgraph for "
                + getName() + " (" + ex.getMessage() + ")");
        } catch (InconsistentProgramError ex) {
            Globals.userError("Program is inconsistent: " + ex.getMessage());
            /* XXX set things up for retrying */
        }
    }
}
