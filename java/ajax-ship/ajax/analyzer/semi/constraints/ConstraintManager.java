/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.jbc.*;
import ajax.jbc.util.*;
import ajax.solver.*;
import java.util.*;
import ajax.analyzer.*;
import ajax.analyzer.semi.*;
import ajax.analyzer.util.*;
import ajax.util.*;
import ajax.Globals;
import ajax.analyzer.semantics.*;

public class ConstraintManager implements DataConstants {
    public static final boolean useControlFlowPolymorphism = false; // CONFIG
    public static final boolean makeExceptionsGlobal = true; // CONFIG
/** When this is turned on, all class initialization is moved to the very
    beginning of the program and any exceptions thrown are ignored.
    Strictly speaking, this is not the correct semantics, since inner code
    may be able to catch exceptions thrown by the initializers, and this
    will not be reflected in the analysis. But it saves adding a lot of
    constraints... although maybe not now that we support "globalized"
    global variables and exceptions! */
    public static final boolean hoistClassInitializers = true; // CONFIG
    public static final boolean makeSingleUseValuesNonpolymorphic = true; // CONFIG
    
    public static final boolean makeGlobalComponents = !VarData.applyGlobal;
    public static final boolean makeExceptionComponents = !VarData.applyGlobal || !makeExceptionsGlobal;

    public static final JBCType LONG_LO32 = new JBCBaseType("<long_lo32>");
    public static final JBCType LONG_HI32 = new JBCBaseType("<long_hi32>");
    public static final JBCType DOUBLE_LO32 = new JBCBaseType("<double_lo32>");
    public static final JBCType DOUBLE_HI32 = new JBCBaseType("<double_hi32>");
    public static final JBCType GLOBALS = new JBCBaseType("<globals>");
    public static final JBCType SUBOBJECT = new JBCBaseType("<subobject>");
    public static final JBCType SUBCHUNK = new JBCBaseType("<subchunk>");
    
    private Variable[] specialVars = null;
    
    private static final String[] instructionExceptions = {
        "java.lang.VirtualMachineError",
        "java.lang.LinkageError",
        "java.lang.NullPointerException",
        "java.lang.ArrayIndexOutOfBoundsException",
        "java.lang.ArrayStoreException",
        "java.lang.ArithmeticException",
        "java.lang.NegativeArraySizeException",
        "java.lang.ClassCastException",
        "java.lang.IllegalMonitorStateException",
        "java.lang.ThreadDeath"
    };
    
    private JBCClass[] instructionExceptionClasses = null;
    
    static final int VAR_NULL          = 0;
    static final int VAR_INS_EXN       = 1;
    static final int VAR_WRAP_INIT_EXN = 2;
    
    private static final int QUERY_SOURCE = 0;
    private static final int QUERY_TARGET = 1;
    private static final int QUERY_INDETERMINATE = 2;
    
    private AnalyzerEnvironment env;
    
    private JBCClass javaLangObject = null;
    private UserField stringConstField = null;
    
/** This table maps a JBCMethod to the Variable representing the function
    corresponding to the method. */
    private Hashtable methodToVariable = new Hashtable();
    
/** This table maps a JBCMethod to a BytecodeExpressionEvaluator. */
    private Hashtable methodToBytecodeEvaluators = new Hashtable();
    
    private BytecodeExpressionEvaluator uncompressedEvaluator = null;
    
/** This table maps JBCMethods and Strings to FlowgraphExpressionEvaluators. */
    private Hashtable keyToFlowgraphEvaluators = new Hashtable();
    
    private FlowgraphExpressionEvaluator uncompressedFlowgraphEvaluator = null;
    
/** This table maps a JBCClass to the Variable representing proto-object
    of the class. */
    private Hashtable classToVariable = new Hashtable();
    
/** This table maps a native code name String to the
    Variable representing the function corresponding to it. */
    private Hashtable nativeNameToVariable = new Hashtable();
    
/** This table maps a JBCMethod representing the first declaration of a method
    to a Hashtable of maps from an InvocationContext where that declaration is
    referenced to an InvocationContextComponent allowing access to the copy of the method used
    at that site. */
    private Hashtable declaringMethodToSiteComponents = new Hashtable();

/** This table maps a JBCClass to a Vector containing its subclasses. 
    The classes and subclasses that appear in this data structure are only the
    classes that have Variables associated with them in classToVariable (and
    the superclasses of such classes). This includes interfaces and subinterface
    and interface implementation relationships. */
    private Hashtable classToSubclasses = new Hashtable();

/** Create a unique primordial invocation context. This is used to bootstrap
    the analyzer, and it's also used in places where we need a context but
    we don't care what it is. */
    private InvocationContext rootContext = new InvocationContext(this);
    
/** This table keeps track of which classes have already been initialized,
    if hoistClassInitializers is true. */
    private CompactSet initializedClasses = new CompactSet();

/** This variable represents the function that initializes all classes,
    if hoistClassInitializers is true. */
    private Variable classInitializerFunction = null;
    
/** This variable represents the global "globals" object, if
    makeGlobalComponents is true. */
    private Variable globalsObject = null;
    
/** This variable represents the global "exception" object, if
    makeExceptionComponents is true. */
    private Variable exceptionObject = null;
    
    private SEMIVirtualCallResolver callResolver;
    private SEMISingletonUsageDetector singletonUsageDetector;
    private boolean treatVirtualCallsAsNondeterminism = false;
    private boolean breakoutDowncastSubobjects = true;        
    private boolean useSubchunks = true;                      
    private boolean useSubobjects = true;                     
    private boolean trackArrayIndices = false;                
    
    private World solver;
    
    public ConstraintManager(AnalyzerEnvironment env) {
        this.env = env;
        solver = env.getSolver();
        
        World w = getSolver();
        
        globalsObject = new Variable(w);
        exceptionObject = new Variable(w);
        globalsObject.setGlobal(w);
        exceptionObject.setGlobal(w);
    }
    
    public void configureAnalyses() {
        Analyzer analyzer = env.getPrephase();
        
        callResolver = new SEMIVirtualCallResolver(this, analyzer != null);
        singletonUsageDetector = new SEMISingletonUsageDetector(this,
            makeSingleUseValuesNonpolymorphic && analyzer != null);
        
        if (analyzer != null) {
            callResolver.configure(analyzer);
            singletonUsageDetector.configure(analyzer);
        }
    }
    
    public void setBreakoutDowncastSubobjects(boolean b) {
        breakoutDowncastSubobjects = b;
    }
    
    public void setUseSubchunks(boolean b) {
        useSubchunks = b;
    }
    
    public void setUseSubobjects(boolean b) {
        useSubobjects = b;
    }

    public void setTrackArrayIndices(boolean b) {
	trackArrayIndices = b;
    }
    
    public void setNondeterministicVirtuals(boolean b) {
        treatVirtualCallsAsNondeterminism = b;
    }
    
    boolean isUsingSubchunks() {
        return useSubchunks;
    }
    
    boolean isUsingSubobjects() {
        return useSubobjects;
    }
    
    boolean isVirtualCallingNondeterministic() {
        return treatVirtualCallsAsNondeterminism;
    }

    boolean isTrackingArrayIndices() {
	return trackArrayIndices;
    }
    
    public Semantics getSemantics() {
        return env.getSemantics();
    }
    
    public InvocationContext getRootInvocationContext() {
        return rootContext;
    }
    
    private Variable makeInstructionExceptionVar() {
        env.makeFlowgraphLive("_magicexn");
        return getFlowgraphVar(rootContext, "_magicexn");
    }
    
    private Variable makeNullVar() {
        return new Variable(getSolver());
    }
    
    private Variable makeWrapInitExnVar() {
        env.makeFlowgraphLive("_wrapclassinitializerexn");
        return getFlowgraphVar(rootContext, "_wrapclassinitializerexn");
    }
    
    private void initSpecialVars() {
        Variable[] specials = { makeNullVar(),
            makeInstructionExceptionVar(), makeWrapInitExnVar() };
            
        specialVars = specials;
    }
    
    Variable makeStringConst(Variable globals) {
        if (stringConstField == null) {
            stringConstField = env.getWorld().getSpecialClass("java.lang.String")
                .registerUserField("internstr", true);
        }
        
        return makeStaticUserField(Variable.DMODE, globals, stringConstField, Variable.DMODE);
    }
    
    Variable getSpecialVar(int index) {
        if (specialVars == null) {
            initSpecialVars();
        }
        
        return specialVars[index];
    }
    
    boolean addTopLevelClassInitialization(JBCClass c) {
        if (Globals.debug && !hoistClassInitializers) {
            Globals.localError("Requested toplevel class initialization but class initializers are not supposed to be hoisted!");
        }
        
        if (initializedClasses.get(c) == null) {
            initializedClasses.addUnconditionally(c);
            return true;
        } else {
            return false;
        }
    }
    
    Variable getClassInitializerFunction() {
        if (Globals.debug && !hoistClassInitializers) {
            Globals.localError("Requested class initializer function but class initializers are not supposed to be hoisted!");
        }
        
        if (classInitializerFunction == null) {
            classInitializerFunction = new Variable(getSolver());
        }
        
        return classInitializerFunction;
    }
    
    boolean catchesInstructionExceptions(JBCClass c) {
        if (instructionExceptionClasses == null) {
            instructionExceptionClasses = new JBCClass[instructionExceptions.length];

            for (int i = 0; i < instructionExceptions.length; i++) {
                instructionExceptionClasses[i] = getSpecialClass(instructionExceptions[i]);
            }
        }

        for (int i = 0; i < instructionExceptionClasses.length; i++) {
            if (c.hasCommonSubclassWith(instructionExceptionClasses[i])) {
                return true;
            }
        }

        return false;
    }
    
    private Variable applyCompoundExpression(JBCExpression expression, Variable v) {
        if (v == null) {
            return null;
        }
        
        if (expression instanceof JBCFieldExpression) {
            JBCFieldExpression fieldExpr = (JBCFieldExpression)expression;
            JBCField f = fieldExpr.getField();
            
            makeNonstaticField(Variable.CMODE, v, f, Variable.DMODE);
            return makeNonstaticField(Variable.DMODE, v, f, Variable.DMODE);
        } else {
            JBCUserFieldExpression fieldExpr = (JBCUserFieldExpression)expression;
            UserField f = fieldExpr.getField();
            
            makeNonstaticUserField(Variable.CMODE, v, f, Variable.DMODE);
            return makeNonstaticUserField(Variable.DMODE, v, f, Variable.DMODE);
        }
    }
    
    private Variable internalGetVar(JBCMethod m, int offset, JBCExpression expression) {
        if (expression instanceof JBCCompoundExpression) {
            Variable v = internalGetVar(m, offset, ((JBCCompoundExpression)expression).getBase());
            
            return applyCompoundExpression(expression, v);
        } else {
            return getEvaluator(m).getVar(this, offset, expression);
        }
    }
    
    private Variable internalGetVar(JBCMethod m, ExternalCFGNode node, JBCExpression expression) {
        if (expression instanceof JBCCompoundExpression) {
            Variable v = internalGetVar(m, node, ((JBCCompoundExpression)expression).getBase());
            
            return applyCompoundExpression(expression, v);
        } else {
            return getFlowgraphEvaluator(m).getVar(this, node, expression);
        }
    }
    
    private Variable internalGetVar(String name, ExternalCFGNode node, JBCExpression expression) {
        if (expression instanceof JBCCompoundExpression) {
            Variable v = internalGetVar(name, node, ((JBCCompoundExpression)expression).getBase());
            
            return applyCompoundExpression(expression, v);
        } else {
            return getFlowgraphEvaluator(name).getVar(this, node, expression);
        }
    }
    
    private Variable[] makeHook(Location l, Variable v, boolean isTarget,
        boolean isAlwaysObject) {
        if (v == null) {
            return null;
        } else {
            Variable[] vs = SlotManager.makeQueryHook(this, v, isTarget,
                isAlwaysObject);
            Semantics s = getSemantics();
            
            if (s instanceof TransitiveSemantics) {
                Location transitiveLocation = ((TransitiveSemantics)s).getLocation();
                
                if (transitiveLocation.equals(l)) {
  		    for (int i = 0; i < vs.length; i++) { 
                        vs[i].setGlobal(getSolver());
		    }
                }
            }
            
            return vs;
        }
    }

    private boolean checkIsAlwaysObject(JBCExpression expr) {
        if (expr instanceof JBCFieldExpression) {
            return ((JBCFieldExpression)expr).getField().getFieldType()
                instanceof JBCObjectType;
        } else {
            return false;
	}
    }
    
    public Variable[] getVar(JBCMethod m, int offset, JBCExpression expression, boolean isTarget) {
        return makeHook(new JBCLocation(m, offset),
            internalGetVar(m, offset, expression), isTarget,
            checkIsAlwaysObject(expression));
    }
    
    public Variable[] getVar(String name, ExternalCFGNode node, JBCExpression expression, boolean isTarget) {
        return makeHook(new ExternalLocation(name, node),
            internalGetVar(name, node, expression), isTarget,
            checkIsAlwaysObject(expression));
    }
    
    public Variable[] getVar(JBCMethod method, ExternalCFGNode node, JBCExpression expression, boolean isTarget) {
        return makeHook(new ExternalLocation(method, node),
            internalGetVar(method, node, expression), isTarget,
            checkIsAlwaysObject(expression));
    }
    
    private void clearUncompressedBytecodeExpressionEvaluator() {
        if (uncompressedEvaluator != null) {
            if (env.isNewQueryFamilyDisabled()) {
                methodToBytecodeEvaluators.remove(uncompressedEvaluator.getMethod());
            } else {
                uncompressedEvaluator.compress();
            }
        }
    }
    
    private BytecodeExpressionEvaluator getEvaluator(JBCMethod m) {
        BytecodeExpressionEvaluator evaluator =
            (BytecodeExpressionEvaluator)methodToBytecodeEvaluators.get(m);
            
        if (evaluator == null) {
            return null;
        } else if (evaluator.equals(uncompressedEvaluator)) {
            return evaluator;
        } else {
            clearUncompressedBytecodeExpressionEvaluator();
            uncompressedEvaluator = evaluator;
            evaluator.uncompress();
            return evaluator;
        }
    }
    
    public JBCClass getJavaLangObject() {
        if (javaLangObject == null) {
            javaLangObject = env.getWorld().getSpecialClass("java.lang.Object");
        }
        return javaLangObject;
    }
    
    Variable getGlobalsVar() {
        if (makeGlobalComponents) {
            Globals.localError("Cannot access globals object when passing globals as parameters");
        }
        return globalsObject;
    }
    
    Variable getExceptionVar() {
        if (makeExceptionComponents) {
            Globals.localError("Cannot access exception object when passing exception as result");
        }
        return exceptionObject;
    }
    
    private FlowgraphExpressionEvaluator getFlowgraphEvaluator(String name) {
        return internalGetFlowgraphEvaluator(name);
    }
    
    private FlowgraphExpressionEvaluator getFlowgraphEvaluator(JBCMethod method) {
        return internalGetFlowgraphEvaluator(method);
    }
    
    private void clearUncompressedFlowgraphExpressionEvaluator() {
        if (uncompressedFlowgraphEvaluator != null) {
            if (env.isNewQueryFamilyDisabled()) {
                keyToFlowgraphEvaluators.remove(uncompressedFlowgraphEvaluator.getKey());
            } else {
                uncompressedFlowgraphEvaluator.compress();
            }
        }
    }
    
    private FlowgraphExpressionEvaluator internalGetFlowgraphEvaluator(Object key) {
        FlowgraphExpressionEvaluator evaluator =
            (FlowgraphExpressionEvaluator)keyToFlowgraphEvaluators.get(key);
            
        if (evaluator == null) {
            return null;
        } else if (evaluator.equals(uncompressedFlowgraphEvaluator)) {
            return evaluator;
        } else {
            clearUncompressedFlowgraphExpressionEvaluator();
            uncompressedFlowgraphEvaluator = evaluator;
            evaluator.uncompress();
            return evaluator;
        }
    }
    
    void setFlowgraphExpressionEvaluator(String name, FlowgraphExpressionEvaluator e) {
        internalSetFlowgraphExpressionEvaluator(name, e);
    }
    
    void setFlowgraphExpressionEvaluator(JBCMethod method, FlowgraphExpressionEvaluator e) {
        internalSetFlowgraphExpressionEvaluator(method, e);
    }
    
    private void internalSetFlowgraphExpressionEvaluator(Object key, FlowgraphExpressionEvaluator e) {
        clearUncompressedFlowgraphExpressionEvaluator();
        uncompressedFlowgraphEvaluator = e;
            
        keyToFlowgraphEvaluators.put(key, e);
    }

    void setBytecodeExpressionEvaluator(JBCMethod m, BytecodeExpressionEvaluator e) {
        clearUncompressedBytecodeExpressionEvaluator();
        uncompressedEvaluator = e;
            
        methodToBytecodeEvaluators.put(m, e);
    }

    Variable getStaticFieldVar(Variable globalsVar, JBCField f) {
        if (globalsVar == null) {
            return null;
        } else {
            /* access the field in both modes, since we don't know which mode the query intends */
            makeStaticField(Variable.CMODE, globalsVar, f, Variable.DMODE);
            return makeStaticField(Variable.DMODE, globalsVar, f, Variable.DMODE);
        }
    }
    
    Variable getStaticUserFieldVar(Variable globalsVar, UserField f) {
        if (globalsVar == null) {
            return null;
        } else {
            /* access the field in both modes, since we don't know which mode the query intends */
            makeStaticUserField(Variable.CMODE, globalsVar, f, Variable.DMODE);
            return makeStaticUserField(Variable.DMODE, globalsVar, f, Variable.DMODE);
        }
    }
    
/**
This method retrieves a Variable corresponding to the native code
function 'name'. The analysis is done in the caller-context
'context'. Typically the caller of this method will define a new
polymorphic instance of the result.

Currently, the context is ignored since the context only currently
contains the call site, and the analysis is independent of the call site.
*/
    public Variable getFlowgraphVar(InvocationContext context, String name) {
        Object obj = nativeNameToVariable.get(name);
        
        if (obj != null) {
            return (Variable)obj;
        } else {
            Variable v = new Variable(getSolver());
            
            nativeNameToVariable.put(name, v);
            
            if (Globals.debug) env.addToken(v, new ExternalCFGVarDebugToken(name));
            
            return v;
        }
    }
    
    void addToken(Variable v, Token t) {
        env.addToken(v, t);
    }
    
    void setJBCType(Variable v, JBCType t) {
        if (Globals.debug) {
            env.setJBCType(v, t);
        }
    }
    
    World getSolver() {
        return solver;
    }
    
    ExternalFlowgraph getNativeCode(String name) {
        return env.getWorld().getNativeCode(name);
    }
    
    ExternalFlowgraph getNativeCode(JBCMethod name) {
        return env.getWorld().getNativeCode(name);
    }
    
    JBCClass getSpecialClass(String name) {
        return env.getWorld().getSpecialClass(name);
    }
    
    SEMIVirtualCallResolver getCallResolver() {
        return callResolver;
    }
    
    SEMISingletonUsageDetector getSingletonUsageDetector() {
        return singletonUsageDetector;
    }
    
    public void notifyLive(JBCMethod m) {
        (new JBCConstraintGenerator(this, m, rootContext)).makeLive();
    }
    
    public void notifyLive(JBCMethod m, ExternalFlowgraph fg) {
        (new FlowgraphConstraintGenerator(this, m, fg, rootContext)).makeLive();
    }
    
    public void notifyLive(String flowgraphName) {
        (new FlowgraphConstraintGenerator(this, flowgraphName, rootContext)).makeLive();
    }
    
    public void updateChange(JBCMethod m) {
        if (methodToBytecodeEvaluators.get(m) != null) {
            (new JBCConstraintGenerator(this, m, rootContext)).makeLive();
        }
    }
    
    public void updateChange(JBCMethod m, ExternalFlowgraph fg) {
        if (keyToFlowgraphEvaluators.get(m) != null) {
            (new FlowgraphConstraintGenerator(this, m, fg, rootContext)).makeLive();
        }
    }
    
    public void updateChange(String flowgraphName) {
        if (keyToFlowgraphEvaluators.get(flowgraphName) != null) {
            (new FlowgraphConstraintGenerator(this, flowgraphName, rootContext)).makeLive();
        }
    }
    
    private void addClassInitializer(Variable v) {
        if (Globals.debug && v == null) {
            Globals.nonlocalError("Notified code as top-level before notifying live!");
        }
        
        World w = getSolver();
        Variable initializer = getClassInitializerFunction()
            .getInstance(w, new JBCInternalInstance());
        
        if (makeGlobalComponents) {
            initializer.getComponent(w, JBCMethodGlobalsArgComponent.get())
                .makeEqual(w, v.getComponent(w, JBCMethodGlobalsArgComponent.get()));
        }
        
        if (makeExceptionComponents) {
            initializer.getComponent(w, JBCMethodExceptionComponent.get())
                .makeEqual(w, v.getComponent(w, JBCMethodExceptionComponent.get()));
        }
    }
    
    public void notifyTopLevel(String flowgraphName) {
        if (hoistClassInitializers) {
            addClassInitializer(getFlowgraphVar(rootContext, flowgraphName));
        }
    }
    
    Variable getMethodVar(InvocationContext context, JBCMethod m) {
        Object obj = methodToVariable.get(m);
        
        if (obj != null) {
            return (Variable)obj;
        } else {
            Variable v = new Variable(getSolver());
            
            methodToVariable.put(m, v);
            
            if (Globals.debug) env.addToken(v, new JBCMethodVarDebugToken(m));
            
            setJBCType(v, m.getMethodType());
            
            return v;
        }
    }
    
    private Vector addSubclassInfo(JBCClass c) {
        Object subclassesObj = classToSubclasses.get(c);
        
        if (subclassesObj == null) {
            Vector subclasses = new Vector();
            
            classToSubclasses.put(c, subclasses);
            
            JBCClass superclass = c.getSuperClass();
            
            if (superclass != null) {
                addSubclassInfo(superclass).addElement(c);
            }
            
            JBCClass[] superInterfaces = c.getSuperInterfaces();
            
            for (int i = 0; i < superInterfaces.length; i++) {
                addSubclassInfo(superInterfaces[i]).addElement(c);
            }
            
            return subclasses;
        } else {
            return (Vector)subclassesObj;
        }
    }
    
    private void addAllClassMethods(JBCClass c, JBCClass parent, CompactSet visitedClasses) {
        if (visitedClasses.get(parent) == null) {
            visitedClasses.addUnconditionally(parent);
            
            for (Enumeration e = parent.getMethods(); e.hasMoreElements();) {
                Object mObj = e.nextElement();
                Object contextMapObj = declaringMethodToSiteComponents.get(mObj);
                
                if (contextMapObj != null) {
                    Hashtable contextMap = (Hashtable)contextMapObj;
                    JBCMethod m = (JBCMethod)mObj;
                    
                    for (Enumeration contexts = contextMap.keys(); contexts.hasMoreElements();) {
                        InvocationContext context = (InvocationContext)contexts.nextElement();
                        
                        addClassMethod(c, m, context,
                            (InvocationContextComponent)contextMap.get(context));
                    }
                }
            }
            
            JBCClass superclass = parent.getSuperClass();
            
            if (superclass != null) {
                addAllClassMethods(c, superclass, visitedClasses);
            }
            
            JBCClass[] superInterfaces = parent.getSuperInterfaces();
            
            for (int i = 0; i < superInterfaces.length; i++) {
                addAllClassMethods(c, superInterfaces[i], visitedClasses);
            }
        }
    }
    
    Variable getClassInitializerMethod(InvocationContext context, JBCClass c) {
        JBCMethod m = c.getInitializerMethod();
        
        if (m == null) {
            return null;
        } else {
            return getMethodVar(context, m);
        }
    }
    
    Variable getClassVar(JBCClass c) {
        Object obj = classToVariable.get(c);
        
        if (obj != null) {
            return (Variable)obj;
        } else {
            Variable v = new Variable(getSolver());
            
            classToVariable.put(c, v);
            setJBCType(v, JBCType.OBJECT);
            
            if (Globals.debug) env.addToken(v, new JBCClassVarDebugToken(c));
            
            /* make sure this class and any superclasses are in the classToSubclasses structure */
            addSubclassInfo(c);
            addAllClassMethods(c, c, new CompactSet());
            
            SlotManager.addSubchunkStructure(this, c, v);
            
            return v;
        }
    }
    
/* The following methods deal with object slots. They are farmed out to
   SlotManager for clarity. */
    Variable makeDowncast(Variable objVar, JBCClass c) {
        if (breakoutDowncastSubobjects) {
            return SlotManager.makeDowncast(this, objVar, c);
        } else {
            return objVar;
        }
    }
    
    Variable makeNonstaticField(int mode, Variable objVar, JBCField f, int structMode) {
        return SlotManager.makeNonstaticField(this, mode, objVar, f, structMode);
    }
    
    Variable makeStaticField(int mode, Variable globals, JBCField f, int structMode) {
        return SlotManager.makeStaticField(this, mode, globals, f, structMode);
    }
    
    Variable makeNonstaticUserField(int mode, Variable objVar, UserField f, int structMode) {
        return SlotManager.makeNonstaticUserField(this, mode, objVar, f, structMode);
    }
    
    Variable makeStaticUserField(int mode, Variable globals, UserField f, int structMode) {
        return SlotManager.makeStaticUserField(this, mode, globals, f, structMode);
    }
    
    void makeInitializedValue(Variable v) {
        SlotManager.makeInitializedValue(this, v);
    }
    
    private void addClassMethod(JBCClass c, JBCMethod m,
        InvocationContext context, InvocationContextComponent component) {
        JBCMethod method = c.getImplementationMethod(m);
        World w = getSolver();
        
        if (method == null) {
            Globals.nonlocalError("Unimplemented method " + m.getMethodName()
                + " in instantiated class " + c.getClassName());
        } else {
            getMethodVar(context, method)
                .getInstance(w, new JBCInheritedMethodInstance(c, m))
                .makeEqual(w,
                    SlotManager.makeNonstaticMethod(this, Variable.CMODE, ((Variable)classToVariable.get(c)),
                        m, component));
        }
    }
    
    private void updateClassWithMethodInvocation(JBCClass c, JBCMethod m,
        InvocationContext context, InvocationContextComponent component,
        Hashtable visited) {
        if (visited.get(c) == null) {
            visited.put(c, c);
            
            if (classToVariable.get(c) != null) {
                addClassMethod(c, m, context, component);
            }
            
            Object cSubclassesObj = classToSubclasses.get(c);
            
            if (cSubclassesObj != null) {
                for (Enumeration e = ((Vector)cSubclassesObj).elements(); e.hasMoreElements();) {
                    updateClassWithMethodInvocation((JBCClass)e.nextElement(),
                        m, context, component, visited);
                }
            }
        }
    }
    
    boolean useStaticDispatch(InvocationContext context, JBCMethod m) {
        return JBCCodeUtilities.useStaticDispatch(m);
    }
    
    Variable makeNonstaticMethod(int mode, InvocationContext context, Variable objVar, JBCMethod method) {
        Object siteTableObj = declaringMethodToSiteComponents.get(method);
        Hashtable siteTable;
        
        if (siteTableObj != null) {
            siteTable = (Hashtable)siteTableObj;
        } else {
            siteTable = new Hashtable();
            declaringMethodToSiteComponents.put(method, siteTable);
        }
        
        Object siteComponentObj = siteTable.get(context);
        InvocationContextComponent siteComponent;
        
        if (siteComponentObj != null) {
            siteComponent = (InvocationContextComponent)siteComponentObj;
        } else {
            siteComponent = new InvocationContextComponent(context);
            siteTable.put(context, siteComponent);
            
            updateClassWithMethodInvocation(method.getContainingClass(), method,
                context, siteComponent, new Hashtable());
        }
        
        return SlotManager.makeNonstaticMethod(this, mode, objVar, method, siteComponent);
    }
    
/**
For debugging only.
*/
    public String dumpVarInfo(Variable v) {
        return env.dumpVarInfo(v);
    }
}
