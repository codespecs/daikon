/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.rta;

import ajax.analyzer.*;
import ajax.util.*;
import ajax.jbc.*;
import ajax.jbc.util.*;
import java.util.*;
import ajax.Globals;
import ajax.analyzer.semantics.*;
import ajax.jbc.typechecker.*;

/**
The RTA analysis basically determines whether two value-points are related
by comparing their Java types. It keeps track of which classes may actually
be instantiated, by recording the live calls to 'new', and uses those
classes to determine the "type compatibility" relation.

Sometimes we do not know the type of a value-point, e.g. in some user fields,
or in code specifications. These points are assigned a TOP type, giving us
a coarse approximation.

We compute a DAG of types; edges represent the subtype/supertype relation. The DAG
is constructed lazily and has the property that types which are known to have no instantiations
(and no instantiated subtypes) are not represented in the graph.
*/
public class RTA implements GenericAnalyzer, JBCObserver, OpcodeConstants {
/** The type of all values. */
    static final JBCType TOP = new JBCBaseType("TOP");
    
    private static final Object DEFAULT_QUERY_FIELD = new String("DEFAULT_QUERY_FIELD");
    
    private GenericAnalyzerConsumer consumer = null;
    private JBCWorld world;
    private boolean useExactClassTypes = true;
    private boolean useInstanceOfTracking = true;
    private Location transitiveSemanticsLocation = null;
    private boolean distinctPrimitives = true;
    private boolean mergingComparisons = false;
    
/** The set of types we believe could be instantiated. */
    private CompactSet instantiatedTypes = null;
/** The map from types to their subtypes in the type DAG */
    private Hashtable typeToSubtypes = new Hashtable();
    private Hashtable queryFamilies = new Hashtable();
    
    private Object cachedTypeInfoKey = null;
    private Object cachedTypeInfo = null;
    
    public RTA(JBCWorld world) {
        this.world = world;
        
        world.addObserver(this);
    }
    
    public void setUsePreciseClassTypes(boolean v) {
        useExactClassTypes = v;
    }
    
    public void setUseInstanceOfTracking(boolean v) {
        useInstanceOfTracking = v;
    }
    
    private Error dieInvalidSemantics() {
        return new Error("Reference-combining semantics not supported by RTA!");
    }

    private int[] convertingOps = {
        OP_i2l, OP_i2f, OP_i2d, OP_l2i, OP_l2f, OP_l2d, OP_f2i,
        OP_f2l, OP_f2d, OP_d2i, OP_d2l, OP_d2f, OP_i2b, OP_i2c, OP_i2s,
        OP_lcmp, OP_fcmpl, OP_fcmpg, OP_dcmpl, OP_dcmpg,
    };

    private int[] objectOps = {
        OP_getfield, OP_arraylength, OP_instanceof
    };

    private int[] heterogenousInputOps = {
        OP_lshl, OP_lshr, OP_lushr
    };
    
    private int[] refComparisons = {
        OP_if_acmpeq, OP_if_acmpne
    };
    
    public void setSemantics(Semantics s) {
        transitiveSemanticsLocation = null;
        
        if (s instanceof StandardSemantics) {
        } else if (s instanceof TransitiveSemantics) {
            transitiveSemanticsLocation = ((TransitiveSemantics)s).getLocation();
        } else if (s instanceof CombiningSemantics) {
            CombiningSemantics c = (CombiningSemantics)s;

            for (int i = 0; i < objectOps.length; i++) {
  	        if (c.isCombiningInputWithOutput((byte)objectOps[i])) {
                    throw dieInvalidSemantics();
	        }
	    }

            for (int i = 0; i < convertingOps.length; i++) {
  	        if (c.isCombiningInputWithOutput((byte)convertingOps[i])) {
                    distinctPrimitives = false;
	        }
	    }

            for (int i = 0; i < heterogenousInputOps.length; i++) {
  	        if (c.isCombiningInputs((byte)heterogenousInputOps[i])) {
                    distinctPrimitives = false;
	        }
	    }

            for (int i = 0; i < refComparisons.length; i++) {
  	        if (c.isCombiningInputs((byte)refComparisons[i])) {
                    mergingComparisons = true;
	        }
	    }
        }
    }
    
    private void addBaseType(Object type) {
        instantiatedTypes.addUnconditionally(type);
        ((Vector)typeToSubtypes.get(TOP)).addElement(type);
    }
    
    private void initTypeTables() {
        instantiatedTypes.addUnconditionally(TOP);
        typeToSubtypes.put(TOP, new Vector());
        addBaseType(JBCType.RETURNADDR);
        addBaseType(JBCType.INT);
        addBaseType(JBCType.LONG);
        addBaseType(JBCType.FLOAT);
        addBaseType(JBCType.DOUBLE);
        addBaseType(world.getSpecialClass("java.lang.Object"));
        notifyClassInstantiated(world.getSpecialClass("java.lang.String"));
    }
    
    private void conditionalInit() {
        if (instantiatedTypes == null) {
            instantiatedTypes = new CompactSet();
            initTypeTables();
        }
    }
    
    GenericAnalyzerConsumer getConsumer() {
        return consumer;
    }
    
    public void setConsumer(GenericAnalyzerConsumer consumer) {
        this.consumer = consumer;
    }
    
    public JBCWorld getWorld() {
        return world;
    }

    public boolean workBeforeLiveProcessing() {
        return false;
    }
    
    public boolean work() {
        return false;
    }
    
/**
Computes an Enumeration of the subtypes of the given type, using the type DAG (see
introduction).
*/
    Enumeration getSubtypes(Object type) {
        Vector subtypes = (Vector)typeToSubtypes.get(type);
        
        if (subtypes == null) {
            return EmptyEnumerator.get();
        } else {
            return subtypes.elements();
        }
    }
    
    private static JBCClass getImmediateSuperType(JBCClass c) {
        if (c.isArray()) {
            // For an array, the immediate supertype is an array
            // of the immediate supertype of the component type, unless
            // the component type is Object or primitive
            JBCClass componentClass = c.getArrayComponentClass();
            
            if (componentClass != null) {
                JBCClass componentSuper = getImmediateSuperType(componentClass);

                if (componentSuper != null) {
                    return componentSuper.getArrayOf();
                }
            }
        }
        
        // For Object[] and primitive arrays, will return Object
        return c.getSuperClass();
    }
    
/**
Computes an Enumeration of the supertypes of the given type, using the type DAG (see
introduction).
*/
    Enumeration getSupertypes(Object type) {
        if (type instanceof JBCClass) {
            JBCClass c = (JBCClass)type;
            JBCClass superclass = c.getSuperClass();
            
            if (superclass == null) {
                // here and below, we assume that if a class has no
                // superclass (i.e. the class is Object), then the class
                // has no superinterfaces
                return new SingletonEnumerator(TOP);
            } else {
                JBCClass[] superinterfaces = c.getSuperInterfaces();
                
                if (superinterfaces.length == 0) {
                    return new SingletonEnumerator(superclass);
                } else {
                    JBCClass[] supers = new JBCClass[superinterfaces.length + 1];
                    
                    supers[0] = getImmediateSuperType(c);
                    System.arraycopy(superinterfaces, 0, supers, 1, superinterfaces.length);
                    
                    return new ArrayEnumerator(supers);
                }
            }
        } else if (type instanceof PreciseClassType) {
            return new SingletonEnumerator(((PreciseClassType)type).getObjectType().getClassDef());
        } else if (type == TOP) {
            return EmptyEnumerator.get();
        } else {
            if (Globals.debug && !(type instanceof JBCBaseType)) {
                Globals.localError("invalid type: " + type);
            }
            
            return new SingletonEnumerator(TOP);
        }
    }
    
    boolean isTypeInstantiated(Object type) {
        return instantiatedTypes.get(type) != null;
    }
    
    private void notifyClassInstantiated(JBCClass c) {
        notifyTypeInstantiated(PreciseClassType.get(c));
    }
    
    private void notifyTypeInstantiated(Object type) {
        if (instantiatedTypes.get(type) == null) {
            instantiatedTypes.addUnconditionally(type);
            
            for (Enumeration e = getSupertypes(type); e.hasMoreElements();) {
                Object supertype = e.nextElement();
                
                notifyTypeInstantiated(supertype);
                
                Vector subtypes = (Vector)typeToSubtypes.get(supertype);
                
                if (subtypes == null) {
                    subtypes = new Vector();
                    typeToSubtypes.put(supertype, subtypes);
                }
                
                subtypes.addElement(type);

                for (Enumeration e2 = queryFamilies.elements(); e2.hasMoreElements();) {
                    ((RTAQueryFamily)e2.nextElement()).addSubtypeRelationship(supertype, type);
                }
            }
            
            for (Enumeration e = queryFamilies.elements(); e.hasMoreElements();) {
                ((RTAQueryFamily)e.nextElement()).addTypeInstantiation(type);
            }
        }
    }
    
    private void examineFlowgraph(ExternalFlowgraph fg) {
        for (Enumeration e = ExternalFlowgraphUtilities.getDefNodes(fg).elements(); e.hasMoreElements();) {
            Object node = e.nextElement();
            
            if (node instanceof ExternalCFGNewObjectDefNode) {
                notifyClassInstantiated(((ExternalCFGNewObjectDefNode)node).getObjectClass());
            }
        }
    }
    
    private void examineCode(JBCMethod method) {
        MethodData data = method.getData();
        byte[] code = data.getCode();
        boolean[] instructionStarts = JBCCodeUtilities.getInstructionStarts(data);
        
        for (int i = 0; i < instructionStarts.length; i++) {
            if (instructionStarts[i]) {
                switch (code[i] & 0xFF) {
                    case OpcodeConstants.OP_new:
                    case OpcodeConstants.OP_newarray:
                    case OpcodeConstants.OP_anewarray:
                    case OpcodeConstants.OP_multianewarray: {
                        JBCClass c = JBCCodeUtilities.resolveInstructionClass(method, code, i);
                        
                        if (c != null) {
                            notifyClassInstantiated(c);
                        }
                        break;
                    }
                }
            }
        }
    }
    
    public boolean isNativeSpecificationAccurate() {
        return true;
    }
    
    public void notifyLive(JBCMethod method) {
        conditionalInit();
        
        examineCode(method);
    }
    
    public void notifyLive(JBCMethod method, ExternalFlowgraph spec) {
        conditionalInit();
        
        examineFlowgraph(spec);
    }
    
    public void notifyLive(String flowgraphName) {
        conditionalInit();
        
        examineFlowgraph(world.getNativeCode(flowgraphName));
    }
    
    public void addQueryFamily(JBCQueryFamily family) {
        queryFamilies.put(family, new RTAQueryFamily(this, family));
    }
    
    public void removeQueryFamily(JBCQueryFamily family) {
        queryFamilies.remove(family);
    }
    
    private Object getCachedTypeInfo(Object key) {
        if (cachedTypeInfoKey != null && cachedTypeInfoKey.equals(key)) {
            return cachedTypeInfo;
        } else {
            return null;
        }
    }
    
    private Enumeration getTypesAt(JBCMethod method, int offset, JBCExpression expression) {
        if (transitiveSemanticsLocation instanceof JBCLocation) {
            JBCLocation l = (JBCLocation)transitiveSemanticsLocation;
            
            if (l.getMethod().equals(method) && l.getOffset() == offset) {
                return new SingletonEnumerator(TOP);
            }
        }
        
        RTABytecodeTypeInfo typeInfo = (RTABytecodeTypeInfo)getCachedTypeInfo(method);
        
        if (typeInfo == null) {
            BytecodeTypechecker checker = new BytecodeTypechecker(method);
            
            checker.setEnablePreciseClasses(useExactClassTypes);
            checker.setTrackInstanceOf(useInstanceOfTracking);
            checker.execute();

            if (mergingComparisons && checker.didCompareIncomparableObjectTypes()) {
                Globals.userError("Found comparison of incomparable object types");
	    }
            
            typeInfo = new RTABytecodeTypeInfo(checker, distinctPrimitives);
            cachedTypeInfoKey = method;
            cachedTypeInfo = typeInfo;
        }
        
        return typeInfo.getTypesAt(offset, expression.removeQueryField());
    }
    
    private Enumeration getTypesAt(JBCMethod method, ExternalCFGNode node, JBCExpression expression) {
        if (transitiveSemanticsLocation instanceof ExternalLocation) {
            ExternalLocation l = (ExternalLocation)transitiveSemanticsLocation;
            
            if (l.getFunction().equals(method) && l.getNode().equals(node)) {
                return new SingletonEnumerator(TOP);
            }
        }
        
        RTAFlowgraphTypeInfo typeInfo = (RTAFlowgraphTypeInfo)getCachedTypeInfo(method);
        
        if (typeInfo == null) {
            typeInfo = new RTAFlowgraphTypeInfo(method, method.getNativeCode(),
                distinctPrimitives);
            cachedTypeInfoKey = method;
            cachedTypeInfo = typeInfo;
        }
        
        return typeInfo.getTypesAt(node, expression.removeQueryField());
    }
    
    private Enumeration getTypesAt(String name, ExternalCFGNode node, JBCExpression expression) {
        if (transitiveSemanticsLocation instanceof ExternalLocation) {
            ExternalLocation l = (ExternalLocation)transitiveSemanticsLocation;
            
            if (l.getFunction().equals(name) && l.getNode().equals(node)) {
                return new SingletonEnumerator(TOP);
            }
        }
        
        RTAFlowgraphTypeInfo typeInfo = (RTAFlowgraphTypeInfo)getCachedTypeInfo(name);
        
        if (typeInfo == null) {
            typeInfo = new RTAFlowgraphTypeInfo(world, world.getNativeCode(name),
                distinctPrimitives);
            cachedTypeInfoKey = name;
            cachedTypeInfo = typeInfo;
        }
        
        return typeInfo.getTypesAt(node, expression.removeQueryField());
    }

    public void addSourceDatum(JBCQueryFamily family, JBCMethod method, int offset, JBCExpressionContext context, JBCExpression expression, Object intermediate) {
        conditionalInit();
        
        ((RTAQueryFamily)queryFamilies.get(family)).
            addSourceDatum(getTypesAt(method, offset, expression),
                expression.getQueryField(), intermediate);
    }
    
    public void addSourceDatum(JBCQueryFamily family, JBCMethod method, ExternalCFGNode node, JBCExpressionContext context, JBCExpression expression, Object intermediate) {
        conditionalInit();
        
        ((RTAQueryFamily)queryFamilies.get(family)).
            addSourceDatum(getTypesAt(method, node, expression),
                expression.getQueryField(), intermediate);
    }
    
    public void addSourceDatum(JBCQueryFamily family, String name, ExternalCFGNode node, JBCExpressionContext context, JBCExpression expression, Object intermediate) {
        conditionalInit();
        
        ((RTAQueryFamily)queryFamilies.get(family)).
            addSourceDatum(getTypesAt(name, node, expression),
                expression.getQueryField(), intermediate);
    }
    
    public void addTargetDatum(JBCQueryFamily family, JBCMethod method, int offset, JBCExpressionContext context, JBCExpression expression, Object targetCookie) {
        conditionalInit();
        
        ((RTAQueryFamily)queryFamilies.get(family)).
            addTargetDatum(getTypesAt(method, offset, expression),
                expression.getQueryField(), targetCookie);
    }
    
    public void addTargetDatum(JBCQueryFamily family, JBCMethod method, ExternalCFGNode node, JBCExpressionContext context, JBCExpression expression, Object targetCookie) {
        conditionalInit();
        
        ((RTAQueryFamily)queryFamilies.get(family)).
            addTargetDatum(getTypesAt(method, node, expression),
                expression.getQueryField(), targetCookie);
    }
    
    public void addTargetDatum(JBCQueryFamily family, String name, ExternalCFGNode node, JBCExpressionContext context, JBCExpression expression, Object targetCookie) {
        conditionalInit();
        
        ((RTAQueryFamily)queryFamilies.get(family)).
            addTargetDatum(getTypesAt(name, node, expression),
                expression.getQueryField(), targetCookie);
    }

    public void notifyClassChanged(JBCClass c, ClassData from, ClassData to) {
        for (Enumeration e = c.getMethods(); e.hasMoreElements();) {
            examineCode((JBCMethod)e.nextElement());
        }
    }
    
    public void notifyClassLoaded(JBCClass c) {
        /* do nothing, we don't care */
    }
    
    public void notifyNativeCodeLoaded(JBCMethod m, ExternalFlowgraph fg) {
        /* do nothing, we don't care */
    }
    
    public void notifyNativeCodeLoaded(String name, ExternalFlowgraph fg) {
        /* do nothing, we don't care */
    }
    
    public void notifyNativeCodeChanged(JBCMethod m, ExternalFlowgraph from, ExternalFlowgraph to) {
        conditionalInit();
        examineFlowgraph(to);
    }
    
    public void notifyNativeCodeChanged(String name, ExternalFlowgraph from, ExternalFlowgraph to) {
        conditionalInit();
        examineFlowgraph(to);
    }
    
    public void notifyAddedMainInvocation(String name) {
        /* do nothing, we don't care */
    }
}
