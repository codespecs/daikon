/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver;

import java.util.*;
import ajax.Globals;
import ajax.util.*;

/*
The constraint set is represented as follows:

Equality constraints are represented by a union-find data structure that groups
Variables into equivalence classes. The "head" Variable of each class has
a reference to a VarData that contains the data representing the component
and instantiation constraints for that equivalence class.

Component constraints <b>from</b> this object are stored in a hashtable
(for arbitrary component types) or in an array (for components that were hinted to be
indexed according to a fixed arity).

Component constraints <b>to</b> this object are stored in a doubly-linked
list.

Instantiation constraints <b>from</b> this object are stored in a
hashtable or in an array (for variables with two instantiations) or in a
single object (for variables that have just one instantiation).
Instantiation constraints <b>to</b> this object are stored in a doubly-linked
list.

For efficiency, some cul-de-sacs are suppressed. A cul-de-sac is a variable that
is an instance of at most one variable (multiple instantiation relationships
between the two variables are allowed).

Soundness of suppression:
vSuppose x and y are variables that are not cul-de-sacs. Then there is a
variable z that is a (transitive) instance of both x and y if and only if
there is a variable z' that is a transitive instance of both x and y where
z' is not a cul-de-sac.

We achieve this suppression by delaying the application of the rule that
propagates components to instances. Instead, we propagate "advertisements" of
components. When two advertisements for the same component label
reach the same variable (and they do not have the same source), then we
perform actual propagation. Likewise, when an advertisement reaches a real
component, we perform actual propagation.

We have the "pseudo-invariant" that no instance of an advertisement is a real
component. This is "psuedo" because at any given time there could be
advertisements with instances that are real components, and those components
would be dirty. So we have to make sure we work alright when it's not true.
*/
public class VarData {
    public static final boolean applyExtendedOccursCheckToChildren = false; // CONFIG
    public static final boolean applyGlobal = true; // CONFIG
    public static final boolean obeyQueryNotifications = false;
    
    public static final boolean dumpAdvertisementBottlenecks = false;
    
    /** Either 'null', or an InstantiationRecord, or a CompactSet.
       It is the collection of InstantiationRecords that lead to the instances
       of this variable.
    */
    private Object instances = null;

    /** Either 'null', or an InstantiationRecord, or an IdentityCompactSet.
    */
    private Object sources = null;
    
    /** Either 'null', or a ComponentRecord, or a CompactSet.
       It is the collection of ComponentRecords that lead to the components
       of this variable.
    */
    private Object components = null;
    
    /** Either 'null', or a ComponentRecord, or an IdentityCompactSet.
    */
    private Object parents = null;

    /** This is the component cluster of the variable. */
    private ComponentCluster cluster;
    
    private int flags;
    
    private static final int VAR_GLOBAL            = 0x01;
    private static final int VAR_QUERIED           = 0x02;
    private static final int VAR_INHERIT_SOURCES   = 0x04;
    private static final int VAR_INHERIT_INSTANCES = 0x08;
    
    VarData() {
        this(new ComponentCluster());
    }
    
    VarData(ComponentCluster cluster) {
        this.cluster = cluster;
        
        flags = cluster != null ? 0 : VAR_GLOBAL;
    }
    
    int getFlags() {
        return flags;
    }
    
    private boolean isGlobal() {
        return (flags & VAR_GLOBAL) != 0;
    }
    
    private void makeGlobal(World w, Variable v, Vector varsToMakeEqual) {
        Vector instanceLabels = new Vector();
        
        for (Enumeration e = enumerateTrueComponents(); e.hasMoreElements();) {
            ComponentRecord r = (ComponentRecord)e.nextElement();
            
            w.notifyGlobalComponent(r);
        }
        
        cluster = null;
        
        for (Enumeration e = enumerateInstances(w); e.hasMoreElements();) {
            InstantiationRecord r = (InstantiationRecord)e.nextElement();
            
            instanceLabels.addElement(r.label);
            addVarsToMakeEqual(varsToMakeEqual, v, r.to);
        }
        
        for (Enumeration e = instanceLabels.elements(); e.hasMoreElements();) {
            InstanceLabel label = (InstanceLabel)e.nextElement();
            
            destroyInstance(w, label);
        }
        
        for (Enumeration e = enumerateTrueComponents(); e.hasMoreElements();) {
            ComponentRecord r = (ComponentRecord)e.nextElement();
            Variable childHead = r.getComponentHead();
            VarData childData = childHead.getData();
            
            w.notifyGlobalComponent(r);
            
            if (w.getDumpOperations() && !childData.isGlobal()) {
                Globals.writeLog(this, "Following global component from " + Globals.getHexID(v.getHead()));
            }
            childData.setGlobal(w, childHead, varsToMakeEqual);
        }
    }
    
    void setGlobal(World w, Variable v, Vector varsToMakeEqual) {
        if (applyGlobal && !isGlobal()) {
            validateInstances(w, false);
            validateAllInstanceSources(w);
            
            flags |= VAR_GLOBAL;
            if (w.getDumpOperations()) {
                Globals.writeLog(this, "G " + Globals.getHexID(v.getHead()));
            }
            makeGlobal(w, v, varsToMakeEqual);
        }
    }
    
    void setGlobal(World w, Variable v) {
        if (applyGlobal && !isGlobal()) {
            Vector varsToMakeEqual = new Vector();
            
            setGlobal(w, v, varsToMakeEqual);
            makeVarsEqual(w, varsToMakeEqual);
        }
    }
    
    private boolean isQueried() {
        return (flags & VAR_QUERIED) != 0;
    }
    
    private void setQueried() {
        flags |= VAR_QUERIED;
    }
    
    void setQueried(World w, Variable v) {
        if (obeyQueryNotifications && !isQueried()) {
            setQueried();
        }
    }
    
    ComponentCluster getCluster() {
        return cluster;
    }
    
    private static Object makeObjectFromSet(CompactSetBase set) {
        switch (set.size()) {
            case 0:  return null;
            case 1:  return set.elements().nextElement();
            default: return set;
        }
    }
    
    private static int countObject(Object o) {
        if (o == null) {
            return 0;
        } else if (o instanceof CompactSetBase) {
            return ((CompactSetBase)o).size();
        } else {
            return 1;
        }
    }
    
    private static Enumeration enumerateObject(Object o) {
        if (o == null) {
            return EmptyEnumerator.get();
        } else if (o instanceof CompactSetBase) {
            return ((CompactSetBase)o).elements();
        } else {
            return new SingletonEnumerator(o);
        }
    }

    private static int computePolarity(ComponentRecord r,
        InstantiationRecord instance) {
        switch (r.getLabel().getVariance()) {
            case ComponentLabel.INVARIANT:
                return InstantiationRecord.POLARITY_BOTH;
            case ComponentLabel.COVARIANT:
                return instance.getPolarity();
            case ComponentLabel.CONTRAVARIANT: {
	        switch (instance.getPolarity()) {
                    case InstantiationRecord.POLARITY_POS:
                        return InstantiationRecord.POLARITY_NEG;
                    case InstantiationRecord.POLARITY_NEG:
                        return InstantiationRecord.POLARITY_POS;
                    case InstantiationRecord.POLARITY_BOTH:
                        return InstantiationRecord.POLARITY_BOTH;
                    default:
                        Globals.localError("Unknown polarity: " 
                            + instance.getPolarity());
                        return 0;
		}
	    }
            default:
                Globals.localError("Uknown variance: "
                    + r.getLabel().getVariance());
                return 0;
        }
    }
    
/**
Dependencies of inherited sources:
parents
sources of parents
components of sources of parents
*/
    private SourceCompactSet computeInheritedSources(World w) {
        SourceCompactSet result = new SourceCompactSet();
        
        for (Enumeration e = enumerateParents(); e.hasMoreElements();) {
            ComponentRecord r = (ComponentRecord)e.nextElement();
                
            for (Enumeration e2 = r.getParentHead().getData().enumerateSources(w);
                e2.hasMoreElements();) {
                InstantiationRecord source = (InstantiationRecord)e2.nextElement();
                ComponentRecord r2 = source.getSourceHead().getData().getComponent(r.label);
                
                if (r2 != null && r2.getKind() == ComponentRecord.COMPONENT
                    && modesMatch(r2.getModes(), r.getModeInstances())) {
                    VarData r2ChildData = r2.getComponentHead().getData();
                    
                    if (!r2ChildData.isGlobal()) {
                        result.addUnconditionally(
                           new InstantiationRecord(r2.getComponentHead(), r.getComponentHead(), source.label, computePolarity(r, source)));
                    }
                }
            }
        }
        
        return result;
    }
    
    private CompactSet computeInheritedInstances(World w) {
        CompactSet result = new CompactSet();
        
        if (!isGlobal()) {
            for (Enumeration e = enumerateParents(); e.hasMoreElements();) {
                ComponentRecord r = (ComponentRecord)e.nextElement();
                
                for (Enumeration e2 = r.getParentHead().getData().enumerateInstances(w);
                    e2.hasMoreElements();) {
                    InstantiationRecord instance = (InstantiationRecord)e2.nextElement();
                    
                    if (result.get(instance.label) == null) {
                        ComponentRecord r2 = instance.getInstanceHead().getData().getComponent(r.label);
                        
                        if (r2 != null && r2.getKind() == ComponentRecord.COMPONENT
                            && modesMatch(r.getModes(), r2.getModeInstances())) {
                            result.addUnconditionally(
                                new InstantiationRecord(r.getComponentHead(), r2.getComponentHead(), instance.label, computePolarity(r, instance)));
                        }
                    }
                }
            }
        }
        
        return result;
    }
    
    boolean isSourceInheritanceValid(World w) {
        SourceCompactSet inherited = computeInheritedSources(w);
        
        if (countObject(sources) != inherited.size()) {
            return false;
        } else {
            for (Enumeration e = enumerateObject(sources); e.hasMoreElements();) {
                if (inherited.get(e.nextElement()) == null) {
                    return false;
                }
            }
            
            return true;
        }
    }
    
    boolean isInstanceInheritanceValid(World w) {
        CompactSet inherited = computeInheritedInstances(w);
        
        if (countObject(instances) != inherited.size()) {
            return false;
        } else {
            for (Enumeration e = enumerateObject(instances); e.hasMoreElements();) {
                if (inherited.get(e.nextElement()) == null) {
                    return false;
                }
            }
            
            return true;
        }
    }
    
    void cacheSources(World w, boolean updated) {
        if (!w.isCheckingConsistency()) {
            sources = null;
        }
        flags |= VAR_INHERIT_SOURCES;
    }
    
    void cacheInstances(World w, boolean updated) {
        if (!w.isCheckingConsistency()) {
            instances = null;
        }
        flags |= VAR_INHERIT_INSTANCES;
    }
    
    void validateSources(World w, boolean toBeUpdated) {
        if (!World.performCaching) {
            return;
        }
        
        if (toBeUpdated) {
            w.notifyVarDataUpdatedSources(this);
        }
        
        if ((flags & VAR_INHERIT_SOURCES) != 0) {
            w.notifyVarDataUncachedSources(this);
            flags &= ~VAR_INHERIT_SOURCES;
            
            if (!w.isCheckingConsistency()) {
                if (w.getDumpOperations() && countParents() > 0) {
                    Globals.writeLog(this, "Allocating sources for "
                        + Globals.getHexID(((ComponentRecord)enumerateParents().nextElement()).getComponentHead()));
                }
                    
                SourceCompactSet inheritedSources = computeInheritedSources(w);
                IdentityCompactSet realSources = new IdentityCompactSet();
                
                for (Enumeration e = inheritedSources.elements(); e.hasMoreElements();) {
                    InstantiationRecord source = (InstantiationRecord)e.nextElement();
                    VarData sourceData = source.getSourceHead().getData();
                    InstantiationRecord realSource;
                    
                    if ((sourceData.flags & VAR_INHERIT_INSTANCES) == 0) {
                        realSource = sourceData.getInstance(w, source.label);
                    } else {
                        realSource = source;
                    }
                    
                    if (Globals.debug && realSource == null) {
                        Globals.localError("Matching source not found for "
                            + Globals.getHexID(source.getSourceHead()) + "->"
                            + Globals.getHexID(source.getInstanceHead()));
                    }
                    
                    realSources.addUnconditionally(realSource);
                }
                
                sources = makeObjectFromSet(realSources);
            }
        }
    }
    
    void validateInstances(World w, boolean toBeUpdated) {
        if (!World.performCaching) {
            return;
        }
        
        if (toBeUpdated) {
            w.notifyVarDataUpdatedInstances(this);
        }
        
        if ((flags & VAR_INHERIT_INSTANCES) != 0) {
            w.notifyVarDataUncachedInstances(this);
            flags &= ~VAR_INHERIT_INSTANCES;
            
            if (!w.isCheckingConsistency()) {
                if (w.getDumpOperations() && countParents() > 0) {
                    Globals.writeLog(this, "Allocating instances for "
                        + Globals.getHexID(((ComponentRecord)enumerateParents().nextElement()).getComponentHead()));
                }
                    
                CompactSet inheritedInstances = computeInheritedInstances(w);
                CompactSet realInstances = new CompactSet();
                
                for (Enumeration e = inheritedInstances.elements(); e.hasMoreElements();) {
                    InstantiationRecord instance = (InstantiationRecord)e.nextElement();
                    Variable instanceHead = instance.getInstanceHead();
                    VarData instanceData = instanceHead.getData();
                    InstantiationRecord realInstance;
                    
                    if ((instanceData.flags & VAR_INHERIT_SOURCES) == 0) {
                        realInstance = instanceData.lookupSource(instance.getSourceHead(), instance.label);
                    } else {
                        realInstance = instance;
                    }
                    
                    if (Globals.debug && realInstance == null) {
                        Globals.localError("Matching instance not found for "
                            + Globals.getHexID(instance.getSourceHead()) + "->"
                            + Globals.getHexID(instance.getInstanceHead()));
                    }
                    
                    realInstances.addUnconditionally(realInstance);
                }
                
                instances = makeObjectFromSet(realInstances);
            }
        }
    }
    
    int countSources(World w) {
        validateSources(w, false);
        
        return countObject(sources);
    }
    
    private void removeSource(World w, InstantiationRecord r) {
        validateSources(w, true);
        
        if (sources instanceof IdentityCompactSet) {
            IdentityCompactSet set = (IdentityCompactSet)sources;
            
            if (set.remove(r) == null) {
                if (Globals.debug) {
                    Globals.localError("Removing nonexistent source!");
                }
            }
            
            if (set.size() == 1) {
                sources = set.elements().nextElement();
            }
        } else if (sources == r) {
            sources = null;
        } else if (Globals.debug) {
            Globals.localError("Removing nonexistent source!");
        }
    }
    
    private InstantiationRecord lookupSource(Variable sourceVarHead, InstanceLabel l) {
        if (sources == null) {
            return null;
        } else if (sources instanceof InstantiationRecord) {
            InstantiationRecord source = (InstantiationRecord)sources;
            
            return (source.getSourceHead() == sourceVarHead
                    && source.label.equals(l)) ? source : null;
        } else {
            for (Enumeration e = ((IdentityCompactSet)sources).elements(); e.hasMoreElements();) {
                InstantiationRecord source = (InstantiationRecord)e.nextElement();
                
                if (source.getSourceHead() == sourceVarHead && source.label.equals(l)) {
                    return source;
                }
            }
            
            return null;
        }
    }
    
    private void addSource(World w, InstantiationRecord r) {
        validateSources(w, true);
        
        if (sources == null) {
            sources = r;
        } else if (sources instanceof InstantiationRecord) {
            Object[] records = { sources, r };
            
            sources = new IdentityCompactSet(2, new ArrayEnumerator(records));
        } else {
            ((IdentityCompactSet)sources).addUnconditionally(r);
        }
    }
    
    private Enumeration enumerateSourcesSafely(World w) {
        validateSources(w, false);
        
        return enumerateObject(sources);
    }
    
    private Enumeration enumerateSources(World w) {
        return enumerateSourcesSafely(w);
    }
    
    private Enumeration enumerateInstancesSafely(World w) {
        validateInstances(w, false);
        
        return enumerateObject(instances);
    }
    
    Enumeration enumerateInstances(World w) {
        return enumerateInstancesSafely(w);
    }
    
    int countInstances(World w) {
        validateInstances(w, false);
        
        return countObject(instances);
    }
    
    private void addInstanceUnconditionally(World w, InstantiationRecord r) {
        validateInstances(w, true);
        
        if (instances == null) {
            instances = r;
        } else if (instances instanceof InstantiationRecord) {
            Object[] objs = { instances, r };
            
            instances = new CompactSet(2, new ArrayEnumerator(objs));
        } else {
            ((CompactSet)instances).addUnconditionally(r);
        }
    }
    
    private void removeInstance(World w, InstantiationRecord r) {
        validateInstances(w, true);
        
        if (instances instanceof CompactSet) {
            CompactSet set = (CompactSet)instances;
            
            if (set.remove(r) == null) {
                if (Globals.debug) {
                    Globals.localError("Removing nonexistent instance!");
                }
            }
            
            if (set.size() == 1) {
                instances = set.elements().nextElement();
            }
        } else if (instances == r) {
            instances = null;
        } else if (Globals.debug) {
            Globals.localError("Removing nonexistent instance!");
        }
    }
    
    private InstantiationRecord lookupInstance(InstanceLabel l) {
        if (instances == null) {
            return null;
        } else if (instances instanceof InstantiationRecord) {
            InstantiationRecord instance = (InstantiationRecord)instances;
            
            return instance.label.equals(l) ? instance : null;
        } else {
            return (InstantiationRecord)((CompactSet)instances).get(l);
        }
    }
    
    InstantiationRecord getInstance(World w, InstanceLabel l) {
        validateInstances(w, false);
        
        return lookupInstance(l);
    }
    
    int countParents() {
        return countObject(parents);
    }
    
    private void mergeParents(World w, VarData d) {
        validateSources(w, true);
        validateInstances(w, true);
        d.validateSources(w, true);
        d.validateInstances(w, true);
        
        if (countParents() < d.countParents()) {
            for (Enumeration e = enumerateParents(); e.hasMoreElements();) {
                d.addParent(w, (ComponentRecord)e.nextElement());
            }
            
            parents = d.parents;
        } else {
            for (Enumeration e = d.enumerateParents(); e.hasMoreElements();) {
                addParent(w, (ComponentRecord)e.nextElement());
            }
        }
    }
    
    private void removeParent(World w, ComponentRecord r) {
        validateInstances(w, true);
        validateSources(w, true);
        
        if (parents instanceof IdentityCompactSet) {
            IdentityCompactSet set = (IdentityCompactSet)parents;
            
            if (set.remove(r) == null) {
                if (Globals.debug) {
                    Globals.localError("Removing nonexistent parent!");
                }
            }
            
            if (set.size() == 1) {
                parents = set.elements().nextElement();
            }
        } else if (parents == r) {
            parents = null;
        } else if (Globals.debug) {
            Globals.localError("Removing nonexistent parent!");
        }
    }
    
    private void addParent(World w, ComponentRecord r) {
        validateInstances(w, true);
        validateSources(w, true);
        
        if (parents == null) {
            parents = r;
        } else if (parents instanceof ComponentRecord) {
            Object[] parentList = { r, parents };
            
            parents = new IdentityCompactSet(2, new ArrayEnumerator(parentList));
        } else {
            ((IdentityCompactSet)parents).addUnconditionally(r);
        }
    }
    
    Enumeration enumerateParents() {
        return enumerateObject(parents);
    }
    
    Enumeration enumerateComponents() {
        return enumerateObject(components);
    }
    
    Enumeration enumerateTrueComponents() {
        if (components == null) {
            return EmptyEnumerator.get();
        } else if (components instanceof CompactSetBase) {
            return new TrueComponentEnumerator(((CompactSetBase)components).elements());
        } else if (((ComponentRecord)components).getKind() == ComponentRecord.COMPONENT) {
            return new SingletonEnumerator(components);
        } else {
            return EmptyEnumerator.get();
        }
    }
    
    private int countComponents() {
        return countObject(components);
    }
    
    private void addComponentUnconditionally(Object r) {
        if (components == null) {
            components = r;
        } else if (components instanceof ComponentRecord) {
            Object[] records = { r, components };
            
            components = new CompactSet(2, new ArrayEnumerator(records));
        } else {
            ((CompactSet)components).addUnconditionally(r);
        }
    }
    
    ComponentRecord getComponent(ComponentLabel l) {
        if (components == null) {
            return null;
        } else if (components instanceof ComponentRecord) {
            if (components.equals(l)) {
                return (ComponentRecord)components;
            } else {
                return null;
            }
        } else {
            return (ComponentRecord)((CompactSet)components).get(l);
        }
    }
    
    private ComponentRecord accessComponent(ComponentLabel label, Variable v) {
        ComponentRecord r = getComponent(label);
        
        if (r != null || v == null) {
            return r;
        } else {
            r = new ComponentRecord(v, label);
            
            addComponentUnconditionally(r);
            
            return r;
        }
    }
    
/**
Destroys the instance constraint with this variable as the source and the given label.
All components have their instances destroyed similarly.
This is a very unsafe operation. Use with extreme caution. It is used by the cut-through
logic to destroy cut-through instances when they've been made redundant.
*/
    void destroyInstance(World w, InstanceLabel label) {
        InstantiationRecord r = getInstance(w, label);
        
        if (r != null) {
            r.getInstanceHead().getData().removeSource(w, r);
            removeInstance(w, r);
            
            for (Enumeration e = enumerateTrueComponents(); e.hasMoreElements();) {
                ((ComponentRecord)e.nextElement()).getComponentHead().getData().destroyInstance(w, label);
            }
        }
    }
    
    Variable getComponent(World w, int modes, Variable vHead, ComponentLabel label) {
        if (!w.getApplyComponentModes()) {
            modes = (1 << Variable.CMODE) | (1 << Variable.DMODE);
        }
        
        ComponentRecord r = accessComponent(label, vHead);
        
        if (r.getKind() == ComponentRecord.COMPONENT) {
            if (r.willAddModes(modes)) {
                notifyComponentSourcesDirty(w, r);
                notifyComponentInstancesDirty(w, r);
                r.addModes(modes);
                r.addModeInstances(modes);
            }
        
            return r.child;
        } else {
            Variable child = new Variable(w);
            VarData data = new VarData(cluster);
         
            /* if this variable is global, then cluster is null, and data will be initialized
               to global. */
            child.setData(w, data);
            r.setReal(child);
            
            notifyComponentSourcesDirty(w, r);
            notifyComponentInstancesDirty(w, r);
            
            r.addModes(modes);
            r.addModeInstances(modes);
            data.addParent(w, r);
            w.notifyNewComponent(child, vHead);
            return child;  // same as r.child
        }
    }
    
    void hintFixedArity(int n) {
        // fixed arity stuff disabled, for now
    }
    
    private void hintFixedArity(VarData source) {
        // fixed arity stuff disabled, for now
    }
    
    private static Variable findAdvertisementBottleneck(Variable v1, Variable v2,
        ComponentLabel label) {
        IdentityCompactSet v1Bottlenecks = new IdentityCompactSet();
        Variable vHead = v1 == null ? null : v1.getHead();
        
        while (vHead != null) {
            ComponentRecord r = vHead.getData().getComponent(label);
            
            v1Bottlenecks.addUnconditionally(vHead);
            
            if (r.getKind() == ComponentRecord.COMPONENT) {
                break;
            } else {
                vHead = r.getComponentHead();
            }
        }

        vHead = v2 == null ? null : v2.getHead();
        
        while (vHead != null) {
            if (v1Bottlenecks.get(vHead) != null) {
                if (dumpAdvertisementBottlenecks) {
                    Globals.writeLog(VarData.class, "Find bottleneck from "
                        + Globals.getHexID(v1.getHead()) + " and "
                        + Globals.getHexID(v2.getHead()) + " yields "
                        + Globals.getHexID(vHead));
                }

                return vHead;
            }
            
            ComponentRecord r = vHead.getData().getComponent(label);
            
            if (r.getKind() == ComponentRecord.COMPONENT) {
                break;
            } else {
                vHead = r.getComponentHead();
            }
        }
        
        if (dumpAdvertisementBottlenecks) {
            Globals.writeLog(VarData.class, "Find bottleneck from "
                + Globals.getHexID(v1.getHead()) + " and "
                + Globals.getHexID(v2.getHead()) + " yields null");
        }
        
        return null;
    }
    
    private void validateDirtySources(World w, ComponentRecord r, Enumeration e, boolean updated) {
        if (!World.performCaching) {
            return;
        }
        
        if (r.getKind() == ComponentRecord.COMPONENT) {
            r.getComponentHead().getData().validateSources(w, updated);
            
            for (; e.hasMoreElements();) {
                ComponentRecord sr = ((InstantiationRecord)e.nextElement()).getSourceHead().getData().getComponent(r.label);
                    
                if (sr != null && sr.getKind() == ComponentRecord.COMPONENT) {
                    sr.getComponentHead().getData().validateInstances(w, updated);
                }
            }
        }
    }
    
    private void validateDirtyInstances(World w, ComponentRecord r, Enumeration e, boolean updated) {
        if (!World.performCaching) {
            return;
        }
        
        if (r.getKind() == ComponentRecord.COMPONENT) {
            r.getComponentHead().getData().validateInstances(w, updated);
            
            for (; e.hasMoreElements();) {
                ComponentRecord ir = ((InstantiationRecord)e.nextElement()).getInstanceHead().getData().getComponent(r.label);
                    
                if (ir != null && ir.getKind() == ComponentRecord.COMPONENT) {
                    ir.getComponentHead().getData().validateSources(w, updated);
                }
            }
        }
    }
    
    private void validateAllInstanceSources(World w) {
        if (!World.performCaching) {
            return;
        }
        
        for (Enumeration e = enumerateInstances(w); e.hasMoreElements();) {
            ((InstantiationRecord)e.nextElement()).getInstanceHead().getData().validateSources(w, false);
        }
    }
    
    private void validateAllSourceInstances(World w) {
        if (!World.performCaching) {
            return;
        }
        
        for (Enumeration e = enumerateSources(w); e.hasMoreElements();) {
            ((InstantiationRecord)e.nextElement()).getSourceHead().getData().validateInstances(w, false);
        }
    }
    
    private void validateAllComponentsDirty(World w, VarData data) {
        if (!World.performCaching) {
            return;
        }
        
        for (Enumeration e = enumerateComponents(); e.hasMoreElements();) {
            ComponentRecord r = (ComponentRecord)e.nextElement();
            
            data.validateDirtyInstances(w, r, data.enumerateInstances(w), false);
            data.validateDirtySources(w, r, data.enumerateSources(w), false);
        }
    }
    
/** Notes that components are dirty with respect to a particular set of sources and instances */
    private void notifyAllComponentsDirty(World w, VarData data) {
        for (Enumeration e = enumerateComponents(); e.hasMoreElements();) {
            ComponentRecord r = (ComponentRecord)e.nextElement();
            
            data.notifyComponentInstancesDirty(w, r);
            data.notifyComponentSourcesDirty(w, r);
        }
    }
    
    private void notifyComponentInstancesDirty(World w, ComponentRecord r) {
        w.notifyComponentInstancesDirty(r, enumerateInstances(w));
        
        if (World.performCaching) {
            validateDirtyInstances(w, r, enumerateInstances(w), true);
        }
    }

    private void notifyComponentSourcesDirty(World w, ComponentRecord r) {
        w.notifyComponentSourcesDirty(r, enumerateSources(w));
        
        if (World.performCaching) {
            validateDirtySources(w, r, enumerateSources(w), true);
        }
    }

    private void mergeComponentPair(World w, VarData fromData, Vector varsToMakeEqual,
        ComponentRecord into, ComponentRecord from) {
        w.mergeComponentDirtyState(into, from);
        
        if (into.getKind() == ComponentRecord.ADVERTISEMENT) {
            if (from.getKind() == ComponentRecord.ADVERTISEMENT) {
                Variable commonAdvertisementBottleneck =
                    findAdvertisementBottleneck(from.child, into.child, into.label);
                    
                if (commonAdvertisementBottleneck != null) {
                    if (into.getComponentHead() != commonAdvertisementBottleneck) {
                        notifyComponentInstancesDirty(w, into);
                        into.child = commonAdvertisementBottleneck;
                    }
                } else {
                    Variable child = new Variable(w);
                    VarData data = new VarData(cluster);
                    
                    child.setData(w, data);
                    into.setReal(child);
                    
                    notifyComponentSourcesDirty(w, into);
                    notifyComponentInstancesDirty(w, into);
                    fromData.notifyComponentSourcesDirty(w, into);
                    fromData.notifyComponentInstancesDirty(w, into);
                    
                    data.addParent(w, into);
                    w.notifyNewComponent(child, into.getParentHead());
                }
            } else {
                Variable fromChildHead = from.getComponentHead();
                VarData childData = fromChildHead.getData();
                
                childData.removeParent(w, from);
                into.setReal(fromChildHead);
                
                notifyComponentSourcesDirty(w, from);
                notifyComponentInstancesDirty(w, from);
                
                into.addModes(from.getModes());
                into.addModeInstances(from.getModeInstances());
                childData.addParent(w, into);
            }
        } else if (from.getKind() == ComponentRecord.ADVERTISEMENT) {
            fromData.notifyComponentSourcesDirty(w, into);
            fromData.notifyComponentInstancesDirty(w, into);
        } else {
            Variable fromChildHead = from.getComponentHead();
            
            addVarsToMakeEqual(varsToMakeEqual, into.getComponentHead(), fromChildHead);

            if (into.willAddModes(from.getModes())) {
                notifyComponentInstancesDirty(w, into);
                into.addModes(from.getModes());
            }
            if (from.willAddModes(into.getModes())) {
                fromData.notifyComponentInstancesDirty(w, into);
            }
            
            if (into.willAddModeInstances(from.getModeInstances())) {
                notifyComponentSourcesDirty(w, into);
                into.addModeInstances(from.getModeInstances());
            }
            if (from.willAddModeInstances(into.getModeInstances())) {
                fromData.notifyComponentSourcesDirty(w, into);
            }
            
            fromChildHead.getData().removeParent(w, from);
        }
    }
    
    private static void makeVarsEqual(World w, Vector varsToMakeEqual) {
        for (Enumeration e = varsToMakeEqual.elements(); e.hasMoreElements();) {
            Variable v1 = (Variable)e.nextElement();
            Variable v2 = (Variable)e.nextElement();
            
            v1.makeEqual(w, v2);
        }
    }
    
    private void mergeInstances(World w, Vector varsToMakeEqual, VarData data, Variable vHead) {
        VarData mergeToInstantiations;
        Enumeration mergeFromInstantiations;
        
        if (data.countInstances(w) > countInstances(w)) {
            mergeToInstantiations = data;
            mergeFromInstantiations = enumerateInstances(w);
        } else {
            mergeToInstantiations = this;
            mergeFromInstantiations = data.enumerateInstances(w);
        }

        for (; mergeFromInstantiations.hasMoreElements();) {
            InstantiationRecord record = (InstantiationRecord)
                mergeFromInstantiations.nextElement();
            InstantiationRecord to = mergeToInstantiations.getInstance(w, record.label);
                
            if (to != null) {
                Variable recordToHead = record.getInstanceHead();
                
                addVarsToMakeEqual(varsToMakeEqual, recordToHead, to.getInstanceHead());
                record.updatePolarity(to.getPolarity());
                
                recordToHead.getData().removeSource(w, record);
            } else {
                mergeToInstantiations.addInstanceUnconditionally(w, record);
            }
        }
        
        instances = mergeToInstantiations.instances;
    }
    
    private void mergeComponents(Vector varsToMakeEqual, World w, VarData data, Variable vHead) {
        VarData mergeToComponents;
        VarData mergeFromComponents;
        
        if (countComponents() < data.countComponents()) {
            mergeToComponents = data;
            mergeFromComponents = this;
        } else {
            mergeToComponents = this;
            mergeFromComponents = data;
        }

        for (Enumeration e = mergeFromComponents.enumerateComponents(); e.hasMoreElements();) {
            ComponentRecord record = (ComponentRecord)e.nextElement();
            Object obj = mergeToComponents.getComponent(record.label);
            
            if (obj != null) {
                mergeToComponents.mergeComponentPair(w, mergeFromComponents, varsToMakeEqual, (ComponentRecord)obj, record);
            } else {
                mergeToComponents.notifyComponentSourcesDirty(w, record);
                mergeToComponents.notifyComponentInstancesDirty(w, record);
                mergeToComponents.addComponentUnconditionally(record);
            }
        }
        
        for (Enumeration e = mergeToComponents.enumerateComponents(); e.hasMoreElements();) {
            ComponentRecord record = (ComponentRecord)e.nextElement();
            
            if (mergeFromComponents.getComponent(record.label) == null) {
                mergeFromComponents.notifyComponentSourcesDirty(w, record);
                mergeFromComponents.notifyComponentInstancesDirty(w, record);
            }
        }
        
        components = mergeToComponents.components;
    }
    
    private void mergeSources(World w, VarData d) {
        if (countSources(w) < d.countSources(w)) {
            for (Enumeration e = enumerateSources(w); e.hasMoreElements();) {
                d.addSource(w, (InstantiationRecord)e.nextElement());
            }
            
            sources = d.sources;
        } else {
            for (Enumeration e = d.enumerateSources(w); e.hasMoreElements();) {
                addSource(w, (InstantiationRecord)e.nextElement());
            }
        }
    }
    
    boolean swapMergingVariables(VarData mergingFrom) {
        return false;
    }
    
/**
Merges two sets of variable data. The information in "data" is moved into
"this". "data" must <b>not<\b> be referenced by any head variables.
*/
    void mergeWith(World w, VarData data, Variable vHead, Variable dataHead) {
        /* this is used to save the variables that we want to make
           equal. For sanity, we do all these operations after we've
           finished bringing 'this' into a consistent state.
           It's a Vector of Variables. They occur in pairs: the 2n'th
           element is made equal to the 2n+1'th element. */
        Vector varsToMakeEqual = new Vector();
        
        if (Globals.debug && data == this) {
            Globals.localError("Cannot merge with self!!!");
        }
        
        if (applyGlobal && (isGlobal() || data.isGlobal())) {
            setGlobal(w, vHead, varsToMakeEqual);
            data.setGlobal(w, vHead, varsToMakeEqual);
        }
        
        if (data.isQueried()) {
            setQueried();
        }
        
        validateSources(w, false);
        data.validateSources(w, false);
        validateInstances(w, false);
        data.validateInstances(w, false);
        validateAllInstanceSources(w);
        data.validateAllInstanceSources(w);
        validateAllSourceInstances(w);
        data.validateAllSourceInstances(w);
        
        validateAllComponentsDirty(w, data);
        validateAllComponentsDirty(w, this);
        data.validateAllComponentsDirty(w, this);
        data.validateAllComponentsDirty(w, data);
        
        dataHead.bindTo(vHead);
        
        // NO NEW VALIDATIONS SHOULD OCCUR FROM HERE UNTIL NOTED BELOW
        
        /* merge component-parents-of-this */
        /* This must come before we merge components. Component merging assumes that the
           parent lists are in a consistent state; in particular, it assumes that the parent
           lists for the variable we're merging to, contain the merged parents. */
        mergeParents(w, data);
        
        /* merge components-of-this */
        /* Any components in one variable not in the other may require invalidation
           to propagate advertisements. */
        if (components == null) {
            components = data.components;
            notifyAllComponentsDirty(w, this);
        } else if (data.components == null) {
            notifyAllComponentsDirty(w, data);
        } else {
            mergeComponents(varsToMakeEqual, w, data, vHead);
        }
        
        /* merge instantiations-to-this */
        /* This must come before we merge instances. Instance merging assumes that the
           source lists are in a consistent state; in particular, it assumes that the source
           lists for the variable we're merging to contain the merged sources. */
        mergeSources(w, data);
        
        /* merge instantiations-from-this */
        if (instances == null) {
            instances = data.instances;
        } else if (data.instances == null) {
        } else {
            mergeInstances(w, varsToMakeEqual, data, vHead);
        }
        
        // NEW VALIDATIONS CAN OCCUR AGAIN
        
        if (cluster != null) { /* the merged variable is not global */
            cluster.mergeWith(w, data.cluster);
        }
        
        data.parents = null;
        data.sources = null;
        data.components = null;
        data.instances = null;
        
        /* now that this object is in a consistent state, do all the
           leftover work. */
        makeVarsEqual(w, varsToMakeEqual);
    }
    
    private InstantiationRecord makeNewInstantiation(World w, Variable vHead,
        InstanceLabel label, Variable instanceHead, int modes) {
        VarData data;
            
        if (instanceHead == null) {
            instanceHead = new Variable(w);
            data = instanceHead.getData();
            
            data.hintFixedArity(this);
        } else {
            data = instanceHead.getData();
        }
        
        data.validateSources(w, true);
        
        InstantiationRecord record =
            new InstantiationRecord(vHead, instanceHead, label, modes);
        
        for (Enumeration e = enumerateComponents(); e.hasMoreElements();) {
            ComponentRecord r = (ComponentRecord)e.nextElement();
            
            w.notifyComponentInstancesDirty(r, new SingletonEnumerator(record));
            if (World.performCaching) {
                validateDirtyInstances(w, r, new SingletonEnumerator(record), true);
            }
        }
        for (Enumeration e = data.enumerateComponents(); e.hasMoreElements();) {
            ComponentRecord r = (ComponentRecord)e.nextElement();
            
            w.notifyComponentSourcesDirty(r, new SingletonEnumerator(record));
            if (World.performCaching) {
                validateDirtySources(w, r, new SingletonEnumerator(record), true);
            }
        }
        
        data.addSource(w, record);
        w.notifyNewInstance(instanceHead, vHead);
        
        if (data.cluster != null) {
            data.cluster.setInstanceOf(w, cluster);
        }
        
        return record;
    }
    
    Variable getInstance(World w, int modes, Variable v, InstanceLabel label) {
        return setInstance(w, v, label, null, null, null, modes);
    }

    Variable setInstance(World w, Variable v, InstanceLabel label,
        Variable instance, Vector varsToMakeEqual, boolean[] isNewInstance,
        int modes) {
        Variable instanceHead = instance != null ? instance.getHead() : null;
        InstantiationRecord curRecord = getInstance(w, label);
        
        validateInstances(w, true);
        
        if (curRecord == null) {
            if (applyGlobal && isGlobal()) {
                if (isNewInstance != null) {
                    isNewInstance[0] = false;
                }
                
                if (instanceHead != null) {
                    addVarsToMakeEqual(varsToMakeEqual, v, instanceHead);
                }
                
                return v;
            } else {
                InstantiationRecord record = makeNewInstantiation(w, v.getHead(), label, instanceHead, modes);
                
                if (isNewInstance != null) {
                    isNewInstance[0] = true;
                }
                
                addInstanceUnconditionally(w, record);
                
                return record.to;
            }
        } else {
            if (isNewInstance != null) {
                isNewInstance[0] = false;
            }

            curRecord.updatePolarity(modes);
            
            if (instanceHead != null) {
                Variable toHead = curRecord.getInstanceHead();
                
                addVarsToMakeEqual(varsToMakeEqual, toHead, instanceHead);
                return toHead;
            } else {
                return curRecord.to;
            }
        }
    }
    
    static void fillInAdvertisement(World w, ComponentRecord r) {
        Variable child = new Variable(w);
        Variable rParentHead = r.getParentHead();
        VarData rParentHeadData = rParentHead.getData();
        VarData data = new VarData(rParentHeadData.cluster);
        
        child.setData(w, data);
        r.setReal(child);
        
        rParentHeadData.notifyComponentSourcesDirty(w, r);
        rParentHeadData.notifyComponentInstancesDirty(w, r);
        
        data.addParent(w, r);
        w.notifyNewComponent(child, rParentHead);
    }
    
    static private void pushAdvertisementUp(World w, Variable baseHead, Variable vHead, ComponentLabel l,
        int modes, InstantiationRecord instantiation) {
        Variable instantiationToHead = instantiation.getInstanceHead();
        VarData instantiationToHeadData = instantiationToHead.getData();
        ComponentRecord r = instantiationToHeadData.accessComponent(l, instantiationToHead);
        
        switch (r.getKind()) {
            case ComponentRecord.BLANK:
                r.setAdvertisement(baseHead);
                
                for (Enumeration e = instantiationToHead.getData().enumerateInstancesSafely(w); e.hasMoreElements();) {
                    pushAdvertisementUp(w, baseHead, instantiationToHead, l, modes, (InstantiationRecord)e.nextElement());
                }
                break;
            case ComponentRecord.ADVERTISEMENT: {
                Variable commonAdvertismentBottleneck =
                    findAdvertisementBottleneck(baseHead, r.child, l);
                
                if (commonAdvertismentBottleneck != null) {
                    if (r.getComponentHead() != commonAdvertismentBottleneck) {
                        instantiationToHeadData.notifyComponentInstancesDirty(w, r);
                        r.child = commonAdvertismentBottleneck;
                    }
                } else {
                    fillInAdvertisement(w, r);
                    r.getComponentHead().getData().hintFixedArity(vHead.getData());
                }
                break;
            }
            case ComponentRecord.COMPONENT: {
                /* we must make things dirty here because we're coming from an
                   advertisement, which may need to be filled in now that we've
                   detected a component. */
                   
                instantiationToHeadData.notifyComponentSourcesDirty(w, r);
                
                if (r.willAddModes(modes)) {
                    instantiationToHeadData.notifyComponentInstancesDirty(w, r);
                    r.addModes(modes);
                    r.addModeInstances(modes);
                }
                break;
            }
        }
    }
    
    static private void addVarsToMakeEqual(Vector varsToMakeEqual, Variable v1Head, Variable v2Head) {
        if (v1Head != v2Head) {
            varsToMakeEqual.addElement(v1Head);
            varsToMakeEqual.addElement(v2Head);
        }
    }

/**
Find all variables t0 such that (t0, tn) is an extended occurs violation,
(i.e. such that tn is (transitively) a component of t0 and tn is (transitively)
an instance of t0.)
All such variables are made equal to tn. tn-1 is also specified (it's known because
we only do the XOC when we're possibly in an infinite loop). tn-1 must be the last variable
before tn on the instance chain.

We use the component cluster information to speed things up. We have the
invariant that a variable belongs to the same cluster as its components: thus
t0 must be in the same cluster as tn. Also, we have the invariant that
the cluster level instance graph is acyclic; thus if we follow a chain of
instances to a different cluster level, we can never get back to the level of tn
and hence we can never find a t0 (which has the same cluster level as tn, of
course).

When the CONFIG variable "coalesceClusterLevelInstances" is true, then every
new instance that we find that's in the same cluster level is converted to an
equality. This will eliminate most polymorphism within a cluster level, but it
may be faster.
*/
    private static void doExtendedOccursCheck(World w, Variable tN, Variable tNMinusOne,
        Vector varsToMakeEqual) {
        Variable tNHead = tN.getHead();
        ComponentCluster tNCluster = tNHead.getData().cluster;

        // don't do anything if it's global
        if (tNCluster != null) {
            searchInstancesFrom(w, tNMinusOne.getHead(), tNHead, tNCluster,
                varsToMakeEqual, new IdentityCompactSet());
        }
    }

    private static boolean searchForComponent(Variable vHead, Variable searchHead,
        IdentityCompactSet visited) {
        if (visited.get(vHead) == null) {
            visited.addUnconditionally(vHead);
            
            if (vHead == searchHead) {
                return true;
            } else {
                for (Enumeration e = vHead.getData().enumerateTrueComponents(); e.hasMoreElements();) {
                    if (searchForComponent(
                        ((ComponentElement)e.nextElement()).getComponentHead(),
                        searchHead, visited)) {
                        return true;
                    }
                }
                
                return false;
            }
        } else {
            return false;
        }
    }

    private static boolean searchInstancesFrom(World w, Variable vHead, Variable searchForHead,
        ComponentCluster searchCluster, Vector varsToMakeEqual, IdentityCompactSet visited) {
        boolean makeVEqualWithSearchHead = false;
           
        if (visited.get(vHead) == null) {
            visited.addUnconditionally(vHead);
            
            VarData vData = vHead.getData();
            ComponentCluster vCluster = vData.cluster;
            
            if (vCluster.equalLevels(w, searchCluster)) {
                if (w.getCoalesceClusterLevelInstances()) {
                    addVarsToMakeEqual(varsToMakeEqual, vHead, searchForHead);
                } else if (vCluster.equals(searchCluster)) {
                    if (vHead == searchForHead) {
                        // make all variables in the cycle equal
                        makeVEqualWithSearchHead = true;
                    } else if (searchForComponent(vHead, searchForHead, new IdentityCompactSet())) {
                        addVarsToMakeEqual(varsToMakeEqual, vHead, searchForHead);
                    }
                }

                for (Enumeration e = vData.enumerateSources(w); e.hasMoreElements();) {
                    InstantiationRecord source = (InstantiationRecord)e.nextElement();
                    
                    if (searchInstancesFrom(w, source.getSourceHead(), searchForHead, searchCluster,
                        varsToMakeEqual, visited)) {
                        makeVEqualWithSearchHead = true;
                    }
                }
            }
            
            if (makeVEqualWithSearchHead) {
                addVarsToMakeEqual(varsToMakeEqual, vHead, searchForHead);
            }
        }
        
        return makeVEqualWithSearchHead;
    }
    
    private static Variable findRootHead(Variable v, ComponentLabel l) {
        Variable vHead = v.getHead();
        
        while (true) {
            ComponentRecord r = vHead.getData().getComponent(l);
        
            if (r.getKind() == ComponentRecord.COMPONENT) {
                return vHead;
            } else {
                vHead = r.getComponentHead();
            }
        }
    }
    
    private static int fillInAdvertisements(World w, ComponentLabel l,
        InstantiationRecord sourceInstance, Variable instanceHead,
        Vector varsToMakeEqual) {
        Variable vHead = sourceInstance.getSourceHead();
        VarData vData = vHead.getData();
        ComponentRecord r = vData.getComponent(l);
        
        if (r != null) {
            switch (r.getKind()) {
                case ComponentRecord.ADVERTISEMENT: {
                    fillInAdvertisement(w, r);
                    return 0;
                }
                case ComponentRecord.COMPONENT:
                    return r.getModes();
                default:
                    throw Globals.localError("Invalid component kind");
            }
        } else {
            return 0;
        }
    }
    
    static boolean modesMatch(int modes, int modeInstances) {
        return ((modes & (1 << Variable.CMODE)) != 0 && (modeInstances & (1 << Variable.DMODE)) != 0)
            || ((modes & (1 << Variable.DMODE)) != 0 && (modeInstances & (1 << Variable.CMODE)) != 0);
    }
    
    private static void pushHasModeInstancesDown(World w, Variable vHead,
        ComponentRecord vComponent, Vector varsToMakeEqual, InstantiationRecord source,
        boolean[] cachedFlag) {
        ComponentLabel l = vComponent.label;
        Variable sourceHead = source.getSourceHead();
        VarData sourceData = sourceHead.getData();
        ComponentRecord r = sourceData.getComponent(l);
            
        if (r != null && r.getKind() == ComponentRecord.COMPONENT) {
            int modeInstances = vComponent.getModeInstances();
            int rModeInstances = r.getModeInstances();
            
            if (r.willAddModeInstances(modeInstances)) {
                sourceData.notifyComponentSourcesDirty(w, r);
                r.addModeInstances(modeInstances);
            }
            
            if (modesMatch(r.getModes(), modeInstances)) {
                Variable vChild = vComponent.getComponentHead();
                Variable rChild = r.getComponentHead();
                boolean[] isNewInstance = cachedFlag;
                
                rChild.getData().setInstance(w, rChild, source.label, vChild, varsToMakeEqual, isNewInstance, computePolarity(r, source));
                
                if (isNewInstance[0]) {
                    if (applyExtendedOccursCheckToChildren) {
                        doExtendedOccursCheck(w, vChild, rChild, varsToMakeEqual);
                    } else {
                        doExtendedOccursCheck(w, vHead, sourceHead, varsToMakeEqual);
                    }
                } /* otherwise there was already an instance relationship there,
                     so no occurs check is required */
            }
        }
    }
    
    private static void pushUpInstanceData(World w, Variable vHead, ComponentRecord r,
        int modes, Vector varsToMakeEqual, InstantiationRecord instance) {
        Variable instanceHead = instance.getInstanceHead();
        VarData instanceHeadData = instanceHead.getData();
        ComponentLabel r_label = r.label;
        ComponentRecord instanceComponent = instanceHeadData.accessComponent(r_label, instanceHead);
        
        switch (instanceComponent.getKind()) {
            case ComponentRecord.BLANK:
                instanceComponent.setAdvertisement(vHead);
                for (Enumeration e = instanceHead.getData().enumerateInstancesSafely(w); e.hasMoreElements();) {
                    pushAdvertisementUp(w, vHead, instanceHead, r_label, modes, (InstantiationRecord)e.nextElement());
                }
                break;
            case ComponentRecord.ADVERTISEMENT: {
                Variable commonAdvertisementBottleneck =
                    findAdvertisementBottleneck(vHead, instanceComponent.child, r_label);
                
                if (commonAdvertisementBottleneck != null) {
                    if (instanceComponent.getComponentHead() != commonAdvertisementBottleneck) {
                        instanceHeadData.notifyComponentInstancesDirty(w, instanceComponent);
                        instanceComponent.child = commonAdvertisementBottleneck;
                    }
                } else {
                    fillInAdvertisement(w, instanceComponent);
                    instanceComponent.getComponentHead().getData().hintFixedArity(vHead.getData());
                }
                break;
            }
            case ComponentRecord.COMPONENT: {
                int instanceComponentModes = instanceComponent.getModes();
                
                if (instanceComponent.willAddModes(modes)) {
                    instanceHeadData.notifyComponentInstancesDirty(w, instanceComponent);
                    if (instanceComponent.willAddModeInstances(modes)) {
                        instanceHeadData.notifyComponentSourcesDirty(w, instanceComponent);
                        instanceComponent.addModeInstances(modes);
                    }
                    instanceComponent.addModes(modes);
                }
                
                if (r.willAddModeInstances(instanceComponentModes)) {
                    vHead.getData().notifyComponentSourcesDirty(w, r);
                    r.addModeInstances(instanceComponentModes);
                }
                
                if (modesMatch(modes, instanceComponent.getModeInstances())) {
                    Variable instanceChild = instanceComponent.getComponentHead();
                    Variable rChild = r.getComponentHead();
                    boolean[] isNewInstance = new boolean[1];
                        
                    /* rChild could be the current variable, but this wouldn't interfere with
                       the instance enumeration because the instance must already exist! */
                    rChild.getData().setInstance(w, rChild, instance.label, instanceChild, varsToMakeEqual, isNewInstance, computePolarity(r, instance));
                    
                    if (isNewInstance[0]) {
                        if (applyExtendedOccursCheckToChildren) {
                            doExtendedOccursCheck(w, instanceChild, rChild, varsToMakeEqual);
                        } else {
                            doExtendedOccursCheck(w, instanceHead, vHead, varsToMakeEqual);
                        }
                    } /* otherwise there was already an instance relationship there,
                         so no occurs check is required */
                }
            }
        }
    }
    
    static void cleanDirtyComponent(World w, ComponentRecord r, int flags, IdentityCompactSet sources, CompactSet instances) {
        Variable vHead = r.getParentHead();
        ComponentLabel r_label = r.label;
        VarData vData = vHead.getData();
        
/* r might not be in use any more, so we'd better get the latest version of
   any mutable data */
        r = vData.getComponent(r_label);
        
        if (r.getKind() != ComponentRecord.COMPONENT) {
            /* do advertisment processing */
            if (Globals.debug && r.getKind() != ComponentRecord.ADVERTISEMENT) {
                Globals.localError("Unknown component kind!");
            }
            
            if ((flags & ComponentRecord.INSTANCES_DIRTY) != 0) {
                Variable baseHead = r.getComponentHead();
                int modes = r.getModes();
                
                if (instances != null) {
                    for (Enumeration e = instances.elements(); e.hasMoreElements();) {
                        InstantiationRecord instance = vData.getInstance(w, (InstanceLabel)e.nextElement());
                        
                        if (instance != null) {
                            pushAdvertisementUp(w, baseHead, vHead, r_label, modes, instance);
                        }
                    }
                } else {
                    for (Enumeration e = vData.enumerateInstancesSafely(w); e.hasMoreElements();) {
                        pushAdvertisementUp(w, baseHead, vHead, r_label, modes, (InstantiationRecord)e.nextElement());
                    }
                }
            }

            return;
        }

        VarData rChildData = r.getComponentHead().getData();
        
        vData.validateSources(w, false);
        rChildData.validateSources(w, false);
        vData.validateInstances(w, false);
        rChildData.validateInstances(w, false);
            
        int oldModes = r.getModes();
        int modes = oldModes;
        Vector varsToMakeEqual = new Vector();
        
        if ((flags & ComponentRecord.SOURCES_DIRTY) != 0) {
            if (sources != null) {
                /* This is a map from source variables S to cut-through instances that take S to vHead */
                IdentityHashtable cutThroughSources = new IdentityHashtable();
                
                /* fill in advertisments and pull modes up from sources */
                for (Enumeration e = sources.elements(); e.hasMoreElements();) {
                    InstantiationRecord source = (InstantiationRecord)e.nextElement();
                    InstantiationRecord realSource = source.getSourceHead().getData().getInstance(w, source.label);
                    
                    if (realSource != null) {
                        modes |= fillInAdvertisements(w, r_label, realSource, vHead, varsToMakeEqual);
                    }
                }
                
                Enumeration e;
                
                vData.notifyComponentInstancesDirty(w, r);
                r.addModes(modes);
                if (r.willAddModeInstances(modes)) {
                    vData.notifyComponentSourcesDirty(w, r);
                    r.addModeInstances(modes);
                    e = vData.enumerateSourcesSafely(w);
                } else {
                    e = sources.elements();
                }
                
                boolean[] cachedFlag = new boolean[1];

                /* push instance mode data down, and create instances for matching components */
                for (; e.hasMoreElements();) {
                    InstantiationRecord source = (InstantiationRecord)e.nextElement();
                    InstantiationRecord realSource = source.getSourceHead().getData().getInstance(w, source.label);
                    
                    if (realSource != null) {
                        pushHasModeInstancesDown(w, vHead, r, varsToMakeEqual, realSource, cachedFlag);
                    }
                }
            } else {
                /* fill in advertisments and pull modes up from sources */
         /* This loop, and the pushHasModeInstancesDown loop below, is the performance bottleneck. */
                for (Enumeration e = vData.enumerateSourcesSafely(w); e.hasMoreElements();) {
                    modes |= fillInAdvertisements(w, r_label,
                        (InstantiationRecord)e.nextElement(), vHead, varsToMakeEqual);
                }
            
                if (r.willAddModes(modes)) {
                    vData.notifyComponentInstancesDirty(w, r);
                    vData.notifyComponentSourcesDirty(w, r);
                    r.addModes(modes);
                    r.addModeInstances(modes);
                }

                boolean[] cachedFlag = new boolean[1];

                /* push instance mode data down, and create instances for matching components */
                for (Enumeration e = vData.enumerateSourcesSafely(w); e.hasMoreElements();) {
                    pushHasModeInstancesDown(w, vHead, r, varsToMakeEqual, (InstantiationRecord)e.nextElement(), cachedFlag);
                }
            }
        }
        
        if (oldModes != modes || (flags & ComponentRecord.INSTANCES_DIRTY) != 0) {
            /* push modes up to instances and fill in advertisements there
               (adding new advertisements for missing components, and
               possibly filling in advertisements to form real components if
               we need to. */
            /* we may get new instance modes, in which case we invalidate ourselves
               so we can restart. */
            if (instances != null) {
                for (Enumeration e = instances.elements(); e.hasMoreElements();) {
                    InstantiationRecord instance = vData.getInstance(w, (InstanceLabel)e.nextElement());
                    
                    if (instance != null) {
                        pushUpInstanceData(w, vHead, r, modes, varsToMakeEqual, instance);
                    }
                }
            } else {
                for (Enumeration e = vData.enumerateInstancesSafely(w); e.hasMoreElements();) {
                    pushUpInstanceData(w, vHead, r, modes, varsToMakeEqual, (InstantiationRecord)e.nextElement());
                }
            }
        }
        
        makeVarsEqual(w, varsToMakeEqual);
    }
    
/**
This is really only for debugging purposes.
*/
    String getClusterID() {
        return cluster != null
            ? Globals.getHexID(cluster.getHead()) + " " + Globals.getHexID(cluster.getLevel().getHead())
            : "GLOBAL";
    }
    
/**
@return an Enumeration of InstanceElements
*/
    Enumeration getInstances(World w) {
        return enumerateInstances(w);
    }

/**
@return an Enumeration of ComponentElements
*/
    Enumeration getComponents(World w) {
        return enumerateTrueComponents();
    }

/**
@return an Enumeration of SourceElements
*/
    Enumeration getSources(World w) {
        return enumerateSources(w);
    }

/**
@return an Enumeration of ParentElements
*/
    Enumeration getParents(World w) {
        return enumerateParents();
    }
}
