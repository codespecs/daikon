/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver;

import java.util.*;
import ajax.Globals;
import ajax.util.*;

public class World {
    public static final boolean verifyLevelStrategy = false;
    public static final boolean performCaching = false; // CONFIG
    
    private Vector varObservers = new Vector();
    private Vector dirtyComponentClusterLevels = new Vector();
    private IdentityCompactSet varDataCachingSources = new IdentityCompactSet();
    private IdentityCompactSet varDataCachingInstances = new IdentityCompactSet();
    private IdentityHashtable varDataSourceCaches = new IdentityHashtable();
    private IdentityHashtable varDataInstanceCaches = new IdentityHashtable();
    private IdentityHashtable dirtySourceSets = new IdentityHashtable();
    private IdentityHashtable dirtyInstanceSets = new IdentityHashtable();
    private IdentityHashtable highPriorityClusterLevels = new IdentityHashtable();
    private IdentityHashtable lowPriorityClusterLevels = new IdentityHashtable();
    private Stack dirtyGlobalComponentStack = new Stack();
    private Stack delayedWork = new Stack();
    private CompactSet delayedWorkIndex = new CompactSet();
    private boolean coalesceClusterLevelInstances = false;
    private boolean dumpOperations = false;
    private boolean checkConsistency = false;
    private boolean applyComponentModes = true;
    private boolean applyInstanceModes = true;
    
    private static String WILL_CACHE = new String("WILL_CACHE");
    private static String DONT_CACHE = new String("DONT_CACHE");
    private static String IS_CACHED = new String("IS_CACHED");
    
    private ComponentClusterLevel globalClusterLevel = new ComponentClusterLevel();
    
    public World() {
    }
    
    void notifyVarDataUpdatedInstances(VarData data) {
        varDataInstanceCaches.put(data, Boolean.TRUE);
    }
    
    void notifyVarDataUncachedInstances(VarData data) {
        if (varDataInstanceCaches.get(data) == null) {
            varDataInstanceCaches.put(data, Boolean.FALSE);
        }
        if (Globals.debug && isCheckingConsistency()) {
            varDataCachingInstances.remove(data);
        }
    }
    
    void notifyVarDataUpdatedSources(VarData data) {
        varDataSourceCaches.put(data, Boolean.TRUE);
    }
    
    void notifyVarDataUncachedSources(VarData data) {
        if (varDataSourceCaches.get(data) == null) {
            varDataSourceCaches.put(data, Boolean.FALSE);
        }
        if (Globals.debug && isCheckingConsistency()) {
            varDataCachingSources.remove(data);
        }
    }
    
    private void updateCache(IdentityHashtable cache, VarData data, IdentityHashtable state,
        boolean isInstances) {
        boolean anythingToCache = true;
	    // (isInstances ? data.countInstances(this) : data.countSources(this)) > 0;

        if (anythingToCache && data.countParents() == 1 && cache.get(data) != null) {
            Object curState = state.get(data);
                
            if (curState == null) {
                state.put(data, WILL_CACHE);
                
                for (Enumeration e = data.enumerateTrueComponents(); e.hasMoreElements();) {
                    updateCache(cache, ((ComponentRecord)e.nextElement()).getComponentHead().getData(),
                        state, isInstances);
                }
                
                if (state.get(data) == WILL_CACHE) {
                    boolean updated = ((Boolean)cache.get(data)).booleanValue();

                    if (isInstances) {
                        if (!updated || data.isInstanceInheritanceValid(this)) {
                            if (getDumpOperations()) {
                                Globals.writeLog(this, "Caching instances for "
                                    + Globals.getHexID(((ComponentRecord)data.enumerateParents().nextElement()).getComponentHead()));
                            }
                            data.cacheInstances(this, updated);
                            if (Globals.debug && isCheckingConsistency()) {
                                varDataCachingInstances.add(data);
                            }
                        }
                    } else {
                        if (!updated || data.isSourceInheritanceValid(this)) {
                            if (getDumpOperations()) {
                                Globals.writeLog(this, "Caching sources for "
                                    + Globals.getHexID(((ComponentRecord)data.enumerateParents().nextElement()).getComponentHead()));
                            }
                            data.cacheSources(this, updated);
                            if (Globals.debug && isCheckingConsistency()) {
                                varDataCachingSources.add(data);
                            }
                        }
                    }
                }
                
                state.put(data, IS_CACHED);
            } else if (curState == WILL_CACHE) {
                state.put(data, DONT_CACHE);
            }
        }
    }
    
    private void updateCache(IdentityHashtable cache, boolean isInstances) {
        IdentityHashtable state = new IdentityHashtable();
            
        for (Enumeration e = cache.keys(); e.hasMoreElements();) {
            VarData data = (VarData)e.nextElement();
            
            updateCache(cache, data, state, isInstances);
        }
    }
    
    private void updateCaches() {
        if (Globals.debug && isCheckingConsistency()) {
            for (Enumeration e = varDataCachingInstances.elements(); e.hasMoreElements();) {
                VarData data = (VarData)e.nextElement();
                
                if (getDumpOperations()) {
                    Globals.writeLog(this, "Checking instance inheritance for "
                        + Globals.getHexID(((ComponentRecord)data.enumerateParents().nextElement()).getComponentHead()));
                }
                if (!data.isInstanceInheritanceValid(this)) {
                    Globals.localError("Invalid instance inheritance for "
                        + Globals.getHexID(((ComponentRecord)data.enumerateParents().nextElement()).getComponentHead()));
                }
            }
            for (Enumeration e = varDataCachingSources.elements(); e.hasMoreElements();) {
                VarData data = (VarData)e.nextElement();

                if (getDumpOperations()) {
                    Globals.writeLog(this, "Checking source inheritance for "
                        + Globals.getHexID(((ComponentRecord)data.enumerateParents().nextElement()).getComponentHead()));
                }
                if (!data.isSourceInheritanceValid(this)) {
                    Globals.localError("Invalid source inheritance for "
                        + Globals.getHexID(((ComponentRecord)data.enumerateParents().nextElement()).getComponentHead()));
                }
            }
            varDataCachingInstances = new IdentityCompactSet();
            varDataCachingSources = new IdentityCompactSet();
        }
        
        IdentityHashtable tmp = varDataSourceCaches;
        
        varDataSourceCaches = new IdentityHashtable();
        updateCache(tmp, false);
        
        tmp = varDataInstanceCaches;
        
        varDataInstanceCaches = new IdentityHashtable();
        updateCache(tmp, true);
    }
    
    public void setDumpOperations(boolean dumpOperations) {
        this.dumpOperations = dumpOperations;
    }
    
    public void setCoalesceClusterLevelInstances(boolean coalesceClusterLevelInstances) {
        this.coalesceClusterLevelInstances = coalesceClusterLevelInstances;
    }
    
    public boolean getDumpOperations() {
        return dumpOperations;
    }
    
    public boolean getCoalesceClusterLevelInstances() {
        return coalesceClusterLevelInstances;
    }
    
    public boolean setApplyComponentModes(boolean b) {
        return b;
    }
    
    public boolean getApplyComponentModes() {
        return applyComponentModes;
    }
    
    public boolean setApplyInstanceModes(boolean b) {
        return b;
    }
    
    public boolean getApplyInstanceModes() {
        return applyInstanceModes;
    }
    
    public void setCheckConsistency(boolean b) {
        checkConsistency = b;
    }
    
    public boolean isCheckingConsistency() {
        return checkConsistency;
    }
    
    int getNumDirtyComponentClusterLevels() {
        return highPriorityClusterLevels.size() + lowPriorityClusterLevels.size()
            + (!dirtyGlobalComponentStack.isEmpty() ? 1 : 0);
    }
    
    private IdentityHashtable getDirtyComponentTable(ComponentClusterLevel level) {
        if (level == globalClusterLevel) {
            return null;
        } else {
            switch (level.getSourceDirtyComponents()) {
                case ComponentClusterLevel.SOURCES_UNKNOWN:
                case ComponentClusterLevel.SOURCES_DIRTY:
                    return lowPriorityClusterLevels;
                case ComponentClusterLevel.SOURCES_CLEAN:
                    return highPriorityClusterLevels;
                default:
                    throw Globals.localError("Invalid source-dirty-components state");
            }
        }
    }
    
    private void addDirtyComponent(ComponentRecord r) {
        ComponentCluster cluster = r.getParentHead().getData().getCluster();
        ComponentClusterLevel level = cluster != null ? cluster.getLevel().getHead() : globalClusterLevel;
        IdentityHashtable table = getDirtyComponentTable(level);
        
        if (table != null) {
            Stack s = (Stack)table.get(level);
            
            if (s == null) {
                s = new Stack();
                
                if (Globals.debug && level.getHead() != level) {
                    Globals.localError("Level in table is not a head!");
                }
                table.put(level, s);
                
                level.setHasDirtyComponents(this, true);
            }
            s.push(r);
        } else {
            Stack s = dirtyGlobalComponentStack;
            boolean wasEmpty = s.isEmpty(); 
            
            s.push(r);
            if (wasEmpty) {
                level.setHasDirtyComponents(this, true);
            }
        }
    }
    
    private static int countElements(Enumeration e) {
        int count = 0;
        
        while (e.hasMoreElements()) {
            count++;
            e.nextElement();
        }
        return count;
    }
    
    private CompactSet setupComponentInstances(ComponentRecord r) {
        if (!internalNotifyComponentDirty(r, ComponentRecord.INSTANCES_DIRTY)) {
            CompactSet componentInstances = new CompactSet();
            
            dirtyInstanceSets.put(r, componentInstances);
            
            return componentInstances;
        } else {
            return (CompactSet)dirtyInstanceSets.get(r);
        }
    }
    
    private IdentityCompactSet setupComponentSources(ComponentRecord r) {
        if (!internalNotifyComponentDirty(r, ComponentRecord.SOURCES_DIRTY)) {
            IdentityCompactSet componentSources = new IdentityCompactSet();
            
            dirtySourceSets.put(r, componentSources);
            
            return componentSources;
        } else {
            return (IdentityCompactSet)dirtySourceSets.get(r);
        }
    }
    
    void notifyComponentInstancesDirty(ComponentRecord r, Enumeration e) {
        if (e.hasMoreElements()) {
            CompactSet componentInstances = setupComponentInstances(r);

            if (componentInstances != null) {
                int count = 0;
                
                while (e.hasMoreElements()) {
                    InstantiationRecord instance = (InstantiationRecord)e.nextElement();
                    
                    count++;
                    
                    if (Globals.debug && instance.getSourceHead() != r.getParentHead()) {
                        Globals.localError("Mismatched instance with component!");
                    }
                    
                    componentInstances.add(instance.label);
                }
                
                writeDirtyNotificationLog(r, 0, count);
                
                pruneInstanceSet(r, componentInstances);
            } else if (getDumpOperations()) {
                writeDirtyNotificationLog(r, 0, countElements(e));
            }
        }
    }
    
    private void pruneInstanceSet(ComponentRecord r, CompactSet componentInstances) {
        if (componentInstances.size() > 50) {
            dirtyInstanceSets.remove(r);
        }
    }
    
    private void pruneSourceSet(ComponentRecord r, IdentityCompactSet componentSources) {
        if (componentSources.size() > 50) {
            dirtySourceSets.remove(r);
        }
    }
    
    void notifyComponentSourcesDirty(ComponentRecord r, Enumeration e) {
        if (e.hasMoreElements()) {
            IdentityCompactSet componentSources = setupComponentSources(r);

            if (componentSources != null) {
                int count = 0;
                
                while (e.hasMoreElements()) {
                    InstantiationRecord source = (InstantiationRecord)e.nextElement();
                    
                    count++;
                    
                    if (Globals.debug && source.getInstanceHead() != r.getParentHead()) {
                        Globals.localError("Mismatched source with component!");
                    }
                    componentSources.add(source);
                }
                
                writeDirtyNotificationLog(r, count, 0);
                pruneSourceSet(r, componentSources);
            } else if (getDumpOperations()) {
                writeDirtyNotificationLog(r, countElements(e), 0);
            }
        }
    }
    
    private static String getCountString(String label, int num) {
        if (num == 0) {
            return "";
        } else if (num == Integer.MAX_VALUE) {
            return label + "ALL";
        } else {
            return label + num;
        }
    }
    
    private String makeComponentString(ComponentRecord r) {
        return Globals.getHexID(r.getParentHead())
            + " " + Globals.getHexID(r.getComponentHead())
            + " (" + StringUtils.abbreviate(r.label.toString()) + ", " + r.toString() + ") "
            + (r.getKind() == ComponentRecord.ADVERTISEMENT ? "A" : "C");
    }
    
    private void writeDirtyNotificationLog(ComponentRecord r, int sources,
        int instances) {
        if (getDumpOperations()) {
            Globals.writeLog(this, "D " + makeComponentString(r)
                + getCountString(" I=", instances)
                + getCountString(" S=", sources));
        }
    }
    
/**
@return true if the component was already dirty in at least one of the given mode(s)
*/
    private boolean internalNotifyComponentDirty(ComponentRecord r, int flags) {
        int curDirty = r.getDirtyFlags();
        
        if (curDirty == 0) {
            r.setDirtyFlags(flags);
            addDirtyComponent(r);
            return false;
        } else {
            r.setDirtyFlags(flags | curDirty);
            return (flags & curDirty) != 0;
        }
    }
    
    void mergeComponentDirtyState(ComponentRecord to, ComponentRecord from) {
        int fromDirty = from.getDirtyFlags();
        
        if (fromDirty != 0) {
            from.setDirtyFlags(0);
            
            CompactSet instances = (CompactSet)dirtyInstanceSets.remove(from);
            IdentityCompactSet sources = (IdentityCompactSet)dirtySourceSets.remove(from);
            
            if (Globals.debug && instances != null && (fromDirty & ComponentRecord.INSTANCES_DIRTY) == 0) {
                Globals.localError("Component has dirty instance set, but its instances aren't dirty!");
            }
            if (Globals.debug && sources != null && (fromDirty & ComponentRecord.SOURCES_DIRTY) == 0) {
                Globals.localError("Component has dirty source set, but its sources aren't dirty!");
            }
            
            if ((fromDirty & ComponentRecord.INSTANCES_DIRTY) != 0) {
                if (instances == null) {
                    if (internalNotifyComponentDirty(to, ComponentRecord.INSTANCES_DIRTY)) {
                        dirtyInstanceSets.remove(to);
                    }
                } else {
                    CompactSet componentInstances = setupComponentInstances(to);

                    if (componentInstances != null) {
                        if (componentInstances.size() < instances.size()) {
                            CompactSet tmp = componentInstances;
                            
                            componentInstances = instances;
                            dirtyInstanceSets.put(to, componentInstances);
                            instances = tmp;
                        }
                        
                        for (Enumeration e = instances.elements(); e.hasMoreElements();) {
                            componentInstances.add(e.nextElement());
                        }
                    }
                }
            }

            if ((fromDirty & ComponentRecord.SOURCES_DIRTY) != 0) {
                if (sources == null) {
                    if (internalNotifyComponentDirty(to, ComponentRecord.SOURCES_DIRTY)) {
                        dirtySourceSets.remove(to);
                    }
                } else {
                    IdentityCompactSet componentSources = setupComponentSources(to);

                    if (componentSources != null) {
                        if (componentSources.size() < sources.size()) {
                            IdentityCompactSet tmp = componentSources;
                            
                            componentSources = sources;
                            dirtySourceSets.put(to, componentSources);
                            sources = tmp;
                        }
                        
                        for (Enumeration e = sources.elements(); e.hasMoreElements();) {
                            componentSources.add(e.nextElement());
                        }
                    }
                }
            }
        }
    }
    
    private static Stack mergeStacks(Stack s1, Stack s2) {
        if (s1 == null) {
            return s2;
        } else if (s2 == null) {
            return s1;
        } else {
            if (s1.size() < s2.size()) {
                Stack tmp = s1;
                
                s1 = s2;
                s2 = tmp;
            }
            
            while (!s2.isEmpty()) {
                s1.push(s2.pop());
            }
            
            return s1;
        }
    }
    
    void notifyClustersMerged(ComponentCluster to, ComponentCluster from) {
        if (getDumpOperations()) {
            Globals.writeLog(this, "S " + Globals.getHexID(to)
                + "<-" + Globals.getHexID(from));
        }
    }
    
    void notifyClusterLevelsMerged(ComponentClusterLevel to, ComponentClusterLevel from,
        boolean fixingCycle) {
        if (getDumpOperations()) {
            Globals.writeLog(this, "L " + Globals.getHexID(to)
                + "<-" + Globals.getHexID(from) + (fixingCycle ? " (cycle)" : ""));
        }
        
        if (Globals.debug && (to == globalClusterLevel || from == globalClusterLevel)) {
            Globals.localError("Cannot merge the global cluster level!");
        }
        
        IdentityHashtable toTable = getDirtyComponentTable(to);
        IdentityHashtable fromTable = getDirtyComponentTable(from);
        Stack s1 = (Stack)toTable.get(to);
        Stack s2 = (Stack)fromTable.get(from);
        
        if (Globals.debug && from.hasDirtyComponents() != (s2 != null)) {
            Globals.localError("From level status does not match table entry existence");
        }
        
        Stack result = mergeStacks(s1, s2);
        
        if (result != null) {
            if (Globals.debug && to.getHead() != to) {
                Globals.localError("Level in table is not a head!");
            }
            toTable.put(to, result);
            fromTable.remove(from);
        }
    }
    
    private static String getStatusString(int status) {
        switch (status) {
            case ComponentClusterLevel.SOURCES_UNKNOWN: return "UNKNOWN";
            case ComponentClusterLevel.SOURCES_DIRTY:   return "DIRTY";
            case ComponentClusterLevel.SOURCES_CLEAN:   return "CLEAN";
            default: 
                throw Globals.localError("Invalid source-dirty-components state");
        }
    }
    
    void notifyClusterLevelDirtySourceComponents(ComponentClusterLevel level, int newStatus) {
        if (level != globalClusterLevel) {
            IdentityHashtable curTable = getDirtyComponentTable(level);
            IdentityHashtable newTable;
            
            switch (newStatus) {
                case ComponentClusterLevel.SOURCES_UNKNOWN:
                case ComponentClusterLevel.SOURCES_DIRTY:
                    newTable = lowPriorityClusterLevels;
                    break;
                case ComponentClusterLevel.SOURCES_CLEAN:
                    newTable = highPriorityClusterLevels;
                    break;
                default:
                    throw Globals.localError("Invalid source-dirty-components state");
            }
            
            if (Globals.debug && level.hasDirtyComponents() && curTable.get(level) == null) {
                Globals.localError("We lost a cluster level! WHOOP, WHOOP!");
            }
            
            if (curTable != newTable) {
                Object o = curTable.remove(level);
                
                // If the level is dirty, then o has the stack of dirty components for the level
                // If the level is not dirty, o is null
                if (o != null) {
                    if (Globals.debug && level.getHead() != level) {
                        Globals.localError("Level in table is not a head!");
                    }
                    newTable.put(level, o);
                }
            }
        }
    }
    
    Enumeration flushDirtyComponentClusterLevels() {
        if (dirtyComponentClusterLevels.size() == 0) {
            return null;
        } else {
            Enumeration result = dirtyComponentClusterLevels.elements();
            
            dirtyComponentClusterLevels = new Vector();
            
            return result;
        }
    }
    
    void notifyGlobalComponent(ComponentRecord r) {
        // We don't need to do anything at the moment.
        // It may be that r is dirty, and therefore on the dirty component stack of
        // some component cluster level, where it doesn't belong.
        // That's too bad. Removing it from the stack would be ugly and inefficient.
        // It may get cleaned ahead of its time, that's all.
    }
    
    void makeComponentClusterLevelDirty(ComponentClusterLevel c) {
        dirtyComponentClusterLevels.addElement(c);
    }
    
    private ComponentRecord grabDirtyComponentToClean() {
        if (highPriorityClusterLevels.size() == 0) {
            ComponentClusterLevel.makeConsistent(this);
            
            if (highPriorityClusterLevels.size() == 0) {
                if (lowPriorityClusterLevels.size() == 0) {
                    if (dirtyGlobalComponentStack.isEmpty()) {
                        return null;
                    } else {
                        ComponentRecord r = (ComponentRecord)dirtyGlobalComponentStack.pop();
                        
                        if (dirtyGlobalComponentStack.isEmpty()) {
                            globalClusterLevel.setHasDirtyComponents(this, false);
                        }
                        
                        return r;
                    }
                } else {
                    ComponentClusterLevel level = (ComponentClusterLevel)lowPriorityClusterLevels
                        .keys().nextElement();
                    
                    level.computeSourceDirtyComponents(this);
                        
                    if (Globals.debug && highPriorityClusterLevels.size() == 0) {
                        Globals.localError("ClusterLevels are consistent but all dirty levels have unclean sources?");
                    }
                }
            }
        }

        ComponentClusterLevel level =
            (ComponentClusterLevel)highPriorityClusterLevels.keys().nextElement();
        Stack s = (Stack)highPriorityClusterLevels.get(level);
        ComponentRecord r = (ComponentRecord)s.pop();
        
        if (s.isEmpty()) {
            highPriorityClusterLevels.remove(level);
            level.setHasDirtyComponents(this, false);
        }
        
        if (verifyLevelStrategy) {
            level.verifyCleanSources();
        }
        
        return r;
    }
    
    private String getInstanceString(ComponentRecord r, int flags, CompactSet instances) {
        if ((flags & ComponentRecord.INSTANCES_DIRTY) == 0) {
            return "";
        } else {
            int fullCount = r.getParentHead().getData().countInstances(this);
            
            return " I=" + fullCount + ","
                + (instances != null ? instances.size() : fullCount);
        }
    }
    
    private static String printSourceVars(IdentityCompactSet sources) {
        StringBuffer buf = new StringBuffer();
        
        for (Enumeration e = sources.elements(); e.hasMoreElements();) {
            if (buf.length() > 0) {
                buf.append("|");
            }
            buf.append(Globals.getHexID(((InstantiationRecord)e.nextElement()).getSourceHead()));
        }
        
        return buf.toString();
    }
    
    private String getSourceString(ComponentRecord r, int flags, IdentityCompactSet sources) {
        if ((flags & ComponentRecord.SOURCES_DIRTY) == 0) {
            return "";
        } else {
            int fullCount = r.getParentHead().getData().countSources(this);
            String s = " S=" + fullCount + ",";
            
            if (sources == null) {
                return s + fullCount;
            } else {
                int size = sources.size();
                
                if (size < 5) {
                    return s + printSourceVars(sources);
                } else {
                    return s + size;
                }
            }
        }
    }
    
    private boolean cleanDirtyComponent() {
        ComponentRecord r;
        int dirtyFlags;
        
        do {
            r = grabDirtyComponentToClean();
            
            if (r == null) {
                return false;
            }
            
            dirtyFlags = r.getDirtyFlags();
        } while (dirtyFlags == 0);
        
        CompactSet instances = (CompactSet)dirtyInstanceSets.remove(r);
        IdentityCompactSet sources = (IdentityCompactSet)dirtySourceSets.remove(r);
        
        if (Globals.debug && instances != null && (dirtyFlags & ComponentRecord.INSTANCES_DIRTY) == 0) {
            Globals.localError("Component has dirty instance set, but its instances aren't dirty!");
        }
        if (Globals.debug && sources != null && (dirtyFlags & ComponentRecord.SOURCES_DIRTY) == 0) {
            Globals.localError("Component has dirty source set, but its sources aren't dirty!");
        }
        
        if (getDumpOperations()) {
            Globals.writeLog(this, "[ " + makeComponentString(r)
                + getInstanceString(r, dirtyFlags, instances)
                + getSourceString(r, dirtyFlags, sources));
        }
        
        r.setDirtyFlags(0);
        VarData.cleanDirtyComponent(this, r, dirtyFlags, sources, instances);

        if (getDumpOperations()) {
            Globals.writeLog(this, "] " + makeComponentString(r));
        }
        
        return true;
    }
    
    public void addVarObserver(VarObserver o) {
        varObservers.addElement(o);
    }
    
    public static IdentityCompactSet mergeSets(IdentityCompactSet set1, IdentityCompactSet set2) {
        if (set1.size() < set2.size()) {
            IdentityCompactSet tmp = set1;
            
            set1 = set2;
            set2 = tmp;
        }
        
        for (Enumeration e = set2.elements(); e.hasMoreElements();) {
            set1.add(e.nextElement());
        }
        
        return set1;
    }
    
    void notifyVarsMerged(Variable to, Variable from) {
        if (getDumpOperations()) {
            Globals.writeLog(this, "M " + Globals.getHexID(to) + "<-" + Globals.getHexID(from));
        }
        
        for (Enumeration e = varObservers.elements(); e.hasMoreElements();) {
            ((VarObserver)e.nextElement()).notifyVarsMerged(to, from);
        }
    }
    
    void addDelayedWork(DelayedWork d) {
        if (delayedWorkIndex.get(d) == null) {
            delayedWork.push(d);
            delayedWorkIndex.addUnconditionally(d);
        }
    }
    
    private void doDelayedWork() {
        while (!delayedWork.isEmpty()) {
            DelayedWork d = (DelayedWork)delayedWork.pop();
            
            delayedWorkIndex.remove(d);
            d.doWork(this);
        }
    }
    
    void notifyNewInstance(Variable instance, Variable of) {
        if (getDumpOperations()) {
            Globals.writeLog(this, "I " + Globals.getHexID(of) + "->" + Globals.getHexID(instance));
        }
        
        for (Enumeration e = varObservers.elements(); e.hasMoreElements();) {
            ((VarObserver)e.nextElement()).notifyNewInstance(instance, of);
        }
    }
    
    void notifyNewComponent(Variable component, Variable of) {
        if (getDumpOperations()) {
            Globals.writeLog(this, "C " + Globals.getHexID(of) + "->" + Globals.getHexID(component));
        }
        
        for (Enumeration e = varObservers.elements(); e.hasMoreElements();) {
            ((VarObserver)e.nextElement()).notifyNewComponent(component, of);
        }
    }
    
    private void checkSpecialObject() {
        Object o = IdentityManager.getSpecialObject();
        
        if (o instanceof Variable) {
            VarData data = ((Variable)o).getHead().getData();

            Globals.writeLog(this, "_SPECIAL instances: " + data.countInstances(this));
        }
    }
        
    public boolean work() {
        checkSpecialObject();
        
        boolean result = cleanDirtyComponent();

        checkSpecialObject();

        doDelayedWork();
        if (performCaching) {
            updateCaches();
        }
        
        checkSpecialObject();
        
        return result;
    }
}
