/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver;

import ajax.Globals;
import java.util.*;
import ajax.util.*;

class ComponentClusterLevel {
    private static final boolean logDirtyDecisions = false;
    
    private ComponentClusterLevel parent = null;
/* incoming and outgoing instantiation edges. No self-edges are allowed. */
    private IdentityCompactSet prev = new IdentityCompactSet();
    private IdentityCompactSet next = new IdentityCompactSet();
    private int flags = 0;
    
    private static final int DIRTY                                = 0x1;
    private static final int HAS_DIRTY_COMPONENTS                 = 0x2;
    /* This flag is set when we know that a (transitive) source has some dirty component(s). */
    private static final int SOURCES_HAVE_SOME_DIRTY_COMPONENTS   = 0x4;
    /* This flag is set when we know that all (transitive) sources have no dirty components. */
    private static final int SOURCES_HAVE_NO_DIRTY_COMPONENTS     = 0x8;
    
    private static final Object VISITED = new String("VISITED");
    private static final Object ONSTACK = new String("ONSTACK");
    
    public static final int SOURCES_UNKNOWN = 0;
    public static final int SOURCES_DIRTY   = 1;
    public static final int SOURCES_CLEAN   = 2;
    
    private static int numWithDirty = 0;
    
    private boolean isDirty() {
        return (flags & DIRTY) != 0;
    }
    
    private void setDirty(boolean dirty) {
        if (dirty) {
            flags |= DIRTY;
        } else {
            flags &= ~DIRTY;
        }
    }
    
    void verifyCleanSources() {
        ComponentClusterLevel head = getHead();
        
        head.internalVerifyCleanSources(new IdentityCompactSet());
    }
    
    private void internalVerifyCleanSources(IdentityCompactSet visited) {
        visited.addUnconditionally(this);

        for (Enumeration e = prev.elements(); e.hasMoreElements();) {
            ComponentClusterLevel level = (ComponentClusterLevel)e.nextElement();
            
            if (visited.get(level) == null) {
                visited.addUnconditionally(level);
                
                if ((level.flags & HAS_DIRTY_COMPONENTS) != 0) {
                    Globals.localError("Expected cluster level to have clean sources, but it doesn't");
                } else {
                    level.internalVerifyCleanSources(visited);
                }
            }
        }
    }
    
    private void internalSetUnknownDirtyComponentsUnconditionally(World w) {
        w.notifyClusterLevelDirtySourceComponents(this, SOURCES_UNKNOWN);
        
        flags &= ~(SOURCES_HAVE_SOME_DIRTY_COMPONENTS | SOURCES_HAVE_NO_DIRTY_COMPONENTS);
        
        for (Enumeration e = next.elements(); e.hasMoreElements();) {
            ((ComponentClusterLevel)e.nextElement()).internalSetUnknownDirtyComponents(w);
        }
    }
    
    private void internalSetUnknownDirtyComponents(World w) {
        if ((flags & (SOURCES_HAVE_SOME_DIRTY_COMPONENTS | SOURCES_HAVE_NO_DIRTY_COMPONENTS)) != 0) {
            internalSetUnknownDirtyComponentsUnconditionally(w);
        }
    }
    
    /** WARNING: This does NOT find the head automatically */
    int getSourceDirtyComponents() {
        if ((flags & SOURCES_HAVE_SOME_DIRTY_COMPONENTS) != 0) {
            return SOURCES_DIRTY;
        } else if ((flags & SOURCES_HAVE_NO_DIRTY_COMPONENTS) != 0) {
            return SOURCES_CLEAN;
        } else {
            return SOURCES_UNKNOWN;
        }
    }
    
    /** WARNING: This does NOT find the head automatically */
    boolean hasDirtyComponents() {
        return (flags & HAS_DIRTY_COMPONENTS) != 0;
    }
    
    void setHasDirtyComponents(World w, boolean dirtyComponents) {
        ComponentClusterLevel head = getHead();
        
        if (((head.flags & HAS_DIRTY_COMPONENTS) != 0) != dirtyComponents) {
            head.setHasDirtyComponentsUnconditionally(w, dirtyComponents);
        }

        if (Globals.debug && w.getNumDirtyComponentClusterLevels() != numWithDirty) {
            Globals.localError("numWithDirty mismatch: " + w.getNumDirtyComponentClusterLevels()
                + ", " + numWithDirty);
        }
    }
    
    private void setHasDirtyComponentsUnconditionally(World w, boolean dirtyComponents) {
        if (dirtyComponents) {
            if (Globals.debug && (flags & HAS_DIRTY_COMPONENTS) == 0) {
                numWithDirty++;
            }
            flags |= HAS_DIRTY_COMPONENTS;
        } else {
            if (Globals.debug && (flags & HAS_DIRTY_COMPONENTS) != 0) {
                numWithDirty--;
            }
            flags &= ~HAS_DIRTY_COMPONENTS;
        }
        
        /* We may encounter cycles in the component cluster levels, since we do not
           necessarily have them in the "consistent" state. */
        IdentityCompactSet visited = new IdentityCompactSet();
        
        for (Enumeration e = next.elements(); e.hasMoreElements();) {
            ((ComponentClusterLevel)e.nextElement()).updateSourcesHaveDirtyComponents(w,
                dirtyComponents || (flags & SOURCES_HAVE_SOME_DIRTY_COMPONENTS) != 0, visited);
        }
    }
    
    void computeSourceDirtyComponents(World w) {
        getHead().internalComputeSourceDirtyComponents(w);
    }
    
    private void correctDirtyComponents(World w) {
        if ((flags & SOURCES_HAVE_NO_DIRTY_COMPONENTS) != 0) {
            w.notifyClusterLevelDirtySourceComponents(this, SOURCES_DIRTY);
            
            flags = (flags & ~SOURCES_HAVE_NO_DIRTY_COMPONENTS)
                | SOURCES_HAVE_SOME_DIRTY_COMPONENTS;

            for (Enumeration e = next.elements(); e.hasMoreElements();) {
                ((ComponentClusterLevel)e.nextElement()).correctDirtyComponents(w);
            }
        }
    }

/**
@return true iff this has a dirty component or one of its (transitive) sources does.
  Actually, it may "incorrectly" return false if we're in part of a cycle, but this is
  cleaned up by correctDirtyComponents later.
*/
    private boolean internalComputeSourceDirtyComponents(World w) {
        /* We may encounter cycles in the component cluster levels, since we do not
           necessarily have them in the "consistent" state. */
        if ((flags & SOURCES_HAVE_SOME_DIRTY_COMPONENTS) != 0) {
            return true;
        } else if ((flags & SOURCES_HAVE_NO_DIRTY_COMPONENTS) != 0) {
            return (flags & HAS_DIRTY_COMPONENTS) != 0;
        } else {
            boolean dirtyComponents = false;
            
            w.notifyClusterLevelDirtySourceComponents(this, SOURCES_CLEAN);
            
            flags |= SOURCES_HAVE_NO_DIRTY_COMPONENTS;
            
            for (Enumeration e = prev.elements(); e.hasMoreElements();) {
                if (((ComponentClusterLevel)e.nextElement()).internalComputeSourceDirtyComponents(w)) {
                    dirtyComponents = true;
                }
            }
            
            if (dirtyComponents) {
                correctDirtyComponents(w);
            }
            
            return dirtyComponents || (flags & HAS_DIRTY_COMPONENTS) != 0;
        }
    }
    
    private int updateSourcesHaveDirtyComponents(World w, boolean dirtyComponents, IdentityCompactSet visited) {
        if (visited.get(this) == null) {
            visited.addUnconditionally(this);
            
            if ((flags & SOURCES_HAVE_NO_DIRTY_COMPONENTS) != 0) {
                if (dirtyComponents) {
                    if (logDirtyDecisions) {
                        boolean foundDirty = false;
                        
                        for (Enumeration e = prev.elements(); !foundDirty && e.hasMoreElements();) {
                            ComponentClusterLevel level = (ComponentClusterLevel)e.nextElement();
                            
                            if ((level.flags & (HAS_DIRTY_COMPONENTS | SOURCES_HAVE_SOME_DIRTY_COMPONENTS)) != 0) {
                                Globals.writeLog(this, "Making "
                                    + Globals.getHexID(this)
                                    + " dirty from "
                                    + Globals.getHexID(level));
                                foundDirty = true;
                            }
                        }
                        
                        if (!foundDirty) {
                            Globals.localError("Updating with dirty, but no dirty predecessor found?");
                        }
                    }
                    
                    w.notifyClusterLevelDirtySourceComponents(this, SOURCES_DIRTY);
                    
                    flags = (flags & ~SOURCES_HAVE_NO_DIRTY_COMPONENTS)
                        | SOURCES_HAVE_SOME_DIRTY_COMPONENTS;
                    
                    if ((flags & HAS_DIRTY_COMPONENTS) == 0) {
                        int instanceFlags = 0;
                    
                        for (Enumeration e = next.elements(); e.hasMoreElements();) {
                            instanceFlags |= ((ComponentClusterLevel)e.nextElement()).updateSourcesHaveDirtyComponents(w, true, visited);
                        }
                        
                        if (instanceFlags == 0) {
                            w.notifyClusterLevelDirtySourceComponents(this, SOURCES_UNKNOWN);

                            flags &= ~SOURCES_HAVE_SOME_DIRTY_COMPONENTS;
                        }
                    }
                }
            } else if ((flags & SOURCES_HAVE_SOME_DIRTY_COMPONENTS) != 0) {
                if (!dirtyComponents) {
                    boolean componentsDirty = false;
                    
                    w.notifyClusterLevelDirtySourceComponents(this, SOURCES_CLEAN);
                    
                    flags = (flags & ~SOURCES_HAVE_SOME_DIRTY_COMPONENTS)
                        | SOURCES_HAVE_NO_DIRTY_COMPONENTS;
                    
                    for (Enumeration e = prev.elements(); !componentsDirty && e.hasMoreElements();) {
                        int sourceFlags = ((ComponentClusterLevel)e.nextElement()).flags;
                        
                        if ((sourceFlags & SOURCES_HAVE_SOME_DIRTY_COMPONENTS) != 0) {
                            componentsDirty = true;
                        } else if ((sourceFlags & SOURCES_HAVE_NO_DIRTY_COMPONENTS) == 0) {
                            Globals.localError("Invariant violated: all predecessors of known-dirty-state cluster levels must also be known-dirty-state");
                        }
                        
                        if ((sourceFlags & HAS_DIRTY_COMPONENTS) != 0) {
                            componentsDirty = true;
                        }
                    }
                    
                    if (componentsDirty) {
                        /* then there's really no change. */
                        w.notifyClusterLevelDirtySourceComponents(this, SOURCES_DIRTY);
                        
                        flags = (flags & ~SOURCES_HAVE_NO_DIRTY_COMPONENTS)
                            | SOURCES_HAVE_SOME_DIRTY_COMPONENTS;
                    } else if ((flags & HAS_DIRTY_COMPONENTS) == 0) {
                        int instanceFlags = 0;
                    
                        for (Enumeration e = next.elements(); e.hasMoreElements();) {
                            instanceFlags |= ((ComponentClusterLevel)e.nextElement()).updateSourcesHaveDirtyComponents(w, false, visited);
                        }
                        
                        if (instanceFlags == 0) {
                            w.notifyClusterLevelDirtySourceComponents(this, SOURCES_UNKNOWN);

                            flags &= ~SOURCES_HAVE_NO_DIRTY_COMPONENTS;
                        }
                    }
                }
            }
            
            visited.remove(this);
        }
        
        return flags & (SOURCES_HAVE_SOME_DIRTY_COMPONENTS | SOURCES_HAVE_NO_DIRTY_COMPONENTS);
    }

    ComponentClusterLevel() {
    }

    ComponentClusterLevel getHead() {
        if (parent != null) {
            ComponentClusterLevel head = parent.getHead();
            
            parent = head;
            return head;
        } else {
            return this;
        }
    }
    
    private void changeNext(ComponentClusterLevel from, ComponentClusterLevel to) {
        if (next.remove(from) == null) {
            Globals.localError("inconsistent pointer state");
        }
        
        next.add(to);
    }
    
    private void changePrev(ComponentClusterLevel from, ComponentClusterLevel to) {
        if (prev.remove(from) == null) {
            Globals.localError("inconsistent pointer state");
        }
        
        prev.add(to);
    }
    
    private static IdentityCompactSet mergeSets(IdentityCompactSet h1, IdentityCompactSet h2) {
        if (h1.size() > h2.size()) {
            IdentityCompactSet tmp = h1;
            
            h1 = h2;
            h2 = tmp;
        }
        
        for (Enumeration e = h1.elements(); e.hasMoreElements();) {
            h2.add(e.nextElement());
        }
        
        return h2;
    }
    
    void mergeWith(World w, ComponentClusterLevel c) {
        mergeWith(w, c, false);
    }
    
    private void mergeWith(World w, ComponentClusterLevel c, boolean fixingCycle) {
        ComponentClusterLevel head = getHead();
        ComponentClusterLevel cHead = c.getHead();
        
        if (head != cHead) {
            int mySize = head.prev.size() + head.next.size();
            int hisSize = cHead.prev.size() + cHead.next.size();
            
            if (mySize > hisSize) {
                ComponentClusterLevel tmp = cHead;
                
                cHead = head;
                head = tmp;
            }
            
            head.prev.remove(cHead);
            head.next.remove(cHead);
            cHead.prev.remove(head);
            cHead.next.remove(head);
            for (Enumeration e = head.prev.elements(); e.hasMoreElements();) {
                ((ComponentClusterLevel)e.nextElement()).changeNext(head, cHead);
            }
            for (Enumeration e = head.next.elements(); e.hasMoreElements();) {
                ((ComponentClusterLevel)e.nextElement()).changePrev(head, cHead);
            }
            
            head.parent = cHead;
            cHead.prev = mergeSets(cHead.prev, head.prev);
            cHead.next = mergeSets(cHead.next, head.next);
            head.prev = null;
            head.next = null;
            if (head.isDirty()) {
                cHead.setDirty(true);
            } else if (!cHead.isDirty()) {
                cHead.setDirty(true);
                w.makeComponentClusterLevelDirty(cHead);
            }
            
            cHead.internalSetUnknownDirtyComponentsUnconditionally(w);

            w.notifyClusterLevelsMerged(cHead, head, fixingCycle);
            
            cHead.setHasDirtyComponentsUnconditionally(w,
                (cHead.flags & HAS_DIRTY_COMPONENTS) != 0
                    || (head.flags & HAS_DIRTY_COMPONENTS) != 0);
                    
            if (Globals.debug && (head.flags & HAS_DIRTY_COMPONENTS) != 0) {
                numWithDirty--;
            }
                    
            if (Globals.debug && w.getNumDirtyComponentClusterLevels() != numWithDirty) {
                Globals.localError("numWithDirty mismatch: " + w.getNumDirtyComponentClusterLevels()
                    + ", " + numWithDirty);
            }
        }
    }
    
    static ComponentClusterLevel searchForCycles(ComponentClusterLevel cHead, IdentityHashtable visited,
        Vector clustersToMakeEqual) {
        Object o = visited.get(cHead);
        
        if (o != null) {
            if (o == VISITED) {
                return null;
            } else {
                return cHead;
            }
        } else {
            visited.put(cHead, ONSTACK);
            
            for (Enumeration e = cHead.prev.elements(); e.hasMoreElements();) {
                ComponentClusterLevel prevHead = (ComponentClusterLevel)e.nextElement();
                ComponentClusterLevel cycleRootHead = searchForCycles(prevHead, visited, clustersToMakeEqual);
                
                if (cycleRootHead != null && cycleRootHead != cHead) {
                    clustersToMakeEqual.addElement(cycleRootHead);
                    clustersToMakeEqual.addElement(cHead);
                    visited.put(cHead, VISITED);
                    return cycleRootHead;
                }
            }
            
            visited.put(cHead, VISITED);
            return null;
        }
    }

    static void makeConsistent(World w) {
        Enumeration e;
        
        while ((e = w.flushDirtyComponentClusterLevels()) != null && e.hasMoreElements()) {
            IdentityHashtable visited = new IdentityHashtable();
            Vector clustersToMakeEqual = new Vector();
            
            do {
                ComponentClusterLevel cHead = ((ComponentClusterLevel)e.nextElement()).getHead();
                
                cHead.setDirty(false);
                
                ComponentClusterLevel result = searchForCycles(cHead, visited, clustersToMakeEqual);
                
                if (Globals.debug && result != null) {
                    Globals.localError("searchForCycles should not return a cycle to the top level!");
                }
            } while (e.hasMoreElements());
            
            for (Enumeration makeEqual = clustersToMakeEqual.elements(); makeEqual.hasMoreElements();) {
                ComponentClusterLevel mergeTo = (ComponentClusterLevel)makeEqual.nextElement();
                ComponentClusterLevel mergeFrom = (ComponentClusterLevel)makeEqual.nextElement();
                
                mergeTo.mergeWith(w, mergeFrom, true);
            }
        }
    }
    
    void setInstanceOf(World w, ComponentClusterLevel c) {
        ComponentClusterLevel cHead = c.getHead();
        ComponentClusterLevel head = getHead();
        
        if (cHead != head && head.prev.get(cHead) == null) {
            head.prev.add(cHead);
            cHead.next.add(head);
            
            if (!cHead.isDirty()) {
                cHead.setDirty(true);
                w.makeComponentClusterLevelDirty(cHead);
            }
            
            if ((cHead.flags & (SOURCES_HAVE_SOME_DIRTY_COMPONENTS | SOURCES_HAVE_NO_DIRTY_COMPONENTS)) != 0) {
                head.updateSourcesHaveDirtyComponents(w,
                    (cHead.flags & (HAS_DIRTY_COMPONENTS | SOURCES_HAVE_SOME_DIRTY_COMPONENTS)) != 0,
                    new IdentityCompactSet());
            } else {
                head.internalSetUnknownDirtyComponents(w);
            }
        }
    }

    public int hashCode() {
        return IdentityManager.getIdentityHashCode(getHead());
    }
}
