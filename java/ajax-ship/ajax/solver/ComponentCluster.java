/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver;

import ajax.Globals;
import java.util.*;
import ajax.util.IdentityManager;

/**
Each Variable is assigned to a ComponentCluster. The invariant is maintained that
if V1 has a component V2, then V1 and V2 are in the same ComponentCluster OR
V2 is global.
*/
class ComponentCluster {
    private ComponentCluster parent = null;
    private ComponentClusterLevel level = null;
    
    ComponentCluster() {
    }

    ComponentCluster getHead() {
        if (parent != null) {
            if (Globals.debug && parent == this) {
                Globals.localError("Cycle in parenthood?");
            }
            
            ComponentCluster head = this;
            
            for (; head.parent != null; head = head.parent) {
            }
            
            ComponentCluster c = this;
            
            while (c != head) {
                ComponentCluster next = c.parent;
                
                c.parent = head;
                c = next;
            }
            
            return head;
        } else {
            return this;
        }
    }
    
    void mergeWith(World w, ComponentCluster c) {
        ComponentCluster head = getHead();
        ComponentCluster cHead = c.getHead();
        
        if (head != cHead) {
            head.parent = cHead;
            
            w.notifyClustersMerged(cHead, head);
            
            if (head.level != null) {
                if (cHead.level != null) {
                    cHead.level.mergeWith(w, head.level);
                } else {
                    cHead.level = head.level;
                }
                head.level = null;
            }
        }
    }
    
    public boolean equals(Object o) {
        if (o instanceof ComponentCluster) {
            ComponentCluster c = (ComponentCluster)o;
            
            return getHead() == c.getHead();
        } else {
            return false;
        }
    }
    
    public int hashCode() {
        return IdentityManager.getIdentityHashCode(getHead());
    }
    
    boolean equalLevels(World w, ComponentCluster c) {
        ComponentClusterLevel level = getHead().level;
        
        if (level == null) {
            return false;
        } else {
            ComponentClusterLevel level2 = c.getHead().level;
            
            if (level2 == null) {
                return false;
            } else {
                ComponentClusterLevel.makeConsistent(w);
                
                return level.getHead() == level2.getHead();
            }
        }
    }
    
    ComponentClusterLevel getLevel() {
        ComponentCluster head = getHead();
        
        if (head.level == null) {
            head.level = new ComponentClusterLevel();
        }
        
        return head.level;
    }   
    
    void setInstanceOf(World w, ComponentCluster c) {
        getLevel().setInstanceOf(w, c.getLevel());
    }
}
