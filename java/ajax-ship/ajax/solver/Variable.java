/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver;

import ajax.Globals;
import java.util.*;
import ajax.util.IdentityManager;

/**
This class defines variables in the constraint system. They are the main interface
used to define a system of constraints. Constraints are established using the
getComponent, getInstance and makeEqual methods.
*/
public class Variable {
    private Variable boundTo = null;
    private VarData data = null;

/** This flag is used when a component constraint is added in "Constructor Mode". */
    public static final int CMODE = 0;
/** This flag is used when a component constraint is added in "Destructor Mode". */
    public static final int DMODE = 1;
    
    public static final int PMODE = 0;
    public static final int NMODE = 1;

/**
Create a fresh Variable.

@param w the constraint system that the variable belongs to.
*/
    public Variable(World w) {
        if (w.getDumpOperations()) {
            data = new VarData();
            
            Globals.writeLog(this, "B " + Globals.getHexID(this)
                + " " + getClusterID());
        }
    }

/**
Get the canonical version of a Variable. This will return a Variable that
is known to be equal to 'this' according to the constraint system. Some
operations may be slightly more efficient operating on the head than on the original.

@return the canonical version of 'this'
*/
    public Variable getHead() {
        if (boundTo != null) {
            boundTo = boundTo.getHead();
            return boundTo;
        } else {
            return this;
        }
    }
    
    protected VarData getData() {
        if (Globals.debug && boundTo != null) {
            Globals.localError("Getting data of non-head Variable");
        }
        
        if (data == null) {
            data = new VarData();
        }
        
        return data;
    }
    
    void setData(World w, VarData data) {
        if (Globals.debug && (boundTo != null
                || (!w.getDumpOperations() && this.data != null))) {
            Globals.localError("misapplied setData");
        }
        
        this.data = data;
        
        if (w.getDumpOperations()) {
            Globals.writeLog(this, "B " + Globals.getHexID(this)
                + " " + getClusterID());
        }
    }

    public void hintFixedArity(int n) {
        getHead().getData().hintFixedArity(n);
    }
    
    void bindTo(Variable v) {
        data = null;
        boundTo = v;
    }
    
    public void makeEqual(World w, Variable v) {
        Variable myHead = getHead();
        Variable vHead = v.getHead();
        
        if (myHead != vHead) {
            if (myHead.data == null) {
                w.notifyVarsMerged(vHead, myHead);
                myHead.boundTo = vHead;
            } else if (vHead.data == null) {
                w.notifyVarsMerged(myHead, vHead);
                vHead.boundTo = myHead;
            } else {
                if (myHead.data.swapMergingVariables(vHead.data)) {
                    Variable tmp = myHead;
                    
                    myHead = vHead;
                    vHead = tmp;
                }
            
                w.notifyVarsMerged(myHead, vHead);

                myHead.data.mergeWith(w, vHead.data, myHead, vHead);
            }
        }
    }
    
    public Variable getComponent(World w, int mode, ComponentLabel label) {
        if (Globals.debug && mode != CMODE && mode != DMODE) {
            Globals.nonlocalError("Invalid component mode specified");
        }
        
        Variable head = getHead();
        
        return head.getData().getComponent(w, 1 << mode, head, label);
    }
    
    public Variable getComponent(World w, ComponentLabel label) {
        Variable head = getHead();
        
        return head.getData().getComponent(w, (1 << CMODE) | (1 << DMODE), head, label);
    }
    
    public Variable getInstance(World w, int mode, InstanceLabel label) {
        if (Globals.debug && mode != PMODE && mode != NMODE) {
            Globals.nonlocalError("Invalid instance mode specified");
        }
        
        Variable head = getHead();
        
        return head.getData().getInstance(w, 1 << mode, head, label);
    }
    
    public Variable getInstance(World w, InstanceLabel label) {
        Variable head = getHead();
        
        return head.getData().getInstance(w, (1 << PMODE) | (1 << NMODE), head, label);
    }
    
/**
Marks this variable as "global". Every instance of a global variable is equal to the
global variable. Every component of a global variable is global.
*/
    public void setGlobal(World w) {
        Variable head = getHead();
        
        head.getData().setGlobal(w, head);
    }
    
/**
Marks this variable as "queried". Only queried variables have proper solutions in
the constraint system.
*/
    public void setQueried(World w) {
        Variable head = getHead();
        
        head.getData().setQueried(w, head);
    }
    
/**
@return an Enumeration of InstanceElements
*/
    public Enumeration getInstances(World w) {
        return getHead().getData().getInstances(w);
    }

/**
@return an Enumeration of ComponentElements
*/
    public Enumeration getComponents(World w) {
        return getHead().getData().getComponents(w);
    }

/**
@return an Enumeration of SourceElements
*/
    public Enumeration getSources(World w) {
        return getHead().getData().getSources(w);
    }

/**
@return an Enumeration of ParentElements
*/
    public Enumeration getParents(World w) {
        return getHead().getData().getParents(w);
    }
    
    public String toString() {
        return Globals.getHexID(getHead());
    }
    
/**
This is really only for debugging purposes.
*/
    public String getClusterID() {
        return getHead().getData().getClusterID();
    }

/**
This is really only for debugging purposes.
*/
    public int getDataFlags() {
        return getHead().getData().getFlags();
    }

    public int hashCode() {
        return IdentityManager.getIdentityHashCode(getHead());
    }
}
