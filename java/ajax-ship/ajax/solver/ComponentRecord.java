/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver;

import ajax.Globals;

class ComponentRecord implements ComponentElement, ParentElement {
    private int flags;
    Variable parent;
    Variable child;
    ComponentLabel label;
    
    static final int BLANK         = 0;
    static final int ADVERTISEMENT = 1; // in which case 'child' is the source
    static final int COMPONENT     = 2;
    
    private static final int KIND_MASK      = 0x3;
    
    public static final int SOURCES_DIRTY   = 0x4;
    public static final int INSTANCES_DIRTY = 0x8;
    public static final int DIRTY_BITS      = SOURCES_DIRTY | INSTANCES_DIRTY;
    
    public static final int ADVERTISEMENT_HAS_COMPONENT_INSTANCE = 0x100;
    
    private static final int HAS_MODE_SHIFT = 4;
    private static final int HAS_INSTANCE_SHIFT = 6;
    private static final int MODE_MASK = 0x03;
  
    ComponentRecord(ComponentLabel label) {
        this(null, null, label);
    }
    
    ComponentRecord(Variable parent, ComponentLabel label) {
        this(parent, null, label);
    }
    
    ComponentRecord(Variable parent, Variable child, ComponentLabel label) {
        flags = BLANK;
        this.parent = parent;
        this.child = child;
        this.label = label;
    }
    
    public int hashCode() {
        return label.hashCode();
    }
    
    public boolean equals(Object o) {
        return o instanceof ComponentLabel ? label.equals(o)
            : ((ComponentRecord)o).label.equals(label);
    }
    
    void setReal(Variable component) {
        flags = (flags & ~KIND_MASK) | COMPONENT;
        child = component;
    }
    
    boolean willAddModes(int modeBits) {
        if (Globals.debug && (modeBits & ~MODE_MASK) != 0) {
            Globals.localError("Invalid mode bits");
        }
        
        return (flags | (modeBits << HAS_MODE_SHIFT)) != flags;
    }
    
/**
If this changes the mode, then the component must be made dirty WRT its instances.
*/
    void addModes(int modeBits) {
        if (Globals.debug && (modeBits & ~MODE_MASK) != 0) {
            Globals.localError("Invalid mode bits");
        }
        
        flags |= modeBits << HAS_MODE_SHIFT;
    }
    
    boolean willAddModeInstances(int modeBits) {
        if (Globals.debug && (modeBits & ~MODE_MASK) != 0) {
            Globals.localError("Invalid mode bits");
        }
        
        return (flags | (modeBits << HAS_INSTANCE_SHIFT)) != flags;
    }
    
/**
If this changes the mode, then the component must be made dirt WRT its sources.
*/
    void addModeInstances(int modeBits) {
        if (Globals.debug && (modeBits & ~MODE_MASK) != 0) {
            Globals.localError("Invalid mode bits");
        }
        
        flags |= modeBits << HAS_INSTANCE_SHIFT;
    }
    
    int getModes() {
        return (flags >> HAS_MODE_SHIFT) & MODE_MASK;
    }
    
    int getModeInstances() {
        return (flags >> HAS_INSTANCE_SHIFT) & MODE_MASK;
    }
    
    void setAdvertisement(Variable source) {
        flags = (flags & ~KIND_MASK) | ADVERTISEMENT;
        child = source;
    }
    
    int getDirtyFlags() {
        return flags & DIRTY_BITS;
    }
    
    void setAdvertisementHasComponentInstance() {
        flags |= ADVERTISEMENT_HAS_COMPONENT_INSTANCE;
    }
    
    boolean doesAdvertisementHaveComponentInstance() {
        return (flags & ADVERTISEMENT_HAS_COMPONENT_INSTANCE) != 0;
    }
    
    void setDirtyFlags(int dirty) {
        if (Globals.debug && (dirty & ~DIRTY_BITS) != 0) {
            Globals.localError("Invalid dirty bits: " + dirty);
        }
        
        flags = (flags & ~DIRTY_BITS) | dirty;
    }
    
    public void validate(World w) throws ValidationException {
        if (getDirtyFlags() != 0) {
            throw new ValidationException("Component dirty: " + getDirtyFlags());
        }
        
        if ((getModes() & ~getModeInstances()) != 0) {
            throw new ValidationException("Mode bits did not propagate to mode instance bits");
        }
    }
    
    int getKind() {
        return flags & KIND_MASK;
    }
    
    public Variable getComponent() {
        return child;
    }

    public Variable getComponentHead() {
        Variable v = child.getHead();
        
        child = v;
        return v;
    }
    
    public Variable getParent() {
        return parent;
    }
    
    public Variable getParentHead() {
        Variable v = parent.getHead();
        
        parent = v;
        return v;
    }
    
    public ComponentLabel getLabel() {
        return label;
    }
    
    public boolean hasMode(int mode) {
        if (Globals.debug && mode != Variable.CMODE && mode != Variable.DMODE) {
            Globals.nonlocalError("Invalid mode requested");
        }
        
        return (flags & (1 << (mode + HAS_MODE_SHIFT))) != 0;
    }
    
    private static String getModeString(int bits) {
        switch (bits) {
            case 0:
                return "";
            case 1 << Variable.CMODE:
                return "C";
            case 1 << Variable.DMODE:
                return "D";
            case (1 << Variable.CMODE) | (1 << Variable.DMODE):
                return "C,D";
            default:
                throw Globals.localError("Invalid bit mask");
        }
    }
            
    public String toString() {
        return "M=" + getModeString(getModes())
            + ", I=" + getModeString(getModeInstances());
    }
}
