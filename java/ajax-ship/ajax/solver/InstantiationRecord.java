/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver;

import ajax.Globals;
import java.util.Enumeration;

class InstantiationRecord implements SourceElement, InstanceElement {
    Variable from;
    Variable to;
    InstanceLabel label;
    private int flags = 0;

    public static final int POLARITY_POS = 1 << Variable.PMODE;
    public static final int POLARITY_NEG = 1 << Variable.NMODE;
    public static final int POLARITY_BOTH = POLARITY_POS | POLARITY_NEG;
    
    InstantiationRecord(Variable from, Variable to, InstanceLabel label,
			int polarity) {
        this.from = from;
        this.to = to;
        this.label = label;
        updatePolarity(polarity);
    }

    public int hashCode() {
        return label.hashCode();
    }
    
    public boolean equals(Object o) {
        return o instanceof InstanceLabel ? label.equals(o)
            : ((InstantiationRecord)o).label.equals(label);
    }
    
    public String toString() {
        return label.toString() + " (" + Globals.getHexID(getSourceHead())
            + " to " + Globals.getHexID(getInstanceHead()) + ", "
            + flags + ")";
    }

    public int getPolarity() {
        return flags;
    }

    public void updatePolarity(int polarity) {
        if (Globals.debug && (polarity & ~POLARITY_BOTH) != 0) {
            Globals.localError("Invalid polarity: " + polarity);
	}
        flags |= polarity;
    }
    
    public Variable getSource() {
        return from;
    }
    
    public Variable getSourceHead() {
        Variable v = from.getHead();
        
        from = v;
        return v;
    }
    
    public InstanceLabel getLabel() {
        return label;
    }
    
    public Variable getInstanceHead() {
        Variable v = to.getHead();
        
        to = v;
        return v;
    }

    public Variable getInstance() {
        return to;
    }

    public void validate(World w) throws ValidationException {
        VarData sourceData = getSourceHead().getData();
        
        for (Enumeration e = getInstanceHead().getData().enumerateComponents(); e.hasMoreElements();) {
            ComponentRecord rTo = (ComponentRecord)e.nextElement();
            
            if (rTo.getKind() == ComponentRecord.COMPONENT) {
                ComponentRecord rFrom = sourceData.getComponent(rTo.label);
                
                if (rFrom != null) {
                    int rFromModes = rFrom.getModes();
                    int rToModes = rTo.getModes();
                    
                    if (rFrom.getKind() == ComponentRecord.COMPONENT
                        && VarData.modesMatch(rFromModes, rToModes)
                        && sourceData.getInstance(w, label) == null) {
                        throw new ValidationException("Instance should be present for component " + rTo.label);
                    }
                    
                    if ((rFromModes & ~rToModes) != 0) {
                        throw new ValidationException("Modes did not propagate for component " + rTo.label);
                    }
                    
                    if ((rTo.getModeInstances() & ~rFrom.getModeInstances()) != 0) {
                        throw new ValidationException("Instance modes did not propagate for component " + rTo.label);
                    }
                }
            }
        }
    }
}
