/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.ComponentLabel;
import ajax.Globals;

class JBCInstructionLocalVarComponent extends ComponentLabel {
    private int element;
    
    private static JBCInstructionLocalVarComponent[] components =
        new JBCInstructionLocalVarComponent[0];
    
    private JBCInstructionLocalVarComponent(int element) {
        if (Globals.debug) this.element = element;
    }
    
    static JBCInstructionLocalVarComponent get(int element) {
        if (element >= 0) {
            if (element >= components.length) {
                JBCInstructionLocalVarComponent[] cs =
                    new JBCInstructionLocalVarComponent[element*2 + 1];
                
                System.arraycopy(components, 0, cs, 0, components.length);
                for (int i = components.length; i < cs.length; i++) {
                    cs[i] = new JBCInstructionLocalVarComponent(i);
                }
                
                components = cs;
            }
            
            return components[element];
        } else {
            throw Globals.nonlocalError("Negative local variables not allowed");
        }
    }
    
    public String toString() {
        return "local variable " + element;
    }
}
