/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.ComponentLabel;
import ajax.Globals;

class JBCInstructionStackElemComponent extends ComponentLabel {
    private int element;
    
    private static JBCInstructionStackElemComponent[] components =
        new JBCInstructionStackElemComponent[0];
    
    private JBCInstructionStackElemComponent(int element) {
        if (Globals.debug) this.element = element;
    }
    
    static JBCInstructionStackElemComponent get(int element) {
        if (element >= 0) {
            if (element >= components.length) {
                JBCInstructionStackElemComponent[] cs =
                    new JBCInstructionStackElemComponent[element*2 + 1];
                
                System.arraycopy(components, 0, cs, 0, components.length);
                for (int i = components.length; i < cs.length; i++) {
                    cs[i] = new JBCInstructionStackElemComponent(i);
                }
                
                components = cs;
            }
            
            return components[element];
        } else {
            throw Globals.nonlocalError("Negative elements not allowed");
        }
    }
    
    public String toString() {
        return "stack element " + element;
    }
}
