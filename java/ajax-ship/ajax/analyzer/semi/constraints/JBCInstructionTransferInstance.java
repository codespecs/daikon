/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.InstanceLabel;
import ajax.jbc.JBCMethod;
import ajax.Globals;

class JBCInstructionTransferInstance extends InstanceLabel {
    private int from;
    private int to;
    private JBCMethod method;
    private int discriminator;
    
    private JBCInstructionTransferInstance(JBCMethod method, int from, int to, int discriminator) {
        if (Globals.debug) {
            this.from = from;
            this.to = to;
            this.method = method;
            this.discriminator = discriminator;
        }
    }
    
    static JBCInstructionTransferInstance get(JBCMethod method, int from, int to, int discriminator) {
        return new JBCInstructionTransferInstance(method, from, to, discriminator);
    }
    
    public String toString() {
        if (Globals.debug) {
            return "control transfer from " + from + " to " + to + " in " + method;
        } else {
            return super.toString();
        }
    }
}
