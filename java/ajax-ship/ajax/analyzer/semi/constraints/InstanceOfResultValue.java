/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.jbc.*;

class InstanceOfResultValue extends AbstractStackValue implements OpcodeConstants {
    private byte[] code;
    private AbstractStackValue objectSource;
    private JBCClass c;
    private ConstraintManager manager;
    
    InstanceOfResultValue(ConstraintManager manager, byte[] code, AbstractStackValue objectSource, JBCClass c) {
        this.code = code;
        this.objectSource = objectSource;
        this.c = c;
    }
    
    ConstraintManager getManager() {
        return manager;
    }
    
    AbstractStackValue getSourceValue() {
        return objectSource;
    }
    
    JBCClass getInstanceClass() {
        return c;
    }
    
    SpecialPredecessorRouting notifyConsumed(int consumerOffset) {
        switch (code[consumerOffset] & 0xFF) {
            case OP_ifeq:
                return new InstanceOfRouting(this, 0);
            case OP_ifne:
            case OP_ifgt:
                return new InstanceOfRouting(this, 1);
            default:
                return null;
        }
    }
    
    boolean isEqualValue(Object o) {
        return o == this;
    }
}
