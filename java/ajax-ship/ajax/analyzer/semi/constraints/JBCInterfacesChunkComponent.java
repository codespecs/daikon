/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.ComponentLabel;

class JBCInterfacesChunkComponent extends ComponentLabel {
    private static JBCInterfacesChunkComponent obj = new JBCInterfacesChunkComponent();
    
    private JBCInterfacesChunkComponent() {
    }
    
    static JBCInterfacesChunkComponent get() {
        return obj;
    }
    
    public int getVariance() {
        return COVARIANT;
    }

    public String toString() {
        return "subchunk for interfaces";
    }
}
