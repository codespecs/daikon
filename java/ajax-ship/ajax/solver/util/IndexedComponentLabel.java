/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver.util;

import ajax.solver.ComponentLabel;

public class IndexedComponentLabel extends ComponentLabel {
    private int index;
    
    public IndexedComponentLabel(int index) {
        super();
        this.index = index;
    }
    
    public int hashCode() {
        return index;
    }
    
    public boolean equals(Object o) {
        return o instanceof IndexedComponentLabel &&
            ((IndexedComponentLabel)o).index == index;
    }
    
    public int getComponentIndex() {
        return index;
    }
}
