/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.ComponentLabel;
import ajax.Globals;

public class JBCMethodArgComponent extends ComponentLabel {
    private static JBCMethodArgComponent[] labels =
        new JBCMethodArgComponent[0];
        
    private int index;
   
    private JBCMethodArgComponent(int index) {
        if (Globals.debug) this.index = index;
    }
   
    public static JBCMethodArgComponent get(int index) {
        if (index >= labels.length) {
            JBCMethodArgComponent[] newLabels =
                new JBCMethodArgComponent[index*2 + 1];
               
            System.arraycopy(labels, 0, newLabels, 0, labels.length);
            
            for (int i = labels.length; i < newLabels.length; i++) {
                newLabels[i] = new JBCMethodArgComponent(i);
            }
            
            labels = newLabels;
        }
        
        return labels[index];
    }

    public int getVariance() {
        return COVARIANT;
    }
    
    public String toString() {
        if (Globals.debug) {
            return "method arg #" + index;
        } else {
            return "method arg";
        }
    }
}
