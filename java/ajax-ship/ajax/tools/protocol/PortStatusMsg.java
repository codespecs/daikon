/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.protocol;

public class PortStatusMsg {
    private int newState;
    
    PortStatusMsg(int state) {
        newState = state;
    }
    
    public int getState() {
        return newState;
    }
    
    public String toString() {
        return "PortStatusMsg: " + newState;
    }
}
