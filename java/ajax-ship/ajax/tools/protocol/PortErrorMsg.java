/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.protocol;

import java.io.*;

public class PortErrorMsg {
    private Exception ex;
    private int direction;
    
    public final static int RECEIVING = 0;
    public final static int SENDING = 1;
    
    PortErrorMsg(int direction, Exception ex) {
        this.direction = direction;
        this.ex = ex;
    }
    
    public int getDirection() {
        return direction;
    }
    
    public Exception getException() {
        return ex;
    }
    
    public String toString() {
        StringWriter w = new StringWriter();
        
        ex.printStackTrace(new PrintWriter(w));
        
        return "PortErrorMsg: " + w.toString();
    }
}
