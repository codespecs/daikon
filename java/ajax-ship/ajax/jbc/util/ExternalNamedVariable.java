/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.jbc.*;

public class ExternalNamedVariable implements ExternalCFGVariable, java.io.Serializable {
    private String name;
    
    public ExternalNamedVariable(String name) {
        this.name = name;
    }
    
    public String toString() {
        return name;
    }
}
