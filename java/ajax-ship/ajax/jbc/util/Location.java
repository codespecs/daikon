/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.jbc.*;

public abstract class Location {
    abstract public JBCField getAccessedField();
    abstract public JBCMethod getCalledMethod();
    
/**
@return the JBCMethod or String naming the enclosing code
*/
    abstract public Object getFunction();
}
