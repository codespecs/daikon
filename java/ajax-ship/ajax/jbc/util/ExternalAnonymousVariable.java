/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.jbc.*;
import java.io.Serializable;
import ajax.util.IdentityManager;

public class ExternalAnonymousVariable implements ExternalCFGVariable, Serializable {
    public ExternalAnonymousVariable() {
    }
    
    public int hashCode() {
        return IdentityManager.getIdentityHashCode(this);
    }
}
