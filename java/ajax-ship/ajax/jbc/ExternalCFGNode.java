/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

import java.util.Enumeration;

public interface ExternalCFGNode {
/** The ordering of the successors must be consistent. */
    public Enumeration getSuccessors();
}
