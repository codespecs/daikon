/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver.test;

import java.util.Hashtable;
import ajax.solver.*;

abstract class Term {
    Term() {
    }
    
    final static boolean USEMODES = false;
    
    public abstract Variable makeType(World w, Environment env);
}
