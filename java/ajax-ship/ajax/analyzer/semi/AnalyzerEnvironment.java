/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi;

import ajax.solver.*;
import ajax.jbc.*;
import java.util.Enumeration;
import ajax.analyzer.*;
import ajax.analyzer.semantics.*;

public interface AnalyzerEnvironment {
    public World getSolver();
    public JBCWorld getWorld();
    public Semantics getSemantics();
    public boolean isNewQueryFamilyDisabled();
    
    public void addToken(Variable v, Token t);
    public void removeToken(Variable v, Token t);
    public Token getFirstToken(Variable v);
    
    public void setJBCType(Variable v, JBCType t);
    
    public Analyzer getPrephase();
    
    public void makeFlowgraphLive(String name);
    
    /* for debugging only */
    public String dumpVarInfo(Variable v);
}
