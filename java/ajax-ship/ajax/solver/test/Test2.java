/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver.test;

import ajax.solver.*;
import java.util.*;
import ajax.solver.util.IndexedComponentLabel;

public class Test2 {
    static int randInt(Random rnums, int limit) {
        return Math.abs(rnums.nextInt()%limit);
    }
    
    static boolean validateAll(Hashtable validated, World w, Variable v) {
        if (validated.get(v) != null) {
            return true;
        } else {
            validated.put(v, v);
            
            boolean singleSource = true;
            Variable singleSourceVar = null;
                    
            for (Enumeration e = v.getSources(w); e.hasMoreElements();) {
                Variable source = ((SourceElement)e.nextElement()).getSource();
                
                if (singleSourceVar != null && !singleSourceVar.equals(source)) {
                    singleSource = false;
                } else {
                    singleSourceVar = source;
                }
            }
            
            if (!singleSource) {
                for (Enumeration e = v.getSources(w); e.hasMoreElements();) {
                    SourceElement sourceElement = (SourceElement)e.nextElement();
                    Variable source = sourceElement.getSource();
                    InstanceLabel iLabel = sourceElement.getLabel();
                    
                    for (Enumeration components = source.getComponents(w);
                        components.hasMoreElements();) {
                        ComponentElement componentElement = (ComponentElement)components.nextElement();
                        ComponentLabel cLabel = componentElement.getLabel();
                        
                        if (!componentElement.getComponent().getInstance(w, iLabel).
                            equals(v.getComponent(w, cLabel))) {
                            return false;
                        }
                    }
                }
            }
            
            for (Enumeration e = v.getComponents(w); e.hasMoreElements();) {
                if (!validateAll(validated, w,
                    ((ComponentElement)e.nextElement()).getComponent())) {
                    return false;
                }
            }
            
            for (Enumeration e = v.getInstances(w); e.hasMoreElements();) {
                if (!validateAll(validated, w,
                    ((InstanceElement)e.nextElement()).getInstance())) {
                    return false;
                }
            }
            
            return true;
        }
    }
    
    static boolean runTest(long seed, int numVars, int maxArity,
        double probNullary, int maxInstances, double probNoInstances) {
        Random rnums = new Random(seed);
        World w = new World();
        InstanceLabel[] ils = new InstanceLabel[maxInstances];
        ComponentLabel[] cls = new ComponentLabel[maxArity];
        Variable[] vars = new Variable[numVars];
        
        for (int i = 0; i < ils.length; i++) {
            ils[i] = new InstanceLabel();
        }

        for (int i = 0; i < cls.length; i++) {
            if (i < 10) {
                cls[i] = new IndexedComponentLabel(i);
            } else {
                cls[i] = new ComponentLabel();
            }
        }
        
        for (int i = 0; i < vars.length; i++) {
            vars[i] = new Variable(w);
        }
        
        for (int i = 0; i < vars.length; i++) {
            if (rnums.nextDouble() > probNullary) {
                for (int j = randInt(rnums, maxArity); j > 0; j--) {
                    vars[i].getComponent(w, cls[randInt(rnums, cls.length)]).
                        makeEqual(w, vars[randInt(rnums, vars.length)]);
                }
            }

            if (rnums.nextDouble() > probNoInstances) {
                for (int j = randInt(rnums, maxInstances); j > 0; j--) {
                    vars[i].getInstance(w, ils[randInt(rnums, ils.length)]).
                        makeEqual(w, vars[randInt(rnums, vars.length)]);
                }
            }
        }
        
        while (w.work()) {
        }
        
        rnums = new Random(seed);
        
        boolean OK = true;
        Hashtable validated = new Hashtable();
        
        for (int i = 0; i < vars.length; i++) {
            if (rnums.nextDouble() > probNullary) {
                for (int j = randInt(rnums, maxArity); j > 0; j--) {
                    if (!vars[i].getComponent(w, cls[randInt(rnums, cls.length)]).
                        equals(vars[randInt(rnums, vars.length)])) {
                        OK = false;
                    }
                }
            }

            if (rnums.nextDouble() > probNoInstances) {
                for (int j = randInt(rnums, maxInstances); j > 0; j--) {
                    if (!vars[i].getInstance(w, ils[randInt(rnums, ils.length)]).
                        equals(vars[randInt(rnums, vars.length)])) {
                        OK = false;
                    }
                }
            }
            
            if (!validateAll(validated, w, vars[i])) {
                OK = false;
            }
        }
        
        return OK;
    }
    
    public static void main(String[] args) {
        System.out.print("ajax.solver.test.Test2: ");
        if (runTest(1001, 100, 20, 0.5, 5, 0.3)) {
            System.out.println("passed");
        } else {
            System.out.println("failed");
        }
    }
}
