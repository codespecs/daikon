/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver.test;

import ajax.solver.*;
import java.util.*;
import ajax.solver.util.SolverDebug;

public class Test1 {
    public static void main(String[] args) {
        System.out.print("ajax.solver.test.Test1: ");

        World w = new World();
        InstanceLabel il1 = new InstanceLabel();
        InstanceLabel il2 = new InstanceLabel();
        ComponentLabel cl1 = new ComponentLabel();

        Variable v1 = new Variable(w);
        v1.getInstance(w, il1).makeEqual(w, v1.getComponent(w, cl1));

        Variable v2 = new Variable(w);
        v2.getComponent(w, cl1).makeEqual(w, v2);

        v1.getInstance(w, il2).makeEqual(w, v2);

        Variable[] vs = { v1, v2 };

        do {
            System.out.println(SolverDebug.dumpVarInfo(vs, w));
        } while (w.work());

        System.out.println(SolverDebug.dumpVarInfo(vs, w));

        System.out.println("passed");
    }
}
