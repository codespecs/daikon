/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.jgrep;

import ajax.jbc.util.*;
import ajax.jbc.util.salamis.SalamisCodeLoader;
import ajax.jbc.*;
import ajax.analyzer.*;
import ajax.analyzer.semi.*;

public class JGrep {
    public static void printUsage() {
        System.err.println(
"Usage: jgrep -write <package>.<class>.<method>:<variable>.<fields>");
    }
    
    public static void main(String[] args) {
        JBCWorld world = new JBCWorld();
        SEMIAnalyzer engine = new SEMIAnalyzer(world);
        // final Analyzer analyzer = new Analyzer(engine);
        StandardClassLoader loader = new StandardClassLoader(world);
        SalamisCodeLoader nativeEnvironment = new SalamisCodeLoader(loader);

        world.setSystemClassLoader(loader);
        
        try {
            if (args[0].equals("-write")) { /*
                JBCExpression anchor = getAnchor(args[1]);
                
                */
            } else {
                System.err.println("Invalid argument: " + args[0]);
                printUsage();
            }
        } catch (ArrayIndexOutOfBoundsException ex) {
            System.err.println("Insufficient number of arguments");
            printUsage();
        }
    }
}
