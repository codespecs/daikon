/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.util;

import java.util.*;
import ajax.jbc.*;
import ajax.analyzer.*;

public class NewObjectMethodSource implements DatumSpecifier, OpcodeConstants {
    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, ExternalFlowgraph fg, Vector externalNodes) {
        MethodQueryUtilities.addClassMethodImplementationSources(family, method, fg, externalNodes);
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, String name, ExternalFlowgraph fg, Vector externalNodes) {
        MethodQueryUtilities.addClassMethodImplementationSources(family, name, fg, externalNodes);
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, byte[] code, boolean[] instructions) {
        MethodQueryUtilities.addClassMethodImplementationSources(family, method, code, instructions);
    }
}
