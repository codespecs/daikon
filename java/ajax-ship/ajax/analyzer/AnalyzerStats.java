/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer;

import ajax.jbc.*;

public interface AnalyzerStats {
    public void notifyMethodLive(JBCMethod m);
    public void notifyNativeCodeLive(String name);
}
