/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.util;

public interface ResultListener {
    public void registerTarget(Object targetCookie);
    public void updateResult(Object targetCookie, Object intermediate);
    public void notifyAnalysisComplete();
}
