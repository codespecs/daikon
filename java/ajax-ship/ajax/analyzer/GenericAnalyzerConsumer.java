/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer;

public interface GenericAnalyzerConsumer {
    public boolean isNewQueryFamilyDisabled();
    public void updateResult(JBCQueryFamily family, Object targetCookie, Object intermediate);
    public void makeFlowgraphLive(String flowgraphName);
    public CoveragePolicy getCoveragePolicy();
}
