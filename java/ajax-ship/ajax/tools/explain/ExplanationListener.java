/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.explain;

public interface ExplanationListener {
    public void notifyReachedFromPoint1(SearchItem item);
    public void notifyReachedFromPoint2(SearchItem item);
    public void notifyReached(SearchItem item);
    public void terminate();
}
