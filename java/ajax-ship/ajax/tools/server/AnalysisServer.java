/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.server;

import ajax.analyzer.*;
import ajax.tools.protocol.*;

public interface AnalysisServer {
    public void init(Server server, Analyzer analyzer, MessagePort port, AnalysisRequest request);
    public void removedFromServer();
}
