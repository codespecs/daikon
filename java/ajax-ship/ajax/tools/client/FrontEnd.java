/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.client;

import ajax.tools.protocol.*;

public interface FrontEnd extends MessageHandler {
/**
Called in UI thread.
*/
    public void init(Client client, String programName, MessagePort port);
}
