/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.protocol;

import java.io.*;

public interface MessageHandler {
/**
This is always called from the same thread for a given MessagePort.
It's OK to block, although further message receives on that message port
will be delayed until this one is done.
*/
  public void handleMessage(MessagePort port, Object o);
}
