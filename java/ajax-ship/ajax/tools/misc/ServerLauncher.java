/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.misc;

import ajax.tools.protocol.*;

public class ServerLauncher {
    public static void main(String[] args) {
        MessagePort port = new SocketMessagePort(
            new DummyHandler(), "localhost", ServerRegistry.DEFAULT_PORT);
            
        port.sendMessage(new LaunchData(args[0], args[1], args[2]));
        
        try {
            Thread.sleep(2000);
        } catch (InterruptedException ex) {
        }
        
        System.exit(0);
    }
}
