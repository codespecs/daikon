/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.misc;

import java.io.*;

public class ServerData implements Serializable {
    private String programTitle;
    private int serverPort;
    
    public ServerData(String programTitle, int serverPort) {
        this.programTitle = programTitle;
        this.serverPort = serverPort;
    }
    
    public String getProgramTitle() {
        return programTitle;
    }
    
    public int getServerPort() {
        return serverPort;
    }
    
    public String toString() {
        return "ServerData: " + programTitle + " (" + serverPort + ")";
    }
}
