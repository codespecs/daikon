/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.misc;

import ajax.tools.protocol.*;
import java.io.*;
import ajax.util.*;
import java.util.*;
import java.net.*;

/**
Registration Protocol:

ServerTool S opens a port to the registry R

S -->{ServerData} R         // Sends the analyzed program name
( R -->{Object} S )*    // Pings the server periodically; message data is ignored

Client C opens a port to the registry R

( C -->{String} R             // String must be "GetServerList"
  R -->{ServerData[]} C )*    // Sends list of server names
  
ServerLauncher L opens a port to the registry R

L -->(LaunchData) R        // Sends the data about the program to be analyzed
*/
public class ServerRegistry implements MessageHandler {
    public static final int DEFAULT_PORT = 15217;
    public static final String PING = "Ping";
    
    private Hashtable servers = new Hashtable();
    private ObjectQueue q = new ObjectQueue();
    
    public static void main(String[] args) {
        (new ServerRegistry(args)).start();
    }
    
    public ServerRegistry(String[] args) {
    }
    
    public void start() {
        try {
            SocketMessagePort.runServer(this, new ServerSocket(DEFAULT_PORT));
        } catch (IOException ex) {
            System.err.println(ex);
            return;
        }
        
        while (true) {
            try {
                Thread.sleep(5000);
            } catch (InterruptedException ex) {
            }
            
            synchronized (this) {
                for (Enumeration e = servers.keys(); e.hasMoreElements();) {
                    ((MessagePort)e.nextElement()).sendMessage(PING);
                }
            }
        }
    }
    
    private void sendServerList(MessagePort port) {
        ServerData[] data = new ServerData[servers.size()];
        int i = 0;
        
        for (Enumeration e = servers.elements(); e.hasMoreElements();) {
            data[i] = (ServerData)e.nextElement();
            i++;
        }
        
        port.sendMessage(data);
    }
    
    private void launchServer(LaunchData data) {
        String[] args = {
            "java",
            "-Xmx30m",
            "-cp", "c:\\users\\roc\\lackwit\\ajax",
            "ajax.tools.benchmarks.ServerTool",
            data.getProgramTitle(),
            data.getMainClass(),
            "-cp", data.getJARFile() + ";D:\\programs\\jdk117\\Lib\\classes.zip;d:\\programs\\jdk117\\src",
            "-nostderr",
            "-timelimit", Integer.toString(24*60*60),
            "-dumpdir", data.getJARFile() + ".dir",
        };
        
        System.out.println("Executing: " + StringUtils.join(" ", args));
        try {
            Runtime.getRuntime().exec(args);
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }
    
    public synchronized void handleMessage(MessagePort port, Object o) {
        if (o instanceof String) {
            if (o.equals("GetServerList")) {
                sendServerList(port);
            } else {
                System.err.println("Unhandled message: " + o);
            }
        } else if (o instanceof PortErrorMsg) {
            servers.remove(port);
        } else if (o instanceof PortStatusMsg) {
        } else if (o instanceof ServerData) {
            servers.put(port, o);
        } else if (o instanceof LaunchData) {
            launchServer((LaunchData)o);
        } else {
            System.err.println("Unhandled message: " + o);
        }
    }
}
