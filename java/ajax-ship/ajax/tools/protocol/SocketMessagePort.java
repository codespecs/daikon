/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.protocol;

import java.io.*;
import java.net.*;

public class SocketMessagePort extends MessagePort {
    private Socket socket;
    
    private void initSocket(Socket socket, boolean isServer) {
        this.socket = socket;
        
        if (socket == null) {
            dispose();
        } else {
            ObjectInputStream receiver = null;
            ObjectOutputStream sender = null;
            
            for (int i = 0; i < 2; i++) {
                if (i == (isServer ? 0 : 1)) {
                    try {
                        receiver = new ObjectInputStream(new BufferedInputStream(socket.getInputStream()));
                    } catch (IOException ex) {
                        signalError(PortErrorMsg.RECEIVING, ex);
                        return;
                    }
                } else {
                    try {
                        sender = new ObjectOutputStream(new BufferedOutputStream(socket.getOutputStream()));
                        sender.flush();
                    } catch (IOException ex) {
                        signalError(PortErrorMsg.SENDING, ex);
                        return;
                    }
                }
            }
            
            init(receiver, sender);
        }
    }
    
    private SocketMessagePort(MessageHandler handler, Socket socket) {
        super(handler);
        
        initSocket(socket, true);
    }
    
    public SocketMessagePort(MessageHandler handler, final String host, final int port) {
        super(handler);
        
        Thread t = new Thread(new Runnable() {
                public void run() { offlineConnect(host, port); }
            }, "Message Port Connect");
            
        t.setDaemon(true);
        t.start();
    }
    
    private void offlineConnect(String host, int port) {
        try {
            initSocket(new Socket(host, port), false);
        } catch (IOException ex) {
            signalError(PortErrorMsg.RECEIVING, ex);
        }
    }
    
    public static void runServer(final MessageHandler handler, final ServerSocket socket) throws IOException {
        Thread t = new Thread(new Runnable() {
                public void run() { offlineAcceptLoop(handler, socket); }
            }, "Message Port Accept Loop");
        
        t.setDaemon(true);
        t.start();
    }
    
    private static void offlineAcceptLoop(MessageHandler handler, ServerSocket socket) {
        try {
            while (true) {
                new SocketMessagePort(handler, socket.accept());
            }
        } catch (IOException ex) {
        } finally {
            try {
                socket.close();
            } catch (IOException ex) {
            }
        }
    }
}
