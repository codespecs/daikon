/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.protocol;

import java.io.*;
import ajax.util.*;
import java.util.*;

public abstract class MessagePort {
    private ObjectInputStream input;
    private ObjectOutputStream output;
    private MessageHandler handler;
    private ObjectQueue writeQueue = new ObjectQueue();
    private Vector readList = new Vector();
    private int state = UNOPENED;
    private boolean logging = true;
    
    public static final int UNOPENED = 0;
    public static final int OPEN = 1;
    public static final int CLOSED = 2;
    
    protected MessagePort(MessageHandler handler) {
        this.handler = handler;
    }
    
    protected synchronized void signalError(int mode, Exception ex) {
        if (state != CLOSED) {
            readList.addElement(new PortErrorMsg(mode, ex));
            dispose();
        }
    }
    
    private void log(String s) {
        if (logging) {
            System.err.println(s);
        }
    }
    
    protected MessageHandler getHandler() {
        return handler;
    }
    
    protected void init(ObjectInputStream input, ObjectOutputStream output) {
        synchronized (this) {
            this.input = input;
            this.output = output;
            
            setState(OPEN);
            notify();
        }
        
        Thread t = new Thread(new Runnable() {
                public void run() { offlineReadLoop(); }
            }, "Message Port Read Loop");
        
        t.setDaemon(true);
        t.start();
        
        t = new Thread(new Runnable() {
                public void run() { offlineWriteLoop(); }
            }, "Message Port Write Loop");
        
        t.setDaemon(true);
        t.start();
    }
    
    public synchronized int getState() {
        return state;
    }
    
    private void setState(int state) {
        this.state = state;
        
        log("CHANGED STATE: " + state);
        readList.addElement(new PortStatusMsg(state));
    }
    
    public synchronized void dispose() {
        if (state == OPEN) {
            setState(CLOSED);
            
            try {
                writeQueue.terminate();
                input.close();
                output.close();
            } catch (IOException ex) {
            }
        } else if (state == UNOPENED) {
            setState(CLOSED);
        }
    }
    
/**
This never blocks.
*/
    public void sendMessage(Object o) {
        synchronized (this) {
            if (state != CLOSED) {
                writeQueue.addObject(o);
            }
        }
    }
    
    private void offlineWriteLoop() {
        try {
            while (true) {
                Object o = writeQueue.removeObject();
                ObjectOutputStream out;
                
                synchronized (this) {
                    if (state == OPEN) {
                        out = output;
                    } else {
                        return;
                    }
                }
                
                log("SENDING: " + o);
                out.writeObject(o);
                out.flush();
            }
        } catch (IOException ex) {
            signalError(PortErrorMsg.SENDING, ex);
        } catch (NoSuchElementException ex) {
        }
    }
    
    private void offlineReadLoop() {
        Vector list = null;
                
        synchronized (this) {
            while (state == UNOPENED) {
                try {
                    wait();
                } catch (InterruptedException ex) {
                }
            }
                
            list = readList;
            readList = new Vector();
        }
        
        if (list != null) {
            for (Enumeration e = list.elements(); e.hasMoreElements();) {
                Object o = e.nextElement();
                
                log("RECEIVING: " + o);
                handler.handleMessage(this, o);
            }
        }
        
        try {
            while (true) {
                ObjectInputStream in;
            
                synchronized (this) {
                    if (state == OPEN) {
                        in = input;
                    } else {
                        break;
                    }
                }
                
                Object o = in.readObject();
                
                log("RECEIVING: " + o);
                handler.handleMessage(this, o);
            }
        } catch (IOException ex) {
            signalError(PortErrorMsg.RECEIVING, ex);
        } catch (ClassNotFoundException ex) {
            signalError(PortErrorMsg.RECEIVING, ex);
        }
        
        synchronized (this) {
            list = readList;
            readList = null;
        }
        
        if (list != null) {
            for (Enumeration e = list.elements(); e.hasMoreElements();) {
                Object o = e.nextElement();
                
                log("RECEIVING: " + o);
                handler.handleMessage(this, o);
            }
        }
    }
}
