/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util;

import java.util.*;
import ajax.Globals;
import java.io.*;

/**
This class provides a "robust" mechanism for assigning identity hash
codes to objects. It's robust in that it's much more deterministic than the
system method.
*/

class DebuggingIdentityBucket {
    Object key;
    int code;
    DebuggingIdentityBucket next;
    
    DebuggingIdentityBucket(Object key, int code, DebuggingIdentityBucket next) {
        this.key = key;
        this.code = code;
        this.next = next;
    }
}

public class IdentityManager {
    private static IdentityManager identityManager
        = Globals.debugDeterminism ? new IdentityManager() : null;
    
    private boolean bindingsLogInitialized = false;
    private Writer identityBindings;
    private int count = 0;
    private DebuggingIdentityBucket[] buckets = new DebuggingIdentityBucket[10];
    private Hashtable classTable = new Hashtable();
    
    private Object specialObject = null;
    
    public static Object getSpecialObject() {
        return identityManager != null ? identityManager.specialObject : null;
    }
    
    private Writer getBindingsLog() {
        if (!bindingsLogInitialized) {
            try {
                identityBindings = new BufferedWriter(new FileWriter("d:\\tmp\\bindings"));
                bindingsLogInitialized = true;
            } catch (IOException ex) {
                System.err.println(ex);
                bindingsLogInitialized = true;
            }
        }
        return identityBindings;
    }
    
    public static void setBindingsLogFileName(String fileName) {
        if (identityManager != null) {
            try {
                if (fileName == null) {
                    identityManager.identityBindings = null;
                } else {
                    identityManager.identityBindings =
                        new BufferedWriter(new FileWriter(fileName));
                }
                identityManager.bindingsLogInitialized = true;
            } catch (IOException ex) {
                System.err.println(ex);
            }
        }
    }
    
    private IdentityManager() {
    }
    
    public static int getIdentityHashCode(Object o) {
        if (!Globals.debugDeterminism) {
            return System.identityHashCode(o);
        } else {
            return identityManager.getCode(o);
        }
    }  
     
    private int makeCode(Object o) {
        String className = o.getClass().getName().intern();
        Object v = classTable.get(className);
        int[] cell;
        
        if (v != null) {
            cell = (int[])v;
        } else {
            // We use our own hash algorithm here because different VMs
            // implement String.hashCode() differently, and I want the 
            // IdentityManager values to be independent of the VM
            int curValue = 1430101;
            
            for (int i = className.length() - 1; i >= 0; i--) {
                curValue = (curValue << 3) + curValue - className.charAt(i);
            }
            curValue *= 168901091;
            
            cell = new int[1];
            cell[0] = curValue;
            classTable.put(className, cell);
        }
        
        int curValue = cell[0];
        
        cell[0] = curValue + 1;
        return curValue;
    }
    
    private int getCode(Object o) {
        int hash = (System.identityHashCode(o) & 0x7FFFFFFF) % buckets.length;
            
        for (DebuggingIdentityBucket b = buckets[hash]; b != null; b = b.next) {
            if (b.key == o) {
                return b.code;
            }
        }
            
        count++;
        if (count > buckets.length) {
            rehash();
        }
            
        int code = makeCode(o);
        Writer log = getBindingsLog();
        
        if (log != null) {
            try {
                log.write("0x" + Integer.toHexString(System.identityHashCode(o)*8)
                    + "->0x" + Integer.toHexString(code) + "\n");
                log.flush();
            } catch (IOException ex) {
            }
        }
        
        if (code == 0x95d90112) {
            if (specialObject != null) {
                Globals.writeLog(this, "ERROR: Changing special object!");
            }
            specialObject = o;
        }
        
        addObject(o, code);
            
        return code;
    }
    
    private void addObject(Object o, int code) {
        int hash = (System.identityHashCode(o) & 0x7FFFFFFF) % buckets.length;
        
        buckets[hash] = new DebuggingIdentityBucket(o, code, buckets[hash]);
    }
    
    private void rehash() {
        DebuggingIdentityBucket[] oldBuckets = buckets;
        
        buckets = new DebuggingIdentityBucket[oldBuckets.length*2 + 1];
        
        for (int i = 0; i < oldBuckets.length; i++) {
            for (DebuggingIdentityBucket b = oldBuckets[i]; b != null; b = b.next) {
                addObject(b.key, b.code);
            }
        }
    }
}
