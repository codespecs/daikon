/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import java.util.zip.ZipEntry;
import java.util.*;

class ZipClassReaderEntryList implements Enumeration {
    private String nextClass;
    private Enumeration zipEntries;
    private boolean gotNextElement;
    
    ZipClassReaderEntryList(Enumeration zipEntries) {
        this.zipEntries = zipEntries;
        gotNextElement = false;
    }
    
    private void findNextClass() {
        if (!gotNextElement) {
            gotNextElement = true;
            
            while (zipEntries.hasMoreElements()) {
                ZipEntry entry = (ZipEntry)zipEntries.nextElement();
                String name = entry.getName();
                
                if (name.endsWith(".class") && !entry.isDirectory()) {
                    nextClass = name.substring(0, name.length() - 6)
                        .replace('/', '.');
                    return;
                }
            }
            
            nextClass = null;
        }
    }
    
    public boolean hasMoreElements() {
        findNextClass();        
        
        return nextClass != null;
    }
    
    public Object nextElement() {
        findNextClass();
        
        if (nextClass != null) {
            gotNextElement = false;
            return nextClass;
        } else {
            throw new NoSuchElementException();
        }
    }
}
