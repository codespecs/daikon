/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import java.io.File;
import java.util.*;

class FileSystemClassList implements Enumeration {
    private String prefix;
    private String[] fileList;
    private File dir;
    private FileSystemClassList subdirLister = null;
    private int nextFile = 0;
    private String next = null;
    
    FileSystemClassList(File dir) {
        this(dir, "");
    }
    
    private FileSystemClassList(File dir, String prefix) {
        this.prefix = prefix;
        this.dir = dir;
        this.fileList = dir.list();
    }
    
    private void advanceToNextElement() {
        if (next != null) {
            /* do nothing, we're already at the next element */
        } else if (subdirLister != null && subdirLister.hasMoreElements()) {
            next = (String)subdirLister.nextElement();
        } else {
            while (nextFile < fileList.length) {
                String name = fileList[nextFile];
                File f = new File(dir, name);
                
                nextFile++;
                
                if (f.isDirectory()) {
                    subdirLister = new FileSystemClassList(f, prefix + name + ".");
                    if (subdirLister.hasMoreElements()) {
                        next = (String)subdirLister.nextElement();
                        return;
                    }
                } else if (name.endsWith(".class")) {
                    next = prefix + name.substring(0, name.length() - 6);
                    return;
                }
            }
        }
    }
    
    public boolean hasMoreElements() {
        advanceToNextElement();
        return next != null;
    }
    
    public Object nextElement() {
        advanceToNextElement();
        if (next != null) {
            String result = next;
            
            next = null;
            return result;
        } else {
            throw new NoSuchElementException();
        }
    }
}
