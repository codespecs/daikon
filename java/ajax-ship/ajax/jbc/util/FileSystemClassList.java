/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import java.io.File;
import java.io.IOException;
import java.util.*;
import ajax.Globals;

class FileSystemClassList implements Enumeration {
    private String prefix;
    private String[] fileList;
    private File dir;
    private FileSystemClassList subdirLister = null;
    private int nextFile = 0;
    private String next = null;
    private Hashtable dirsSeen = null;
    
    FileSystemClassList(File dir) {
        this(dir, "", new Hashtable());
    }
    
    private FileSystemClassList(File dir, String prefix, Hashtable dirsSeen) {
        this.prefix = prefix;
        this.dir = dir;
        this.fileList = dir.list();
        this.dirsSeen = dirsSeen;

        if (fileList == null) {
            Globals.localError("Cannot read file names in " + dir);
            fileList = new String[0];
	}
    }
    
    /**
     * Prevents infinite recursion when directory symlinks point backwards
     * @return true if had not been seen before (was added)
     **/
    private boolean markSeen(File candidate) {
        try {
	    String canonical = candidate.getCanonicalPath();
	    Object last = dirsSeen.put(canonical, canonical);
	    return (last == null);
	} catch (IOException e) {
	    // File system error; better bail.
	  throw new RuntimeException(e.toString());
	}
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
                
		if (f.isDirectory() && markSeen(f)) {
                    subdirLister = new FileSystemClassList(f, prefix + name + ".", dirsSeen);
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
