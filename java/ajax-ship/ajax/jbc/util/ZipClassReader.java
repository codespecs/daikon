/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import java.io.*;
import java.util.*;
import java.util.zip.*;
import ajax.util.*;
import ajax.jbc.InvalidClassDataError;

public class ZipClassReader implements ClassReader {
    private ZipFile zip;
    
    public ZipClassReader(String name) throws IOException {
        this(new ZipFile(name));
    }
    
    public ZipClassReader(File file) throws IOException {
        this(new ZipFile(file));
    }
    
    public ZipClassReader(ZipFile zip) {
        this.zip = zip;
    }
    
    public byte[] readClass(String name) {
        try {
            ZipEntry entry = zip.getEntry(name.replace('.', '/') + ".class");
            
            if (entry == null) {
                return null;
            } else {
                InputStream in = zip.getInputStream(entry);
                int size = (int)Math.min(entry.getSize(), (long)Integer.MAX_VALUE);
                
                if (size >= 0) {
		    return IOUtils.readBytes(in, size);
                } else {
		    return IOUtils.readBytes(in);
		}
            }
        } catch (IOException ex) {
            throw new InvalidClassDataError("Error reading class from ZIP file: " + ex.getMessage());
        }
    }
    
    public String readSource(String className, String fileName) {
        int lastDot = className.lastIndexOf('.');
	String name;

	if (lastDot >= 0) {
	    name = className.substring(0, lastDot).replace('.', '/') + "/" + fileName;
	} else {
	    name = fileName;
	}

        try {
            ZipEntry entry = zip.getEntry(name);
            
            if (entry == null) {
                return null;
            } else {
                return IOUtils.readString(
		    new InputStreamReader(zip.getInputStream(entry)));
            }
        } catch (IOException ex) {
            throw new InvalidClassDataError("Error reading class from ZIP file: " + ex.getMessage());
        }
    }

    public Enumeration getClassNames() {
        return new ZipClassReaderEntryList(zip.entries());
    }
    
    public String toString() {
        return "zip file " + zip.getName();
    }
}
