/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import java.io.*;
import java.util.Enumeration;
import ajax.util.*;
import ajax.jbc.InvalidClassDataError;

public class FileSystemClassReader implements ClassReader {
    private File dir;
    
    public FileSystemClassReader(String dirName) throws IOException {
        this(new File(dirName));
    }
    
    public FileSystemClassReader(File dir) throws IOException {
        this.dir = dir;
        
        if (!dir.isDirectory()) {
            throw new IOException("Supplied directory " + dir + " is not really a directory");
        }
    }
    
    public byte[] readClass(String name) {
        File f = new File(dir, name.replace('.', File.separatorChar) + ".class");
        try {
            return IOUtils.readFileBytes(f);
        } catch (FileNotFoundException ex) {
            return null;
        } catch (IOException ex) {
            throw new InvalidClassDataError("Class file error: " + f);
        }
    }

    public String readSource(String className, String fileName) {
        int lastDot = className.lastIndexOf('.');
	File packageDir;

	if (lastDot >= 0) {
	    packageDir = new File(dir,
                className.substring(0, lastDot).replace('.', File.separatorChar));
	} else {
	    packageDir = dir;
	}

        File f = new File(packageDir, fileName);

        try {
            return IOUtils.readString(new FileReader(f));
        } catch (FileNotFoundException ex) {
            return null;
        } catch (IOException ex) {
            throw new InvalidClassDataError("Class file error: " + f);
        }
    }
    
    public Enumeration getClassNames() {
        return new FileSystemClassList(dir);
    }
    
    public String toString() {
        return "directory " + dir.getPath();
    }
}
