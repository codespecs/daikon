/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import java.util.Enumeration;

public interface ClassReader {
/**
Read a class given the name.

@param name the class name, fully qualified, dot-separated
@return null if the class cannot be read, otherwise the class file data
*/
    public byte[] readClass(String name);

    public String readSource(String className, String fileName);
    
/**
Get a list of class names that are available for reading.
Note that this list may not be exhaustive --- some readers (e.g. URLs) may not
be able to list all the classes that they actually could read. Some
of the classes on the list may not be successfully read, either.

@return an enumeration of Strings
*/
    public Enumeration getClassNames();
}
