/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.util.*;
import ajax.jbc.util.*;

/**
This interface defines a predicate on class names.
*/
public interface ClassMask {
/**
Given a StandardClassLoader, extract the names of the classes it can load
that satisfy the mask. Here, we consider a class loader to be able to
load an array if it can load the array's element type.

@param loader the class loader whose classes will be scanned
@return a set of matching class names (Strings)
*/
    public CompactSet findMatchingClasses(StandardClassLoader loader);

/**
Determine whether a given class name satisfies the mask.

@param name the name of the class
@return true iff the class satisfies the mask
*/
    public boolean isMatchingClass(String name);
}
