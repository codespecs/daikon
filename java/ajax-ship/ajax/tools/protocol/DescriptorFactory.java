/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.protocol;

import ajax.jbc.*;
import ajax.jbc.util.*;

public class DescriptorFactory {
    public static ClassDescriptor makeDescriptor(JBCClass c) {
        return new ClassDescriptor(c.getClassName(), c.getSourceFileName());
    }
    
    public static MethodDescriptor makeDescriptor(JBCMethod m) {
        return new MethodDescriptor(
            makeDescriptor(m.getContainingClass()),
            m.getMethodName(), m.getMethodTypeName());
    }
    
    public static LocationDescriptor makeDescriptor(JBCLocation location) {
        return new LocationDescriptor(
            makeDescriptor(location.getMethod()),
            location.getOffset(),
            JBCCodeUtilities.getSourceLineNumber(location.getMethod(),
                location.getOffset()));
    }
}
