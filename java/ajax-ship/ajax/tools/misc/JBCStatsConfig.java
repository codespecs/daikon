/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.misc;

import ajax.jbc.util.*;
import ajax.jbc.*;

class JBCStatsConfig extends JBCConfig {
    private String classPath;
    private StandardClassLoader loader = null;
    
    JBCStatsConfig(String classPath) {
        this.classPath = classPath;
    }
    
    JBCStatsConfig() {
        this(null);
    }
    
    StandardClassLoader getLoader() {
        return loader;
    }
    
    protected JBCClassLoader makeSystemClassLoader() {
        if (classPath != null) {
            loader = new StandardClassLoader(getWorld(), classPath);
        } else {
            loader = new StandardClassLoader(getWorld());
        }
        
        return loader;
    }
}
