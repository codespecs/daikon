/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.benchmarks;

import ajax.Globals;
import ajax.jbc.util.*;
import ajax.jbc.util.reflect.*;
import ajax.jbc.*;
import ajax.analyzer.*;
import ajax.analyzer.util.*;
import ajax.solver.World;

class GeneralBenchmarkConfig extends JBCConfig {
    private String classPath = null;
    private BasicAnalyzerStats stats = null;
    private boolean inUse = false;
    private String appClassPath = null;
    
    void setAppClassPath(String classPath) {
        if (inUse) {
            Globals.localError("GeneralBenchmarkConfig is already in use!");
        }
        
        this.appClassPath = classPath;
    }
    
    void setClassPath(String classPath) {
        if (inUse) {
            Globals.localError("GeneralBenchmarkConfig is already in use!");
        }
        
        this.classPath = classPath;
    }
    
    void setStats(BasicAnalyzerStats stats) {
        if (inUse) {
            Globals.localError("GeneralBenchmarkConfig is already in use!");
        }
        
        this.stats = stats;
    }
    
    public JBCClassLoader makeApplicationClassLoader() {
        if (appClassPath == null) {
            return super.makeApplicationClassLoader();
        } else {
            JBCClassLoader appLoader = new StandardClassLoader(getWorld(), appClassPath);
            
            appLoader.setParent(super.makeApplicationClassLoader());
            return appLoader;
        }
    }
    
    protected JBCWorld makeJBCWorld() {
        inUse = true;
        
        JBCWorld w = new JBCWorld();
        
        if (stats != null) {
            w.addObserver(stats);
        }
        
        return w;
    }
    
    public ReflectionHandler getReflectionHandler() {
        return super.getReflectionHandler();
    }
    
    protected JBCClassLoader makeSystemClassLoader() {
        inUse = true;
        
        if (classPath != null) {
            return new StandardClassLoader(getWorld(), classPath);
        } else {
            return new StandardClassLoader(getWorld());
        }
    }
}
