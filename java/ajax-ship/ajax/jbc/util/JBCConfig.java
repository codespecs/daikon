/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.Globals;
import ajax.jbc.*;
import ajax.jbc.util.salamis.*;
import ajax.jbc.util.reflect.*;

public class JBCConfig {
    private JBCWorld w = null;
    private JBCNativeCodeLoader nativeLoader = null;
    private ReflectionHandler reflectionHandler = null;
    private JBCClassLoader appClassLoader = null;
    
    public JBCWorld getWorld() {
        if (w == null) {
            w = makeJBCWorld();
        }
        
        return w;
    }
    
    public JBCConfig() {
    }
    
    public void init() {
        JBCClassLoader loader = makeSystemClassLoader();
        
        getWorld().setSystemClassLoader(loader);
        
        appClassLoader = makeApplicationClassLoader();
        nativeLoader = makeNativeCodeLoader(appClassLoader);
    }
    
    public void addMainInvocation(JBCClassLoader loader, String className) throws UnresolvedClassException, MissingMethodException, AmbiguousMethodException {
        String mainInvocation = ((SalamisCodeLoader)nativeLoader).makeMainInvocation(loader, className);
        
        if (mainInvocation == null) {
            throw Globals.nonlocalError("Invalid specification resource");
        }

        getWorld().addMainInvocation(mainInvocation);
    }
    
    final public void addMainInvocation(String name) throws UnresolvedClassException, MissingMethodException, AmbiguousMethodException {
        addMainInvocation(appClassLoader, name);
    }
    
    protected JBCWorld makeJBCWorld() {
        return new JBCWorld();
    }
    
    protected JBCClassLoader makeSystemClassLoader() {
        return new StandardClassLoader(getWorld());
    }
    
    public JBCClassLoader makeApplicationClassLoader() {
        return getWorld().getSystemClassLoader();
    }
    
    public JBCClassLoader getApplicationClassLoader() {
        return appClassLoader;
    }
    
    protected ReflectionHandler getReflectionHandler() {
        return reflectionHandler;
    }
    
    protected JBCNativeCodeLoader makeNativeCodeLoader(JBCClassLoader loader) {
        if (loader instanceof StandardClassLoader) {
            reflectionHandler = new ReflectionHandler((StandardClassLoader)loader);
        }
        
        return new SalamisCodeLoader(loader);
    }
}
