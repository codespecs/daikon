/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

import ajax.Globals;
import java.util.*;
import ajax.util.*;

/**
This class maintains a description of a Java bytecode program. This description
includes detailed information about the bytecode classes that comprise the program,
but also some information about the native code that those classes may call. The
intent is that this information will be used by tools, especially semantic analyzers.
<P>
The user provides instances of JBCClassLoader and/or JBCNativeCodeLoader that
furnish class and native code data to the JBCWorld on demand. The general function
of the JBCWorld is to link these individual items together to form a description
of an entire program. This is done lazily, whenever necessary to satify requests
for information about a class.
<P>
The description always grows incrementally. Classes and native code information
are loaded on demand. Furthermore, the description is mutable: existing classes
and native code information can change. (This may be useful for software
development environments.) Additions and changes to the description are reported
via the JBCObserver interface.
<P>
To try to keep things sane in the presence of mutability, we factor the description
into a set of immutable "names", and a set of immutable "data". Names are
the classes JBCClass, JBCMethod, JBCField, and Strings (for native functions).
Data are the ClassData interface and its helpers, and the ExternalFlowgraph
interface and its helpers. The only mutable part of the description is the
mapping between names and data.
<P>
Currently, mutability is not fully supported. Much of the problem is that
mutability generally implies that at some times, the program will not be globally
consistent, and technically an invalid Java program. For example, temporarily
some methods may be called that do not exist. This can make things rather complex.
<P>
The ajax.jbc.util package contains many classes designed to work with this code.
See the example programs in ajax.jbc.util.test for how to fit things together.
<P>
Currently, no synchronization is done. Therefore the user is responsible for making
sure a JBCWorld and all its associated data is only ever manipulated by one
thread at a time.
*/
public class JBCWorld {
    private JBCObserver observer = null;
    private JBCClassLoader systemClassLoader = null;
    private Vector nativeCodeLoaders = new Vector();
    private Hashtable nativeCodeCache = new Hashtable();
    private Hashtable userFields = new Hashtable();
    private boolean lenientVerification = false;
    private Hashtable classUnknownBehavior = new Hashtable();

    private static final Object NATIVE_CODE_NOT_FOUND = new String("NATIVE_CODE_NOT_FOUND");

    private static String[] specialClassNames = {
        "java.lang.VirtualMachineError",
        "java.lang.LinkageError",
        "java.lang.NullPointerException",
        "java.lang.ArrayIndexOutOfBoundsException",
        "java.lang.ArrayStoreException",
        "java.lang.ArithmeticException",
        "java.lang.NegativeArraySizeException",
        "java.lang.ClassCastException",
        "java.lang.IllegalMonitorStateException",
        "java.lang.ThreadDeath",
        "java.lang.InternalError",
        "java.lang.OutOfMemoryError",
        "java.lang.StackOverflowError",
        "java.lang.UnknownError",
        "java.lang.AbstractMethodError",
        "java.lang.ClassCircularityError",
        "java.lang.ClassFormatError",
        "java.lang.ExceptionInInitializerError",
        "java.lang.IllegalAccessError",
        "java.lang.IncompatibleClassChangeError",
        "java.lang.InstantiationError",
        "java.lang.NoClassDefFoundError",
        "java.lang.NoSuchFieldError",
        "java.lang.NoSuchMethodError",
        "java.lang.UnsatisfiedLinkError",
        "java.lang.VerifyError",
        "java.lang.Object",
        "java.lang.Thread",
        "java.lang.Throwable",
        "java.lang.String"
    };

/**
Create an empty world. You'll want to call at least setSystemClassLoader to make it functional.

@see #setSystemClassLoader
*/
    public JBCWorld() {
    }
    
/**
When lenient verification is on, we skip some checks for well-formed bytecode. Some broken
compilers generate code that is invalid.

@param state turn leniency on or off
*/
    public void setVerificationLenient(boolean state) {
        lenientVerification = state;
    }
    
/**
Determine whether leniency is on or off.

@return true iff leniency is required
*/
    public boolean isVerificationLenient() {
        return lenientVerification;
    }
    
/**
Add an observer. All observers receive notifications when the state of the world changes
(e.g. a new class is loaded, or an existing class is changed).
The notifications are not delivered in any fixed order.

@param observer the observing object
*/
    public void addObserver(JBCObserver observer) {
        if (this.observer != null) {
            this.observer = new JBCObserverMulticaster(this.observer, observer);
        } else {
            this.observer = observer;
        }
    }
    
    void addNativeCodeLoader(JBCNativeCodeLoader loader) {
        nativeCodeLoaders.addElement(loader);
    }
    
/**
Assign the system class loader. This loader must be able to load the java.lang.* classes.
All array classes are loaded in this loader.

@param loader the class loader
*/
    public void setSystemClassLoader(JBCClassLoader loader) {
        if (Globals.debug && systemClassLoader != null && !loader.equals(systemClassLoader)) {
            Globals.nonlocalError("Cannot reassign system class loader");
        } else if (Globals.debug && loader.getWorld() != this) {
            Globals.nonlocalError("The system class loader's world is incorrect!");
        } else {
            systemClassLoader = loader;
           
            for (int i = 0; i < specialClassNames.length; i++) {
                String name = specialClassNames[i];
                
                if (loader.getClass(name) == null) {
                    Globals.nonlocalError("Class loader " + loader + " could not load system class " + name);
                }
            }

            char[] baseArrayTypes = { 'B', 'C', 'D', 'F', 'I', 'J', 'S', 'Z' };

            for (int i = 0; i < baseArrayTypes.length; i++) {
                String name = "[" + baseArrayTypes[i];
                
                loader.setClass(new JBCArrayClassData(name));
            }
        }
    }
    
/**
Return the system class loader.

@return the system class loader.
*/
    public JBCClassLoader getSystemClassLoader() {
        return systemClassLoader;
    }
  
/**
Load a class through the system class loader.

@param name the name of the class to load (fully qualified)
@return the loaded class, or null if it is not found
*/
    public JBCClass getSpecialClass(String name) {
        return systemClassLoader.getClass(name);
    }
    
/**
Load the native code specification for a native function. A native function
is a subroutine that can be reused by the definitions of different native methods
(or functions).

@param name the name of the function
@return the spec for the function, or null if not found
*/
    public ExternalFlowgraph getNativeCode(String name) {
        Object resultObj = nativeCodeCache.get(name);
        
        if (resultObj != null) {
            if (resultObj == NATIVE_CODE_NOT_FOUND) {
                return null;
            } else {
                return (ExternalFlowgraph)resultObj;
            }
        } else {
            for (Enumeration e = nativeCodeLoaders.elements(); e.hasMoreElements();) {
                ExternalFlowgraph fg = ((JBCNativeCodeLoader)e.nextElement()).loadFunctionCode(name);
                
                if (fg != null) {
                    nativeCodeCache.put(name, fg);
                    if (observer != null) {
                        observer.notifyNativeCodeLoaded(name, fg);
                    }
                    return fg;
                }
            }
            
            nativeCodeCache.put(name, NATIVE_CODE_NOT_FOUND);
            return null;
        }
    }
    
/**
Load the native code specification for a native method.

@param name the method
@return the spec for the method, or null if not found
*/
    public ExternalFlowgraph getNativeCode(JBCMethod name) {
        Object resultObj = nativeCodeCache.get(name);
        
        if (resultObj != null) {
            if (resultObj == NATIVE_CODE_NOT_FOUND) {
                return null;
            } else {
                return (ExternalFlowgraph)resultObj;
            }
        } else {
            for (Enumeration e = nativeCodeLoaders.elements(); e.hasMoreElements();) {
                ExternalFlowgraph fg = ((JBCNativeCodeLoader)e.nextElement()).loadMethodCode(name);
                
                if (fg != null) {
                    nativeCodeCache.put(name, fg);
                    if (observer != null) {
                        observer.notifyNativeCodeLoaded(name, fg);
                    }
                    return fg;
                }
            }
            
            nativeCodeCache.put(name, NATIVE_CODE_NOT_FOUND);
            return null;
        }
    }
    
    void setMethodNativeCode(JBCMethod name) {
        Object oldObj = nativeCodeCache.get(name);
        
        for (Enumeration e = nativeCodeLoaders.elements(); e.hasMoreElements();) {
            ExternalFlowgraph fg = ((JBCNativeCodeLoader)e.nextElement()).loadMethodCode(name);
            
            if (fg != null) {
                nativeCodeCache.put(name, fg);
                
                if (observer != null) {
                    ExternalFlowgraph oldFG = oldObj instanceof ExternalFlowgraph 
                        ? (ExternalFlowgraph)oldObj : null;
                        
                    if (oldFG != null) {
                        if (!oldFG.equals(fg)) {
                            observer.notifyNativeCodeChanged(name, oldFG, fg);
                        }
                    } else {
                        observer.notifyNativeCodeLoaded(name, fg);
                    }
                }
                return;
            }
        }
    }
    
    void setFunctionNativeCode(String name) {
        Object oldObj = nativeCodeCache.get(name);
        
        for (Enumeration e = nativeCodeLoaders.elements(); e.hasMoreElements();) {
            ExternalFlowgraph fg = ((JBCNativeCodeLoader)e.nextElement()).loadFunctionCode(name);
            
            if (fg != null) {
                nativeCodeCache.put(name, fg);
                
                if (observer != null) {
                    ExternalFlowgraph oldFG = oldObj instanceof ExternalFlowgraph 
                        ? (ExternalFlowgraph)oldObj : null;
                        
                    if (oldFG != null) {
                        if (!oldFG.equals(fg)) {
                            observer.notifyNativeCodeChanged(name, oldFG, fg);
                        }
                    } else {
                        observer.notifyNativeCodeLoaded(name, fg);
                    }
                }
                return;
            }
        }
    }
    
    void notifyClassLoaded(JBCClass c) {
        Globals.writeLog(this, "Loaded " + c.toString());
        
        if (observer != null) {
            observer.notifyClassLoaded(c);
        }
    }
    
    void notifyClassChanged(JBCClass c, ClassData from, ClassData to) {
        Globals.writeLog(this, "Changed " + c.toString());
        
        if (observer != null) {
            observer.notifyClassChanged(c, from, to);
        }
    }
    
    public void setClassUnknownBehavior(Enumeration classes, UnknownBehavior behavior) {
        while (classes.hasMoreElements()) {
            classUnknownBehavior.put(classes.nextElement(), behavior);
        }
    }
    
    public ExternalFlowgraph getUnknownBehavior(JBCMethod m) {
        UnknownBehavior b = (UnknownBehavior)classUnknownBehavior.get(m.getContainingClass());
        
        if (b == null) {
            return null;
        } else {
            return b.getBehavior(m);
        }
    }

/**
Notify the system that a native function is an entry point for the virtual machine; i.e.
this function is live. This is necessary to start the analysis process; otherwise, the
analyses will conclude that nothing is live.

@param name the native function to be made live
*/
    public void addMainInvocation(String name) {
        if (observer != null) {
            observer.notifyAddedMainInvocation(name);
        }
    }
}
