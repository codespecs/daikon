/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.*;
import ajax.analyzer.semi.*;
import ajax.jbc.*;
import java.util.Stack;

/**
The type structure of objects is rather complex. The decisions
regarding that structure are encapsulated here.

For each class (not interface) C, there is a potential "subobject" component that contains
the fields declared in C and the nonstatic methods that are first declared in C.
This subobject component is actually a child of a "subchunk" component
corresponding to C. Every class has a corresponding subchunk component except
for java.lang.Object, for which the subchunk is actually the object itself.
A subchunk component for C is a child of the subchunk component for the parent
class of C (or a child of the main object C's superclass is java.lang.Object).

The main object has a special component, the "interfaces chunk". The
interfaces chunk has one component for each interface, the "interface
subobject". Each interface subobject contains the methods declared by that
interface.

The main object has another special component, the "null chunk".
The null chunk has no components of its own; it's only there to
see which object references may be null --- those whose null chunk
can be traced back to the primordial null object maintained by the
ConstraintManager.

Note that not all of these components will normally be present in any
given type --- only the ones for which the analysis infers information.

The decisions here are driven by several needs:

<UL>
<LI>efficient field and method extraction, especially for interface methods</LI>
<LI>easy, fairly accurate downcasting to class types</LI>
<LI>beneficial interfaction with the optimizations in the solver,
    in particular, the constraint suppression system</LI>
</UL>

Note that downcasting to interface types is not supported (we just punt
and treat it as a no-op). It's not clear how that could work, anyway.

Static fields are treated as components of a single global object that
is passed around everywhere. There are no subobjects in the globals object;
the static fields are direct components of the subchunks.

A note about CMODE/DMODE. We have to be consistent about what CMODE and DMODE mean
for each different kind of component. For the object structural components
(subobject and subchunk components), CMODE means constructing (i.e. tupling)
and DMODE means destructing. This is in contrast to the meaning for
field components, where CMODE means reading and DMODE means writing.

It is the responsibility of other code to make sure that these modes are used
consistently. Structural components are CMODE'ed when a field or method is created,
and DMODE'ed when a field or nonstatic method is accessed. A static field is created when
the class is initialized. A nonstatic field is created when an object is constructed.
A nonstatic method is created when the object is created. A nonstatic method is accessed
when it is called, and a field is accessed when it is read or written. A field component
is CMODE'ed when the field is written and DMODE'ed when the field is read.

NOTE: We need to preserve this behaviour w.r.t. user fields. Somehow we have to arrange
for structural components to be CMODE'ed when a user field is created.
The JBCConstraintGenerator already does this for arrays, which use user fields
to store the length and data.
*/
class SlotManager {
    private static Stack getSuperClasses(JBCClass c) {
        Stack s = new Stack();
        
        c = c.getSuperClass();
        while (c != null) {
            s.push(c);
            c = c.getSuperClass();
        }
        
        return s;
    }
    
    private static Variable getSubobject(ConstraintManager manager, int mode, Variable objVar, JBCClass c) {
        Variable subobjectVar = getSubchunk(manager, mode, objVar, c);
        
        if (manager.isUsingSubobjects()) {
            subobjectVar = subobjectVar.getComponent(manager.getSolver(), mode, JBCSubobjectComponent.get(c));
            manager.setJBCType(subobjectVar, ConstraintManager.SUBOBJECT);
        }
            
        return subobjectVar;
    }
    
    private static Variable getSubchunk(ConstraintManager manager, int mode, Variable objVar, JBCClass c) {
        if (manager.isUsingSubchunks()) {
            if (c.isInterface()) {
                objVar = objVar.getComponent(manager.getSolver(), mode, JBCInterfacesChunkComponent.get());
                    
                manager.setJBCType(objVar, ConstraintManager.SUBCHUNK);
            } else {
                Stack supers = getSuperClasses(c);

                if (!supers.isEmpty()) {
                    World w = manager.getSolver();
                        
                    /* There's no chunk for java.lang.Object, so ignore it */
                    supers.pop();

                    while (!supers.isEmpty()) {
                        objVar = objVar.getComponent(w, mode, JBCSubchunkComponent.get((JBCClass)supers.pop()));
                        manager.setJBCType(objVar, ConstraintManager.SUBCHUNK);
                    }
                        
                    objVar = objVar.getComponent(w, mode, JBCSubchunkComponent.get(c));
                    manager.setJBCType(objVar, ConstraintManager.SUBCHUNK);
                }
            }
        }
        
        return objVar;
    }
    
    static Variable makeDowncast(ConstraintManager manager,
        Variable objVar, JBCClass c) {
        manager.setJBCType(objVar, JBCType.OBJECT);
        
        if (!manager.isUsingSubchunks() || !manager.isUsingSubobjects() || c.isInterface()) {
            return objVar;
        } else {
            Stack supers = getSuperClasses(c);
            
            if (supers.isEmpty()) {
                return objVar;
            } else {
                World w = manager.getSolver();
                Variable result = new Variable(w);
                Variable resultChunk = result;
                Variable objVarChunk = objVar;
                JBCClass root = (JBCClass)supers.pop(); /* should be java.lang.Object */
                ComponentLabel rootComponent = JBCSubobjectComponent.get(root);
                ComponentLabel interfacesComponent = JBCInterfacesChunkComponent.get();
                Variable resultRootSubobject = result.getComponent(w, Variable.CMODE, rootComponent);
                Variable resultInterfacesComponent = result.getComponent(w, Variable.CMODE, interfacesComponent);
                
                objVar.getComponent(w, Variable.DMODE, rootComponent).makeEqual(w, resultRootSubobject);
                manager.setJBCType(resultRootSubobject, ConstraintManager.SUBOBJECT);
                    
                objVar.getComponent(w, Variable.DMODE, interfacesComponent)
                    .makeEqual(w, resultInterfacesComponent);
                manager.setJBCType(resultInterfacesComponent, ConstraintManager.SUBCHUNK);
                
                while (!supers.isEmpty()) {
                    JBCClass sclass = (JBCClass)supers.pop();
                    ComponentLabel subchunkComponent = JBCSubchunkComponent.get(sclass);
                    ComponentLabel subobjectComponent = JBCSubobjectComponent.get(sclass);
                    
                    objVarChunk = objVarChunk.getComponent(w, Variable.DMODE, subchunkComponent);
                    resultChunk = resultChunk.getComponent(w, Variable.CMODE, subchunkComponent);
                    
                    manager.setJBCType(objVarChunk, ConstraintManager.SUBCHUNK);
                    manager.setJBCType(resultChunk, ConstraintManager.SUBCHUNK);

                    Variable resultChunkSubobject = resultChunk.getComponent(w, Variable.CMODE, subobjectComponent);
                    
                    objVarChunk.getComponent(w, Variable.DMODE, subobjectComponent)
                        .makeEqual(w, resultChunkSubobject);
                    manager.setJBCType(resultChunkSubobject, ConstraintManager.SUBOBJECT);
                }
                
                ComponentLabel cSubchunkComponent = JBCSubchunkComponent.get(c);
                Variable resultChunkSubchunk = resultChunk.getComponent(w, Variable.CMODE, cSubchunkComponent);
                
                objVarChunk.getComponent(w, Variable.DMODE, cSubchunkComponent)
                    .makeEqual(w, resultChunkSubchunk);
                manager.setJBCType(resultChunkSubchunk, ConstraintManager.SUBCHUNK);
        
                manager.setJBCType(result, JBCType.OBJECT);
        
                return result;
            }
        }
    }
    
    static Variable makeNonstaticField(ConstraintManager manager, int mode, Variable objVar, JBCField f,
        int structMode) {
        manager.setJBCType(objVar, JBCType.OBJECT);
        
        Variable result = getSubobject(manager, structMode, objVar, f.getContainingClass())
            .getComponent(manager.getSolver(), mode, JBCFieldComponent.get(f));
        
        manager.setJBCType(result, f.getFieldType());
        
        return result;
    }
    
/**
This method is called when a new class prototype is created. We need to flesh out the
subchunk components with CMODE, because otherwise they may only be accessed in DMODE
and we will lose. We can lose because the object may be downcast before the first access to
a field or nonstatic method; this downcast destroys and recreates the structure, meaning that
the original structure is never accessed in CMODE.
*/
    static void addSubchunkStructure(ConstraintManager manager, JBCClass c, Variable objVar) {
        Stack supers = getSuperClasses(c);
        World w = manager.getSolver();

        if (manager.isUsingSubchunks()) {
            objVar.getComponent(w, Variable.CMODE, JBCInterfacesChunkComponent.get());
        }
        
        if (supers.isEmpty()) {
            // java.lang.Object
            // Always put in an Object subobject, for the queryhook
            objVar.getComponent(w, Variable.CMODE, JBCSubobjectComponent.get(c));
        } else {
            // java.lang.Object
            // Always put in an Object subobject, for the queryhook
            objVar.getComponent(w, Variable.CMODE, JBCSubobjectComponent.get((JBCClass)supers.pop()));

            while (!supers.isEmpty()) {
                JBCClass superClass = (JBCClass)supers.pop();
                
                if (manager.isUsingSubchunks()) {
                    objVar = objVar.getComponent(w, Variable.CMODE, JBCSubchunkComponent.get(superClass));
                }
                if (manager.isUsingSubobjects()) {
                    objVar.getComponent(w, Variable.CMODE, JBCSubobjectComponent.get(superClass));
                }
            }
            
            if (manager.isUsingSubchunks()) {
                objVar = objVar.getComponent(w, Variable.CMODE, JBCSubchunkComponent.get(c));
            }
            
            if (manager.isUsingSubobjects()) {
                objVar.getComponent(w, Variable.CMODE, JBCSubobjectComponent.get(c));
            }
        }
    }
    
    static void makeInitializedValue(ConstraintManager manager, Variable v) {
        getSubobject(manager, Variable.CMODE, v, manager.getJavaLangObject());
    }
    
    static Variable[] makeQueryHook(ConstraintManager manager, Variable v, boolean isTarget, boolean isAlwaysObject) {
        World w = manager.getSolver();
        Variable hookVar;
        
        if (isTarget) {
            hookVar = v;
        } else {
            hookVar = new Variable(w);
            hookVar.getInstance(w, JBCSourceHookInstance.get()).makeEqual(w, v);
        }
        
        Variable[] result;
        if (isAlwaysObject) {
          result = new Variable[1];
	} else {
          result = new Variable[2];
          result[1] = hookVar;
	}
        result[0] = hookVar.getComponent(w, Variable.DMODE,
          JBCSubobjectComponent.get(manager.getJavaLangObject()));
        
        for (int i = 0; i < result.length; i++) {
          result[i].setQueried(w);
	}
            
        return result;
    }
    
    static Variable makeNonstaticUserField(ConstraintManager manager, int mode, Variable objVar,
        UserField f, int structMode) {
        Variable result = getSubobject(manager, structMode, objVar, f.getContainingClass())
            .getComponent(manager.getSolver(), mode, JBCUserFieldComponent.get(f));
        
        return result;
    }
    
    static Variable makeStaticField(ConstraintManager manager, int mode, Variable globals, JBCField f,
        int structMode) {
        manager.setJBCType(globals, ConstraintManager.GLOBALS);
        
        Variable result = getSubchunk(manager, structMode, globals, f.getContainingClass())
            .getComponent(manager.getSolver(), mode, JBCFieldComponent.get(f));
            
        manager.setJBCType(result, f.getFieldType());
        
        return result;
    }
    
    static Variable makeStaticUserField(ConstraintManager manager, int mode, Variable globals,
        UserField f, int structMode) {
        manager.setJBCType(globals, ConstraintManager.GLOBALS);
        
        Variable result = getSubchunk(manager, structMode, globals, f.getContainingClass())
            .getComponent(manager.getSolver(), mode, JBCUserFieldComponent.get(f));
        
        return result;
    }
    
    static Variable makeNonstaticMethod(ConstraintManager manager, int mode, Variable objVar, JBCMethod m, InvocationContextComponent cc) {
        manager.setJBCType(objVar, JBCType.OBJECT);
        
        Variable result = getSubobject(manager, mode, objVar, m.getContainingClass())
            .getComponent(manager.getSolver(), mode, cc);
        
        manager.setJBCType(result, m.getMethodType());
        
        return result;
    }
}
