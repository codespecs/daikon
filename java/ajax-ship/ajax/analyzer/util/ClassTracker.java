/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.util;

import ajax.analyzer.*;
import ajax.jbc.*;
import ajax.Globals;
import ajax.util.CompactSet;
import java.util.*;

/**
This class is a base class for query families that want to know about the possible classes
of objects occuring at a value-point. It manages intermediates that correspond to points
in the lattice of Java class and interface types. Thus each intermediate corresponds to
a Java class or interface plus a set of interfaces. (The set of interfaces is necessary
so that every pair of classes has a unique least upper bound.)

The intermediate Object is either a JBCClass representing a class or interface, or
an array of JBCClasses representing a class or interface and the set of interfaces.
If it's a JBCClass[], then if there is a proper class in the array it will occur
in the first element.
*/
public class ClassTracker extends IndirectionQueryFamily {
    public ClassTracker(Analyzer analyzer, DatumSpecifier[] specifiers, ResultListener[] listeners) {
        super(analyzer, specifiers, listeners);
    }
    
    private static void addClassAndInterfaces(Hashtable set, JBCClass c) {
        if (set.get(c) == null) {
            set.put(c, c);
            
            JBCClass[] superInterfaces = c.getSuperInterfaces();
            
            for (int i = 0; i < superInterfaces.length; i++) {
                addClassAndInterfaces(set, superInterfaces[i]);
            }
        }
    }
    
    private static void addSuperAndInterfaces(Hashtable set, JBCClass c) {
        JBCClass[] superInterfaces = c.getSuperInterfaces();
        
        for (int i = 0; i < superInterfaces.length; i++) {
            addClassAndInterfaces(set, superInterfaces[i]);
        }
        
        for (JBCClass cSuperClass = c.getSuperClass();
            cSuperClass != null; cSuperClass = cSuperClass.getSuperClass()) {
            addClassAndInterfaces(set, cSuperClass);
        }
    }
    
    public static boolean isContainedIn(Object o1, Object o2) {
        JBCClass c1 = o1 instanceof JBCClass ? (JBCClass)o1 : null;
        JBCClass c2 = o2 instanceof JBCClass ? (JBCClass)o2 : null;
        
        if (c1 != null && c2 != null) {
            return c1.isSubclassOf(c2);
        }
        
        JBCClass[] list1 = c1 == null ? ((ClassSet)o1).getClasses() : null;
        JBCClass[] list2 = c2 == null ? ((ClassSet)o2).getClasses() : null;
        
        if (c1 == null) {
            c1 = list1[0];
        }
        if (c2 == null) {
            c2 = list2[0];
        }
        
        if (!c1.isSubclassOf(c2)) {
            return false;
        }
        
        if (list2 != null) {
            for (int i = 1; i < list2.length; i++) {
                JBCClass interface2 = list2[i];
                boolean foundSubclass = c1.isSubclassOf(interface2);
                
                if (list1 != null) {
                    for (int j = 1; j < list1.length && !foundSubclass; j++) {
                        foundSubclass = list1[j].isSubclassOf(interface2);
                    }
                }
                
                if (!foundSubclass) {
                    return false;
                }
            }
        }
        
        return true;
    }
    
    public Object joinIntermediates(Object o1, Object o2) {
        try {
            return join(o1, o2);
        } catch (StackOverflowError ex) {
            System.out.println(toString(o1));
            System.out.println(toString(o2));
            System.out.flush();
            throw ex;
        }
    }
    
    public Object intersectIntermediates(Object o1, Object o2) {
        return intersect(o1, o2);
    }
    
    public static String toString(Object o) {
        if (o instanceof JBCClass) {
            return ((JBCClass)o).toString();
        } else if (o == null) {
            return "None";
        } else {
            JBCClass[] classes = ((ClassSet)o).getClasses();
            StringBuffer buf = new StringBuffer("{");
            
            for (int i = 0; i < classes.length; i++) {
                if (i > 0) {
                    buf.append(", ");
                }
                buf.append(classes[i].toString());
            }
            buf.append("}");
            return buf.toString();
        }
    }
    
    public static JBCClass getActualClass(Object o) {
        if (o == null) {
            return null;
        } else if (o instanceof JBCClass) {
            return (JBCClass)o;
        } else {
            return ((ClassSet)o).getClasses()[0];
        }
    }
    
    public static Object intersect(Object o1, Object o2) {
        JBCClass c1 = o1 instanceof JBCClass ? (JBCClass)o1 : null;
        JBCClass c2 = o2 instanceof JBCClass ? (JBCClass)o2 : null;
        
        if (c1 != null && c2 != null) {
            if (c1.isSubclassOf(c2)) {
                return c1;
            } else if (c2.isSubclassOf(c1)) {
                return c2;
            } else {
                return null;
            }
        }

        JBCClass[] list1 = c1 == null ? ((ClassSet)o1).getClasses() : null;
        JBCClass[] list2 = c2 == null ? ((ClassSet)o2).getClasses() : null;
        
        if (c1 == null) {
            c1 = list1[0];
        }
        if (c2 == null) {
            c2 = list2[0];
        }
        
        CompactSet classes = new CompactSet();
        
        if (c1.isSubclassOf(c2)) {
            classes.addUnconditionally(c1);
        } else if (c2.isSubclassOf(c1)) {
            classes.addUnconditionally(c2);
        } else {
            return null;
        }
        
        if (list1 != null) {
            for (int i = 1; i < list1.length; i++) {
                classes.add(list1[i]);
            }
        }

        if (list2 != null) {
            for (int i = 1; i < list2.length; i++) {
                classes.add(list2[i]);
            }
        }
        
        return makeCanonicalRepresentation(classes);
    }
    
    private static Object makeCanonicalRepresentation(CompactSet classes) {
        Hashtable redundantCommonSupers = new Hashtable();
        
        for (Enumeration e = classes.elements(); e.hasMoreElements();) {
            addSuperAndInterfaces(redundantCommonSupers, (JBCClass)e.nextElement());
        }
        
        int numNonredundantClasses = 0;
        JBCClass baseClass = null;
        
        for (Enumeration e = classes.elements(); e.hasMoreElements();) {
            Object o = e.nextElement();
            
            if (redundantCommonSupers.get(o) == null) {
                JBCClass c = (JBCClass)o;
                
                if (!c.isInterface()) {
                    if (Globals.debug && baseClass != null) {
                        Globals.localError("Multiple base classes detected!");
                    }
                    baseClass = c;
                }
                
                numNonredundantClasses++;
            }
        }
        
        if (numNonredundantClasses == 1) {
            if (baseClass != null) {
                return baseClass;
            } else {
                for (Enumeration e = classes.elements(); e.hasMoreElements();) {
                    Object o = e.nextElement();
                    
                    if (redundantCommonSupers.get(o) == null) {
                        return (JBCClass)o;
                    }
                }
                
                throw Globals.localError("Should never reach here!");
            }
        } else {
            JBCClass[] interfaces = new JBCClass[numNonredundantClasses];
            int index;
            
            if (baseClass != null) {
                interfaces[0] = baseClass;
                index = 1;
            } else {
                index = 0;
            }
            
            for (Enumeration e = classes.elements(); e.hasMoreElements();) {
                Object o = e.nextElement();
                
                if (o != baseClass && redundantCommonSupers.get(o) == null) {
                    interfaces[index] = (JBCClass)o;
                    index++;
                }
            }

            return new ClassSet(interfaces);
        }
    }
    
    public static Object join(Object o1, Object o2) {
        JBCClass c1 = o1 instanceof JBCClass ? (JBCClass)o1 : null;
        JBCClass c2 = o2 instanceof JBCClass ? (JBCClass)o2 : null;
        
        if (c1 != null && c2 != null) {
            if (c1.isSubclassOf(c2)) {
                return c2;
            } else if (c2.isSubclassOf(c1)) {
                return c1;
            }
        }
        
        JBCClass[] list1 = c1 == null ? ((ClassSet)o1).getClasses() : null;
        JBCClass[] list2 = c2 == null ? ((ClassSet)o2).getClasses() : null;
        
        if (c1 == null) {
            c1 = list1[0];
        }
        if (c2 == null) {
            c2 = list2[0];
        }
        
        Hashtable c1Supers = new Hashtable();
        Hashtable c2Supers = new Hashtable();
        
        for (JBCClass c1SuperClass = c1;
            c1SuperClass != null; c1SuperClass = c1SuperClass.getSuperClass()) {
            addClassAndInterfaces(c1Supers, c1SuperClass);
        }
        if (list1 != null) {
            for (int i = 1; i < list1.length; i++) {
                addClassAndInterfaces(c1Supers, list1[i]);
            }
        }
        
        for (JBCClass c2SuperClass = c2;
            c2SuperClass != null; c2SuperClass = c2SuperClass.getSuperClass()) {
            addClassAndInterfaces(c2Supers, c2SuperClass);
        }
        if (list2 != null) {
            for (int i = 1; i < list2.length; i++) {
                addClassAndInterfaces(c2Supers, list2[i]);
            }
        }
        
        /* put smaller set into c1 */
        if (c1Supers.size() > c2Supers.size()) {
            Hashtable tmp = c1Supers;
            
            c1Supers = c2Supers;
            c2Supers = tmp;
        }
        
        CompactSet commonSupers = new CompactSet();
        
        for (Enumeration e = c1Supers.keys(); e.hasMoreElements();) {
            Object k = e.nextElement();
            
            if (c2Supers.get(k) != null) {
                commonSupers.addUnconditionally(k);
            }
        }
        
        return makeCanonicalRepresentation(commonSupers);
    }
}
