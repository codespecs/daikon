/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver.test;

import ajax.solver.ComponentLabel;
import ajax.util.Association;

class TypeComponent extends ComponentLabel {
    private static Association map = new Association();
    
    private String name;
    
    String getName() {
        return name;
    }

    private TypeComponent(String name) {
        super();
        this.name = name;
    }
    
    static TypeComponent get(String name) {
        Object o = map.get(name);
        
        if (o != null) {
            return (TypeComponent)o;
        } else {
            TypeComponent t = new TypeComponent(name);
            
            map.put(name, t);
            return t;
        }
    }
    
    public String toString() {
        return name;
    }
}
