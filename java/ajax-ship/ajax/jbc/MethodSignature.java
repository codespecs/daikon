/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

public class MethodSignature {
    private String name;
    private String type;
    
    public MethodSignature(JBCMethod m) {
        this(m.getMethodName(), m.getMethodTypeName());
    }
    
    public MethodSignature(String name, String type) {
        this.name = name;
        this.type = type;
    }
    
    public String getName() {
        return name;
    }
    
    public String getType() {
        return type;
    }
    
    public int hashCode() {
        return name.hashCode() + type.hashCode()*189437411;
    }
    
    public boolean equals(Object o) {
        if (o instanceof MethodSignature) {
            MethodSignature sig = (MethodSignature)o;
            
            return sig.name.equals(name) && sig.type.equals(type);
        } else {
            return false;
        }
    }
}
