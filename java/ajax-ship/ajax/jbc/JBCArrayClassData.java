/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

class JBCArrayClassData implements ClassData, DataConstants {
    private String name;
    
    private static final String[] arrayInterfaces
        = { "java.lang.Cloneable", "java.io.Serializable" };
    private static final FieldData[] emptyFieldArray = new FieldData[0];
    private static final MethodData[] emptyMethodArray = new MethodData[0];
    
    JBCArrayClassData(String name) {
        this.name = name;
    }
    
    public int getAccessFlags() {
        return ACC_PUBLIC | ACC_FINAL;
    }
    
    public String getClassName() {
        return name;
    }
    
    public String getSuperClassName() {
        return "java.lang.Object";
    }
    
    public String[] getSuperInterfaceNames() {
        return arrayInterfaces;
    }
    
    public FieldData[] getFields() {
        return emptyFieldArray;
    }
    
    public MethodData[] getMethods() {
        return emptyMethodArray;
    }
    
    public String getSourceFileName() {
        return null;
    }
}
