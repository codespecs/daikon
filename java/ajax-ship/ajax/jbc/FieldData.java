/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

public interface FieldData {
    public int getAccessFlags();
    public String getFieldName();
    public String getFieldType();
    
    public Object getInitialValue();
}
