/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

public class NonexistentFieldError extends Error {
    private JBCField field;
    
    NonexistentFieldError(JBCField field) {
        super("Field " + field.getFieldName() + " does not exist");
        this.field = field;
    }
    
    public JBCField getField() {
        return field;
    }
}
